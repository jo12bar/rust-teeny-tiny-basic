mod ast;
mod lexer;
mod parser;
mod span;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use ast::{BinaryOp, Expr, Func, Value, ValueError};
use chumsky::{prelude::*, Stream};
use color_eyre::{eyre::eyre, Report as EyreReport, Section};
use lexer::lexer;
use parser::parse_func;
use span::{Span, Spanned};
use std::{collections::HashMap, env, fs};
use thiserror::Error;

#[derive(Error, Debug)]
enum EvaluationError {
    #[error("Value error: {source}")]
    ValueError {
        #[from]
        source: ValueError,
    },

    #[error("No such variable or function `{name}` accessible in scope.")]
    NoSuchVariable { name: String, span: Span },

    #[error("Conditions in if-blocks must be booleans; Found `{actual_value:?}`")]
    IfStatementConditionIsntBoolean { actual_value: Value, span: Span },

    #[error("Function call error: {source}")]
    FunctionCallError {
        #[from]
        source: FunctionCallError,
    },
}

impl EvaluationError {
    /// Gets the span of this error.
    fn span(&self) -> Span {
        match self {
            EvaluationError::ValueError {
                source: ValueError::NumConversionError { span, .. },
            } => span.clone(),
            EvaluationError::NoSuchVariable { span, .. } => span.clone(),
            EvaluationError::IfStatementConditionIsntBoolean { span, .. } => span.clone(),
            EvaluationError::FunctionCallError { source } => source.span(),
        }
    }
}

#[derive(Error, Debug)]
enum FunctionCallError {
    #[error("Function `{name}` called with wrong number of arguments (expected {expected_len}, found {found_len})")]
    WrongNumberArgs {
        name: String,
        expected_len: usize,
        found_len: usize,
        span: Span,
    },

    #[error("`{value:?}` is not callable")]
    NotCallable { value: Value, span: Span },
}

impl FunctionCallError {
    /// Gets the span of this error.
    fn span(&self) -> Span {
        match self {
            FunctionCallError::WrongNumberArgs { span, .. } => span.clone(),
            FunctionCallError::NotCallable { span, .. } => span.clone(),
        }
    }
}

fn eval_expr(
    expr: &Spanned<Expr>,
    funcs: &HashMap<String, Func>,
    stack: &mut Vec<(String, Value)>,
) -> Result<Value, EvaluationError> {
    Ok(match &expr.0 {
        // Error expressions only get created by parser errors. They cannot exist in a valid AST.
        Expr::Error => unreachable!(),

        // Values just get returned.
        Expr::Value(val) => val.clone(),

        // Evaluate each expression in a list and collect the result
        Expr::List(items) => Value::List(
            items
                .iter()
                .map(|item| eval_expr(item, funcs, stack))
                .collect::<Result<_, _>>()?,
        ),

        // Evaluate a local (either a variable or a function), if it exists.
        Expr::Local(name) => stack
            .iter()
            .rev()
            .find(|(l, _)| l == name)
            .map(|(_, v)| v.clone())
            .or_else(|| Some(Value::Func(name.clone())).filter(|_| funcs.contains_key(name)))
            .ok_or_else(|| EvaluationError::NoSuchVariable {
                name: name.to_string(),
                span: expr.1.clone(),
            })?,

        // Push the new variable onto the stack. Then evaluates everything _after_ the let binding
        // and returns the result (after removing the variable from the stack).
        Expr::Let { name, value, body } => {
            let value = eval_expr(value, funcs, stack)?;
            stack.push((name.clone(), value));
            let result = eval_expr(body, funcs, stack)?;
            stack.pop();
            result
        }

        // Evaluate two expressions. This allows for chaining expressions one after another.
        // The result of evaluating the first expression is thrown away, and the result of
        // evaluating the second is returned.
        Expr::Then { expr, next } => {
            eval_expr(expr, funcs, stack)?;
            eval_expr(next, funcs, stack)?
        }

        // Addition
        Expr::Binary {
            left,
            op: BinaryOp::Add,
            right,
        } => Value::Num(
            eval_expr(left, funcs, stack)?.num(left.1.clone())?
                + eval_expr(right, funcs, stack)?.num(right.1.clone())?,
        ),

        // Subtraction
        Expr::Binary {
            left,
            op: BinaryOp::Sub,
            right,
        } => Value::Num(
            eval_expr(left, funcs, stack)?.num(left.1.clone())?
                - eval_expr(right, funcs, stack)?.num(right.1.clone())?,
        ),

        // Multiplication
        Expr::Binary {
            left,
            op: BinaryOp::Mul,
            right,
        } => Value::Num(
            eval_expr(left, funcs, stack)?.num(left.1.clone())?
                * eval_expr(right, funcs, stack)?.num(right.1.clone())?,
        ),

        // Division
        Expr::Binary {
            left,
            op: BinaryOp::Div,
            right,
        } => Value::Num(
            eval_expr(left, funcs, stack)?.num(left.1.clone())?
                / eval_expr(right, funcs, stack)?.num(right.1.clone())?,
        ),

        // Equality
        Expr::Binary {
            left,
            op: BinaryOp::Eq,
            right,
        } => Value::Bool(eval_expr(left, funcs, stack)? == eval_expr(right, funcs, stack)?),

        // Inequality
        Expr::Binary {
            left,
            op: BinaryOp::NotEq,
            right,
        } => Value::Bool(eval_expr(left, funcs, stack)? != eval_expr(right, funcs, stack)?),

        // Call a function.
        Expr::Call { func, args } => {
            let f = eval_expr(func, funcs, stack)?;

            match f {
                Value::Func(name) => {
                    let f = &funcs[&name];
                    // Build the new stack for the called function by starting with the passed-in arguments
                    let mut stack = if f.args.len() != args.len() {
                        return Err(FunctionCallError::WrongNumberArgs {
                            name,
                            expected_len: f.args.len(),
                            found_len: args.len(),
                            span: expr.1.clone(),
                        }
                        .into());
                    } else {
                        f.args
                            .iter()
                            .zip(args.iter())
                            .map(|(name, arg)| Ok((name.clone(), eval_expr(arg, funcs, stack)?)))
                            .collect::<Result<_, EvaluationError>>()?
                    };
                    eval_expr(&f.body, funcs, &mut stack)?
                }

                f => {
                    return Err(FunctionCallError::NotCallable {
                        value: f,
                        span: func.1.clone(),
                    }
                    .into())
                }
            }
        }

        // Evaluate a condition, and then execute & return either the if branch or the else branch
        Expr::If {
            cond,
            if_branch,
            else_branch,
        } => {
            let c = eval_expr(cond, funcs, stack)?;

            match c {
                Value::Bool(true) => eval_expr(if_branch, funcs, stack)?,
                Value::Bool(false) => eval_expr(else_branch, funcs, stack)?,

                c => {
                    return Err(EvaluationError::IfStatementConditionIsntBoolean {
                        actual_value: c,
                        span: cond.1.clone(),
                    })
                }
            }
        }

        Expr::Print(expr) => {
            let val = eval_expr(expr, funcs, stack)?;
            println!("{val}");
            val
        }
    })
}

fn main() -> Result<(), EyreReport> {
    color_eyre::install()?;

    let src = fs::read_to_string(env::args().nth(1).ok_or(eyre!("Expected file argument"))?)
        .section("Failed to read file.")?;

    let (tokens, mut errs) = lexer().parse_recovery(src.as_str());

    let parse_errs = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (ast, parse_errs) =
            parse_func().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(funcs) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            if let Some(main) = funcs.get("main") {
                if !main.args.is_empty() {
                    return Err(eyre!(
                        "The `main` function is not allowed to take any arguments."
                    ));
                }

                match eval_expr(&main.body, &funcs, &mut Vec::new()) {
                    Ok(val) => println!("Return value: {val}"),
                    Err(e) => errs.push(Simple::custom(e.span(), e.to_string())),
                }
            } else {
                return Err(eyre!(
                    "Source file does not contain a function called `main`!"
                ));
            }
        }

        parse_errs
    } else {
        Vec::new()
    };

    let error_iter = errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())));

    let mut has_errors = false;

    for err in error_iter {
        has_errors = true;

        let report = Report::build(ReportKind::Error, (), err.span().start);

        let report = match err.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(err.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            err.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),

            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected {}",
                    if err.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if err.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        err.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(err.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            err.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),

            chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(err.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().eprint(Source::from(&src))?;
    }

    if has_errors {
        std::process::exit(1);
    }

    Ok(())
}
