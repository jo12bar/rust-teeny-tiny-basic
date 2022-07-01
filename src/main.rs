mod ast;
mod parser;

use ast::Expr;
use chumsky::prelude::*;
use color_eyre::{eyre::eyre, Report, Section};
use parser::parser;
use std::{env, fs};

fn eval<'a>(
    expr: &'a Expr,
    vars: &mut Vec<(&'a String, f64)>,
    funcs: &mut Vec<(&'a String, &'a [String], &'a Expr)>,
) -> Result<f64, Report> {
    match expr {
        Expr::Num(x) => Ok(*x),

        Expr::Neg(a) => Ok(-eval(a, vars, funcs)?),

        Expr::Add(a, b) => Ok(eval(a, vars, funcs)? + eval(b, vars, funcs)?),
        Expr::Sub(a, b) => Ok(eval(a, vars, funcs)? - eval(b, vars, funcs)?),
        Expr::Mul(a, b) => Ok(eval(a, vars, funcs)? * eval(b, vars, funcs)?),
        Expr::Div(a, b) => Ok(eval(a, vars, funcs)? / eval(b, vars, funcs)?),

        Expr::Let { name, rhs, then } => {
            let rhs = eval(rhs, vars, funcs)?;
            vars.push((name, rhs));
            let output = eval(then, vars, funcs);
            vars.pop();
            output
        }

        Expr::Var(name) => {
            if let Some((_, val)) = vars.iter().rev().find(|(var, _)| *var == name) {
                Ok(*val)
            } else {
                Err(eyre!("Cannot find variable `{name}` in scope"))
            }
        }

        Expr::Fn {
            name,
            args,
            body,
            then,
        } => {
            funcs.push((name, args, body));
            let output = eval(then, vars, funcs);
            funcs.pop();
            output
        }

        Expr::Call(name, args) => {
            if let Some((_, arg_names, body)) =
                funcs.iter().rev().find(|(var, _, _)| *var == name).copied()
            {
                if arg_names.len() == args.len() {
                    let mut args = args
                        .iter()
                        .map(|arg| eval(arg, vars, funcs))
                        .zip(arg_names.iter())
                        .map(|(val, name)| Ok((name, val?)))
                        .collect::<Result<_, Report>>()?;

                    vars.append(&mut args);

                    let output = eval(body, vars, funcs);

                    vars.truncate(vars.len() - args.len());

                    output
                } else {
                    Err(eyre!(
                        "Wrong number of arguments for function `{name}`: expected {}, found {}",
                        arg_names.len(),
                        args.len()
                    ))
                }
            } else {
                Err(eyre!("Cannot find function `{name}` in scope"))
            }
        }
    }
}

fn main() -> Result<(), Report> {
    color_eyre::install()?;

    let src = fs::read_to_string(env::args().nth(1).ok_or(eyre!("Expected file argument"))?)
        .section("Failed to read file.")?;

    match parser().parse(src) {
        Ok(ast) => match eval(&ast, &mut Vec::new(), &mut Vec::new()) {
            Ok(output) => println!("{output}"),
            Err(eval_err) => println!("Evaluation error: {eval_err}"),
        },
        Err(parse_errors) => parse_errors
            .into_iter()
            .for_each(|e| eprintln!("Parse error: {}", e)),
    }

    Ok(())
}
