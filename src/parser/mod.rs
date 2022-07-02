mod expr;

use crate::{
    ast::{Expr, Func},
    lexer::token::Token,
};
use chumsky::prelude::*;
use std::collections::HashMap;

pub use expr::parse_expr;

/// Parse a function. Since nano-Rust currently doesn't allow any top-level
/// statements other than functions, this is probably the best entry point
/// into the parser system.
///
/// Outputs a hashmap from strings (function names) to the functions themselves.
pub fn parse_func() -> impl Parser<Token, HashMap<String, Func>, Error = Simple<Token>> + Clone {
    // Match any identifiers
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = ident
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let func = just(Token::Fn)
        // parse function name
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("function name"),
        )
        // parse argument list
        .then(args)
        // parse function body
        .then(
            parse_expr()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                // Attempt to recover with anythign that looks like a function
                // body but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    |span| (Expr::Error, span),
                )),
        )
        .map(|((name, args), body)| (name, Func { args, body }))
        .labelled("function");

    func.repeated()
        .try_map(|fs, _| {
            let mut funcs = HashMap::new();

            for ((name, name_span), func) in fs {
                if funcs.insert(name.clone(), func).is_some() {
                    return Err(Simple::custom(
                        name_span,
                        format!("Function `{name}` already exists"),
                    ));
                }
            }

            Ok(funcs)
        })
        .then_ignore(end())
}
