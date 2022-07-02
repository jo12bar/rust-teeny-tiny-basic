pub mod token;

use crate::span::Spanned;
use chumsky::prelude::*;
use token::Token;

/// Takes a raw stream of characters and parses it into tokens (with corresponding source code spans).
pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let num = lex_num();
    let string = lex_string();
    let op = lex_op();
    let ctrl = lex_ctrl();
    let ident = lex_ident();

    // A single token can be any of the above.
    let token = num
        .or(string)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

/// Lexes a number - integer or floating point.
fn lex_num() -> impl Parser<char, Token, Error = Simple<char>> {
    text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num)
}

/// Lexes a string. Currently, there is no way to escape quotes inside of strings.
fn lex_string() -> impl Parser<char, Token, Error = Simple<char>> {
    just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
}

/// Lexes an operator.
fn lex_op() -> impl Parser<char, Token, Error = Simple<char>> {
    one_of("+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op)
}

/// Lexes a control character (delimiters, semicolons, etc...).
fn lex_ctrl() -> impl Parser<char, Token, Error = Simple<char>> {
    one_of("()[]{};,").map(Token::Ctrl)
}

/// Lexes an identifier or keyword.
fn lex_ident() -> impl Parser<char, Token, Error = Simple<char>> {
    text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "print" => Token::Print,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,

        _ => Token::Ident(ident),
    })
}
