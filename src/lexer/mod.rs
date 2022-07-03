pub mod token;

use crate::span::Spanned;
use chumsky::prelude::*;
pub use token::Token;

/// Takes a raw stream of characters and parses it into tokens (with corresponding source code spans).
pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let other_whitespace = lex_non_newline_whitespace();

    let newline = lex_newline_and_comments();
    let num = lex_num();
    let string = lex_string();
    let op = lex_op();
    let ident = lex_ident();

    // A single token can be any of the above.
    let token = num
        .or(string)
        .or(op)
        .or(ident)
        .or(newline.clone())
        .recover_with(skip_then_retry_until([]));

    // Parse multiple tokens, attaching spans to them
    let tokens = token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(other_whitespace.repeated())
        .repeated()
        // make sure there's a newline at the end of input
        .chain(
            newline
                .clone()
                // if there isn't a newline at the end of input, just insert a fake newline token
                .or(end().rewind().to(Token::Newline))
                // make sure to attach a span! this might be incorrect for the fake newlines
                .map_with_span(|tok, span| (tok, span)),
        )
        .then_ignore(end());

    // just completely ignore all comments and newlines before the first non-newline
    // token
    newline.repeated().ignore_then(tokens)
}

/// Lexes newlines, handling both CRLF and LF. Multiple consecutive newlines are
/// collapsed into one for the sake of simpler parsing further down the compilation
/// chain. (though of course, do NOT assume that newline tokens won't be followed by other
/// newline tokens!)
///
/// This function also handles tossing out comments. Since comments can only occur
/// at either the end of a line or completely on their own line, they should be
/// collapsed down into newline tokens.
fn lex_newline_and_comments() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let other_whitespace = lex_non_newline_whitespace();

    let comment = just("#")
        .then(take_until(just('\n')))
        .padded_by(other_whitespace.repeated())
        .to(())
        .labelled("comment");

    text::newline()
        .or(comment)
        .repeated()
        .at_least(1)
        .to(Token::Newline)
        .labelled("newline")
}

/// Lexes non-newline whitespace, and return nothing if successful.
fn lex_non_newline_whitespace() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    // See https://doc.rust-lang.org/reference/whitespace.html
    one_of("\t ").to(()).labelled("whitespace")
}

/// Lexes a number - integer or floating point.
fn lex_num() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let frac = just('.').chain(text::digits(10));

    let exp = just('e')
        .or(just('E'))
        .chain(just('+').or(just('-')).or_not())
        .chain(text::digits(10));

    just('-')
        .or_not()
        .chain(text::int(10))
        .chain(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .collect::<String>()
        .map(Token::Num)
        .labelled("number")
}

/// Lexes a string. Currently, there is no way to escape quotes inside of strings.
fn lex_string() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    let escape = just('\\')
        .ignore_then(
            just('\\')
                .or(just('/'))
                .or(just('"'))
                .or(just('b').to('\x08'))
                .or(just('f').to('\x0C'))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\t'))
                .or(just('u').ignore_then(
                    // unicode UTF-32 escapes
                    filter(|c: &char| c.is_digit(16))
                        .repeated()
                        .exactly(4)
                        .collect::<String>()
                        .validate(|digits, span, emit| {
                            char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                                .unwrap_or_else(|| {
                                    emit(Simple::custom(span, "Invalid unicode character"));
                                    '\u{FFFD}' // unicode replacement character
                                })
                        }),
                )),
        )
        .labelled("string escape character");

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("string")
}

/// Lexes an operator.
fn lex_op() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    just('+')
        .to(Token::Plus)
        .or(just('-').to(Token::Minus))
        .or(just('*').to(Token::Asterisk))
        .or(just('/').to(Token::Slash))
        .or(just("==").to(Token::EqEq))
        .or(just("!=").to(Token::NotEq))
        .or(just(">=").to(Token::GtEq))
        .or(just("<=").to(Token::LtEq))
        .or(just('>').to(Token::Gt))
        .or(just('<').to(Token::Lt))
        .or(just('=').to(Token::Eq))
}

/// Lexes an identifier or keyword.
fn lex_ident() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::ident().map(|ident: String| match ident.to_uppercase().as_str() {
        "LABEL" => Token::Label,
        "GOTO" => Token::Goto,
        "PRINT" => Token::Print,
        "INPUT" => Token::Input,
        "LET" => Token::Let,
        "IF" => Token::If,
        "THEN" => Token::Then,
        "ENDIF" => Token::EndIf,
        "WHILE" => Token::While,
        "REPEAT" => Token::Repeat,
        "ENDWHILE" => Token::EndWhile,

        _ => Token::Ident(ident),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn newlines() {
        let newline = lex_newline_and_comments();

        let conforming_input = "\n\n\r\n\r\n\r\n\n\r\n";
        let nonconforming_input = ";\n";

        assert_eq!(newline.parse(conforming_input), Ok(Token::Newline));
        assert!(newline.parse(nonconforming_input).is_err());
    }
}
