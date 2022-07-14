use super::expression::Expression;
use crate::{lexer::Token, span::Spanned};
use chumsky::prelude::*;

/// Statements can't be saved in variables or used for comparisons. That's basically the main
/// difference.
///
/// Oh yeah, and also:
///
/// > "These do stuff."
///
/// &mdash; probably someone smarter than me
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Error,

    Print(Spanned<Expression>),

    If {
        condition: Spanned<Expression>,
        body: Vec<Spanned<Self>>,
    },

    While {
        condition: Spanned<Expression>,
        body: Vec<Spanned<Self>>,
    },

    Label(String),
    Goto(String),

    Let {
        name: String,
        value: Spanned<Expression>,
    },

    Input(String),
}

impl Statement {
    pub fn parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        // have to use a recursive parser due to `IF` and `WHILE` statements
        recursive(|statement| {
            let print = Self::print_parser();
            let label = Self::label_parser();
            let goto = Self::goto_parser();
            let input = Self::input_parser();
            let if_stmt = Self::if_parser(statement.clone());
            let while_stmt = Self::while_parser(statement);
            let let_stmt = Self::let_parser();

            // we expect at least one newline at the end of every statement
            print
                .or(label)
                .or(goto)
                .or(input)
                .or(let_stmt)
                .or(if_stmt)
                .or(while_stmt)
                .then_ignore(just(Token::Newline).repeated().at_least(1))
        })
    }

    fn ident_parser() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
        select! { Token::Ident(ident) => ident }.labelled("identifier")
    }

    fn print_parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        just(Token::Print)
            .ignore_then(Expression::parser())
            .map_with_span(|expr, span| (Self::Print(expr), span))
    }

    fn label_parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        just(Token::Label)
            .ignore_then(Self::ident_parser())
            .map_with_span(|expr, span| (Self::Label(expr), span))
    }

    fn goto_parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        just(Token::Goto)
            .ignore_then(Self::ident_parser())
            .map_with_span(|expr, span| (Self::Goto(expr), span))
    }

    fn input_parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        just(Token::Input)
            .ignore_then(Self::ident_parser())
            .map_with_span(|expr, span| (Self::Input(expr), span))
    }

    fn if_parser<StatementParser>(
        statement_parser: StatementParser,
    ) -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone
    where
        StatementParser: Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone,
    {
        let condition = Expression::parser();
        let body = statement_parser.repeated();

        just(Token::If)
            .ignore_then(condition)
            .then(body.delimited_by(
                just(Token::Then).then(just(Token::Newline).repeated().at_least(1)),
                just(Token::EndIf),
            ))
            .map_with_span(|(condition, body), span| (Self::If { condition, body }, span))
            // attempt to recover anything that looks like an if statement condition but contains errors
            .recover_with(nested_delimiters(
                Token::If,
                Token::Then,
                [
                    (Token::LParen, Token::RParen),
                    (Token::Then, Token::EndIf),
                    (Token::While, Token::Repeat),
                    (Token::Repeat, Token::EndWhile),
                ],
                |span| (Self::Error, span),
            ))
            // attempt to recover anything that looks like an if statement body but contains errors
            .recover_with(nested_delimiters(
                Token::Then,
                Token::EndIf,
                [
                    (Token::LParen, Token::RParen),
                    (Token::If, Token::Then),
                    (Token::While, Token::Repeat),
                    (Token::Repeat, Token::EndWhile),
                ],
                |span| (Self::Error, span),
            ))
    }

    fn while_parser<StatementParser>(
        statement_parser: StatementParser,
    ) -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone
    where
        StatementParser: Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone,
    {
        let condition = Expression::parser();
        let body = statement_parser.repeated();

        just(Token::While)
            .ignore_then(condition)
            .then(body.delimited_by(
                just(Token::Repeat).then(just(Token::Newline).repeated().at_least(1)),
                just(Token::EndWhile),
            ))
            .map_with_span(|(condition, body), span| (Self::While { condition, body }, span))
            // attempt to recover anything that looks like a while statement condition but contains errors
            .recover_with(nested_delimiters(
                Token::While,
                Token::Repeat,
                [
                    (Token::LParen, Token::RParen),
                    (Token::Repeat, Token::EndWhile),
                    (Token::If, Token::Then),
                    (Token::Then, Token::EndIf),
                ],
                |span| (Self::Error, span),
            ))
            // attempt to recover anything that looks like a while statement body but contains errors
            .recover_with(nested_delimiters(
                Token::Repeat,
                Token::EndWhile,
                [
                    (Token::LParen, Token::RParen),
                    (Token::While, Token::Repeat),
                    (Token::If, Token::Then),
                    (Token::Then, Token::EndIf),
                ],
                |span| (Self::Error, span),
            ))
    }

    fn let_parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        let name = Self::ident_parser();
        let value = Expression::parser();

        just(Token::Let)
            .ignore_then(name)
            .then_ignore(just(Token::Eq))
            .then(value)
            .map_with_span(|(name, value), span| (Self::Let { name, value }, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::lexer, parser::expression::BinaryOp};
    use chumsky::Stream;

    fn parse_helper(input: &str) -> Spanned<Statement> {
        let input_len = input.chars().count();
        let tokens = lexer().parse(input).unwrap();

        Statement::parser()
            .parse(Stream::from_iter(
                input_len..input_len + 1,
                tokens.into_iter(),
            ))
            .unwrap()
    }

    #[test]
    fn parse_print() {
        let input = r#"pRiNT "Hello, world!\nYou're the best!""#;

        let expected = (
            Statement::Print((
                Expression::Str("Hello, world!\nYou're the best!".to_string()),
                6..39,
            )),
            0..39,
        );

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_label() {
        let input = r#"LABEL upDog_9"#;

        let expected = (Statement::Label("upDog_9".to_string()), 0..13);

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_goto() {
        let input = r#"GoTo upDog_9"#;

        let expected = (Statement::Goto("upDog_9".to_string()), 0..12);

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_input() {
        let input = r#"inPUT upDog_9"#;

        let expected = (Statement::Input("upDog_9".to_string()), 0..13);

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_if() {
        let input = r#"IF foo >= 3.2 THEN
PRINT "works"
INPUT but_why
ENDIF"#;

        let expected = (
            Statement::If {
                condition: (
                    Expression::Binary {
                        left: Box::new((Expression::Local("foo".to_string()), 3..6)),
                        op: BinaryOp::GtEq,
                        right: Box::new((Expression::Num(3.2), 10..13)),
                    },
                    3..13,
                ),

                body: vec![
                    (
                        Statement::Print((Expression::Str("works".to_string()), 25..32)),
                        19..32,
                    ),
                    (Statement::Input("but_why".to_string()), 33..46),
                ],
            },
            0..52,
        );

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_while() {
        let input = r#"
WHILE bar == 1 REPEAT
    # do something horrible
    LABEL oh_no
    PRINT "Make it stop!"
    GOTO oh_no
ENDWHILE"#;

        let expected = (
            Statement::While {
                condition: (
                    Expression::Binary {
                        left: Box::new((Expression::Local("bar".to_string()), 7..10)),
                        op: BinaryOp::Eq,
                        right: Box::new((Expression::Num(1.0), 14..15)),
                    },
                    7..15,
                ),
                body: vec![
                    (Statement::Label("oh_no".to_string()), 55..66),
                    (
                        Statement::Print((Expression::Str("Make it stop!".to_string()), 77..92)),
                        71..92,
                    ),
                    (Statement::Goto("oh_no".to_string()), 97..107),
                ],
            },
            1..116,
        );

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_let() {
        let input = r#"Let i = i + 1"#;

        let expected = (
            Statement::Let {
                name: "i".to_string(),
                value: (
                    Expression::Binary {
                        left: Box::new((Expression::Local("i".to_string()), 8..9)),
                        op: BinaryOp::Add,
                        right: Box::new((Expression::Num(1.0), 12..13)),
                    },
                    8..13,
                ),
            },
            0..13,
        );

        let ast = parse_helper(input);
        assert_eq!(ast, expected);
    }
}
