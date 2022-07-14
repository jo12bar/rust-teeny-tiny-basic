use crate::{lexer::Token, span::Spanned};
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
}

/// An expression. Note that expressions are recursive!
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// For when an error occurs while building the AST - allows for some recovery and cleaner
    /// error reporting.
    Error,

    /// A numeric value.
    Num(f64),

    /// A string value.
    Str(String),

    /// The name of a locally-accessible variable or function.
    Local(String),

    /// For a single expression being acted on by a unary operator.
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<Self>>,
    },

    /// For two expressions being acted on by a binary operator.
    Binary {
        left: Box<Spanned<Self>>,
        op: BinaryOp,
        right: Box<Spanned<Self>>,
    },
}

impl Expression {
    /// Parse an expression from a sequence of tokens.
    pub fn parser() -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone {
        recursive(|expr| {
            // "Atoms" are expressions that contain no ambiguity.
            let atom = Self::parse_atom(expr);

            // Unary ops have higher precedence than other operators.
            let unary = Self::parse_unary_ops(atom);

            // Product ops (multiply and divide) have the same precedence.
            let product = Self::parse_binary_op(
                unary,
                just(Token::Asterisk)
                    .to(BinaryOp::Mul)
                    .or(just(Token::Slash).to(BinaryOp::Div)),
            );

            // Sum ops (add and subtract) have equal precendence.
            let sum = Self::parse_binary_op(
                product,
                just(Token::Plus)
                    .to(BinaryOp::Add)
                    .or(just(Token::Minus).to(BinaryOp::Sub)),
            );

            // Greater than and less than ops have equal precedence.
            let ordering = Self::parse_binary_op(
                sum,
                just(Token::GtEq)
                    .to(BinaryOp::GtEq)
                    .or(just(Token::LtEq).to(BinaryOp::LtEq))
                    .or(just(Token::Gt).to(BinaryOp::Gt))
                    .or(just(Token::Lt).to(BinaryOp::Lt)),
            );

            // Equality and inequality operators have the same precedence.
            Self::parse_binary_op(
                ordering,
                just(Token::EqEq)
                    .to(BinaryOp::Eq)
                    .or(just(Token::NotEq).to(BinaryOp::NotEq)),
            )
        })
    }

    fn parse_atom<ExprParser>(
        expr_parser: ExprParser,
    ) -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone
    where
        ExprParser: Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone,
    {
        let value = Self::parse_value();
        let ident = Self::parse_ident();

        // Atoms could be values
        value
            // If an ident is found, then the atom is a reference to some locally-accessible variable
            .or(ident.map(Self::Local))
            // Attach a span to the atom
            .map_with_span(|atom, span| (atom, span))
            // Atoms can also be normal expressions, but surrounded with parentheses
            .or(expr_parser.delimited_by(
                just(Token::LParen).then_ignore(just(Token::Newline).repeated()),
                just(Token::Newline)
                    .repeated()
                    .ignore_then(just(Token::RParen)),
            ))
            // Attempt to recover anything that looks like a parenthised expression but contains errors
            .recover_with(nested_delimiters(
                Token::LParen,
                Token::RParen,
                [
                    (Token::If, Token::Then),
                    (Token::Then, Token::EndIf),
                    (Token::While, Token::Repeat),
                    (Token::Repeat, Token::EndWhile),
                ],
                |span| (Self::Error, span),
            ))
    }

    fn parse_value() -> impl Parser<Token, Self, Error = Simple<Token>> + Clone {
        select! {
            Token::Str(s) => Self::Str(s),
            Token::Num(n) => Self::Num(n.parse().unwrap()),
        }
        .labelled("value")
    }

    fn parse_ident() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
        select! { Token::Ident(ident) => ident }.labelled("identifier")
    }

    fn parse_unary_ops<HigherPrecedenceParser>(
        higher_precedence_parser: HigherPrecedenceParser,
    ) -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone
    where
        HigherPrecedenceParser: Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone,
    {
        let op = select! {
            Token::Minus => UnaryOp::Neg,
            Token::Plus => UnaryOp::Pos,
        };
        op.then(higher_precedence_parser.clone())
            .map_with_span(|(op, expr), span| {
                (
                    Self::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    span,
                )
            })
            .or(higher_precedence_parser)
    }

    fn parse_binary_op<HigherPrecedenceParser, OpParser>(
        higher_precedence_parser: HigherPrecedenceParser,
        op_parser: OpParser,
    ) -> impl Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone
    where
        HigherPrecedenceParser: Parser<Token, Spanned<Self>, Error = Simple<Token>> + Clone,
        OpParser: Parser<Token, BinaryOp, Error = Simple<Token>> + Clone,
    {
        higher_precedence_parser
            .clone()
            .then(op_parser.then(higher_precedence_parser).repeated())
            .foldl(|left, (op, right)| {
                let span = left.1.start..right.1.end;
                (
                    Self::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span,
                )
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer;
    use chumsky::Stream;

    #[test]
    fn parse_numbers() {
        let test_cases = [
            ("1", (Expression::Num(1.0), 0..1)),
            ("-1", (Expression::Num(-1.0), 0..2)),
            ("1.23", (Expression::Num(1.23), 0..4)),
            ("-1.23", (Expression::Num(-1.23), 0..5)),
            ("1.230", (Expression::Num(1.23), 0..5)),
            ("-1.230", (Expression::Num(-1.23), 0..6)),
            ("420.69e5", (Expression::Num(420.69e5), 0..8)),
            ("420.69e+05", (Expression::Num(420.69e5), 0..10)),
            ("420.69e-5", (Expression::Num(420.69e-5), 0..9)),
            ("-420.69e-5", (Expression::Num(-420.69e-5), 0..10)),
            ("-420.69e+5", (Expression::Num(-420.69e5), 0..10)),
        ];

        for (input, expected_ast) in test_cases {
            let input_len = input.chars().count();

            let tokens = lexer().parse(input).unwrap();
            let ast = Expression::parser()
                .parse(Stream::from_iter(
                    input_len..input_len + 1,
                    tokens.into_iter(),
                ))
                .unwrap();

            assert_eq!(ast, expected_ast, "failed to parse {}", input);
        }
    }

    #[test]
    fn parse_string() {
        let input = r#""\"This is a test string,\"\n- Test string, 2022""#;
        let input_len = input.chars().count();

        let expected = (
            Expression::Str("\"This is a test string,\"\n- Test string, 2022".to_string()),
            0..49,
        );

        let tokens = lexer().parse(input).unwrap();
        let ast = Expression::parser()
            .parse(Stream::from_iter(
                input_len..input_len + 1,
                tokens.into_iter(),
            ))
            .unwrap();

        assert_eq!(ast, expected);
    }
}
