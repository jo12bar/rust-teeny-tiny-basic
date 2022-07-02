use crate::{
    ast::{BinaryOp, Expr, Value},
    lexer::token::Token,
    span::{Span, Spanned},
};
use chumsky::prelude::*;

/// Parse a stream of [`Token`]s into a spanned [`Expr`] AST tree.
pub fn parse_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = parse_raw_expr(expr.clone());

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover with anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            ));

        // Parse an "if / else if / else if ... / else" chain
        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    // else branch either contains another if statement, a block of expressions, or
                    // just doesn't exist
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((cond, if_branch), else_branch), span: Span| {
                    (
                        Expr::If {
                            cond: Box::new(cond),
                            if_branch: Box::new(if_branch),
                            else_branch: Box::new(match else_branch {
                                Some(else_branch) => else_branch,
                                // If an `if` branch has no trailing `else` branch, we just magic up one that produces null
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        },
                        span,
                    )
                })
        });

        // Both `block`s and `if` are "block expressions" that can appear in the place of statements
        let block_expr = block.or(if_).labelled("block");

        // Fold a whole bunch of "block expressions" together into a `Expr::Then` tree.
        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|this_block_expr, next_block_expr| {
                let span = this_block_expr.1.start..next_block_expr.1.end;
                (
                    Expr::Then {
                        expr: Box::new(this_block_expr),
                        next: Box::new(next_block_expr),
                    },
                    span,
                )
            });

        block_chain
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|this_expr, next_expr| {
                let (span_end, next_expr) = match next_expr {
                    Some(next_expr) => (next_expr.1.end, next_expr),
                    None => (
                        this_expr.1.end,
                        (Expr::Value(Value::Null), this_expr.1.clone()),
                    ),
                };

                let span = this_expr.1.start..span_end; // FIXME: This span is not correct
                (
                    Expr::Then {
                        expr: Box::new(this_expr),
                        next: Box::new(next_expr),
                    },
                    span,
                )
            })
    })
}

/// Parse a "raw expression". Raw expressions are standalone, solitary expressions that aren't
/// followed by any others. So things like block expressions, if expressions, and so on *aren't*
/// raw expressions. Requires an expression parser as an argument, as raw expressions __can__ contain
/// block expressions and such (they just can't be followed by other expressions).
fn parse_raw_expr(
    expr: Recursive<Token, Spanned<Expr>, Simple<Token>>,
) -> Recursive<Token, Spanned<Expr>, Simple<Token>> {
    recursive(|raw_expr| {
        let value = parse_value();
        let ident = parse_ident();

        // A list of expressions
        let items = parse_list_items(expr.clone());
        let list = parse_list(items.clone());

        // A variable binding expression
        let let_ = parse_let(ident.clone(), raw_expr, expr.clone());

        // `Atoms` are expressions that contain no ambiguity
        let atom = parse_atom(value, ident, let_, list, expr.clone());

        // Function calls have very high precedence, so we prioritise them.
        let call = parse_call(atom, items);

        // Product ops (multiply and divide) have equal precedence
        let product = parse_binary_op(
            call,
            just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div)),
        );

        // Sum ops (add and subtract) have equal precedence
        let sum = parse_binary_op(
            product,
            just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub)),
        );

        // Comparison ops (equal, not-equal) have equal precedence
        parse_binary_op(
            sum,
            just(Token::Op("==".to_string()))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq)),
        )
    })
}

/// Parse a single value.
fn parse_value() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    select! {
        Token::Null => Expr::Value(Value::Null),
        Token::Bool(x) => Expr::Value(Value::Bool(x)),
        Token::Num(n) => Expr::Value(Value::Num(n.parse().unwrap())),
        Token::Str(s) => Expr::Value(Value::Str(s)),
    }
    .labelled("value")
}

/// Parse an identifier, extracting the identifier's string name.
fn parse_ident() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Ident(ident) => ident }.labelled("identifier")
}

/// Parse the *items* within a list of expressions.
fn parse_list_items<P>(
    expr_parser: P,
) -> impl Parser<Token, Vec<Spanned<Expr>>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    expr_parser
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
}

/// Parse a list of expressions, delimited with square brackets.
fn parse_list<P>(items_parser: P) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Vec<Spanned<Expr>>, Error = Simple<Token>> + Clone,
{
    items_parser
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map(Expr::List)
}

/// Parse a let binding expression.
fn parse_let<NameParser, ValueParser, BodyParser>(
    name_parser: NameParser,
    value_parser: ValueParser,
    body_parser: BodyParser,
) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone
where
    NameParser: Parser<Token, String, Error = Simple<Token>> + Clone,
    ValueParser: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
    BodyParser: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    just(Token::Let)
        .ignore_then(name_parser)
        .then_ignore(just(Token::Op("=".to_string())))
        .then(value_parser)
        .then_ignore(just(Token::Ctrl(';')))
        .then(body_parser)
        .map(|((name, value), body)| Expr::Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
        })
}

/// Parses a print expression.
fn parse_print<P>(expr_parser: P) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    just(Token::Print)
        .ignore_then(expr_parser.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .map(|expr| Expr::Print(Box::new(expr)))
}

/// Parses an "atom". Atoms are expressions that have no ambiguity as to what they actually are.
fn parse_atom<ValueParser, IdentParser, LetParser, ListParser, ExprParser>(
    value_parser: ValueParser,
    ident_parser: IdentParser,
    let_parser: LetParser,
    list_parser: ListParser,
    expr_parser: ExprParser,
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    ValueParser: Parser<Token, Expr, Error = Simple<Token>> + Clone,
    IdentParser: Parser<Token, String, Error = Simple<Token>> + Clone,
    LetParser: Parser<Token, Expr, Error = Simple<Token>> + Clone,
    ListParser: Parser<Token, Expr, Error = Simple<Token>> + Clone,
    ExprParser: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
{
    value_parser
        .or(ident_parser.map(Expr::Local))
        .or(let_parser)
        .or(list_parser)
        // In nano-Rust, `print` is just a keyword
        .or(parse_print(expr_parser.clone()))
        // Attach a span to the atom
        .map_with_span(|expr, span| (expr, span))
        // Atoms can also just be normal expressions, but surrounded with parentheses
        .or(expr_parser.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        // Attempt to recover anything that looks like a parenthised expression but contains errors
        .recover_with(nested_delimiters(
            Token::Ctrl('('),
            Token::Ctrl(')'),
            [
                (Token::Ctrl('['), Token::Ctrl(']')),
                (Token::Ctrl('{'), Token::Ctrl('}')),
            ],
            |span| (Expr::Error, span),
        ))
        // Attempt to recover anything that looks like a list but contains errors
        .recover_with(nested_delimiters(
            Token::Ctrl('['),
            Token::Ctrl(']'),
            [
                (Token::Ctrl('('), Token::Ctrl(')')),
                (Token::Ctrl('{'), Token::Ctrl('}')),
            ],
            |span| (Expr::Error, span),
        ))
}

/// Parse a function call.
fn parse_call<AtomParser, ItemsParser>(
    atom_parser: AtomParser,
    items_parser: ItemsParser,
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    AtomParser: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
    ItemsParser: Parser<Token, Vec<Spanned<Expr>>, Error = Simple<Token>> + Clone,
{
    atom_parser
        .then(
            // function call arguments
            items_parser
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with_span(|args, span: Span| (args, span))
                .repeated(),
        )
        // fold nested function calls together
        .foldl(|func, args| {
            let span = func.1.start..args.1.end;
            (
                Expr::Call {
                    func: Box::new(func),
                    args: args.0,
                },
                span,
            )
        })
}

/// Parses a binary operator. Requires a higher-precedence operator to preferentially parse first
/// and wrap in the left and right sides.
fn parse_binary_op<HigherPrecedenceParser, OpParser>(
    higher_precedence_parser: HigherPrecedenceParser,
    op_parser: OpParser,
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone
where
    HigherPrecedenceParser: Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone,
    OpParser: Parser<Token, BinaryOp, Error = Simple<Token>> + Clone,
{
    higher_precedence_parser
        .clone()
        .then(op_parser.then(higher_precedence_parser).repeated())
        .foldl(|left, (op, right)| {
            let span = left.1.start..right.1.end;
            (
                Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            )
        })
}
