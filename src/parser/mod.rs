pub mod expression;
pub mod statement;

use crate::{lexer::Token, span::Spanned};
use chumsky::prelude::*;
use statement::Statement;

/// Parse several statements, constituting a program.
pub fn parser() -> impl Parser<Token, Vec<Spanned<Statement>>, Error = Simple<Token>> {
    Statement::parser()
        .repeated()
        .at_least(1)
        .then_ignore(end())
}
