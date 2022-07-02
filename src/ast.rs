use crate::span::{Span, Spanned};
use std::fmt;
use thiserror::Error;

/// A nano-Rust literal value.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    List(Vec<Value>),
    Func(String),
}

impl Value {
    /// Attempt to convert the value into a Rust number. Will only return `Ok` if `self` is a [`Value::Num`].
    /// The passed-in span is for attaching span information to the error if `self` is *not* a [`Value::Num`].
    pub fn num(self, span: Span) -> Result<f64, ValueError> {
        if let Value::Num(x) = self {
            Ok(x)
        } else {
            Err(ValueError::NumConversionError { value: self, span })
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(x) => write!(f, "{x}"),
            Value::Num(x) => write!(f, "{x}"),
            Value::Str(x) => write!(f, "{x}"),
            Value::List(xs) => write!(
                f,
                "[{}]",
                xs.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Func(name) => write!(f, "<function: {name}>"),
        }
    }
}

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Value `{value:?}` is not a number")]
    NumConversionError { value: Value, span: Span },
}

/// A binary nano-Rust operator.
#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

/// An expression node in the AST. Child nodes are spanned so we can generate useful runtime errors.
#[derive(Clone, Debug)]
pub enum Expr {
    /// For when building an AST node fails for some reason.
    Error,
    /// A literal value.
    Value(Value),
    /// A list of expressions.
    List(Vec<Spanned<Self>>),
    /// The name of a locally-accessible variable or function.
    Local(String),
    /// A variable declaration/definition.
    Let {
        /// the name of the binding
        name: String,
        /// what the binding is set to
        value: Box<Spanned<Self>>,
        /// the body is the set of expressions that the binding is valid within
        body: Box<Spanned<Self>>,
    },
    /// For an expression node immediately followed by some other expression node.
    Then {
        expr: Box<Spanned<Self>>,
        next: Box<Spanned<Self>>,
    },
    /// Two expression being compared by some binary operator.
    Binary {
        left: Box<Spanned<Self>>,
        op: BinaryOp,
        right: Box<Spanned<Self>>,
    },
    /// A function call with a list of arguments.
    Call {
        func: Box<Spanned<Self>>,
        args: Vec<Spanned<Self>>,
    },
    /// An if-else expression.
    If {
        cond: Box<Spanned<Self>>,
        if_branch: Box<Spanned<Self>>,
        else_branch: Box<Spanned<Self>>,
    },
    /// A print statement (since print is a builtin in this language).
    Print(Box<Spanned<Self>>),
}

/// A function node in the AST. Since functions are one of the few things that aren't expressions.
#[derive(Clone, Debug)]
pub struct Func {
    pub args: Vec<String>,
    pub body: Spanned<Expr>,
}
