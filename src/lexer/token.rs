use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Newline,

    Ident(String),

    Num(String),
    Str(String),

    // keywords
    Label,
    Goto,
    Print,
    Input,
    Let,

    If,
    Then,
    EndIf,

    While,
    Repeat,
    EndWhile,

    // operators
    Eq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,

    // Various brackets
    LParen,
    RParen,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Newline => write!(f, "\\n"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Num(n) => write!(f, "{n}"),
            Token::Str(s) => write!(f, "{s}"),
            Token::Label => write!(f, "LABEL"),
            Token::Goto => write!(f, "GOTO"),
            Token::Print => write!(f, "PRINT"),
            Token::Input => write!(f, "INPUT"),
            Token::Let => write!(f, "LET"),
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::EndIf => write!(f, "ENDIF"),
            Token::While => write!(f, "WHILE"),
            Token::Repeat => write!(f, "REPEAT"),
            Token::EndWhile => write!(f, "ENDWHILE"),
            Token::Eq => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::LtEq => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::GtEq => write!(f, ">="),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}
