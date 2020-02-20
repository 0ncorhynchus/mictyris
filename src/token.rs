#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    // Atomosphere
    Whitespace,
    Comment,

    // Identifier and Literals
    Ident,
    Bool,
    Number,
    Character,
    Str,

    // Special tokens
    OpenParen,
    CloseParen,
    SharpParen,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Dot,
}
