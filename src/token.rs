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
