#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    // Atomosphere
    Whitespace,
    Comment,

    Identifier,
    Boolean,
    Number,
    Character,
    String,
    OpenParen,
    CloseParen,
    SharpParen,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Dot,
}
