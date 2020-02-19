#[derive(Debug, PartialEq)]
pub enum Token {
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
