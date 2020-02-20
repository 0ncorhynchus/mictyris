#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier,
    Boolean,
    Number,
    Character(char),
    String(String),
    OpenParen,
    CloseParen,
    SharpParen,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Dot,
}
