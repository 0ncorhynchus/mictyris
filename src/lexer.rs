mod cursor;

use std::fmt;

use TokenKind::*;

pub fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' => true,
        _ => false,
    }
}

pub fn is_delimiter(c: char) -> bool {
    match c {
        '(' | ')' | '"' | ';' => true,
        c => is_whitespace(c),
    }
}

pub fn is_initial(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' => true,
        '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        c if 'a' <= c && c <= 'z' => true,
        c if 'A' <= c && c <= 'Z' => true,
        _ => false,
    }
}

pub fn is_subsequent(c: char) -> bool {
    match c {
        c if is_initial(c) => true,
        c if c.is_digit(10) => true,
        '+' | '-' | '.' | '@' => true,
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(Identifier),
    Bool(bool),
    Number(f64),
    Character(char),
    Str(String),
    OpenParen,
    CloseParen,
    SharpParen,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Dot,
}

impl TokenKind {
    pub fn ident(&self) -> Option<&Identifier> {
        match self {
            Self::Ident(ident) => Some(ident),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Identifier {
    Var(String),
    Else,
    Arrow,
    Define,
    Unquote,
    UnquoteSplicing,
    Quote,
    Lambda,
    If,
    Set,
    Begin,
    Cond,
    And,
    Or,
    Case,
    Let,
    LetStar,
    Letrec,
    Do,
    Delay,
    Quasiquote,
}

impl Identifier {
    pub fn var(&self) -> Option<String> {
        match self {
            Self::Var(var) => Some(var.to_string()),
            _ => None,
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(ident) => write!(f, "{}", ident),
            Self::Else => write!(f, "else"),
            Self::Arrow => write!(f, "=>"),
            Self::Define => write!(f, "define"),
            Self::Unquote => write!(f, "unquote"),
            Self::UnquoteSplicing => write!(f, "unquote-splicing"),
            Self::Quote => write!(f, "quote"),
            Self::Lambda => write!(f, "lambda"),
            Self::If => write!(f, "if"),
            Self::Set => write!(f, "set!"),
            Self::Begin => write!(f, "begin"),
            Self::Cond => write!(f, "cond"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Case => write!(f, "case"),
            Self::Let => write!(f, "let"),
            Self::LetStar => write!(f, "let*"),
            Self::Letrec => write!(f, "letrec"),
            Self::Do => write!(f, "do"),
            Self::Delay => write!(f, "delay"),
            Self::Quasiquote => write!(f, "quasiquote"),
        }
    }
}

pub struct Lexer<'a> {
    cursor: cursor::Cursor<'a>,
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: cursor::Cursor::new(input),
            input,
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> Option<Token> {
        let tok = self.cursor.get_token()?;

        let start = self.pos;
        self.pos += tok.len;

        let kind = match tok.kind {
            cursor::TokenKind::Whitespace => return self.parse(),
            cursor::TokenKind::Comment => return self.parse(),

            cursor::TokenKind::Ident => self.parse_ident(start)?,
            cursor::TokenKind::Bool => self.parse_bool(start)?,
            cursor::TokenKind::Number => self.parse_number(start)?,
            cursor::TokenKind::Character => self.parse_character(start)?,
            cursor::TokenKind::Str => Str(self.substr(start).to_string()),

            cursor::TokenKind::OpenParen => OpenParen,
            cursor::TokenKind::CloseParen => CloseParen,
            cursor::TokenKind::SharpParen => SharpParen,
            cursor::TokenKind::Quote => Quote,
            cursor::TokenKind::Backquote => Backquote,
            cursor::TokenKind::Comma => Comma,
            cursor::TokenKind::CommaAt => CommaAt,
            cursor::TokenKind::Dot => Dot,
        };

        Some(Token::new(kind))
    }

    fn substr(&self, start: usize) -> &'a str {
        &self.input[start..self.pos]
    }

    fn parse_ident(&mut self, start: usize) -> Option<TokenKind> {
        let ident = self.substr(start);
        let ident = match ident.to_lowercase().as_str() {
            "else" => Identifier::Else,
            "=>" => Identifier::Arrow,
            "define" => Identifier::Define,
            "unquote" => Identifier::Unquote,
            "unquote-splicing" => Identifier::UnquoteSplicing,
            "quote" => Identifier::Quote,
            "lambda" => Identifier::Lambda,
            "if" => Identifier::If,
            "set!" => Identifier::Set,
            "begin" => Identifier::Begin,
            "cond" => Identifier::Cond,
            "and" => Identifier::And,
            "or" => Identifier::Or,
            "case" => Identifier::Case,
            "let" => Identifier::Let,
            "let*" => Identifier::LetStar,
            "letrec" => Identifier::Letrec,
            "do" => Identifier::Do,
            "delay" => Identifier::Delay,
            "quasiquote" => Identifier::Quasiquote,
            _ => Identifier::Var(ident.to_string()),
        };
        Some(Ident(ident))
    }

    fn parse_bool(&mut self, start: usize) -> Option<TokenKind> {
        match self.substr(start) {
            "#t" => Some(Bool(true)),
            "#f" => Some(Bool(false)),
            _ => None,
        }
    }

    fn parse_number(&mut self, start: usize) -> Option<TokenKind> {
        let num: f64 = self.substr(start).parse().ok()?;
        Some(Number(num))
    }

    fn parse_character(&mut self, start: usize) -> Option<TokenKind> {
        let s = self.substr(start + 2);

        let c = if s.len() == 1 {
            s.chars().next()?
        } else {
            match s.to_lowercase().as_str() {
                "space" => ' ',
                "newline" => '\n',
                _ => return None,
            }
        };

        Some(Character(c))
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ident() {
        let mut lexer = Lexer::new("lambda ...");
        assert_eq!(lexer.parse(), Some(Token::new(Ident(Identifier::Lambda))));
        assert_eq!(
            lexer.parse(),
            Some(Token::new(Ident(Identifier::Var("...".to_string()))))
        );
        assert_eq!(lexer.parse(), None);
    }

    #[test]
    fn test_parse_bool() {
        let mut lexer = Lexer::new("#t #f");
        assert_eq!(lexer.parse(), Some(Token::new(Bool(true))));
        assert_eq!(lexer.parse(), Some(Token::new(Bool(false))));
        assert_eq!(lexer.parse(), None);
    }

    #[test]
    fn test_parse_number() {
        let mut lexer = Lexer::new("0.1 100");
        assert_eq!(lexer.parse(), Some(Token::new(Number(0.1))));
        assert_eq!(lexer.parse(), Some(Token::new(Number(100.0))));
        assert_eq!(lexer.parse(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a #\\space");
        assert_eq!(lexer.parse(), Some(Token::new(Character('a'))));
        assert_eq!(lexer.parse(), Some(Token::new(Character(' '))));
        assert_eq!(lexer.parse(), None);
    }

    #[test]
    fn test_parse_special_tokens() {
        let mut lexer = Lexer::new("()#('`,,@.");
        assert_eq!(lexer.parse(), Some(Token::new(OpenParen)));
        assert_eq!(lexer.parse(), Some(Token::new(CloseParen)));
        assert_eq!(lexer.parse(), Some(Token::new(SharpParen)));
        assert_eq!(lexer.parse(), Some(Token::new(Quote)));
        assert_eq!(lexer.parse(), Some(Token::new(Backquote)));
        assert_eq!(lexer.parse(), Some(Token::new(Comma)));
        assert_eq!(lexer.parse(), Some(Token::new(CommaAt)));
        assert_eq!(lexer.parse(), Some(Token::new(Dot)));
        assert_eq!(lexer.parse(), None);
    }
}
