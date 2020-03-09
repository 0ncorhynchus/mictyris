use super::{is_delimiter, is_initial, is_subsequent, is_whitespace};
use std::str::Chars;

use TokenKind::*;

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

pub struct Cursor<'a> {
    chars: Chars<'a>,
    last_len: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            last_len: input.len(),
            chars: input.chars(),
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn eat(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn terminated<P: Fn(char) -> bool>(&self, pred: P) -> bool {
        if let Some(c) = self.peek() {
            pred(c)
        } else {
            true
        }
    }

    pub fn len_eaten(&mut self) -> usize {
        let current = self.chars.as_str().len();
        let len = self.last_len - current;
        self.last_len = current;
        len
    }

    pub fn get_token(&mut self) -> Option<Token> {
        let kind = self.get_token_kind()?;
        let len = self.len_eaten();
        Some(Token::new(kind, len))
    }

    pub fn get_token_kind(&mut self) -> Option<TokenKind> {
        match self.eat()? {
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '#' => match self.eat()? {
                '(' => Some(SharpParen),
                '\\' => {
                    self.eat()?;
                    self.eat_until(is_delimiter);
                    Some(Character)
                }
                't' | 'f' => Some(Bool),
                'i' | 'e' | 'b' | 'o' | 'd' | 'x' => {
                    self.eat_until(is_delimiter);
                    Some(Number)
                }
                _ => None,
            },
            '"' => {
                self.string()?;
                Some(Str)
            }
            '\'' => Some(Quote),
            '`' => Some(Backquote),
            ',' => {
                if self.peek() == Some('@') {
                    self.eat();
                    Some(CommaAt)
                } else {
                    Some(Comma)
                }
            }
            '.' => {
                if self.terminated(is_delimiter) {
                    Some(Dot)
                } else if self.triple_dot() {
                    Some(Ident)
                } else if let Some(c) = self.peek() {
                    if c.is_digit(10) {
                        self.eat_until(is_delimiter);
                        Some(Number)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ';' => {
                while let Some(c) = self.eat() {
                    if c == '\n' {
                        break;
                    }
                }
                Some(Comment)
            }
            '+' | '-' => {
                if self.terminated(is_delimiter) {
                    Some(Ident)
                } else {
                    self.eat_until(is_delimiter);
                    Some(Number)
                }
            }
            c if is_initial(c) => {
                self.eat_while(is_subsequent);
                if self.terminated(is_delimiter) {
                    Some(Ident)
                } else {
                    None
                }
            }
            c if is_whitespace(c) => {
                self.eat_while(is_whitespace);
                Some(Whitespace)
            }
            c if c.is_digit(10) => {
                self.eat_until(is_delimiter);
                Some(Number)
            }
            _ => None,
        }
    }

    fn eat_while<P: Fn(char) -> bool>(&mut self, pred: P) {
        while let Some(c) = self.peek() {
            if pred(c) {
                self.eat();
            } else {
                break;
            }
        }
    }

    fn eat_until<P: Fn(char) -> bool>(&mut self, pred: P) {
        self.eat_while(|c| !pred(c));
    }

    fn string(&mut self) -> Option<()> {
        while let Some(c) = self.eat() {
            match c {
                '"' => return Some(()),
                '\\' => match self.eat()? {
                    '"' | '\\' => (),
                    _ => return None,
                },
                _ => (),
            }
        }
        None
    }

    fn triple_dot(&mut self) -> bool {
        if self.eat() != Some('.') {
            return false;
        }
        if self.eat() != Some('.') {
            return false;
        }
        self.terminated(is_delimiter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_special_tokens() {
        let mut lexer = Cursor::new("()#('`,,@.");
        assert_eq!(lexer.get_token(), Some(Token::new(OpenParen, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(CloseParen, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(SharpParen, 2)));
        assert_eq!(lexer.get_token(), Some(Token::new(Quote, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Backquote, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Comma, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(CommaAt, 2)));
        assert_eq!(lexer.get_token(), Some(Token::new(Dot, 1)));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_comment() {
        let mut lexer = Cursor::new("; This is a comment\n(");
        assert_eq!(lexer.get_token(), Some(Token::new(Comment, 20)));
        assert_eq!(lexer.get_token(), Some(Token::new(OpenParen, 1)));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Cursor::new(". ..");
        assert_eq!(lexer.get_token(), Some(Token::new(Dot, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Cursor::new("\"string\"\"\\\"\"");
        assert_eq!(lexer.get_token(), Some(Token::new(Str, 8)));
        assert_eq!(lexer.get_token(), Some(Token::new(Str, 4)));
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Cursor::new("\"string");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Cursor::new("\\a");
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Cursor::new("#\\a");
        assert_eq!(lexer.get_token(), Some(Token::new(Character, 3)));

        let mut lexer = Cursor::new("#\\");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Cursor::new("#\\newline");
        assert_eq!(lexer.get_token(), Some(Token::new(Character, 9)));
    }

    #[test]
    fn test_parse_boolean() {
        let mut lexer = Cursor::new("#t #f");
        assert_eq!(lexer.get_token(), Some(Token::new(Bool, 2)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Bool, 2)));
    }

    #[test]
    fn test_parse_peculiar_identifier() {
        let mut lexer = Cursor::new("+ - ... ");
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 3)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
    }

    #[test]
    fn test_parse_identifier() {
        let mut lexer = Cursor::new("lambda x list->vector complex? ");
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 6)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 12)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Ident, 8)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
    }

    #[test]
    fn test_parse_digit10() {
        let mut lexer = Cursor::new("1000 +10 10## -1/2 .001");
        assert_eq!(lexer.get_token(), Some(Token::new(Number, 4)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Number, 3)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Number, 4)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Number, 4)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Number, 4)));
    }
}
