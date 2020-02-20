use crate::cursor::Cursor;
use crate::token::TokenKind::*;
use crate::token::*;

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

impl Cursor<'_> {
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
                    Some(Character)
                }
                't' | 'f' => Some(Bool),
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
            c if is_whitespace(c) => {
                while let Some(c) = self.peek() {
                    if is_whitespace(c) {
                        self.eat();
                    } else {
                        break;
                    }
                }
                Some(Whitespace)
            }
            _ => None,
        }
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
    }

    #[test]
    fn test_parse_boolean() {
        let mut lexer = Cursor::new("#t #f");
        assert_eq!(lexer.get_token(), Some(Token::new(Bool, 2)));
        assert_eq!(lexer.get_token(), Some(Token::new(Whitespace, 1)));
        assert_eq!(lexer.get_token(), Some(Token::new(Bool, 2)));
    }
}
