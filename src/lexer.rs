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

pub struct Lexer<'a> {
    stream: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: Cursor::new(input),
        }
    }

    pub fn get_token(&mut self) -> Option<TokenKind> {
        match self.stream.eat()? {
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '#' => match self.stream.eat()? {
                '(' => Some(SharpParen),
                '\\' => {
                    self.stream.eat()?;
                    Some(Character)
                }
                _ => None,
            },
            '"' => {
                self.parse_string()?;
                Some(Str)
            }
            '\'' => Some(Quote),
            '`' => Some(Backquote),
            ',' => {
                if self.stream.peek() == Some('@') {
                    self.stream.eat();
                    Some(CommaAt)
                } else {
                    Some(Comma)
                }
            }
            '.' => {
                if let Some(next) = self.stream.peek() {
                    if !is_delimiter(next) {
                        return None;
                    }
                }
                Some(Dot)
            }
            ';' => {
                while let Some(c) = self.stream.eat() {
                    if c == '\n' {
                        break;
                    }
                }
                Some(Comment)
            }
            c if is_whitespace(c) => {
                while let Some(c) = self.stream.peek() {
                    if is_whitespace(c) {
                        self.stream.eat();
                    } else {
                        break;
                    }
                }
                Some(Whitespace)
            }
            _ => None,
        }
    }

    fn parse_string(&mut self) -> Option<()> {
        while let Some(c) = self.stream.eat() {
            match c {
                '"' => return Some(()),
                '\\' => match self.stream.eat()? {
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
        let mut lexer = Lexer::new("()#('`,,@.");
        assert_eq!(lexer.get_token(), Some(OpenParen));
        assert_eq!(lexer.get_token(), Some(CloseParen));
        assert_eq!(lexer.get_token(), Some(SharpParen));
        assert_eq!(lexer.get_token(), Some(Quote));
        assert_eq!(lexer.get_token(), Some(Backquote));
        assert_eq!(lexer.get_token(), Some(Comma));
        assert_eq!(lexer.get_token(), Some(CommaAt));
        assert_eq!(lexer.get_token(), Some(Dot));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("; This is a comment\n(");
        assert_eq!(lexer.get_token(), Some(Comment));
        assert_eq!(lexer.get_token(), Some(OpenParen));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Lexer::new(". ..");
        assert_eq!(lexer.get_token(), Some(Dot));
        assert_eq!(lexer.get_token(), Some(Whitespace));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Lexer::new("\"string\"\"\\\"\"");
        assert_eq!(lexer.get_token(), Some(Str));
        assert_eq!(lexer.get_token(), Some(Str));
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\"string");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\\a");
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a");
        assert_eq!(lexer.get_token(), Some(Character));

        let mut lexer = Lexer::new("#\\");
        assert_eq!(lexer.get_token(), None);
    }
}
