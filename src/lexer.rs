use crate::cursor::Cursor;
use crate::token::*;

pub fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' => true,
        _ => false,
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
            '(' => Some(TokenKind::OpenParen),
            ')' => Some(TokenKind::CloseParen),
            '#' => match self.stream.eat()? {
                '(' => Some(TokenKind::SharpParen),
                '\\' => {
                    self.stream.eat()?;
                    Some(TokenKind::Character)
                }
                _ => None,
            },
            '"' => {
                self.parse_string()?;
                Some(TokenKind::String)
            }
            '\'' => Some(TokenKind::Quote),
            '`' => Some(TokenKind::Backquote),
            ',' => {
                if self.stream.peek() == Some('@') {
                    self.stream.eat();
                    Some(TokenKind::CommaAt)
                } else {
                    Some(TokenKind::Comma)
                }
            }
            '.' => {
                if let Some(next) = self.stream.peek() {
                    if !Self::is_delimiter(&next) {
                        return None;
                    }
                }
                Some(TokenKind::Dot)
            }
            ';' => {
                while let Some(c) = self.stream.eat() {
                    if c == '\n' {
                        break;
                    }
                }
                Some(TokenKind::Comment)
            }
            c if is_whitespace(c) => {
                while let Some(c) = self.stream.peek() {
                    if is_whitespace(c) {
                        self.stream.eat();
                    } else {
                        break;
                    }
                }
                Some(TokenKind::Whitespace)
            }
            _ => None,
        }
    }

    fn parse_string(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(c) = self.stream.eat() {
            match c {
                '"' => return Some(buffer),
                '\\' => {
                    if let Some(c) = self.stream.eat() {
                        match c {
                            '"' | '\\' => {
                                buffer.push('\\');
                                buffer.push(c);
                            }
                            _ => return None,
                        }
                    } else {
                        return None;
                    }
                }
                _ => buffer.push(c),
            }
        }
        None
    }

    fn is_delimiter(c: &char) -> bool {
        if c.is_ascii_whitespace() {
            return true;
        }
        match c {
            '(' | ')' | '"' | ';' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_special_tokens() {
        let mut lexer = Lexer::new("()#('`,,@.");
        assert_eq!(lexer.get_token(), Some(TokenKind::OpenParen));
        assert_eq!(lexer.get_token(), Some(TokenKind::CloseParen));
        assert_eq!(lexer.get_token(), Some(TokenKind::SharpParen));
        assert_eq!(lexer.get_token(), Some(TokenKind::Quote));
        assert_eq!(lexer.get_token(), Some(TokenKind::Backquote));
        assert_eq!(lexer.get_token(), Some(TokenKind::Comma));
        assert_eq!(lexer.get_token(), Some(TokenKind::CommaAt));
        assert_eq!(lexer.get_token(), Some(TokenKind::Dot));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("; This is a comment\n(");
        assert_eq!(lexer.get_token(), Some(TokenKind::Comment));
        assert_eq!(lexer.get_token(), Some(TokenKind::OpenParen));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Lexer::new(". ..");
        assert_eq!(lexer.get_token(), Some(TokenKind::Dot));
        assert_eq!(lexer.get_token(), Some(TokenKind::Whitespace));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Lexer::new("\"string\"\"\\\"\"");
        assert_eq!(lexer.get_token(), Some(TokenKind::String));
        assert_eq!(lexer.get_token(), Some(TokenKind::String));
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\"string");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\\a");
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a");
        assert_eq!(lexer.get_token(), Some(TokenKind::Character));

        let mut lexer = Lexer::new("#\\");
        assert_eq!(lexer.get_token(), None);
    }
}
