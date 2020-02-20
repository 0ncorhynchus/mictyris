use crate::cursor::Cursor;
use crate::token::*;

pub struct Lexer<'a> {
    stream: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: Cursor::new(input),
        }
    }

    pub fn get_token(&mut self) -> Option<Token> {
        self.skip_atomosphere();
        match self.stream.eat()? {
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '#' => match self.stream.eat()? {
                '(' => Some(Token::SharpParen),
                '\\' => Some(Token::Character(self.stream.eat()?)),
                _ => None,
            },
            '"' => Some(Token::String(self.parse_string()?)),
            '\'' => Some(Token::Quote),
            '`' => Some(Token::Backquote),
            ',' => {
                if self.stream.peek() == Some('@') {
                    self.stream.eat();
                    Some(Token::CommaAt)
                } else {
                    Some(Token::Comma)
                }
            }
            '.' => {
                if let Some(next) = self.stream.peek() {
                    if !Self::is_delimiter(&next) {
                        return None;
                    }
                }
                Some(Token::Dot)
            }
            _ => None,
        }
    }

    fn skip_atomosphere(&mut self) {
        while let Some(c) = self.stream.peek() {
            match c {
                ' ' | '\t' | '\n' => {
                    self.stream.eat();
                }
                ';' => {
                    self.stream.eat();
                    while let Some(c) = self.stream.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.stream.eat();
                    }
                }
                _ => break,
            }
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
        assert_eq!(lexer.get_token(), Some(Token::OpenParen));
        assert_eq!(lexer.get_token(), Some(Token::CloseParen));
        assert_eq!(lexer.get_token(), Some(Token::SharpParen));
        assert_eq!(lexer.get_token(), Some(Token::Quote));
        assert_eq!(lexer.get_token(), Some(Token::Backquote));
        assert_eq!(lexer.get_token(), Some(Token::Comma));
        assert_eq!(lexer.get_token(), Some(Token::CommaAt));
        assert_eq!(lexer.get_token(), Some(Token::Dot));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_atomosphere() {
        let mut lexer = Lexer::new("; This is a comment\n(");
        assert_eq!(lexer.get_token(), Some(Token::OpenParen));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Lexer::new(". ..");
        assert_eq!(lexer.get_token(), Some(Token::Dot));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Lexer::new("\"string\"\"\\\"\"");
        assert_eq!(lexer.get_token(), Some(Token::String("string".to_string())));
        assert_eq!(lexer.get_token(), Some(Token::String("\\\"".to_string())));
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\"string");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Lexer::new("\\a");
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a");
        assert_eq!(lexer.get_token(), Some(Token::Character('a')));

        let mut lexer = Lexer::new("#\\");
        assert_eq!(lexer.get_token(), None);
    }
}
