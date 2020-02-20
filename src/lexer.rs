use crate::token::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub struct Lexer<S> {
    stream: S,
}

impl<I> Lexer<Peekable<I>>
where
    I: Iterator<Item = char>,
{
    pub fn new(stream: I) -> Self {
        Self {
            stream: stream.peekable(),
        }
    }

    pub fn get_token(&mut self) -> Token {
        self.skip_atomosphere();
        if let Some(c) = self.stream.next() {
            match c {
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '#' => {
                    if let Some(next) = self.stream.next() {
                        match next {
                            '(' => Token::SharpParen,
                            '\\' => {
                                if let Some(c) = self.stream.next() {
                                    Token::Character(c)
                                } else {
                                    Token::Unknown
                                }
                            }
                            _ => Token::Unknown,
                        }
                    } else {
                        Token::Unknown
                    }
                }
                '"' => {
                    if let Some(s) = self.parse_string() {
                        Token::String(s)
                    } else {
                        Token::Unknown
                    }
                }
                '\'' => Token::Quote,
                '`' => Token::Backquote,
                ',' => {
                    if self.stream.peek() == Some(&'@') {
                        self.stream.next();
                        Token::CommaAt
                    } else {
                        Token::Comma
                    }
                }
                '.' => {
                    if let Some(next) = self.stream.peek() {
                        if !Self::is_delimiter(next) {
                            return Token::Unknown;
                        }
                    }
                    Token::Dot
                }
                _ => Token::Unknown,
            }
        } else {
            Token::Unknown
        }
    }

    fn skip_atomosphere(&mut self) {
        while let Some(c) = self.stream.peek() {
            match c {
                ' ' | '\t' | '\n' => {
                    self.stream.next();
                }
                ';' => {
                    self.stream.next();
                    while let Some(c) = self.stream.peek() {
                        if c == &'\n' {
                            break;
                        }
                        self.stream.next();
                    }
                }
                _ => break,
            }
        }
    }

    fn parse_string(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(c) = self.stream.next() {
            match c {
                '"' => return Some(buffer),
                '\\' => {
                    if let Some(c) = self.stream.next() {
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
        let mut lexer = Lexer::new("()#('`,,@.".chars());
        assert_eq!(lexer.get_token(), Token::OpenParen);
        assert_eq!(lexer.get_token(), Token::CloseParen);
        assert_eq!(lexer.get_token(), Token::SharpParen);
        assert_eq!(lexer.get_token(), Token::Quote);
        assert_eq!(lexer.get_token(), Token::Backquote);
        assert_eq!(lexer.get_token(), Token::Comma);
        assert_eq!(lexer.get_token(), Token::CommaAt);
        assert_eq!(lexer.get_token(), Token::Dot);
        assert_eq!(lexer.get_token(), Token::Unknown);
    }

    #[test]
    fn test_atomosphere() {
        let mut lexer = Lexer::new("; This is a comment\n(".chars());
        assert_eq!(lexer.get_token(), Token::OpenParen);
        assert_eq!(lexer.get_token(), Token::Unknown);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Lexer::new(". ..".chars());
        assert_eq!(lexer.get_token(), Token::Dot);
        assert_eq!(lexer.get_token(), Token::Unknown);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Lexer::new("\"string\"\"\\\"\"".chars());
        assert_eq!(lexer.get_token(), Token::String("string".to_string()));
        assert_eq!(lexer.get_token(), Token::String("\\\"".to_string()));
        assert_eq!(lexer.get_token(), Token::Unknown);

        let mut lexer = Lexer::new("\"string".chars());
        assert_eq!(lexer.get_token(), Token::Unknown);

        let mut lexer = Lexer::new("\\a".chars());
        assert_eq!(lexer.get_token(), Token::Unknown);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a".chars());
        assert_eq!(lexer.get_token(), Token::Character('a'));

        let mut lexer = Lexer::new("#\\".chars());
        assert_eq!(lexer.get_token(), Token::Unknown);
    }
}
