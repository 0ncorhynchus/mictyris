use crate::token::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    ExpectedDelimiter,
    InvalidCharacter(char),
    UnexpectedEOF,
    UnterminatedString,
}

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

    pub fn get_token(&mut self) -> Result<Token, LexerError> {
        self.skip_atomosphere();
        if let Some(c) = self.stream.next() {
            match c {
                '(' => Ok(Token::OpenParen),
                ')' => Ok(Token::CloseParen),
                '#' => {
                    if let Some(next) = self.stream.next() {
                        match next {
                            '(' => Ok(Token::SharpParen),
                            '\\' => {
                                if let Some(c) = self.stream.next() {
                                    Ok(Token::Character(c))
                                } else {
                                    Err(LexerError::UnexpectedEOF)
                                }
                            }
                            _ => Err(LexerError::InvalidCharacter(next)),
                        }
                    } else {
                        Err(LexerError::UnexpectedEOF)
                    }
                }
                '"' => Ok(Token::String(self.parse_string()?)),
                '\'' => Ok(Token::Quote),
                '`' => Ok(Token::Backquote),
                ',' => {
                    if self.stream.peek() == Some(&'@') {
                        self.stream.next();
                        Ok(Token::CommaAt)
                    } else {
                        Ok(Token::Comma)
                    }
                }
                '.' => {
                    if let Some(next) = self.stream.peek() {
                        if !Self::is_delimiter(next) {
                            return Err(LexerError::ExpectedDelimiter);
                        }
                    }
                    Ok(Token::Dot)
                }
                _ => Err(LexerError::InvalidCharacter(c)),
            }
        } else {
            Ok(Token::Unknown)
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

    fn parse_string(&mut self) -> Result<String, LexerError> {
        let mut buffer = String::new();
        while let Some(c) = self.stream.next() {
            match c {
                '"' => return Ok(buffer),
                '\\' => {
                    if let Some(c) = self.stream.next() {
                        match c {
                            '"' | '\\' => {
                                buffer.push('\\');
                                buffer.push(c);
                            }
                            _ => return Err(LexerError::InvalidCharacter('\\')),
                        }
                    } else {
                        return Err(LexerError::UnterminatedString);
                    }
                }
                _ => buffer.push(c),
            }
        }
        Err(LexerError::UnterminatedString)
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
        assert_eq!(lexer.get_token(), Ok(Some(Token::OpenParen)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::CloseParen)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::SharpParen)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::Quote)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::Backquote)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::Comma)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::CommaAt)));
        assert_eq!(lexer.get_token(), Ok(Some(Token::Dot)));
        assert_eq!(lexer.get_token(), Ok(Token::Unknown));
    }

    #[test]
    fn test_atomosphere() {
        let mut lexer = Lexer::new("; This is a comment\n(".chars());
        assert_eq!(lexer.get_token(), Ok(Some(Token::OpenParen)));
        assert_eq!(lexer.get_token(), Ok(Token::Unknown));
    }

    #[test]
    fn test_termination() {
        let mut lexer = Lexer::new(". ..".chars());
        assert_eq!(lexer.get_token(), Ok(Some(Token::Dot)));
        assert_eq!(lexer.get_token(), Err(LexerError::ExpectedDelimiter));
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Lexer::new("\"string\"\"\\\"\"".chars());
        assert_eq!(
            lexer.get_token(),
            Ok(Some(Token::String("string".to_string())))
        );
        assert_eq!(
            lexer.get_token(),
            Ok(Some(Token::String("\\\"".to_string())))
        );
        assert_eq!(lexer.get_token(), Ok(Token::Unknown));

        let mut lexer = Lexer::new("\"string".chars());
        assert_eq!(lexer.get_token(), Err(LexerError::UnterminatedString));

        let mut lexer = Lexer::new("\\a".chars());
        assert_eq!(lexer.get_token(), Err(LexerError::InvalidCharacter('\\')));
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Lexer::new("#\\a".chars());
        assert_eq!(lexer.get_token(), Ok(Some(Token::Character('a'))));

        let mut lexer = Lexer::new("#\\".chars());
        assert_eq!(lexer.get_token(), Err(LexerError::UnexpectedEOF));
    }
}
