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
    pub fn get_token(&mut self) -> Option<TokenKind> {
        match self.eat()? {
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '#' => match self.eat()? {
                '(' => Some(SharpParen),
                '\\' => {
                    self.eat()?;
                    Some(Character)
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
                if let Some(next) = self.peek() {
                    if !is_delimiter(next) {
                        return None;
                    }
                }
                Some(Dot)
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
        let mut lexer = Cursor::new("; This is a comment\n(");
        assert_eq!(lexer.get_token(), Some(Comment));
        assert_eq!(lexer.get_token(), Some(OpenParen));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_termination() {
        let mut lexer = Cursor::new(". ..");
        assert_eq!(lexer.get_token(), Some(Dot));
        assert_eq!(lexer.get_token(), Some(Whitespace));
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_string() {
        let mut lexer = Cursor::new("\"string\"\"\\\"\"");
        assert_eq!(lexer.get_token(), Some(Str));
        assert_eq!(lexer.get_token(), Some(Str));
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Cursor::new("\"string");
        assert_eq!(lexer.get_token(), None);

        let mut lexer = Cursor::new("\\a");
        assert_eq!(lexer.get_token(), None);
    }

    #[test]
    fn test_parse_character() {
        let mut lexer = Cursor::new("#\\a");
        assert_eq!(lexer.get_token(), Some(Character));

        let mut lexer = Cursor::new("#\\");
        assert_eq!(lexer.get_token(), None);
    }
}
