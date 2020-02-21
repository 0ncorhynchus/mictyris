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
                self.eat_until(is_delimiter);
                Some(Ident)
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
