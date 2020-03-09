use crate::lexer::*;

use Expr::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Variable(String),                              // variable
    Literal(Lit),                                  // literal
    ProcCall(Box<Expr>, Vec<Expr>),                // procedure call
    Lambda(Formals, Vec<Def>, Vec<Expr>),          // lambda expression
    Cond(Box<Expr>, Box<Expr>, Option<Box<Expr>>), // conditional
    Assignment(String, Box<Expr>),                 // assignment
    Derived,                                       // derived expression
    MacroUse,                                      // macro use
    MacroBlock,                                    // macro block
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    Number(f64),
    Character(char),
    Str(String),
    Quote(Datum),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Datum {
    Bool(bool),
    Number(f64),
    Character(char),
    Str(String),
    Symbol(Identifier),
    List(ListDatum),
    Vector(Vec<Datum>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ListDatum {
    List(Vec<Datum>),
    Cons(Vec<Datum>, Box<Datum>),
    Abbrev(Box<Datum>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Formals {
    List(Vec<String>),
    Dot(Vec<String>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    Variable(String, Box<Expr>),
}

impl Def {
    pub fn get_var(&self) -> &str {
        match self {
            Self::Variable(var, _) => var,
        }
    }

    pub fn get_expr(&self) -> &Expr {
        match self {
            Self::Variable(_, expr) => expr,
        }
    }
}

pub struct Lookahead<I>
where
    I: Iterator,
{
    inner: I,
    first: Option<I::Item>,
    second: Option<I::Item>,
}

impl<I> Lookahead<I>
where
    I: Iterator,
{
    pub fn new(mut inner: I) -> Self {
        let first = inner.next();
        let second = inner.next();
        Self {
            inner,
            first,
            second,
        }
    }

    pub fn first(&self) -> Option<&I::Item> {
        self.first.as_ref()
    }

    pub fn second(&self) -> Option<&I::Item> {
        self.second.as_ref()
    }
}

impl<I> Iterator for Lookahead<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut retval = self.inner.next();
        std::mem::swap(&mut retval, &mut self.first);
        std::mem::swap(&mut self.first, &mut self.second);
        retval
    }
}

pub struct Parser<'a> {
    lexer: Lookahead<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lookahead::new(Lexer::new(input)),
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        let tok = self.lexer.next()?;
        match tok.kind {
            TokenKind::Ident(ident) => Some(Variable(ident.var()?)),
            TokenKind::Bool(b) => Some(Literal(Lit::Bool(b))),
            TokenKind::Number(num) => Some(Literal(Lit::Number(num))),
            TokenKind::Character(c) => Some(Literal(Lit::Character(c))),
            TokenKind::Str(s) => Some(Literal(Lit::Str(s))),
            TokenKind::OpenParen => match self.lexer.first()?.kind.ident() {
                Some(ident) => match ident {
                    Identifier::Lambda => self.parse_lambda(),
                    Identifier::If => self.parse_conditional(),
                    Identifier::Set => self.parse_assign(),
                    Identifier::Var(_) => self.parse_call(),
                    _ => None,
                },
                None => self.parse_call(),
            },
            TokenKind::Quote => Some(Literal(Lit::Quote(self.parse_datum()?))),
            _ => None,
        }
    }

    fn eat_close_paren(&mut self) -> bool {
        if let Some(tok) = self.lexer.first() {
            if tok.kind == TokenKind::CloseParen {
                self.lexer.next();
                return true;
            }
        }
        false
    }

    fn parse_lambda(&mut self) -> Option<Expr> {
        self.lexer.next();
        let formals = self.parse_formals()?;
        let (defs, body) = self.parse_body()?;
        if self.eat_close_paren() {
            Some(Lambda(formals, defs, body))
        } else {
            None
        }
    }

    fn parse_formals(&mut self) -> Option<Formals> {
        match self.lexer.next()?.kind {
            TokenKind::Ident(ident) => Some(Formals::Dot(vec![], ident.var()?)),
            TokenKind::OpenParen => {
                let mut idents = Vec::new();
                while let Some(tok) = self.lexer.next() {
                    match tok.kind {
                        TokenKind::CloseParen => {
                            return Some(Formals::List(idents));
                        }
                        TokenKind::Dot => {
                            if idents.is_empty() {
                                return None;
                            }
                            let last = self.lexer.next()?.kind.ident()?.var()?;
                            if self.eat_close_paren() {
                                return Some(Formals::Dot(idents, last));
                            }
                            return None;
                        }
                        TokenKind::Ident(ident) => {
                            idents.push(ident.var()?);
                        }
                        _ => return None,
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn parse_body(&mut self) -> Option<(Vec<Def>, Vec<Expr>)> {
        let mut defs = vec![];
        while let Some(tok) = self.lexer.first() {
            if tok.kind != TokenKind::OpenParen {
                break;
            }

            if let Some(tok) = self.lexer.second() {
                if tok.kind.ident() == Some(&Identifier::Define) {
                    defs.push(self.parse_define()?);
                    continue;
                }
            }
            break;
        }

        let mut exprs = Vec::new();
        while let Some(tok) = self.lexer.first() {
            if tok.kind == TokenKind::CloseParen {
                if exprs.is_empty() {
                    return None;
                } else {
                    return Some((defs, exprs));
                }
            } else {
                exprs.push(self.parse()?);
            }
        }
        None
    }

    fn parse_define(&mut self) -> Option<Def> {
        self.lexer.next(); // eat '('
        self.lexer.next(); // eat "define"
        let var = self.lexer.next()?.kind.ident()?.var()?;
        let expr = Box::new(self.parse()?);
        if self.eat_close_paren() {
            Some(Def::Variable(var, expr))
        } else {
            None
        }
    }

    fn parse_conditional(&mut self) -> Option<Expr> {
        self.lexer.next();
        let test = Box::new(self.parse()?);
        let consequent = Box::new(self.parse()?);
        let alternate = if self.eat_close_paren() {
            None
        } else {
            let alternate = self.parse()?;
            if self.eat_close_paren() {
                Some(Box::new(alternate))
            } else {
                return None;
            }
        };
        Some(Cond(test, consequent, alternate))
    }

    fn parse_assign(&mut self) -> Option<Expr> {
        self.lexer.next();
        let var = self.lexer.next()?.kind.ident()?.var()?;
        let expr = Box::new(self.parse()?);
        if self.eat_close_paren() {
            Some(Assignment(var, expr))
        } else {
            None
        }
    }

    fn parse_call(&mut self) -> Option<Expr> {
        let operator = Box::new(self.parse()?);
        let operands = self.parse_to_close()?;
        Some(ProcCall(operator, operands))
    }

    fn parse_to_close(&mut self) -> Option<Vec<Expr>> {
        let mut exprs = Vec::new();
        loop {
            if self.eat_close_paren() {
                break;
            }
            exprs.push(self.parse()?);
        }
        Some(exprs)
    }

    fn parse_datum(&mut self) -> Option<Datum> {
        let tok = self.lexer.next()?;
        match tok.kind {
            TokenKind::Ident(ident) => Some(Datum::Symbol(ident)),
            TokenKind::Bool(b) => Some(Datum::Bool(b)),
            TokenKind::Number(num) => Some(Datum::Number(num)),
            TokenKind::Character(c) => Some(Datum::Character(c)),
            TokenKind::Str(s) => Some(Datum::Str(s)),
            TokenKind::OpenParen => self.parse_list(),
            TokenKind::SharpParen => self.parse_vector(),
            TokenKind::Quote => self.parse_abbrev_list(),
            TokenKind::Backquote => self.parse_abbrev_list(),
            TokenKind::Comma => self.parse_abbrev_list(),
            TokenKind::CommaAt => self.parse_abbrev_list(),
            _ => None,
        }
    }

    fn parse_abbrev_list(&mut self) -> Option<Datum> {
        Some(Datum::List(ListDatum::Abbrev(Box::new(
            self.parse_datum()?,
        ))))
    }

    fn parse_list(&mut self) -> Option<Datum> {
        let mut data = Vec::new();
        while let Some(tok) = self.lexer.first() {
            match tok.kind {
                TokenKind::CloseParen => {
                    self.lexer.next();
                    return Some(Datum::List(ListDatum::List(data)));
                }
                TokenKind::Dot => {
                    if data.is_empty() {
                        return None;
                    }
                    self.lexer.next();
                    let last = Box::new(self.parse_datum()?);
                    if self.eat_close_paren() {
                        return Some(Datum::List(ListDatum::Cons(data, last)));
                    }
                    return None;
                }
                _ => data.push(self.parse_datum()?),
            }
        }
        None
    }

    fn parse_vector(&mut self) -> Option<Datum> {
        let mut data = Vec::new();
        loop {
            if self.eat_close_paren() {
                break;
            }
            data.push(self.parse_datum()?);
        }
        Some(Datum::Vector(data))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookahead() {
        let xs = [1, 2, 3];
        let mut iter = Lookahead::new(xs.iter());

        assert_eq!(iter.first(), Some(&&1));
        assert_eq!(iter.second(), Some(&&2));
        assert_eq!(iter.next(), Some(&1));

        assert_eq!(iter.first(), Some(&&2));
        assert_eq!(iter.second(), Some(&&3));
        assert_eq!(iter.next(), Some(&2));

        assert_eq!(iter.first(), Some(&&3));
        assert_eq!(iter.second(), None);
        assert_eq!(iter.next(), Some(&3));

        assert_eq!(iter.first(), None);
        assert_eq!(iter.second(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_call_lambda() {
        let input = "((lambda (x) x) #t)";
        let mut parser = Parser::new(input);
        let answer = ProcCall(
            Box::new(Lambda(
                Formals::List(vec!["x".to_string()]),
                vec![],
                vec![Variable("x".to_string())],
            )),
            vec![Literal(Lit::Bool(true))],
        );
        assert_eq!(parser.parse(), Some(answer));
    }
}
