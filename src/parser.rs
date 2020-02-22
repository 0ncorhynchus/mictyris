use crate::lexer::*;
use std::iter::Peekable;

use Expr::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Variable(String),                              // variable
    Literal(Lit),                                  // literal
    ProcCall(Box<Expr>, Vec<Expr>),                // procedure call
    Lambda(Vec<String>, Vec<Expr>),                // lambda expression
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
    Symbol(String),
    List(Vec<Datum>),
    Vector(Vec<Datum>),
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        let tok = self.lexer.next()?;
        match tok.kind {
            TokenKind::Ident(ident) => Some(Variable(ident)),
            TokenKind::Bool(b) => Some(Literal(Lit::Bool(b))),
            TokenKind::Number(num) => Some(Literal(Lit::Number(num))),
            TokenKind::Character(c) => Some(Literal(Lit::Character(c))),
            TokenKind::Str(s) => Some(Literal(Lit::Str(s))),
            TokenKind::OpenParen => {
                if let TokenKind::Ident(ident) = &self.lexer.peek()?.kind {
                    match ident.as_str() {
                        "lambda" => self.parse_lambda(),
                        "if" => self.parse_conditional(),
                        "set!" => self.parse_assign(),
                        _ => self.parse_call(),
                    }
                } else {
                    self.parse_call()
                }
            }
            TokenKind::Quote => Some(Literal(Lit::Quote(self.parse_datum()?))),
            _ => None,
        }
    }

    fn eat_close_paren(&mut self) -> bool {
        if let Some(tok) = self.lexer.peek() {
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
        let body = self.parse_body()?;
        if self.eat_close_paren() {
            Some(Lambda(formals, body))
        } else {
            None
        }
    }

    fn parse_formals(&mut self) -> Option<Vec<String>> {
        match self.lexer.next()?.kind {
            TokenKind::Ident(ident) => Some(vec![ident]),
            TokenKind::OpenParen => {
                let mut idents = Vec::new();
                while let Some(tok) = self.lexer.next() {
                    match tok.kind {
                        TokenKind::CloseParen => {
                            return Some(idents);
                        }
                        TokenKind::Dot => {
                            if idents.is_empty() {
                                return None;
                            }
                            if let TokenKind::Ident(ident) = self.lexer.next()?.kind {
                                idents.push(ident);
                            } else {
                                return None;
                            }
                            if self.eat_close_paren() {
                                return Some(idents);
                            }
                            return None;
                        }
                        TokenKind::Ident(ident) => {
                            idents.push(ident);
                        }
                        _ => return None,
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn parse_body(&mut self) -> Option<Vec<Expr>> {
        // TODO: parse definitions
        let mut exprs = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            if tok.kind == TokenKind::CloseParen {
                if exprs.is_empty() {
                    return None;
                } else {
                    return Some(exprs);
                }
            } else {
                exprs.push(self.parse()?);
            }
        }
        None
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
        let var = if let TokenKind::Ident(ident) = self.lexer.next()?.kind {
            ident
        } else {
            return None;
        };
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
        Some(Datum::List(vec![self.parse_datum()?]))
    }

    fn parse_list(&mut self) -> Option<Datum> {
        let mut data = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            match tok.kind {
                TokenKind::CloseParen => {
                    self.lexer.next();
                    return Some(Datum::List(data));
                }
                TokenKind::Dot => {
                    if data.is_empty() {
                        return None;
                    }
                    self.lexer.next();
                    data.push(self.parse_datum()?);
                    if self.eat_close_paren() {
                        return Some(Datum::List(data));
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
