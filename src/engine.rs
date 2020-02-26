use crate::lexer::Identifier;
use crate::parser::{Datum, Lit};
use crate::pass::*;

use std::fmt;

use Value::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Symbol(Identifier),
    Character(char),
    Number(f64),
    Pair,
    Vector,
    Str(String),
    Bool(bool),
    Miscellaneous,
    Procedure,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol(ident) => write!(f, "{}", ident),
            Character(c) => write!(f, "#\\{}", c),
            Number(n) => write!(f, "{}", n),
            Pair => write!(f, ""),
            Vector => write!(f, ""),
            Str(s) => write!(f, "{}", s),
            Bool(b) => write!(f, "{}", b),
            Miscellaneous => write!(f, ""),
            Procedure => write!(f, ""),
        }
    }
}

pub struct Engine {}

impl Engine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, ast: &AST) -> Option<Value> {
        match ast {
            AST::Const(lit) => Some(self.literal(lit)),
            _ => None,
        }
    }

    // K: Con -> E
    fn literal(&self, lit: &Lit) -> Value {
        match lit {
            Lit::Bool(b) => Bool(*b),
            Lit::Number(n) => Number(*n),
            Lit::Character(c) => Character(*c),
            Lit::Str(s) => Str(s.clone()),
            Lit::Quote(d) => self.datum(d),
        }
    }

    fn datum(&self, datum: &Datum) -> Value {
        match datum {
            Datum::Bool(b) => Bool(*b),
            Datum::Number(n) => Number(*n),
            Datum::Character(c) => Character(*c),
            Datum::Str(s) => Str(s.clone()),
            Datum::Symbol(ident) => Symbol(ident.clone()),
            Datum::List(_) => Pair,
            Datum::Vector(_) => Vector,
        }
    }
}
