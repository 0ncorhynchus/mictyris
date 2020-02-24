use crate::parser::Lit;
use crate::pass::*;

use std::fmt;

use Value::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Symbol,
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
            Symbol => write!(f, ""),
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
            AST::Const(lit) => match lit {
                Lit::Bool(b) => Some(Bool(*b)),
                Lit::Number(n) => Some(Number(*n)),
                Lit::Character(c) => Some(Character(*c)),
                Lit::Str(s) => Some(Str(s.to_string())),
                Lit::Quote(_d) => None,
            },
            _ => None,
        }
    }
}
