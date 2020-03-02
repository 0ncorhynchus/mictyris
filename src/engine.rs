use crate::lexer::Identifier;
use crate::parser::{Datum, Lit};
use crate::pass::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

    // Miscellaneous
    Unspecified,

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
            Unspecified => write!(f, "<unspecified>"),
            Procedure => write!(f, ""),
        }
    }
}

#[derive(Default)]
pub struct Engine {
    env: Rc<RefCell<Environment>>,
    store: Store,
}

impl Engine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, variable: &str, value: Value) {
        let location = Location(self.store.inner.len());
        self.env
            .borrow_mut()
            .inner
            .insert(variable.to_lowercase(), location);
        self.store.inner.push(value);
    }

    pub fn eval(&mut self, ast: &AST) -> Answer {
        let expr_cont: ExprCont = Rc::new(RefCell::new(|values: &[Value]| {
            let answer = values.last().cloned();
            let cont: CommCont = Box::new(move |_store: &mut Store| answer.clone());
            cont
        }));

        let cont = eval(ast, Rc::clone(&self.env), expr_cont);
        cont(&mut self.store)
    }
}

fn eval(ast: &AST, env: Rc<RefCell<Environment>>, expr_cont: ExprCont) -> CommCont {
    match ast {
        AST::Const(lit) => eval_literal(lit, expr_cont),
        AST::Var(ident) => eval_variable(ident, env, expr_cont),
        AST::Cond(test, conseq, alter) => match alter {
            Some(alter) => eval_conditional1(test, conseq, alter, env, expr_cont),
            None => eval_conditional2(test, conseq, env, expr_cont),
        },
        _ => unimplemented!(),
    }
}

fn eval_literal(lit: &Lit, expr_cont: ExprCont) -> CommCont {
    fn literal(lit: &Lit) -> Value {
        match lit {
            Lit::Bool(b) => Bool(*b),
            Lit::Number(n) => Number(*n),
            Lit::Character(c) => Character(*c),
            Lit::Str(s) => Str(s.clone()),
            Lit::Quote(d) => datum(d),
        }
    }

    fn datum(datum: &Datum) -> Value {
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

    send(literal(lit), expr_cont)
}

fn eval_variable(ident: &str, env: Rc<RefCell<Environment>>, expr_cont: ExprCont) -> CommCont {
    let location = match env.borrow().lookup(ident) {
        Some(location) => location,
        None => {
            return wrong("undefined variable");
        }
    };
    let cont = single(Box::new(move |value| {
        send(value.clone(), Rc::clone(&expr_cont))
    }));
    hold(location, cont)
}

fn eval_conditional1(
    test: &AST,
    conseq: &AST,
    alter: &AST,
    env: Rc<RefCell<Environment>>,
    cont: ExprCont,
) -> CommCont {
    let conseq = conseq.clone();
    let alter = alter.clone();
    let copied_env = Rc::clone(&env);
    let cont = single(Box::new(move |value| {
        let cont = Rc::clone(&cont);
        let env = Rc::clone(&copied_env);
        if truish(value) {
            eval(&conseq.clone(), env, cont)
        } else {
            eval(&alter.clone(), env, cont)
        }
    }));
    eval(test, env, cont)
}

fn eval_conditional2(
    test: &AST,
    conseq: &AST,
    env: Rc<RefCell<Environment>>,
    cont: ExprCont,
) -> CommCont {
    let conseq = conseq.clone();
    let copied_env = Rc::clone(&env);
    let cont = single(Box::new(move |value| {
        let cont = Rc::clone(&cont);
        let env = Rc::clone(&copied_env);
        if truish(value) {
            eval(&conseq.clone(), env, cont)
        } else {
            send(Unspecified, cont)
        }
    }));
    eval(test, env, cont)
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Location(usize);

#[derive(Default)]
struct Environment {
    inner: HashMap<String, Location>,
}

impl Environment {
    fn lookup(&self, ident: &str) -> Option<Location> {
        self.inner.get(ident).copied()
    }
}

#[derive(Default)]
struct Store {
    inner: Vec<Value>,
}

impl Store {
    fn get(&self, location: Location) -> Option<&Value> {
        self.inner.get(location.0)
    }
}

type Answer = Option<Value>;

type CommCont = Box<dyn Fn(&mut Store) -> Answer>;
type ExprCont = Rc<RefCell<dyn Fn(&[Value]) -> CommCont>>;

fn send(value: Value, cont: ExprCont) -> CommCont {
    cont.borrow()(&[value])
}

fn wrong(message: &'static str) -> CommCont {
    eprintln!("{}", message);
    Box::new(|_store: &mut Store| None)
}

fn single(f: Box<dyn Fn(&Value) -> CommCont>) -> ExprCont {
    Rc::new(RefCell::new(move |exprs: &[Value]| match exprs {
        [expr] => f(&expr),
        _ => wrong("wrong number of return values"),
    }))
}

fn hold(location: Location, cont: ExprCont) -> CommCont {
    Box::new(move |store: &mut Store| {
        let cont = send(store.get(location)?.clone(), Rc::clone(&cont));
        cont(store)
    })
}

fn truish(value: &Value) -> bool {
    *value != Bool(false)
}
