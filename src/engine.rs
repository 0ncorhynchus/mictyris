mod auxiliary;
pub mod procedure;
mod storage;

use self::auxiliary::*;
use self::procedure::*;
use self::storage::*;
use crate::lexer::Identifier;
use crate::parser::{Datum, Formals, ListDatum, Lit};
use crate::pass::*;
use std::fmt;
use std::rc::Rc;

use Value::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Symbol(Identifier),
    Character(char),
    Number(f64),
    Pair(Location, Location, bool),
    Vector,
    Str(String),
    Bool(bool),

    // Miscellaneous
    Null,
    Unspecified,
    Undefined,

    Procedure(Proc),
}

impl Value {
    pub fn number(&self) -> Option<f64> {
        match self {
            Number(num) => Some(*num),
            _ => None,
        }
    }

    pub fn pair(&self) -> Option<(Location, Location, bool)> {
        match self {
            Pair(car, cdr, mutable) => Some((car.clone(), cdr.clone(), *mutable)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EvalError {
    pub message: String,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Default)]
pub struct Engine {
    env: Env,
    store: Store,
}

impl Engine {
    pub fn new() -> Self {
        let mut engine = Self::default();
        engine.register_proc("list", procedure::list);
        engine.register_proc("cons", procedure::cons);
        engine.register_proc("<", procedure::less);
        engine.register_proc("+", procedure::add);
        engine.register_proc("car", procedure::car);
        engine.register_proc("cdr", procedure::cdr);
        engine.register_proc("set-car!", procedure::setcar);
        engine.register_proc("eqv?", procedure::eqv);
        engine
    }

    pub fn register(&mut self, variable: &str, value: Value) {
        let location = self.store.reserve();
        self.store.update(&location, value);
        self.env.borrow_mut().insert(variable, location);
    }

    pub fn register_proc<F: 'static>(&mut self, variable: &str, proc: F)
    where
        F: Fn(&[Value], ExprCont) -> CommCont,
    {
        let location = self.store.reserve();
        self.store
            .update(&location, Procedure(Proc::new(Rc::new(proc))));
        self.env.borrow_mut().insert(variable, location);
    }

    pub fn eval(&mut self, ast: &AST) -> Answer {
        let expr_cont: ExprCont = Rc::new(|mut values: Vec<Value>| {
            let answer = values.pop().unwrap_or(Unspecified);
            let cont: CommCont = Rc::new(move |_store: &mut Store| Ok(answer.clone()));
            cont
        });

        let cont = eval(ast, Rc::clone(&self.env), expr_cont);
        cont(&mut self.store)
    }

    pub fn eval_and_print(&mut self, ast: &AST) {
        let expr_cont: ExprCont = Rc::new(|mut values: Vec<Value>| {
            if let Some(value) = values.pop() {
                write(value)
            } else {
                Rc::new(|_| Ok(Unspecified))
            }
        });

        let cont = eval(ast, Rc::clone(&self.env), expr_cont);
        match cont(&mut self.store) {
            Ok(_) => (),
            Err(err) => eprintln!("Error: {}", err),
        }
    }
}

fn eval(ast: &AST, env: Env, expr_cont: ExprCont) -> CommCont {
    match ast {
        AST::Const(lit) => eval_literal(lit, expr_cont),
        AST::Var(ident) => eval_variable(ident, env, expr_cont),
        AST::Call(f, args) => eval_proc_call(f, args, env, expr_cont),
        AST::Lambda(args, commands, expr) => match args {
            Formals::List(args) => eval_lambda(args, commands, expr, env, expr_cont),
            Formals::Dot(args, var) => eval_lambda_dot(args, var, commands, expr, env, expr_cont),
        },
        AST::Cond(test, conseq, alter) => match alter {
            Some(alter) => eval_conditional1(test, conseq, alter, env, expr_cont),
            None => eval_conditional2(test, conseq, env, expr_cont),
        },
        AST::Assign(ident, expr) => eval_assign(ident, expr, env, expr_cont),
    }
}

fn eval_literal(lit: &Lit, expr_cont: ExprCont) -> CommCont {
    fn literal(store: &mut Store, lit: &Lit) -> Value {
        match lit {
            Lit::Bool(b) => Bool(*b),
            Lit::Number(n) => Number(*n),
            Lit::Character(c) => Character(*c),
            Lit::Str(s) => Str(s.clone()),
            Lit::Quote(d) => eval_datum(store, d),
        }
    }

    fn eval_datum(store: &mut Store, datum: &Datum) -> Value {
        match datum {
            Datum::Bool(b) => Bool(*b),
            Datum::Number(n) => Number(*n),
            Datum::Character(c) => Character(*c),
            Datum::Str(s) => Str(s.clone()),
            Datum::Symbol(ident) => Symbol(ident.clone()),
            Datum::List(data) => match data {
                ListDatum::List(data) => data.iter().rev().fold(Null, |acc, x| {
                    let cdr = store.reserve();
                    store.update(&cdr, acc);

                    let car = store.reserve();
                    let x = eval_datum(store, x);
                    store.update(&car, x);

                    Pair(car, cdr, true)
                }),
                ListDatum::Cons(data, last) => {
                    let last = eval_datum(store, last);
                    data.iter().rev().fold(last, |acc, x| {
                        let cdr = store.reserve();
                        store.update(&cdr, acc);

                        let car = store.reserve();
                        let x = eval_datum(store, x);
                        store.update(&car, x);

                        Pair(car, cdr, true)
                    })
                }
                ListDatum::Abbrev(_) => unimplemented!(),
            },
            Datum::Vector(_) => Vector,
        }
    }

    let lit = lit.clone();
    Rc::new(move |store| send(literal(store, &lit), Rc::clone(&expr_cont))(store))
}

fn eval_variable(ident: &str, env: Env, expr_cont: ExprCont) -> CommCont {
    let location = match env.borrow().lookup(ident) {
        Some(location) => location,
        None => {
            return wrong("undefined variable");
        }
    };
    let cont = single(move |value| send(value, Rc::clone(&expr_cont)));
    hold(location, cont)
}

fn eval_proc_call(f: &AST, args: &[AST], env: Env, cont: ExprCont) -> CommCont {
    let mut exprs = Vec::with_capacity(args.len() + 1);
    exprs.push(f.clone());
    exprs.extend_from_slice(args);

    let cont: ExprCont = Rc::new(move |values: Vec<Value>| {
        let (f, args) = values.split_first().unwrap();
        applicate(f, args, Rc::clone(&cont))
    });

    eval_list(&exprs, env, cont)
}

fn eval_list(exprs: &[AST], env: Env, cont: ExprCont) -> CommCont {
    match exprs.split_first() {
        None => cont(vec![]),
        Some((head, tail)) => {
            let tail = tail.to_vec();
            let copied_env = Rc::clone(&env);

            let cont = single(move |value: Value| {
                let cont = Rc::clone(&cont);

                let cont: ExprCont = Rc::new(move |mut values| {
                    values.insert(0, value.clone());
                    Rc::clone(&cont)(values)
                });

                eval_list(&tail, Rc::clone(&copied_env), Rc::clone(&cont))
            });

            eval(head, env, cont)
        }
    }
}

fn eval_lambda(
    args: &[String],
    commands: &[AST],
    expr: &AST,
    env: Env,
    cont: ExprCont,
) -> CommCont {
    let args = args.to_vec();
    let commands = commands.to_vec();
    let expr = expr.clone();
    Rc::new(move |store: &mut Store| {
        let args = args.clone();
        let commands = commands.clone();
        let expr = expr.clone();
        let env = Rc::clone(&env);

        let inner = Rc::new(move |values: &[Value], cont: ExprCont| {
            let args = args.clone();
            let commands = commands.clone();
            let expr = expr.clone();

            if values.len() == args.len() {
                let env = Rc::clone(&env);
                let f = Rc::new(move |locations: &[Location]| {
                    let env = extends(&env, &args, &locations);
                    let cont = eval(&expr, Rc::clone(&env), Rc::clone(&cont));
                    eval_commands(&commands, env, cont)
                });
                tievals(f, values)
            } else {
                wrong("wrong number of arguments")
            }
        });

        // store.update(&store.reserve(), Unspecified);

        let proc = Procedure(Proc::new(inner));
        send(proc, Rc::clone(&cont))(store)
    })
}

#[allow(unused_variables)]
fn eval_lambda_dot(
    args: &[String],
    var: &str,
    commands: &[AST],
    expr: &AST,
    env: Env,
    cont: ExprCont,
) -> CommCont {
    let min_args = args.len();
    let mut args = args.to_vec();
    args.push(var.to_string());
    let commands = commands.to_vec();
    let expr = expr.clone();
    Rc::new(move |store: &mut Store| {
        let args = args.clone();
        let commands = commands.clone();
        let expr = expr.clone();
        let env = Rc::clone(&env);
        let location = store.reserve();

        let inner = Rc::new(move |values: &[Value], cont: ExprCont| {
            let args = args.clone();
            let commands = commands.clone();
            let expr = expr.clone();

            if values.len() >= min_args {
                let env = Rc::clone(&env);
                let f = Rc::new(move |locations: &[Location]| {
                    let env = extends(&env, &args, &locations);
                    let cont = eval(&expr, Rc::clone(&env), Rc::clone(&cont));
                    eval_commands(&commands, env, cont)
                });
                tievalsrest(f, values, min_args)
            } else {
                wrong("too few arguments")
            }
        });

        // store.update(&location, Unspecified);

        let proc = Procedure(Proc::new(inner));
        send(proc, Rc::clone(&cont))(store)
    })
}

fn eval_commands(commands: &[AST], env: Env, cont: CommCont) -> CommCont {
    match commands.split_first() {
        Some((head, tail)) => {
            let tail = tail.to_vec();
            let copied_env = Rc::clone(&env);
            let cont = Rc::new(move |_: Vec<Value>| {
                eval_commands(&tail, Rc::clone(&copied_env), Rc::clone(&cont))
            });
            eval(head, Rc::clone(&env), cont)
        }
        None => cont,
    }
}

fn eval_conditional1(test: &AST, conseq: &AST, alter: &AST, env: Env, cont: ExprCont) -> CommCont {
    let conseq = conseq.clone();
    let alter = alter.clone();
    let copied_env = Rc::clone(&env);
    let cont = single(move |value| {
        let cont = Rc::clone(&cont);
        let env = Rc::clone(&copied_env);
        if truish(value) {
            eval(&conseq.clone(), env, cont)
        } else {
            eval(&alter.clone(), env, cont)
        }
    });
    eval(test, env, cont)
}

fn eval_conditional2(test: &AST, conseq: &AST, env: Env, cont: ExprCont) -> CommCont {
    let conseq = conseq.clone();
    let copied_env = Rc::clone(&env);
    let cont = single(move |value| {
        let cont = Rc::clone(&cont);
        let env = Rc::clone(&copied_env);
        if truish(value) {
            eval(&conseq.clone(), env, cont)
        } else {
            send(Unspecified, cont)
        }
    });
    eval(test, env, cont)
}

fn eval_assign(ident: &str, expr: &AST, env: Env, cont: ExprCont) -> CommCont {
    let ident = ident.to_string();
    let copied_env = Rc::clone(&env);
    let cont = single(move |value: Value| {
        let location = match env.borrow().lookup(&ident) {
            Some(location) => location,
            None => {
                return wrong("undefined variable");
            }
        };
        assign(location, value, send(Unspecified, Rc::clone(&cont)))
    });
    eval(expr, copied_env, cont)
}

pub type Answer = Result<Value, EvalError>;

#[derive(Clone)]
pub struct Proc {
    inner: Rc<dyn Fn(&[Value], ExprCont) -> CommCont>,
}

impl Proc {
    fn new(inner: Rc<dyn Fn(&[Value], ExprCont) -> CommCont>) -> Self {
        Self { inner }
    }
}

impl fmt::Debug for Proc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Proc")
    }
}

impl PartialEq for Proc {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

pub type CommCont = Rc<dyn Fn(&mut Store) -> Answer>;
pub type ExprCont = Rc<dyn Fn(Vec<Value>) -> CommCont>;

pub fn write(value: Value) -> CommCont {
    fn fmt(store: &Store, value: &Value) -> String {
        match value {
            Symbol(ident) => format!("{}", ident),
            Character(c) => format!("#\\{}", c),
            Number(n) => format!("{}", n),
            Pair(loc1, loc2, _) => format!(
                "({} . {})",
                fmt(store, &store.get(loc1)),
                fmt(store, &store.get(loc2)),
            ),
            Vector => "".to_string(),
            Str(s) => s.clone(),
            Bool(b) => format!("{}", b),
            Null => "()".to_string(),
            Unspecified => "<unspecified>".to_string(),
            Undefined => "<undefined>".to_string(),
            Procedure(_) => "<procedure>".to_string(),
        }
    }

    Rc::new(move |store: &mut Store| {
        println!("{}", fmt(store, &value));
        Ok(Unspecified)
    })
}
