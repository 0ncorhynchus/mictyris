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

    Procedure(Proc),
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
            Procedure(_) => write!(f, "<procedure>"),
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
        let location = self.store.reserve();
        self.env
            .borrow_mut()
            .inner
            .insert(variable.to_lowercase(), location);
        self.store.update(location, value);
    }

    pub fn register_proc(
        &mut self,
        variable: &str,
        proc: Rc<dyn Fn(&[Value], ExprCont) -> CommCont>,
    ) {
        let location = self.store.reserve();
        let proc = Procedure(Proc {
            location,
            inner: proc,
        });
        self.env
            .borrow_mut()
            .inner
            .insert(variable.to_lowercase(), location);
        self.store.update(location, proc);
    }

    pub fn eval(&mut self, ast: &AST) -> Answer {
        let expr_cont: ExprCont = Rc::new(RefCell::new(|values: &[Value]| {
            let answer = values.last().cloned();
            let cont: CommCont = Rc::new(move |_store: &mut Store| answer.clone());
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
        AST::Call(f, args) => eval_proc_call(f, args, env, expr_cont),
        AST::Lambda(args, commands, expr) => eval_lambda(args, commands, expr, env, expr_cont),
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
    let cont = single(Rc::new(move |value| {
        send(value.clone(), Rc::clone(&expr_cont))
    }));
    hold(location, cont)
}

fn eval_proc_call(
    f: &AST,
    args: &[AST],
    env: Rc<RefCell<Environment>>,
    cont: ExprCont,
) -> CommCont {
    let mut exprs = Vec::with_capacity(args.len() + 1);
    exprs.push(f.clone());
    exprs.extend_from_slice(args);

    let cont: ExprCont = Rc::new(RefCell::new(move |values: &[Value]| {
        let (f, args) = values.split_first().unwrap();
        applicate(f, args, Rc::clone(&cont))
    }));

    eval_list(&exprs, env, cont)
}

fn eval_list(exprs: &[AST], env: Rc<RefCell<Environment>>, cont: ExprCont) -> CommCont {
    match exprs.split_first() {
        None => cont.borrow()(&[]),
        Some((head, tail)) => {
            let tail = tail.to_vec();
            let copied_env = Rc::clone(&env);

            let cont = single(Rc::new(move |value: &Value| {
                let value = value.clone();
                let cont = Rc::clone(&cont);

                let cont: ExprCont = Rc::new(RefCell::new(move |values: &[Value]| {
                    let mut args = Vec::with_capacity(values.len() + 1);
                    args.push(value.clone());
                    args.extend_from_slice(values);

                    Rc::clone(&cont).borrow()(&args)
                }));

                eval_list(&tail, Rc::clone(&copied_env), Rc::clone(&cont))
            }));

            eval(head, env, cont)
        }
    }
}

fn eval_lambda(
    args: &[String],
    commands: &[AST],
    expr: &AST,
    env: Rc<RefCell<Environment>>,
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
        let location = store.reserve();

        let inner = Rc::new(move |values: &[Value], cont: ExprCont| {
            let args = args.clone();
            let commands = commands.clone();
            let expr = expr.clone();

            if values.len() == args.len() {
                let env = Rc::clone(&env);
                let f = Rc::new(move |locations: &[Location]| {
                    let env = Rc::clone(&env);
                    let pairs: Vec<_> = args
                        .iter()
                        .cloned()
                        .zip(locations.iter().copied())
                        .collect();
                    env.borrow_mut().extends(&pairs);

                    let cont = eval(&expr, Rc::clone(&env), Rc::clone(&cont));
                    eval_commands(&commands, env, cont)
                });
                tievals(f, values)
            } else {
                wrong("wrong number of arguments")
            }
        });
        let proc = Proc { location, inner };
        store.update(location, Unspecified);
        send(Procedure(proc), Rc::clone(&cont))(store)
    })
}

fn eval_commands(commands: &[AST], env: Rc<RefCell<Environment>>, cont: CommCont) -> CommCont {
    match commands.split_first() {
        Some((head, tail)) => {
            let tail = tail.to_vec();
            let copied_env = Rc::clone(&env);
            let cont = Rc::new(RefCell::new(move |_: &[Value]| {
                eval_commands(&tail, Rc::clone(&copied_env), Rc::clone(&cont))
            }));
            eval(head, Rc::clone(&env), cont)
        }
        None => cont,
    }
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
    let cont = single(Rc::new(move |value| {
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
    let cont = single(Rc::new(move |value| {
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

    fn extends(&mut self, pairs: &[(String, Location)]) {
        for (ident, location) in pairs {
            self.inner.insert(ident.to_lowercase(), *location);
        }
    }
}

#[derive(Default)]
pub struct Store {
    inner: Vec<Option<Value>>,
}

impl Store {
    fn get(&self, location: Location) -> Option<&Value> {
        self.inner.get(location.0)?.as_ref()
    }

    fn reserve(&mut self) -> Location {
        let location = Location(self.inner.len());
        self.inner.push(None);
        location
    }

    fn update(&mut self, location: Location, value: Value) {
        self.inner[location.0] = Some(value);
    }
}

pub type Answer = Option<Value>;

#[derive(Clone)]
pub struct Proc {
    location: Location,
    inner: Rc<dyn Fn(&[Value], ExprCont) -> CommCont>,
}

impl fmt::Debug for Proc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Proc {{ location: {:?} }}", self.location)
    }
}

impl PartialEq for Proc {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

pub type CommCont = Rc<dyn Fn(&mut Store) -> Answer>;
pub type ExprCont = Rc<RefCell<dyn Fn(&[Value]) -> CommCont>>;

fn send(value: Value, cont: ExprCont) -> CommCont {
    cont.borrow()(&[value])
}

fn wrong(message: &'static str) -> CommCont {
    eprintln!("{}", message);
    Rc::new(|_store: &mut Store| None)
}

fn single(f: Rc<dyn Fn(&Value) -> CommCont>) -> ExprCont {
    Rc::new(RefCell::new(move |exprs: &[Value]| match exprs {
        [expr] => f(&expr),
        _ => wrong("wrong number of return values"),
    }))
}

fn hold(location: Location, cont: ExprCont) -> CommCont {
    Rc::new(move |store: &mut Store| {
        let cont = send(store.get(location)?.clone(), Rc::clone(&cont));
        cont(store)
    })
}

fn truish(value: &Value) -> bool {
    *value != Bool(false)
}

fn applicate(f: &Value, args: &[Value], cont: ExprCont) -> CommCont {
    match f {
        Procedure(proc) => (proc.inner)(args, cont),
        _ => wrong("bad procedure"),
    }
}

fn tievals(f: Rc<dyn Fn(&[Location]) -> CommCont>, values: &[Value]) -> CommCont {
    match values.split_first() {
        Some((head, tail)) => {
            let head = head.clone();
            let tail = tail.to_vec();
            let f = Rc::clone(&f);
            Rc::new(move |store: &mut Store| {
                let location = store.reserve();
                let f = Rc::clone(&f);
                let new_f = Rc::new(move |locations: &[Location]| {
                    let mut new_locs = Vec::with_capacity(locations.len() + 1);
                    new_locs.push(location);
                    new_locs.extend_from_slice(locations);
                    f(&new_locs)
                });

                store.update(location, head.clone());
                tievals(new_f, &tail)(store)
            })
        }
        None => f(&[]),
    }
}
