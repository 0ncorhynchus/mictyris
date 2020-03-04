use super::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Location(Rc<RefCell<Value>>);

pub type Env = Rc<RefCell<Environment>>;

#[derive(Default, Clone)]
pub struct Environment {
    root: Option<Env>,
    inner: HashMap<String, Location>,
}

impl Environment {
    pub fn make_scope(root: Env) -> Self {
        Self {
            root: Some(root),
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, ident: &str, location: Location) {
        self.inner.insert(ident.to_lowercase(), location);
    }

    pub fn lookup(&self, ident: &str) -> Option<Location> {
        match self.inner.get(ident) {
            Some(location) => Some(location.clone()),
            None => self.root.as_ref()?.borrow().lookup(ident),
        }
    }
}

pub fn extends(root: &Env, args: &[String], locations: &[Location]) -> Env {
    let mut env = Environment::make_scope(Rc::clone(root));
    let itr = args.iter().zip(locations.iter().cloned());
    for (ident, location) in itr {
        env.inner.insert(ident.to_lowercase(), location);
    }
    Rc::new(RefCell::new(env))
}

#[derive(Default)]
pub struct Store;

impl Store {
    pub fn get(&self, location: &Location) -> Value {
        location.0.borrow().clone()
    }

    pub fn reserve(&mut self) -> Location {
        Location(Rc::new(RefCell::new(Value::Undefined)))
    }

    pub fn update(&mut self, location: &Location, value: Value) {
        *location.0.borrow_mut() = value;
    }
}
