use super::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Location(Rc<RefCell<Value>>);

pub type Env = Rc<RefCell<Environment>>;

#[derive(Default)]
pub struct Environment {
    inner: HashMap<String, Location>,
}

impl Environment {
    pub fn insert(&mut self, ident: &str, location: Location) {
        self.inner.insert(ident.to_lowercase(), location);
    }

    pub fn lookup(&self, ident: &str) -> Option<Location> {
        self.inner.get(ident).cloned()
    }

    pub fn extends(&mut self, pairs: Vec<(String, Location)>) {
        for (ident, location) in pairs {
            self.inner.insert(ident.to_lowercase(), location);
        }
    }
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
