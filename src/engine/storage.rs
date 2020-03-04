use super::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Location(usize);

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
        self.inner.get(ident).copied()
    }

    pub fn extends(&mut self, pairs: &[(String, Location)]) {
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
    pub fn get(&self, location: Location) -> Option<&Value> {
        self.inner.get(location.0)?.as_ref()
    }

    pub fn reserve(&mut self) -> Location {
        let location = Location(self.inner.len());
        self.inner.push(None);
        location
    }

    pub fn update(&mut self, location: Location, value: Value) {
        self.inner[location.0] = Some(value);
    }
}
