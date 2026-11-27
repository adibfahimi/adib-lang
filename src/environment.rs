use crate::value::Value;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub(crate) type EnvRef = Rc<RefCell<Environment>>;

pub(crate) struct Environment {
    values: HashMap<String, Value>,
    constants: HashSet<String>,
    parent: Option<EnvRef>,
}

impl Environment {
    pub(crate) fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            values: HashMap::new(),
            constants: HashSet::new(),
            parent: None,
        }))
    }

    pub(crate) fn with_parent(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            values: HashMap::new(),
            constants: HashSet::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub(crate) fn define(&mut self, name: String, value: Value, is_const: bool) {
        if is_const {
            self.constants.insert(name.clone());
        } else {
            self.constants.remove(&name);
        }
        self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.values.get(name) {
            return Some(value.clone());
        }
        self.parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(name))
    }

    pub(crate) fn assign(&mut self, name: &str, value: Value) -> AssignResult {
        if self.constants.contains(name) {
            return AssignResult::Constant;
        }
        if let Some(slot) = self.values.get_mut(name) {
            *slot = value;
            return AssignResult::Ok;
        }
        match &self.parent {
            Some(parent) => parent.borrow_mut().assign(name, value),
            None => AssignResult::NotFound,
        }
    }

    /// Update the value of an existing binding without changing const-ness.
    ///
    /// This is used for index assignment: a `const` array cannot be rebound, but
    /// its elements are still mutable.
    pub(crate) fn set_existing(&mut self, name: &str, value: Value) -> bool {
        if let Some(slot) = self.values.get_mut(name) {
            *slot = value;
            return true;
        }
        match &self.parent {
            Some(parent) => parent.borrow_mut().set_existing(name, value),
            None => false,
        }
    }
}

pub(crate) enum AssignResult {
    Ok,
    Constant,
    NotFound,
}
