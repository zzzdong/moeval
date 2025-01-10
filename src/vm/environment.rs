use std::collections::HashMap;

use super::{Value, ValueRef, Callable, NativeFunction};


pub struct Environment {
    symbols: HashMap<String, ValueRef>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: impl ToString, value: impl Into<Value>) {
        self.symbols
            .insert(name.to_string(), ValueRef::new(value.into()));
    }

    // pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    // where
    //     F: Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError> + 'static,
    // {
    //     let name = name.to_string();
    //     self.define(name.clone(), NativeFunction::new(name, Box::new(func)));
    // }

    pub fn define_function<Args: 'static>(
        &mut self,
        name: impl ToString,
        callable: impl Callable<Args>,
    ) {
        self.define(
            name.to_string(),
            NativeFunction::new(name, Box::new(callable.into_function())),
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        self.symbols.get(name.as_ref()).cloned()
    }
}