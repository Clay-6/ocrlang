use std::collections::HashMap;

use hir::Stmt;
use smol_str::SmolStr;

use crate::Value;

#[derive(Default)]
pub struct Env {
    bindings: HashMap<SmolStr, Binding>,
}

pub enum Binding {
    Const(Value),
    Var(Value),
    Func(Vec<Stmt>),
}

impl Env {
    pub fn insert(&mut self, name: SmolStr, binding: Binding) {
        self.bindings.insert(name, binding);
    }

    pub fn insert_constant(&mut self, name: SmolStr, value: Value) -> Result<(), ()> {
        if let std::collections::hash_map::Entry::Vacant(e) = self.bindings.entry(name) {
            e.insert(Binding::Const(value));
            Ok(())
        } else {
            Err(())
        }
    }
}
