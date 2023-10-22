use std::collections::HashMap;

use hir::Stmt;
use smol_str::SmolStr;

use crate::Value;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env {
    bindings: HashMap<SmolStr, Binding>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Const(Value),
    Var(Value),
    Func(Subprogram),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Subprogram {
    pub(crate) kind: SubprogKind,
    pub(crate) params: Vec<SmolStr>,
    pub(crate) body: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SubprogKind {
    Function,
    Procedure,
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

    pub(crate) fn get_var(&self, name: &str) -> Option<Value> {
        self.bindings
            .get(name)
            .and_then(|b| {
                if let Binding::Var(v) = b {
                    Some(v)
                } else if let Binding::Const(c) = b {
                    Some(c)
                } else {
                    None
                }
            })
            .cloned()
    }

    pub(crate) fn get_subprogram(&self, callee: &str) -> Option<Subprogram> {
        self.bindings
            .get(callee)
            .and_then(|b| {
                if let Binding::Func(f) = b {
                    Some(f)
                } else {
                    None
                }
            })
            .cloned()
    }
}
