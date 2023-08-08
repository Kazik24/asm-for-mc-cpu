use crate::mylang::ast::Type;
use std::cell::Cell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub struct ValueAllocator {
    index: Rc<Cell<u32>>,
    regs: Rc<Vec<bool>>,
}

#[derive(Copy, Clone, PartialEq)]
pub struct Value {
    pub index: u32,
    scope_bound: bool,
    pub argument: bool,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.argument {
            write!(f, "arg[{}]", self.index)
        } else if self.scope_bound {
            write!(f, "var[{}]", self.index)
        } else {
            write!(f, "tmp[{}]", self.index)
        }
    }
}

impl Value {
    pub fn bind_scope(mut self) -> Self {
        self.scope_bound = true;
        self
    }
    pub fn bind_arg(mut self) -> Self {
        self.argument = true;
        self.scope_bound = true;
        self
    }
    pub fn is_bound(&self) -> bool {
        self.scope_bound
    }
    pub fn as_typed(&self, ty: &Type) -> TypedValue {
        TypedValue { value: *self, ty: ty.clone() }
    }
}

#[derive(Clone, Debug)]
pub struct TypedValue {
    pub value: Value,
    pub ty: Type,
}

impl TypedValue {
    pub fn bind_scope(self) -> Self {
        Self { value: self.value.bind_scope(), ty: self.ty }
    }
    pub fn bind_arg(self) -> Self {
        Self { value: self.value.bind_arg(), ty: self.ty }
    }
}

impl ValueAllocator {
    pub fn new() -> Self {
        Self { index: Rc::new(Cell::new(1)), regs: Default::default() }
    }

    pub fn fork(&self) -> Self {
        todo!()
    }

    pub fn join(&mut self, other: Self) {}

    pub fn value_count(&self) -> usize {
        self.regs.iter().filter(|v| **v).count()
    }

    pub fn make(&mut self) -> Value {
        let idx = self.index.get();
        self.index.set(idx.checked_add(1).expect("Too much variables"));
        Value { index: idx, scope_bound: false, argument: false }
    }

    pub fn make_ty(&mut self, ty: &Type) -> TypedValue {
        TypedValue { value: self.make(), ty: ty.clone() }
    }

    pub fn kill(&mut self, value: Value) {
        if !value.scope_bound {
            self.kill_scoped(value);
        }
    }

    pub fn kill_scoped(&mut self, value: Value) {}
}
