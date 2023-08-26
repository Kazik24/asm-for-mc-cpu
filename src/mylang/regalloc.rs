use crate::mylang::ast::{Item, Type};
use crate::mylang::ir::ConstVal;
use std::fmt::{Debug, Formatter};

pub struct ValueAllocator {
    index: u32,
}

#[derive(Copy, Clone, PartialEq)]
pub struct Value {
    pub index: u32,
    pub kind: ValueKind,
    pub use_kind: UseKind,
    is_const: bool,
    const_val: ConstVal,
}
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ValueKind {
    Tmp,
    Var,
    Arg,
    Ret,
}
//todo encode usage in values, useful for register allocations
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UseKind {
    None,         //regular usage of variable, either read or write
    Create,       //first use of value, register should be allocated for it
    Drop,         //value was read last time, associated register can be dropped
    UselessWrite, // value was written last time and never used again (this write can be removed)
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = self.get_const() {
            return write!(f, "const({c})");
        }
        match self.use_kind {
            UseKind::None => {}
            UseKind::Drop => write!(f, "drop_")?,
            UseKind::Create => write!(f, "new_")?,
            UseKind::UselessWrite => write!(f, "useless_")?,
        }
        match self.kind {
            ValueKind::Tmp => write!(f, "tmp")?,
            ValueKind::Var => write!(f, "var")?,
            ValueKind::Arg => write!(f, "arg")?,
            ValueKind::Ret => write!(f, "ret")?,
        }
        write!(f, "[{}]", self.index)
    }
}

impl Value {
    pub fn zero_const() -> Self {
        Value { const_val: 0, is_const: true, kind: ValueKind::Tmp, use_kind: UseKind::None, index: u32::MAX }
    }
    pub fn new_index(index: u32) -> Self {
        Self { index, kind: ValueKind::Tmp, const_val: 0, use_kind: UseKind::None, is_const: false }
    }
    pub fn bind_scope(mut self) -> Self {
        self.kind = ValueKind::Var;
        self
    }
    pub fn bind_arg(mut self) -> Self {
        self.kind = ValueKind::Arg;
        self
    }
    pub fn set_const(&mut self, value: ConstVal) {
        self.is_const = true;
        self.const_val = value;
    }
    pub fn unset_const(&mut self) {
        self.is_const = false;
    }
    pub fn is_locked_const(&self) -> bool {
        self.is_const && self.index == u32::MAX
    }
    pub fn set_lock_if_const(&mut self) {
        if self.get_const().is_some() {
            self.index = u32::MAX;
        }
    }
    pub fn get_const(&self) -> Option<ConstVal> {
        self.is_const.then_some(self.const_val)
    }
    pub fn is_bound(&self) -> bool {
        matches!(self.kind, ValueKind::Var | ValueKind::Arg)
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
        Self { index: 1 }
    }
    pub fn make(&mut self) -> Value {
        let idx = self.index;
        self.index = idx.checked_add(1).expect("Too much variables");
        Value::new_index(idx)
    }
    pub fn make_ty(&mut self, ty: &Type) -> TypedValue {
        TypedValue { value: self.make(), ty: ty.clone() }
    }
    pub fn kill(&mut self, value: Value) {
        if !value.is_bound() {
            self.kill_scoped(value);
        }
    }
    pub fn kill_scoped(&mut self, value: Value) {}
}

pub struct RegAlloc {
    allocated: Vec<RegState>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum RegState {
    Empty,
    Occupied(u32),
    Temp,
    Reserved,
}

#[derive(Copy, Clone, PartialEq)]
pub struct Reg {
    pub num: u16,
}

impl Debug for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.num)
    }
}

impl RegAlloc {
    pub fn new(exclude: impl IntoIterator<Item = u32>) -> Self {
        let mut regs = exclude.into_iter().collect::<Vec<_>>();
        regs.sort_unstable();
        let allocated = if regs.is_empty() {
            Vec::new()
        } else {
            let mut vec = vec![RegState::Empty; *regs.last().unwrap() as usize + 1];
            for i in regs {
                vec[i as usize] = RegState::Reserved;
            }
            vec
        };
        Self { allocated }
    }

    pub fn get_reg(&mut self, value: Value) -> Reg {
        if let Some(pos) = self.allocated.iter().position(|v| *v == RegState::Empty) {
            self.allocated[pos] = RegState::Occupied(value.index);
            Reg { num: pos as _ }
        } else {
            let pos = self.allocated.len();
            self.allocated.push(RegState::Occupied(value.index));
            Reg { num: pos as _ }
        }
    }

    pub fn get_temp_reg(&mut self) -> Reg {
        if let Some(pos) = self.allocated.iter().position(|v| *v == RegState::Empty) {
            self.allocated[pos] = RegState::Temp;
            Reg { num: pos as _ }
        } else {
            let pos = self.allocated.len();
            self.allocated.push(RegState::Temp);
            Reg { num: pos as _ }
        }
    }

    pub fn drop_value_reg(&mut self, value: Value) {
        let pos = self
            .allocated
            .iter()
            .position(|v| *v == RegState::Occupied(value.index))
            .expect("Register for this value is not allocated");
        self.allocated[pos] = RegState::Empty;
    }

    pub fn drop_reg(&mut self, reg: Reg) {
        let slot = self.allocated.get_mut(reg.num as usize).expect("Error: cannot drop unallocated register");
        assert!(matches!(*slot, RegState::Occupied(_) | RegState::Temp), "Error: cannot drop unallocated register");
        *slot = RegState::Empty;
    }
}
