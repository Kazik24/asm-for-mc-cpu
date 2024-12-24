use crate::mylang::ast::{ConstDef, Expression, FuncDef, GlobalDef, Identifier, Item, Statement, Type};
use crate::mylang::optimizer::ConstProp;
use crate::mylang::regalloc::{TypedValue, UseKind, Value, ValueAllocator, ValueKind};
use crate::mylang::LoweringError;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::ops::{Not, Range};
use std::rc::Rc;

pub type ConstVal = u16;
pub const MAIN_FUNC_NAME: &str = "main";

#[derive(Clone, Debug)]
pub enum IrOp {
    Const(Value, ConstVal),    //dst, src
    Add(Value, Value, Value),  //dst, left, right
    Sub(Value, Value, Value),  //dst, left, right
    UMul(Value, Value, Value), //dst, left, right
    IMul(Value, Value, Value), //dst, left, right
    Shl(Value, Value, Value),  //dst, left, right
    Shr(Value, Value, Value),  //dst, left, right
    Ashr(Value, Value, Value), //dst, left, right
    And(Value, Value, Value),  //dst, left, right
    Or(Value, Value, Value),   //dst, left, right
    Xor(Value, Value, Value),  //dst, left, right

    CmpLt(Value, Value, Value),  //dst_bool, left, right
    CmpGe(Value, Value, Value),  //dst_bool, left, right
    CmpLts(Value, Value, Value), //dst_bool, left, right
    CmpGes(Value, Value, Value), //dst_bool, left, right
    CmpEq(Value, Value, Value),  //dst_bool, left, right
    CmpNe(Value, Value, Value),  //dst_bool, left, right

    JumpFalse(Value, Label), //bool, target
    JumpTrue(Value, Label),  //bool, target, can be replaced with negation of value and JumpFalse
    Goto(Label),
    Return,
    Label(Label),
    Kill(Value), //drop value register
    CallVoid(RefIdx, Vec<Value>),
    CallValue(RefIdx, Value, Vec<Value>),

    Not(Value, Value),        //dst, src
    Neg(Value, Value),        //dst, src
    WordToByte(Value, Value), //dst, src
    HiToByte(Value, Value),   //dst, src
    ByteToWord(Value, Value), //dst, src
    ByteExtend(Value, Value), //dst, src
    Copy(Value, Value),       //dst, src
    PtrLoad(Value, Value),    //dst, src_address, volatile operation
    PtrStore(Value, Value),   //dst_address, src, volatile operation
}

impl IrOp {
    pub fn vals_mut(&mut self) -> (Option<&mut Value>, Vec<&mut Value>) {
        use IrOp::*;
        match self {
            Const(p, _) => (Some(p), vec![]),
            Add(p, a, b) | Sub(p, a, b) | UMul(p, a, b) | IMul(p, a, b) | Shr(p, a, b) => (Some(p), vec![a, b]),
            Shl(p, a, b) | Ashr(p, a, b) | And(p, a, b) | Or(p, a, b) | Xor(p, a, b) => (Some(p), vec![a, b]),
            CmpLt(p, a, b) | CmpGe(p, a, b) | CmpLts(p, a, b) | CmpGes(p, a, b) => (Some(p), vec![a, b]),
            CmpEq(p, a, b) | CmpNe(p, a, b) => (Some(p), vec![a, b]),
            JumpFalse(v, _) | JumpTrue(v, _) => (None, vec![v]),
            Goto(_) | Return | Label(_) | Kill(_) => (None, vec![]),
            CallVoid(_, v) => (None, v.iter_mut().collect()),
            CallValue(_, r, v) => (Some(r), v.iter_mut().collect()),
            Not(p, v) | Neg(p, v) | WordToByte(p, v) | ByteToWord(p, v) | ByteExtend(p, v) => (Some(p), vec![v]),
            HiToByte(p, v) => (Some(p), vec![v]),
            Copy(p, v) | PtrLoad(p, v) => (Some(p), vec![v]),
            PtrStore(d, v) => (None, vec![d, v]),
        }
    }

    pub fn propagate_const(&self, consts: &ConstProp) -> Option<ConstVal> {
        use IrOp::*;
        match *self {
            Add(_, a, b) => Some(consts.get(a)?.wrapping_add(consts.get(b)?)),
            Sub(_, a, b) => Some(consts.get(a)?.wrapping_sub(consts.get(b)?)),
            UMul(_, a, b) => Some(consts.get(a)?.wrapping_mul(consts.get(b)?)),
            IMul(_, a, b) => Some((consts.get(a)? as i16).wrapping_mul((consts.get(b)? as i16)) as _),
            Shl(_, a, b) => Some(consts.get(a)?.wrapping_shl(consts.get(b)? as _)),
            Shr(_, a, b) => Some(consts.get(a)?.wrapping_shr(consts.get(b)? as _)),
            Ashr(_, a, b) => Some((consts.get(a)? as i16).wrapping_shr(consts.get(b)? as _) as u16),
            And(_, a, b) => Some(consts.get(a)? & consts.get(b)?),
            Or(_, a, b) => Some(consts.get(a)? | consts.get(b)?),
            Xor(_, a, b) => Some(consts.get(a)? ^ consts.get(b)?),

            CmpLt(_, a, b) => Some((consts.get(a)? < consts.get(b)?) as _),
            CmpGe(_, a, b) => Some((consts.get(a)? >= consts.get(b)?) as _),
            CmpLts(_, a, b) => Some(((consts.get(a)? as i16) < (consts.get(b)? as i16)) as _),
            CmpGes(_, a, b) => Some(((consts.get(a)? as i16) >= (consts.get(b)? as i16)) as _),
            CmpEq(_, a, b) => Some((consts.get(a)? == consts.get(b)?) as _),
            CmpNe(_, a, b) => Some((consts.get(a)? != consts.get(b)?) as _),

            Not(_, v) => Some(!consts.get(v)?),
            Neg(_, v) => Some(consts.get(v)?.wrapping_neg()),
            WordToByte(_, v) => Some(consts.get(v)? & 0xff),
            HiToByte(_, v) => Some(consts.get(v)? >> 8),
            ByteToWord(_, v) => Some(consts.get(v)? & 0xff),
            ByteExtend(_, v) => Some(((consts.get(v)? as i8) as i16) as u16),
            Copy(_, v) => consts.get(v),
            _ => None,
        }
    }

    pub fn set_const_states(&mut self, consts: &ConstProp) {
        for val in self.vals_mut().1 {
            if val.is_locked_const() {
                continue;
            }
            if let Some(Some(c)) = consts.consts.get(&val.index) {
                val.set_const(*c);
            } else {
                val.unset_const();
            }
        }
    }

    pub fn binary_op(&self, a: ConstVal, b: ConstVal) -> Option<ConstVal> {
        use IrOp::*;
        match self {
            Add(..) => Some(a.wrapping_add(b)),
            Sub(..) => Some(a.wrapping_sub(b)),
            UMul(..) => Some(a.wrapping_mul(b)),
            IMul(..) => Some((a as i16).wrapping_mul(b as i16) as _),
            Shl(..) => Some(a.wrapping_shl(b as _)),
            Shr(..) => Some(a.wrapping_shr(b as _)),
            Ashr(..) => Some((a as i16).wrapping_shr(b as _) as u16),
            And(..) => Some(a & b),
            Or(..) => Some(a | b),
            Xor(..) => Some(a ^ b),

            CmpLt(..) => Some((a < b) as _),
            CmpGe(..) => Some((a >= b) as _),
            CmpLts(..) => Some(((a as i16) < (b as i16)) as _),
            CmpGes(..) => Some(((a as i16) >= (b as i16)) as _),
            CmpEq(..) => Some((a == b) as _),
            CmpNe(..) => Some((a != b) as _),
            _ => None,
        }
    }
    pub fn is_binary_op(&self) -> bool {
        self.binary_op(0, 0).is_some()
    }
    pub fn is_unary_op(&self) -> bool {
        self.unary_op(0).is_some()
    }

    pub fn unary_op(&self, v: ConstVal) -> Option<ConstVal> {
        use IrOp::*;
        match self {
            Not(..) => Some(!v),
            Neg(..) => Some(v.wrapping_neg()),
            WordToByte(..) => Some(v & 0xff),
            HiToByte(..) => Some(v >> 8),
            ByteToWord(..) => Some(v & 0xff),
            ByteExtend(..) => Some(((v as i8) as i16) as u16),
            Copy(..) => Some(v),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Label {
    pub index: u32,
}
impl Debug for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "label[{}]", self.index)
    }
}
#[derive(Debug, Clone)]
pub struct LoweredFunction {
    pub name: Identifier,
    pub inline: bool,
    pub types: Vec<Type>,
    pub args: Vec<Value>,
    pub return_value: Option<TypedValue>,
    pub opcodes: Vec<IrOp>,
}
#[derive(Debug)]
pub struct Initializer {
    //ast: ConstDef,
    name: Identifier,
    ty: Type,
    value: InitValue,
}
#[derive(Clone, Debug)]
pub enum InitValue {
    NotResolved,
    Const(ConstVal),
    AddressTo(Rc<[ConstVal]>, Range<usize>),
}
impl InitValue {
    pub fn get_const(&self) -> Option<ConstVal> {
        match self {
            Self::Const(v) => Some(*v),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Lowered {
    pub functions: HashMap<RefIdx, LoweredFunction>,
    pub main_function: RefIdx,
    pub consts: Vec<Initializer>,
    pub symbols: SymbolTable,
}
#[derive(Debug)]
pub struct SymbolTable {
    pub consts: HashMap<String, usize>,
    pub functions: HashMap<String, RefIdx>,
    pub globals: HashMap<String, usize>,
}
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct RefIdx(u32);
impl From<usize> for RefIdx {
    fn from(value: usize) -> Self {
        RefIdx(value.try_into().expect("cannot cast usize to u32"))
    }
}

pub struct RefAst {
    consts: Vec<ConstDef>,
    functions: Vec<FuncDef>,
    globals: Vec<GlobalDef>,
}

impl SymbolTable {
    pub fn scan_symbols<'a>(items: impl IntoIterator<Item = &'a Item>) -> Result<(Self, RefAst), Vec<LoweringError>> {
        let mut names: HashMap<String, Identifier> = HashMap::new();
        let mut consts = Vec::new();
        let mut functions = Vec::new();
        let mut globals = Vec::new();
        let mut errors = Vec::new();
        for item in items {
            let name = match item {
                Item::Const(constant) => {
                    consts.push(constant.clone());
                    &constant.name
                }
                Item::Func(func) => {
                    functions.push(func.clone());
                    &func.name
                }
                Item::Global(glob) => {
                    globals.push(glob.clone());
                    &glob.name
                }
            };
            if let Some(prev) = names.get(name.value.as_str()) {
                errors.push(LoweringError::DuplicatedItem(prev.span, name.span));
            } else {
                names.insert(name.value.clone(), name.clone());
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        let table = Self {
            functions: functions.iter().enumerate().map(|(i, v)| (v.name.value.clone(), RefIdx(i as u32))).collect(),
            consts: consts.iter().enumerate().map(|(i, v)| (v.name.value.clone(), i)).collect(),
            globals: globals.iter().enumerate().map(|(i, v)| (v.name.value.clone(), i)).collect(),
        };
        let ast = RefAst { consts, functions, globals };
        Ok((table, ast))
    }
}

impl Lowered {
    pub fn lower_all(table: SymbolTable, ast: &RefAst) -> Result<Self, Vec<LoweringError>> {
        let mut this = Self {
            consts: ast
                .consts
                .iter()
                .map(|v| Initializer { name: v.name.clone(), ty: v.ty.clone(), value: InitValue::NotResolved })
                .collect(),
            functions: HashMap::new(),
            symbols: table,
            main_function: RefIdx(0),
        };
        this.lower_constants(&ast)?;
        for (index, func) in ast.functions.iter().enumerate() {
            let f = LoweredFunction::lower(func, &this.symbols, &this.consts, &ast.functions)?;
            this.functions.insert(index.into(), f);
        }
        let main = this.symbols.functions.get(MAIN_FUNC_NAME);
        this.main_function = *main.ok_or_else(|| vec![LoweringError::MainFunctionNotFound])?;
        let mf = &this.functions[&this.main_function];
        if !mf.args.is_empty() {
            return Err(vec![LoweringError::MainShouldHaveNoArgs(mf.name.span)]);
        }
        Ok(this)
    }

    /// recursively resolve all constants
    fn lower_constants(&mut self, ast: &RefAst) -> Result<(), Vec<LoweringError>> {
        let mut errors = Vec::new();
        let mut values_table = HashMap::new();
        loop {
            let mut any_resolved = false;
            let resolved = values_table.keys().copied().collect::<HashSet<_>>();
            let consts = self.consts.iter_mut().zip(ast.consts.iter());
            for (i, (c, a)) in consts.enumerate().filter(|(i, _)| !resolved.contains(i)) {
                match c.try_lower(a, &self.symbols, &values_table) {
                    Ok(()) => {
                        any_resolved = true;
                        let val = c.value.get_const().expect("Logic error, value should be resolved");
                        values_table.insert(i, (val, c.ty.clone()));
                    }
                    Err(Some(e)) => {
                        errors.push(e);
                    }
                    Err(None) => {}
                }
            }
            if !errors.is_empty() {
                return Err(errors);
            }
            if !any_resolved {
                break;
            }
        }
        let unresolved = self
            .consts
            .iter()
            .filter(|v| v.value.get_const().is_none())
            .map(|v| LoweringError::CannotResolveConst(v.name.span))
            .collect::<Vec<_>>();
        if !unresolved.is_empty() {
            return Err(unresolved);
        }
        Ok(())
    }

    fn garbage_collect_functions(&mut self) {}
}

impl LoweredFunction {
    fn lower(
        ast: &FuncDef,
        symbols: &SymbolTable,
        consts: &[Initializer],
        func: &[FuncDef],
    ) -> Result<Self, Vec<LoweringError>> {
        let mut opcodes = Vec::new();
        let mut vals = ValueAllocator::new();
        let mut first_scope = Vec::with_capacity(ast.args.len());
        let args = ast.args.iter().map(|arg| {
            let value = vals.make_ty(&arg.ty).bind_arg();
            let val = value.value;
            first_scope.push((arg.name.clone(), value));
            val
        });
        let args = args.collect::<Vec<_>>();

        let mut ctx = FuncCtx {
            symbols,
            vals,
            func_defs: func,
            opcodes: &mut opcodes,
            consts,
            scope: vec![first_scope],
            label_idx: 0,
            loop_stack: Vec::new(),
            expected_return: ast.ret.clone(),
            return_val: None,
        };

        Self::visit_block(&ast.stt, &mut ctx).map_err(|v| vec![v])?;

        if let Some(ast_ret) = &ast.ret {
            if ctx.return_val.is_none() {
                return Err(vec![LoweringError::FunctionMustReturnValue(ast.name.span)]);
            }
            let last = ctx.opcodes.iter().rev().filter(|v| !matches!(v, IrOp::Kill(_))).next();
            let last = last.ok_or(vec![LoweringError::FunctionMustReturnValue(ast.name.span)])?;
            match last {
                IrOp::Goto(_) => {}
                IrOp::Return => {}
                _ => return Err(vec![LoweringError::FunctionMustReturnValue(ast.name.span)]),
            }
        } else {
            assert!(ctx.return_val.is_none(), "Function must not return value");
        }
        ctx.drop_scope();
        let return_value = ctx.return_val.map(|v| v.as_typed(ctx.expected_return.as_ref().unwrap()));
        Ok(Self {
            return_value,
            args,
            opcodes,
            inline: ast.inline,
            name: ast.name.clone(),
            types: ast.args.iter().map(|v| v.ty.clone()).collect(),
        })
    }

    fn visit_stat(stat: &Statement, ctx: &mut FuncCtx) -> Result<(), LoweringError> {
        match stat {
            Statement::Var { name, ty, expr } => {
                let value = ctx.make_ty(&ty).bind_scope();
                Self::visit_expr(expr, ctx, Some(value.clone()))?;
                let scope = ctx.scope.last_mut().expect("No scopes?");
                scope.push((name.clone(), value));
            }
            Statement::Expr(expr) => {
                if let Expression::Call(name, args) = &**expr {
                    let (func_idx, val_args, ret) = Self::begin_call(ctx, name, args)?;
                    //todo inline functions
                    if let Some(ret) = ret {
                        let place = ctx.make();
                        ctx.opcodes.push(IrOp::CallValue(func_idx, place, val_args.clone()));
                        ctx.kill(place);
                    } else {
                        //void return type
                        ctx.opcodes.push(IrOp::CallVoid(func_idx, val_args.clone()));
                    }
                    for a in val_args.into_iter().rev() {
                        ctx.kill(a);
                    }
                } else {
                    let value = Self::visit_expr(expr, ctx, None)?;
                    ctx.kill(value.value);
                }
            }
            Statement::Block(stt) => Self::visit_block(stt, ctx)?,
            Statement::If { cond, block, els } => {
                let cond_val = Self::visit_expr(cond, ctx, None)?;
                let false_lab = ctx.next_label();
                ctx.opcodes.push(IrOp::JumpFalse(cond_val.value, false_lab));
                ctx.kill(cond_val.value);

                println!("If Scope: {:?}", ctx.scope.iter().map(|v| v.len()).collect::<Vec<_>>());
                Self::visit_block(block, ctx)?;
                println!("If Scope end: {:?}", ctx.scope.iter().map(|v| v.len()).collect::<Vec<_>>());
                if let Some(els) = els {
                    let end_label = ctx.next_label();
                    ctx.opcodes.push(IrOp::Goto(end_label));
                    ctx.opcodes.push(IrOp::Label(false_lab));
                    println!("Else Scope: {:?}", ctx.scope.iter().map(|v| v.len()).collect::<Vec<_>>());
                    Self::visit_block(els, ctx)?;
                    ctx.opcodes.push(IrOp::Label(end_label));
                } else {
                    ctx.opcodes.push(IrOp::Label(false_lab));
                }
            }
            Statement::While { cond, block } => {
                let start = ctx.next_label();
                let end = ctx.next_label();
                ctx.opcodes.push(IrOp::Label(start));
                let cond_val = Self::visit_expr(cond, ctx, None)?;
                ctx.opcodes.push(IrOp::JumpFalse(cond_val.value, end));
                ctx.kill(cond_val.value);

                let current_scope = ctx.scope.len();
                ctx.loop_stack.push((start, end, current_scope));
                Self::visit_block(block, ctx)?;
                ctx.loop_stack.pop();
                ctx.opcodes.push(IrOp::Goto(start));
                ctx.opcodes.push(IrOp::Label(end));
            }

            Statement::DoWhile { cond, block } => {
                let start = ctx.next_label();
                let end = ctx.next_label();
                ctx.opcodes.push(IrOp::Label(start));
                let current_scope = ctx.scope.len();
                ctx.loop_stack.push((start, end, current_scope));
                let op_idx = ctx.opcodes.len();
                Self::visit_block(block, ctx)?;
                ctx.loop_stack.pop();
                //check if any break of this loop exist
                let mut is_break = false;
                for op in &ctx.opcodes[op_idx..] {
                    match op {
                        IrOp::Label(lab) if *lab == end => is_break = true,
                        _ => {}
                    }
                }
                //loop condition
                let cond_val = Self::visit_expr(cond, ctx, None)?;
                ctx.opcodes.push(IrOp::JumpTrue(cond_val.value, start));
                ctx.kill(cond_val.value);
                if is_break {
                    ctx.opcodes.push(IrOp::Label(end));
                }
            }
            Statement::LoopForever(block) => {
                let start = ctx.next_label();
                let end = ctx.next_label();
                ctx.opcodes.push(IrOp::Label(start));
                let current_scope = ctx.scope.len();
                ctx.loop_stack.push((start, end, current_scope));
                Self::visit_block(block, ctx)?;
                ctx.loop_stack.pop();
                ctx.opcodes.push(IrOp::Goto(start));
                ctx.opcodes.push(IrOp::Label(end));
            }
            Statement::Break(span) => {
                let &(_, end, max_scope) = ctx.loop_stack.last().ok_or(LoweringError::BreakOutsideLoop(*span))?;
                ctx.rewind_scope_until(max_scope);
                ctx.opcodes.push(IrOp::Goto(end));
            }
            Statement::Continue(span) => {
                let &(start, _, max_scope) = ctx.loop_stack.last().ok_or(LoweringError::ContinueOutsideLoop(*span))?;
                ctx.rewind_scope_until(max_scope);
                ctx.opcodes.push(IrOp::Goto(start));
            }
            Statement::Assign(place, value) => Self::visit_assign(place, ctx, value)?,
            Statement::Return(kw, expr) => {
                if expr.is_some() != ctx.expected_return.is_some() {
                    return Err(if ctx.expected_return.is_none() {
                        LoweringError::FunctionReturnsVoid(*kw)
                    } else {
                        LoweringError::FunctionReturnsValue(*kw)
                    });
                }
                if let Some((expr, ty)) = expr.as_ref().zip(ctx.expected_return.clone()) {
                    //value return
                    if let Some(ret) = ctx.return_val {
                        let ret = ret.as_typed(&ty);
                        Self::visit_expr(expr, ctx, Some(ret))?;
                    } else {
                        let mut place = ctx.make_ty(&ty);
                        place.value.kind = ValueKind::Ret;
                        ctx.return_val = Some(place.value);
                        Self::visit_expr(expr, ctx, Some(place))?;
                    }
                    ctx.opcodes.push(IrOp::Return);
                } else {
                    //void return
                    assert!(ctx.return_val.is_none());
                    ctx.opcodes.push(IrOp::Return);
                }
            }
        }
        Ok(())
    }

    fn visit_assign(ass: &Expression, ctx: &mut FuncCtx, value: &Expression) -> Result<(), LoweringError> {
        match ass {
            Expression::Name(name) => {
                if let Some(var) = ctx.get_var(&name.value) {
                    Self::visit_expr(value, ctx, Some(var))?;
                } else if let Some(c) = ctx.get_const(&name.value) {
                    return Err(LoweringError::CannotAssignToConst(name.span));
                } else {
                    return Err(LoweringError::VariableOrConstNotFound(name.span));
                }
            }
            Expression::Pointer(expr) => {
                let val = Self::visit_expr(value, ctx, None)?;
                if !val.ty.is_word_sized() {
                    return Err(LoweringError::ExpectedWordSizedType(val.ty.span()));
                }
                let p = Self::visit_expr(expr, ctx, None)?;
                if !p.ty.type_eq_ptr(&val.ty) {
                    return Err(LoweringError::TypesDontMatch(p.ty.span(), val.ty.span()));
                }
                ctx.opcodes.push(IrOp::PtrStore(p.value, val.value));
                ctx.kill(p.value);
                ctx.kill(val.value);
            }
            Expression::Index(array, index) => {
                let val = Self::visit_expr(value, ctx, None)?;
                let arr = Self::visit_expr(array, ctx, None)?;
                let Type::Ptr(_, pt) = arr.ty else {
                    return Err(LoweringError::ExpectedPointerType(array.span()));
                };
                if !val.ty.type_eq(&pt) {
                    return Err(LoweringError::TypesDontMatch(val.ty.span(), pt.span()));
                }
                let idx = Self::visit_expr(index, ctx, None)?;
                if !matches!(idx.ty, Type::U16(_)) {
                    return Err(LoweringError::ExpectedUintType(index.span()));
                }
                let one = ctx.make();
                ctx.opcodes.push(IrOp::Const(one, 1));
                ctx.opcodes.push(IrOp::Shl(idx.value, idx.value, one));
                ctx.kill(one);
                ctx.opcodes.push(IrOp::Add(arr.value, arr.value, idx.value));
                ctx.kill(idx.value);
                ctx.opcodes.push(IrOp::PtrStore(arr.value, val.value));
                ctx.kill(arr.value);
                ctx.kill(val.value);
            }
            _ => return Err(LoweringError::UnexpectedPlaceExpression(ass.span())),
        }
        Ok(())
    }

    fn visit_block(block: &[Statement], ctx: &mut FuncCtx) -> Result<(), LoweringError> {
        ctx.new_scope();
        for s in block {
            Self::visit_stat(s, ctx)?;
        }
        ctx.drop_scope();
        Ok(())
    }

    fn visit_expr(
        expr: &Expression,
        ctx: &mut FuncCtx,
        place: Option<TypedValue>,
    ) -> Result<TypedValue, LoweringError> {
        use IrOp::*;
        match expr {
            Expression::Number(v, sign, n) => {
                let val = u16::try_from(*v)
                    .or_else(|_| i16::try_from(*v).map(|v| v as u16))
                    .map_err(|_| LoweringError::NumberTooLarge(*n))?;
                let ty = if *sign { Type::I16(*n) } else { Type::U16(*n) };
                let place = place.unwrap_or_else(|| ctx.make_ty(&ty));
                ctx.opcodes.push(IrOp::Const(place.value, val));
                Ok(place)
            }
            Expression::Name(ident) => {
                //try find variable
                if let Some(var) = ctx.get_var(&ident.value) {
                    match place {
                        None => Ok(var),
                        Some(place) => {
                            ctx.opcodes.push(IrOp::Copy(place.value, var.value));
                            Ok(place)
                        }
                    }
                } else if let Some(cons) = ctx.get_const(&ident.value) {
                    let place = place.unwrap_or_else(|| ctx.make_ty(&cons.1));
                    ctx.opcodes.push(IrOp::Const(place.value, cons.0));
                    Ok(place)
                } else {
                    Err(LoweringError::VariableOrConstNotFound(ident.span))
                }
            }
            Expression::Paren(expr) => Self::visit_expr(expr, ctx, place),
            Expression::Add(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Add),
            Expression::Sub(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Sub),
            Expression::Mul(a, b) => Self::typed_binary_op(a, b, ctx, place, |a, b, c, t| {
                if !t.is_number() {
                    return Err(LoweringError::ExpectedNumberType(t.span()));
                }
                if t.signed() {
                    Ok(IrOp::IMul(a, b, c))
                } else {
                    Ok(IrOp::UMul(a, b, c))
                }
            }),
            Expression::Shr(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Shr),
            Expression::AriShr(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Ashr),
            Expression::Shl(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Shl),
            Expression::And(a, b) => Self::binary_op(a, b, ctx, place, IrOp::And),
            Expression::Or(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Or),
            Expression::Xor(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Xor),
            Expression::Eq(a, b) => Self::bin_eq(a, b, ctx, place, |_, d, a, b| IrOp::CmpEq(d, a, b)),
            Expression::Ne(a, b) => Self::bin_eq(a, b, ctx, place, |_, d, a, b| IrOp::CmpNe(d, a, b)),
            Expression::Ge(a, b) => {
                Self::bin_eq(a, b, ctx, place, |s, d, a, b| if s { CmpGes(d, a, b) } else { CmpGe(d, a, b) })
            }
            Expression::Gr(a, b) => {
                Self::bin_eq(a, b, ctx, place, |s, d, a, b| if s { CmpLt(d, b, a) } else { CmpLts(d, b, a) })
            }
            Expression::Lo(a, b) => {
                Self::bin_eq(a, b, ctx, place, |s, d, a, b| if s { CmpLt(d, a, b) } else { CmpLts(d, a, b) })
            }
            Expression::Le(a, b) => {
                Self::bin_eq(a, b, ctx, place, |s, d, a, b| if s { CmpGes(d, b, a) } else { CmpGe(d, b, a) })
            }
            Expression::Pointer(e) => {
                let p = Self::visit_expr(e, ctx, None)?;
                let Type::Ptr(_, pt) = p.ty else {
                    return Err(LoweringError::ExpectedPointerType(e.span()));
                };
                let place = place.unwrap_or_else(|| ctx.make_ty(&pt));
                if !place.ty.type_eq(&pt) {
                    return Err(LoweringError::ExpectingOtherType(place.ty.span()));
                }
                ctx.opcodes.push(IrOp::PtrLoad(place.value, p.value));
                ctx.kill(p.value);
                Ok(place)
            }
            Expression::Index(array, index) => {
                let val = Self::visit_expr(array, ctx, None)?;
                let Type::Ptr(_, pt) = val.ty else {
                    return Err(LoweringError::ExpectedPointerType(array.span()));
                };
                let idx = Self::visit_expr(index, ctx, None)?;
                if !matches!(idx.ty, Type::U16(_)) {
                    return Err(LoweringError::ExpectedUintType(index.span()));
                }
                let one = ctx.make();
                ctx.opcodes.push(IrOp::Const(one, 1));
                ctx.opcodes.push(IrOp::Shl(idx.value, idx.value, one));
                ctx.kill(one);
                ctx.opcodes.push(IrOp::Add(val.value, val.value, idx.value));
                ctx.kill(idx.value);
                let place = place.unwrap_or_else(|| ctx.make_ty(&pt));
                if !place.ty.type_eq(&pt) {
                    return Err(LoweringError::ExpectingOtherType(place.ty.span()));
                }
                ctx.opcodes.push(IrOp::PtrLoad(place.value, val.value));
                ctx.kill(val.value);
                Ok(place)
            }
            Expression::Cast(ty, expr) => {
                let val = Self::visit_expr(expr, ctx, None)?;
                let place = place.unwrap_or_else(|| ctx.make_ty(&ty));
                let vs = val.ty.size_of();
                let ps = ty.size_of();
                let op = match (val.ty.signed(), ty.signed()) {
                    (false, false) | (true, true) if vs == ps => IrOp::Copy(place.value, val.value),
                    (false, false) if vs > ps => IrOp::WordToByte(place.value, val.value),
                    (false, false) => IrOp::ByteToWord(place.value, val.value),
                    (true, true) if vs > ps => IrOp::ByteExtend(place.value, val.value),
                    (true, true) => IrOp::ByteExtend(place.value, val.value),
                    (false, true) => IrOp::ByteExtend(place.value, val.value),
                    (true, false) if vs > ps => IrOp::ByteToWord(place.value, val.value),
                    (true, false) if vs == ps && vs == 2 => IrOp::Copy(place.value, val.value),
                    //todo
                    _ => IrOp::Copy(place.value, val.value),
                };
                ctx.opcodes.push(op);
                ctx.kill(val.value);
                Ok(place)
            }
            Expression::Not(expr) => {
                let val = Self::visit_expr(expr, ctx, None)?;
                let place = place.unwrap_or_else(|| ctx.make_ty(&val.ty));
                ctx.opcodes.push(IrOp::Not(place.value, val.value));
                ctx.kill(val.value);
                Ok(place)
            }
            Expression::Neg(expr) => {
                let val = Self::visit_expr(expr, ctx, None)?;
                let place = place.unwrap_or_else(|| ctx.make_ty(&val.ty));
                ctx.opcodes.push(IrOp::Neg(place.value, val.value));
                ctx.kill(val.value);
                Ok(place)
            }
            Expression::Call(name, args_expr) => {
                let (func_idx, val_args, ret) = Self::begin_call(ctx, name, args_expr)?;
                //todo inline functions
                //return value
                if let Some(ret) = ret {
                    let place = place.unwrap_or_else(|| ctx.make_ty(&ret));
                    ctx.opcodes.push(IrOp::CallValue(func_idx, place.value, val_args.clone()));
                    for a in val_args.into_iter().rev() {
                        ctx.kill(a);
                    }
                    Ok(place)
                } else {
                    //void return type in expression
                    return Err(LoweringError::VoidReturnType(name.span));
                }
            }
            Expression::String(span, text) => {
                todo!("Strings are not supported now")
            }
            Expression::Character(span, text) => {
                todo!("Chars are not supported now")
            }
            Expression::ArrayInit(span, ex) => {
                todo!("Array initialization is not supported now")
            }
            Expression::AccessName(expr, name) => {
                todo!("Dot operators are not supported now")
            }
        }
    }

    fn begin_call(
        ctx: &mut FuncCtx,
        name: &Identifier,
        args_expr: &[Expression],
    ) -> Result<(RefIdx, Vec<Value>, Option<Type>), LoweringError> {
        let func_idx = *ctx.symbols.functions.get(&name.value).ok_or(LoweringError::FunctionNotFound(name.span))?;
        let args = args_expr.iter().map(|expr| Self::visit_expr(expr, ctx, None));
        let args = args.collect::<Result<Vec<_>, _>>()?;
        let func = &ctx.func_defs[func_idx.0 as usize];
        //check arg types
        for (i, (val, ast)) in args.iter().zip(&func.args).enumerate() {
            if !val.ty.type_eq(&ast.ty) {
                let expr = args_expr[i].span();
                return Err(LoweringError::ArgTypeDontMatch(expr, ast.ty.span()));
            }
        }

        let val_args = args.into_iter().map(|v| v.value).collect::<Vec<_>>();
        Ok((func_idx, val_args, func.ret.clone()))
    }

    fn binary_op(
        a: &Expression,
        b: &Expression,
        ctx: &mut FuncCtx,
        place: Option<TypedValue>,
        func: impl FnOnce(Value, Value, Value) -> IrOp,
    ) -> Result<TypedValue, LoweringError> {
        Self::typed_binary_op(a, b, ctx, place, |a, b, c, _| Ok(func(a, b, c)))
    }
    fn typed_binary_op(
        a: &Expression,
        b: &Expression,
        ctx: &mut FuncCtx,
        place: Option<TypedValue>,
        func: impl FnOnce(Value, Value, Value, &Type) -> Result<IrOp, LoweringError>,
    ) -> Result<TypedValue, LoweringError> {
        let a = Self::visit_expr(a, ctx, None)?;
        let b = Self::visit_expr(b, ctx, None)?;
        if !a.ty.type_eq(&b.ty) {
            return Err(LoweringError::TypesDontMatch(a.ty.span(), b.ty.span()));
        }
        //todo type of whole expr
        let ty = a.ty;
        let place = place.unwrap_or_else(|| ctx.make_ty(&ty));
        if !place.ty.type_eq(&ty) {
            return Err(LoweringError::TypesDontMatch(ty.span(), place.ty.span()));
        }
        ctx.opcodes.push(func(place.value, a.value, b.value, &ty)?);
        ctx.kill(a.value);
        ctx.kill(b.value);
        Ok(place)
    }
    fn bin_eq(
        a: &Expression,
        b: &Expression,
        ctx: &mut FuncCtx,
        place: Option<TypedValue>,
        func: impl FnOnce(bool, Value, Value, Value) -> IrOp,
    ) -> Result<TypedValue, LoweringError> {
        let a = Self::visit_expr(a, ctx, None)?;
        let b = Self::visit_expr(b, ctx, None)?;
        if !a.ty.type_eq(&b.ty) {
            return Err(LoweringError::TypesDontMatch(a.ty.span(), b.ty.span()));
        }
        //todo type of whole expr
        let ty = a.ty;
        let place = place.unwrap_or_else(|| ctx.make_ty(&ty));
        if !place.ty.type_eq(&ty) {
            return Err(LoweringError::TypesDontMatch(ty.span(), place.ty.span()));
        }
        ctx.opcodes.push(func(ty.signed(), place.value, a.value, b.value));
        ctx.kill(a.value);
        ctx.kill(b.value);
        Ok(place)
    }
}

struct FuncCtx<'a> {
    symbols: &'a SymbolTable,
    func_defs: &'a [FuncDef],
    vals: ValueAllocator,
    opcodes: &'a mut Vec<IrOp>,
    consts: &'a [Initializer],
    scope: Vec<Vec<(Identifier, TypedValue)>>,
    label_idx: u32,
    loop_stack: Vec<(Label, Label, usize)>,
    expected_return: Option<Type>,
    return_val: Option<Value>,
}

impl FuncCtx<'_> {
    pub fn get_var(&self, name: &str) -> Option<TypedValue> {
        let var = self.scope.iter().rev().flat_map(|s| s.iter().rev()).find(|(ident, _)| ident.value == name);
        var.map(|(_, var)| var.clone())
    }
    pub fn new_scope(&mut self) {
        self.scope.push(Vec::new());
    }
    pub fn next_label(&mut self) -> Label {
        let val = self.label_idx;
        self.label_idx += 1;
        Label { index: val }
    }
    pub fn drop_scope(&mut self) {
        let scope = self.scope.pop().expect("Scope stack is empty");
        for (_, mut val) in scope.into_iter().rev() {
            self.vals.kill_scoped(val.value); //kills scope bound values
            val.value.use_kind = UseKind::Drop;
            self.opcodes.push(IrOp::Kill(val.value));
        }
    }
    pub fn drop_scope_until(&mut self, max_scope: usize) {
        while self.scope.len() > max_scope {
            self.drop_scope();
        }
    }
    pub fn rewind_scope_until(&mut self, max_scope: usize) {
        for scope in self.scope[max_scope..].iter().rev() {
            for (_, mut val) in scope.iter().cloned().rev() {
                self.vals.kill_scoped(val.value); //kills scope bound values
                val.value.use_kind = UseKind::Drop;
                self.opcodes.push(IrOp::Kill(val.value));
            }
        }
    }
    pub fn get_const(&self, name: &str) -> Option<(ConstVal, Type)> {
        self.consts
            .iter()
            .find(|c| c.name.value == name)
            .map(|c| (c.value.get_const().expect("Consts should be initialized ath this stage"), c.ty.clone()))
    }
    pub fn make(&mut self) -> Value {
        self.vals.make()
    }
    pub fn make_ty(&mut self, ty: &Type) -> TypedValue {
        TypedValue { value: self.make(), ty: ty.clone() }
    }
    pub fn kill(&mut self, value: Value) {
        self.vals.kill(value);
    }
}

impl Initializer {
    pub fn try_lower(
        &mut self,
        ast: &ConstDef,
        symbols: &SymbolTable,
        values: &HashMap<usize, (ConstVal, Type)>,
    ) -> Result<(), Option<LoweringError>> {
        let (value, typ) = Self::visit_expr(&ast.init, symbols, values)?;
        if !typ.type_eq(&self.ty) {
            return Err(Some(LoweringError::TypesDontMatch(typ.span(), self.ty.span())));
        }
        self.value = InitValue::Const(value);
        Ok(())
    }

    fn visit_expr(
        expr: &Expression,
        s: &SymbolTable,
        v: &HashMap<usize, (ConstVal, Type)>,
    ) -> Result<(ConstVal, Type), Option<LoweringError>> {
        match expr {
            Expression::Paren(e) => Ok(Self::visit_expr(&*e, s, v)?),
            Expression::Not(e) => {
                let (a, t) = Self::visit_expr(&*e, s, v)?;
                Ok((!a, t))
            }
            Expression::Neg(e) => {
                let (a, t) = Self::visit_expr(&*e, s, v)?;
                Ok((a.wrapping_neg(), t))
            }
            Expression::Add(a, b) => Self::const_bin_op(a, b, s, v, |a, b| a.wrapping_add(b)),
            Expression::Sub(a, b) => Self::const_bin_op(a, b, s, v, |a, b| a.wrapping_sub(b)),
            Expression::Mul(a, b) => Self::const_bin_op(a, b, s, v, |a, b| a.wrapping_mul(b)),
            Expression::Number(v, sign, n) => {
                let val = u16::try_from(*v)
                    .or_else(|_| i16::try_from(*v).map(|v| v as u16))
                    .map_err(|_| Some(LoweringError::NumberTooLarge(*n)))?;
                let ty = if *sign { Type::I16(*n) } else { Type::U16(*n) };
                Ok((val, ty))
            }
            Expression::Name(var) => {
                let var = s.consts.get(&var.value).ok_or(LoweringError::VariableOrConstNotFound(var.span))?;
                match v.get(var) {
                    Some(var) => Ok(var.clone()), //resolve value
                    None => Err(None),            //unresolved yet
                }
            }
            _ => todo!(),
        }
    }

    fn const_bin_op(
        a: &Expression,
        b: &Expression,
        s: &SymbolTable,
        v: &HashMap<usize, (ConstVal, Type)>,
        func: impl FnOnce(ConstVal, ConstVal) -> ConstVal,
    ) -> Result<(ConstVal, Type), Option<LoweringError>> {
        let (a, at) = Self::visit_expr(a, s, v)?;
        let (b, bt) = Self::visit_expr(b, s, v)?;
        if !at.type_eq(&bt) {
            return Err(Some(LoweringError::TypesDontMatch(at.span(), bt.span())));
        }
        //todo type of whole expression
        Ok((func(a, b), at))
    }
}

#[cfg(test)]
mod tests {
    use crate::mylang::ast::Statement::{Var, While};
    use crate::mylang::ast::*;
    use crate::mylang::codegen::{generate_code, CodegenOptions};
    use crate::mylang::ir::{Lowered, RefIdx, SymbolTable};
    use crate::mylang::optimizer::{optimize_ir, OptimizerOptions};
    use crate::mylang::preproc::{Source, Span};
    use crate::mylang::{Compiler, MapSourceLoader};
    use std::time::Instant;
    use std::vec;
    use Expression::*;

    #[test]
    fn test_lowering() {
        let text = r#"
            const C1: u16 = 4 + C2;
            const C2: u16 = 5 + 11;

            fn main(){
                op_func(1,2);
                op_func(1,2);
                op_func(1,2);
                op_func(1,2);
                test_func(3);
                test_func(3);
                test_func(3);
                test_func(3);
                memcpy(0 as *u16, 1 as *u16,1 as *u16);
                memcpy(0 as *u16, 1 as *u16,1 as *u16);
                memcpy(0 as *u16, 1 as *u16,1 as *u16);
                memcpy(0 as *u16, 1 as *u16,1 as *u16);
            }

            fn op_func(a: u16, b: u16) -> u16 {
                if a < b {
                    return 42;
                }
                return a - b;
            }

            fn test_func(arg1: u16){
                var count: u16 = 0 + 1;
                var cond: u16 = 6 + 4 - 10;
                while count != cond {
                    count = op_func(count, 1); //todo const arguments don't optimize jumps
                }
            }

            fn memcpy(src: *u16, src_end: *u16, dst: *u16){
                while src != src_end {
                    var temp: u16 = *src;
                    *dst = temp;
                    src = ptr_add(src, 1);
                    dst = ptr_add(dst, 1);
                }
            }

            fn ptr_add(ptr: *u16, count: u16)-> *u16 {
                return ptr + count as *u16;
            }

            fn collatz(ptr: *u16, stop: u16) {
                var n: u16 = *ptr;
                while n != stop {
                    ptr = ptr_add(ptr, 2);
                    if n & 1 == 0 {
                        n = n >> 1;
                    }else{
                        n = n * 2 + n + 1;
                    }
                    *ptr = n;
                }
            }

            fn assigns(val: u16) -> u16{
                var a: u16 = val;
                var b: u16 = a;
                var c: u16 = b;
                var d: u16 = c;
                return d;
            }

            fn shift(result: *u16,a: *u16, b:u16){
                *result = *a << b;
            }

            fn bubble_sort(array: *u16, len: u16) {
                var i: u16 = 1;
                while i < len {
                    var key: u16 = array[i];
                    var j: u16 = i - 1;

                    while (j >= 0) & (array[j] > key) {
                        array[j + 1] = array[j];
                        j = j - 1;
                    }
                    array[j + 1] = key;
                    i = i + 1;
                }
            }

            fn insertion_sort(start: *u16, end: *u16){
                var i: *u16 = ptr_add(start, 2);
                while i < end {
                    var key: u16 = *i;
                    var j: *u16 = i - 2 as *u16;
                    i = ptr_add(i, 2);

                    loop {
                        var curr: u16 = *j;
                        if curr <= key {
                            *ptr_add(j, 2) = curr;
                            j = j - 2 as *u16;
                            if end > j {
                                continue;
                            }
                        }
                        j = ptr_add(j,2);
                        *j = key;
                    }
                }
            }

            fn shift_by_sth(a: u16) -> u16 {
                return a * 4;
            }

            fn fibonacci(n: u16)->u16{
                if n < 2{
                    return n;
                }
                return fibonacci(n - 1) + fibonacci(n - 2);
            }

        "#;
        let loader = MapSourceLoader([("main".to_string(), text.to_string())].into_iter().collect());
        let ast = parse_ast("main", Box::new(loader.clone())).0.unwrap();

        let (table, ast) = SymbolTable::scan_symbols(ast.iter()).unwrap();
        let mut lower = Lowered::lower_all(table, &ast).unwrap();
        let index = RefIdx(3);
        let before_opt = lower.functions[&index].opcodes.clone();
        for (i, op) in before_opt.iter().enumerate() {
            println!("{i}: {:?}", op);
        }
        println!("****** Optimize");
        let start = Instant::now();
        optimize_ir(&mut lower, OptimizerOptions::SPEED);
        println!("****** After opt (took: {:.3?}) ({} functions)", start.elapsed(), lower.functions.len());
        for (i, op) in lower.functions[&index].opcodes.iter().enumerate() {
            println!("{i}: {:?}", op);
        }

        println!("**** Generated code:");
        let code = generate_code(&lower, CodegenOptions::BEDROCK_CORE_CODEGEN);
    }
}
