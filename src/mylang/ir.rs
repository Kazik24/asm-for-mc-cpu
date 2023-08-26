use crate::mylang::ast::{ConstDef, Expression, FuncDef, GlobalDef, Identifier, Item, Statement, Type};
use crate::mylang::optimizer::ConstProp;
use crate::mylang::regalloc::{TypedValue, UseKind, Value, ValueAllocator, ValueKind};
use crate::mylang::LoweringError;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::rc::Rc;

pub type ConstVal = u16;

#[derive(Clone, Debug)]
pub enum IrOp {
    Const(Value, ConstVal),    //dst, src
    Add(Value, Value, Value),  //dst, left, right
    Sub(Value, Value, Value),  //dst, left, right
    Mul(Value, Value, Value),  //dst, left, right
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
    CallVoid(usize, Vec<Value>),
    CallValue(usize, Value, Vec<Value>),

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
            Add(p, a, b) | Sub(p, a, b) | Mul(p, a, b) | Shr(p, a, b) | Shl(p, a, b) => (Some(p), vec![a, b]),
            Ashr(p, a, b) | And(p, a, b) | Or(p, a, b) | Xor(p, a, b) => (Some(p), vec![a, b]),
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
            Mul(_, a, b) => Some(consts.get(a)?.wrapping_mul(consts.get(b)?)),
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
            Mul(..) => Some(a.wrapping_mul(b)),
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
    pub functions: Vec<LoweredFunction>,
    pub consts: Vec<Initializer>,
    pub symbols: SymbolTable,
}
#[derive(Debug)]
pub struct SymbolTable {
    pub consts: HashMap<String, usize>,
    pub functions: HashMap<String, usize>,
    pub globals: HashMap<String, usize>,
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
            functions: functions.iter().enumerate().map(|(i, v)| (v.name.value.clone(), i)).collect(),
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
            functions: Vec::new(),
            symbols: table,
        };
        this.lower_constants(&ast)?;
        for func in &ast.functions {
            let f = LoweredFunction::lower(func, &this.symbols, &this.consts, &ast.functions)?;
            this.functions.push(f);
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
            let last = ctx.opcodes.last().ok_or(vec![LoweringError::FunctionMustReturnValue(ast.name.span)])?;
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

                Self::visit_block(block, ctx)?;
                if let Some(els) = els {
                    let end_label = ctx.next_label();
                    ctx.opcodes.push(IrOp::Goto(end_label));
                    ctx.opcodes.push(IrOp::Label(false_lab));
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
                while ctx.scope.len() > max_scope {
                    ctx.drop_scope();
                }
                ctx.opcodes.push(IrOp::Goto(end));
            }
            Statement::Continue(span) => {
                let &(start, _, max_scope) = ctx.loop_stack.last().ok_or(LoweringError::ContinueOutsideLoop(*span))?;
                while ctx.scope.len() > max_scope {
                    ctx.drop_scope();
                }
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
                if !p.ty.type_eq(&Type::Ptr(Box::new(val.ty.clone()))) {
                    return Err(LoweringError::TypesDontMatch(p.ty.span(), val.ty.span()));
                }
                ctx.opcodes.push(IrOp::PtrStore(p.value, val.value));
                ctx.kill(p.value);
                ctx.kill(val.value);
            }
            Expression::Index(array, index) => {
                let val = Self::visit_expr(value, ctx, None)?;
                let arr = Self::visit_expr(array, ctx, None)?;
                let Type::Ptr(pt) = arr.ty else {
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
            Expression::Mul(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Mul),
            Expression::Shr(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Shr),
            Expression::AriShr(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Ashr),
            Expression::Shl(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Shl),
            Expression::And(a, b) => Self::binary_op(a, b, ctx, place, IrOp::And),
            Expression::Or(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Or),
            Expression::Xor(a, b) => Self::binary_op(a, b, ctx, place, IrOp::Xor),
            Expression::Eq(a, b) => Self::bin_eq(a, b, ctx, place, |_, d, a, b| IrOp::CmpEq(d, a, b)),
            Expression::Ne(a, b) => Self::bin_eq(a, b, ctx, place, |_, d, a, b| IrOp::CmpNe(d, a, b)),
            Expression::Ge(a, b) => {
                Self::bin_eq(
                    a,
                    b,
                    ctx,
                    place,
                    |s, d, a, b| if s { IrOp::CmpGes(d, a, b) } else { IrOp::CmpGe(d, a, b) },
                )
            }
            Expression::Gr(a, b) => {
                Self::bin_eq(
                    a,
                    b,
                    ctx,
                    place,
                    |s, d, a, b| if s { IrOp::CmpLt(d, b, a) } else { IrOp::CmpLts(d, b, a) },
                )
            }
            Expression::Lo(a, b) => {
                Self::bin_eq(
                    a,
                    b,
                    ctx,
                    place,
                    |s, d, a, b| if s { IrOp::CmpLt(d, a, b) } else { IrOp::CmpLts(d, a, b) },
                )
            }
            Expression::Le(a, b) => {
                Self::bin_eq(
                    a,
                    b,
                    ctx,
                    place,
                    |s, d, a, b| if s { IrOp::CmpGes(d, b, a) } else { IrOp::CmpGe(d, b, a) },
                )
            }
            Expression::Pointer(e) => {
                let p = Self::visit_expr(e, ctx, None)?;
                let Type::Ptr(pt) = p.ty else {
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
                let Type::Ptr(pt) = val.ty else {
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
            _ => todo!(),
        }
    }

    fn begin_call(
        ctx: &mut FuncCtx,
        name: &Identifier,
        args_expr: &[Expression],
    ) -> Result<(usize, Vec<Value>, Option<Type>), LoweringError> {
        let func_idx = *ctx.symbols.functions.get(&name.value).ok_or(LoweringError::FunctionNotFound(name.span))?;
        let args = args_expr.iter().map(|expr| Self::visit_expr(expr, ctx, None));
        let args = args.collect::<Result<Vec<_>, _>>()?;
        let func = &ctx.func_defs[func_idx];
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
        ctx.opcodes.push(func(place.value, a.value, b.value));
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
    use crate::mylang::ast::*;
    use crate::mylang::codegen::{generate_code, CodegenOptions};
    use crate::mylang::ir::{Lowered, SymbolTable};
    use crate::mylang::optimizer::{optimize_ir, OptimizerOptions};
    use crate::mylang::preproc::Span;
    use std::time::Instant;
    use std::vec;
    use Expression::*;

    const S: Span = Span::mocked();
    const U16: Type = Type::U16(S);

    #[track_caller]
    fn ptr_ty() -> Type {
        Type::Ptr(Box::new(Type::U16(Span::mocked_caller())))
    }
    #[track_caller]
    fn mock_name(s: &str) -> Identifier {
        Identifier { value: s.to_string(), span: Span::mocked_caller() }
    }
    fn mock_arg(s: &str, ty: Type) -> Argument {
        Argument { name: mock_name(s), ty }
    }

    fn num(val: i64) -> Box<Expression> {
        Box::new(Number(val, false, S))
    }
    fn var(s: &str) -> Box<Expression> {
        Box::new(Name(mock_name(s)))
    }

    #[test]
    fn test_lowering() {
        let ast = vec![
            Item::Const(ConstDef { name: mock_name("C1"), ty: U16, init: Box::new(Add(num(4), var("C2"))) }),
            Item::Const(ConstDef { name: mock_name("C2"), ty: U16, init: Box::new(Add(num(5), num(11))) }),
            Item::Func(FuncDef {
                name: mock_name("op_func"),
                ret: Some(U16),
                args: vec![mock_arg("a", U16), mock_arg("b", U16)],
                inline: false,
                stt: vec![
                    Statement::If {
                        cond: Box::new(Lo(var("a"), var("b"))), //todo type inferred here is signed
                        els: None,
                        block: vec![Statement::Return(S, Some(num(42)))],
                    },
                    Statement::Return(S, Some(Box::new(Sub(var("a"), var("b"))))),
                ],
            }),
            Item::Func(FuncDef {
                name: mock_name("test_func"),
                ret: None,
                args: vec![mock_arg("arg1", U16)],
                inline: false,
                stt: vec![
                    Statement::Var { name: mock_name("count"), ty: U16, expr: Box::new(Add(num(0), num(1))) },
                    Statement::Var { name: mock_name("cond"), ty: U16, expr: (num(6) + num(4)) - num(10) },
                    Statement::While {
                        cond: Box::new(Ne(var("count"), var("cond"))),
                        block: vec![Statement::Assign(
                            var("count"),
                            Box::new(Call(mock_name("op_func"), vec![*var("count"), *num(1)])), //todo const arguments don't optimize jumps
                        )],
                    },
                ],
            }),
            Item::Func(FuncDef {
                name: mock_name("memcpy"),
                ret: None,
                args: vec![mock_arg("src", ptr_ty()), mock_arg("src_end", ptr_ty()), mock_arg("dst", ptr_ty())],
                inline: false,
                stt: vec![Statement::While {
                    cond: Box::new(Expression::Ne(var("src"), var("src_end"))),
                    block: vec![
                        Statement::Var {
                            name: mock_name("temp"),
                            ty: Type::U16(Span::mocked_caller()),
                            expr: Box::new(Expression::Pointer(var("src"))),
                        },
                        Statement::Assign(Box::new(Expression::Pointer(var("dst"))), var("temp")),
                        Statement::Assign(var("src"), Box::new(Call(mock_name("ptr_add"), vec![*var("src"), *num(1)]))),
                        Statement::Assign(var("dst"), Box::new(Call(mock_name("ptr_add"), vec![*var("dst"), *num(1)]))),
                    ],
                }],
            }),
            Item::Func(FuncDef {
                name: mock_name("ptr_add"),
                ret: Some(ptr_ty()),
                args: vec![mock_arg("ptr", ptr_ty()), mock_arg("count", U16)],
                inline: false,
                stt: vec![Statement::Return(S, Some(var("ptr") + Box::new(Expression::Cast(ptr_ty(), num(1)))))],
            }),
        ];

        let (table, ast) = SymbolTable::scan_symbols(ast.iter()).unwrap();
        let mut lower = Lowered::lower_all(table, &ast).unwrap();
        let index = 2;
        let before_opt = lower.functions[index].opcodes.clone();
        for (i, op) in before_opt.iter().enumerate() {
            println!("{i}: {:?}", op);
        }
        println!("****** Optimize");
        let start = Instant::now();
        optimize_ir(&mut lower, OptimizerOptions::SPEED);
        println!("****** After opt (took: {:.3?})", start.elapsed());
        for (i, op) in lower.functions[index].opcodes.iter().enumerate() {
            println!("{i}: {:?}", op);
        }

        println!("**** Generated code:");
        let code = generate_code(
            &lower,
            CodegenOptions { stack_reg: 14, link_reg: 13, pc_reg: 15, temp_reg: 12, zero_reg: 0 },
        );
    }
}
