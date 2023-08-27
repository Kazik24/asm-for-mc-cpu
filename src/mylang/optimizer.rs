use crate::mylang::ir::{ConstVal, IrOp, Label, Lowered, LoweredFunction};
use crate::mylang::regalloc::{UseKind, Value};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::rc::Rc;

#[derive(Copy, Clone)]
pub struct OptimizerOptions {
    /// Hard inline threshold, if function is called less or eq number of times as this value,
    /// then it is inlined unconditionally.
    pub max_inlines_t1: u32,
    /// If function has eq or less opcodes that that AND is called eq or less times than `max_inlines_t2`
    /// then it is inlined.
    pub max_inline_ir_ops: u32,
    /// If any function is called more times than that, do not inline it.
    pub max_inlines_t2: u32,
    /// Max number of iterations for which try to unroll loop
    pub max_loop_unroll: u32,
    /// Optimization passes count
    pub passes: u32,
}

impl OptimizerOptions {
    pub const SIZE: Self =
        Self { max_inlines_t1: 1, max_inline_ir_ops: 0, max_inlines_t2: 1, max_loop_unroll: 1, passes: 1 };
    pub const SPEED: Self =
        Self { max_inlines_t1: 4, max_inline_ir_ops: 32, max_inlines_t2: 16, max_loop_unroll: 16, passes: 1 };
}
impl OptimizerOptions {
    fn select_to_inline(&self, func: &[LoweredFunction]) -> HashSet<usize> {
        let func_counts = get_call_counts(func);
        func_counts
            .iter()
            .filter_map(|(&idx, &count)| {
                if count <= self.max_inlines_t1 {
                    return Some(idx);
                }
                let ir_len = func[idx].opcodes.iter().map(|op| match op {
                    IrOp::Label(_) | IrOp::Kill(_) => 0, //don't count this opcodes
                    _ => 1,
                });
                if count <= self.max_inlines_t2 && ir_len.sum::<u32>() <= self.max_inline_ir_ops {
                    Some(idx)
                } else {
                    None
                }
            })
            .collect()
    }
}

pub fn optimize_ir(lowered: &mut Lowered, options: OptimizerOptions) {
    for i in 0..options.passes {
        println!("Pass: {i}");
        for func in &mut lowered.functions {
            optimize_function(func);
            assert_optimized(&mut func.opcodes);
        }
        let to_inline = options.select_to_inline(&lowered.functions);
        let list = lowered.functions.clone();
        for func in lowered.functions.iter_mut() {
            inline_all(func, &list, &to_inline);
            println!("Inlined opcodes of {}:", func.name.value);
            for (i, v) in func.opcodes.iter().enumerate() {
                println!("{i} = {v:?}");
            }
        }
        println!("Post inline optimization");
        for func in &mut lowered.functions {
            optimize_function(func);
            assert_optimized(&mut func.opcodes);
        }
    }
    //todo garbage collect unused functions
}

fn assert_optimized(opcodes: &mut [IrOp]) {
    for (i, op) in opcodes.iter_mut().enumerate() {
        if !op.is_unary_op() && !op.is_binary_op() {
            if let (IrOp::JumpFalse(v, _) | IrOp::JumpTrue(v, _)) = op {
                if v.get_const().is_some() {
                    panic!("constant in jump [{i}]: {op:?}");
                }
            }
            continue;
        }
        let args = op.vals_mut().1;
        let len = args.len();
        let c = op.vals_mut().1.into_iter().filter(|v| v.get_const().is_some()).count();
        if c == len {
            panic!("all values are constant [{i}]: {op:?}");
        }
    }
}

fn get_call_counts(functions: &[LoweredFunction]) -> HashMap<usize, u32> {
    let mut calls = HashMap::new();
    let opcodes = functions.iter().flat_map(|v| v.opcodes.iter());
    for op in opcodes {
        if let IrOp::CallVoid(idx, _) | IrOp::CallValue(idx, _, _) = op {
            let count = calls.entry(*idx).or_insert(0);
            *count += 1;
        }
    }
    calls
}

fn optimize_function(func: &mut LoweredFunction) {
    ConstProp::propagate_consts(&mut func.opcodes);
    ValueInfo::reduce_dead_values(func);
    LabelInfo::reduce_dead_labels(&mut func.opcodes);
    ValueInfo::reduce_dead_values(func);
}

struct LabelInfo {
    jumps: Vec<usize>,
    place: usize,
}
impl LabelInfo {
    pub fn scan(opcodes: &[IrOp]) -> HashMap<Label, LabelInfo> {
        let mut labels = HashMap::new();
        for (i, op) in opcodes.iter().enumerate() {
            match op {
                IrOp::Goto(lab) | IrOp::JumpTrue(_, lab) | IrOp::JumpFalse(_, lab) => {
                    let label = labels.entry(*lab).or_insert(LabelInfo { jumps: Vec::new(), place: usize::MAX });
                    label.jumps.push(i);
                }
                IrOp::Label(lab) => {
                    let label = labels.entry(*lab).or_insert(LabelInfo { jumps: Vec::new(), place: usize::MAX });
                    assert_eq!(label.place, usize::MAX, "Duplicated label");
                    label.place = i;
                }
                _ => {}
            }
        }
        for info in labels.values() {
            assert_ne!(info.place, usize::MAX);
        }
        labels
    }

    /// reindex labels to have continuous indexes starting at offset, returns label count
    fn reindex_labels(opcodes: &mut [IrOp], offset: u32) -> u32 {
        use IrOp::*;
        let mut curr_index = offset;
        let labels = LabelInfo::scan(opcodes).into_iter().collect::<BTreeMap<_, _>>();
        for (label, info) in labels {
            let IrOp::Label(lab) = &mut opcodes[info.place] else { unreachable!() };
            lab.index = curr_index;
            for idx in info.jumps {
                let (Goto(v) | JumpFalse(_, v) | JumpTrue(_, v)) = &mut opcodes[idx] else { unreachable!() };
                v.index = curr_index;
            }
            curr_index += 1;
        }
        curr_index - offset
    }

    fn reduce_dead_labels(opcodes: &mut Vec<IrOp>) {
        use IrOp::*;
        Self::remove_dead_code(opcodes);

        //remove jumps immediately followed by it's label
        let mut remove_ops = BTreeSet::new();
        let labels = LabelInfo::scan(opcodes);
        let mut remap = HashMap::new();
        for (i, pair) in opcodes.windows(2).enumerate() {
            match pair {
                [Goto(a) | JumpTrue(_, a) | JumpFalse(_, a), IrOp::Label(b)] if a == b => {
                    remove_ops.insert(i); //remove jump always
                    if labels[a].jumps.len() == 1 {
                        remove_ops.insert(i + 1); //remove label if it was only jump
                    }
                }
                //case when label a can point immediately to b without indirection jump
                [IrOp::Label(a), IrOp::Goto(b)] if a != b => {
                    remove_ops.insert(i);
                    remap.insert(*a, *b);
                }
                _ => {}
            }
        }

        //remove lonely labels
        for (_, info) in labels {
            if info.jumps.is_empty() {
                remove_ops.insert(info.place);
            }
        }

        for rem in remove_ops.into_iter().rev() {
            opcodes.remove(rem);
        }
        while let Some(IrOp::Return) = opcodes.last() {
            opcodes.pop();
        }
        //remap jumps to different indexes
        for op in opcodes {
            if let (IrOp::Goto(old) | IrOp::JumpTrue(_, old) | IrOp::JumpFalse(_, old)) = op {
                if let Some(new) = remap.get(old) {
                    *old = *new
                }
            }
        }
    }

    fn remove_dead_code(opcodes: &mut Vec<IrOp>) {
        let labels = Self::scan(opcodes);
        let mut visited = HashSet::new();
        walk_opcodes(opcodes, &labels, (), |idx, op, _, jump| visited.insert(idx));
        for rem in (0..opcodes.len()).rev().filter(|v| !visited.contains(v)) {
            opcodes.remove(rem);
        }
    }
}

#[derive(Default, Debug)]
struct ValueInfo {
    reads: Vec<(usize, usize, bool)>,
    argument: bool,
    return_val: bool,
    kills: Vec<usize>,
    writes: Vec<usize>,
}

impl ValueInfo {
    pub fn scan(func: &mut LoweredFunction) -> HashMap<u32, Self> {
        let mut vals = HashMap::new();
        for arg in &func.args {
            vals.insert(
                arg.index,
                ValueInfo { argument: true, return_val: false, writes: vec![], kills: vec![], reads: vec![] },
            );
        }
        if let Some(ret) = &func.return_value {
            vals.insert(
                ret.value.index,
                ValueInfo { argument: false, return_val: true, reads: vec![], kills: vec![], writes: vec![] },
            );
        }
        for (i, op) in func.opcodes.iter_mut().enumerate() {
            if let IrOp::Kill(v) = op {
                vals.entry(v.index).or_insert(ValueInfo::default()).kills.push(i);
                continue;
            }
            let ops = format!("{op:?}");
            let (dst, src) = op.vals_mut();
            //println!("{dst:?}: {src:?} op: {ops}");
            if let Some(dst) = dst {
                vals.entry(dst.index).or_insert(ValueInfo::default()).writes.push(i);
            }
            for (j, src) in src.iter().enumerate() {
                if src.is_locked_const() {
                    continue;
                }
                let info = vals.entry(src.index).or_insert(ValueInfo::default());
                let is_const = src.get_const().is_some();
                info.reads.push((i, j, is_const));
            }
        }
        //remove entry for locked constants
        vals.remove(&u32::MAX);
        vals
    }

    pub fn any_const_read(&self) -> bool {
        self.reads.iter().any(|(_, _, c)| *c)
    }

    pub fn reindex_values(
        func: &mut LoweredFunction,
        offset: u32,
        args_ret: Option<(Vec<Value>, Option<Value>)>,
    ) -> u32 {
        fn replace_index(value: &mut Value, curr_index: &mut u32, changed: &mut BTreeMap<u32, Value>) {
            let index = changed.entry(value.index).or_insert_with(|| {
                let mut val = *value;
                val.index = *curr_index;
                *curr_index += 1;
                val
            });
            *value = *index;
        }

        let mut curr_index = offset;
        let values = ValueInfo::scan(func).into_iter().collect::<BTreeMap<_, _>>();
        let mut changed = BTreeMap::new();
        if let Some((new_args, new_ret)) = args_ret {
            assert_eq!(new_args.len(), func.args.len());
            assert_eq!(new_ret.is_some(), func.return_value.is_some());
            for (arg, new) in func.args.iter_mut().zip(new_args) {
                changed.insert(arg.index, new);
                *arg = new;
            }

            if let Some((ret, new)) = func.return_value.as_mut().zip(new_ret) {
                changed.insert(ret.value.index, new);
                ret.value = new;
            }
        } else {
            for arg in func.args.iter_mut() {
                let mut val = *arg;
                val.index = curr_index;
                changed.insert(arg.index, val);
                arg.index = curr_index;
                curr_index += 1;
            }

            if let Some(ret) = &mut func.return_value {
                replace_index(&mut ret.value, &mut curr_index, &mut changed);
            }
        }

        for (index, info) in values {
            for (op_idx, src_idx, _is_const) in &info.reads {
                let value = &mut func.opcodes[*op_idx].vals_mut().1[*src_idx];
                replace_index(value, &mut curr_index, &mut changed);
            }
            for dst_idx in &info.writes {
                let value = func.opcodes[*dst_idx].vals_mut().0.unwrap();
                replace_index(value, &mut curr_index, &mut changed);
            }
            for src_idx in &info.kills {
                let IrOp::Kill(value) = &mut func.opcodes[*src_idx] else { unreachable!() };
                replace_index(value, &mut curr_index, &mut changed);
            }
        }
        curr_index - offset
    }

    pub fn reduce_dead_values(func: &mut LoweredFunction) {
        let mut to_remove = BTreeSet::new();
        loop {
            let mut changed = false;
            let values = ValueInfo::scan(func);
            // println!("dead scan");
            // for (k, v) in values.iter().collect::<BTreeMap<_, _>>() {
            //     println!("{k} = {v:?}");
            // }

            to_remove.clear();
            for (index, info) in &values {
                if info.writes.is_empty() && !info.argument && info.kills.is_empty() {
                    panic!("Value never written {index} {info:?}");
                }
                if info.writes.is_empty() && !info.kills.is_empty() {
                    to_remove.extend(info.kills.iter().copied());
                }
                if info.reads.is_empty() && !info.return_val {
                    to_remove.extend(info.writes.iter().copied());
                }
            }

            //remove unnecesary copies
            for i in 0..func.opcodes.len() {
                if let IrOp::Copy(dst, src) = func.opcodes[i] {
                    let dst_info = values.get(&dst.index).unwrap();
                    if dst_info.writes.len() == 1 {
                        to_remove.insert(i);
                        for (idx, src_idx, _) in &dst_info.reads {
                            let value = &mut func.opcodes[*idx].vals_mut().1[*src_idx];
                            **value = src;
                            changed = true;
                        }
                    }
                }
            }

            for i in to_remove.iter().rev().copied() {
                func.opcodes.remove(i);
            }

            if !changed && to_remove.is_empty() {
                break;
            }
        }
    }

    fn annotate_lifetimes(func: &mut LoweredFunction) {
        let value_map = Self::scan(func);
        let labels = LabelInfo::scan(&func.opcodes);
        let init: Rc<HashMap<u32, ()>> = Default::default();
        walk_opcodes(&mut func.opcodes, &labels, init, |idx, op, state, prev_instr| {
            //todo add places where values are dropped
            for val in op.vals_mut().1 {
                if val.get_const().is_some() {
                    val.use_kind = UseKind::Drop;
                    continue;
                }
            }

            false
        })
    }
}

#[derive(Default, Clone)]
pub struct ConstProp {
    pub consts: Rc<HashMap<u32, Option<ConstVal>>>,
}

impl ConstProp {
    fn propagate_consts(opcodes: &mut Vec<IrOp>) {
        let mut consts = std::iter::repeat_with(|| None).take(opcodes.len()).collect::<Vec<_>>().into_boxed_slice();
        let mut backup = consts.clone();
        loop {
            ConstProp::propagate_once(opcodes, &mut consts);
            if backup == consts {
                break;
            }
            backup.copy_from_slice(&consts);
        }
        println!("Consts:");
        for (i, c) in consts.iter().enumerate() {
            println!("{i}: {c:?}");
        }

        Self::fill_constants(opcodes, &consts);
        Self::lock_const_reads(opcodes);
    }

    fn fill_constants(opcodes: &mut Vec<IrOp>, consts: &[Option<u16>]) {
        let mut to_remove = BTreeSet::new();
        for (i, op) in opcodes.iter_mut().enumerate() {
            let Some(v) = consts[i] else { continue };
            match *op {
                IrOp::JumpTrue(_, l) => {
                    println!("Fill jump true {i}");
                    if v & 1 != 0 {
                        *op = IrOp::Goto(l);
                    } else {
                        to_remove.insert(i);
                    }
                }
                IrOp::JumpFalse(_, l) => {
                    println!("Fill jump false {i}");
                    if v & 1 == 0 {
                        *op = IrOp::Goto(l);
                    } else {
                        to_remove.insert(i);
                    }
                }
                _ => {
                    let place = op.vals_mut().0.expect("This opcode should have dst value");
                    *op = IrOp::Const(*place, v);
                }
                _ => {}
            }
        }
        for rem in to_remove.into_iter().rev() {
            opcodes.remove(rem);
        }
    }

    fn lock_const_reads(opcodes: &mut [IrOp]) {
        for val in opcodes.iter_mut().flat_map(|v| v.vals_mut().1) {
            val.set_lock_if_const();
        }
    }

    fn propagate_once(opcodes: &mut Vec<IrOp>, x: &mut [Option<u16>]) {
        let labels = LabelInfo::scan(opcodes);
        let mut label_states = HashMap::new();
        walk_opcodes(opcodes, &labels, ConstProp::default(), |idx, op, state, jump| {
            op.set_const_states(state);
            match *op {
                IrOp::Const(v, val) => _ = state.consts().insert(v.index, Some(val)),
                IrOp::JumpTrue(v, _) | IrOp::JumpFalse(v, _) => {
                    if let Some(value) = state.consts.get(&v.index).copied().flatten() {
                        x[idx] = Some(value);
                    } else {
                        x[idx] = None;
                    }
                }
                IrOp::Label(lab) => match label_states.get_mut(&lab) {
                    None => {
                        let info =
                            LabelState { prev_consts: ConstProp::clone(state), jump_source: HashSet::from([jump]) };
                        label_states.insert(lab, info);
                    }
                    Some(info) if !info.jump_source.contains(&jump) => {
                        info.jump_source.insert(jump);
                        if info.prev_consts.consts != state.consts {
                            for (curr_key, curr_vals) in state.consts.iter() {
                                if let Some(prev_vals) = info.prev_consts.consts.get(curr_key) {
                                    //if states are different (they contain different values, or one is None which means many values)
                                    if curr_vals != prev_vals {
                                        //set state to None - indicates many possible values
                                        info.prev_consts.consts().insert(*curr_key, None);
                                    }
                                    //else no action is required
                                } else {
                                    //update to new constant's state
                                    info.prev_consts.consts().insert(*curr_key, *curr_vals);
                                }
                            }
                            *state = info.prev_consts.clone();
                        }
                    }
                    _ => return false,
                },

                _ => {
                    let result = op.propagate_const(state);
                    x[idx] = result;
                    if let Some(val) = op.vals_mut().0 {
                        match (state.consts.get(&val.index), result) {
                            (Some(Some(c)), Some(r)) if *c != r => _ = state.consts().insert(val.index, None), //different consts
                            (Some(Some(_)), Some(_)) => {} //same consts
                            (Some(Some(_)), None) => _ = state.consts().insert(val.index, None), //result not const
                            (Some(None), _) => {}          //multiple values already in dst
                            (None, Some(r)) => _ = state.consts().insert(val.index, Some(r)), //first const
                            (None, None) => {}
                        }
                    }
                }
            }
            true
        });
    }

    fn consts(&mut self) -> &mut HashMap<u32, Option<u16>> {
        Rc::make_mut(&mut self.consts)
    }
    pub fn get(&self, value: Value) -> Option<ConstVal> {
        if let Some(v) = value.get_const() {
            return Some(v);
        }
        self.consts.get(&value.index).copied().flatten()
    }
}

#[derive(Default)]
struct LabelState {
    prev_consts: ConstProp,
    jump_source: HashSet<usize>,
}

fn walk_opcodes<T: Clone>(
    opcodes: &mut [IrOp],
    labels: &HashMap<Label, LabelInfo>,
    init: T,
    mut func: impl FnMut(usize, &mut IrOp, &mut T, usize) -> bool,
) {
    let mut stack = Vec::new();
    let len = opcodes.len();
    if !opcodes.is_empty() {
        stack.push((0, init, 0));
    }
    while let Some((index, mut value, jump_from)) = stack.pop() {
        let opcode = &mut opcodes[index];

        if !func(index, opcode, &mut value, jump_from) {
            continue;
        }

        //take the jump
        match opcode {
            IrOp::Return => continue,
            IrOp::JumpTrue(_, lab) | IrOp::JumpFalse(_, lab) | IrOp::Goto(lab) => {
                let info = labels.get(&lab).expect("Jump to unknown label");
                assert!(info.place < len);
                stack.push((info.place, value.clone(), index));
            }
            _ => {}
        }
        //for JumpTrue and JumpFalse, also push next opcode and split execution into two paths
        if !matches!(opcode, IrOp::Goto(_)) {
            let next = index + 1; //next opcode
            if next < len {
                stack.push((next, value, index));
            }
        }
    }
}

fn inline_all(func: &mut LoweredFunction, list: &[LoweredFunction], inlines: &HashSet<usize>) -> bool {
    let label_count = LabelInfo::reindex_labels(&mut func.opcodes, 0);
    let value_count = ValueInfo::reindex_values(func, 0, None);
    println!("label_count: {label_count}, value_count: {value_count}");

    let calls_to_inline = func.opcodes.iter().enumerate().filter_map(|(i, op)| match op {
        IrOp::CallVoid(idx, args) if inlines.contains(idx) => Some((i, &list[*idx], None, args.clone())),
        IrOp::CallValue(idx, ret, args) if inlines.contains(idx) => Some((i, &list[*idx], Some(*ret), args.clone())),
        _ => None,
    });
    let calls_to_inline = calls_to_inline.collect::<Vec<_>>();

    let mut label_offset = label_count;
    let mut value_offset = value_count;
    for (index, to_inline, ret, args) in calls_to_inline.into_iter().rev() {
        let mut to_inline = to_inline.clone();
        //reindex all labels and values to not collide with current function's ones
        label_offset += LabelInfo::reindex_labels(&mut to_inline.opcodes, label_offset);
        value_offset += ValueInfo::reindex_values(&mut to_inline, value_offset, Some((args, ret)));

        //replace return opcodes with jumps to label at the end of functions
        let return_label = Label { index: label_offset };
        label_offset += 1;
        for v in to_inline.opcodes.iter_mut() {
            if matches!(v, IrOp::Return) {
                *v = IrOp::Goto(return_label);
            }
        }
        to_inline.opcodes.push(IrOp::Label(return_label));

        //insert function's opcodes in place of a call
        func.opcodes.splice(index..=index, to_inline.opcodes);
    }

    //2. reindex values
    //

    false
}

//todo value tree to compare if given values can be optimized out because they are the same
enum ValueTree {
    Const(ConstVal),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    BranchOf { cond: Box<Self>, on_true: Box<Self>, on_false: Box<Self> },
}

impl ValueTree {
    // check if both values would be the same
    pub fn is_equivalent(&self, other: &Self) -> bool {
        todo!()
    }

    //calculate const exprs and refactor math operation order to still be equivalent but more optimized.
    pub fn optimize_reduce(&self) {
        todo!()
    }
}
