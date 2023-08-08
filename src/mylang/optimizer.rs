use crate::mylang::ir::{IrOp, Label, Lowered, LoweredFunction};
use crate::mylang::regalloc::Value;
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::TryInto;
use std::rc::Rc;

pub struct OptimizerOptions {
    /// Hard inline threshold, if function is called less or eq number of times as this value,
    /// then it is inlined unconditionally.
    max_inlines_t1: u32,
    /// If function has eq or less opcodes that that AND is called eq or less times than `max_inlines_t2`
    /// then it is inlined.
    max_inline_ir_ops: u32,
    /// If any function is called more times than that, do not inline it.
    max_inlines_t2: u32,
    /// Max number of iterations for which try to unroll loop
    max_loop_unroll: u32,
    /// Optimization passes count
    passes: u32,
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
                let ir_len = func[idx].opcodes.len() as u32;
                if count <= self.max_inlines_t1 {
                    Some(idx)
                } else if count <= self.max_inlines_t2 && ir_len <= self.max_inline_ir_ops {
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
        }
        let to_inline = options.select_to_inline(&lowered.functions);
        for func in &mut lowered.functions {
            inline_all(func, &to_inline);
        }
        for func in &mut lowered.functions {
            optimize_function(func);
        }
    }
    //todo garbage collect unused functions
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
        for (i, pair) in opcodes.windows(2).enumerate() {
            match pair {
                [Goto(a) | JumpTrue(_, a) | JumpFalse(_, a), IrOp::Label(b)] if a == b => {
                    remove_ops.insert(i); //remove jump always
                    if labels[a].jumps.len() == 1 {
                        remove_ops.insert(i + 1); //remove label if it was only jump
                    }
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
    reads: Vec<(usize, usize)>,
    argument: bool,
    return_val: bool,
    writes: Vec<usize>,
}

impl ValueInfo {
    pub fn scan(func: &mut LoweredFunction) -> HashMap<u32, Self> {
        let mut vals = HashMap::new();
        for arg in &func.args {
            vals.insert(
                arg.index,
                ValueInfo { argument: true, return_val: false, writes: Vec::new(), reads: Vec::new() },
            );
        }
        if let Some(ret) = &func.return_value {
            vals.insert(
                ret.value.index,
                ValueInfo { argument: false, return_val: true, reads: Vec::new(), writes: Vec::new() },
            );
        }
        for (i, op) in func.opcodes.iter_mut().enumerate() {
            let ops = format!("{op:?}");
            let (dst, src) = op.vals_mut();
            //println!("{dst:?}: {src:?} op: {ops}");
            if let Some(dst) = dst {
                let info = vals.entry(dst.index).or_insert(ValueInfo::default());
                info.writes.push(i);
            }
            for (j, src) in src.iter().enumerate() {
                let info = vals.entry(src.index).or_insert(ValueInfo::default());
                info.reads.push((i, j));
            }
        }
        vals
    }

    pub fn reindex_values(func: &mut LoweredFunction, offset: u32) -> u32 {
        use IrOp::*;
        let mut curr_index = offset;
        let values = ValueInfo::scan(func).into_iter().collect::<BTreeMap<_, _>>();
        let mut changed = BTreeMap::new();
        for arg in func.args.iter_mut() {
            changed.insert(arg.index, curr_index);
            arg.index = curr_index;
            curr_index += 1;
        }

        for (index, info) in values {
            for (op_idx, src_idx) in &info.reads {
                let value = &mut func.opcodes[*op_idx].vals_mut().1[*src_idx];
                let index = changed.entry(value.index).or_insert_with(|| {
                    let idx = curr_index;
                    curr_index += 1;
                    idx
                });
                value.index = *index;
            }
            for dst_idx in &info.writes {
                let value = func.opcodes[*dst_idx].vals_mut().0.unwrap();
                let index = changed.entry(value.index).or_insert_with(|| {
                    let idx = curr_index;
                    curr_index += 1;
                    idx
                });
                value.index = *index;
            }
        }

        if let Some(ret) = &mut func.return_value {
            let index = changed.entry(ret.value.index).or_insert_with(|| {
                let idx = curr_index;
                curr_index += 1;
                idx
            });
            ret.value.index = *index;
        }
        curr_index - offset
    }

    pub fn reduce_dead_values(func: &mut LoweredFunction) {
        let mut to_remove = BTreeSet::new();
        loop {
            let values = ValueInfo::scan(func);
            // println!("dead scan");
            // for (k, v) in values.iter().collect::<BTreeMap<_, _>>() {
            //     println!("{k} = {v:?}");
            // }

            to_remove.clear();
            for (index, info) in &values {
                if info.writes.is_empty() && !info.argument {
                    panic!("Value never written");
                }
                if info.reads.is_empty() && !info.return_val {
                    to_remove.extend(info.writes.iter().copied());
                }
            }
            for i in to_remove.iter().rev().copied() {
                func.opcodes.remove(i);
            }

            if to_remove.is_empty() {
                break;
            }
        }
    }
}

#[derive(Default, Clone)]
struct ConstProp {
    consts: Rc<HashMap<u32, Option<u16>>>,
}

impl ConstProp {
    fn propagate_consts(opcodes: &mut Vec<IrOp>) {
        let mut consts = std::iter::repeat_with(|| None).take(opcodes.len()).collect::<Vec<_>>().into_boxed_slice();
        let mut backup = consts.clone();
        loop {
            propagate_consts_once(opcodes, &mut consts);
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
    fn consts(&mut self) -> &mut HashMap<u32, Option<u16>> {
        Rc::make_mut(&mut self.consts)
    }
}

#[derive(Default)]
struct LabelState {
    prev_consts: ConstProp,
    jump_source: HashSet<usize>,
}
fn propagate_consts_once(opcodes: &mut Vec<IrOp>, x: &mut [Option<u16>]) {
    let labels = LabelInfo::scan(opcodes);
    let mut label_states = HashMap::new();
    walk_opcodes(opcodes, &labels, ConstProp::default(), |idx, op, state, jump| {
        if const_prop_bin_op(op, state, &mut x[idx]) {
            return true;
        }
        if const_prop_unary_op(op, state, &mut x[idx]) {
            return true;
        }
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
                    let info = LabelState { prev_consts: ConstProp::clone(state), jump_source: HashSet::from([jump]) };
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

            _ => {}
        }
        true
    });
}

fn const_prop_bin_op(op: &mut IrOp, state: &mut ConstProp, x: &mut Option<u16>) -> bool {
    if op.is_binary_op() {
        let (a, b) = {
            let mut iter = op.vals_mut().1.into_iter();
            let a = iter.next().unwrap();
            (a, iter.next().unwrap())
        };
        let ab = state.consts.get(&a.index).zip(state.consts.get(&b.index));
        let ab = ab.and_then(|(&a, &b)| a.zip(b));
        let dst = *op.vals_mut().0.unwrap();

        if let Some((a, b)) = ab {
            let value = op.binary_op(a, b).unwrap();

            if state.consts.get(&dst.index) != Some(&Some(value)) {
                state.consts().insert(dst.index, Some(value));
            }
            *x = Some(value);
        } else {
            *x = None;
            // if previously was constant, set this state to unstable
            match state.consts.get(&dst.index) {
                None => {}
                Some(&None) => {}
                Some(&Some(_)) => _ = state.consts().insert(dst.index, None),
            }
        }
        return true;
    }
    false
}

fn const_prop_unary_op(op: &mut IrOp, state: &mut ConstProp, x: &mut Option<u16>) -> bool {
    if op.is_unary_op() {
        let dst = *op.vals_mut().0.unwrap();
        let a = op.vals_mut().1.pop().unwrap();

        if let Some(a) = state.consts.get(&a.index).copied().flatten() {
            let value = op.unary_op(a).unwrap();

            if state.consts.get(&dst.index) != Some(&Some(value)) {
                state.consts().insert(dst.index, Some(value));
            }
            *x = Some(value);
        } else {
            *x = None;
            // if previously was constant, set this state to unstable
            match state.consts.get(&dst.index) {
                None => {}
                Some(&None) => {}
                Some(&Some(_)) => _ = state.consts().insert(dst.index, None),
            }
        }
        return true;
    }
    false
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

fn inline_all(func: &mut LoweredFunction, inlines: &HashSet<usize>) -> bool {
    let label_count = LabelInfo::reindex_labels(&mut func.opcodes, 0);
    let value_count = ValueInfo::reindex_values(func, 0);
    println!("label_count: {label_count}, value_count: {value_count}");
    //2. reindex values
    //

    false
}
