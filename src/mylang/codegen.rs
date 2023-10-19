use crate::assembler::compile_assembly;
use crate::emulator::{Command, Opcode, SubCommand};
use crate::mylang::ir::{IrOp, Label, Lowered, LoweredFunction};
use crate::mylang::regalloc::{Reg, RegAlloc, UseKind, Value};
use std::fmt::{Arguments, Debug};

#[derive(Copy, Clone)]
pub struct CodegenOptions {
    pub stack_reg: u32,
    pub link_reg: u32,
    pub pc_reg: u32,
    pub temp_reg: u32, //for storing temporary values for opcodes, e.g some constant, or address for ram access
    pub zero_reg: u32,
    pub argument_regs: u32,
    pub register_count: u32,
}

pub fn generate_code_as_assembly(ir: &Lowered, options: CodegenOptions) -> String {
    let mut opcodes = CodeWriter::default();

    let reserved_regs = [options.zero_reg, options.stack_reg, options.link_reg, options.pc_reg];

    for (index, func) in ir.functions.iter().enumerate() {
        let mut opcodes = Vec::new();
        let regs = RegAlloc::new(reserved_regs, [options.temp_reg], options.register_count);
        let mut ctx = CodegenCtx { regs, opcodes: &mut opcodes, config: options };
        generate_function(func, &mut ctx);
        insert_spilled_registers(ctx.opcodes);
        println!("Generated code for function {}", func.name.value);
        for (i, op) in opcodes.iter().enumerate() {
            println!("[{i}]: {op:?}");
        }
    }

    opcodes.code
}

pub fn generate_code(ir: &Lowered, options: CodegenOptions) -> Vec<Opcode> {
    let source = generate_code_as_assembly(ir, options);
    compile_assembly(&source).expect("Code generation should never produce invalid opcode")
}

pub struct Codegen {
    functions: Vec<CodegenFunc>,
}

pub struct CodegenFunc {
    index: usize,
    debug_name: String,
    opcodes: Vec<PrelinkOpcode>,
}

pub struct LinkSections {
    data: Vec<()>,
    code: Vec<()>,
}

impl LinkSections {}

#[derive(Debug, Default)]
pub struct CodeWriter {
    code: String,
    ident: String,
}

impl CodeWriter {
    fn line(&mut self, ident: u32, args: Arguments) {
        use std::fmt::Write;
        for _ in 0..ident {
            self.code.push_str(&self.ident);
        }
        writeln!(&mut self.code, "{args}").unwrap();
    }
    pub fn new_line(&mut self) {
        self.line(0, format_args!(""));
    }
    pub fn label(&mut self, label: &str) {
        self.line(0, format_args!("{label}:"));
    }
    pub fn op<'a>(&mut self, name: &str, args: impl IntoIterator<Item = &'a str>) {
        let args = args.into_iter().collect::<Vec<_>>().join(", ");
        self.line(1, format_args!("{name:<8} {args}"));
    }
}

pub struct LinkerDataChunk {}

type RegNum = u16;
#[derive(Copy, Clone, Debug)]
pub enum PrelinkOpcode {
    CallZ(Reg, usize), //call conditionally when reg self.1 lsb is 0
    JmpZ(Reg, Offset), //if reg == r0 and offset is small, use ads instruction on pc here
    Label(Label),

    Op(Command, Reg, Reg, RegOrConst), //Regular command, dst, regA, regB or 4 bit const
    Ex(SubCommand, Reg, Reg),          //Extension command, dst, src
    Imm(u16),
}

impl PrelinkOpcode {
    fn mov(ctx: &CodegenCtx<'_>, dst: Reg, src: Reg) -> Self {
        Self::Op(Command::Or, dst, src, RegOrConst::Reg(ctx.zero_reg()))
    }
}

fn generate_function(func: &LoweredFunction, ctx: &mut CodegenCtx) {
    //todo better args reg allocation
    if let Some(ret) = &func.return_value {
        ctx.regs.get_reg(ret.value);
    }
    for arg in &func.args {
        ctx.regs.get_reg(*arg);
    }

    for op in &func.opcodes {
        use IrOp::*;
        use RegOrConst::*;
        match *op {
            Const(dst, val) => {
                let reg = ctx.regs.get_reg(dst);
                ctx.load_const(reg, val);
            }
            Add(dst, a, b) => ctx.add_op(dst, a, b),
            Sub(dst, a, b) => ctx.sub_op(dst, a, b),
            UMul(dst, a, b) => ctx.mul_op(dst, a, b, false),
            IMul(dst, a, b) => ctx.mul_op(dst, a, b, true),
            Shl(dst, a, b) => ctx.shift_op(dst, a, b, SubCommand::SetLHZ, SubCommand::Shl),
            Shr(dst, a, b) => ctx.shift_op(dst, a, b, SubCommand::SetHLZ, SubCommand::Shr),
            Ashr(dst, a, b) => ctx.shift_op(dst, a, b, SubCommand::SetHLS, SubCommand::Ashr),
            And(dst, a, b) => ctx.simple_op(dst, a, b, Command::And),
            Or(dst, a, b) => ctx.simple_op(dst, a, b, Command::Or),
            Xor(dst, a, b) => ctx.simple_op(dst, a, b, Command::Xor),
            CmpLt(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpLt),
            CmpGe(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpGe),
            CmpLts(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpLts),
            CmpGes(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpGes),
            CmpEq(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpEq),
            CmpNe(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpNe),
            JumpFalse(val, lab) => {
                assert!(val.get_const().is_none());
                let reg = ctx.get_maybe_dropped(val);
                ctx.opcodes.push(PrelinkOpcode::JmpZ(reg, Offset::Label(lab)))
            }
            JumpTrue(val, lab) => {
                assert!(val.get_const().is_none());
                let reg = ctx.get_maybe_dropped(val);
                let temp = ctx.regs.get_temp_reg();
                ctx.regs.drop_reg(temp);
                ctx.opcodes.push(PrelinkOpcode::Ex(SubCommand::Not, temp, reg));
                ctx.opcodes.push(PrelinkOpcode::JmpZ(reg, Offset::Label(lab)))
            }
            Goto(l) => ctx.opcodes.push(PrelinkOpcode::JmpZ(ctx.zero_reg(), Offset::Label(l))),
            Return => {}
            Label(l) => ctx.opcodes.push(PrelinkOpcode::Label(l)),
            Kill(val) => ctx.regs.drop_value_reg(val),
            CallVoid(idx, ref args) => ctx.call_void(idx, &args),
            CallValue(_, _, _) => {}
            Not(dst, src) => ctx.unary_op(dst, src, |d, s, _z| PrelinkOpcode::Ex(SubCommand::Not, d, s)),
            Neg(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Op(Command::Sub, d, z, Reg(s))),
            WordToByte(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLZ, d, z)),
            HiToByte(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetHLZ, d, z)),
            ByteToWord(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLZ, d, z)),
            ByteExtend(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLS, d, z)),
            Copy(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Op(Command::Or, d, s, Reg(z))),
            PtrLoad(dst, addr) => {
                assert!(dst.get_const().is_none());
                let dst = ctx.regs.get_reg(dst);
                let reg = match addr.get_const() {
                    Some(ac) => ctx.temp_const_reg(ac),
                    None => ctx.get_maybe_dropped(addr),
                };
                ctx.opcodes.push(PrelinkOpcode::Op(Command::Movw, dst, reg, Reg(ctx.zero_reg())));
            }
            PtrStore(dst, value) => {
                let mut temp1 = None;
                let dst_addr = match dst.get_const() {
                    Some(c) => {
                        let t = ctx.regs.get_temp_reg();
                        ctx.load_const(t, c);
                        temp1 = Some(t);
                        t
                    }
                    None => ctx.get_maybe_dropped(dst),
                };
                let src_val = match value.get_const() {
                    Some(c) => ctx.temp_const_reg(c),
                    None => ctx.get_maybe_dropped(value),
                };
                if let Some(t) = temp1 {
                    ctx.regs.drop_reg(t);
                }
                //both registers have now desired values, and are marked as unallocated, so after this op they can be assigned to other ops
                ctx.opcodes.push(PrelinkOpcode::Op(Command::Movw, ctx.zero_reg(), dst_addr, Reg(src_val)));
            }
        }
    }
}

fn insert_spilled_registers(opcodes: &mut Vec<PrelinkOpcode>) {}

struct CodegenCtx<'a> {
    regs: RegAlloc,
    opcodes: &'a mut Vec<PrelinkOpcode>,
    config: CodegenOptions,
}

#[derive(Copy, Clone, Debug)]
pub enum RegOrConst {
    Reg(Reg),
    Small(u16),
}

#[derive(Copy, Clone, Debug)]
pub enum Offset {
    Label(Label),
    PcOffset(i16), // 0 means instruction after this one, 1 means skipping single word instruction, -1 is jmpz instruction
}

impl CodegenCtx<'_> {
    fn simple_op(&mut self, dst: Value, a: Value, b: Value, cmd: Command) {
        let (areg_val, breg_val) = match (a.get_const(), b.get_const()) {
            (None, None) => {
                let ar = self.regs.get_reg(a);
                let br = self.regs.get_reg(b);
                if a.index == b.index {
                    assert_eq!(a.is_drop(), b.is_drop());
                }
                if a.is_drop() {
                    self.regs.drop_value_reg(a);
                }
                if b.is_drop() && (a.index != b.index) {
                    self.regs.drop_value_reg(b);
                }
                (ar, RegOrConst::Reg(br))
            }
            (Some(ac), None) => {
                let reg = self.regs.get_reg(b);
                if b.is_drop() {
                    self.regs.drop_value_reg(b);
                }
                (self.temp_const_reg(ac), RegOrConst::Reg(reg))
            }
            (None, Some(bc)) => {
                let reg = self.regs.get_reg(a);
                if b.is_drop() {
                    self.regs.drop_value_reg(a);
                }
                (reg, RegOrConst::Reg(self.temp_const_reg(bc)))
            }
            _ => unreachable!("Unoptimized add instruction with two constants {dst:?} {a:?} {b:?}"),
        };
        let dst = self.regs.get_reg(dst);
        self.opcodes.push(PrelinkOpcode::Op(cmd, dst, areg_val, breg_val));
    }

    fn zero_reg(&self) -> Reg {
        Reg { num: self.config.zero_reg as _ }
    }

    fn unary_op(&mut self, dst: Value, src: Value, func: impl FnOnce(Reg, Reg, Reg) -> PrelinkOpcode) {
        assert!(src.get_const().is_none());
        let reg = self.regs.get_reg(src);
        if src.is_drop() {
            self.regs.drop_value_reg(src);
        }
        let dst = self.regs.get_reg(dst);
        self.opcodes.push(func(dst, reg, self.zero_reg()));
    }

    fn temp_const_reg(&mut self, value: u16) -> Reg {
        if value == 0 {
            return self.zero_reg();
        }
        let temp = self.regs.get_temp_reg();
        self.load_const(temp, value);
        self.regs.drop_reg(temp);
        temp
    }

    fn load_const(&mut self, dst: Reg, value: u16) {
        let signed_val = value as i16;
        if value == 0 {
            self.opcodes.push(PrelinkOpcode::mov(self, dst, self.zero_reg()));
        } else if signed_val >= -8 && signed_val <= 7 {
            self.opcodes.push(PrelinkOpcode::Op(Command::Ads, dst, self.zero_reg(), RegOrConst::Small(value & 0xf)))
        } else {
            self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Cldi, dst, self.zero_reg()));
            self.opcodes.push(PrelinkOpcode::Imm(value));
        }
    }
    fn add_op(&mut self, dst: Value, a: Value, b: Value) {
        let (cmd, areg_val, breg_val) = match (a.get_const(), b.get_const()) {
            (None, None) => {
                let a = self.regs.get_reg(a);
                (Command::Add, a, RegOrConst::Reg(self.regs.get_reg(b)))
            }
            (Some(ac), None) => {
                let signed_ac = ac as i16;
                if signed_ac >= -8 && signed_ac <= 7 {
                    (Command::Ads, self.get_maybe_dropped(b), RegOrConst::Small(ac & 0xf))
                } else {
                    (Command::Add, self.get_maybe_dropped(b), RegOrConst::Reg(self.temp_const_reg(ac)))
                }
            }
            (None, Some(bc)) => {
                let signed_bc = bc as i16;
                if signed_bc >= -8 && signed_bc <= 7 {
                    (Command::Ads, self.regs.get_reg(a), RegOrConst::Small(bc & 0xf))
                } else {
                    (Command::Add, self.regs.get_reg(a), RegOrConst::Reg(self.temp_const_reg(bc)))
                }
            }
            _ => unreachable!("Unoptimized add instruction with two constants {dst:?} {a:?} {b:?}"),
        };
        let dst = self.regs.get_reg(dst);
        self.opcodes.push(PrelinkOpcode::Op(cmd, dst, areg_val, breg_val));
    }
    fn sub_op(&mut self, dst: Value, a: Value, b: Value) {
        let (cmd, areg_val, breg_val) = match (a.get_const(), b.get_const()) {
            (None, None) => {
                let a = self.regs.get_reg(a);
                (Command::Sub, a, RegOrConst::Reg(self.regs.get_reg(b)))
            }
            (Some(ac), None) => {
                if ac == 0 {
                    (Command::Sub, self.zero_reg(), RegOrConst::Reg(self.regs.get_reg(b)))
                } else {
                    let temp = self.temp_const_reg(ac);
                    (Command::Sub, temp, RegOrConst::Reg(self.regs.get_reg(b)))
                }
            }
            (None, Some(bc)) => {
                let signed_bc = bc as i16;
                if signed_bc >= -7 && signed_bc <= 8 {
                    let bc = (-signed_bc) as u16;
                    (Command::Add, self.regs.get_reg(a), RegOrConst::Small(bc & 0xf))
                } else {
                    (Command::Add, self.regs.get_reg(a), RegOrConst::Reg(self.temp_const_reg(bc)))
                }
            }
            _ => unreachable!("Unoptimized sub instruction with two constants {dst:?} {a:?} {b:?}"),
        };
        let dst = self.regs.get_reg(dst);
        self.opcodes.push(PrelinkOpcode::Op(cmd, dst, areg_val, breg_val));
    }

    // right = SetHLZ + Shr, arithmetic right = SetHLS + Ashr, left = SetLHZ + Shl
    fn shift_cmd_const(&mut self, dst: Reg, src: Reg, by: u16, first: SubCommand, iter: SubCommand) {
        let mut cur_src = src;
        if by & 0b1000 != 0 {
            self.opcodes.push(PrelinkOpcode::Ex(first, dst, cur_src));
            cur_src = dst;
        }
        for _ in 0..(by & 0b111) {
            self.opcodes.push(PrelinkOpcode::Ex(iter, dst, cur_src));
            cur_src = dst;
        }
    }

    fn shift_cmd_var(&mut self, dst: Reg, shift: Reg, by8: SubCommand, by1: SubCommand) {
        self.opcodes.push(PrelinkOpcode::JmpZ(shift, Offset::PcOffset(1)));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Shr, shift, shift));
        self.opcodes.push(PrelinkOpcode::JmpZ(shift, Offset::PcOffset(2)));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Shr, shift, shift));
        self.opcodes.push(PrelinkOpcode::JmpZ(shift, Offset::PcOffset(4)));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(by1, dst, dst));
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Shr, shift, shift));
        self.opcodes.push(PrelinkOpcode::JmpZ(shift, Offset::PcOffset(1)));
        self.opcodes.push(PrelinkOpcode::Ex(by8, dst, dst));
    }

    fn mul_op(&mut self, dst: Value, a: Value, b: Value, signed: bool) {
        if signed {
            todo!("Signed multiply is not implemented");
        }
        match (a.get_const(), b.get_const()) {
            (_, None) => {
                todo!("Multiply by varialbe is not supported yet")
            }
            (None, Some(bc)) => {
                let (dst, src, _) = self.maybe_transfer_value(dst, a);
                if signed {
                    todo!("Only unsigned powers of 2 are supported for now");
                } else {
                    self.mul_const_unsigned(dst, src, bc);
                }
            }
            _ => unreachable!("Unoptimized multiply instruction with two constants {dst:?} {a:?} {b:?}"),
        };
    }

    fn mul_unsigned(&mut self, dst: Value, a: Value, b: Value) {
        let [a_val, b_reg] = self.copy_to_temp_reg([a, b]);
        let dst = self.regs.get_reg(dst);
        //dst = a_val
        self.opcodes.push(PrelinkOpcode::mov(self, dst, a_val));
        //zero dst if first bit of b_reg is zero
        self.opcodes.push(PrelinkOpcode::Op(Command::Cmov, dst, self.zero_reg(), RegOrConst::Reg(b_reg)));

        //label here
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Shr, b_reg, b_reg));
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Shl, a_val, a_val));
    }

    fn mul_const_unsigned(&mut self, dst: Reg, src: Reg, by: u16) {
        if by == 0 {
            self.opcodes.push(PrelinkOpcode::mov(self, dst, self.zero_reg()));
            return;
        } else if by == 1 {
            if src != dst {
                self.opcodes.push(PrelinkOpcode::mov(self, dst, src));
            } //else, registers are the same and mul by 1 is noop
            return;
        } else if by.is_power_of_two() {
            self.shift_cmd_const(dst, src, by.trailing_zeros() as _, SubCommand::SetLHZ, SubCommand::Shl);
            return;
        } else if by == 3 {
        } else if by.count_ones() == 2 {
            todo!()
        }

        let temp = self.regs.get_temp_reg();
        self.regs.drop_reg(temp);
        let mut part_reg = src;

        //todo
        for mask in (0..u16::BITS).map(|v| (1 << v) as u16) {
            if by & mask != 0 {}
        }
    }

    fn get_maybe_dropped(&mut self, value: Value) -> Reg {
        let reg = self.regs.get_reg(value);
        if value.is_drop() {
            self.regs.drop_value_reg(value);
        }
        reg
    }

    /// return (dst reg, src reg, true if regs are the same)
    fn maybe_transfer_value(&mut self, dst: Value, src: Value) -> (Reg, Reg, bool) {
        let src_reg = self.regs.get_reg(src);
        if src.is_drop() {
            self.regs.drop_value_reg(src);
            self.regs.claim_reg(src_reg, dst);
            (src_reg, src_reg, true)
        } else {
            (self.regs.get_reg(dst), src_reg, false)
        }
    }

    fn copy_to_temp_reg<const N: usize>(&mut self, value: [Value; N]) -> [Reg; N] {
        assert!(N <= 3);
        let mut result = [Reg { num: 0 }; N];

        let mut to_drop = [Result::<Reg, Value>::Ok(Reg { num: 0 }); N];

        for (i, value) in value.into_iter().enumerate() {
            if value.is_drop() {
                let r = self.regs.get_reg(value);
                to_drop[i] = Err(value);
                result[i] = r;
            } else {
                let b = self.regs.get_reg(value);
                let temp = self.regs.get_temp_reg();
                self.opcodes.push(PrelinkOpcode::mov(self, temp, b));
                to_drop[i] = Ok(temp);
                result[i] = temp;
            }
        }
        for d in to_drop {
            match d {
                Ok(Reg { num: 0 }) => {}
                Ok(v) => self.regs.drop_reg(v),
                Err(v) => self.regs.drop_value_reg(v),
            }
        }
        result
    }

    fn result_reg_with_copy(&mut self, dst: Value, value: Value) -> Reg {
        if let Some(vc) = value.get_const() {
            let reg = self.regs.get_reg(dst);
            self.load_const(reg, vc);
            reg
        } else {
            let reg = self.regs.get_reg(dst);
            //todo should this be more optimized to avoid allocations when dst reg == value reg?
            let r = self.get_maybe_dropped(value);
            self.opcodes.push(PrelinkOpcode::mov(self, reg, r));
            reg
        }
    }

    /// Copies `value` into `dst` register and allocates temporary register for `operand`, this function tries
    /// to reuse registers which are marked for drop
    fn copy_result_and_temp_operand(&mut self, dst: Value, value: Value, operand: Value) -> (Reg, Reg) {
        assert!(operand.get_const().is_none()); //operand cannot be const
        if let Some(vc) = value.get_const() {
            let reg = self.regs.get_reg(dst);
            self.load_const(reg, vc);
            let [ope] = self.copy_to_temp_reg([operand]);
            (reg, ope)
        } else {
            let r = self.get_maybe_dropped(value);
            let reg = self.regs.get_reg(dst);
            self.opcodes.push(PrelinkOpcode::mov(self, reg, r));
            let [ope] = self.copy_to_temp_reg([operand]);
            (reg, ope)
        }
    }

    fn shift_op(&mut self, dst: Value, a: Value, b: Value, by8: SubCommand, by1: SubCommand) {
        match (a.get_const(), b.get_const()) {
            (_, None) => {
                let (dst, shift) = self.copy_result_and_temp_operand(dst, a, b);
                self.shift_cmd_var(dst, shift, by8, by1);
            }
            (None, Some(bc)) => {
                let src = self.get_maybe_dropped(a);
                let dst = self.regs.get_reg(dst);
                self.shift_cmd_const(dst, src, bc, by8, by1);
            }
            _ => unreachable!("Unoptimized shr instruction with two constants {dst:?} {a:?} {b:?}"),
        };
    }

    fn call_void(&mut self, index: usize, args: &[Value]) {
        todo!()
    }
}
