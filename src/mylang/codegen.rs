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
}

pub fn generate_code_as_assembly(ir: &Lowered, options: CodegenOptions) -> String {
    let mut opcodes = CodeWriter::default();

    for (index, func) in ir.functions.iter().enumerate() {
        let mut opcodes = Vec::new();
        let mut ctx = CodegenCtx { regs: RegAlloc::new([0, 13, 14, 15]), opcodes: &mut opcodes, config: options };
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
    JmpZ(Reg, Offset),
    Label(Label),

    Op(Command, Reg, Reg, RegOrConst), //Regular command, dst, regA, regB or 4 bit const
    Ex(SubCommand, Reg, Reg),          //Extension command, dst, src
    Imm(u16),
}

fn generate_function(func: &LoweredFunction, ctx: &mut CodegenCtx) {
    for op in &func.opcodes {
        use IrOp::*;
        match *op {
            Const(dst, val) => {
                let reg = ctx.regs.get_reg(dst);
                ctx.load_const(reg, val);
            }
            Add(dst, a, b) => ctx.add_op(dst, a, b),
            Sub(dst, a, b) => ctx.sub_op(dst, a, b),
            Mul(_, _, _) => {}
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
                let reg = ctx.regs.get_reg(val);
                ctx.opcodes.push(PrelinkOpcode::JmpZ(reg, Offset::Label(lab)))
            }
            JumpTrue(val, lab) => {
                assert!(val.get_const().is_none());
                let temp = ctx.regs.get_temp_reg();
                let reg = ctx.regs.get_reg(val);
                ctx.regs.drop_reg(temp);
                ctx.opcodes.push(PrelinkOpcode::Ex(SubCommand::Not, temp, reg));
                ctx.opcodes.push(PrelinkOpcode::JmpZ(reg, Offset::Label(lab)))
            }
            Goto(l) => ctx.opcodes.push(PrelinkOpcode::JmpZ(ctx.zero_reg(), Offset::Label(l))),
            Return => {}
            Label(l) => ctx.opcodes.push(PrelinkOpcode::Label(l)),
            Kill(val) => ctx.regs.drop_value_reg(val),
            CallVoid(_, _) => {}
            CallValue(_, _, _) => {}
            Not(dst, src) => ctx.unary_op(dst, src, |d, s, _z| PrelinkOpcode::Ex(SubCommand::Not, d, s)),
            Neg(dst, src) => {
                ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Op(Command::Sub, d, z, RegOrConst::Reg(s)))
            }
            WordToByte(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLZ, d, z)),
            HiToByte(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetHLZ, d, z)),
            ByteToWord(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLZ, d, z)),
            ByteExtend(dst, src) => ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Ex(SubCommand::SetLLS, d, z)),
            Copy(dst, src) => {
                ctx.unary_op(dst, src, |d, s, z| PrelinkOpcode::Op(Command::Or, d, s, RegOrConst::Reg(z)))
            }
            PtrLoad(dst, addr) => {
                assert!(dst.get_const().is_none());
                let dst = ctx.regs.get_reg(dst);
                let reg = match addr.get_const() {
                    Some(ac) => ctx.temp_const_reg(ac),
                    None => ctx.regs.get_reg(addr),
                };
                ctx.opcodes.push(PrelinkOpcode::Op(Command::Movw, dst, reg, RegOrConst::Reg(ctx.zero_reg())));
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
                    None => ctx.regs.get_reg(dst),
                };
                let src_val = match value.get_const() {
                    Some(c) => ctx.temp_const_reg(c),
                    None => ctx.regs.get_reg(value),
                };
                if let Some(t) = temp1 {
                    ctx.regs.drop_reg(t);
                }
                //both registers have now desired values, and are marked as unallocated, so after this op they can be assigned to other ops
                ctx.opcodes.push(PrelinkOpcode::Op(Command::Movw, ctx.zero_reg(), dst_addr, RegOrConst::Reg(src_val)));
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
            (Some(ac), None) => (self.temp_const_reg(ac), RegOrConst::Reg(self.regs.get_reg(b))),
            (None, Some(bc)) => (self.regs.get_reg(a), RegOrConst::Reg(self.temp_const_reg(bc))),
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
        let dst = self.regs.get_reg(dst);
        let src = self.regs.get_reg(src);
        self.opcodes.push(func(dst, src, self.zero_reg()));
    }

    fn temp_const_reg(&mut self, value: u16) -> Reg {
        let temp = self.regs.get_temp_reg();
        self.load_const(temp, value);
        self.regs.drop_reg(temp);
        temp
    }

    fn load_const(&mut self, dst: Reg, value: u16) {
        let signed_val = value as i16;
        if signed_val >= -8 && signed_val <= 7 {
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
                    (Command::Ads, self.regs.get_reg(b), RegOrConst::Small(ac & 0xf))
                } else {
                    (Command::Add, self.regs.get_reg(b), RegOrConst::Reg(self.temp_const_reg(ac)))
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

    fn mul_const(&mut self, dst: Reg, src: Reg, by: u16) {
        let temp = self.regs.get_temp_reg();
    }

    fn copy_to_temp_reg(&mut self, value: Value) -> Reg {
        if value.use_kind == UseKind::Drop {
            let r = self.regs.get_reg(value);
            self.regs.drop_value_reg(value);
            r
        } else {
            let b = self.regs.get_reg(value);
            let temp = self.regs.get_temp_reg();
            self.opcodes.push(PrelinkOpcode::Op(Command::Or, temp, b, RegOrConst::Reg(self.zero_reg())));
            self.regs.drop_reg(temp);
            temp
        }
    }

    fn result_reg_with_copy(&mut self, dst: Value, value: Value) -> Reg {
        let reg = self.regs.get_reg(dst);
        if let Some(vc) = value.get_const() {
            self.load_const(reg, vc);
        } else {
            let r = self.regs.get_reg(value);
            if value.is_drop() {
                self.regs.drop_value_reg(value);
            }
            self.opcodes.push(PrelinkOpcode::Op(Command::Or, reg, r, RegOrConst::Reg(self.zero_reg())));
        }
        reg
    }

    fn shift_op(&mut self, dst: Value, a: Value, b: Value, by8: SubCommand, by1: SubCommand) {
        match (a.get_const(), b.get_const()) {
            (_, None) => {
                let dst = self.result_reg_with_copy(dst, a);
                let shift = self.copy_to_temp_reg(b);
                self.shift_cmd_var(dst, shift, by8, by1);
            }
            (None, Some(bc)) => {
                let dst = self.regs.get_reg(dst);
                let src = self.regs.get_reg(a);
                self.shift_cmd_const(dst, src, bc, by8, by1);
            }
            _ => unreachable!("Unoptimized shr instruction with two constants {dst:?} {a:?} {b:?}"),
        };
    }
}
