use crate::assembler::compile_assembly;
use crate::emulator::{Command, Opcode, SubCommand};
use crate::mylang::ir::{IrOp, Label, Lowered, LoweredFunction};
use crate::mylang::regalloc::{Reg, RegAlloc, Value};
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

    for func in &ir.functions {
        let mut opcodes = Vec::new();
        let mut ctx = CodegenCtx { regs: RegAlloc::new([0, 13, 14, 15]), opcodes: &mut opcodes, config: options };
        generate_function(func, &mut ctx);
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
    Call(usize),
    CondCall(usize, Reg), //call conditionally when reg self.1 lsb is 0
    Jmpz(Reg, Label),
    Goto(Label),
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
                let signed_val = val as i16;
                if signed_val >= -8 && signed_val <= 7 {
                    ctx.opcodes.push(PrelinkOpcode::Op(Command::Ads, reg, ctx.zero_reg(), RegOrConst::Small(val & 0xf)))
                } else {
                    ctx.opcodes.push(PrelinkOpcode::Ex(SubCommand::Cldi, reg, ctx.zero_reg()));
                    ctx.opcodes.push(PrelinkOpcode::Imm(val));
                }
            }
            Add(dst, a, b) => ctx.add_op(dst, a, b),
            Sub(dst, a, b) => ctx.sub_op(dst, a, b),
            Mul(_, _, _) => {}
            Shl(_, _, _) => {}
            Shr(_, _, _) => {}
            Ashr(_, _, _) => {}
            And(dst, a, b) => ctx.simple_op(dst, a, b, Command::And),
            Or(dst, a, b) => ctx.simple_op(dst, a, b, Command::Or),
            Xor(dst, a, b) => ctx.simple_op(dst, a, b, Command::Xor),
            CmpLt(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpLt),
            CmpGe(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpGe),
            CmpLts(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpLts),
            CmpGes(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpGes),
            CmpEq(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpEq),
            CmpNe(dst, a, b) => ctx.simple_op(dst, a, b, Command::CmpNe),
            JumpFalse(_, _) => {}
            JumpTrue(_, _) => {}
            Goto(_) => {}
            Return => {}
            Label(l) => ctx.opcodes.push(PrelinkOpcode::Label(l)),
            Kill(val) => {} //ctx.regs.drop_value_reg(val),
            CallVoid(_, _) => {}
            CallValue(_, _, _) => {}
            Not(_, _) => {}
            Neg(dst, src) => ctx.sub_op(dst, Value::zero_const(), src),
            WordToByte(_, _) => {}
            HiToByte(_, _) => {}
            ByteToWord(_, _) => {}
            ByteExtend(_, _) => {}
            Copy(_, _) => {}
            PtrLoad(_, _) => {}
            PtrStore(_, _) => {}
        }
    }
}

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

impl CodegenCtx<'_> {
    fn simple_op(&mut self, dst: Value, a: Value, b: Value, cmd: Command) {
        let (areg_val, breg_val) = match (a.get_const(), b.get_const()) {
            (None, None) => {
                let a = self.regs.get_reg(a);
                (a, RegOrConst::Reg(self.regs.get_reg(b)))
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

    fn temp_const_reg(&mut self, value: u16) -> Reg {
        let temp = self.regs.get_temp_reg();
        //we know this constant can be ony represented as 2 machine words instructions
        //no point to defining it as pre-link LDI instruction.
        self.opcodes.push(PrelinkOpcode::Ex(SubCommand::Cldi, temp, self.zero_reg()));
        self.opcodes.push(PrelinkOpcode::Imm(value));
        //we will not use this reg anymore (next instruction will use it, but we can drop it already
        //since next instruction can allocate only destination registers, and this is a source reg)
        self.regs.drop_reg(temp);
        temp
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
}
