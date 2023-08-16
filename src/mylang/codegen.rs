use crate::assembler::compile_assembly;
use crate::emulator::Opcode;
use crate::mylang::ir::{IrOp, Label, Lowered, LoweredFunction};
use std::fmt::Arguments;

#[derive(Copy, Clone)]
pub struct CodegenOptions {
    pub stack_reg: usize,
    pub link_reg: usize,
}

pub fn generate_code_as_assembly(ir: &Lowered, options: CodegenOptions) -> String {
    let mut opcodes = CodeWriter::default();

    for func in &ir.functions {
        generate_function(func, &mut opcodes);
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

pub enum PrelinkOpcode {
    Call(usize),
    CondCall(usize, u8), //call conditionally when reg self.1 lsb is 0
    Ldi(),
    Jmpz(Label),
    Op(Opcode),
}

fn generate_function(func: &LoweredFunction, opcodes: &mut CodeWriter) {
    for op in &func.opcodes {
        use IrOp::*;
        match op {
            Const(_, _) => {}
            Add(_, _, _) => {}
            Sub(_, _, _) => {}
            Mul(_, _, _) => {}
            Shl(_, _, _) => {}
            Shr(_, _, _) => {}
            Ashr(_, _, _) => {}
            And(_, _, _) => {}
            Or(_, _, _) => {}
            Xor(_, _, _) => {}
            CmpLt(_, _, _) => {}
            CmpGe(_, _, _) => {}
            CmpLts(_, _, _) => {}
            CmpGes(_, _, _) => {}
            CmpEq(_, _, _) => {}
            CmpNe(_, _, _) => {}
            JumpFalse(_, _) => {}
            JumpTrue(_, _) => {}
            Goto(_) => {}
            Return => {}
            Label(_) => {}
            Kill(_) => {}
            CallVoid(_, _) => {}
            CallValue(_, _, _) => {}
            Not(_, _) => {}
            Neg(_, _) => {}
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
