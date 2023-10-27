mod ast;
mod codegen;
mod ir;
mod linker;
mod optimizer;
mod preproc;
mod regalloc;

use crate::emulator::Opcode;
use crate::mylang::ast::parse_ast;
use crate::mylang::codegen::{generate_code, CodegenOptions};
use crate::mylang::ir::{Lowered, SymbolTable};
use crate::mylang::optimizer::{optimize_ir, OptimizerOptions};
use crate::mylang::preproc::{Span, TextInfo};
use std::collections::HashMap;
use std::sync::Arc;

pub fn compile(main_file: &str, loader: Box<dyn SourceLoader>) -> Result<Vec<Opcode>, CompileErrors> {
    //parse source files into abstract syntax tree (AST)
    let (result, sources) = parse_ast(main_file, loader);
    let result = result.map_err(|e| CompileErrors { errors: e.into_iter().map(|v| format!("{v}")).collect() })?;

    //create symbol table and lower into intermediate representation (IR)
    let (table, ast) = SymbolTable::scan_symbols(&result).unwrap();
    let mut lowered_ir = Lowered::lower_all(table, &ast).unwrap();

    //high level optimization, e.g inline some functions, calculate const expressions, reduce jumps
    optimize_ir(&mut lowered_ir, OptimizerOptions::SPEED);

    //generate machine instructions, and perform hardware specific
    //optimizations (e.g select smaller instructions for short jumps)
    let code = generate_code(&lowered_ir, BEDROCK_CORE_CODEGEN);
    Ok(code)
}

pub const BEDROCK_CORE_CODEGEN: CodegenOptions =
    CodegenOptions { pc_reg: 15, stack_reg: 14, link_reg: 13, temp_reg: 12, argument_regs: 4, register_count: 16 };

pub struct CompileErrors {
    errors: Vec<String>,
}

pub trait SourceLoader: Send + Sync {
    fn load_source(&self, path: &str) -> Option<Arc<str>>;
}

pub struct MapSourceLoader {
    pub src: HashMap<String, String>,
}

impl SourceLoader for MapSourceLoader {
    fn load_source(&self, path: &str) -> Option<Arc<str>> {
        self.src.get(path).cloned().map(|v| v.into())
    }
}

#[derive(Debug)]
pub enum LoweringError {
    FunctionCallInConst(Span), //function was called in const initializer
    CannotResolveConst(Span),
    NumberTooLarge(Span),
    VariableOrConstNotFound(Span),
    TypesDontMatch(Span, Span),
    VoidTypeInConst,
    CannotAssignToConst(Span),
    BreakOutsideLoop(Span),
    ExpectedPointerType(Span),
    ExpectingOtherType(Span),
    ExpectedUintType(Span),
    ExpectedNumberType(Span),
    ExpectedWordSizedType(Span),
    FunctionMustReturnValue(Span),
    FunctionNotFound(Span),
    VoidReturnType(Span),
    FunctionReturnsVoid(Span),
    FunctionReturnsValue(Span),
    ArgTypeDontMatch(Span, Span),
    UnexpectedPlaceExpression(Span),
    ContinueOutsideLoop(Span),
    CircularConstDependency(Span),
    DuplicatedItem(Span, Span),
    MainFunctionNotFound,
    MainShouldHaveNoArgs(Span),
}

pub fn format_span_place(span: Span, text: &str, info: &TextInfo) -> String {
    let mut s = String::new();

    s
}
