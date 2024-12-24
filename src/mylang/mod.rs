mod ast;
mod codegen;
mod ir;
mod linker;
mod optimizer;
mod preproc;
mod regalloc;

use crate::emulator::Opcode;
use crate::mylang::ast::{parse_ast, AstError};
use crate::mylang::codegen::{generate_code, CodegenOptions};
use crate::mylang::ir::{Lowered, SymbolTable};
use crate::mylang::optimizer::{optimize_ir, OptimizerOptions};
use crate::mylang::preproc::{SourceMap, Span, TextInfo};
use std::collections::HashMap;
use std::sync::Arc;

pub struct Compiler {
    optimizer_options: OptimizerOptions,
    codegen_options: CodegenOptions,
}

impl Compiler {
    pub const BEDROCK_SPEED: Self = Self::new(OptimizerOptions::SPEED, CodegenOptions::BEDROCK_CORE_CODEGEN);
    pub const fn new(optimizer: OptimizerOptions, codegen: CodegenOptions) -> Self {
        Self { optimizer_options: optimizer, codegen_options: codegen }
    }

    pub fn compile(&self, main_file: &str, loader: Box<dyn SourceLoader>) -> Result<Vec<Opcode>, CompileErrors> {
        //parse source files into abstract syntax tree (AST)
        let (result, sources) = parse_ast(main_file, loader);
        let result = result.map_err(|e| CompileErrors::from_ast(e, &sources))?;

        //create symbol table and lower into intermediate representation (IR)
        let (table, ast) = SymbolTable::scan_symbols(&result).map_err(|e| CompileErrors::from_lowering(e, &sources))?;
        let mut lowered_ir = Lowered::lower_all(table, &ast).map_err(|e| CompileErrors::from_lowering(e, &sources))?;

        //high level optimization, e.g inline some functions, calculate const expressions, reduce jumps
        optimize_ir(&mut lowered_ir, self.optimizer_options);

        //generate machine instructions, and perform hardware specific
        //optimizations (e.g select smaller instructions for short jumps)
        let code = generate_code(&lowered_ir, self.codegen_options);
        Ok(code)
    }
}

impl OptimizerOptions {
    pub const SIZE: Self = Self {
        max_inlines_t1: 1,
        max_inline_ir_ops: 0,
        max_inlines_t2: 1,
        max_loop_unroll: 1,
        passes: 1,
        garbage_collect: true,
    };
    pub const SPEED: Self = Self {
        max_inlines_t1: 4,
        max_inline_ir_ops: 32,
        max_inlines_t2: 16,
        max_loop_unroll: 16,
        passes: 1,
        garbage_collect: false,
    };
}

impl CodegenOptions {
    pub const BEDROCK_CORE_CODEGEN: CodegenOptions =
        CodegenOptions { pc_reg: 15, stack_reg: 14, link_reg: 13, temp_reg: 12, argument_regs: 4, register_count: 16 };
}

#[derive(Debug, Clone)]
pub struct CompileErrors {
    errors: Vec<String>,
}

impl CompileErrors {
    pub fn from_ast(err: Vec<AstError>, map: &SourceMap) -> Self {
        Self { errors: err.into_iter().map(|v| format!("{v}")).collect() }
    }

    pub fn from_lowering(err: Vec<LoweringError>, map: &SourceMap) -> Self {
        Self { errors: err.into_iter().map(|v| format!("{v:?}")).collect() }
    }
}

pub trait SourceLoader: Send + Sync {
    fn load_source(&self, path: &str) -> Option<Arc<str>>;
}

#[derive(Debug, Clone)]
pub struct MapSourceLoader(HashMap<String, String>);

impl SourceLoader for MapSourceLoader {
    fn load_source(&self, path: &str) -> Option<Arc<str>> {
        self.0.get(path).cloned().map(|v| v.into())
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
