mod ast;
mod compiler;
mod ir;
mod optimizer;
mod preproc;
mod regalloc;

use crate::mylang::preproc::Span;
pub use compiler::*;
use std::collections::HashMap;

pub struct CompileErrors {
    errors: Vec<String>,
}

pub trait SourceLoader: Send + Sync {
    fn load_source(&self, path: &str) -> String;
}

pub struct MapSourceLoader {
    pub src: HashMap<String, String>,
}

impl SourceLoader for MapSourceLoader {
    fn load_source(&self, path: &str) -> String {
        self.src.get(path).cloned().unwrap_or_default()
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
}
