use crate::emulator::Opcode;
use crate::mylang::preproc::{token_iter, SourceMap, Token};
use crate::mylang::{CompileErrors, SourceLoader};
use std::sync::Arc;

pub fn compile(main: &str, loader: Arc<dyn SourceLoader>) -> Result<Vec<Opcode>, CompileErrors> {
    let tokens = tokenize(main, loader)?;

    Ok(Vec::new())
}

fn tokenize(main: &str, loader: Arc<dyn SourceLoader>) -> Result<(Vec<Token>, SourceMap), CompileErrors> {
    let source = process_tree(main, loader);
    SourceMap::set_new(source.clone());
    let map = SourceMap::global();
    Ok((token_iter(source, false).collect(), map))
}

fn process_tree(main: &str, loader: Arc<dyn SourceLoader>) -> Arc<str> {
    //todo for now only supports single file
    loader.load_source(main).into()
}
