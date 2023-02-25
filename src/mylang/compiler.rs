use std::sync::Arc;
use crate::mylang::preproc::{SourceMap, Token, token_iter};
use crate::mylang::{CompileErrors, SourceLoader};
use crate::emulator::Opcode;

pub fn compile(main: &str, loader: Arc<dyn SourceLoader>)->Result<Vec<Opcode>,CompileErrors>{
    let tokens = tokenize(main,loader)?;



    Ok(Vec::new())
}



fn tokenize(main: &str, loader: Arc<dyn SourceLoader>)->Result<(Vec<Token>,SourceMap),CompileErrors>{
    let source = process_tree(main,loader);
    SourceMap::set_new(source.clone());
    let map = SourceMap::global();
    Ok((token_iter(source,false).collect(),map))
}

fn process_tree(main: &str, loader: Arc<dyn SourceLoader>)->Arc<str>{
    //todo for now only supports single file
    loader.load_source(main).into()
}
