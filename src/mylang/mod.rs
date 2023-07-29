mod ast;
mod compiler;
mod preproc;

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
