use super::gen_ir::gen_ir;
use super::gen_x86::gen_x86;
use super::irdump::dump_ir;
use super::parse::parse;
use super::preprocess::Preprocessor;
use super::regalloc::alloc_regs;
use super::sema::sema;
use super::token::tokenize;

use crate::clang::preprocess::SourceLoader;
use std::collections::HashMap;
use std::env;
use std::fmt::{Debug, Formatter};
use std::process;

fn usage() -> ! {
    eprintln!("Usage: 9cc [-dump-ir1] [-dump-ir2] <file>");
    process::exit(1)
}

pub fn compile_clang() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        usage();
    }

    let mut dump_ir1 = false;
    let mut dump_ir2 = false;
    let path;

    if args.len() == 3 && args[1] == "-dump-ir1" {
        dump_ir1 = true;
        path = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-ir2" {
        dump_ir2 = true;
        path = args[2].clone();
    } else {
        if args.len() != 2 {
            usage();
        }
        path = args[1].clone();
    }

    // Tokenize and parse.
    let tokens = tokenize(path, &mut Preprocessor::new(Box::new(MapLoader(HashMap::new()))));

    let nodes = parse(&tokens);
    let (nodes, globals) = sema(nodes);
    let mut fns = gen_ir(nodes);

    if dump_ir1 {
        dump_ir(&fns);
    }

    alloc_regs(&mut fns);

    if dump_ir2 {
        dump_ir(&fns);
    }

    gen_x86(globals, fns);
}

pub struct MapLoader(HashMap<String, String>);
impl MapLoader {
    pub fn main(src: &str) -> Self {
        Self([("main.c".to_string(), src.to_string())].iter().cloned().collect())
    }
}
impl SourceLoader for MapLoader {
    fn load_source(&self, path: &str) -> String {
        self.0.get(path).cloned().unwrap_or(String::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_check() {
        let source = "
        void swap(int *xp, int *yp) {
            int temp = *xp;
            *xp = *yp;
            *yp = temp;
        }
        void bubblesort(int arr[],int n){
            int i;
            int j;
            for (i = 0; i < n-1; i++) {
                for (j = 0; j < n-i-1; j++) {
                    if (arr[j] > arr[j+1]) {
                        swap(&arr[j], &arr[j+1]);
                    }
                }
            }
        }

        ";

        let tokens = tokenize("main.c".to_string(), &mut Preprocessor::new(Box::new(MapLoader::main(source))));

        let nodes = parse(&tokens);
        let (nodes, globals) = sema(nodes);
        let mut fns = gen_ir(nodes.clone());

        println!("Nodes: {:#?}", nodes);
        println!("**********************************");

        println!(
            "Fns: {:#?}",
            fns.iter().map(|v| v.ir.iter().map(|v| DebugFlat(v)).collect::<Vec<_>>()).collect::<Vec<_>>()
        );
    }
}

struct DebugFlat<'a, T>(&'a T);
impl<'a, T: Debug> Debug for DebugFlat<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
