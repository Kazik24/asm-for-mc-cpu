#![allow(unused)]
#![deny(unsafe_code)]
use crate::emulator::Opcode;
use crate::vm::VirtualMachine;
use crate::assembler::{compile_assembly, get_pos};
use clap::*;
use std::io::{Read, BufWriter, Write};
use std::fs::{OpenOptions, File, read_to_string};
use std::path::Path;
use std::array::IntoIter;

mod emulator;
mod vm;
mod assembler;
#[cfg(test)]
mod tests;

static INPUT: &str = "input";
static OUTPUT: &str = "output";

fn main() {

    let args = App::new("Assembly compiler/emulator").version("0.1").author("Kazik24")
        .arg(Arg::with_name(INPUT).short("i").value_name("FILE")
		    .help("Input assembly text file to compile from, or standard input when not specified."))
        .arg(Arg::with_name(OUTPUT).short("o").value_name("FILE")
            .help(concat!("Output compiled file, when file with '.hex' extension is used then it will output ",
            "text with hexadecimal opcode in each line, else default to output binary."))).get_matches();
    let source = match args.value_of(INPUT) {
        Some(file) => match read_to_string(Path::new(file)) {
            Ok(s) => s,
            Err(err) => {
                println!("Error: cannot read source from file \"{}\": {}",file,err);
                return;
            }
        }
        None => {
            let mut source = String::with_capacity(1024*32);
            if let Err(err) = std::io::stdin().lock().read_to_string(&mut source) {
                println!("Error: cannot read source from standard input: {}",err);
                return;
            }
            source
        }
    };
    let ops = match compile_assembly(&source) {
        Ok(o) => o,
        Err(err) => {
            println!("{}",err);
            return;
        }
    };
    drop(source);
    match args.value_of(OUTPUT) {
        Some(output) => {
            match write_opcodes(ops,Path::new(output)) {
                Err(err) => {
                    println!("Error: cannot write to file \"{}\": {}",output,err);
                    return;
                }
                _ => {}
            }
        }
        None => {
            println!("Info: No output file specified, compiled with no errors.");
            return;
        }
    }
    println!("Info: Compiled successfully.");
}

fn write_opcodes(ops: Vec<Opcode>,path: &Path)->std::io::Result<()>{
    let hex = path.extension().map(|s|s.to_string_lossy().into_owned()).as_deref() == Some("hex");
    let mut file = OpenOptions::new().write(true).create(true).truncate(true).open(path)?;
    if hex {
        let mut file = BufWriter::new(file);
        for (i,op) in ops.into_iter().enumerate() {
            if i != 0 {writeln!(file)?;}
            write!(file,"{:04X}",op.bits())?;
        }
    }else{
        let bytes = ops.into_iter().flat_map(|o|IntoIter::new(o.bits().to_le_bytes())).collect::<Vec<_>>();
        file.write_all(&bytes)?;
    }
    Ok(())
}
