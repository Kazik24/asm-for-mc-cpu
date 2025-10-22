#![allow(unused)]
#![deny(unsafe_code)]
use crate::assembler::compile_assembly;
use crate::emulator::Opcode;
use crate::gui::launch_emulator_gui;
use crate::vm::VirtualMachine;
use clap::*;
use std::fs::{read_to_string, OpenOptions};
use std::io::{BufWriter, Read, Write};
use std::path::Path;

mod assembler;
mod emulator;
mod gui;
mod mylang;
#[cfg(test)]
mod tests;
mod vm;

static INPUT: &str = "input";
static OUTPUT: &str = "output";
static EMU: &str = "emulate";

fn main() {
    let args = App::new("Assembly compiler/emulator").version("0.1.0").author("Kazik24")
        .arg(Arg::with_name(INPUT).short("i").value_name("FILE")
		    .help("Input assembly text file to compile from, or standard input when not specified."))
        .arg(Arg::with_name(OUTPUT).short("o").value_name("FILE")
            .help(concat!("Output compiled file. ",
            "When file with '.bin' extension is used it will output text with binary opcodes in each line ",
            "with comments representing binary instruction names. ",
            "When file with '.hex' extension is used then it will output ",
            "text with hexadecimal opcode in each line, else default to output binary.")))
        .arg(Arg::with_name(EMU).short("e").value_name("RAM_SIZE").required(false)
            .help("If compiled successfully, runs emulator on compiled program with given in argument amount of ram cells assigned.")).get_matches();
    let source = match args.value_of(INPUT) {
        Some(file) => match read_to_string(Path::new(file)) {
            Ok(s) => s,
            Err(err) => {
                println!("Error: cannot read source from file \"{}\": {}", file, err);
                return;
            }
        },
        None => {
            let mut source = String::with_capacity(1024 * 32);
            if let Err(err) = std::io::stdin().lock().read_to_string(&mut source) {
                println!("Error: cannot read source from standard input: {}", err);
                return;
            }
            source
        }
    };
    let ops = match compile_assembly(&source) {
        Ok(o) => o,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };
    drop(source);
    match args.value_of(OUTPUT) {
        Some(output) => match write_opcodes(&ops, Path::new(output), true, true) {
            Err(err) => {
                println!("Error: cannot write to file \"{}\": {}", output, err);
            }
            _ => println!("Info: Compiled successfully."),
        },
        None => {
            println!("Info: No output file specified. Compiled successfully.");
        }
    }

    if let Some(value) = args.value_of(EMU) {
        let mut vm = match value.parse() {
            Ok(value) => VirtualMachine::new(value),
            Err(_) => {
                println!("Error when launching emulator.");
                println!("Value \"{}\" is not valid memory amount.", value);
                return;
            }
        };
        vm.load_start(ops.iter().copied());
        println!("Launching emulator with {} ram cells ({} bytes)", vm.ram().len(), vm.ram().len() * 2);
        launch_emulator_gui(vm);
        println!("Info: Emulator closed.")
    }
}

fn write_opcodes(ops: &[Opcode], path: &Path, comment: bool, count: bool) -> std::io::Result<()> {
    let mut file = OpenOptions::new().write(true).create(true).truncate(true).open(path)?;
    match path.extension().map(|s| s.to_string_lossy().into_owned()).as_deref() {
        Some("hex") => {
            let mut file = BufWriter::new(file);
            for (i, op) in ops.iter().copied().enumerate() {
                if i != 0 {
                    writeln!(file)?;
                }
                write!(file, "{:04X}", op.bits())?;
            }
        }
        Some("bit") => {
            let mut file = BufWriter::new(file);
            for (i, op) in ops.iter().copied().enumerate() {
                if i != 0 {
                    writeln!(file)?;
                }
                let v = op.bits();
                if count {
                    write!(file, "{:04X}: ", i)?;
                }
                write!(file, "{:04b} {:04b} {:04b} {:04b}", (v >> 12) & 0xf, (v >> 8) & 0xf, (v >> 4) & 0xf, v & 0xf)?;
                if comment {
                    if i != 0 && ops[i - 1].word_count() == 2 {
                        write!(file, "    #0x{:04X}", op.bits())?;
                    } else {
                        write!(file, "    {}", op)?;
                    }
                }
            }
        }
        _ => {
            let bytes = ops.iter().copied().flat_map(|o| o.bits().to_le_bytes()).collect::<Vec<_>>();
            file.write_all(&bytes)?;
        }
    }
    Ok(())
}
