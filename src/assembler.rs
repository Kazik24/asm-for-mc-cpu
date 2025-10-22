use crate::emulator::{Opcode, SubCommand};
use std::collections::HashMap;
use std::fmt::Write;
use std::num::ParseIntError;

const MAX_ADDRESS: u32 = 2u32.pow(15) - 1;
#[derive(Clone, Eq, PartialEq, Debug)]
enum AsmArgument<'a> {
    Number(&'a str, Result<i32, ParseIntError>), //#1234, #-1234
    Label(&'a str),                              //@label
    Name(&'a str, Result<(u16, RegByte), ()>),   //r10
    Index(&'a str, Result<(u16, RegByte), ()>),  //[r3]
    Unknown(&'a str),
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum RegByte {
    All,
    High,
    Low,
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum AsmCommand<'a> {
    Label(&'a str),
    Cmd(&'a str, Vec<AsmArgument<'a>>),
    Error(&'a str),
}
fn is_label(l: &str) -> bool {
    let mut c = l.chars();
    match c.next() {
        Some(ch) if ch == '_' || ch.is_alphabetic() => {}
        _ => return false,
    }
    c.all(|c| c == '_' || c.is_alphanumeric())
}

fn command_stream(text: &str) -> impl Iterator<Item = AsmCommand<'_>> {
    fn parse_reg(name: &str) -> Result<(u16, RegByte), ()> {
        let mut ch = name.chars();
        match ch.next() {
            Some('r') | Some('R') => {}
            _ => return Err(()),
        }
        Ok(match ch.as_str().chars().last() {
            Some('H') | Some('h') => {
                let mut c = ch.as_str().chars();
                c.next_back();
                (c.as_str().parse().map_err(|_| ())?, RegByte::High)
            }
            Some('L') | Some('l') => {
                let mut c = ch.as_str().chars();
                c.next_back();
                (c.as_str().parse().map_err(|_| ())?, RegByte::Low)
            }
            _ => (ch.as_str().parse().map_err(|_| ())?, RegByte::All),
        })
    }
    text.lines()
        .flat_map(|line| {
            let s = line.trim_start();
            s.find(';').map(|pos| &s[..pos]).unwrap_or(s).trim_end().split_inclusive(':')
            //split by labels to parts
        })
        .filter(|s| !s.is_empty())
        .map(|s| {
            if let Some(pos) = s.find(':') {
                let s = s[0..pos].trim();
                return if is_label(s) { AsmCommand::Label(s) } else { AsmCommand::Error(s) };
            }
            let (name, rest) = match s.find(char::is_whitespace).map(|pos| (&s[..pos], s[pos..].trim_start())) {
                Some(s) if is_label(s.0) => s,
                _ => {
                    if s.is_empty() || !is_label(s) {
                        return AsmCommand::Error(s);
                    }
                    return AsmCommand::Cmd(s, Vec::new());
                }
            };
            let args = rest
                .split(',')
                .map(|a| a.trim())
                .map(|a| {
                    if a.starts_with('#') {
                        let n = &a['#'.len_utf8()..];
                        if n.starts_with("0x") {
                            AsmArgument::Number(n, i32::from_str_radix(n["0x".len()..].to_lowercase().as_str(), 16))
                        } else {
                            AsmArgument::Number(n, n.parse())
                        }
                    } else if a.starts_with('@') {
                        AsmArgument::Label(&a['@'.len_utf8()..])
                    } else if a.starts_with('[') && a.ends_with(']') {
                        let n = a['['.len_utf8()..(a.len() - ']'.len_utf8())].trim();
                        AsmArgument::Index(n, parse_reg(n))
                    } else if is_label(a) {
                        AsmArgument::Name(a, parse_reg(a))
                    } else {
                        AsmArgument::Unknown(a)
                    }
                })
                .collect();
            AsmCommand::Cmd(name, args)
        })
}

pub fn get_offset_in_str(parent: &str, child: &str) -> Option<usize> {
    let start = parent.as_ptr() as usize;
    let sub = child.as_ptr() as usize;
    if sub >= start && start + parent.len() >= sub + child.len() {
        Some(sub - start)
    } else {
        None
    }
}
pub fn get_pos<'a>(source: &'a str, value: &str) -> (&'a str, usize, usize) {
    let off = get_offset_in_str(source, value).expect("value is not sub-slice of source");
    let line = source[..off].lines().count();
    if line <= 0 {
        return (&source[..off], 1, 0);
    }
    let line = if source[..off].ends_with(|c| c == '\n' || c == '\r') { line + 1 } else { line };
    match source.lines().nth(line - 1) {
        Some(l) => (l, line, value.as_ptr() as usize - l.as_ptr() as usize),
        None => (&source[(source.len() - 1)..], line, 0), //last line
    }
}

macro_rules! opcode_regs {
    ($args:expr,3,$opcode:ident) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
                if *dst < 16 && *arg1 < 16 && *arg2 < 16 =>
            {
                Ok(Precompiled::op(Opcode::par($opcode, *dst, *arg1, *arg2)))
            }
            _ => Err(("Expected 3 full register names".to_string(), $args)),
        }
    };
    ($args:expr,3 rev,$opcode:ident) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
                if *dst < 16 && *arg1 < 16 && *arg2 < 16 =>
            {
                Ok(Precompiled::op(Opcode::par($opcode, *dst, *arg2, *arg1)))
            }
            _ => Err(("Expected 3 full register names".to_string(), $args)),
        }
    };
    ($args:expr,2,$opcode:ident,$spec:expr) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((src, All)))] if *dst < 16 && *src < 16 => {
                Ok(Precompiled::op(Opcode::par($opcode, *dst, *src, $spec)))
            }
            _ => Err(("Expected 2 full register names".to_string(), $args)),
        }
    };
}
#[derive(Clone, Eq, PartialEq, Debug)]
struct Precompiled<'a>(Opcode, Option<Opcode>, Option<&'a str>);
impl<'a> Precompiled<'a> {
    pub fn op(opcode: Opcode) -> Self {
        Self(opcode, None, None)
    }
    pub fn two(opcode: Opcode, value: u16) -> Self {
        Self(opcode, Some(Opcode::from(value)), None)
    }
    pub fn lab(opcode: Opcode, label: &'a str) -> Self {
        Self(opcode, None, Some(label))
    }
    pub fn def_lab(label: &'a str) -> Self {
        Self(Opcode::from(0), Some(Opcode::from(0)), Some(label))
    }
}
fn precompile_command<'a>(
    mnem: &'a str,
    args: Vec<AsmArgument<'a>>,
) -> Result<Precompiled<'a>, (String, Vec<AsmArgument<'a>>)> {
    use crate::emulator::Command::*;
    use AsmArgument::*;
    use RegByte::*;
    fn byte_copy_replace(dst: u16, src: u16, bd: bool, bs: bool) -> Opcode {
        match (bd, bs) {
            (false, false) => Opcode::ex(dst, src, SubCommand::MLL),
            (true, false) => Opcode::ex(dst, src, SubCommand::MLH),
            (false, true) => Opcode::ex(dst, src, SubCommand::MHL),
            (true, true) => Opcode::ex(dst, src, SubCommand::MHH),
        }
    }
    fn byte_copy_fill(dst: u16, src: u16, bd: bool, bs: bool, signext: bool) -> Opcode {
        match (bd, bs) {
            (false, false) => Opcode::ex(dst, src, if signext { SubCommand::SetLLS } else { SubCommand::SetLLZ }),
            (true, false) => Opcode::ex(dst, src, SubCommand::SetLHZ),
            (false, true) => Opcode::ex(dst, src, if signext { SubCommand::SetHLS } else { SubCommand::SetHLZ }),
            (true, true) => Opcode::ex(dst, src, SubCommand::SetHHZ),
        }
    }
    fn mov(
        dst: (u16, RegByte),
        src: (u16, RegByte),
        signext: bool,
        func: impl FnOnce(u16, u16, bool, bool) -> Opcode,
    ) -> Result<Precompiled<'static>, String> {
        if src.0 < 16 && dst.0 < 16 {
            Ok(match (dst, src) {
                ((dst, Low), (src, Low)) => Precompiled::op(func(dst, src, false, false)),
                ((dst, Low), (src, High)) => Precompiled::op(func(dst, src, false, true)),
                ((dst, High), (src, Low)) => Precompiled::op(func(dst, src, true, false)),
                ((dst, High), (src, High)) => Precompiled::op(func(dst, src, true, true)),
                ((dst, All), (src, All)) => Precompiled::op(Opcode::par(Or, dst, src, 0)),
                ((dst, All), (src, Low)) => {
                    Precompiled::op(Opcode::ex(dst, src, if signext { SubCommand::SetLLS } else { SubCommand::SetLLZ }))
                }
                ((dst, All), (src, High)) => {
                    Precompiled::op(Opcode::ex(dst, src, if signext { SubCommand::SetHLS } else { SubCommand::SetHLZ }))
                }
                ((dst, Low), (src, All)) => Precompiled::op(func(dst, src, false, false)),
                ((dst, High), (src, All)) => Precompiled::op(func(dst, src, true, false)),
            })
        } else {
            Err("Allowed register are r0..r15".to_string())
        }
    }
    match mnem.to_lowercase().as_str() {
        "nop" => {
            if !args.is_empty() {
                return Err(("Expected no arguments".to_string(), args));
            }
            Ok(Precompiled::op(Opcode::from(0)))
        }
        "dw" => match args.as_slice() {
            [Number(_, Ok(val))] => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Value {} out of range", val), args))
                } else {
                    Ok(Precompiled::op(Opcode::from(*val as u16)))
                }
            }
            [Label(lab)] => Ok(Precompiled::def_lab(lab)),
            _ => Err(("Expected value or label".to_string(), args)),
        },
        "add" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Number(_, Ok(val))] => {
                if *dst < 16 && *arg1 < 16 && *val >= -8 && *val <= 7 {
                    Ok(Precompiled::op(Opcode::par(Ads, *dst, *arg1, *val as _)))
                } else {
                    Err(("Expected 2 full register names and immediate from range [-8,7]".to_string(), args))
                }
            }
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
                if *dst < 16 && *arg1 < 16 && *arg2 < 16 =>
            {
                Ok(Precompiled::op(Opcode::par(Add, *dst, *arg1, *arg2)))
            }
            _ => Err(("Expected 3 full register names or 2 register names and immediate".to_string(), args)),
        },
        "sub" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Number(_, Ok(val))] => {
                if *dst < 16 && *arg1 < 16 && *val >= -7 && *val <= 8 {
                    Ok(Precompiled::op(Opcode::par(Ads, *dst, *arg1, (-*val) as _)))
                } else {
                    Err(("Expected 2 full register names and immediate from range [-7,8]".to_string(), args))
                }
            }
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
                if *dst < 16 && *arg1 < 16 && *arg2 < 16 =>
            {
                Ok(Precompiled::op(Opcode::par(Sub, *dst, *arg1, *arg2)))
            }
            _ => Err(("Expected 3 full register names or 2 register names and immediate".to_string(), args)),
        },
        "ads" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Number(_, Ok(val))]
                if *dst < 16 && *arg1 < 16 && *val >= -8 && *val <= 7 =>
            {
                Ok(Precompiled::op(Opcode::par(Ads, *dst, *arg1, *val as _)))
            }
            _ => Err(("Expected 2 full register names and immediate from range [-8,7]".to_string(), args)),
        },
        "and" => opcode_regs!(args, 3, And).map(|mut v| {
            if v.0.r().as_u16() == 0 {
                v.0 = Opcode::from(0); //replace by noop
            }
            v
        }),
        "or" => opcode_regs!(args, 3, Or),
        "xor" => opcode_regs!(args, 3, Xor),
        "eq" => opcode_regs!(args, 3, CmpEq),
        "ne" => opcode_regs!(args, 3, CmpNe),
        "lt" => opcode_regs!(args, 3, CmpLt),
        "gt" => opcode_regs!(args,3 rev,CmpLt),
        "le" => opcode_regs!(args,3 rev,CmpGe),
        "ge" => opcode_regs!(args, 3, CmpGe),
        "lts" => opcode_regs!(args, 3, CmpLts),
        "gts" => opcode_regs!(args,3 rev,CmpLts),
        "les" => opcode_regs!(args,3 rev,CmpGes),
        "ges" => opcode_regs!(args, 3, CmpGes),
        "movw" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
            | [Name(_, Ok((dst, All))), Index(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
                if *dst < 16 && *arg1 < 16 && *arg2 < 16 =>
            {
                Ok(Precompiled::op(Opcode::par(Movw, *dst, *arg1, *arg2)))
            }
            _ => Err(("Expected 3 full register names".to_string(), args)),
        },
        "cmov" => opcode_regs!(args, 3, Cmov),
        "cmovb" => opcode_regs!(args, 3, Cmovb),
        "cldi" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Number(_, Ok(val)), Name(_, Ok((cond, All)))] if *dst < 16 && *cond < 16 => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), vec![args[1].clone()]))
                } else {
                    Ok(Precompiled::two(Opcode::ex(*dst, *cond, SubCommand::Cldi), *val as u16))
                }
            }
            [Name(_, Ok((dst, All))), Label(lab), Name(_, Ok((cond, All)))] if *dst < 16 && *cond < 16 => {
                Ok(Precompiled::lab(Opcode::ex(*dst, *cond, SubCommand::Cldi), lab))
            }
            _ => Err(("Expected 2 full register names and 16-bit immediate".to_string(), args)),
        },
        "ldi" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Number(_, Ok(val))] if *dst < 16 => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), vec![args[1].clone()]))
                } else {
                    Ok(Precompiled::two(Opcode::ex(*dst, 0, SubCommand::Cldi), *val as u16))
                }
            }
            [Name(_, Ok((dst, All))), Label(lab)] if *dst < 16 => {
                Ok(Precompiled::lab(Opcode::ex(*dst, 0, SubCommand::Cldi), lab))
            }
            _ => Err(("Expected 2 full register names and 16-bit immediate".to_string(), args)),
        },
        "shr" => opcode_regs!(args, 2, Ex, SubCommand::Shr as u16),
        "ashr" => opcode_regs!(args, 2, Ex, SubCommand::Ashr as u16),
        "shl" => opcode_regs!(args, 2, Ex, SubCommand::Shl as u16),
        "movz" => match args.as_slice() {
            [Name(_, Ok(dst)), Name(_, Ok(src))] => {
                mov(*dst, *src, false, |a, b, c, d| byte_copy_fill(a, b, c, d, false)).map_err(|s| (s, args))
            }
            _ => Err(("Expected 2 register names".to_string(), args)),
        },
        "movs" => match args.as_slice() {
            [Name(_, Ok(dst)), Name(_, Ok(src))] => {
                mov(*dst, *src, true, |a, b, c, d| byte_copy_fill(a, b, c, d, true)).map_err(|s| (s, args))
            }
            _ => Err(("Expected 2 register names".to_string(), args)),
        },
        "not" => opcode_regs!(args, 2, Ex, SubCommand::Not as u16),
        "call" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Label(lab)] if *dst < 16 => {
                Ok(Precompiled::lab(Opcode::ex(*dst, 0, SubCommand::Call), lab))
            }
            [Name(_, Ok((dst, All))), Number(_, Ok(val))] if *dst < 16 => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), args))
                } else {
                    Ok(Precompiled::two(Opcode::ex(*dst, 0, SubCommand::Call), *val as u16))
                }
            }
            _ => Err(("Expected register followed by label or immediate value".to_string(), args)),
        },
        "ccll" => match args.as_slice() {
            [Name(_, Ok((dst, All))), Label(lab), Name(_, Ok((con, All)))] if *dst < 16 && *con < 16 => {
                Ok(Precompiled::lab(Opcode::ex(*dst, *con, SubCommand::Call), lab))
            }
            [Name(_, Ok((dst, All))), Number(_, Ok(val)), Name(_, Ok((con, All)))] if *dst < 16 && *con < 16 => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), args))
                } else {
                    Ok(Precompiled::two(Opcode::ex(*dst, *con, SubCommand::Call), *val as u16))
                }
            }
            _ => Err(("Expected register followed by label or immediate value".to_string(), args)),
        },
        //common aliases
        "mov" => {
            match args.as_slice() {
                [Name(_, Ok(dst)), Name(_, Ok(src))] => {
                    mov(*dst, *src, false, byte_copy_replace).map_err(|s| (s, args))
                }
                [Name(_, Ok(dst)), Index(_, Ok(src))] if dst.0 < 16 && src.0 < 16 => match (*dst, *src) {
                    ((0, All), (_, All)) => Ok(Precompiled::op(Opcode::from(0))), //nop
                    ((dst, All), (src, All)) => Ok(Precompiled::op(Opcode::par(Movw, dst, src, 0))),
                    _ => Err(("Only full registers are allowed when indexing memory".to_string(), args)),
                },
                [Name(_, Ok(_)), Index(_, Ok(_))] => Err(("Allowed register are r0..r15".to_string(), args)),
                [Index(_, Ok(dst)), Name(_, Ok(src))] if dst.0 < 16 && src.0 < 16 => match (*dst, *src) {
                    ((dst, All), (src, All)) => Ok(Precompiled::op(Opcode::par(Movw, 0, dst, src))),
                    _ => Err(("Only full registers are allowed when indexing memory".to_string(), args)),
                },
                [Name(_, Ok(dst)), Number(_, Ok(val))] => match (*dst, *val) {
                    ((dst, All), val) if dst < 16 => {
                        if val > u16::MAX as _ || val < i16::MIN as _ {
                            Err((format!("Immediate {} out of range", val), vec![args[1].clone()]))
                        } else {
                            if val & 0xfff8 == 0xfff8 || val & 0xfff8 == 0 {
                                Ok(Precompiled::op(Opcode::par(Ads, dst, 0, (val & 0xf) as u16)))
                            } else {
                                Ok(Precompiled::two(Opcode::ex(dst, 0, SubCommand::Cldi), val as u16))
                            }
                        }
                    }
                    ((_, All), _) => Err(("Allowed register are r0..r15".to_string(), args)),
                    _ => Err(("Only full registers are allowed when loading immediate value".to_string(), args)),
                },
                [Name(_, Ok(dst)), Label(lab)] => match (*dst, lab) {
                    ((dst, All), lab) if dst < 16 => Ok(Precompiled::lab(Opcode::ex(dst, 0, SubCommand::Cldi), lab)),
                    ((_, All), _) => Err(("Allowed register are r0..r15".to_string(), args)),
                    _ => Err(("Only full registers are allowed when loading immediate value".to_string(), args)),
                },
                _ => Err(("Expected register/index followed by register/index/immediate/label".to_string(), args)),
            }
        }
        "jmp" => match args.as_slice() {
            [Name(_, Ok((src, All)))] if *src < 16 => Ok(Precompiled::op(Opcode::par(Or, 15, *src, 0))),
            [Label(lab)] => Ok(Precompiled::lab(Opcode::ex(15, 0, SubCommand::Cldi), lab)),
            [Number(_, Ok(val))] => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), args))
                } else {
                    Ok(Precompiled::two(Opcode::ex(15, 0, SubCommand::Cldi), *val as u16))
                }
            }
            _ => Err(("Expected 1 full register name".to_string(), args)),
        },
        "jmpz" => match args.as_slice() {
            [Name(_, Ok((src, All))), Name(_, Ok((cond, All)))] if *src < 16 && *cond < 16 => {
                Ok(Precompiled::op(Opcode::par(Cmov, 15, *src, *cond)))
            }
            [Label(lab), Name(_, Ok((cond, All)))] if *cond < 16 => {
                Ok(Precompiled::lab(Opcode::ex(15, *cond, SubCommand::Cldi), lab))
            }
            [Number(_, Ok(val)), Name(_, Ok((cond, All)))] if *cond < 16 => {
                if *val > u16::MAX as _ || *val < i16::MIN as _ {
                    Err((format!("Immediate {} out of range", val), args))
                } else {
                    Ok(Precompiled::two(Opcode::ex(15, *cond, SubCommand::Cldi), *val as u16))
                }
            }
            _ => Err(("Expected 1 full register name".to_string(), args)),
        },
        "halt" => {
            if !args.is_empty() {
                return Err(("Expected no arguments".to_string(), args));
            }
            Ok(Precompiled::op(Opcode::par(And, 0, 15, 15)))
        }
        "kill" => {
            if !args.is_empty() {
                return Err(("Expected no arguments".to_string(), args));
            }
            Ok(Precompiled::op(Opcode::par(And, 0, 14, 15)))
        }
        "rst" => {
            if !args.is_empty() {
                return Err(("Expected no arguments".to_string(), args));
            }
            Ok(Precompiled::op(Opcode::par(And, 0, 13, 15)))
        }
        "clk" => {
            //set clock prescaler
            todo!()
        }
        _ => Err((format!("Unknown mnemonic \"{}\"", mnem), args)),
    }
}

fn format_error(err: (&str, usize, usize)) -> String {
    let mut errors = format!(" at: {}:{}", err.1, err.2);
    writeln!(errors).unwrap();
    let num = format!("{}:", err.1);
    let count = num.chars().count() + err.0.chars().take(err.2).count();
    writeln!(errors, "{}{}", num, err.0).unwrap();
    write!(errors, "{}^", " ".repeat(count)).unwrap();
    errors
}

pub fn compile_assembly(source: &str) -> Result<Vec<Opcode>, String> {
    use AsmArgument::*;
    let mut errors = String::new();
    let ops: Vec<_> = command_stream(source)
        .map(|c| match c {
            AsmCommand::Cmd(name, args) => precompile_command(name, args).map_err(|mut e| {
                e.1.push(AsmArgument::Unknown(name));
                e
            }),
            AsmCommand::Label(lab) => Ok(Precompiled::lab(Opcode::from(0), lab)),
            AsmCommand::Error(err) => Err((format!("Syntax error: {}", err), vec![Unknown(err)])),
        })
        .filter_map(|op| {
            //format errors from parsing
            match op {
                Ok(prec) => Some(prec),
                Err((text, args)) => {
                    let err = args.first().map(|a| {
                        get_pos(
                            source,
                            match a {
                                Unknown(s) | Number(s, _) | Name(s, _) | Label(s) | Index(s, _) => s,
                            },
                        )
                    });
                    write!(errors, "Syntax Error: {}", text).unwrap();
                    if let Some(pos) = err {
                        writeln!(errors, "{}", format_error(pos)).unwrap();
                    } else {
                        writeln!(errors).unwrap();
                    }
                    None
                }
            }
        })
        .collect();
    if !errors.is_empty() {
        return Err(errors.trim().to_string());
    }

    //link resolving phase
    let mut labels = HashMap::new();
    let mut repeated_label = Vec::new();
    let mut current = 0;
    let ops: Vec<_> = ops
        .into_iter()
        .filter_map(|pre| {
            match pre {
                //find labels and save their addresses
                Precompiled(op, None, Some(label)) if op == Opcode::from(0) => {
                    //found label
                    if let Some(_) = labels.insert(label, current) {
                        repeated_label.push(label);
                    }
                    //dont increment current
                    return None; //remove label mark from list
                }
                //calculate size of other ops
                Precompiled(_, None, Some(_)) => current += 2, //2 words opcode with label
                Precompiled(_, Some(_), None) => current += 2, //regular 2 words opcode
                Precompiled(_, None, None) => current += 1,    //1 word opcode
                Precompiled(_, Some(_), Some(_)) => current += 1, //1 word opcode - label value
            }
            Some(pre)
        })
        .collect();
    if !repeated_label.is_empty() {
        return Err(repeated_label
            .into_iter()
            .map(|lab| format!("Link Error: Repeated label \"{}\"{}", lab, format_error(get_pos(source, lab))))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    //paste label addresses in specific places
    let mut errors = String::new();
    let result = ops
        .into_iter()
        .flat_map(|op| {
            let Precompiled(op_first, op_sec, label) = op;
            if let Some(label) = label {
                let Some(address) = labels.get(label).copied() else {
                    writeln!(errors, "Link Error: Unknown label \"{}\"{}", label, format_error(get_pos(source, label)))
                        .unwrap();
                    return vec![];
                };
                if address > MAX_ADDRESS {
                    writeln!(
                        errors,
                        "Link Error: Label out of max program bounds \"{}\"{}",
                        label,
                        format_error(get_pos(source, label))
                    )
                    .unwrap();
                    vec![]
                } else if op_sec.is_none() {
                    vec![op_first, Opcode::from(address as u16)]
                } else {
                    //label value without any additional opcodes
                    vec![Opcode::from(address as u16)]
                }
            } else {
                match op_sec {
                    Some(sec) => vec![op_first, sec],
                    None => vec![op_first],
                }
            }
        })
        .collect::<Vec<_>>();
    if !errors.is_empty() {
        return Err(errors.trim().to_string());
    }
    assert_eq!(result.len(), current as _);
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_precompile(text: &str) -> Vec<Result<Precompiled<'_>, (String, Vec<AsmArgument<'_>>)>> {
        command_stream(text)
            .map(|c| match c {
                AsmCommand::Cmd(name, args) => precompile_command(name, args),
                AsmCommand::Label(lab) => Ok(Precompiled::lab(Opcode::from(0), lab)),
                v => panic!("Bad command {:?}", v),
            })
            .collect()
    }
    fn unwrap_1<T>(arr: Vec<T>) -> T {
        assert_eq!(arr.len(), 1);
        arr.into_iter().next().unwrap()
    }
    fn unwrap_2<T>(arr: Vec<T>) -> (T, T) {
        assert_eq!(arr.len(), 2);
        let mut it = arr.into_iter();
        let a = it.next().unwrap();
        (a, it.next().unwrap())
    }
    fn unwrap_3<T>(arr: Vec<T>) -> (T, T, T) {
        assert_eq!(arr.len(), 3);
        let mut it = arr.into_iter();
        let a = it.next().unwrap();
        let b = it.next().unwrap();
        (a, b, it.next().unwrap())
    }
    #[test]
    fn test_get_pos() {
        let text = " \n ";
        assert!(matches!(get_pos(text, &text[0..0]), (_, 1, 0)));
        assert!(matches!(get_pos(text, &text[0..1]), (_, 1, 0)));
        assert!(matches!(get_pos(text, &text[0..3]), (_, 1, 0)));
        assert!(matches!(get_pos(text, &text[1..1]), (_, 1, 1)));
        assert!(matches!(get_pos(text, &text[1..2]), (_, 1, 1)));
        assert!(matches!(get_pos(text, &text[2..2]), (_, 2, 0)));
        assert!(matches!(get_pos(text, &text[2..3]), (_, 2, 0)));
        assert!(matches!(get_pos(text, &text[3..3]), (_, 2, 1)));
        let text = " \n";
        assert!(matches!(get_pos(text, &text[2..2]), (_, 2, 0)));
    }
    #[test]
    fn test_basic() {
        let res = run_precompile("nop");
        assert_eq!(res.len(), 1);
        assert_eq!(res[0], Ok(Precompiled::op(Opcode::from(0))));
        let res = run_precompile("nop r1");
        assert!(
            matches!(res.as_slice(),[Err((_,args))] if args.as_slice() == [AsmArgument::Name("r1",Ok((1,RegByte::All)))])
        );
    }
}
