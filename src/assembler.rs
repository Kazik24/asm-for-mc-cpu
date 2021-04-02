use crate::emulator::Opcode;
use std::convert::TryFrom;
use std::num::ParseIntError;
use std::fmt::Write;
use std::collections::HashMap;
use std::array::IntoIter;

const MAX_ADDRESS: usize = 2usize.pow(15) - 1;
#[derive(Clone,Eq,PartialEq,Debug)]
enum AsmArgument<'a>{
    Number(&'a str,Result<i32,ParseIntError>), //#1234, #-1234
    Label(&'a str),  //@label
    Name(&'a str,Result<(u16,RegByte),()>),   //r10
    Index(&'a str,Result<(u16,RegByte),()>),  //[r3]
    Unknown(&'a str),
}
#[derive(Copy,Clone,Eq,PartialEq,Debug)]
enum RegByte{All,High,Low}

#[derive(Clone,Eq,PartialEq,Debug)]
enum AsmCommand<'a>{
    Label(&'a str),
    Cmd(&'a str,Vec<AsmArgument<'a>>),
    Error(&'a str),
}
fn is_label(l: &str)->bool{
    let mut c = l.chars();
    match c.next() {
        Some(ch) if ch == '_' || ch.is_alphabetic() => {}
        _ => return false,
    }
    c.all(|c|c == '_' || c.is_alphanumeric())
}

fn command_stream(text: &str)->impl Iterator<Item=AsmCommand> {
    fn parse_reg(name: &str)->Result<(u16,RegByte),()>{
        let mut ch = name.chars();
        match ch.next() {
            Some('r') | Some('R') => {}, _ => return Err(()),
        }
        Ok(match ch.as_str().chars().last() {
            Some('H') | Some('h') =>{
                let mut c = ch.as_str().chars();
                c.next_back();
                (c.as_str().parse().map_err(|_|())?,RegByte::High)
            }
            Some('L') | Some('l') => {
                let mut c = ch.as_str().chars();
                c.next_back();
                (c.as_str().parse().map_err(|_|())?,RegByte::Low)
            }
            _ => (ch.as_str().parse().map_err(|_|())?,RegByte::All)
        })
    }
    text.lines().map(|line|{
            let s = line.trim_start();
            s.find(';').map(|pos|&s[..pos]).unwrap_or(s).trim_end()
        }).filter(|s|!s.is_empty()).map(|s|{
        if let Some(pos) = s.find(':') {
            let s = s[0..pos].trim();
            return if is_label(s) { AsmCommand::Label(s) }
            else { AsmCommand::Error(s) };
        }
        let (name,rest) = match s.find(char::is_whitespace).map(|pos|(&s[..pos],s[pos..].trim_start())) {
            Some(s) if is_label(s.0) => s, _ => {
                if s.is_empty() || !is_label(s) { return AsmCommand::Error(s) }
                return AsmCommand::Cmd(s,Vec::new())
            }
        };
        let args = rest.split(',').map(|a|a.trim()).map(|a|{
            if a.starts_with('#') {
                let n = &a['#'.len_utf8()..];
                AsmArgument::Number(n,n.parse())
            } else if a.starts_with('@') { AsmArgument::Label(&a['@'.len_utf8()..]) }
            else if a.starts_with('[') && a.ends_with(']'){
                let n = a['['.len_utf8()..(a.len() - ']'.len_utf8())].trim();
                AsmArgument::Index(n,parse_reg(n))
            } else if is_label(a) { AsmArgument::Name(a,parse_reg(a)) }
            else { AsmArgument::Unknown(a) }
        }).collect();
        AsmCommand::Cmd(name,args)
    })
}

pub fn get_offset_in_str(parent: &str,child: &str)->Option<usize>{
    let start = parent.as_ptr() as usize;
    let sub = child.as_ptr() as usize;
    if sub >= start && start + parent.len() >= sub + child.len() { Some(sub - start) } else { None }
}
pub fn get_pos<'a>(source: &'a str,value: &str)->(&'a str,usize,usize) {
    let off = get_offset_in_str(source,value).expect("value is not sub-slice of source");
    let line = source[..off].lines().count();
    if line <= 0 { return (&source[..off],1,0) }
    let line = if source[..off].ends_with(|c|{c=='\n'||c=='\r'}) { line + 1 } else { line };
    match source.lines().nth(line-1) {
        Some(l) => (l,line,value.as_ptr() as usize - l.as_ptr() as usize),
        None => (&source[(source.len()-1)..],line,0),//last line
    }
}

macro_rules! opcode_regs {
    ($args:expr,3,$opcode:ident) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
            if *dst < 16 && *arg1 < 16 && *arg2 < 16 => Ok(Precompiled(Opcode::par($opcode,*dst,*arg1,*arg2), None, None)),
            _ => { Err(("Expected 3 full register names".to_string(), $args)) }
        }
    };
    ($args:expr,3 rev,$opcode:ident) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
            if *dst < 16 && *arg1 < 16 && *arg2 < 16 => Ok(Precompiled(Opcode::par($opcode,*dst,*arg2,*arg1), None, None)),
            _ => { Err(("Expected 3 full register names".to_string(), $args)) }
        }
    };
    ($args:expr,2,$opcode:ident,$spec:expr) => {
        match $args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((src, All)))]
            if *dst < 16 && *src < 16 => Ok(Precompiled(Opcode::par($opcode,*dst,*src,$spec), None, None)),
            _ => { Err(("Expected 2 full register names".to_string(), $args)) }
        }
    };
}
#[derive(Clone,Eq,PartialEq,Debug)]
struct Precompiled<'a>(Opcode,Option<Opcode>,Option<&'a str>);
fn precompile_command<'a>(mnem: &'a str,args: Vec<AsmArgument<'a>>)->Result<Precompiled<'a>,(String,Vec<AsmArgument<'a>>)>{
    use AsmArgument::*;
    use RegByte::*;
    use crate::emulator::{Command::*,Arg::*};
    fn byte_copy_replace(dst: u16,src: u16,bd: bool,bs: bool)->Opcode{
        match (bd,bs) {
            (false,false) => Opcode::par(Movx,dst,src,8),
            (true,false) => Opcode::par(Movx,dst,src,9),
            (false,true) => Opcode::par(Movx,dst,src,10),
            (true,true) => Opcode::par(Movx,dst,src,11),
        }
    }
    fn byte_copy_fill(dst: u16,src: u16,bd: bool,bs: bool,signext: bool)->Opcode{
        match (bd,bs) {
            (false,false) => Opcode::par(Movx,dst,src,if signext { 7 } else { 6 }),
            (true,false) => Opcode::par(Movx,dst,src,12),
            (false,true) => Opcode::par(Movx,dst,src,if signext { 5 } else { 4 }),
            (true,true) => Opcode::par(Movx,dst,src,13),
        }
    }
    fn mov(dst: (u16, RegByte), src: (u16, RegByte),signext: bool, func: impl FnOnce(u16,u16,bool,bool)->Opcode) ->Result<Precompiled<'static>,String>{
        if src.0 < 16 && dst.0 < 16 {
            Ok(match (dst,src) {
                ((dst,Low),(src,Low)) => Precompiled(func(dst,src,false,false), None, None),
                ((dst,Low),(src,High)) => Precompiled(func(dst,src,false,true), None, None),
                ((dst,High),(src,Low)) => Precompiled(func(dst,src,true,false), None, None),
                ((dst,High),(src,High)) => Precompiled(func(dst,src,true,true), None, None),
                ((dst,All),(src,All)) => Precompiled(Opcode::par(Or,dst,src,0), None, None),
                ((dst,All),(src,Low)) => Precompiled(Opcode::par(Movx,dst,src,if signext { 7 } else { 6 }), None, None),
                ((dst,All),(src,High)) => Precompiled(Opcode::par(Movx,dst,src,if signext { 5 } else { 4 }), None, None),
                ((dst,Low),(src,All)) => Precompiled(func(dst,src,false,false), None, None),
                ((dst,High),(src,All)) => Precompiled(func(dst,src,true,false), None, None),
            })
        }else { Err("Allowed register are r0..r15".to_string())}
    }
    match mnem.to_lowercase().as_str() {
        "nop" => {
            if !args.is_empty() { return Err(("Expected no arguments".to_string(),args))}
            return Ok(Precompiled(Opcode::from(0),None,None));
        }
        "dw" => {
            return match args.as_slice() {
                [Number(_, Ok(val))] => {
                    if *val > u16::MAX as _ || *val < i16::MIN as _ { Err((format!("Value {:?} out of range",val),args)) }
                    else { Ok(Precompiled(Opcode::from(*val as u16), None, None)) }
                },
                _ => Err(("Expected value".to_string(), args)),
            }
        }
        "add" => return opcode_regs!(args,3,Add), //todo user might accidentally halt cpu with ADD r0,r15,r15
        "sub" => return opcode_regs!(args,3,Sub),
        "ads" => {
            return match args.as_slice() {
                [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Number(_, Ok(val))]
                if *dst < 16 && *arg1 < 16 && *val >= -8 && *val <= 7 => Ok(Precompiled(Opcode::par(Ads,*dst,*arg1,*val as _), None, None)),
                _ => Err(("Expected 2 full register names and immediate from range [-8,7]".to_string(), args)),
            }
        }
        "and" => return opcode_regs!(args,3,And),
        "or" => return opcode_regs!(args,3,Or),
        "xor" => return opcode_regs!(args,3,Xor),
        "eq" => return opcode_regs!(args,3,CmpEq),
        "ne" => return opcode_regs!(args,3,CmpNe),
        "lt" => return opcode_regs!(args,3,CmpLt),
        "gt" => return opcode_regs!(args,3 rev,CmpLt),
        "le" => return opcode_regs!(args,3 rev,CmpGe),
        "ge" => return opcode_regs!(args,3,CmpGe),
        "lts" => return opcode_regs!(args,3,CmpLts),
        "gts" => return opcode_regs!(args,3 rev,CmpLts),
        "les" => return opcode_regs!(args,3 rev,CmpGes),
        "ges" => return opcode_regs!(args,3,CmpGes),
        "movw" => return match args.as_slice() {
            [Name(_, Ok((dst, All))), Name(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
            | [Name(_, Ok((dst, All))), Index(_, Ok((arg1, All))), Name(_, Ok((arg2, All)))]
            if *dst < 16 && *arg1 < 16 && *arg2 < 16 => Ok(Precompiled(Opcode::par(Movw,*dst,*arg1,*arg2), None, None)),
            _ => Err(("Expected 3 full register names".to_string(), args)),
        },
        "cmov" => return opcode_regs!(args,3,Cmov),
        "cmovb" => return opcode_regs!(args,3,Cmovb),
        "cldi" => {
            return match args.as_slice() {
                [Name(_, Ok((dst, All))), Number(_, Ok(val)), Name(_, Ok((cond, All)))] if *dst < 16 && *cond < 16 =>{
                    if *val > u16::MAX as _ || *val < i16::MIN as _ { Err((format!("Immediate {:?} out of range",val),vec![args[1].clone()])) }
                    else { Ok(Precompiled(Opcode::par(Movx,*dst,*cond,0), Some(Opcode::from(*val as u16)), None)) }
                },
                [Name(_, Ok((dst, All))), Label(lab), Name(_, Ok((cond, All)))]
                if *dst < 16 && *cond < 16 => Ok(Precompiled(Opcode::par(Movx,*dst,*cond,0), None, Some(lab))),
                _ => Err(("Expected 2 full register names and 16-bit immediate".to_string(), args)),
            }
        },
        "ldi" => {
            return match args.as_slice() {
                [Name(_, Ok((dst, All))), Number(_, Ok(val))] if *dst < 16 =>{
                    if *val > u16::MAX as _ || *val < i16::MIN as _ { Err((format!("Immediate {:?} out of range",val),vec![args[1].clone()])) }
                    else { Ok(Precompiled(Opcode::par(Movx,*dst,0,0), Some(Opcode::from(*val as u16)), None)) }
                },
                [Name(_, Ok((dst, All))), Label(lab)] if *dst < 16 => Ok(Precompiled(Opcode::par(Movx,*dst,0,0), None, Some(lab))),
                _ => Err(("Expected 2 full register names and 16-bit immediate".to_string(), args)),
            }
        }
        "shr" => return opcode_regs!(args,2,Movx,1),
        "ashr" => return opcode_regs!(args,2,Movx,2),
        "shl" => return opcode_regs!(args,2,Movx,3),
        "movz" => {
            return match args.as_slice() {
                [Name(_, Ok(dst)), Name(_, Ok(src))] => mov(*dst, *src,false,|a,b,c,d|byte_copy_fill(a,b,c,d,false)).map_err(|s|(s, args)),
                _ => Err(("Expected 2 register names".to_string(), args)),
            }
        }
        "movs" => {
            return match args.as_slice() {
                [Name(_, Ok(dst)), Name(_, Ok(src))] => mov(*dst, *src,true,|a,b,c,d|byte_copy_fill(a,b,c,d,true)).map_err(|s|(s, args)),
                _ => Err(("Expected 2 register names".to_string(), args)),
            }
        }
        "not" => return opcode_regs!(args,2,Movx,14),
        "call" => return opcode_regs!(args,2,Movx,15),
        //common aliases
        "mov" => {
            return match args.as_slice() {
                [Name(_, Ok(dst)), Name(_, Ok(src))] => mov(*dst, *src,false,byte_copy_replace).map_err(|s|(s, args)),
                [Name(_, Ok(dst)), Index(_, Ok(src))] if dst.0 < 16 && src.0 < 16 => match (*dst,*src) {
                    ((0,All),(_,All)) => Ok(Precompiled(Opcode::from(0), None, None)), //nop
                    ((dst,All),(src,All)) => Ok(Precompiled(Opcode::par(Movw,dst,src,0), None, None)),
                    _ => Err(("Only full registers are allowed when indexing memory".to_string(), args))
                },
                [Name(_, Ok(dst)), Index(_, Ok(src))] => Err(("Allowed register are r0..r15".to_string(),args)),
                [Index(_, Ok(dst)), Name(_, Ok(src))] if dst.0 < 16 && src.0 < 16 => match (*dst,*src) {
                    ((dst,All),(src,All)) => Ok(Precompiled(Opcode::par(Movw,0,dst,src), None, None)),
                    _ => Err(("Only full registers are allowed when indexing memory".to_string(), args))
                },
                [Name(_, Ok(dst)), Number(_, Ok(val))] => match (*dst,*val) {
                    ((dst,All),val) if dst < 16 => {
                        if val > u16::MAX as _ || val < i16::MIN as _ { Err((format!("Immediate {:?} out of range",val),vec![args[1].clone()])) }
                        else {
                            if val & 0xfff8 == 0xfff8 || val & 0xfff8 == 0 {
                                Ok(Precompiled(Opcode::par(Ads,dst,0,(val & 0xf) as u16), None, None))
                            }else{
                                Ok(Precompiled(Opcode::par(Movx,dst,0,0), Some(Opcode::from(val as u16)), None))
                            }
                        }
                    }
                    ((_,All),_) => Err(("Allowed register are r0..r15".to_string(), args)),
                    _ => Err(("Only full registers are allowed when loading immediate value".to_string(), args))
                },
                [Name(_, Ok(dst)), Label(lab)] => match (*dst,lab) {
                    ((dst,All),lab) if dst < 16 => Ok(Precompiled(Opcode::par(Movx,dst,0,0), None, Some(lab))),
                    ((_,All),_) => Err(("Allowed register are r0..r15".to_string(), args)),
                    _ => Err(("Only full registers are allowed when loading immediate value".to_string(), args))
                },
                _ => Err(("Expected register/index followed by register/index/immediate/label".to_string(), args)),
            }
        }
        "jmp" => {
            return match args.as_slice() {
                [Name(_, Ok((src,All)))] if *src < 16 => Ok(Precompiled(Opcode::par(Or,15,*src,0), None, None)),
                [Label(lab)] => Ok(Precompiled(Opcode::par(Movx,15,0,0), None, Some(lab))),
                [Number(_,Ok(val))] => {
                    if *val > u16::MAX as _ || *val < i16::MIN as _ { Err((format!("Immediate {:?} out of range",val),args)) }
                    else { Ok(Precompiled(Opcode::par(Movx,15,0,0), Some(Opcode::from(*val as u16)), None)) }
                }
                _ => Err(("Expected 1 full register name".to_string(), args)),
            }
        }
        "jmpz" => {
            return match args.as_slice() {
                [Name(_, Ok((src,All))),Name(_, Ok((cond,All)))] if *src < 16 && *cond < 16 =>
                    Ok(Precompiled(Opcode::par(Cmov,15,*src,*cond), None, None)),
                [Label(lab),Name(_, Ok((cond,All)))] if *cond < 16 =>
                    Ok(Precompiled(Opcode::par(Movx,15,*cond,0), None, Some(lab))),
                [Number(_,Ok(val)),Name(_, Ok((cond,All)))] if *cond < 16 =>{
                    if *val > u16::MAX as _ || *val < i16::MIN as _ { Err((format!("Immediate {:?} out of range",val),args)) }
                    else { Ok(Precompiled(Opcode::par(Movx,15,*cond,0), Some(Opcode::from(*val as u16)), None)) }
                }
                _ => Err(("Expected 1 full register name".to_string(), args)),
            }
        }
        "halt" => {
            if !args.is_empty() { return Err(("Expected no arguments".to_string(),args))}
            return Ok(Precompiled(Opcode::par(Add,0,15,15),None,None));
        }
        _ => return Err((format!("Unknown mnemonic {:?}",mnem),args)),
    }
}


fn format_error(err: (&str,usize,usize))->String{
    let mut errors = format!(" at: {}:{}",err.1,err.2);
    writeln!(errors);
    let num = format!("{}:",err.1);
    let count = num.chars().count() + err.0[..err.2].chars().count();
    writeln!(errors,"{}{}",num,err.0);
    write!(errors,"{}^"," ".repeat(count));
    errors
}

pub fn compile_assembly(source: &str) ->Result<Vec<Opcode>,String>{
    use AsmArgument::*;
    let mut errors = String::new();
    let ops: Vec<_> = command_stream(source).map(|c|{
        match c {
            AsmCommand::Cmd(name,args) => precompile_command(name,args).map_err(|mut e|{ e.1.push(AsmArgument::Unknown(name));e }),
            AsmCommand::Label(lab) => Ok(Precompiled(Opcode::from(0),None,Some(lab))),
            AsmCommand::Error(err) => Err((format!("Syntax error: {:?}",err),vec![Unknown(err)])),
        }
    }).filter_map(|op|{
        //format errors from parsing
        match op {
            Ok(prec) => Some(prec),
            Err((text,args)) => {
                let err = args.first().map(|a|get_pos(source,match a { Unknown(s) | Number(s,_) | Name(s,_) | Label(s) | Index(s,_) => s }));
                write!(errors,"Syntax Error: {}",text);
                if let Some(pos) = err {
                    writeln!(errors,"{}",format_error(pos));
                }else{
                    writeln!(errors);
                }
                None
            }

        }
    }).collect();
    if !errors.is_empty() {
        return Err(errors.trim().to_string())
    }

    //link resolving phase
    let mut labels = HashMap::new();
    let mut repeated_label = Vec::new();
    let mut current = 0;
    let ops: Vec<_> = ops.into_iter().filter_map(|pre|{
        //find labels and save their addresses
        if pre.0 == Opcode::from(0) {
            if let (None,Some(label)) = (pre.1,pre.2) { //found label
                if let Some(_) = labels.insert(label,current) {
                    repeated_label.push(label);
                }
                //dont increment current
                return None; //remove label mark from list
            }
        }
        match pre {
            Precompiled(_,None,Some(_)) => current += 2, //2 words opcode with label
            Precompiled(_,Some(_),None) => current += 2, //regular 2 words opcode
            Precompiled(_,None,None) => current += 1, //1 word opcode
            _ => unreachable!(),
        }
        Some(pre)
    }).collect();
    if !repeated_label.is_empty() {
        return Err(repeated_label.into_iter().map(|lab|{
            format!("Link Error: Repeated label \"{}\"{}",lab,format_error(get_pos(source,lab)))
        }).collect::<Vec<_>>().join("\n"));
    }
    //paste label addresses in specific places
    let mut errors = String::new();
    let result = ops.into_iter().flat_map(|op|{
        if let (None,Some(label)) = (op.1,op.2) {
            let address = labels.get(label).copied();
            return if address.is_none() {
                writeln!(errors, "Link Error: Unknown label \"{}\"{}", label, format_error(get_pos(source, label)));
                vec![]
            } else if address.unwrap() > MAX_ADDRESS {
                writeln!(errors, "Link Error: Label out of max program bounds \"{}\"{}", label, format_error(get_pos(source, label)));
                vec![]
            } else {
                vec![op.0, Opcode::from(address.unwrap() as u16)]
            }
        }
        assert_eq!(op.2,None);
        match op.1 {
            Some(sec) => vec![op.0,sec],
            None => vec![op.0]
        }
    }).collect();
    if !errors.is_empty() {
        return Err(errors.trim().to_string())
    }
    Ok(result)
}


#[cfg(test)]
mod tests{
    use super::*;
    fn run_precompile(text: &str) ->Vec<Result<Precompiled, (String, Vec<AsmArgument>)>> {
        command_stream(text).map(|c|{
            match c {
                AsmCommand::Cmd(name,args) => precompile_command(name,args),
                AsmCommand::Label(lab) => Ok(Precompiled(Opcode::from(0),None,Some(lab))),
                v => panic!("Bad command {:?}",v),
            }
        }).collect()
    }
    fn unwrap_1<T>(arr: Vec<T>)->T{
        assert_eq!(arr.len(),1);
        arr.into_iter().next().unwrap()
    }
    fn unwrap_2<T>(arr: Vec<T>)->(T,T){
        assert_eq!(arr.len(),2);
        let mut it = arr.into_iter();
        let a = it.next().unwrap();
        (a,it.next().unwrap())
    }
    fn unwrap_3<T>(arr: Vec<T>)->(T,T,T){
        assert_eq!(arr.len(),3);
        let mut it = arr.into_iter();
        let a = it.next().unwrap();
        let b = it.next().unwrap();
        (a,b,it.next().unwrap())
    }
    #[test]
    fn test_get_pos(){
        let text = " \n ";
        assert!(matches!(get_pos(text,&text[0..0]),(_,1,0)));
        assert!(matches!(get_pos(text,&text[0..1]),(_,1,0)));
        assert!(matches!(get_pos(text,&text[0..3]),(_,1,0)));
        assert!(matches!(get_pos(text,&text[1..1]),(_,1,1)));
        assert!(matches!(get_pos(text,&text[1..2]),(_,1,1)));
        assert!(matches!(get_pos(text,&text[2..2]),(_,2,0)));
        assert!(matches!(get_pos(text,&text[2..3]),(_,2,0)));
        assert!(matches!(get_pos(text,&text[3..3]),(_,2,1)));
        let text = " \n";
        assert!(matches!(get_pos(text,&text[2..2]),(_,2,0)));
    }
    #[test]
    fn test_basic(){
        let res = run_precompile("nop");
        assert_eq!(res.len(),1);
        assert_eq!(res[0],Ok(Precompiled(Opcode::from(0),None,None)));
        let res = run_precompile("nop r1");
        assert!(matches!(res.as_slice(),[Err((_,args))] if args.as_slice() == [AsmArgument::Name("r1",Ok((1,RegByte::All)))]));
    }
}