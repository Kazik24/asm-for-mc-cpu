
use num_derive::*;
use num_traits::FromPrimitive;
use std::fmt::{Debug, Formatter, Display};


#[derive(Copy,Clone,Eq,PartialEq,Hash)]
pub struct Opcode{
    bits: u16,
}
impl Debug for Opcode{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{:04X}[{:?}",self.bits,self.cmd())?;
        if self.cmd() == Command::Ex {
            write!(f,".{:?} r:{},a:{}]",self.b_sub(),self.r().as_u16(),self.a().as_u16())
        }else{
            write!(f," r:{},a:{},b:{}]",self.r().as_u16(),self.a().as_u16(),self.b().as_u16())
        }

    }
}
impl Display for Opcode{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{:?}",self.cmd())?;
        match self.cmd() {
            Command::Ads => write!(f," {},{},#{}",self.r().name(),self.a().name(),self.b().signext() as i16),
            Command::Ex => write!(f,".{:?} {},{}",self.b_sub(),self.r().name(),self.a().name()),
            _ => write!(f," {},{},{}",self.r().name(),self.a().name(),self.b().name()),
        }
    }
}
impl From<u16> for Opcode{
    fn from(bits: u16) -> Self { Self{ bits } }
}
impl From<Opcode> for u16 {
    fn from(opc: Opcode) -> Self { opc.bits }
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum Command{
    Or = 0,And,Xor,Add,Ads,Sub,CmpLt,CmpGe,CmpLts,CmpGes,CmpEq,CmpNe,Movw,Cmov,Cmovb,Ex
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum SubCommand{
    Shr = 0,Ashr,Shl,Cldi,SetHLZ,SetHLS,SetLLZ,SetLLS,MLL,MLH,MHL,MHH,SetLHZ,SetHHZ,Not,Call
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum Arg{
    R0 = 0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15
}
impl Arg{
    pub fn signext(self)->u16 {
        let a = self.as_u16();
        if a & 0x8 != 0 { a | 0xfff0 } else { a }
    }
    pub fn name(self)->String { format!("{:?}",self).to_lowercase()}
    pub fn as_u16(self)->u16 { self as _ }
    pub fn as_usize(self)->usize { self as _ }
}
impl Opcode{
    pub fn new(cmd: Command,r: Arg,a: Arg,b: Arg)->Self{ Self::par(cmd,r as u16,a as u16,b as u16) }
    pub fn par(cmd: Command,r: u16,a: u16,b: u16)->Self{
        let mut bits = cmd as u16;
        bits |= (r & 0xf) << 4;
        bits |= (a & 0xf) << 8;
        bits |= (b & 0xf) << 12;
        Self{ bits }
    }
    pub fn ex(r: u16,a: u16,b: SubCommand)->Self{
        Self::par(Command::Ex,r,a,b as u16)
    }
    pub fn cmd(self)->Command { Command::from_u16(self.bits & 0xf).unwrap() }
    pub fn r(self)->Arg { Arg::from_u16((self.bits >> 4) & 0xf).unwrap() }
    pub fn a(self)->Arg { Arg::from_u16((self.bits >> 8) & 0xf).unwrap() }
    pub fn b(self)->Arg { Arg::from_u16((self.bits >> 12) & 0xf).unwrap() }
    pub fn b_sub(self)->SubCommand { SubCommand::from_u16((self.bits >> 12) & 0xf).unwrap() }
    pub fn bits(self)->u16{self.bits}
    pub fn word_count(self)->usize{
        match (self.cmd(),self.b_sub()) {
            (Command::Ex,SubCommand::Cldi) => 2,
            (Command::Ex,SubCommand::Call) => 2,
            _ => 1,
        }
    }
}

#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug)]
#[repr(u8)]
pub enum Mux1{ StateInc, StatePass }
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug)]
#[repr(u8)]
pub enum Mux2{ StateDecode, StateSaveToReg(bool), StateSkip}

#[derive(Clone,Eq,PartialEq,Hash,Debug)]
pub struct CpuModel{
    pub reg: [u16;16],
    pub mux1: Mux1,
    pub mux2: Mux2,
    pub inout_data: u16,
    pub decoded_opcode: Opcode,
    pub out_address: u16,
    pub out_read: bool,
    pub out_write: bool,
}


impl CpuModel{

    pub fn new()->Self{
        let mut v = Self{
            reg: [0;16],
            mux1: Mux1::StateInc,
            mux2: Mux2::StateDecode,
            inout_data: 0,
            decoded_opcode: Opcode::from(0),
            out_address: 1, //start from address 1
            out_read: true,
            out_write: false
        };
        v.reg[15] = 1;
        v
    }
    pub fn reset(&mut self){ *self = Self::new() }

    fn mem_read_pc(&mut self){
        self.out_address = self.reg[15];
        self.out_read = true;
        self.out_write = false;
    }
    fn decode_opcode(&mut self)->(u16,u16){
        self.decoded_opcode = Opcode::from(self.inout_data);
        let a = self.reg[self.decoded_opcode.a().as_usize()];
        let b = self.reg[self.decoded_opcode.b().as_usize()];
        if self.decoded_opcode.cmd() == Command::Ex {
            return (a,self.reg[self.decoded_opcode.r().as_usize()]);
        }
        (a,b)
    }
    fn r_write(&mut self,value: u16){
        self.reg[self.decoded_opcode.r().as_usize()] = value;
    }
    #[allow(unreachable_patterns)]
    pub fn tick(&mut self){
        use Command::*;
        use SubCommand::*;
        self.reg[0] = 0; //r0 is hardwired to 0

        //first multiplex
        match self.mux1 {
            Mux1::StateInc => self.reg[15] += 1, //increment program counter
            Mux1::StatePass => {}
        }


        let alu_output;
        let mut reg_write = true;
        let mut read_pc = true;
        //second multiplex
        match self.mux2 {
            Mux2::StateDecode => {
                let (a,b) = self.decode_opcode();
                match self.decoded_opcode.cmd() {
                    And => alu_output = a & b,
                    Or => alu_output = a | b,
                    Xor => alu_output = a ^ b,
                    Add => alu_output = a.wrapping_add(b),
                    Ads => alu_output = a.wrapping_add(self.decoded_opcode.b().signext()),
                    Sub => alu_output = a.wrapping_sub(b),
                    CmpEq => alu_output = (a == b) as _,
                    CmpNe => alu_output = (a != b) as _,
                    CmpLt => alu_output = (a < b) as _,
                    CmpGe => alu_output = (a >= b) as _,
                    CmpLts => alu_output = ((a as i16) < (b as i16)) as _,
                    CmpGes => alu_output = ((a as i16) >= (b as i16)) as _,
                    Movw => {
                        alu_output = 0;//dummy
                        self.out_address = a >> 1;
                        self.mux1 = Mux1::StatePass; //2 cycles so don't increment pc in next tick
                        if self.decoded_opcode.r().as_u16() == 0 { //decoded index points to void reg
                            self.inout_data = b;
                            self.out_read = false;
                            self.out_write = true; //write ram
                            self.mux2 = Mux2::StateSkip;
                        }else{
                            self.out_read = true; //read ram
                            self.out_write = false;
                            self.mux2 = Mux2::StateSaveToReg(false);
                        }
                        read_pc = false;
                    }
                    Cmov => {
                        alu_output = a;
                        reg_write = b & 1 == 0; //if not fulfilled then write to 0 (black hole register)
                    }
                    Cmovb => {
                        reg_write = false;//this po will self decode register
                        alu_output = 0;//dummy value
                        let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                        if b & 1 == 0 {
                            *r = (*r & 0xff00) | (a & 0x00ff);
                        }else{
                            *r = (*r & 0x00ff) | (a << 8);
                        }
                    }
                    Ex => match self.decoded_opcode.b_sub() {
                        Cldi => { //store conditional
                            alu_output = 0;//<don't care> value
                            reg_write = false;
                            self.mux2 = if a & 1 == 0 { Mux2::StateSaveToReg(false) }else{ Mux2::StateSkip };
                        }
                        Shr => alu_output = a >> 1, //logic shr
                        Ashr => alu_output = ((a as i16) >> 1) as u16, //arithmetic shr
                        Shl => alu_output = a << 1, //logic shl
                        //byte mov with sign or zero extension
                        SetHLZ => alu_output = a >> 8, //set high to low zero extend
                        SetHLS => alu_output = ((a as i16) >> 8) as u16,//set high to low sign extended
                        SetLLZ => alu_output = a & 0x00ff,//set low to low zero extend
                        SetLLS => alu_output = ((a as i8) as i16) as u16,//set low to low sign extend
                        //byte manipulation
                        MLL => alu_output = (b & 0xff00) | (a & 0x00ff),//reg[r][7:0] = reg[a][7:0]
                        MLH => alu_output = (b & 0x00ff) | (a << 8),//reg[r][15:8] = reg[a][7:0]
                        MHL => alu_output = (b & 0xff00) | (a >> 8),//reg[r][7:0] = reg[a][15:8]
                        MHH => alu_output = (b & 0x00ff) | (a & 0xff00),//reg[r][15:8] = reg[a][15:8]
                        //utility
                        SetLHZ => alu_output = a << 8,//logic shl 8
                        SetHHZ => alu_output = a & 0xff00,// a & 0xff00
                        Not => alu_output = !a,//negate
                        Call => {//call link
                            alu_output = 0;//<don't care> value
                            reg_write = false;
                            self.mux2 = if a & 1 == 0 { Mux2::StateSaveToReg(true) }else{ Mux2::StateSkip };
                        }
                        op => panic!("Unimplemented Movx variant: {:?}",op),
                    }
                    op => panic!("Unimplemented opcode: {:?}",op),
                }
                if reg_write {
                    self.r_write(alu_output);
                }
                if read_pc {
                    self.mem_read_pc();
                }
            }
            Mux2::StateSaveToReg(call) => { //read data from ram
                self.mux1 = Mux1::StateInc;
                self.mux2 = Mux2::StateDecode;
                let to_write;
                if call {
                    to_write = self.reg[15];
                    self.reg[15] = self.inout_data;
                }else{
                    to_write = self.inout_data;
                }
                self.r_write(to_write);
                self.mem_read_pc();
            }
            Mux2::StateSkip => {
                self.mux1 = Mux1::StateInc;
                self.mux2 = Mux2::StateDecode;
                self.mem_read_pc();
            }
        }

    }

}