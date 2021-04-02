
use num_derive::*;
use num_traits::FromPrimitive;
use std::fmt::{Debug, Formatter};


#[derive(Copy,Clone,Eq,PartialEq,Hash)]
pub struct Opcode{
    bits: u16,
}
impl Debug for Opcode{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{:04X}[{:?} r:{},a:{},b:{}]",self.bits,self.cmd(),self.r().as_u16(),self.a().as_u16(),self.b().as_u16())
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
    Add = 0,Sub,Ads,And,Or,Xor,CmpEq,CmpNe,CmpLt,CmpGe,CmpLts,CmpGes,Movw,Cmov,Cmovb,Movx
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum Arg{
    R0 = 0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15
}
impl Arg{
    pub fn signext(self)->u16 {
        let a = self.as_u16();
        if a & 0x8 != 0 { (a | 0xfff0) } else { a }
    }
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
    pub fn cmd(self)->Command { Command::from_u16(self.bits & 0xf).unwrap() }
    pub fn r(self)->Arg { Arg::from_u16((self.bits >> 4) & 0xf).unwrap() }
    pub fn a(self)->Arg { Arg::from_u16((self.bits >> 8) & 0xf).unwrap() }
    pub fn b(self)->Arg { Arg::from_u16((self.bits >> 12) & 0xf).unwrap() }
}

#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug)]
#[repr(u8)]
pub enum Mux1{ StateInc, StatePass }
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug)]
#[repr(u8)]
pub enum Mux2{ StateDecode, StateSaveToReg, StateSkip}

#[derive(Clone,Debug)]
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
    pub fn tick(&mut self){
        use Command::*;
        self.reg[0] = 0; //r0 is hardwired to 0
        match self.mux1 {
            Mux1::StateInc => self.reg[15] += 1, //increment program counter
            Mux1::StatePass => {}
        }

        match self.mux2 {
            Mux2::StateDecode => {
                self.decoded_opcode = Opcode::from(self.inout_data);
                let a = self.reg[self.decoded_opcode.a().as_usize()];
                let b = self.reg[self.decoded_opcode.b().as_usize()];
                match self.decoded_opcode.cmd() {
                    Add => {
                        self.reg[self.decoded_opcode.r().as_usize()] = a.wrapping_add(b);
                        self.mem_read_pc();
                    }
                    Sub => {
                        self.reg[self.decoded_opcode.r().as_usize()] = a.wrapping_sub(b);
                        self.mem_read_pc();
                    }
                    Ads => {
                        let b = self.decoded_opcode.b().signext();
                        self.reg[self.decoded_opcode.r().as_usize()] = a.wrapping_add(b);
                        self.mem_read_pc();
                    }
                    And => {
                        self.reg[self.decoded_opcode.r().as_usize()] = a & b;
                        self.mem_read_pc();
                    }
                    Or => {
                        self.reg[self.decoded_opcode.r().as_usize()] = a | b;
                        self.mem_read_pc();
                    }
                    Xor => {
                        self.reg[self.decoded_opcode.r().as_usize()] = a ^ b;
                        self.mem_read_pc();
                    }
                    CmpEq => {
                        self.reg[self.decoded_opcode.r().as_usize()] = (a == b) as _;
                        self.mem_read_pc();
                    }
                    CmpNe => {
                        self.reg[self.decoded_opcode.r().as_usize()] = (a != b) as _;
                        self.mem_read_pc();
                    }
                    CmpLt => {
                        self.reg[self.decoded_opcode.r().as_usize()] = (a < b) as _;
                        self.mem_read_pc();
                    }
                    CmpGe => {
                        self.reg[self.decoded_opcode.r().as_usize()] = (a >= b) as _;
                        self.mem_read_pc();
                    }
                    CmpLts => {
                        self.reg[self.decoded_opcode.r().as_usize()] = ((a as i16) < (b as i16)) as _;
                        self.mem_read_pc();
                    }
                    CmpGes => {
                        self.reg[self.decoded_opcode.r().as_usize()] = ((a as i16) >= (b as i16)) as _;
                        self.mem_read_pc();
                    }
                    Movw => {
                        self.out_address = a >> 1;
                        self.mux1 = Mux1::StatePass;
                        if self.decoded_opcode.r().as_u16() == 0 {
                            self.inout_data = b;
                            self.out_read = false;
                            self.out_write = true;
                            self.mux2 = Mux2::StateSkip;
                        }else{
                            self.out_read = true;
                            self.out_write = false;
                            self.mux2 = Mux2::StateSaveToReg;
                        }
                    }
                    Cmov => {
                        if b & 1 == 0 {
                            self.reg[self.decoded_opcode.r().as_usize()] = a;
                        }
                        self.mem_read_pc();
                    }
                    Cmovb => {
                        let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                        if b & 1 == 0 {
                            *r = (*r & 0xff00) | (a & 0x00ff);
                        }else{
                            *r = (*r & 0x00ff) | (a << 8);
                        }
                        self.mem_read_pc();
                    }
                    Movx => match self.decoded_opcode.b() {
                        Arg::R0 => { //store conditional
                            self.mux2 = if a & 1 == 0 { Mux2::StateSaveToReg }else{ Mux2::StateSkip };
                            self.mem_read_pc();
                        }
                        Arg::R1 => { //logic shr
                            self.reg[self.decoded_opcode.r().as_usize()] = a >> 1;
                            self.mem_read_pc();
                        }
                        Arg::R2 => { //arithmetic shr
                            self.reg[self.decoded_opcode.r().as_usize()] = ((a as i16) >> 1) as u16;
                            self.mem_read_pc();
                        }
                        Arg::R3 => { //logic shr
                            self.reg[self.decoded_opcode.r().as_usize()] = a << 1;
                            self.mem_read_pc();
                        }
                        //byte mov with sign or zero extension
                        Arg::R4 => {//set high to low zero extend
                            self.reg[self.decoded_opcode.r().as_usize()] = a >> 8;
                            self.mem_read_pc();
                        }
                        Arg::R5 => {//set high to low sign extended
                            self.reg[self.decoded_opcode.r().as_usize()] = ((a as i16) >> 8) as u16;
                            self.mem_read_pc();
                        }
                        Arg::R6 => {//set low to low zero extend
                            self.reg[self.decoded_opcode.r().as_usize()] = a & 0x00ff;
                            self.mem_read_pc();
                        }
                        Arg::R7 => {//set low to low sign extend
                            self.reg[self.decoded_opcode.r().as_usize()] = ((a as i8) as i16) as u16;
                            self.mem_read_pc();
                        }
                        //byte manipulation
                        Arg::R8 => {//reg[r][7:0] = reg[a][7:0]
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0xff00) | (a & 0x00ff);
                            self.mem_read_pc();
                        }
                        Arg::R9 => {//reg[r][15:8] = reg[a][7:0]
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0x00ff) | (a << 8);
                            self.mem_read_pc();
                        }
                        Arg::R10 => {//reg[r][7:0] = reg[a][15:8]
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0xff00) | (a >> 8);
                            self.mem_read_pc();
                        }
                        Arg::R11 => {//reg[r][15:8] = reg[a][15:8]
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0x00ff) | (a & 0xff00);
                            self.mem_read_pc();
                        }
                        //utility
                        Arg::R12 => {//logic shl 8
                            self.reg[self.decoded_opcode.r().as_usize()] = a << 8;
                            self.mem_read_pc();
                        }
                        Arg::R13 => {// a & 0xff00
                            self.reg[self.decoded_opcode.r().as_usize()] = a & 0xff00;
                            self.mem_read_pc();
                        }
                        Arg::R14 => {//negate
                            self.reg[self.decoded_opcode.r().as_usize()] = !a;
                            self.mem_read_pc();
                        }
                        Arg::R15 => {//call link
                            self.reg[self.decoded_opcode.r().as_usize()] = self.reg[15];
                            self.reg[15] = a;
                            self.mem_read_pc();
                        }
                        op => panic!("Unimplemented Movx variant: {:?}",op),
                    }
                    op => panic!("Unimplemented opcode: {:?}",op),
                }
            }
            Mux2::StateSaveToReg => { //read data from ram
                self.reg[self.decoded_opcode.r().as_usize()] = self.inout_data;
                self.mux1 = Mux1::StateInc;
                self.mux2 = Mux2::StateDecode;
                self.out_read = true;
                self.out_write = false;
                self.out_address = self.reg[15];
            }
            Mux2::StateSkip => {
                self.mux1 = Mux1::StateInc;
                self.mux2 = Mux2::StateDecode;
                self.out_read = true;
                self.out_write = false;
                self.out_address = self.reg[15];
            }
        }

    }

}