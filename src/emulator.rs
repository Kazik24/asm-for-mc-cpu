
use num_derive::*;
use num_traits::FromPrimitive;
use std::fmt::{Debug, Formatter};


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

impl From<u16> for Opcode{
    fn from(bits: u16) -> Self { Self{ bits } }
}
impl From<Opcode> for u16 {
    fn from(opc: Opcode) -> Self { opc.bits }
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum Command{
    Add = 0,Sub,Ads,And,Or,Xor,CmpEq,CmpNe,CmpLt,CmpGe,CmpLts,CmpGes,Movw,Cmov,Cmovb,Ex
}
#[derive(Copy,Clone,Eq,PartialEq,Hash,Debug,FromPrimitive)]
#[repr(u8)]
pub enum SubCommand{
    Cldi = 0,Shr,Ashr,Shl,SetHLZ,SetHLS,SetLLZ,SetLLS,MLL,MLH,MHL,MHH,SetLHZ,SetHHZ,Not,Call
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
    pub fn b_sub(self)->SubCommand { SubCommand::from_u16((self.bits >> 12) & 0xf).unwrap() }
    pub fn bits(self)->u16{self.bits}
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
    fn decode_opcode(&mut self)->(u16,u16){
        self.decoded_opcode = Opcode::from(self.inout_data);
        let a = self.reg[self.decoded_opcode.a().as_usize()];
        let b = self.reg[self.decoded_opcode.b().as_usize()];
        (a,b)
    }
    fn r_write(&mut self,value: u16){
        self.reg[self.decoded_opcode.r().as_usize()] = value;
    }
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

        //second multiplex
        match self.mux2 {
            Mux2::StateDecode => {
                let (a,b) = self.decode_opcode();
                match self.decoded_opcode.cmd() {
                    Add => {
                        alu_output = a.wrapping_add(b);
                        self.mem_read_pc();
                    }
                    Sub => {
                        alu_output = a.wrapping_sub(b);
                        self.mem_read_pc();
                    }
                    Ads => {
                        let b = self.decoded_opcode.b().signext();
                        alu_output = a.wrapping_add(b);
                        self.mem_read_pc();
                    }
                    And => {
                        alu_output = a & b;
                        self.mem_read_pc();
                    }
                    Or => {
                        alu_output = a | b;
                        self.mem_read_pc();
                    }
                    Xor => {
                        alu_output = a ^ b;
                        self.mem_read_pc();
                    }
                    CmpEq => {
                        alu_output = (a == b) as _;
                        self.mem_read_pc();
                    }
                    CmpNe => {
                        alu_output = (a != b) as _;
                        self.mem_read_pc();
                    }
                    CmpLt => {
                        alu_output = (a < b) as _;
                        self.mem_read_pc();
                    }
                    CmpGe => {
                        alu_output = (a >= b) as _;
                        self.mem_read_pc();
                    }
                    CmpLts => {
                        alu_output = ((a as i16) < (b as i16)) as _;
                        self.mem_read_pc();
                    }
                    CmpGes => {
                        alu_output = ((a as i16) >= (b as i16)) as _;
                        self.mem_read_pc();
                    }
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
                            self.mux2 = Mux2::StateSaveToReg;
                        }
                    }
                    Cmov => {
                        alu_output = a;
                        reg_write = b & 1 == 0; //if not fulfilled then write to 0 (black hole register)
                        self.mem_read_pc();
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
                        self.mem_read_pc();
                    }
                    Ex => match self.decoded_opcode.b_sub() {
                        Cldi => { //store conditional
                            alu_output = 0;//<don't care> value
                            reg_write = false;
                            self.mux2 = if a & 1 == 0 { Mux2::StateSaveToReg }else{ Mux2::StateSkip };
                            //todo, currently works wrong (allways stores 0 in reg[r])
                            self.mem_read_pc();
                        }
                        Shr => { //logic shr
                            alu_output = a >> 1;
                            self.mem_read_pc();
                        }
                        Ashr => { //arithmetic shr
                            alu_output = ((a as i16) >> 1) as u16;
                            self.mem_read_pc();
                        }
                        Shl => { //logic shl
                            alu_output = a << 1;
                            self.mem_read_pc();
                        }
                        //byte mov with sign or zero extension
                        SetHLZ => {//set high to low zero extend
                            alu_output = a >> 8;
                            self.mem_read_pc();
                        }
                        SetHLS => {//set high to low sign extended
                            alu_output = ((a as i16) >> 8) as u16;
                            self.mem_read_pc();
                        }
                        SetLLZ => {//set low to low zero extend
                            alu_output = a & 0x00ff;
                            self.mem_read_pc();
                        }
                        SetLLS => {//set low to low sign extend
                            alu_output = ((a as i8) as i16) as u16;
                            self.mem_read_pc();
                        }
                        //byte manipulation
                        MLL => {//reg[r][7:0] = reg[a][7:0]
                            alu_output = 0;//<don't care> value
                            reg_write = false;//this op will self decode register
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0xff00) | (a & 0x00ff);
                            self.mem_read_pc();
                        }
                        MLH => {//reg[r][15:8] = reg[a][7:0]
                            alu_output = 0;//<don't care> value
                            reg_write = false;//this op will self decode register
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0x00ff) | (a << 8);
                            self.mem_read_pc();
                        }
                        MHL => {//reg[r][7:0] = reg[a][15:8]
                            alu_output = 0;//<don't care> value
                            reg_write = false;//this op will self decode register
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0xff00) | (a >> 8);
                            self.mem_read_pc();
                        }
                        MHH => {//reg[r][15:8] = reg[a][15:8]
                            alu_output = 0;//<don't care> value
                            reg_write = false;//this op will self decode register
                            let r = &mut self.reg[self.decoded_opcode.r().as_usize()];
                            *r = (*r & 0x00ff) | (a & 0xff00);
                            self.mem_read_pc();
                        }
                        //utility
                        SetLHZ => {//logic shl 8
                            alu_output = a << 8;
                            self.mem_read_pc();
                        }
                        SetHHZ => {// a & 0xff00
                            alu_output = a & 0xff00;
                            self.mem_read_pc();
                        }
                        Not => {//negate
                            alu_output = !a;
                            self.mem_read_pc();
                        }
                        Call => {//call link
                            alu_output = self.reg[15];
                            self.reg[15] = a;
                            self.mem_read_pc();
                        }
                        op => panic!("Unimplemented Movx variant: {:?}",op),
                    }
                    op => panic!("Unimplemented opcode: {:?}",op),
                }
                if reg_write {
                    self.r_write(alu_output);
                }
            }
            Mux2::StateSaveToReg => { //read data from ram
                self.mux1 = Mux1::StateInc;
                self.mux2 = Mux2::StateDecode;
                self.r_write(self.inout_data);
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