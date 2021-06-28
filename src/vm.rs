use crate::emulator::*;

#[derive(Clone,Eq,PartialEq,Hash,Debug)]
pub struct VirtualMachine{
    ram: Box<[u16]>,
    cpu: CpuModel
}


impl VirtualMachine{


    pub fn new(size: usize)->Self{
        Self{
            ram: vec![0;size].into_boxed_slice(),
            cpu: CpuModel::new(),
        }
    }
    pub fn with(ram_size: usize,start_data: impl IntoIterator<Item=Opcode>)->Self{
        let mut v = Self::new(ram_size);
        v.load_start(start_data);
        v
    }
    pub fn ram(&self)->&[u16] {&self.ram}
    pub fn ram_mut(&mut self)->&mut [u16] {&mut self.ram}
    pub fn load_start_data(&mut self,value: &[u16]){ self.ram[..value.len()].copy_from_slice(value); }
    pub fn load_start(&mut self,it: impl IntoIterator<Item=Opcode>){
        it.into_iter().zip(self.ram.iter_mut()).for_each(|(a,b)|*b = a.into());
    }
    pub fn cpu(&self)->&CpuModel {&self.cpu}
    pub fn cpu_mut(&mut self)->&mut CpuModel {&mut self.cpu}


    pub fn tick_times(&mut self,times: usize,print: bool,print_ram: bool){
        for i in 0..times {
            self.tick(print_ram);
            if print { println!("Tick {}:{:#?}",i,self.cpu);}
        }
    }
    pub fn tick(&mut self,print_ram: bool){
        let address = self.cpu.out_address as usize & 0x7fff;
        match (self.cpu.out_read,self.cpu.out_write) {
            (true,false) => {
                self.cpu.inout_data = self.ram.get(address).copied().unwrap_or(0);
                if print_ram { println!("read  ram[{}]: {:>5}: {:04X}[{}]",address,self.cpu.inout_data,self.cpu.inout_data,Opcode::from(self.cpu.inout_data)); }
            }
            (false,true) if address < self.ram.len() => {
                self.ram[address] = self.cpu.inout_data;
                if print_ram { println!("write ram[{}] ={:>5}: {:04X}[{}]",address,self.cpu.inout_data,self.cpu.inout_data,Opcode::from(self.cpu.inout_data)); }
            }
            (false,true) => {//write to non existing cell
                if print_ram { println!("write ram[{}] not exist",address); }
            }
            v => unreachable!("unknown read/write signal state {:?}",v),
        }
        //clear flags (those must be set by cpu)
        self.cpu.out_read = false;
        self.cpu.out_write = false;
        self.cpu.tick();
    }
}