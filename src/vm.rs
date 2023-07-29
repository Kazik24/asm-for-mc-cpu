use crate::emulator::*;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct VirtualMachine {
    ram: Box<[u16]>,
    cpu: CpuModel,
    ram_backup: Box<[u16]>,
}

impl VirtualMachine {
    pub fn new(size: usize) -> Self {
        Self { ram: vec![0; size].into_boxed_slice(), cpu: CpuModel::new(), ram_backup: Vec::new().into_boxed_slice() }
    }
    pub fn with(ram_size: usize, start_data: impl IntoIterator<Item = Opcode>) -> Self {
        let mut v = Self::new(ram_size);
        v.load_start(start_data);
        v
    }
    pub fn ram(&self) -> &[u16] {
        &self.ram
    }
    pub fn ram_mut(&mut self) -> &mut [u16] {
        &mut self.ram
    }
    pub fn load_start_data(&mut self, value: &[u16]) {
        self.ram[..value.len()].copy_from_slice(value);
        self.ram_backup = self.ram.clone();
    }
    pub fn load_start(&mut self, it: impl IntoIterator<Item = Opcode>) {
        it.into_iter().zip(self.ram.iter_mut()).for_each(|(a, b)| *b = a.into());
        self.ram_backup = self.ram.clone();
    }
    pub fn reset_ram(&mut self) {
        if self.ram.len() != self.ram_backup.len() {
            self.ram.fill(0);
        } else {
            self.ram = self.ram_backup.clone();
        }
    }
    pub fn cpu(&self) -> &CpuModel {
        &self.cpu
    }
    pub fn cpu_mut(&mut self) -> &mut CpuModel {
        &mut self.cpu
    }

    pub fn tick_times(&mut self, times: usize, print: bool, print_ram: bool) -> bool {
        for i in 0..times {
            let res = self.tick(print_ram);
            if print {
                println!("Tick {}:{:#?}", i, self.cpu);
            }
            if !res {
                return false;
            }
        }
        true
    }
    pub fn resume(&mut self) {
        self.cpu.halt = false;
    }
    pub fn tick(&mut self, print_ram: bool) -> bool {
        let address = self.cpu.out_address as usize & 0x7fff;
        match (self.cpu.out_read, self.cpu.out_write) {
            (true, false) => {
                self.cpu.inout_data = self.ram.get(address).copied().unwrap_or(0);
                if print_ram {
                    println!(
                        "read  ram[{}]: {:>5}: {:04X}[{}]",
                        address,
                        self.cpu.inout_data,
                        self.cpu.inout_data,
                        Opcode::from(self.cpu.inout_data)
                    );
                }
            }
            (false, true) if address < self.ram.len() => {
                self.ram[address] = self.cpu.inout_data;
                if print_ram {
                    println!(
                        "write ram[{}] ={:>5}: {:04X}[{}]",
                        address,
                        self.cpu.inout_data,
                        self.cpu.inout_data,
                        Opcode::from(self.cpu.inout_data)
                    );
                }
            }
            (false, true) => {
                //write to non existing cell
                if print_ram {
                    println!("write ram[{}] not exist", address);
                }
            }
            v => unreachable!("unknown read/write signal state {:?}", v),
        }
        //clear flags (those must be set by cpu)
        self.cpu.out_read = false;
        self.cpu.out_write = false;
        self.cpu.tick();
        !self.cpu.halt
    }
}

#[cfg(test)]
mod tests {

    use crate::*;
    use rand::rngs::StdRng;
    use rand::{Rng, SeedableRng};
    use std::cmp::Ordering;

    const QUICKSORT: &str = "
            nop


            mov r1,#{start} ;start address (inclusive)
            mov r2,#{end} ;end address (inclusive)


            mov r13,#0x1FFE ;stack pointer
            ;r14 is return address for call convention


            call r14,@QuickSort
            halt
            nop
            nop
            kill
            nop

        QuickSort:
            lt   r12,r1,r2
            jmpz r14,r12 ;conditional return

            mov  r3,[r2] ;pivot (last element)
            mov  r4,r1   ; initial pivot index
            mov  r5,r1   ; loop index (j)
        QSLoopStart:
            lt   r12,r5,r2
            jmpz @QSLoopEnd,r12
            mov  r6,[r5]
            le   r12,r6,r3  ;main compare operation (determines ordering)
            jmpz @QSIncNext,r12
            mov  r7,[r4]
            mov  [r4],r6
            mov  [r5],r7
            add  r4,r4,#2
        QSIncNext:
            add  r5,r5,#2
            jmp  @QSLoopStart
        QSLoopEnd:
            mov  r5,[r4]
            mov  [r4],r3
            mov  [r2],r5

            ;push return address
            mov  [r13],r14
            add  r13,r13,#-2
            ;push pivot index on stack
            mov  [r13],r4
            add  r13,r13,#-2
            ;push end on stack
            mov  [r13],r2
            add  r13,r13,#-2
            ;set end to pivot index - 2 and call
            add  r2,r4,#-2
            call r14,@QuickSort
            ;pop end
            add  r13,r13,#2
            mov  r2,[r13]
            ;pop pivot
            add  r13,r13,#2
            mov  r1,[r13]
            ;set start to pivot index + 2 and call
            add  r1,r1,#2
            call r14,@QuickSort
            ;pop address and return
            add  r13,r13,#2
            mov  r15,[r13] ;ret
        ";

    #[test]
    fn test_quicksort() {
        let start_cell = 200;
        let end_cell = 400;

        let data_range = start_cell..end_cell;

        let text = QUICKSORT
            .replace("{start}", (data_range.start * 2).to_string().as_str())
            .replace("{end}", (data_range.end * 2).to_string().as_str());

        let opcodes = compile_assembly(&text).unwrap();
        let mut m = VirtualMachine::new(500);
        m.load_start(opcodes.clone());
        let array = &mut m.ram_mut()[data_range.clone()];
        StdRng::seed_from_u64(1234).fill(array);
        launch_emulator_gui(m);

        let data_range = start_cell..end_cell;
        let mut rand = StdRng::seed_from_u64(1234);
        for _ in 0..1 {
            let mut vm = VirtualMachine::new(32000);
            vm.load_start(opcodes.clone());

            let array = &mut vm.ram_mut()[data_range.clone()];
            rand.fill(array);
            //println!("Init: {:?}",array);

            while vm.tick_times(10000, false, false) {
                println!("Executing...");
            }

            let array = &vm.ram()[data_range.clone()];
            //println!("Result: {:?}",array);
            assert!(array
                .windows(2)
                .all(|w| { w[0].partial_cmp(&w[1]).map(|o| o != Ordering::Greater).unwrap_or(false) }));
        }
    }
}
