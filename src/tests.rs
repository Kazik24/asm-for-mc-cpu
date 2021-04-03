use crate::*;
use crate::assembler::*;



#[test]
fn test_compile(){
    let text = r#"
        nop ; must be at start cause core starts execution from word 1 (address 2)
        LDI  r1,#32     ; start address (inclusive)
        LDI  r2,#500   ; length
        LDI  r3,#69     ; value to fill
    Loop:
        MOV  [r1],r3    ; write to memory at address
        EQ   r4,r0,r2   ; check if length is 0
        ADS  r2,r2,#-1  ; decrement length
        ADS  r1,r1,#2   ; increase address by 2
        JMPZ @Loop,r4     ; jump if condition was false

    Halt:
        jmp @Halt
    ;other code
        mov r2,#10000
    TestLoop:
        ads r1,r1,#3
        ads r2,r2,#1
        jmp @TestLoop
    "#;


    let ops = compile_assembly(text).unwrap_or_else(|err|{
        println!("{}",err);
        Vec::new()
    });
    //println!("{:?}",ops);

    let mut vm = VirtualMachine::new(1024*4);
    vm.load_start(ops);
    vm.tick_times(2048,false,false);
    println!("{:#?}",vm.cpu());
    println!("{}",vm.ram()[532]);
}