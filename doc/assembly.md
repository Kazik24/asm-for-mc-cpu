# Assembler Specification
___
[Binary Instruction Representation](binary_opcodes.md)

This section contains allowed opcodes that can be compiled from human-readable
assembly into binary machine code for _BedrockCore_.
## Syntax
### Mnemonics
Mnemonics are names of instructions, they are case-insensitive.
e.g `ADD`,`SUB`,`mov`.

### Labels
Labels are case-sensitive and always starts from alphabetic character or "_". 
They can mark specific address in code when followed by a colon ":". If used in
instruction (preceded by "@") it is converted to address of instruction
immediately after the mark.
```
SomeLabel:              ; define label name here
    MOV r1,r2
    ; ... other instructions ...
    LDI r3,@SomeLabel   ; load address of label into r3
```

### Immediate values and pointers
Sometimes we need to supply opcode argument as immediate value, register number,
or memory index.
```
    MOV  r1,r2          ; move value from register 2 to register 1
    MOV  r1,[r2]        ; move value from ram at address stored in register 2
                        ; to register 1
    LDI  r1,#1234       ; load constant value "1234" to register 1
                        ; (this instruction is 2 machine words long)
    ADS  r1,r0,#2       ; effectively load constant value "2" to register 1
                        ; (this instruction is only 1 machine word, cause
                        ; it really uses ADS (add small signed). So it
                        ; adds constant 2 to register 0 which is allways 0 and
                        ; stores value in register 1)
```

## Processor architecture
* Little endian 16-bit core.
* Von Neumann architecture (single shared memory for instructions and data).
* Memory bus is 16-bit and memory address is 15-bit so maximum amount of memory is
  64 kB.
* Machine word is 16-bit, most instructions are 1 word long. Some instructions also require
  immediate 16-bit value for example for loading to register. Those instructions take 2 machine words.
* Has 16 registers 16-bit each, named: r0, r1, ..., r14, r15
* Register r0 is hard-wired to 0 and writing to it has no effect
* Register r15 is program counter, writing or modifying it causes jump. This register is
  incremented before executing each instruction so for example you can skip next instruction (1 word)
  like this `ADS r15,r15,#1` - this will add 1 to program counter so that it will point to instruction
  after next operation. Keep this in mind when doing relative jumps.
* Registers r1, r2, ..., r13, r14 are for general purpose operations.
* There is no status register, all comparison operations specify output register
  which is written to 1 or 0 depending on success or failure of comparison.
* There is no branch/jump instruction, only conditional `MOV` and conditional
  load which can write value to register depending on lowest bit of condition
  register. It's supposed to be used when writing to r15 a.k.a. program counter.
  Clever tricks with `ADD` are possible that allow to conditionally skip one
  instruction (1 machine word).


## Instruction list
All operations take 1 machine word and 1 execution cycle
(except specified that takes 2).
Instruction and register names are case-insensitive.

Some specific instructions require register **byte addressing** (`MOVZ`,`MOVS` and `MOVB`)
To choose higher or lower byte in register append H or L (upper or lower case) to register
name e.g: `r1H` is higher byte of register 1 and `r2L` is lower byte of register 2.

* `NOP` - no operation, alias to `ADD r0,r0,r0`
* `DW #value` - define 16-bit value at this place in program, you can also place `@label` here to put resolved address of label at this place
* `ADD dst,arg1,arg2` - add two registers and store value in register
* `SUB dst,arg1,arg2` - subtract two registers and store value in register
* `ADS dst,arg1,#arg2` - add register to small signed immediate value and store
  value in register
* `AND dst,arg1,arg2` - perform logical AND of two registers and store value in register
* `OR dst,arg1,arg2` - perform logical OR of two registers and store value in register
* `XOR dst,arg1,arg2` - perform exclusive OR of two registers and store value in register
* `EQ dst,arg1,arg2` - check if two registers are equal,
  if yes then store 1 in register else store 0.
* `NE dst,arg1,arg2` - check if two registers are not equal,
  if yes then store 1 in register else store 0.
* `LT dst,arg1,arg2` - check if register 1 is less than register 2,
  if yes then store 1 in register else store 0.
* `GT dst,arg1,arg2` - check if register 1 is greater than register 2,
  if yes then store 1 in register else store 0, alias to `LT dst,arg2,arg1`
* `LE dst,arg1,arg2` - check if register 1 is lower or equal to register 2,
  if yes then store 1 in register else store 0, alias to `GE dst,arg2,arg1`
* `GE dst,arg1,arg2` - check if register 1 is greater or equal to register 2,
  if yes then store 1 in register else store 0.
* `LTS dst,arg1,arg2` - signed check if register 1 is less than register 2,
  if yes then store 1 in register else store 0.
* `GTS dst,arg1,arg2` - signed check if register 1 is greater than register 2,
  if yes then store 1 in register else store 0, alias to `LTS dst,arg2,arg1`
* `LES dst,arg1,arg2` - signed check if register 1 is lower or equal to register 2,
  if yes then store 1 in register else store 0, alias to `GES dst,arg2,arg1`
* `GES dst,arg1,arg2` - signed check if register 1 is greater or equal to register 2,
  if yes then store 1 in register else store 0.
* `MOVW dst,addr,val` - read/write to ram if dst is register 0 then write register
  val to address at register addr, else read value from ram at address in register
  addr and store it to dst (this operation is 1 machine word but takes 2 cycles to complete)
* `CMOV dst,src,cond` - if lowest bit in register cond is 0 then write src register to
  dst register, else do nothing.
* `CMOVB dst,src,cond` - if lowest bit in register cond is 0 then write lower byte from
  source register to lower byte in destination register, keeping higher byte unchanged, else
  write lower byte from source register to higher byte in destination register, keeping lower byte unchanged.
* `CLDI dst,#value,cond` - if lowest bit in register cond is 0 then write immediate value to destination
  register, else do nothing (takes 2 machine words and 2 cycles to execute).
* `LDI dst,#value` - write immediate value to destination register (takes 2 machine words and 2 cycles to execute), alias to `CLDI dst,#value,r0`
* `SHR dst,src` - shift source register by 1 bit right and write to destination register.
* `ASHR dst,src` - arithmetic shift source register by 1 bit right and write to destination register.
* `SHL dst,src` - shift source register by 1 bit left and write to destination register.
* `MOVZ dst,src` - get higher or lower byte (byte addressed) from source register append zero byte to it and write to
  destination register e.g. `MOVZ r1,r2H`,`MOVZ r10,r3L`. If destination register
  is byte addressed then it will copy result only to this byte, leaving other byte unchanged.
  e.g. `MOVZ r1L,r2L`,`MOVZ r1L,r1H`,`MOVZ r1H,r1L`,`MOVZ r1H,r1H`
* `MOVS dst,src` - get higher or lower byte (byte addressed) from source register sign extend it and write to
  destination register e.g. `MOVS r5,r7H`,`MOVS r4,r2L`. If destination register
  is byte addressed then it will copy result only to this byte, leaving other byte unchanged.
  e.g. `MOVS r1L,r2L`,`MOVS r1L,r1H`,`MOVS r1H,r1L`,`MOVS r1H,r1H` but this is pretty useless
  and just acts exactly as `MOVZ`.
* `NOT dst,src` - negate source register and write to destination register
* `CALL dst,addr` - call link, save program counter pointing to instruction after this to destination register,
  and copy value of address register to program counter (jump with saved pc)
* `HALT` - stop cpu and require external signal to restart it.
* `RST` - reset all registers and cpu state to initial values.
* `KILL` - kill cpu, this works as combined `HALT` and `RST`.


Common aliases:
* `MOV dst,src` - copy value from source to destination register, alias to `OR dst,src,r0`
* `MOV dst,[src]` - copy value from ram at address in source register (ignores bit 0 of that register) to destination register, alias to `MOVW dst,src,r0`.
  Due to `MOVW` behavior, when dst register is `r0` then instruction is replaced by `NOP`
* `MOV [dst],src` - copy value from source register to ram at address in destination register (ignores bit 0 of that register), alias to `MOVW r0,dst,src`
* `MOV dst,#value` - load immediate value to destination register, this is adaptive alias.
When value is in range -8:7 inclusive, then it aliases `ADS dst,r0,#value` (1 word) else it aliases `CLDI dst,#value,r0` (2 words)
* `JMP src` - jump to address in source register, alias to `OR r15,src,r0`
* `JMP #src` - jump to address from immediate value or label, alias to `CLDI r15,#src,r0`
* `JMPZ src,cond` - if lowest bit in register cond is 0 then jump to address in source register, alias to `CMOV r15,src,cond`
* `JMPZ #src,cond` - if lowest bit in register cond is 0 then jump to address from immediate value or label, alias to `CLDI r15,#src,cond`



## Examples
Not really optimized program for filling memory with given value:
```
    LDI  r1,#16     ; start address (inclusive)
    LDI  r2,#2048   ; length
    LDI  r3,#69     ; value to fill
Loop:
    MOV  [r1],r3    ; write to memory at address
    EQ   r4,r0,r2   ; check if length is 0
    ADS  r2,r2,#-1  ; decrement length
    ADS  r1,r1,#2   ; increase address by 2
    JMPZ @Loop,r4     ; jump if condition was false
    HALT            ; done
```