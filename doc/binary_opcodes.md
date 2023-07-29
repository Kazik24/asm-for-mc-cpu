# Binary Representation of Instructions
___
[Assembly Language Specification](assembly.md)

Instructions have 16-bits, each bit is represented by letter or 0/1 literal. Letters describes
values of specific bit fields in instruction.

| `MSB..........LSB` | Opcode description                                                                                   |
|--------------------|------------------------------------------------------------------------------------------------------|
| `bbbbaaaarrrr0000` | Logical OR of two registers, can be used as `mov` e.g. `0000aaaarrrr0100`                            |
| `bbbbaaaarrrr0001` | Logical AND of two registers, has [special functions](#special-functions) when `r` is `0`.           |
| `bbbbaaaarrrr0100` | Logical XOR                                                                                          |
| `vvvvaaaarrrr0010` | ADS - reg[r] = reg[a] + signext(v)  e.g load small signed values = vvvv0000rrrr0010                  |
| `bbbbaaaarrrr0100` | ADD - reg[r] = reg[a] + reg[b] e.g mov = 0000aaaarrrr0000 or aaaa0000rrrr0000 nop = aaaabbbb00000000 | 
| `bbbbaaaarrrr0101` | SUB - reg[r] = reg[a] - reg[b]   combination 1111111100000001 will halt cpu                          |                       
| `bbbbaaaarrrr0110` | CMPLT - unsignd if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0                                  |                               
| `bbbbaaaarrrr0111` | CMPGE - unsignd if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0                                  |                               
| `bbbbaaaarrrr1000` | CMPLTS - signed if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0                                  |                               
| `bbbbaaaarrrr1001` | CMPGES - signed if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0                                  |                               
| `bbbbaaaarrrr1010` | CMPEQ - if reg[a] == reg[b] then reg[r] = 1 else reg[r] = 0                                          |                               
| `bbbbaaaarrrr1011` | CMPNE - if reg[a] != reg[b] then reg[r] = 1 else reg[r] = 0                                          |                               
| `bbbbaaaarrrr1100` | MOVW - if r != 0 then reg[r] = RAM[reg[a][15:1]] else RAM[reg[a][15:1]] = reg[b]                     |                  
| `bbbbaaaarrrr1101` | CMOV - if reg[b] == 0 then reg[r] = reg[a] (conditional move)                                        |                                     
| `bbbbaaaarrrr1110` | RESERVED                                                                                             |                                            
| `ooooaaaarrrr1111` | EX - [Extension opcodes](#extension-opcodes) that require only 2 arguments.                          |                       


### Special functions

Special functions are triggered by `Logical AND` operation when destination register `r`
is set to `0`.

| `MSB..........LSB` | Opcode description                                                                                                                             |
|--------------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| `1111111100000001` | HALT - Stops cpu.                                                                                                                              |
| `1111111000000001` | KILL - Stops cpu and resets all registers and states to default values.                                                                        |
| `1111110100000001` | RST - Resets all registers and states to default values. After this instruction, cpu will star executing program from opcode at cell 1 in RAM. |


### Extension opcodes

Opcodes that require only 2 arguments.

| `MSB..........LSB` | Opcode description                                                                                                            |
|--------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `0000aaaarrrr1111` | SHR - Logical shift right `reg[r] = reg[a] >> 1`                                                                              |
| `0001aaaarrrr1111` | ASHR - Arithmetic shift right `reg[r] = reg[a] >>> 1`                                                                         |
| `0010aaaarrrr1111` | SHL - Logical shift left `reg[r] = reg[a] << 1`                                                                               |
| `0011aaaarrrr1111` | CLDI - Load immediate conditional - if reg[a] == 0 then reg[r] = NEXT OPCODE else SKIP NEXT (LDI)                             |
| `0100aaaarrrr1111` | SETHLZ - Set high to low (zero extend) `reg[r] = zeroext(reg[a][15:8])`                                                       |
| `0101aaaarrrr1111` | SETHLS - Set high to low (sign extend) `reg[r] = signext(reg[a][15:8])`                                                       |
| `0110aaaarrrr1111` | SETLLZ - Set low to low (zero extend) `reg[r] = zeroext(reg[a][7:0])`                                                         |
| `0111aaaarrrr1111` | SETLLS - Set low to low (sign extend) `reg[r] = signext(reg[a][7:0])`                                                         |
| `1000aaaarrrr1111` | MLL - Mov low byte to low `reg[r][7:0] = reg[a][7:0]`                                                                         |
| `1001aaaarrrr1111` | MLH - Mov low byte to high `reg[r][15:8] = reg[a][7:0]`                                                                       |
| `1010aaaarrrr1111` | MHL - Mov high byte to low `reg[r][7:0] = reg[a][15:8]`                                                                       |
| `1011aaaarrrr1111` | MHH - Mov high byte to high `reg[r][15:8] = reg[a][15:8]`                                                                     |
| `1100aaaarrrr1111` | SETLHZ - Logical shift left by 8 bits `reg[r] = reg[a] << 8` (?)                                                              |
| `1101aaaarrrr1111` | SETHHZ - Zero extend lower byte `reg[r] = reg[a] & 0xff00` (?)                                                                |
| `1110aaaarrrr1111` | NOT - Negate `reg[r] = !reg[a]`                                                                                               |
| `1111aaaarrrr1111` | CALL - Conditional call link - if reg[a] == 0 then reg[r] = pc (for next instruction), pc = NEXT OPCODE else SKIP NEXT (Ccll) |

### Interrupt opcodes

| `MSB..........LSB` | Opcode description                                                                                                                                |
|--------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| `001000CWrrrr1111` | W - (0 - read/1 - write) for accessing hidden interrupt reg, C - (0 - keep, 1 - clear) clear interrupt pending flag                               |
| `001001DDrrrr1111` | Read interrupt flags (0 - interrupt pending, 1 - interrupt scheduled, 2 - interrupt disabled) D - (0 - keep, 1 - disable, 2 - enable, 3 - toggle) |
| `0010001000001111` |                                                                                                                                                   |
