# Binary Representation of Instructions
___
[Assembly Language Specification](assembly.md)

Instructions have 16-bits, each bit is represented by letter or 0/1 literal. Letters describes
values of specific bit fields in instruction.

| `MSB..........LSB` | Opcode description    |
| ------------------ | --- |
| `bbbbaaaarrrr0000` | Logical OR of two registers, can be used as `mov` e.g. `0000aaaarrrr0100` |
| `bbbbaaaarrrr0001` | Logical AND of two registers, has [special functions](#special-functions) when `r` is `0`. |
| `bbbbaaaarrrr0100` | Logical XOR |
| `vvvvaaaarrrr0010` | ADS reg[r] = reg[a] + signext(v)  e.g load small signed values = vvvv0000rrrr0010<br>
| `bbbbaaaarrrr0100` | ADD reg[r] = reg[a] + reg[b] e.g mov = 0000aaaarrrr0000 or aaaa0000rrrr0000 nop = aaaabbbb00000000 <br>
| `bbbbaaaarrrr0101` | SUB reg[r] = reg[a] - reg[b]   combination 1111111100000001 will halt cpu<br>
| `bbbbaaaarrrr0110` | CMPLT unsignd if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr0111` | CMPGE unsignd if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr1000` | CMPLTS signed if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr1001` | CMPGES signed if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr1010` | CMPEQ         if reg[a] == reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr1011` | CMPNE         if reg[a] != reg[b] then reg[r] = 1 else reg[r] = 0<br>
| `bbbbaaaarrrr1100` | MOVW if r != 0 then reg[r] = RAM[reg[a][15:1]] else RAM[reg[a][15:1]] = reg[b]<br>
| `bbbbaaaarrrr1101` | CMOV if reg[b] == 0 then reg[r] = reg[a] (conditional move)<br>
| `bbbbaaaarrrr1110` | RESERVED
| `ooooaaaarrrr1111` | EX - [Extension opcodes](#extension-opcodes) that require only 2 arguments.


### Special functions

Special functions are triggered by `Logical AND` operation when destination register `r`
is set to `0`.

| `MSB..........LSB` | Opcode description |
| ------------------ | --- |
| `1111111100000001` | HALT - Stops cpu. |
| `1111111000000001` | KILL - Stops cpu and resets all registers and states to default values. |
| `1111110100000001` | RST - Resets all registers and states to default values. After this instruction, cpu will star executing program from opcode at cell 1 in RAM. |


### Extension opcodes

Opcodes that require only 2 arguments. 

| `MSB..........LSB` | Opcode description    |
| ------------------ | --- |
| `0000aaaarrrr1111` | Logical shift left `reg[r] = reg[a] << 1` |
* o=1 - reg[r] = reg[a] >> 1 (logic shr)<br>
* o=2 - reg[r] = reg[a] >>> 1 (arithmetic shr)<br>
* o=3 - Load immediate conditional - if reg[a] == 0 then reg[r] = NEXT OPCODE else SKIP NEXT (LDI)<br>
  <br>
* o=4 - reg[r] = reg[a][15:8] set high to low zero extend<br>
* o=5 - reg[r] = signext(reg[a][15:8]) set high to low sign extended<br>
* o=6 - reg[r] = reg[a][7:0] (zero extend) set low to low zero extend<br>
* o=7 - reg[r] = signext(reg[a][7:0]) set low to low sign extend<br>
  <br>
* o=8 - reg[r][7:0] = reg[a][7:0]<br>
* o=9 - reg[r][15:8] = reg[a][7:0]<br>
* o=a - reg[r][7:0] = reg[a][15:8]<br>
* o=b - reg[r][15:8] = reg[a][15:8]<br>
  <br>
* o=c - reg[r] = reg[a] << 8 (logic shl) (?)<br>
* o=d - reg[r] = reg[a] & 0xff00 zero extend lower byte (?)<br>
* o=e - negate   reg[r] = !reg[a]<br>
* o=f - conditional call link  - if reg[a] == 0 then reg[r] = pc (for next instruction), pc = NEXT OPCODE else SKIP NEXT (Ccll)