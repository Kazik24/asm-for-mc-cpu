# Binary Representation of Instructions
___
[Assembly Language Specification](assembly.md)

Instructions have 16-bits, each bit is represented by letter or 0/1 literal. Letters describes
values of specific bit fields in instruction.


`MSB..........LSB`<br>
`bbbbaaaarrrr0000` - ADD reg[r] = reg[a] + reg[b] e.g mov = 0000aaaarrrr0000 or aaaa0000rrrr0000 nop = aaaabbbb00000000 <br>
`bbbbaaaarrrr0001` - SUB reg[r] = reg[a] - reg[b]   combination 1111111100000001 will halt cpu<br>
`vvvvaaaarrrr0010` - ADS reg[r] = reg[a] + signext(v)  e.g load small signed values = vvvv0000rrrr0010<br>
`bbbbaaaarrrr0011` - AND		e.g zero register = 0000aaaarrrr0011<br>
`bbbbaaaarrrr0100` - OR		e.g mov = 0000aaaarrrr0100<br>
`bbbbaaaarrrr0101` - XOR<br>
`bbbbaaaarrrr0110` - CMPEQ         if reg[a] == reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr0111` - CMPNE         if reg[a] != reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr1000` - CMPLT unsignd if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr1001` - CMPGE unsignd if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr1010` - CMPLTS signed if reg[a] <  reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr1011` - CMPGES signed if reg[a] >= reg[b] then reg[r] = 1 else reg[r] = 0<br>
`bbbbaaaarrrr1100` - MOVW if r != 0 then reg[r] = RAM[reg[a][15:1]] else RAM[reg[a][15:1]] = reg[b]<br>
`bbbbaaaarrrr1101` - CMOV if reg[b] == 0 then reg[r] = reg[a] (conditional move)<br>
`bbbbaaaarrrr1110` - CMOVB if reg[b] == 0 then reg[r][7:0] = reg[a][7:0] else reg[r][15:8] = reg[a][7:0] (?)<br>
`ooooaaaarrrr1111` - MOVX<br>
* o=0 - Load immediate conditional - if reg[a] == 0 then reg[r] = NEXT OPCODE else SKIP NEXT (LDI)<br>
* o=1 - reg[r] = reg[a] >> 1 (logic shr)<br>
* o=2 - reg[r] = reg[a] >>> 1 (arithmetic shr)<br>
* o=3 - reg[r] = reg[a] << 1 (logic shl)<br>
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
* o=f - call link - reg[r] = pc (for next instruction), pc = reg[a]<br>
