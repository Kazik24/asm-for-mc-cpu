	nop 

	mov  r8,#0x20 ;start source address
	mov  r9,#0x40 ;end source address (exclusive)
;	mov  r3,#0x60 ; start destination address
;	
;memcpyLoop:
;	mov  r7,[r1]   ; r7 = ram[r1]
;	mov  [r3],r7   ; ram[r3] = r7
;	add  r1,r1,#2  ; next ram cell for ptr r1
;	add  r3,r3,#2  ; next ram cell for ptr r3
;	ge   r7,r1,r2  ; r7 = r1 >= r2 (bool)
;	jmpz @memcpyLoop,r7
;Stop:
;	jmp  @Stop
	
	
	
nop
nop
GameOfLife: ;r1 - line address, r12 - dst address, both +2 after this function
	mov  r3,[r1] ;L2
	add  r1,r1,#-2
	mov  r2,[r1] ;L1
	add  r1,r1,#4
	mov  r4,[r1] ;L3
	; r2,r3,r4 - lines -1,0,1, r1 ptr is now incremented (by 2)
	;half adder
	xor  r5,r2,r4  ;S20 = L1 ^ L3;
	and  r6,r2,r4  ;S21 = L1 & L3;
	;full adder
	or   r8,r2,r4  ;S31 = L1 | L3;
	xor  r7,r5,r3  ;S30 = S20 ^ L2;
	and  r8,r8,r3  ;S31 = S31 & L2;
	or   r8,r8,r4  ;S31 = S31 | S21;
	; r2,r4 can be reused
	shr  r2,r7      ;S30 >> 1
	shl  r7,r7      ;S30 << 1
	xor  r9,r2,r5   ;S0 = (S30>>>1) ^ S20
	xor  r9,r7,r9   ;S0 = (S30<< 1) ^ S0
	;LSB Carry
	or   r4,r7,r5   ;S0C = (S30<< 1) | S20;
    and  r7,r7,r5   ;S20_= (S30<< 1) & S20;
    and  r4,r2,r4   ;S0C = (S30>>>1) & S0C;
    and  r4,r4,r7   ;S0C = S0C | S20_;
	;Sum's middle bit
	shr  r2,r8      ;S31 >> 1
	xor  r7,r2,r4   ;S1 = S31>>>1 ^ S0C;
	shl  r10,r8     ;S31 << 1
    xor  r7,r7,r10  ;S1 = S31<<1  ^ S1;
    xor  r7,r7,r6   ;S1 = S21     ^ S1;
	
	or   r13,r9,r3  ;X2 = S0 | L2;
	and  r13,r13,r7 ;X2 = X2 & S1;
	;inhibit
	and  r11,r10,r6 ;INH = S31<<1 & S21;
	xor  r6,r10,r6  ;S1_ = S31<<1 ^ S21;
	and  r3,r2,r4   ;C2 = S31>>1 & S0C;
    or   r13,r13,r3 ;INH = INH | C2;
    xor  r3,r2,r4   ;S2_ = S31>>1 ^ S0C;
    and  r3,r3,r6   ;S2_ = S2_ & S1_;
    or   r13,r13,r3 ;INH = INH | S2_;
	
	not  r11,r11      ;~INH
	and  r13,r13,r11  ;X2 = X2 & ~INH;
	mov  [r12],r13    ;store
	add  r12,r12,#1   ;increment dst address
	jmp  r14          ;return
	
	nop
	nop
	nop
	
Multiply: ;r8 - a, r9 - b, r10 - result
	mov  r12,#1  ; mask lowest bit
	mov  r10,r0  ; clear result register
Multiply_Loop:
	and  r11,r8,r12   ;mask lowest bit
	add  r15,r15,r11 ;skip next instruction if bit was true
	add  r15,r15,#1 ;jump forward 1 instruction (skips add)
	add  r10,r10,r9   ;add shifted value to result
	
	shr  r8,r8  ;next bit
	shl  r9,r9  ;mul r9 by 2
	eq   r11,r8,r0  ;compare if there are ane bits to multiply
	jmpz @Multiply_Loop,r11 ;jump to loop label