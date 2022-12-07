;*******************************
; Bubble sort
;*******************************
	nop

	mov r1,#0x42 ;start address
	mov r2,#0x4A ;end address (exclusive)
	add r9,r1,#2
	
SortLoopOuter:
	mov r3,r1    ; counter
	add r4,r1,#2 ; counter + 1
	mov r8,#0    ; any swap flag
SortLoopInner:
	
	mov r5,[r3]
	mov r6,[r4]
	gt  r7,r5,r6
	or  r8,r7,r8
	jmpz @Next,r7
	mov [r4],r5
	mov [r3],r6
Next:
	add r3,r3,#2 ;next cell
	add r4,r4,#2 ;next cell
	eq  r5,r4,r2
	jmpz @SortLoopInner,r5
	not r8,r8
	add r2,r2,#-2
	eq  r5,r2,r9
	or  r8,r8,r5
	jmpz @SortLoopOuter,r8
	halt
	
nop
nop
nop
nop
dw #4
dw #3
dw #2
dw #1