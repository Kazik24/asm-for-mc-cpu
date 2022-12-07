;*******************************
; Insertion sort algorithm
;*******************************

	nop


	mov r1,#0x6000 ;start address (inclusive)
	mov r2,#0x603F ;end address (exclusive)
	
	call r14,@InsertionSort
	halt
	nop
	kill
	nop
	
	
InsertionSort:
	add  r3,r1,#2  ;i
	mov  r8,@ISInner ;store address for faster jumps
ISOuter:
	lt   r4,r3,r2
	jmpz r14,r4   ;conditional return
	mov  r4,[r3]  ;key
	add  r5,r3,#-2;j
	add  r3,r3,#2
	
ISInner:
	mov  r6,[r5]
	gt   r7,r6,r4 ;main compare operation
	jmpz @ISInnerEnd,r7
	add  r7,r5,#2
	mov  [r7],r6
	add  r5,r5,#-2
	lt   r7,r5,r1
	add  r15,r15,r7 ;skip next command if true
	jmp  r8 ;imp to stored @ISInner
ISInnerEnd:
	add  r5,r5,#2
	mov  [r5],r4
	jmp  @ISOuter



nop
nop
nop
nop
DataStart:
dw #10
dw #9
dw #8
dw #7
dw #6
dw #5
dw #4
dw #3
dw #2
dw #1
DataEnd:
nop