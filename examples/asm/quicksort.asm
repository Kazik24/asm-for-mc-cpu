;*******************************
; Pure quicksort algorithm (heavy recursive, and not super fast on it's own)
;*******************************

	nop


	mov r1,#0x6000 ;start address (inclusive)
	mov r2,#0x603F ;end address (inclusive)
	
	
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



;nop
;nop
;nop
;nop
;DataStart:
;dw #10
;dw #9
;dw #8
;dw #7
;dw #6
;dw #5
;dw #4
;dw #3
;dw #2
;DataEnd:
;dw #1
;nop