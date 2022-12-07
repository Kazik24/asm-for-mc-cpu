;*******************************
; Bresenham's line drawing algorithm
; (requires gpu with ability to set pixel)
;*******************************

	nop


	mov r1,#10  ; x1
	mov r2,#11  ; y1
	mov r3,#30 ; x2
	mov r4,#30 ; y2
	
	call r14,@DrawLine
	halt
	nop
	kill
	nop
	
DrawLine: ;draw line at r1=x1,r2=y1,r3=x2,r4=y2
	
	sub r5,r3,r1 ;dx(r5) = x2(r3) - x1(r1)
	ges r11,r5,r0
	add r15,r15,r11 ;skip next command if true
	sub r5,r0,r5 ;negate r5, r5 is now abs value
	
	sub r6,r4,r2 ;dy(r6) = y2(r4) - y1(r2)
	les r11,r6,r0
	add r15,r15,r11 ;skip next command if true
	sub r6,r0,r6 ;negate r6, r6 is now -abs value
	
	lts r7,r1,r3  
	add r15,r15,r7 ;skip
	add r7,r7,#-1  ;sx = if x1 < x2 {1} else {-1}
	
	lts r8,r2,r4
	add r15,r15,r8 ;skip
	add r8,r8,#-1  ;sy = if y1 < y2 {1} else {-1}
	
	add r9,r5,r6   ;err = dx + dy;
	
	mov r12,#0xE280 ; r12 = command cell of gpu
	mov r11,#0x9009 ; set pixel with fill color (command code)
	
	mov r3,r3L ; create finish word (lower byte is x2)
	mov r3H,r4L ; higher byte is y2
	
PixelLoop:
	mov r10,r1L ; create coordinates to send to gpu (lower byte is x1)
	mov r10H,r2L ; higher byte is y1 
	add r4,r12,#2 ;r4 = gpu coordinate reg ptr
	mov [r4],r10  ;send coordinate
	mov [r12],r11 ;send draw pixel command
	
	mov r4,#0x0801 ; mov from operation buffer to screen
	mov [r12],r4 ;send command
	
	ne  r10,r10,r3 ; check if stop condition
	jmpz r14,r10 ; conditional return from method
	
	shl r10,r9 ;e2 = 2*err
	
	ges r4,r10,r6 ;e2 < dy
	jmpz @DxCmp,r4
	add r9,r9,r6 ;err += dy;
	add r1,r1,r7 ;x0 += sx
DxCmp:
	les r4,r10,r5 ;e2 > dx
	jmpz @PixelLoop,r4
	add r9,r9,r5 ;err += dx;
	add r2,r2,r8 ;y0 += sy
	jmp @PixelLoop
	
	