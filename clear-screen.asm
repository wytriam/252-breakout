; Example
; Name: 				Thomas Frick & Bobby Clarahan
; Course: 				COMP 252
; Instructor: 			Dr. Conlon
; Date started: 		February 3, 2015
; Last modification: 	March 31, 2018
; Purpose of program:	Clear Video Screen
	.CR		6502					; Assemble 6502 language.
	.TF		clear-screen.prg,BIN	; Object file and format
	.LI		toff					; Listing on, no timings included.
	
; Define some constants
space 	= $20				; ASCII code for space.
home	= $7000				;Address of upper left (home) on video screen ; .DW means we use a 2-byte value

	.OR		$0000			;Start code at address $0000
	jmp		start			;$0300 - Assembler has 2 passes, so we can use variables before we define them if they get defined somewhere in the program	;Jump to the beginning of the program, proper.
							; jmp is 4C
	
; Define zero-page storage
curLn	.DW		$7000		; creates a variable to store the current line and starts it on home
linLn	.DW		40			; line length (40)
		.BS		$0300-*		;Skip to the beginning of the program, proper.
							; * means the current instruction in the program counter
							; .BS stands for byte string. This sets aside everything between the variable and $0300

;main code							
start	cld					;Set binary mode. (clear decimal mode) clear decimal is D8
		jsr clrsrn			;jsr - jump to subroutine; clrsrn - clear screen
		brk					;And stop.

;sub-routine clear screen		
clrsrn	ldx #0				;Initialize index registers.
		lda #space			
.nxtLn	ldy #0
.loop	sta (curLn),y		;Print the space
		iny
		cpy linLn				;End of line?
		bmi .loop
		; update the curLn variable (increment by 40)
		lda curLn
		adc #40
		sta curLn
		; deal with over flow
		bcc .cont
		lda curLn+1
		adc #01
		sta curLn+1
.cont	lda #space
		;lda #40
		;adc curLn ;make the accumulator equal to curLn + 40
		;sta curLn ;put that value into curLn, meaning curLn = curLn + 40
		;lda #space ; reset the accumulator
		inx
		cpx #25
		bmi .nxtLn
		rts
		
		.EN					;End of program. Might not be required, but doesn't hurt. Any code below this isn't assembled. 		