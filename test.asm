; Name: 		Thomas Frick & Bobby Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	March 21, 2018
; Purpose of program:	Clear Video Screen
	.CR		6502		; Assemble 6502 language.
	.TF		test.prg,BIN	; Object file and format
	.LI		toff		; Listing on, no timings included.
	
; Define some constants
space 	= $20			;ASCII code for space.
home	= $7000			;Address of upper left (home) on video screen ; .DW means we use a 2-byte value
iobase	= $8800
iostat	= iobase+1		;No spaces in sbasm expressions
iocmd	= iobase+2
ioctrl	= iobase+3

	.OR $0000	;Start code at address $0000
	jmp start	;Jump to the beginning of the program, proper.
	
; Define zero-page storage
curLn	.DW $7000	;creates a variable to store the current line and starts it on home
linLn	.DW 40		;line length (40)
	.BS $0300-*	;Skip to the beginning of the program, proper.

;main code							
start	cld		;Set binary mode. (clear decimal mode) clear decimal is D8
	cli		;Clear interrupt disable bit
	jsr clrsrn	;jsr - jump to subroutine; clrsrn - clear screen
	lda #$0b		
	sta iocmd	;Set command status
	lda #$1a
	sta ioctrl	;0 stop bits, 8 bit word, 2400 baud
getkey	lda iostat	;Read the ACIA status
	and #$08	;Is the rx register empty?
	beq getkey	;Yes, wait for it to fill
	lda iobase	;Otherwise, read into accumulator
	sta $7000
write	pha		;Save accumulator
write1	lda iostat	;Read the ACIA status
	and #$10	;Is the tx register empty?
	beq write1	;No, wait for it to empty
	pla		;Otherwise, load saved accumulator
	sta iobase	;and write to output.
	jmp getkey	;Repeat

	brk		;And stop.

;sub-routine clear screen		
clrsrn	ldx #0				;Initialize index registers.
	lda #space			
.nxtLn	ldy #0
.loop	sta (curLn),y		;Print the space
	iny
	cpy linLn			;End of line?
	bmi .loop
	lda curLn			;update the curLn variable (increment by 40)
	add #40
	sta curLn
	bcc .cont		
	lda curLn+1			;deal with over flow
	add #01
	sta curLn+1
.cont	lda #space
	inx
	cpx #25
	bmi .nxtLn
	rts
	
	.EN					;End of program.