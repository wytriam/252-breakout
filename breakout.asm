; Name: 		Thomas Frick & Robert Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	April 9, 2018
; Purpose of program:	Breakout Game

	.CR	6502			; Assemble 6502 language.
	.TF	breakout.prg,BIN	; Object file and format
; CHANGE THIS TO FOXTROT.PRG FOR SUBMISSION!!!
	.LI	toff			; Listing on, no timings included.
	
; CONSTANTS
space 	= $20		;ASCII code for space.
puck 	= 111		;ASCII code for the puck ('o')
maxRow	= $18		;The largest row value + 1. 24 in decimal
maxCol	= $28		;The largest col value + 1. 40 in decimal
iobase	= $8800		;ACIA I/O vector
iodata	= iobase	;data register
iostat	= iobase+1	;status register
iocmd	= iobase+2	;command register
ioctrl	= iobase+3	;control register
irv	= $FFFA		;interrupt vector start
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.

; VARIABLES
curLine	.DW $7000	;creates a variable to store the current line that is 2 bytes large
inbuff	= * .BS $20	;32-byte circular input buffer
headptr	= .DB 0		;Initialize buffer offsets to zero
tailptr	= .DB 0
	.BS $0300-*	;Skip to the beginning of the program, proper.

; MAIN LOOP	
start	cli		;Clear the interupt bit
	cld		;Set binary mode. (clear decimal mode)
	jsr clrScrn	;Clear the screen.
	jsr initIO
	brk		;Stop the program

;
; sub-routine to initialize interupt handler
; Parameters: none
; Return: none
;
irqinit	lda #irq	;Initialize NMI vector.
	sta irv
	lda #irq+1
	sta irv+1
	lda #initIO	;Initialize RESET vector (originally start)
	sta irv+2
	lda #initIO+1	;(originally start)
	sta irv+3
	lda #irq	;Initialize interrupt vector
	sta irv+4
	lda #irq+1
	sta irv+5
	
;
; sub-routine to initialize the I/O vector
; Parameters: none
; Return: none
;
initIO	lda #%00001001	;No parity, no echo, tx IRQ disable, rx IRQ enable, ~DTR low
	sta iocmd	;
	lda #%00011110	;1 stop bit, 8 bit word, 9600 bps
	sta ioctrl
	rts

; sub-routine to clear screen
; Parameters: none
; Return: none		
;	
clrScrn	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldx #$FF	;clear the x register (and set to -1)
.nextLn	lda #space	;put space in the a register
	ldy #0
.loop	sta (curLine),y	;Print the space
	iny
	cpy #maxCol	;check if y is at end of line.
	bmi .loop
	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #maxCol
	sta curLine
	lda curLine+1	;deal with over flow
	adc #00
	sta curLine+1
	inx
	cpx #maxRow	;This should be maxRow
	bmi .nextLn
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0
	
	.EN