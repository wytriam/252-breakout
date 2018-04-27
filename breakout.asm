; Name: 		Thomas Frick & Robert Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	April 9, 2018
; Purpose of program:	Breakout Game

	.CR 6502		; Assemble 6502 language.
	.TF breakout.prg,BIN	; Object file and format
	.LI toff		; Listing on, no timings included.
	
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
headptr	.DB 0		;Initialize buffer offsets to zero
tailptr	.DB 0
	.BS $0300-*	;Skip to the beginning of the program, proper.

start	lda #%00001001	;No parity, no echo, tx IRQ disable, rx IRQ enable, ~DTR low
	sta iocmd	;
	lda #%00011110	;1 stop bit, 8 bit word, 9600 bps
	sta ioctrl
irqinit	lda #irq	;Initialize NMI vector.
	sta irv
	lda #$03 	;irq+1	;Get the next address (upper byte of irq address)
	sta irv+1
	lda #start	;Initialize RESET vector
	sta irv+2
	lda start+1	
	sta irv+3
	lda #irq	;Initialize interrupt vector
	sta irv+4
	lda #$03
	sta irv+5
;Get one character from the buffer, if there's one there.
main	lda tailptr
	cmp headptr	;Check pointers
	beq empty	;If equal, buffer is empty
	tax
	lda inbuff,X	;Get the character.
	pha		;Char becomes a parameter
	jsr dostuff	;process the character.
	inc tailptr	;Increment the offset.
	lda tailptr	
	and #%00011111	;Clear high 3 bits to make buffer circular.
	sta tailptr
empty	jmp main
	nop
irq	pha		;Save registers.
	txa
	pha
	tya
	pha
	lda headptr	;Get buffer head pointer.
	tax		;Set index register value.
	sec
	sbc tailptr
	and #$1F	;Make circular.
	cmp #$1F	;If headptr - tailptr = 31, buffer is full.
	beq out		;Buffer is full. Can't do anything.
	lda iodata	;Get the character from the keyboard.
	sta inbuff,x	;Store it into the buffer.
	inx		;Next buffer address
	txa
	and #%00011111	;Clear high 3 bits to make buffer circular
	sta headptr
out	pla		;Restore registers
	tay
	pla
	tax
	pla
	cli		;Clear interrupt mask (un-disable)
	rti		;Return from interupt handler.
;
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

;
; sub-routine to print a char to the console
; Parameters: Char to print
; Return: none
; 
dostuff	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
.write1	lda iostat	;Read the ACIA status
	and #$10	;Is the tx register empty?
	beq .write1	;No, wait for it to empty
	pla		;Otherwise, load saved accumulator
	sta iobase	;and write to output
	lda .save+1
	pha
	lda .save
	pha
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.save	.DW 0
.xReg	.DB 0
.yReg	.DB 0	
	
	.EN