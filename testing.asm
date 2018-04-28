	.CR	6502			; Assemble 6502 language.
	.TF	testing.prg,BIN		; Object file and format
	.LI	toff			; Listing on, no timings included.
	
; CONSTANTS
space 	= $20		;ASCII code for space.
home	= $7000		;Address of upper left (home) on video screen 
linLen	= 40
space 	= $20		;ASCII code for space.
puck 	= 111		;ASCII code for the puck ('o')
paddle 	= 61		;ASCII code for the paddle ('=')
brickL	= 91		;ASCII code for the left side of a brick ('[')
brickC	= 35		;ASCII code for the center of a brick ('#')
brickR	= 93		;ASCII code for the right side of a brick (']')
bndry	= 45		;ASCII code for the lower boundary ('-')
padRow	= $16		;The row the paddle occupies
bndryR	= $17		;The row the boundary is on
minRow	= $00		;The smallest row value. 00 in decimal
maxRow	= $18		;The largest row value + 1. 24 in decimal
minCol	= $00		;The smallest col value. 00 in decimal
maxCol	= $28		;The largest col value + 1. 40 in decimal
true 	= $01		;A constant to make code more readable
false 	= $00		;A constant to make code more readable
iobase	= $8800		;ACIA I/O vector
iodata	= iobase	;data register
iostat	= iobase+1	;status register
iocmd	= iobase+2	;command register
ioctrl	= iobase+3	;control register
irv	= $FFFA		;interrupt vector start
mPadLKy	= 44		;ASCII for ',', which we use to move the paddle left
mPadRKy	= 46		;ASCII for '.', which we use to move the paddle right
nOffset	= 48		;The first numerical character in ASCII ('0')
irqAdrU	= $50		;Upper byte of IRQ address. 
irqAdrL	= $00		;Lower byte of IRQ address.
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.
	
;VARIABLES
myFrstV	.DW 40
mySecV	.DW home+10
curLine	.DW $7000	;creates a variable to store the current line that is 2 bytes large
headptr	.DB 0		;Initialize buffer offsets to zero
tailptr	.DB 0
pkRow	.DB $0B		;the row the puck is on. Ranges from 0-23 ($00-$17 hex)
pkCol	.DB $13		;the column the puck is on. Ranges from 0-39 ($00-$27 hex)
rSign	.DB $00		;the positive/negative sign of deltaR. 01 is positive (downwards), 00 is negative (upwards)
cSign	.DB $01		;the positive/negative sign of deltaC. 01 is positive (right), 00 is negative (left)
padColL	.DB 17		;The leftmost column the paddle occupies
padColR	.DB 22		;The rightmost column the paddle occupies
scrOne	.DB 0		;The one's digit of the score
scrTen	.DB 0		;The ten's digit of the score
scrHun	.DB 0		;The hundred's digit of the score
rstFlag	.DB false	;Reset flag
inbuff	= * .BS $20	;32-byte circular input buffer 	THIS VARIABLE MUST BE THE LAST VARIABLE BEFORE MAIN PROGRAM
	.BS $0300-*	;Skip to the beginning of the program, proper.

	
onScrn	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	; save the second byte of the return address
	pla		;get the col parameter
	tay		;store the col parameter in y
	pla		;get the row parameter
	tax		;store the row parameter in x
	clc
	clv
	cpx #minRow	;compare row to upper-most row
	bmi .retFl	;if row<0, return false
	cpx #maxRow+1	;compare row to lower-most row + 1 (bpl is >=)
	bpl .retFl	;if row>=24, return false
	cpy #minCol	;compare col to left-most col
	bmi .retFl	;if col<0, return false
	cpy #maxCol	;compare col to right-most col
	bpl .retFl	;if col>=40, return false
.retTr	lda #true	;put return value on stack
	pha
	jmp .return	;return the function
.retFl	lda #false	;put return value on stack
	pha
.return	lda .save+1	;Restore return address upper byte
	pha
	lda .save	;Restore return address lower byte
	pha
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.save	.DW 0
.xReg	.DB 0
.yReg	.DB 0

setCrLn	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	pla
	tax		;Send the row parameter to x
	lda #$D8	;load 40 back from $7000
	sta curLine
	lda #$6F
	sta curLine+1
	;get address of row
.getRow	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #maxCol
	sta curLine
	lda curLine+1	;deal with over flow
	adc #00
	sta curLine+1
	cpx #$00
	beq .return
	dex
	jmp .getRow
.return	lda .save+1
	pha
	lda .save
	pha
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.save	.DW 0
.xReg	.DB 0
.yReg	.DB 0
	
printC	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	pla		;get column
	tay		;save
	pla		;get row
	tax 
	;make sure parameters are within bounds
	txa
	pha		;store the row parameter
	tya
	pha		;store the col parameter
	jsr onScrn	;check the parameters
	pla		;get the return value
	cmp #false	; if this is not false...
	bne .cont	; carry on
	pla		; take out the last parameter (this line is only called on false)
	jmp .return	; end the function prematurely (this line is only called on false)
.cont	txa
	pha
	jsr setCrLn
	pla 		;get the char from the stack
	sta (curLine),y	;print the char
.return	lda .save+1	;Restore return address upper byte
	pha
	lda .save	;Restore return address lower byte
	pha
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts	
.xReg	.DB 0
.yReg	.DB 0
.save	.DB 0	
	
	
initScr	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldy #$1E	;Set the y to perpare to print
	ldx #$00	;Clear out x
.loop	lda .scrMsg,x	;Get the next char of the score message
	cmp #$00
	beq .return	;End if it's equal
	pha		;Save the char
	lda #$18
	pha		;Push the row
	tya
	pha		;Push the col
	jsr printC	;Print the char
	iny
	inx
	jmp .loop
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.scrMsg	.AZ "Score: 000"
.xReg	.DB 0
.yReg	.DB 0
.save	.DW 0

initCsl	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldy #$1E	;Set the y to perpare to print
	ldx #$00	;Clear out x
.loop	lda .scrMsg,x	;Get the next char of the score message
	cmp #$00
	beq .return	;End if it's equal
	sta iobase
	iny
	inx
	jmp .loop
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.scrMsg	.AZ "Use the < and > keys to move!"
.xReg	.DB 0
.yReg	.DB 0
.save	.DW 0

start	cld		;Set binary mode. (clear decimal mode)
	lda #85
	pha
	lda #10		; Set row to ten
	pha
	lda #03		; Set col. to 3
	pha
	jsr printC
	jsr initCsl
	brk		;Stop the program
	
	
	
	
;init	lda myFrstV
;	cmp 40
;	BNE hitIt	;it hit it!
;	rts
;hitIt	lda space
;	sta home+$1
;	rts