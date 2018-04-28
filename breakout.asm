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

; VARIABLES
curLine	.DW $7000	;creates a variable to store the current line that is 2 bytes large
headptr	.DB 0		;Initialize buffer offsets to zero
tailptr	.DB 0
pkRow	.DB $0B		;the row the puck is on. Ranges from 0-23 ($00-$17 hex)
pkCol	.DB $13		;the column the puck is on. Ranges from 0-39 ($00-$27 hex)
rSign	.DB $01		;the positive/negative sign of deltaR. 01 is positive (downwards), 00 is negative (upwards)
cSign	.DB $01		;the positive/negative sign of deltaC. 01 is positive (right), 00 is negative (left)
padColL	.DB 17		;The leftmost column the paddle occupies
padColR	.DB 22		;The rightmost column the paddle occupies
scrOne	.DB 0		;The one's digit of the score
scrTen	.DB 0		;The ten's digit of the score
scrHun	.DB 0		;The hundred's digit of the score
rstFlag	.DB false	;Reset flag
lives 	.DB 3		;The lives player has
brckCtr	.DB 0		;The number of bricks on screen
inbuff	= * .BS $20	;32-byte circular input buffer 	THIS VARIABLE MUST BE THE LAST VARIABLE BEFORE MAIN PROGRAM
	.BS $0300-*	;Skip to the beginning of the program, proper.

;
; GAME START AND MAIN LOOP
;
start	jsr init	;Initialize the game
main	jsr ioMain
	jsr waste	;Waste time and handle input
	jsr movePk
	lda brckCtr	;Check for winning
	cmp #$00	;If the bricks are 0, you win.
	bne .losChk	;
	jmp resetGm	;Reset the game
.losChk	lda rstFlag	;Check for losing
	cmp #false
	beq main
	jmp resetPk
	brk		;End the program

;
; sub-routine to initialize the game
; Parameters: none
; Return: none
;	
init	jsr ioinit	;Initialize the I/O
	jsr irqinit	;Initialize the IRQ
	jsr clrScrn	;clear the screen
	jsr crsrOff	;turn the cursor off
	jsr drwInit	;Draw the initial game
	rts
	
;
; sub-routine to initialize I/O
; Paramters: none
; Return: none
;
ioinit	lda #%00001001	;No parity, no echo, tx IRQ disable, rx IRQ enable, ~DTR low
	sta iocmd	;
	lda #%00010110	;1 stop bit, 8 bit word, 300 bps
	sta ioctrl
	rts

;
; sub-routine to initialize IRQ
; Parameters: none
; Return: none
;	
irqinit	lda #irq	;Initialize NMI vector.
	sta irv
	lda #irqAdrU 	
	sta irv+1
	lda #start	;Initialize RESET vector
	sta irv+2
	lda start+1	
	sta irv+3
	lda #irq	;Initialize interrupt vector
	sta irv+4
	lda #irqAdrU
	sta irv+5
	rts

;
; sub-routine to draw the initial game
; Parameters: none
; Return: none
;
drwInit	lda #puck	;set the char for the ball ('o')
	pha		;turn that parameter in
	lda pkRow	;store the row parameter
	pha
	lda pkCol	;store the col parameter
	pha
	jsr printC	;print the ball
	ldx padColL	;Get the leftmost column of the paddle
.drawPd	lda #paddle	;Draw the paddle
	pha
	lda #padRow	
	pha
	txa		;Get the correct column to print from the x register
	pha
	jsr printC	;Print the paddle
	inx
	cpx padColR	;Compare to the right column
	bmi .drawPd
	dec padColR	;Make sure the paddle isn't too big
	;Draw the lower boundary line
	lda #bndry
	pha
	lda #bndryR
	pha
	jsr drwLine
	;Draw the bricks
	lda #false	;no offset for first row
	pha
	lda #$00	;Draw the first row of bricks on the top row
	pha
	jsr brckLin	;Draw the first row of bricks
	lda #true	;offset for second row
	pha
	lda #$01	;Set the row
	pha
	jsr brckLin	;Draw the second row of bricks
	lda #false	;no offset for third row
	pha
	lda #$02	;Set the row
	pha
	jsr brckLin	;Draw the third row of bricks
	jsr initScr	;Display the initial score
	jsr initLvs
.done	rts
	
;
; sub-routine to handle input in the buffer
; Parameters: none
; Return: none
;
ioMain	lda tailptr	;Get one character from the buffer, if there's one there.
	cmp headptr	;Check pointers
	beq .empty	;If equal, buffer is empty
	tax
	lda inbuff,X	;Get the character.
	pha		;Char becomes a parameter
	jsr movePd	;process the character.
	inc tailptr	;Increment the offset.
	lda tailptr	
	and #%00011111	;Clear high 3 bits to make buffer circular.
	sta tailptr
.empty	rts		;return	

;
; sub-routine to print a char to the console
; Parameters: Char to print
; Return: none
; 
movePd	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
.write1	lda iostat	;Read the ACIA status
	and #$10	;Is the tx register empty?
	beq .write1	;No, wait for it to empty
	pla		;Otherwise, load saved accumulator
	clc
	cmp #mPadLKy
	beq .mvLeft
	clc
	cmp #mPadRKy
	beq .mvRght
	sta iobase	;Print the unknown char
	jmp .return	;Input did not match any key; return
.mvLeft	jsr moveL
	jmp .return
.mvRght	jsr moveR
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

;
; sub-routine to move the paddle left
; Parameters: none
; Return: none
; 
moveL	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	;Check to make sure you don't move offscreen
	lda padColL
	cmp #minCol
	beq .return	;Get out of here if you're in the left most position
	lda #space	;Clear the right paddle
	pha
	lda #padRow	
	pha
	lda padColR	
	pha
	jsr printC
	dec padColR	;Move the right paddle
	dec padColL	;Move the left paddle
	lda #paddle	;Draw the left paddle
	pha
	lda #padRow	
	pha
	lda padColL	
	pha
	jsr printC
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; sub-routine to move the paddle right
; Parameters: none
; Return: none
; 
moveR	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	;Check to make sure you don't move offscreen
	lda padColR
	cmp #maxCol+$FF	;Get the maxCol-1
	beq .return	;Get out of here if you're in the right most position
	lda #space	;Clear the right paddle
	pha
	lda #padRow	
	pha
	lda padColL	
	pha
	jsr printC
	inc padColR	;Move the left paddle
	inc padColL	;Move the right paddle
	lda #paddle	;Draw the left paddle
	pha
	lda #padRow	
	pha
	lda padColR	
	pha
	jsr printC
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; sub-routine to waste lots of time. This also checks on input. 
; Paramaters: none
; Return: none	
;
waste	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldx #$3F
.loop	cpx #$00
	beq .return
	dex
	jsr wstTm	;waste some time (nop FF * FF times)
	jsr ioMain	;Handle any input (gotta keep that snappy)
	jmp .loop
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; sub-routine to waste time (used to slow down the game)
; Parameters: none
; Return: none
;
wstTm	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldx #$FF
	ldy #$FF
.lpOutr	cpy #$00
	beq .return
	dey
.lpInnr	cpx #$00	;time-wasting loop
	beq .lpOutr	;exit to outer loop
	nop		;nop once
	dex		;decrement x
	jmp .lpInnr	;try again
	; return information
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	
	
;	
; sub-routine to get a char. Argument order is row, column
; Parameters: row, column
; Return: 1 byte (character at row,col); FF if char out of bounds	
;	
getC	stx .xReg	;save the contents of the x-register
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
	cmp #false	;if this is not false...
	bne .cont	;carry on
	lda #$FF		;return FF (this line is only called on false)
	pha
	jmp .return	;end the function prematurely (this line is only called on false)
.cont	txa
	pha
	jsr setCrLn
	;get the char
.getCh	lda (curLine),y
	pha		;save the char
	; return information
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

;
; sub-routine to print a char. Argument order is char, row, column. 
; Parameters: char to print, row, column
; Return: none
;
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
.save	.DW 0
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to check if parameters are on screen (0<=row<=24; 0<=col<=40); 
; parameters: row, col
; returns: true (01) for on screen, false (00) for off screen
;
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
	
;
; sub-routine to turn off cursor
; Parameters: none
; Return: none
;
crsrOff	lda #10         ;First byte links second byte to a specific crtc control register
        sta $9000
        lda #%00001000  ;Disable cursor with 5th bit 1 and 6th bit 0
        sta $9001
	rts

;
; sub-routine to move the puck
; Parameters: none
; Return: none
;
movePk	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	lda #space	;clear the current puck Pos
	pha		;turn that parameter in
	lda pkRow	;set the row to pkRow
	pha
	lda pkCol	;set the col to pkCol
	pha
	jsr printC	;print a space
	;Handle Ball Movement
	jsr mvPkRw
	jsr mvPkCl
	lda #111	;set the char for the ball
	pha		;turn that parameter in
	lda pkRow	;set the row to pkRow
	pha
	lda pkCol	;set the col to pkCol
	pha
	jsr printC	;print a space
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; sub-routine to move the column of the puck by 1
; Parameters: none
; Return: none
;
mvPkCl	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	;Check the direction to move
	lda cSign	;load the sign of the column movement
	clc
	cmp #true	;if sign==true, move right. Otherwise, move left
	beq .mvRgt	;move right
	clc
	cmp #false	;Check to see if sign is negative
	beq .mvLft	;if it is, move left
	;brk 		;The sign was neither positive or negative. Debug and find out how
.mvRgt	inc pkCol	;Increment the column position by 1
	jmp .bnChk	;Check to see if we should bounce
.mvLft	dec pkCol
.bnChk	jsr bounce	;Check to see if the ball should bounce in this position
	pla
	clc
	cmp #false	;If the bounce return is true, we should bounce
	beq .return	;If the bounce is false, though, we can jump to return
	lda cSign	;This code handles how we bounce left/right. First, load the sign....	
	clc
	cmp #$00	;Check if the sign is negative
	beq .nToP	;Change negative to positive
	lda #$00	;Load the negative
	sta cSign	;Save the negative in sign
	jmp .crrctn	;correct the ball from moving to an illegal zone
.nToP	lda #$01	;Load the positive
	sta cSign	;store the new sign in column sign variable
.crrctn	jsr mvPkCl	;correct the ball from moving to an illegal zone
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; sub-routine to move the row of the puck by 1
; Parameters: none
; Return: none
;
mvPkRw	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	lda rSign	;load the sign of the column movement
	clc
	cmp #true	;if sign==true, move down. Otherwise, move up
	beq .mvDn	;move down
	clc
	cmp #false	;Check to see if sign is negative
	beq .mvUp	;if it is, move up
	;brk 		;The sign was neither positive or negative. Debug and find out how
.mvDn	inc pkRow	;Increment the row position by 1
	jmp .bnChk	;Check to see if we should bounce
.mvUp	dec pkRow
.bnChk	jsr bounce	;Check to see if the ball should bounce in this position
	pla
	clc
	cmp #false	;If the bounce return is true, we should bounce
	beq .return	;If the bounce is false, though, we can jump to return
	lda rSign	;This code handles how we bounce up/down. First, load the sign....	
	clc
	cmp #$00	;Check if the sign is negative
	beq .nToP	;Change negative to positive
	lda #$00	;Load the negative
	sta rSign	;Save the negative in sign
	jmp .crrctn	;correct the ball from moving to an illegal zone
.nToP	lda #$01	;Load the positive
	sta rSign	;store the new sign in column sign variable
.crrctn	jsr mvPkRw	;correct the ball from moving to an illegal zone
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to check if the puck should bounce. returns 00 for bounce, 01 for don't bounce
; Parameters: none
; Return: true (01) for bounce, false (00) for no bounce
;
bounce	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	; get what's in the location of the puck
	lda pkRow
	pha
	lda pkCol
	pha
	jsr getC
	pla		;this is the puck location char
	cmp #puck	;the ball can't bounce of off itself
	beq .noBnc	;this check might not be necessary after movePk works
	cmp #brickL	;Check to see if this hit a brick left
	beq .brckCl	;Go to brick collision method
	cmp #brickC	;Check to see if this hit a brick center	
	beq .brckCl	;Go to brick collision method
	cmp #brickR	;Check to see if this hit a brick right
	beq .brckCl	;Go to brick collision method
	cmp #bndry	;Check if puck hit the boundary
	beq .reset	;Puck hit the boundary. Reset
	cmp #space	;make sure the space the puck is in is a space
	bne .bnc	;if it isn't, return true (so that it bounces)
	jmp .noBnc	;area is a space, don't bounce
.reset	lda #true
	sta rstFlag	;Set the reset flag to true
	jmp .noBnc	;Leave function (do not bounce puck)
.brckCl	pha		;Pass the collision char as parameter
	jsr colBrck	;Handle a brick collision
	jmp .bnc	;have the ball bounce
.noBnc	lda #false
	pha
	jmp .return
.bnc	lda #true
	pha
	; return information
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

;
; sub-routine to clear screen
; Parameters: none
; Return: none		
;	
clrScrn	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldx #$FF	;clear the x register (and set to -1)
.nextLn	lda #space	;put space in the a register
	pha
	txa
	adc #$01	;The x counter is offset from the current row by 1. Let's fix that
	pha
	jsr drwLine	;Draw a line of spaces on the current row
	inx
	cpx #maxRow	;This should be maxRow
	bmi .nextLn
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to draw a single character whatever the curLine is
; Parameters: char to draw, row to draw on
; Return: none
; 
drwLine	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	jsr setCrLn	;This pulls the second parameter and sets up curLn correctly
	pla		;get the character to draw
	ldy #0		
.loop	sta (curLine),y	;Print the char
	iny
	cpy #maxCol	;Check if y is at the end of line.
	bmi .loop
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

;
; sub-routine to set the curLine variable
; Parameters: row to have curLine be
; Return: none
;
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

;
; sub-routine to draw a row of bricks
; Parameters: offset (bool), row 
; Return: none
; 
brckLin	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	jsr setCrLn	;Use the row parameter to go the appropriate line
	pla		;Get the offset parameter
	tay		;Save that in the y register
	ldx #$00
.brick	tya 		;Get the column for the leftmost part of the brick
	pha
	jsr drwBrck	;Draw a brick there
	iny 		;Update the column counter
	iny
	iny
	cpx #12		;Check to see if we've drawn all our bricks	
	beq .return	;If we have, return	
	inx		;If we haven't, increment the brick counter and draw another
	jmp .brick
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

;
; sub-routine to draw a brick
; Parameters: (row of brick), column of left brick char
; Return: none
;
drwBrck	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	pla		;Get the column parameter
;	jsr setCrLn	;use the row parameter to modify the current line
	lda #brickL	;Load the left part of the brick
	sta (curLine),y	;Draw the left part of the brick
	iny
	lda #brickC	;Load the center part of the brick
	sta (curLine),y	;Draw the center part of the brick
	iny
	lda #brickR	;Load the right part of the brick
	sta (curLine),y	;Draw the right part of the brick
.return	inc brckCtr	;Increment the brick counter
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

;
; sub-routine to handle a brick collision
; Parameters: brickChar
; Return: none
; 
colBrck	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
		pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	pla 		;get the brickChar
	cmp #brickL
	beq .brckL
	cmp #brickC
	beq .brckC
	cmp #brickR
	beq .brckR
	jmp .return	;Do nothing if this function was called with a non-brick collision char
.brckL	lda pkRow
	sbc #$00
	pha
	lda pkCol	;load the column of the left brick
	pha
	jsr ersBrck
	jmp .return
.brckC	lda pkRow
	pha
	lda pkCol	;load the column of the center brick
	sbc #$01	;Subtract 1 to get left brick
	pha
	jsr ersBrck
	jsr updtScr
	jmp .return
.brckR	lda pkRow
	pha
	lda pkCol	;load the column of the right brick
	sub #$02	;Subtract 2 to get left brick
	pha
	jsr ersBrck
.return	jsr updtScr
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

;
; sub-routine to erase a brick
; Parameters: (row of brick), column of left brick char
; Return: none
;
ersBrck	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	pla
	sta .save	;save the first byte of the return address
	pla
	sta .save+1	;save the second byte of the return address
	pla		;Get the column parameter
	tay
	jsr setCrLn	;use the row parameter to modify the current line
	lda #space	;Set the accumulator to print spaces
	sta (curLine),y	;Erase brickL
	iny
	sta (curLine),y ;Erase brickC
	iny
	sta (curLine),y	;Erase brickR
	dec brckCtr	;Decrement the brick counter
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

;
; sub-routine to initially print the score
; Parameters: none
; Return: none
; 
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


;
; sub-routine to increment score by 1 
; Parameters: none
; Return: none
; 
updtScr	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	inc scrOne	;Increment the one's digit of the score
	lda scrOne	;Load the score
	cmp #$0A	;Compare it to 10
	bmi .return
	lda #$00
	sta scrOne	;zero out the one's place
	inc scrTen	;increment the ten's place
	lda scrTen
	cmp #$0A	;Make sure the ten's digit doesn't <10
	bmi .return
	lda #$00	;Zero out the ten's place
	sta scrTen
	inc scrHun	;And increment the hundred's. Scores of 1000+ improbable. 
.return	jsr prtScr
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to print score on screen
; Parameters: none
; Return: none
;
prtScr	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldy #$25	;Get the hundred's column
	ldx #$18
	lda scrHun	;Load the hundred's place
	clc
	adc #nOffset	;Get the ASCII char for this number
	pha
	txa
	pha
	tya
	pha
	jsr printC	;Print the 100's place
	iny
	lda scrTen	;Load the hundred's place
	clc
	adc #nOffset	;Get the ASCII char for this number
	pha
	txa
	pha
	tya
	pha
	jsr printC	;Print the 100's place
	iny
	lda scrOne	;Load the hundred's place
	clc
	adc #nOffset	;Get the ASCII char for this number
	pha
	txa
	pha
	tya
	pha
	jsr printC	;Print the 100's place
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to initially print the lives
; Parameters: none
; Return: none
; 
initLvs	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldy #$00	;Set the y to perpare to print
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
	jsr updtLvs
	rts
.scrMsg	.AZ "Balls:"
.xReg	.DB 0
.yReg	.DB 0

;
; sub-routine to update the lives counter
; Parameters: none
; Return: none
;
updtLvs	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	; Clear out lives
	ldy lives
	clc
	cpy #$01	;Find out if we have 1+ lives
	bpl .ldb1	;If we do, load a ball
	lda #space	;Otherwise, load a space
	jmp .drsp1
.ldb1	lda #puck	
.drsp1	pha		;And punch that appropriate value into the parameters
	lda #$18
	pha
	lda #$07
	pha
	jsr printC	;Draw the correct character
	ldy lives
	clc
	cpy #$02	;Find out if we have 2+ lives
	bpl .ldb2	;If we do, load a ball
	lda #space	;Otherwise, load a space
	jmp .drsp2
.ldb2	lda #puck
.drsp2	pha		;And punch that appropriate value into the parameters
	lda #$18
	pha
	lda #$08
	pha
	jsr printC	;Draw the correct character
	ldy lives
	clc
	cpy #$03	;Find out if we have 3+ lives
	bpl .ldb3	;If we do, load a ball
	lda #space	;Otherwise, load a space
	jmp .drsp3
.ldb3	lda #puck
.drsp3	pha		;And punch that appropriate value into the parameters
	lda #$18
	pha
	lda #$09
	pha
	jsr printC	;Draw the correct character
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

;
; routine to reset the puck after it hits the bottom
; Parameters: none
; Return: none
; 
resetPk	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-registerjsr waste
	jsr waste	;Let the player see the ball is stuck
	jsr waste
	jsr waste
	jsr waste
	dec lives
	lda lives
	cmp #$00
	bpl .drwLvs
	;Player is out of lives. What do?
	jmp resetGm	;Reset the game
	jmp .cont
.drwLvs	jsr updtLvs	;Update the lives
.cont	lda #$0B	;Default puck row
	sta pkRow	;Reset puck row
	lda #$13	;Default puck col
	sta pkCol	;Reset puck col
	lda #puck	;set the char for the ball ('o')
	pha		;turn that parameter in
	lda pkRow	;store the row parameter
	pha
	lda pkCol	;store the col parameter
	pha
	jsr printC	;print the ball
	;Draw the lower boundary line
	lda #bndry
	pha
	lda #bndryR
	pha
	jsr drwLine
	lda #false	;Set the reset flag back
	sta rstFlag
	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	jmp main
.xReg	.DB 0
.yReg	.DB 0

; 
; routine to reset the game
; Parameters: none
; Return: none
; 
resetGm	lda #$0B
	sta pkRow	;Reset pkRow
	lda #$13	
	sta pkCol	;Reset pkCol
	lda #17
	sta padColL	;Reset padCOlL
	lda #22		
	sta padColR	;Reset padColR
	lda #0
	sta scrOne	;Reset one's digit of score
	lda #0	
	sta scrTen	;Reset ten's digit of score
	lda #0
	sta scrHun	;Reset hundred's digit of score
	lda #false
	sta rstFlag	;Reset resetFlag
	lda #3
	sta lives	;Reset lives
	jmp start	;Restart game

	.BS $5000-*	;Skip to the IRQ function
;
; sub-routine to handle interupts
; Parameters: none
; Return: none
;	
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
	beq .out	;Buffer is full. Can't do anything.
	lda iodata	;Get the character from the keyboard.
	sta inbuff,x	;Store it into the buffer.
	inx		;Next buffer address
	txa
	and #%00011111	;Clear high 3 bits to make buffer circular
	sta headptr
.out	pla		;Restore registers
	tay
	pla
	tax
	pla
	cli		;Clear interrupt mask (un-disable)
	rti		;Return from interupt handler.

	.EN