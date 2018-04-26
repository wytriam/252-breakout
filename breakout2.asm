; Name: 		Thomas Frick & Robert Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	April 9, 2018
; Purpose of program:	Breakout Game

	.CR	6502			; Assemble 6502 language.
	.TF	breakout2.prg,BIN	; Object file and format
; CHANGE THIS TO FOXTROT.PRG FOR SUBMISSION!!!
	.LI	toff			; Listing on, no timings included.
	
; CONSTANTS
space 	= $20		;ASCII code for space.
puck 	= 111		;ASCII code for the puck ('o')
minRow	= $00		;The smallest row value. 00 in decimal
maxRow	= $18		;The largest row value + 1. 24 in decimal
minCol	= $00		;The smallest col value. 00 in decimal
maxCol	= $28		;The largest col value + 1. 40 in decimal
true 	= $01		;A constant to make code more readable
false 	= $00		;A constant to make code more readable
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.

; VARIABLES
curLine	.DW $7000	;creates a variable to store the current line that is 2 bytes large
pkRow	.DB $0B		;the row the puck is on. Ranges from 0-23 ($00-$17 hex)
pkCol	.DB $13		;the column the puck is on. Ranges from 0-39 ($00-$27 hex)
;deltaR	.DB $01		;the change in row - NOT USED (only used if ball moves more than 1 square at a time)
;deltaC	.DB $01		;the change in column - NOT USED (only used if ball moves more than 1 square at a time)
rSign	.DB $00		;the positive/negative sign of deltaR. 01 is positive (downwards), 00 is negative (upwards)
cSign	.DB $01		;the positive/negative sign of deltaC. 01 is positive (right), 00 is negative (left)
;rCnt	.DB $00		;the number of times the row has moved this iteration of movePk
;cCnt	.DB $00		;the number of tiems the col has moved this iteration of movePk
	.BS $0300-*	;Skip to the beginning of the program, proper.

; MAIN LOOP	
start	cld		;Set binary mode. (clear decimal mode)
	jsr init	;initialize the game
.main	jsr waste	;run the clock a few cycles (slow down the ball)
	jsr movePk
	jmp .main
	
	brk		;Stop the program

;
; sub-routine to waste lots of time
; Paramaters: none
; Return: none	
;
waste	stx .xReg	;save the contents of the x-register
	sty .yReg	;save the contents of the y-register
	ldx #$3F
.loop	cpx #$00
	beq .return
	dex
	jsr wstTm	;waste some time (slow ball down)
	jmp .loop
.return	ldx .xReg	;Restore x register
	ldy .yReg	;Restore y register
	rts
.xReg	.DB 0
.yReg	.DB 0	

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
; sub-routine to initialize the game
; Parameters: none
; Return: none
;	
init	jsr clrScrn	;clear the screen
	jsr crsrOff	;turn the cursor off
	lda #puck	;set the char for the ball ('o')
	pha		;turn that parameter in
	lda pkRow	;store the row parameter
	pha
	lda pkCol	;store the col parameter
	pha
	jsr printC	;print the ball
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
	jsr mvPkCl
	jsr mvPkRw
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
	brk 		;The sign was neither positive or negative. Debug and find out how
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
	brk 		;The sign was neither positive or negative. Debug and find out how
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
	cmp #space	;make sure the space the puck is in is a space
	bne .bnc	;if it isn't, return true (so that it bounces)
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
.cont	lda #$D8	;load 40 back from $7000
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
	beq .getCh
	dex
	jmp .getRow
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
.cont	lda #$D8	;load 40 back from $7000
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
	beq .prChr
	dex
	jmp .getRow
	;print char
.prChr	pla 		;get the char from the stack
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
	cpx #maxRow	;compare row to lower-most row + 1 (bpl is >=)
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