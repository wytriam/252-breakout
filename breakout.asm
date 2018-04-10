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
home	= $7000		;Address of upper left (home) on video screen 
linLen	= 40

			;Origin must be set before variables declared. Why?
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.

; VARIABLES
curLine	.DW home	;creates a variable to store the current line that is 2 bytes large
xPuck	.DB 12		;the x position of the puck
deltaX	.DB $01		;the change in x position
xSign	.DB $01		;the positive/negative sign of deltaX. 01 is positive, 00 is negative
xCnt	.DB 00		;
yPuck	.DB 20		;the y position of the puck
deltaY	.DB $01		;the change in y position
ySign	.DB $01		;the positive/negative sign of deltaY. 01 is positive, 00 is negative
yCnt	.DB 00		;
	.BS $0300-*	;Skip to the beginning of the program, proper.

	
start	cld		;Set binary mode. (clear decimal mode)
	jsr init	;initialize the game
	brk		;Stop the program

;
; sub-routine to initialize the game
;	
init	jsr clrScrn	;clear the screen
	jsr crsrOff	;turn the cursor off
	lda #puck	; set the char for the ball
	pha		; turn that parameter in
	lda #xPuck	; set the row to 12
	pha
	lda #yPuck	; set the col to 20
	pha
	jsr printC	; print the ball
	jsr movePk	; move the ball once
	rts		;return to main

;
;sub-routine to move the puck
;
movePk	lda #space	; clear the current puck Pos
	pha		; turn that parameter in
	lda #xPuck	; set the row to xPuck
	pha
	lda #yPuck	; set the col to yPuck
	pha
	jsr printC	; print a space
	; move the ball appropriately
	ldx deltaX	;
	stx xCnt
	ldy deltaY	;
	sty yCnt
.mvmtLp	ldx xCnt
	cpx #00
	beq .doneX
	dex		; still x movement to process
	lda xSign
	cmp #01
	beq .xPlus
	dec xPuck	; x is negative
	jmp .chkX
.xPlus	inc xPuck
.chkX	stx xCnt
	jsr bounce
	pla
	cmp #01
	beq .togX	; x is positive, set xSign to 00
	lda #01
	sta xSign
	jmp .doneX
.togX	lda #00
	sta xSign
.doneX	ldy yCnt
	cpy #00
	beq .doneY
	dey		; still y movement to process
	lda ySign
	cmp #01
	beq .yPlus
	dec yPuck	; y is negative
	jmp .chkY
.yPlus	inc yPuck
.chkY	sty yCnt
	jsr bounce
	pla
	cmp #01
	beq .togY	; x is positive, set xSign to 00
	lda #01
	sta ySign
	jmp .doneY
.togY	lda #00
	sta ySign
.doneY	ldx xCnt
	cpx #00
	bne .mvmtLp
	ldy yCnt
	cpy #00
	bne .mvmtLp
	lda #puck	; set the char for the ball
	pha		; turn that parameter in
	lda #xPuck	; set the row to xPuck
	pha
	lda #yPuck	; set the col to yPuck
	pha
	jsr printC	; print a space
	rts	

;
;sub-routine to check if the puck should bounce. returns 00 for false, 01 for true
;
bounce	pla
	sta .save	; where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	; save the second byte of the return address
	clc
	lda xPuck	; load the x position of the puck
	cmp #40		; make sure x is not going too far to the right
	bpl .rFalse
	clc
	lda xPuck
	cmp #00		; make sure x is not going too far to the left
	bmi .rFalse
	clc
	lda yPuck	; load the x position of the puck
	cmp #24		; make sure y is not going too far to down
	bpl .rFalse
	clc
	lda yPuck
	cmp #00		; make sure y is not going too far to up
	bmi .rFalse
	lda #xPuck
	pha
	lda #yPuck
	pha
	jsr getC
	pla
	cmp #space	; make sure the space the puck is in is a space
	bne .rFalse	; if it isn't, return false
	jmp .rTrue
.rFalse	lda #00
	pha
	jmp .return
.rTrue	lda #01
	pha
	jmp .return
	; return information
.return	lda .save+1
	pha
	lda .save
	pha
	rts
.save	.DW 1

;	
;sub-routine to get a char. Argument order is row, column	
;	
getC	pla
	sta .save	; where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	; save the second byte of the return address
	pla		;get column
	tay		;save
	pla		;get row
	tax
	lda #$D8	;load 40 back from home
	sta curLine
	lda #$6F
	sta curLine+1
	;get address of row
.getRow	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #linLen
	sta curLine
	lda curLine+1	;deal with over flow
	adc #00
	sta curLine+1
	dex
	cpx #00
	bne .getRow
	;get the char
	lda (curLine),y
	pha		;save the char
	; return information
	lda .save+1
	pha
	lda .save
	pha
	rts
.save	.DW 1


;
;sub-routine to print a char. Argument order is char, row, column. 
;
printC	pla
	sta .save	; where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	; save the second byte of the return address
	pla		;get column
	tay		;save
	pla		;get row
	tax
	lda #$D8	;load 40 back from home
	sta curLine
	lda #$6F
	sta curLine+1
	;get address of row
.getRow	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #linLen
	sta curLine
	lda curLine+1	;deal with over flow
	adc #00
	sta curLine+1
	dex
	cpx #00
	bne .getRow
	;print char
	pla 		;get the char from the stack
	sta (curLine),y	;print the char
	; return information
	lda .save+1
	pha
	lda .save
	pha
	rts
.save	.DW 1
	
;
; sub-routine to turn off cursor
;
crsrOff	lda #10         ; First byte links second byte to a specific crtc control register
        sta $9000
        lda #%00001000  ; Disable cursor with 5th bit 1 and 6th bit 0
        sta $9001
	rts

;
; sub-routine to clear screen		
;	
clrScrn	ldx #0		;clear the x register
.nextLn	lda #space	;put space in the a register
	ldy #0
.loop	sta (curLine),y	;Print the space
	iny
	cpy #linLen	;check if y is at end of line.
	bmi .loop
	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #linLen
	sta curLine
	lda curLine+1	;deal with over flow
	adc #00
	sta curLine+1
	inx
	cpx #25
	bmi .nextLn
	rts	
	
	.EN