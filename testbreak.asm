; Name: 		Thomas Frick
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
home	= $7000		;Address of upper left (home) on video screen 
linLen	= 40

			;Origin must be set before variables declared. Why?
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.

; VARIABLES
curLine	.DW home	;creates a variable to store the current line that is 2 bytes large
	.BS $0300-*	;Skip to the beginning of the program, proper.

	
start	cld		;Set binary mode. (clear decimal mode)
	jsr init	;initialize the game
	jsr mainlp	; jumps to main game loop
	brk		;Stop the program

;
; sub-routine to basically run the game
;
mainlp	;if move right button is pressed
	;jsr MoveRight
	;if move left button is pressed
	;jsr MoveLeft
	;if countlp < bignumber
	inc countlp
	;if countlp == bignumber
	;jsr MoveBall

;
;sub-routine to move paddle to the right
;
movepr	lda pr		; Get paddle right location,
	cmp 40		; compare it to far right border.
	BCS moveitr	; If there's room to move, go to moveitr
	rts
moveitr	inc pr
	; at pr, draw padchar
	lda #space
	sta pl
	inc pl
	rts

;
;sub-routine to move paddle to the left
;
movepl	lda 0		; Get far left border,
	cmp pl		; compare it to paddle left.
	BCS moveitl	; If there's room to move, go to moveitl
	rts
moveitl	dec pl
	; at pl, draw padchar
	lda #space
	sta pr
	dec pr
	rts
	
;
; sub-routine to initialize the game
;	
init	jsr clrScrn	;clear the screen
	jsr crsrOff	;turn the cursor off
	lda #111	; set the char for the ball
	pha		; turn that parameter in
	lda #12		; set the row to 12
	pha
	lda #20		; set the col to 20
	pha
	jsr printC	; print the ball
	rts		;return to main
	
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