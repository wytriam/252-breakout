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
curLine	.DW home		;creates a variable to store the current line that is 2 bytes large
line00	.DW home
line01 	.DW home+40	;Addresses to reduce math
line02	.DW home+80
line03	.DW home+120
line04	.DW home+160
line05	.DW home+200
line06	.DW home+240
line07	.DW home+280
line08	.DW home+320
line09	.DW home+360
line10	.DW home+400
line11	.DW home+440
line12	.DW home+480
line13	.DW home+520
line14	.DW home+560
line15	.DW home+600
line16	.DW home+640
line17	.DW home+680
line18	.DW home+720
line19	.DW home+760
line20	.DW home+800
line21	.DW home+840
line22	.DW home+880
line23	.DW home+920
line24	.DW home+960
	.BS	$0300-*	;Skip to the beginning of the program, proper.

	
start	cld		;Set binary mode. (clear decimal mode)
	jsr init	;initialize the game
	brk		;Stop the program

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
	;get address of row
	lda home-40	; this is 40 before curLine, so that curLine can handle variables on the first row. 
	sta curLine	;zero out curLine
	lda home-39
	sta curLine+1
.getRow	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by linLen)
	adc #linLen
	sta curLine
	lda curLine+1	;deal with overflow
	adc #00
	sta curLine+1
	dex
	cpx #$00
	bpl .getRow	;if the value of x is > than 0, loop. 
	pla		;grab the char to write. 
	sta (curLine),y
	;load return address
	lda .save+1
	pha
	lda .save
	pha
	rts
.save	.DW 2
	
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
	lda #space	;put space in the a register
.nextLn	ldy #0
.loop	sta (curLine),y	;Print the space
	iny
	cpy linLen	;check if y is at end of line.
	bmi .loop
	clc		;Clear the carry flag
	lda curLine	;update the curLn variable (increment by 40)
	adc #linLen
	sta curLine
	lda curLine+1	;deal with overflow
	adc #00
	sta curLine+1
	lda #space
	inx
	cpx #25
	bmi .nextLn
	rts	