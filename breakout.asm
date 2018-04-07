; Name: 		Thomas Frick & Robert Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	March 26, 2018
; Purpose of program:	Breakout Game

	.CR	6502			; Assemble 6502 language.
	.TF	breakout.prg,BIN	; Object file and format
; CHANGE THIS TO FOXTROT.PRG FOR SUBMISSION!!!
	.LI	toff			; Listing on, no timings included.
	
; Define some constants
space 	= $20		;ASCII code for space.
pukChar	= 230		;ASCII code for a pixel-filling box
home	= $7000		;Address of upper left (home) on video screen ; .DW means we use a 2-byte value
iobase	= $8800
iostat	= iobase+1	;No spaces in sbasm expressions
iocmd	= iobase+2
ioctrl	= iobase+3

	.OR	$0000	;Start code at address $0000
	jmp	start	;$0300 - Assembler has 2 passes, so we can use variables before we define them if they get defined somewhere in the program	;Jump to the beginning of the program, proper.				;jmp is 4C
	
; Define zero-page storage
curLn	.DW	home	;creates a variable to store the current line and starts it on home
linLn	.DW	40	;line length (40)
pukPos 	.DW	$71CC	;the location of the puck
deltaX	.DB	0	; The horizontal change in the puck.
xSign	.DB	0	; The sign of the deltaX. 0 is positive, 1 is negative
deltaY	.DB	0	; The vertical change in the puck.
ySign	.DB	0	; The sign of the deltaX. 0 is positive, 1 is negative

	.BS	$0300-*	;Skip to the beginning of the program, proper.
			;* means the current instruction in the program counter
			;.BS stands for byte string. This sets aside everything between the variable and $0300

;main code							
start	cld		;Set binary mode. (clear decimal mode) clear decimal is D8
	cli		;Clear interrupt disable bit
	jsr clrsrn	;jsr - jump to subroutine; clrsrn - clear screen			
main	jsr drwPuk	;main redraws the puck and checks for player input
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
	jmp main	;Repeat main loop

	brk		;And stop.
;
; sub-routine to clear screen		
;
clrsrn	ldx #0		;Initialize index registers.
	lda #space			
.nxtLn	ldy #0
.loop	sta (curLn),y	;Print the space
	iny
	cpy linLn	;End of line?
	bmi .loop
	clc		;Clear the carry flag
	lda curLn	;update the curLn variable (increment by 40)
	adc #40
	sta curLn
	lda curLn+1	;deal with over flow
	adc #00
	sta curLn+1
	lda #space
	inx
	cpx #25
	bmi .nxtLn
	rts
;
; sub-routine to draw the puck
;
drwPuk	lda #space	; Prepare to clear screen where puck was
	sta (pukPos)	; Clear screen where puck was
;	ldx #0		; set x counter to 0 (initialize for movement)
;	ldy #0		; set y counter to 0 (initialize for movement)
;.loop	cpx (deltaX)	; loop for moving the puck. this makes sure we don't move past deltaX
;	bne .doneX	;.doneX is for when we've processed the X movement this cycle
;	inx		; we're processing this x, so increment it. 
;	lda (xSign)	; check the direction of deltaX
;	cmp #$00	; this checks if xSign is positive
;	beq .xPos
;	clc		;x is negative
;	lda pukPos	;update the pukPos variable (subtract 1)
;	sbc #01		;decrement by 1
;	sta pukPos
;	lda pukPos+1	;deal with over flow
;	sbc #00
;	sta pukPos+1	
;.xPos	clc		;Clear the carry flag
;	lda pukPos	;update the pukPos variable (increment by 1)
;	adc #01		;we can't use inc because we might have overflow
;	sta curLn
;	lda curLn+1	;deal with over flow
;	adc #00
;	sta curLn+1
;	jsr openPos	;check whether the new location is free
;	cmp #$01	;is this space open? 01 is yes
;	beq .doneX	;ends here if a = $01
;	dex		; we don't want this x to count. undo it. 
;	cmp (deltaX)	;a = 00; we need to flip the xSign. First, check what xSign is
;	beq .flipX	
;	lda #$00	;xSign = 01; Set it to 00
;	sta deltaX	;save the new deltaX
;	jmp .doneX	;we're finished with x
;.flipX	lda #$01	;xSign = 00; Set it to 01
;	sta deltaX	;save the new deltaX
;.doneX	cpy (deltaY)
;	bne .doneY
;	iny		; we're processing this y, so increment it. 
;	lda (ySign)	; check the direction of deltaY
;	cmp #$00	; this checks if ySign is positive
;	beq .yPos
;	clc		;y is negative
;	lda pukPos	;update the pukPos variable (subtract 40)
;	sbc #40		;decrement by 40
;	sta pukPos
;	lda pukPos+1	;deal with over flow
;	sbc #00
;	sta pukPos+1	
;.yPos	clc		;Clear the carry flag
;	lda pukPos	;update the pukPos variable (add 40)
;	adc #40		;we can't use inc because we might have overflow
;	sta curLn
;	lda curLn+1	;deal with over flow
;	adc #00
;	sta curLn+1
;	jsr openPos	;check whether the new location is free
;	cmp #$01	;is this space open? 01 is yes
;	beq .doneY	;ends here if a = $01
;	dey		; we don't want this y to count. undo it. 
;	cmp (deltaY)	;a = 00; we need to flip the ySign. First, check what ySign is
;	beq .flipY	
;	lda #$00	;ySign = 01; Set it to 00
;	sta deltaY	;save the new deltaY
;	jmp .doneY	;we're finished with y
;.flipY	lda #$01	;xSign = 00; Set it to 01
;	sta deltaY	;save the new deltaY
;.doneY	cpx (deltaX)	; compare the x-counter to deltaX
;	bmi .loop	; loop back if x needs more iterations
;	cpy (deltaY)	; compare the y-counter to deltaY
;	bmi .loop
	lda #pukChar	; Prepare to re-draw the puck
	sta (pukPos)	; Re-draw the puck;
	rts
	
;
; sub-routine that checks to see if the position of pukPos is open or not. If the position == space, sets a to $01
;
openPos	lda #space	; Prepare to compare whatever is in memory at pukPos to space
	cmp (pukPos)	; compare a to whatever is in memory locaton pukPos
	beq .true	; a = space. branch to return true
	lda #$00	; a != space. return false
	rts
.true	lda #$01	; return true
	rts


	.EN		;End of program. Might not be required, but doesn't hurt. Any code below this isn't assembled. 		