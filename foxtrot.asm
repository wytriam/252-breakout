; Name: 		Thomas Frick & Robert Clarahan
; Course: 		COMP 252
; Instructor: 		Dr. Conlon
; Date started: 	March 21, 2018
; Last modification: 	April 9, 2018
; Purpose of program:	Breakout Game

	.CR	6502			; Assemble 6502 language.
	.TF	foxtrot.prg,BIN	; Object file and format
	.LI	toff			; Listing on, no timings included.
	
; CONSTANTS
space 	= $20		;ASCII code for space.
home	= $7000		;Address of upper left (home) on video screen
puck 	= 111		;ASCII code for the puck ('o')
linLen	= 40
iobase	= $8800		;ACIA I/O vector
iodata	= iobase	;data register
iostat	= iobase+1	;status register
iocmd	= iobase+2	;command register
ioctrl	= iobase+3	;control register
irv	= $fffa		;interrupt vector start

			;Origin must be set before variables declared. Why?
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.
inbuff	= *
	.BS $20		;32-byte circular input buffer
headptr	.DB 0		;Initialize buffer offsets to zero
tailptr	.DB 0

; VARIABLES
loopcnt	.DW $00
pl	.DW 18
pr	.DW 22
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
	lda #%00001001	;No parity, no echo, tx IRQ disable, rx IRQ
	sta iocmd	;enable, ~DTR low
	lda #%00011110	;1 stop bit, 8 bit word,
	sta ioctrl	;9600 bps
	jsr irqinit	;initialize interrupt and reset vectors	
	jsr init	;initialize the game
main	lda loopcnt
	cmp $FE
	BCS notredy	;if loopcnt is less than $FE, don't do stuff yet
	lda #00
	sta loopcnt	;if loopcnt reached $FF, reset it to 0
getch	lda tailptr
	cmp headptr	;Check pointers.
	beq empty	;If equal, buffer is empty.
	tax
	lda inbuff,X	;Get the character.
	cmp 97		;process the character
	BEQ mpl
	cmp 100
	BEQ mpr
	inc tailptr	;Increment the offset.
	lda tailptr
	and #%00011111	;Clear high 3 bits to make buffer circular.
	sta tailptr
	jmp getch
mpr	jsr movepr
	jmp empty
mpl	jsr movepl
empty	jsr movePk
notredy inc loopcnt
	jmp main
	
	brk		;Stop the program

irq	pha		;Save registers.
	txa
	pha
	tya
	pha
	lda headptr	;Get buffer head pointer.
	tax		;Set index register value.
	sec
	sbc tailptr
	and #$1f	;Make circular
	cmp #$1f	;If headptr - tailptr = 31, buffer is full.
	beq out		;Buffer is full. Can't do anything.
	lda iodata	;Get the character from the keyboard.
	sta inbuff,X	;Store it into the buffer.
	inx		;Next buffer address
	txa
	and #%00011111	;Clear high 3 bits to make buffer circular.
	sta headptr
out	pla		;Restore registers
	tay
	pla
	tax
	pla
	cli		;Clear interrupt mask (un-disable)
	rti		;Return from interrupt handler.
	
;
;sub-routine to initialize interrupt and reset vectors
;
irqinit	lda #irq	;Initialize NMI vector.
	sta irv
	lda #irq+1
	sta irv+1
	lda #start	;Initialize RESET vector.
	sta irv+2
	lda #start+1
	sta irv+3
	lda #irq	;Initialize interrupt vector.
	sta irv+4
	lda #irq+1
	sta irv+5
	rts

;
; sub-routine to initialize the game
;	
init	jsr clrScrn	;clear the screen
	jsr crsrOff	;turn the cursor off
	lda #111	; set the char for the ball
	pha		; turn that parameter in
	lda xPuck	; set the row to 12
	pha
	lda yPuck	; set the col to 20
	pha
	jsr printC	; print the ball
	jsr movePk	; move the ball once

	lda #120
	pha
	lda #23
	pha
	lda pl
	pha
	jsr printC	;print a paddle part - far left
	
	lda #120
	pha
	lda #23
	pha
	lda #19
	pha
	jsr printC	;print a paddle part
	
	lda #120
	pha
	lda #23
	pha
	lda #20
	pha
	jsr printC	;print a paddle part - middle
	
	lda #120
	pha
	lda #23
	pha
	lda #21
	pha
	jsr printC	;print a paddle part
	
	lda #120
	pha
	lda #23
	pha
	lda pr
	pha
	jsr printC	;print a paddle part - far right
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
	lda #111	; set the char for the ball
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

;
;sub-routine to move paddle right
;
movepr	lda pr		; Get paddle right location,
	cmp 40		; compare it to far right border.
	BCS moveitr	; If there's room to move, go to moveitr
	rts
moveitr	inc pr		; Here's how to at pr, draw padchar:
	lda #120	; set the char for the paddle
	pha		; turn that parameter in
	lda #23		; set the row to 23
	pha
	lda pr		; set the col to pr
	pha
	jsr printC	; print the pr

	lda #space	; at pl, draw space
	pha
	lda #23
	pha
	lda pl
	pha
	jsr printC
	inc pl		; move along pl
	rts

;
;sub-routine to move paddle to the left
;
movepl	lda 0		; Get far left border,
	cmp pl		; compare it to paddle left.
	BCS moveitl	; If there's room to move, go to moveitl
	rts
moveitl	dec pl		; Here's how to at pl, draw padchar:
	lda #120	;set the char for the paddle
	pha
	lda #23		; set the row to 23
	pha
	lda pl		; set the col to pl
	pha
	jsr printC	; print the pl
	
	lda #space	; at pr, draw space
	pha
	lda #23
	pha
	lda pr
	pha
	jsr printC
	dec pr
	rts
	
	.EN