; Name: 				Thomas Frick & Bobby Clarahan
; Course: 				COMP 252
; Instructor: 			Dr. Conlon
; Date started: 		February 3, 2015
; Last modification: 	March 31, 2018
; Purpose of program:	Clear Video Screen
	.CR		6502					; Assemble 6502 language.
	.TF		input.prg,BIN			; Object file and format
	.LI		toff					; Listing on, no timings included.
	
iobase	=	$8800
iostat	=	iobase+1	;No spaces in sbasm expressions
iocmd	=	iobase+2
ioctrl	=	iobase+3

	.OR		$0300

start	cli
		lda #$0b
		sta iocmd		; Set command status
		lda #$1a
		sta ioctrl		; 0 stop bits, 8 bit word, 2400 baud
		
	;; Load a character from the keyboard and store it into 
	;; the accumulator
	
getkey	lda	iostat		; Read the ACIA status
		and #$08		; Is the rx register empty?
		beq	getkey		; Yes, wait for it to fill
		lda	iobase		; Otherwise, read into accumulator

;; Write the current char in the accumulator to the console

write	pha				; Save accumulator
write1	lda	iostat		; Read the ACIA status
		and #$10		; Is the tx register empty?
		beq	write1		; No, wait for it to empty
		pla				; Otherwise, load saved accumulator
		sta	iobase		; and write to output.
		
		jmp getkey		; Repeat