	.CR	6502			; Assemble 6502 language.
	.TF	testing.prg,BIN		; Object file and format
	.LI	toff			; Listing on, no timings included.
	
; CONSTANTS
space 	= $20		;ASCII code for space.
home	= $7000		;Address of upper left (home) on video screen 
linLen	= 40

			;Origin must be set before variables declared. Why?
	.OR	$0000	;Start code at address $0000
	jmp	start	;Jump to the beginning of the program, proper.
	
;VARIABLES
myFrstV	.DW 0
mySecV	.DW home+10
	.BS	$0300-*	;Skip to the beginning of the program, proper.

start	cld		;Set binary mode. (clear decimal mode)
	jsr init	;initialize the game
	brk		;Stop the program
	
init	lda myFrstV
	cmp 40
	BCS hitIt	;it hit it!
	rts
hitIt	lda space
	sta home+$1
	rts