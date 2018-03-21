; Example
; Name: 				Thomas Frick
; Course: 				COMP 252
; Instructor: 			Dr. Conlon
; Date started: 		February 3, 2015
; Last modification: 	March 31, 2018
; Purpose of program:	Clear Video Screen
	.CR		6502					; Assemble 6502 language.
	.TF		clear-screen.prg,BIN	; Object file and format
	.LI		toff					; Listing on, no timings included.
	
; Define some constants
space 	= $20				; ASCII code for space.
home	= $7000				;Address of upper left (home) on video screen ; .DW means we use a 2-byte value
statln	= home				;24*40+home		;First space on stat line (bottom line) 

	.OR		$0000			;Start code at address $0000
	jmp		start			;$0300 - Assembler has 2 passes, so we can use variables before we define them if they get defined somewhere in the program	;Jump to the beginning of the program, proper.
							; jmp is 4C
	
; Define zero-page storage
msgptr 	.BS 2				; Pointer to string to print. Belongs to prtmsg
msg		.AZ		" "			;
		.BS		$0300-*		;Skip to the beginning of the program, proper.
							; * means the current instruction in the program counter
							; .BS stands for byte string. This sets aside everything between the variable and $0300
		
start	cld					;Set binary mode. (clear decimal mode) clear decimal is D8
		lda #msg			;Get parameter
		pha					;Push it into the stack
		jsr .prtmsg			;jsr - jump to subroutine; prtmsg - go and print a message
		brk					;And stop.
		
.prtmsg	pla					;Get return address
		sta .retadr
		pla
		sta .retadr+1
		pla
		sta .stradr
		lda #0				;Initialize index registers.
		tay
		tax
.loop	lda msg,x			;Get next character to print.
		cmp #0
		beq .out
		sta statln,y
		inx
		iny
		jmp .loop
.out	lda #space			;Fill rest of line with spaces
.loop2	sta statln,y		;Print the space
		iny
		cpy #40				;End of line?
		bmi .loop2
		lda .retadr+1		;Restore return address to stack
		pha
		lda .retadr
		pha
		rts
.retadr .BS 2
.stradr .BS 1
		.EN					;End of program. Might not be required, but doesn't hurt. Any code below this isn't assembled. 
		