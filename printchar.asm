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

prch	pla
	sta .save	; where does .save and .save+1 actually save? ;save the first byte of the return address
	pla
	sta .save+1	; save the second byte of the return address
	pla		;get column
	tay		;save
	pla		;get row
	tax
			;get address of row
	txa
	asl		;double it
	lda (line00,x)	;get first byte of row address
	sta curline
	inx
	lda (line00,x)	;get 2nd byte of row address
	sta curline+1
	pla		;get char
	sta (curline),y	
	lda .save+1
	pha
	lda .save
	pha
	rts