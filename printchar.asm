curline .DB 2
line00	.DW $7000
line01 	.DW line00+1
line02	.DW line01+1
line03	.DW line02+1
line04	.DW line03+1
line05	.DW line04+1
line06	.DW line05+1
line07	.DW line06+1
line08	.DW line07+1
line09	.DW line08+1
line10	.DW line09+1
line11	.DW line10+1
line12	.DW line11+1
line13	.DW line12+1
line14	.DW line13+1
line15	.DW line14+1
line16	.DW line15+1
line17	.DW line16+1
line18	.DW line17+1
line19	.DW line18+1
line20	.DW line19+1
line21	.DW line20+1
line22	.DW line21+1
line23	.DW line22+1
line24	.DW line23+1

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
	lda(line00,x)	;get first byte of row address
	sta curline
	inx
	lda(line00,x)	;get 2nd byte of row address
	sta curline+1
	pla		;get char
	sta (curline),y	
	lda .save+1
	pha
	lda .save
	pha
	rts