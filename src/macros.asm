; increase 16-bit counters                                                 
!macro dinc .target {
	inc .target
	bne +	; "bne * + 5" would not work in zp
	inc .target + 1
+
}

!macro ina {
	clc
	adc #1
}

!macro dea {
	sec
	sbc #1
}

!macro add168 .target {
	clc
	adc .target
	sta .target
	bcc +
	inc .target + 1
+
}

!macro sub168 .op1, .op2 {
	lda .op1
	sec
	sbc .op2
	sta .op1
	bcs +
	dec .op1 + 1
+
}

!macro sub168c .op1, .op2 {
	lda .op1
	sec
	sbc #.op2
	sta .op1
	bcs +
	dec .op1 + 1
+
}

!macro add168c .op1, .op2 {
	lda .op1
	clc
	adc #.op2
	sta .op1
	bcc +
	dec .op1 + 1
+
}

!macro mulX .target {
	lda #0
	cpx	#0
	beq +
	clc
-	adc .target
	dex 
	bne -
+
}

!macro adr16_plus_x_times_c8 .target, .c {
	txa
	beq ++
-	lda .target
	clc
	adc #.c
	sta .target
	bcc +
	inc .target + 1
+	dex
++	bne -	
}

; adds numbers 1 and 2, writes result to separate location
!macro add16 num1, num2, res {
	clc
	lda num1
	adc num2
	sta res	
	lda num1 + 1
	adc num2 + 1
	sta res + 1
}

; subtracts number 2 from number 1 and writes result out
!macro sub16 num1, num2, res {
	sec
	lda num1
	sbc num2
	sta res
	lda num1 + 1
	sbc num2 + 1
	sta res + 1
}

!macro adr16_plus_y_times_c8 .target, .c {
	tya
	beq ++
-	lda .target
	clc
	adc #.c
	sta .target
	bcc +
	inc .target + 1
+	dey
++	bne -	
}

!macro adr_plus_adr_times_c .target, .mul, .times {
	lda .mul
	pha
	beq ++
-	lda .target
	clc
	adc #.times
	sta .target
	bcc +
	inc .target + 1
+	dec .mul
++	bne -	
	pla
	sta .mul
}

!macro phr {
	pha
	txa	
	pha	
	tya	
	pha	
}

!macro plr {
	pla	
	tay	
	pla	
	tax	
	pla	
}

!macro iny3 {
	iny	
	iny	
	iny	
}

!macro inx3 {
	inx
	inx	
	inx	
}

!macro setDirty {
	lda bgflags
	ora #%00000001
	sta bgflags
}

!macro setClean {
	lda bgflags
	and #%11111110
	sta bgflags
}

!macro incXspr00 {
	inc	_spr0_x
	inc	_spr1_x
	bne +
	lda _spr_x_hi
	ora #%00000011
	sta _spr_x_hi
+
}

!macro checkXCol {
	lda (zal, x)
	cmp #c_charSolid			; erster Char der nicht penetrierbar ist
	bcs +
	jmp ++
+	jsr MMLOADPOSX
++
}

!macro decXspr01 {
	dec	_spr0_x
	dec	_spr1_x
	cmp #$FF
	bne +
	lda _spr_x_hi
	and #%11111100
	sta _spr_x_hi
+
}

!macro color {
	pha
	lda #$00
	sta zdl
	lda #$d4
	sta zdh
	+add16 zal, zdl, zcl
	lda zj
	sta (zcl), y
	pla
}