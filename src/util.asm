dbgcntH  = $30
dbgcntL  = $31
BSOUT = $ffd2

; -------------------------------------------------------------
; Hilfsfunktionen
;--------------------------------------------------------------

SETCOURSOR 	
	clc               ; Carry-Flag = 0 Cursorposition setzen, = 1 Cursorposition lesen
	jsr $fff0         ; Cursor setzen
	rts

CLRSCR
	lda #147          ; Steuerzeichen für Bildschirm löschen
    jsr BSOUT         ; Löschen durchführen
    rts
          	           	
OUT
	sta zi
	+phr
	lda zi
	pha
	lsr
	lsr
	lsr
	lsr
	tax 
	lda convtable,x
	sta $0400,y
	pla
	and #$0f
	tax
	lda convtable,x
	sta $0401,y			
	+plr		
	rts 		

RNDINIT 
	lda #$ff 			; maximum frequency value
	sta _sid_3_frq_L 	; voice 3 frequency low byte
	sta _sid_3_frq_H 	; voice 3 frequency high byte
	lda #$80  			; noise waveform, gate bit off
	sta _sid_3_ctrl 	; voice 3 control register
	rts
      	
FRAMEWAIT  
	ldy #1
.waitvb  
	bit $D011
    bpl .waitvb
.waitvb2 
	bit $D011
    bmi .waitvb2
    dey
   	bne .waitvb
    dex
    bne FRAMEWAIT	
    rts
      	
convtable   !8 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16