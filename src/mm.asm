 !src "macros.asm"
;
;--------------------------------------------------------------------------			
; sys-Zeile fuer den Basicstart 
;--------------------------------------------------------------------------			

*= $0800
!byte $00,$0c,$08,$0a,$00,$9e,$32,$30,$36,$34,$00,$00,$00,$00

;--------------------------------------------------------------------------
; VIC Register
;--------------------------------------------------------------------------
; {{{
_spr0_ptr = $07f8
_spr1_ptr = $07f9
_spr2_ptr = $07fa
_spr3_ptr = $07fb
_spr4_ptr = $07fc
_spr5_ptr = $07fd
_spr6_ptr = $07fe
_spr7_ptr = $07ff
_spr0_x = $d000
_spr0_y = $d001
_spr1_x = $d002
_spr1_y = $d003
_spr2_x = $d004
_spr2_y = $d005
_spr3_x = $d006
_spr3_y = $d007
_spr4_x = $d008
_spr4_y = $d009
_spr5_x = $d00a
_spr5_y = $d00b
_spr6_x = $d00c
_spr6_y = $d00d
_spr7_x = $d00e
_spr7_y = $d00f
_spr_x_hi = $D010
_vic_ctrl_1 = $D011 ; 0..2 Offset oben, 3 25 Zeilen, 4 Bild eingeschaltet, 5 Bitmap modus, 6 Extended Color, 7 bit 9 von $D012
_vic_rast = $D012 ; aktuelle rasterzeile oder rasterzeile für IRQ
_spr_on = $D015 ; H = Spr ein
_vic_ctrl_2 = $d016 ; 0..2 Offset links, 3 40 Spalten, 4 Multicolor
_spr_dbl_x = $d017 ; H = Spr doppelt hoch
_vic_ram = $d018
_vic_irq = $d019 ; 0 durch Raste, 1 durch Spr Bgr, 2 durch Spr Spr, 3 durch LP, 7 durch vic, Hi schreiben: IRQ Flag löschen
_vic_irq_msk = $d01a
_spr_bgr = $d01b ; H = Bgr liegt über Spr
_spr_mcol = $d01c ; H = Spr ist Multicolor
_spr_dbl_y = $d01d ; H = Spr doppelt breit
_vic_border_c = $d020 ; Bordercolor
_vic_bgr_c_0_ext = $d021 ; Hintergrundfarb 0
_vic_bgr_c_1_ext = $d022 ; Hintergrundfarb 1
_vic_bgr_c_2_ext = $d023 ; Hintergrundfarb 2
_spr_c_0 = $d025 ; Gemeinsame Farbe 0 Sprites
_spr_c_1 = $d026 ; Gemeinsame Farbe 1 Sprites
_spr0_c = $d027 ; Farbe Sprite 0
_spr1_c = $d028 ; Farbe Sprite 1
_spr2_c = $d029 ; Farbe Sprite 2
_spr3_c = $d02a ; Farbe Sprite 3
_spr4_c = $d02b ; Farbe Sprite 4
_spr5_c = $d02c ; Farbe Sprite 5
_spr6_c = $d02d ; Farbe Sprite 6
_spr7_c = $d02e ; Farbe Sprite 7
_raster_routine_L = $fffe ; Raster IRQ Vector
_raster_routine_H = $ffff ; Raster IRQ Vector

_sid_rnd = $d41b ; Ch3 Noise Random Werte liegen hier
_sid_3_frq_L = $d40e ; Ch3 Frequenz L
_sid_3_frq_H = $d40f ; Ch3 Frequenz H
_sid_3_ctrl = $d412 ; Ch3 Control Register
; }}}

;--------------------------------------------------------------------------
; Byte breite Konstanten
;--------------------------------------------------------------------------
c_schwarz = $00
c_weiss = 01
c_rot = 02
c_tuerkis = 03
c_violett = 04
c_gruen = 05
c_blau = 06
c_gelb = 07
c_orange = 08
c_braun = 09
c_hellrot = 10
c_grau1 = 11
c_grau2 = 12
c_hellgruen = 13
c_hellblau = 14
c_grau3 = 15

c_fallGeschwindigkeit = 02
c_maxSprungkraft = 15
c_sprHoehe = 21
c_charSolid = $c6
c_charTeleportMin = 72
c_charTeleportMax = 89
c_charHazMin = 144
c_charHazMax = 197

;--------------------------------------------------------------------------
; Kernal Routinene
;--------------------------------------------------------------------------
__clrscr = $e544
__readkey = $ffe4

;--------------------------------------------------------------------------
; Mehrzweck Variablen
;--------------------------------------------------------------------------
zi = $22
zj = $23
zk = $24
zl = $25
zm = $26
zn = $27
zal = $14
zah = $15
zbl = $16
zbh = $17
zcl = $18
zch = $19
zdl = $20
zdh = $21

;--------------------------------------------------------------------------
; Spielvariablen
;--------------------------------------------------------------------------
walkCycle = $50
;------------------
; 0 - Stehen rechts
; 1 - Rechts laufen
; 2 - Links laufen
; 3 - Stehen links
; 4 - Springen
; 5 - Springen verboten
; 6 - schießen
; 7
;------------------
mmmode = $51
mmvy = $52
curLevel = $53
;------------------
; 0 - Dirty
; 1 
; 2 
; 3
; 4 
; 5
; 6
; 7
;------------------
bgflags = $54
mmXalt = $55
mmXhialt = $56
mmYalt = $57
mmPower = $58
mmShoots = $59
;------------------
; 0 - Schuss 1
; 1 - Schuss 2
; 2 - Schuss 3
; 3
; 4 - Schuss 1 links
; 5 - Schuss 1 links
; 6 - Schuss 1 links
; 7
;------------------

;--------------------------------------------------------------------------			
	* =$0810	;Startadresse 
;--------------------------------------------------------------------------			
start  	
	sei     
	lda #0
	sta walkCycle
	sta mmvy
	sta _spr_x_hi
	lda #0
	sta curLevel
	lda #24
	sta _spr0_x
	sta _spr1_x
	lda #100
	sta _spr0_y
	sta _spr1_y
	lda #1
	sta bgflags
	lda $ff
	sta mmPower
	jsr MEMINIT
	jsr SCRINIT
	jsr SPRINIT
	jsr SPRLOAD
	jsr CHARINIT
	jsr RNDINIT
	;jsr RASTERINIT
-	jsr MMSAVEPOS
	jsr READINPUT		; MainLoop Eingabe abfragen mmmode setzen
	jsr MOVEDRAWMM		; Zeichne mm
	jsr DRAWBG			; Hintergrund zeichnen
	jsr GRAVITY 		; Immer fallen
	jsr COLLISION 		; zurück, wenn zu hoch, tief, links, rechts, wand
	jsr COLLISIONOBJECT	; Collision mit Teleporter, Bonus, Gegner
	jsr SHOOTS
	jsr CHECKDEATH
	jsr DEBUG
	ldx #1				; Frame abwarten
	jsr FRAMEWAIT      	;
	jmp -
	rts

;--------------------------------------------------------------------------			
;
;--------------------------------------------------------------------------			
MEMINIT
	lda #%00011110
	sta 1
	rts

;--------------------------------------------------------------------------			
; Eingabe auswerten und mmmode befüllen
;--------------------------------------------------------------------------			
READINPUT  	
	lda $dc01       ; CIA 1
	pha
	and #%00001000	; JS R: Rechts laufen 
	bne +
	lda mmmode
	ora #%00000011	; Laufen rechts hinzu
	and #%11110011  ; Laufen links, Stehen weg
	sta mmmode
	jmp ++
+	lda mmmode
	and #%11111101  ; Laufen weg
	sta mmmode
++	pla
	pha
	and #%00000100	; JS L: Links laufen
	bne	+
	lda mmmode
	ora #%00001100	; Laufen Links hinzu
	and #%11111100  ; Laufen rechts, Stehen weg
	sta mmmode
	jmp ++
+	lda mmmode
	and #%11111011  ; Laufen weg
	sta mmmode
++	pla
	pha
	and #%00000001; JS Btn: Schießen
	bne	+
	lda mmmode
	ora #%00010000  ; Springen hinzu
    sta mmmode
    jmp ++
+	lda mmmode
	and #%11101111  ; Springen weg
	ora #%00100000  ; Springen verbieten hinzu
	sta mmmode
++	pla
	pha
	and #%00010000	; JS Btn: Schießen
	bne	++
	lda mmmode
	ora #%01000000  ; Schießen hinzu
    sta mmmode
++	pla    
	cmp #$ff		; Keine Taste? Stehen
	bne +
	lda mmmode		
	and #%10101001	; Laufen und Springen Weg
	sta mmmode
+	rts
		
;--------------------------------------------------------------------------			
; Player sprite bewegen und auswählen
;--------------------------------------------------------------------------			
MOVEDRAWMM	
; Schießen
	lda mmmode
	and #%01000000
	cmp #%01000000
	bne .SchussEnde
	lda mmShoots
	and #%00000001
	bne s1
	; Schuss 1 frei
	ldx #0
	ldy #0
	lda mmShoots
	ora #%00000001
	sta mmShoots
	jmp se
s1	lda mmShoots
	and #%00000010
	bne s2
	; Schuss 2 frei
	ldx #1
	ldy #2
	lda mmShoots
	ora #%00000010
	sta mmShoots
	jmp se
s2	lda mmShoots
	and #%00000100
	bne .SchussEnde
	; Schuss 3 frei
	ldx #2
	ldy #4
	lda mmShoots
	ora #%00000100
	sta mmShoots
	jmp se
se	
	lda _spr0_y
	clc
	adc #10
	sta _spr2_y, y
	lda _spr0_x
	sta _spr2_x, y
	lda _spr_x_hi
	lda #$d4
	sta _spr2_ptr, x
	and #%00000001
	cmp #%00000001
	bne kh
	lda _spr_x_hi
	ora #%00000100
	sta _spr_x_hi
kh	lda #$d4		; erstes Sprite
	sta _spr2_ptr, x
	
	
+	;Links schießen	
	
.SchussEnde   ;Schießen zuende	
	lda mmmode
	and #%00000010
	cmp #%00000010
	beq .goRight	; laufen
	lda mmmode
	and #%00000100
	cmp #%00000100
	beq .goLeft	; laufen
	jmp +++
.goRight
	lda walkCycle	; alle 8 Pixel nächstes Frame
	!for i, 0, 2{
		clc
		ror
	}
	clc
	cmp #4
	bne +
	sec
	sbc #3
	clc
+	adc #$c0		; erstes Sprite
	sta _spr0_ptr
	clc
	adc #4
	sta _spr1_ptr	
	inc walkCycle
	lda walkCycle
	cmp #40
	bne +
	lda #8			; zurück zum ersten lauf Frame
	sta walkCycle
+	lda _spr0_x	
	clc
	adc #2			; 2 Pixel vor
	sta	_spr0_x
	sta	_spr1_x
	bcc .checkJump
	lda _spr_x_hi
	ora #%00000011
	sta _spr_x_hi
	jmp .checkJump
	rts
.goLeft
	lda walkCycle	; alle 8 Pixel nächstes Frame
	!for i, 0, 2{
		clc
		ror
	}
	clc
	cmp #4
	bne +
	sec
	sbc #3
+	adc #$c8		; erstes Sprite
	sta _spr0_ptr
	clc
	adc #4
	sta _spr1_ptr	
	inc walkCycle
	lda walkCycle
	cmp #40
	bne +
	lda #8			; zurück zum ersten lauf Frame
	sta walkCycle
+	lda _spr0_x	
	sec
	sbc #2			; 2 Pixel vor
	sta	_spr0_x
	sta	_spr1_x
	bcs .checkJump
	lda _spr_x_hi
	and #%11111100
	sta _spr_x_hi
	jmp .checkJump
	rts
.checkJump
	lda mmmode		
	and #%00010000
	cmp #%00010000
	beq .isJumpAllowed
	lda #0
	sta mmvy
	rts
+++ lda mmmode	
	and #%00000001
	cmp #%00000001
	beq .standRight		; stehen
	lda mmmode	
	and #%00001000
	cmp #%00001000
	beq .standLeft		; stehen
	rts				
.standRight
	lda #7			; Nächstes Frame Bein vor
	sta walkCycle			
	lda #$c0		; Kopf0		
	sta _spr0_ptr
	clc
	adc #4			; Bein0
	sta _spr1_ptr
	lda _spr0_y
	clc
	adc #c_sprHoehe	; Beine unter Kopf
	sta	_spr1_y
	jmp .checkJump
	rts 	
.standLeft
	lda #7			; Nächstes Frame Bein vor
	sta walkCycle			
	lda #$c8		; Kopf1
	sta _spr0_ptr
	clc
	adc #4			; Bein1
	sta _spr1_ptr
	lda _spr0_y
	clc
	adc #c_sprHoehe	; Beine unter Kopf
	sta	_spr1_y
	jmp .checkJump
	rts 	
.isJumpAllowed
	lda mmmode
	and #%00100000
	beq .jump
	rts
.jump	
	lda mmmode
	and #%00001000
	beq +
	lda #$d2	; Sprung Sprite
	sta _spr0_ptr
	lda #$d3
	sta _spr1_ptr	
	jmp ++
+	lda #$d0	; Sprung Sprite
	sta _spr0_ptr
	lda #$d1
	sta _spr1_ptr
++	inc mmvy 	; Sprungkraft erhöhen
	lda mmvy
	cmp #c_maxSprungkraft
	bne +		; Maximale Sprungkraft erreicht?
	dec mmvy
	rts
+	lda	_spr0_y
	sec
	sbc #8
	sta	_spr0_y
	clc
	adc #c_sprHoehe
	sta	_spr1_y
	rts

;--------------------------------------------------------------------------			
; Spriteposition auf Background abbilden. Errechnet Reihe/ Spalte des
; Sprites 0 unten rechts.
; zi <- _spr0_x
; zj <- _spr_x_hi
; zk <- _spr0_y
; x -> Reihe
; y -> Spalte
; za -> Zeiger auf ScreenRam
;--------------------------------------------------------------------------			
MMXY2BGXY
	lda _spr0_x
	sta zi
	lda _spr_x_hi
	sta zj
	lda _spr0_y
	sta zk
MMXY2BGXY2
 	ldx #$FF		; Wir wollen 0 aber die Schleife beginnt mit inx
	ldy #0			; sprColConv index
	; Anpassungen an der Grenze von _spr0_x
	lda zi
	cmp #$ff
	bne ++
	ldx #34
	jmp xx
++
	lda zi
	bne ++
	ldx #32
	jmp xx
++
	; ENDE Anpassungen an der Grenze von _spr0_x

-	inx				; Finde ersten Wert der Tabelle > _spr_x
	lda zi
	cmp sprColConv, x
	bcs -
	lda zj			; Korrektur x > 255 MSB von Spr 0 holen
	and #%00000001  ; und maskieren
	cmp #%00000001  ; XPos > 255?
	bne +			
	txa	
	clc		
	adc #32		; Dann 33 hinzu
	tax
+	dex				;
xx	; Grenze
	
	ldy #0			; Finde ersten Wert der Tabelle > _spr_Y
-	iny
	lda zk
	cmp sprColConv, y
	bcs -
	dey				; Korrektur Obere Kannte Spr 1
	dey
	
	; za soll auf Speicherstelle des Sprites unten rechts zeigen
	lda #$00
	sta zal
	lda #$04
	sta zah
	+adr16_plus_x_times_c8 zal, 1
	+adr16_plus_y_times_c8 zal, 40
	rts
	
sprColConv
	!byte  2, 10, 18, 26, 34, 42, 50, 58, 66, 74, 82, 90, 98, 106, 114, 122, 130, 138, 146, 154, 162, 170, 178, 186, 194, 202, 210, 218, 226, 234, 242, 250, 255

;--------------------------------------------------------------------------			
; Schwerkraft, die auf den Spieler wirkt
;--------------------------------------------------------------------------			
GRAVITY	
	lda	_spr0_y
	clc
	adc #c_fallGeschwindigkeit
	sta	_spr0_y
	clc
	adc #c_sprHoehe	; Beine unter Kopf
	sta	_spr1_y	
	rts				

CHECKDEATH
	lda mmPower
	bne +
DEAD	
	lda #c_weiss 		;sprite multicolor 1
	sta _spr_c_0
	lda #c_weiss		;sprite multicolor 2
	sta _spr_c_1
	lda #%11111111 		; alle Sprites Multicolor
	sta	_spr_mcol
	rts
	lda #c_schwarz	
	sta _spr0_c
	lda #c_schwarz
	sta _spr1_c
	
+	rts	

;--------------------------------------------------------------------------			
; speichert die alte pos von MM in mmXalt, mmXhialt und mmYalt
;--------------------------------------------------------------------------			
MMSAVEPOS
	lda _spr0_y
	sta mmYalt
	lda _spr0_x
	sta mmXalt
	lda _spr_x_hi
	sta mmXhialt
	rts

;--------------------------------------------------------------------------			
; holt die alte pos von MM aus mmYalt und setzt spr1
;--------------------------------------------------------------------------			
MMLOADPOSY
	lda mmYalt
	sta _spr0_y
	adc #c_sprHoehe	; Beine unter Kopf
	sta	_spr1_y	
	rts
	
;--------------------------------------------------------------------------			
; holt die alte pos von MM aus mmXalt und setzt spr1
;--------------------------------------------------------------------------			
MMLOADPOSX
	lda mmXalt
	sta _spr0_x
	sta _spr1_x
	lda mmXhialt
	and #%00000011 ; maskiere Spr 0 und 1
	sta _spr_x_hi
	rts

;--------------------------------------------------------------------------			
; Hintergrund malen
;--------------------------------------------------------------------------			
DRAWBG
	lda bgflags	; Wenn nicht dirty sofort wieder raus
	and #%00000001
	cmp #%00000001
	beq +
	rts
+	+setClean	
	
	lda #$00	; Anfang Screen Memory
	sta zal
	lda #$04
	sta zah
	
	lda #2
	sta zi
	ldx curLevel
	+mulX zi
	tax
	lda	levels, x
	sta zbl
	inx
	lda	levels, x
	sta zbh

	ldy #0
---	lda #$1		; 0 Wiederholungen standart
	sta zl
-	lda (zbl), y
	iny
	cmp #$ff	; Ende?
	beq +++		; Springe zu Ende
	cmp #$80	; Reguläres Tile?
	bcs +		; Tile zeichnen
	sta zl		; nächstes zl mal wiederholen Tile wiederholen, wenn $01 auch ok
	cmp	#$01	; Zeilende?
	bne -
	lda #81		; 81 Zeichen weiter 
	+add168 zal
	jmp -

+	sta zk		; Tile speichern
--	lda zk
	and #%01111111	; Maskiere Tile daten
	tax
	; Farbe aussuchen
	lda tileC, x
	sta zj
	
	lda #9		; Alle Tiles 9 chars also erstes char in tile * 9
	sta zi
	+mulX zi
	sta zi		; zi ist jetzt das erste char

	+phr		; Register speichern da von DrawTile genutzt
	jsr DRAWTILE; Malen
	+plr		; Register zurückholen damit Schleifen weiter
	
	lda #73		; 73 Zeichen zurück, damit zal wieder am nächsten Tile liegt
	sta zi
	+sub168 zal, zi

	dec zl		; Tilewiederholung --
	bne	--		; Tile Wiederholen
	jmp ---		; Zurück zum nächsten Wert in Level Tabelle und Wiederholung auf 0
+++	rts

;--------------------------------------------------------------------------			
; zi = char
; zal = pos Oben links
;--------------------------------------------------------------------------			
DRAWTILE
	lda zi
	ldy #0
	sta (zal), y
	+color
	lda zi
	+ina
	iny
	sta (zal), y
	+color
	+ina
	iny
	sta (zal), y
	+color
	!for x, 0, 1{
		pha
		lda #38
		+add168 zal	
		pla	
		+ina
		sta (zal), y
		+color
		+ina
		iny
		sta (zal), y
		+color
		+ina
		iny
		sta (zal), y
		+color
	}
	rts

COLORTILE
	lda #$00
	sta zdl
	lda #$d4
	sta zdh
	+add16 zal, zdl, zcl
	
	lda zj
	ldy #0
	sta (zcl), y
	iny
	sta (zcl), y
	iny
	sta (zcl), y
	!for x, 0, 1{
		pha
		lda #38
		+add168 zal	
		pla	
		sta (zcl), y
		iny
		sta (zcl), y
		iny
		sta (zcl), y
	}
	rts
	
;--------------------------------------------------------------------------			
; Debug: Färbt die chars an den 4 Ecken von Mega man ein 
;--------------------------------------------------------------------------			
DEBUGCOLOR
	lda #$00
	sta zbl
	lda #$d4
	sta zbh
	+add16 zal, zbl, zcl
	ldx #0
	lda zj
	sta (zcl, x)
	rts

;--------------------------------------------------------------------------			
; Kollision prüfen 
;--------------------------------------------------------------------------			
COLLISION
	;jsr MMXY2BGXY
	;jsr DEBUGCOLOR
	;jsr DEBUG

	; Recht und Links werden unterschielich behandelt,
	; Da unterschiedliche Offsets benutzt werden um in etwa
	; Pixelgenau zu sein
	lda mmmode
	and #%00000010
	bne rechts
	lda mmmode
	and #%00000100
	beq +
	jmp links
+	jmp fallen	; Weder Rechts noch links Bewegung also direkt zum fallen

rechts
	; An der Bildschirmkante halten
	ldy #41
	lda _spr_x_hi
	and #%00000011
	beq +
	lda _spr0_x
	cmp #$40
	bcc +
	JSR MMLOADPOSX
	jmp fallen	; Fallen geht auch am Rand noch
+	; Nun die Character basierten Kollisionsabfragen
	jsr MMXY2BGXY		; Ermittel Character unten rechts
	ldx #0
	+sub168c zal, 1		; Zuerst den Fall, das mm an einer Kante
	ldy #40
	lda (zal, x)
	cmp #c_charSolid	; keine Mauer...
	bcs +
	+add168c zal, 1
	+checkXCol			; ...aber über Ihm schon
	+sub168c zal, 1		; korregieren fals Branch genommen wurde
+	+sub168c zal, 39	; Jetzt eine Zeile Höher vor der Fußspitze...
	+checkXCol			; ...Prüfen ob Mauer
	jsr MMXY2BGXY
	ldx #0
	+add168c zal, 39
	lda (zal, x)
	cmp #c_charSolid
	bcs +
	lda (zal, x)
	cmp #c_charSolid
	bcc +
	lda _spr0_y
	sbc #4
	sta _spr0_y
	lda _spr1_y
	sbc #4
	sta _spr1_y
	rts
+	jmp fallenRechts
links
	; Bildschirmkante
	ldy #41
	lda _spr_x_hi
	and #%00000011
	bne +
	lda _spr0_x
	cmp #$19
	bcs +
	JSR MMLOADPOSX
	jmp fallen
+	lda _spr0_x
	sta zi
	inc zi
	inc zi
	lda _spr_x_hi
	sta zj
	lda _spr0_y
	clc
	adc #6
	sta zk
	jsr MMXY2BGXY2
	ldx #0
	+sub168c zal, 2
	lda (zal, x)
	cmp #c_charSolid
	bcs +
	+sub168c zal, 1
	+checkXCol
	+add168c zal, 1
+	+sub168c zal, 41
	+checkXCol
	+add168c zal, 82
	lda (zal, x)
	cmp #c_charSolid
	bcs +
	+sub168c zal, 1
	lda (zal, x)
	cmp #c_charSolid
	bcc +
	lda _spr0_y
	sbc #3
	sta _spr0_y
	lda _spr1_y
	sbc #3
	sta _spr1_y
	rts
+	jmp fallenLinks
fallen
	lda _spr1_y
	cmp #230
	bcc +
	lda #0
	sta mmPower
	rts	
+	lda mmmode
	and #%00000001
	bne fallenRechts
	lda mmmode
	and #%00001000
	bne fallenLinks

fallenRechts
	lda _spr_x_hi
	sta zj
	lda _spr0_x
	clc
	adc #4
	sta zi
	bcc +
	lda zj
	ora #%00000011
	sta zj
+	lda _spr0_y
	sta zk
	jsr MMXY2BGXY2
	+sub168c zal, 1
	ldx #0			
	lda (zal, x)
	cmp #c_charSolid			
	bcs .K
	jmp ++
++  +sub168c zal, 1
	lda (zal, x)
	cmp #c_charSolid			
	bcs .K
	jmp ++
++	+sub168c zal, 1
	lda (zal, x)
	cmp #c_charSolid			
	bcs .K
	jmp ++
.K	jsr MMLOADPOSY
	lda mmmode 		; springen wieder ermöglichen da auf boden
	and #%11011111
	sta mmmode 
	lda #0
	sta mmvy
++	rts

fallenLinks
	lda _spr_x_hi
	sta zj
	lda _spr0_x
	sec
	sbc #6
	sta zi
	bcs +
	lda zj
	ora #%11111100
	sta zj
+	lda _spr0_y
	sta zk
	jsr MMXY2BGXY2
	ldx #0
	lda (zal, x)
	cmp #c_charSolid
	bcs .K2
	jmp ++
++  +sub168c zal, 1
	lda (zal, x)
	cmp #c_charSolid
	bcs .K2
	jmp ++
++	+sub168c zal, 1
	lda (zal, x)
	cmp #c_charSolid
	bcs .K2
	jmp ++
.K2	jsr MMLOADPOSY
	lda mmmode 		; springen wieder ermöglichen da auf boden
	and #%11011111
	sta mmmode 
	lda #0
	sta mmvy
++	rts

COLLISIONOBJECT
	jsr MMXY2BGXY		; Ermittel Character unten rechts
	ldx #0
	+sub168c zal, 41	; Etwa mitte zwischen beinen
	lda (zal, x)		; Char laden
	cmp #c_charTeleportMin
	bcc +				; A < c_charTeleportMin? 
	cmp #c_charTeleportMax
	bcs +				; A >= c_charTeleportMax? 
	inc curLevel		; Teleporter: Nächste Level und neu zeichnen
	+setDirty
+	cmp #c_charHazMin
	bcc +				; A < c_charTeleportMin? 
	cmp #c_charHazMax
	bcs +				; A >= c_charTeleportMax? 
	dec mmPower			; Hazzard	
+	rts

SHOOTS
	inc _spr2_x
	inc _spr3_x
	inc _spr4_x
	rts

;--------------------------------------------------------------------------			
;Sprite Parameter setzen (Frabe, Modus...)
;--------------------------------------------------------------------------			
SPRINIT	
	lda #$00        	
    sta _spr_dbl_x     	; X-Zoom
	sta _spr_dbl_y     	; Y-Zoom   
	lda #$ff          	
	sta _spr_on        	; Sprite 0-7 an
	lda #%11000000 		; Sprite 7 & 8 Xpos > 255
	sta	_spr_x_hi
	lda #c_schwarz 		;sprite multicolor 1
	sta _spr_c_0
	lda #c_blau			;sprite multicolor 2
	sta _spr_c_1
	lda #%11111111 		; alle Sprites Multicolor
	sta	_spr_mcol
	rts
	lda #c_gelb	
	sta _spr0_c
	lda #c_hellblau
	sta _spr1_c
	rts

SCRINIT	
	jmp CLRSCR
    rts
		
;--------------------------------------------------------------------------			
;Sprites in Speicher laden
;--------------------------------------------------------------------------			
SPRLOAD
	!for sp, 0, 20{
		ldx #$3f         	; Spritedaten ....
-		lda (spritespace) + sp * 64,x
       	sta $3000 + sp * 64,x      	; ... einlesen
       	dex		
      	bpl -	
    }
    lda #$c0				;$c0 * $40 = $3000
	!for sp, 0, 9{
		sta _spr0_ptr + sp	
    }	

;--------------------------------------------------------------------------			
;CharacterSet initialisieren
;--------------------------------------------------------------------------			
CHARINIT
	lda _vic_ram
	ora #$0e
	sta _vic_ram
	lda _vic_ctrl_2
	ora #%00010000
	sta _vic_ctrl_2
	lda #c_schwarz
    sta _vic_border_c
    lda #c_grau2
    sta _vic_bgr_c_0_ext	;MC1
    lda #c_schwarz
    sta _vic_bgr_c_1_ext	;MC2
	lda #c_weiss
    sta _vic_bgr_c_2_ext	;MC3
	sta $0286
	
	
	rts
	
;--------------------------------------------------------------------------			
;Raster interupt initialisieren
;--------------------------------------------------------------------------			
RASTERINIT
	sei						;IRQ verbieten
	lda #%01111111			;Timer-IRQs abschalten
 	sta $DC0D				;TODO CIA Konstanten einfügen
 	sta $DD0D
 	lda $DC0D				;Alte CIA IRQs negieren
 	lda $DD0D
 	lda #%00000001			;Raster-IRQs vom VIC-II aktivieren
 	sta _vic_irq_msk	
	lda #50					;Hier soll der erste IRQ kommen
	sta _vic_rast                      
	lda _vic_ctrl_1			;Zur Sicherheit höchste BIT
	and #%01111111			;für die Rasterzeile löschen
	sta _vic_ctrl_1
	lda #$35				;Alle Roms...
	sta $01					;...aus
	lda #<rstIRQ			;Adresse unserer IRQ Routine
	sta _raster_routine_L
 	lda #>rstIRQ
 	sta _raster_routine_H
	cli						;IRQ erlauben
	rts
 		
;--------------------------------------------------------------------------			
;Raster interupt Routine
;--------------------------------------------------------------------------			
rstIRQ	       	
	lda #$ff ; IRQ bestätigen
	sta _vic_irq
	rti ; Return from Interrupt

;--------------------------------------------------------------------------			
;Debugausgabe
;--------------------------------------------------------------------------			
DEBUG
	sta zi
	stx zj
	sty zk

	ldy #0
	lda mmPower
    jsr OUT
    
	+iny3
	lda mmmode
	jsr OUT
       	
	+iny3
	lda _spr0_x
	jsr OUT
    
	+iny3
	lda _spr_x_hi
    jsr OUT   
    
	+iny3
	lda _spr0_y
	jsr OUT
    
	+iny3
	lda mmShoots
	jsr OUT
    
    +iny3
	lda zah
    jsr OUT
    iny
	iny
	lda zal
    jsr OUT
    
    lda zi
	ldx zj
	ldy zk
	
	rts   	

DEBUGA
	sta zl
	stx zm
	sty zn

	ldy #40
	jsr OUT
    
    lda zl
	ldx zm
	ldy zn
	
	rts   	

;--------------------------------------------------------------------------			
;Ausgelagerte Routinen einbinden
;--------------------------------------------------------------------------				
!src "util.asm"
!src "charset1.asm"
!src "sprites.asm"

;--------------------------------------------------------------------------			
;Tabellen
;--------------------------------------------------------------------------				
raster	!byte  0, 50, 92, 134, 176, 218, 0   
xpos	!byte  30, 0, 70, 0, 110, 0, 150, 0, 190, 0, 230, 0, 14, 0, 54, 0  	
tileC	!byte  8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15

level   
!byte 12, $83, $84, $01 
!byte 6, $83, $84, 6, $83, $01 
!byte 13, $85, $01
!byte 13, $82, $01
!byte 13, $82, $01
!byte $96, $82, $96, 9, $82, $89, $01
!byte $96, $82, $96, 9, $82, $88, $01
!byte 6, $96, $94, 6, $96, $01
!byte 13, $96, $01, $ff

level2
!byte 13, $83, $01
!byte $83, 11, $83, $84, $01
!byte $89, 3, $83, $84, 8, $83, $01
!byte $88, 8, $83, $84, 3, $83, $01
!byte $98, 12, $83, $01
!byte 5, $83, $98, 3, $83, $98, 3, $83, $01
!byte 3, $83, $98, 6, $83, 2, $84, $83, $01
!byte 12, $91, $98, $01
!byte 13, $9a, $01
!byte $ff

level3
!byte 13, $8a, $01
!byte 13, $8a, $01
!byte 11, $8a, $89, $8a, $01
!byte 11, $8a, $88, $8a, $01
!byte 11, $8a, $9b, $8a, $01
!byte 13, $8a, $01
!byte $9b, 2, $8a, 2, $9b, 3, $8a, 2, $9b, 3, $8a, $01
!byte $9b, 12, $8a, $01
!byte 13, $91, $01
!byte $ff

level4
!byte $ff

levels
!16	level, level2, level3, level4