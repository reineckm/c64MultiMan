
; Generated by CharPad 2. Assemble with 64TASS or similar.


; General constants:-

TRUE = 1
FALSE = 0
COLRMETH_GLOBAL = 0
COLRMETH_PERTILE = 1
COLRMETH_PERCHAR = 2


; Project constants:-

COLOURING_METHOD = COLRMETH_PERTILE
CHAR_MULTICOLOUR_MODE = TRUE
COLR_SCREEN = 9
COLR_CHAR_DEF = 13
COLR_CHAR_MC1 = 0
COLR_CHAR_MC2 = 1
CHAR_COUNT = 252
TILE_COUNT = 28
TILE_WID = 3
TILE_HEI = 3
MAP_WID = 10
MAP_HEI = 5
MAP_WID_CHRS = 30
MAP_HEI_CHRS = 15
MAP_WID_PXLS = 240
MAP_HEI_PXLS = 120


; Data block size constants:-

SZ_CHARSET_DATA        = 2016
SZ_CHARSET_ATTRIB_DATA = 252
SZ_TILESET_DATA        = 252
SZ_TILESET_ATTRIB_DATA = 28
SZ_MAP_DATA            = 50


; Data block address constants (dummy values):-

ADDR_CHARSET_DATA        = $1000   ; nb. label = 'charset_data'        (block size = $7e0).
ADDR_CHARSET_ATTRIB_DATA = $2000   ; nb. label = 'charset_attrib_data' (block size = $fc).
ADDR_TILESET_DATA        = $3000   ; nb. label = 'tileset_data'        (block size = $fc).
ADDR_TILESET_ATTRIB_DATA = $4000   ; nb. label = 'tileset_attrib_data' (block size = $1c).
ADDR_MAP_DATA            = $5000   ; nb. label = 'map_data'            (block size = $32).



; * INSERT EXAMPLE PROGRAM HERE! * (Or just include this file in your project).



; CHAR SET DATA : 252 (8 byte) chars : total size is 2016 ($7e0) bytes.

* = ADDR_CHARSET_DATA
charset_data

.byte $00,$00,$00,$00,$00,$00,$00,$00,$fc,$cc,$cc,$cc,$cc,$cc,$fc,$00
.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$00,$fc,$0c,$0c,$fc,$c0,$c0,$fc,$00
.byte $fc,$0c,$0c,$3c,$0c,$0c,$fc,$00,$cc,$cc,$cc,$fc,$0c,$0c,$0c,$00
.byte $fc,$c0,$c0,$fc,$0c,$0c,$fc,$00,$c0,$c0,$c0,$fc,$cc,$cc,$fc,$c0
.byte $fc,$0c,$0c,$30,$30,$c0,$c0,$00,$fc,$cc,$cc,$fc,$cc,$cc,$fc,$00
.byte $fc,$cc,$cc,$fc,$0c,$0c,$fc,$00,$fc,$cc,$cc,$fc,$cc,$cc,$cc,$00
.byte $f0,$cc,$cc,$f0,$cc,$cc,$f0,$00,$fc,$c0,$c0,$c0,$c0,$c0,$fc,$00
.byte $f0,$cc,$cc,$cc,$cc,$cc,$f0,$00,$fc,$c0,$c0,$f0,$c0,$c0,$fc,$00
.byte $fc,$c0,$c0,$f0,$c0,$c0,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$55,$55,$55,$55,$65,$55,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$59,$55,$55,$55
.byte $55,$55,$55,$55,$75,$55,$55,$55,$55,$55,$55,$55,$5a,$5a,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$56,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$5d,$55,$55,$55
.byte $55,$55,$55,$55,$55,$55,$56,$5a,$55,$59,$55,$55,$55,$55,$55,$95
.byte $55,$55,$55,$55,$55,$55,$55,$55,$56,$55,$55,$5d,$55,$55,$55,$55
.byte $55,$55,$55,$55,$75,$55,$55,$55,$55,$55,$55,$55,$55,$59,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$59,$55,$55,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
.byte $55,$55,$55,$55,$55,$55,$55,$ff,$55,$55,$55,$55,$55,$55,$55,$ff
.byte $55,$55,$55,$55,$55,$55,$55,$ff,$ff,$55,$55,$ff,$ff,$55,$ff,$55
.byte $ff,$55,$55,$ff,$ff,$55,$ff,$55,$ff,$55,$55,$ff,$ff,$55,$ff,$55
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $00,$00,$00,$00,$00,$00,$00,$00,$3c,$ff,$3f,$3f,$3f,$0f,$0f,$3f
.byte $00,$00,$00,$00,$c0,$c0,$c0,$c0,$00,$00,$00,$03,$03,$0f,$0f,$03
.byte $ff,$fc,$fc,$fc,$f0,$f0,$f0,$fc,$00,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$00,$40,$5a,$5a,$5a,$55,$ff,$ff,$0f,$03,$aa,$aa,$aa,$55
.byte $00,$c0,$c0,$01,$a5,$a5,$a5,$55,$55,$5a,$5a,$5a,$40,$00,$00,$00
.byte $55,$aa,$aa,$aa,$fc,$fc,$fc,$ff,$55,$a5,$a5,$a5,$01,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$3f,$3f,$0f,$0f,$0f,$0f,$3f,$ff
.byte $00,$c0,$c0,$c0,$c0,$c0,$00,$00,$00,$03,$03,$03,$03,$03,$00,$00
.byte $fc,$fc,$f0,$f0,$f0,$fc,$ff,$3f,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$08,$2e,$2e,$b7,$b7
.byte $00,$00,$00,$00,$02,$02,$8b,$8b,$00,$00,$80,$80,$e0,$e0,$78,$78
.byte $d5,$d1,$51,$40,$40,$08,$2a,$aa,$ed,$ed,$75,$74,$54,$10,$12,$8a
.byte $5e,$1e,$17,$07,$05,$85,$a1,$a8,$00,$00,$00,$30,$10,$10,$10,$10
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $10,$10,$10,$10,$10,$10,$10,$10,$00,$00,$00,$0c,$04,$04,$04,$04
.byte $0c,$04,$04,$04,$04,$04,$04,$04,$10,$10,$10,$10,$10,$90,$98,$aa
.byte $04,$04,$04,$04,$24,$24,$a6,$a6,$04,$04,$04,$04,$04,$24,$26,$a6
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$31,$14,$10,$10,$10,$10
.byte $00,$c3,$c3,$00,$00,$00,$00,$00,$00,$00,$4c,$14,$04,$04,$04,$04
.byte $34,$04,$04,$04,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$55
.byte $1c,$10,$10,$10,$40,$40,$40,$40,$00,$00,$00,$00,$00,$03,$03,$03
.byte $00,$3c,$3c,$d7,$d7,$00,$00,$18,$00,$00,$00,$00,$00,$c0,$c0,$c0
.byte $0c,$0c,$0c,$00,$00,$00,$00,$00,$24,$18,$24,$18,$24,$18,$24,$18
.byte $30,$30,$30,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $24,$18,$24,$18,$24,$18,$24,$18,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$02,$02,$02,$28,$28,$be,$be,$be,$ff,$ff,$ff
.byte $00,$00,$00,$00,$00,$80,$80,$80,$02,$0b,$0b,$0b,$0b,$2f,$2f,$2f
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$e0,$e0,$e0,$e0,$e0,$e8,$f8,$f8
.byte $2f,$2f,$2f,$bf,$bf,$bf,$bf,$bf,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $f8,$f8,$fa,$fe,$fe,$fe,$fe,$fe,$00,$00,$00,$30,$10,$10,$10,$10
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $10,$10,$10,$10,$10,$10,$10,$10,$00,$00,$00,$0c,$04,$04,$04,$04
.byte $0c,$04,$04,$04,$04,$04,$04,$04,$10,$10,$10,$10,$10,$90,$98,$aa
.byte $04,$04,$04,$04,$24,$24,$a6,$a6,$04,$04,$04,$04,$04,$24,$26,$a6
.byte $55,$ff,$ff,$ff,$ff,$ff,$55,$7f,$55,$d7,$d7,$d7,$d7,$d7,$55,$ff
.byte $55,$ff,$ff,$ff,$ff,$ff,$55,$fd,$7f,$7f,$7f,$7f,$55,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$55,$d7,$d7,$d7,$fd,$fd,$fd,$fd,$55,$ff,$ff,$ff
.byte $ff,$ff,$55,$7f,$7f,$7f,$7f,$7f,$d7,$d7,$55,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$55,$fd,$fd,$fd,$fd,$fd,$55,$55,$5f,$7f,$5f,$55,$5d,$7f
.byte $55,$55,$ff,$ff,$ff,$55,$55,$55,$55,$55,$f5,$fd,$f5,$55,$75,$fd
.byte $7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$55,$69,$69,$69,$69,$69,$69,$55
.byte $fd,$fd,$fd,$fd,$fd,$fd,$fd,$fd,$7f,$5d,$55,$5f,$7f,$5f,$55,$55
.byte $55,$55,$55,$ff,$ff,$ff,$55,$55,$fd,$75,$55,$f5,$fd,$f5,$55,$55
.byte $55,$55,$55,$55,$5f,$5f,$5f,$5f,$55,$55,$55,$55,$ff,$ff,$ff,$ff
.byte $56,$56,$5a,$5a,$fa,$fa,$fa,$fa,$5f,$5f,$5f,$5f,$5f,$5f,$5f,$5f
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
.byte $5f,$5f,$5f,$5f,$5a,$5a,$6a,$6a,$ff,$ff,$ff,$ff,$aa,$aa,$aa,$aa
.byte $fa,$fa,$fa,$fa,$aa,$aa,$aa,$aa,$55,$40,$d0,$74,$1d,$1d,$1d,$1d
.byte $55,$00,$00,$05,$1f,$05,$00,$00,$55,$15,$7f,$d5,$40,$00,$00,$00
.byte $74,$d0,$40,$01,$07,$5d,$f4,$50,$05,$1f,$75,$d0,$40,$00,$00,$55
.byte $55,$ff,$55,$00,$00,$15,$7f,$d5,$01,$57,$fd,$54,$00,$00,$00,$55
.byte $ff,$55,$00,$15,$7f,$15,$00,$55,$40,$01,$17,$7d,$d4,$40,$00,$55
.byte $aa,$95,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$95,$aa
.byte $aa,$aa,$aa,$aa,$aa,$aa,$6a,$aa,$aa,$aa,$aa,$a9,$aa,$aa,$aa,$aa
.byte $aa,$aa,$aa,$56,$aa,$aa,$95,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$6a,$aa
.byte $aa,$aa,$aa,$aa,$95,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$6a,$aa,$aa,$aa
.byte $aa,$aa,$96,$aa,$aa,$aa,$aa,$aa,$aa,$00,$00,$05,$04,$04,$05,$01
.byte $aa,$00,$aa,$96,$aa,$00,$00,$00,$aa,$00,$00,$54,$04,$04,$04,$04
.byte $01,$0a,$59,$0a,$00,$00,$00,$00,$00,$a0,$65,$a0,$40,$40,$40,$42
.byte $04,$05,$45,$40,$40,$40,$40,$a0,$50,$10,$12,$16,$02,$00,$00,$aa
.byte $42,$42,$a0,$60,$a0,$00,$00,$aa,$65,$a0,$00,$00,$00,$00,$00,$aa



; CHAR SET ATTRIBUTE DATA : 252 attributes : total size is 252 ($fc) bytes.
; nb. Upper nybbles = Material, Lower nybbles = Colour.

* = ADDR_CHARSET_ATTRIB_DATA
charset_attrib_data

.byte $f0,$f0,$f0,$10,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$f0,$f0,$f0,$f0,$f0,$f0
.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0



; TILE SET DATA : 28 (3x3) tiles : total size is 252 ($fc) bytes.

* = ADDR_TILESET_DATA
tileset_data

.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
.byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f
.byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
.byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
.byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
.byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f
.byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
.byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
.byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f
.byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
.byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
.byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
.byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
.byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
.byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb



; TILE SET ATTRIBUTE DATA : 28 attributes : total size is 28 ($1c) bytes.
; nb. Upper nybbles = Unused, Lower nybbles = Colour.

* = ADDR_TILESET_ATTRIB_DATA
tileset_attrib_data

.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0b,$0d,$0d,$0d,$0d,$0d,$0b,$0b,$0b,$0b,$0d,$0d



; MAP DATA : 1 (10x5) map : total size is 50 ($32) bytes.

* = ADDR_MAP_DATA
map_data

.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00

