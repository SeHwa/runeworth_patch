START_INIT: equ $8000
START_PERSIST_CODE: equ $E000
HANGUL_FONT_CHO: equ $E800
HANGUL_FONT_JUNG: equ $E800 + (HANGUL_JUNG_DATA - HANGUL_CHO_DATA)
HANGUL_FONT_JONG: equ $E800 + (HANGUL_JONG_DATA - HANGUL_CHO_DATA)
TABLE_CONV_CHO: equ HANGUL_FONT_CHO + (CONV_CHO_DATA - HANGUL_CHO_DATA)
TABLE_CONV_JUNG: equ HANGUL_FONT_CHO + (CONV_JUNG_DATA - HANGUL_CHO_DATA)
VAR_CODE: equ $EE00
VAR_CHO: equ $EE02
VAR_JUNG: equ $EE03
VAR_JONG: equ $EE04
VAR_FONT: equ $EE10
VAR_FONT_4BPP: equ $EE20
VAR_ARG_FONT: equ $EE80
VAR_ARG_ARR: equ $EEA0
VAR_TEMP_ARR: equ $EEB0

RDVDP: equ $06
WRVDP: equ $07
BDOS: equ $F37D
ENASLT: equ $0024
RAMAD2: equ $F343

GET_TEXT_CHAR: equ $B1F1
PRINT_FONT: equ $B23C

HANGUL_CODE_START: equ $AC
HANGUL_CODE_END: equ $D7

O: equ -$1FEF

; This code should be inserted to offset 0x2000 in game disk A
; and will be loaded to memory address 0x8000.
INIT:
    org START_INIT
    push af
    push bc
    push de
    push hl
    push ix
    push iy
    ; copy data
    ld bc, PROLOGUE - HANGUL_CHO_DATA
    ld hl, HANGUL_CHO_DATA
    ld de, HANGUL_FONT_CHO
    ldir
    ld bc, END - START_PERSIST_CODE
    ld hl, PROLOGUE
    ld de, START_PERSIST_CODE
    ldir
    ; jump to PROLOGUE
    jp START_PERSIST_CODE


HANGUL_CHO_DATA:
    db $00,$00,$7E,$02,$02,$02,$02,$00
    db $00,$00,$40,$40,$40,$40,$7E,$00
    db $00,$00,$7E,$40,$40,$40,$7E,$00
    db $00,$00,$7E,$48,$48,$48,$7E,$00
    db $00,$00,$7E,$02,$7E,$40,$7E,$00
    db $00,$00,$7E,$42,$42,$42,$7E,$00
    db $00,$00,$42,$42,$7E,$42,$7E,$00
    db $00,$00,$3C,$42,$42,$42,$3C,$00
    db $00,$00,$7E,$02,$7E,$02,$02,$00
    db $00,$00,$7E,$40,$7E,$40,$7E,$00
    db $00,$00,$7E,$12,$12,$12,$12,$00
    db $00,$00,$7E,$24,$24,$5A,$00,$00
    db $00,$00,$7E,$24,$24,$24,$7E,$00
    db $00,$00,$54,$54,$7C,$54,$7C,$00
    db $00,$00,$10,$10,$10,$28,$44,$00
    db $00,$00,$7C,$10,$10,$28,$44,$00
    db $00,$00,$10,$7C,$10,$28,$44,$00
    db $00,$00,$10,$7C,$28,$44,$38,$00
    db $00,$00,$14,$14,$2A,$51,$00,$00
    db $00,$00,$28,$28,$54,$54,$00,$00
HANGUL_JUNG_DATA:
    db $00,$02,$02,$02,$03,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$02,$02,$03,$02,$03,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$02,$02,$02,$06,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$02,$02,$06,$02,$06,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$02,$02,$02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$05,$05,$05,$07,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$05,$05,$07,$05,$07,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$05,$05,$05,$0D,$05,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$05,$05,$0D,$05,$0D,$05,$05,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$02,$02,$02,$02,$02,$02,$FA,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$02,$02,$03,$02,$02,$22,$FA,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$05,$05,$05,$07,$05,$25,$FD,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$02,$02,$02,$02,$02,$22,$FA,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$02,$02,$06,$02,$02,$02,$FA,$22,$00,$00,$00,$00,$00,$00
    db $00,$00,$05,$05,$0D,$05,$05,$05,$F5,$25,$00,$00,$00,$00,$00,$00
    db $00,$00,$02,$02,$02,$02,$02,$02,$FA,$22,$00,$00,$00,$00,$00,$00
    db $00,$00,$00,$00,$00,$00,$08,$FF,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$00,$00,$00,$00,$24,$FF,$00,$00,$00,$00,$00,$00,$00,$00
    db $00,$00,$00,$00,$00,$00,$00,$FF,$08,$08,$08,$00,$00,$00,$00,$00
    db $00,$00,$00,$00,$00,$00,$00,$FF,$24,$24,$24,$00,$00,$00,$00,$00
    db $00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$00,$00,$00,$00,$00
HANGUL_JONG_DATA:
    db $00,$00,$00,$00,$00,$00,$00,$00
    db $00,$7E,$02,$02,$02,$00,$00,$00
    db $00,$7E,$12,$12,$12,$00,$00,$00
    db $00,$72,$12,$16,$19,$00,$00,$00
    db $00,$40,$40,$40,$7E,$00,$00,$00
    db $00,$47,$42,$46,$79,$00,$00,$00
    db $00,$44,$5F,$4A,$74,$00,$00,$00
    db $00,$7E,$40,$40,$7E,$00,$00,$00
    db $7E,$02,$7E,$40,$7E,$00,$00,$00
    db $7E,$12,$72,$42,$72,$00,$00,$00
    db $7E,$1A,$7A,$4A,$7E,$00,$00,$00
    db $7A,$1A,$7E,$4A,$7E,$00,$00,$00
    db $72,$12,$76,$49,$71,$00,$00,$00
    db $7E,$18,$7E,$48,$7E,$00,$00,$00
    db $7F,$1A,$7A,$4A,$7F,$00,$00,$00
    db $72,$1F,$76,$49,$76,$00,$00,$00
    db $00,$7E,$42,$42,$7E,$00,$00,$00
    db $42,$42,$7E,$42,$7E,$00,$00,$00
    db $00,$4A,$7A,$4E,$79,$00,$00,$00
    db $00,$08,$14,$22,$41,$00,$00,$00
    db $00,$14,$14,$2A,$51,$00,$00,$00
    db $00,$3C,$42,$42,$3C,$00,$00,$00
    db $00,$7E,$18,$24,$42,$00,$00,$00
    db $08,$7E,$18,$24,$42,$00,$00,$00
    db $00,$7E,$02,$7E,$02,$00,$00,$00
    db $7E,$40,$7E,$40,$7E,$00,$00,$00
    db $00,$7E,$24,$24,$7E,$00,$00,$00
    db $08,$7E,$24,$42,$3C,$00,$00,$00

CONV_CHO_DATA:
    db $00,$0A,$01,$02,$03,$04,$05,$06,$0D,$0E,$12,$07,$0F,$0B,$10,$08,$09,$0C,$11
CONV_JUNG_DATA:
    db $00,$05,$01,$06,$02,$07,$03,$08,$10,$0A,$0B,$0C,$11,$12,$0D,$0E,$0F,$13,$14,$09,$04

; Restore the original code and jump to it
PROLOGUE:
    org START_PERSIST_CODE
    ld a, (RAMAD2)
    ld hl, $8000
    call ENASLT
    ei
    ld c, $1A
    ld de, $8000
    call BDOS
    ld c, $2F
    ld hl, $0800
    ld de, $0597
    call BDOS
    ld a, (RAMAD2)
    ld hl, $8000
    call ENASLT
    ei

    ; patch values
    ld a, $0C
    ld ($B18E+O), a
    ld a, $68
    ld ($B2C8+O), a
    ld hl, $02DE
    ld ($B2CA+O), hl
    ld a, $0C
    ld ($B2CE+O), a
    ld a, $68
    ld ($B2D6+O), a
    ld hl, $02DE
    ld ($B2D8+O), hl
    ld a, $0C
    ld ($B2DC+O), a
    ld a, $68
    ld ($B2E1+O), a
    ld hl, $02DE
    ld ($B2E3+O), hl
    ld a, $0C
    ld ($B2E7+O), a
    ld a, $68
    ld ($B2EC+O), a
    ld hl, $02DE
    ld ($B2EE+O), hl
    ld a, $0C
    ld ($B2F6+O), a

    ; menu select rect y size
    ld a, $0E
    ld ($B5CD+O), a

    ; start menu y coords
    ld a, $28
    ld ($81D2), a
    ld a, $37
    ld ($81D6), a
    ld a, $46
    ld ($81DA), a

    ; string address table (test)
    ld hl, S1
    ld (T+$141*2), hl
    ld hl, S2
    ld (T+$131*2), hl
    ld hl, S3
    ld (T+$132*2), hl
    ld hl, S4
    ld (T+$133*2), hl
    ld hl, S5
    ld (T+$134*2), hl
    ld hl, S6
    ld (T+$13F*2), hl
    ld hl, S7
    ld (T+$14*2), hl

    ; patch code (hooks)
    ld a, $C3
    ld ($B08D+O), a
    ld hl, HOOK_B08D
    ld ($B08E+O), hl
    ld a, $C3
    ld ($B14B+O), a
    ld hl, HOOK_B14B
    ld ($B14C+O), hl
    ld a, $C3
    ld ($A9F7+O), a
    ld hl, HOOK_A9F7
    ld ($A9F8+O), hl
    ld a, $C3
    ld ($AA1B+O), a
    ld hl, HOOK_AA1B
    ld ($AA1C+O), hl

    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    jp START_INIT


DIV16:
    ld hl, $00
    ld a, b
    ld b, $08
Div16_Loop1:
    rla
    adc hl, hl
    sbc hl, de
    jr nc, Div16_NoAdd1
    add hl, de
Div16_NoAdd1:
    djnz Div16_Loop1
    rla
    cpl
    ld b, a
    ld a, c
    ld c, b
    ld b, $08
Div16_Loop2:
    rla
    adc hl, hl
    sbc hl, de
    jr nc, Div16_NoAdd2
    add hl, de
Div16_NoAdd2:
    djnz Div16_Loop2
    rla
    cpl
    ld b, c
    ld c, a
    ret


VDP_INDIRECT_REG_INIT: equ $AA2B ; This function turns off the interrupt.
VDP_LMMC_ARGS: db $60,$00,$DE,$02,$08,$00,$0C,$00
VDP_GET_STATUS:
    push bc
    ld bc, (WRVDP)
    inc c
    out (c), a
    ld a, $8F
    out (c), a
    ld bc, (RDVDP)
    inc c
    in a, (c)
    pop bc
    ret

; input
; (VAR_FONT) = font data (16bytes)
VDP_LMMC:
    push bc
    ld hl, VAR_FONT
    ld de, VAR_FONT_4BPP
    ld b, $0C
LOOP_CONVERT_4BPP:
    ld c, (hl)
    xor a
    bit 7, c
    jr z, LOOP_UNROLL_1
    ld a, $0F
LOOP_UNROLL_1:
    ld (de), a
    inc de
    xor a
    bit 6, c
    jr z, LOOP_UNROLL_2
    ld a, $0F
LOOP_UNROLL_2:
    ld (de), a
    inc de
    xor a
    bit 5, c
    jr z, LOOP_UNROLL_3
    ld a, $0F
LOOP_UNROLL_3:
    ld (de), a
    inc de
    xor a
    bit 4, c
    jr z, LOOP_UNROLL_4
    ld a, $0F
LOOP_UNROLL_4:
    ld (de), a
    inc de
    xor a
    bit 3, c
    jr z, LOOP_UNROLL_5
    ld a, $0F
LOOP_UNROLL_5:
    ld (de), a
    inc de
    xor a
    bit 2, c
    jr z, LOOP_UNROLL_6
    ld a, $0F
LOOP_UNROLL_6:
    ld (de), a
    inc de
    xor a
    bit 1, c
    jr z, LOOP_UNROLL_7
    ld a, $0F
LOOP_UNROLL_7:
    ld (de), a
    inc de
    xor a
    bit 0, c
    jr z, LOOP_UNROLL_8
    ld a, $0F
LOOP_UNROLL_8:
    ld (de), a
    inc de
    inc hl
    djnz LOOP_CONVERT_4BPP

    ld a, $24
    call VDP_INDIRECT_REG_INIT
    ld hl, VDP_LMMC_ARGS
    ld b, $08
    otir

    ld hl, VAR_FONT_4BPP
    ld a, (hl)
    out (c), a
    xor a
    out (c), a
    ld a, $B0
    out (c), a

    ld bc, (WRVDP)
    inc c
    ld a, $AC
    out (c), a
    ld a, $91
    out (c), a
    inc c
    inc c
VDP_LMMC_LOOP:
    ld a, $02
    call VDP_GET_STATUS
    bit 0, a
    jr z, VDP_LMMC_EXIT
    bit 7, a
    jr z, VDP_LMMC_LOOP
    inc hl
    ld a, (hl)
    out (c), a
    jr VDP_LMMC_LOOP
VDP_LMMC_EXIT:
    ld a, $00
    call VDP_GET_STATUS
    pop bc
    ei
    ret

WAIT_VDP:
    ld a, $02
    call VDP_GET_STATUS
    and $01
    jr nz, WAIT_VDP
    xor a
    call VDP_GET_STATUS
    ret

; input
; a = horz or vert (0, 1)
; b = number of line indexes
; (VAR_ARG_FONT) = font data (16bytes)
; (VAR_ARG_ARR) = line indexes
; output
; (VAR_ARG_FONT) = shifted font data
CONST_ZERO:
    db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

SHIFT_HORZ: equ $00
SHIFT_VERT: equ $01
SHIFT_CONST_BITS:
    db %1, %11, %111, %1111, %11111, %111111, %1111111, %11111111
SHIFT_FONT:
    push af
    push bc
    push de
    push hl
    or a
    jr nz, SHIFT_FONT_VERT
SHIFT_FONT_HORZ:
    ld c, $FF
SHIFT_FONT_HORZ_LOOP_FONT:
    inc c
    ld hl, VAR_ARG_FONT
    push bc
    ld b, $00
    add hl, bc
    ld e, (hl)
    pop bc
    ld d, $00
SHIFT_FONT_HORZ_LOOP_ARR:
    push bc
    ld hl, VAR_ARG_ARR
    ld b, $00
    ld c, d
    add hl, bc
    ld a, (hl)
    ld hl, SHIFT_CONST_BITS
    ld b, $00
    ld c, a
    add hl, bc
    ld c, (hl)
    ld a, e
    and c
    sla a
    and c
    ld b, a
    ld a, (hl)
    cpl
    and e
    or b
    ld e, a
    pop bc

    inc d
    ld a, d
    cp b
    jr nz, SHIFT_FONT_HORZ_LOOP_ARR

    push bc
    ld hl, VAR_TEMP_ARR
    ld b, $00
    add hl, bc
    ld (hl), e
    pop bc

    ld a, c
    cp $0F
    jr nz, SHIFT_FONT_HORZ_LOOP_FONT
    jp SHIFT_FONT_END

SHIFT_FONT_VERT:
    ld c, $FF
    ld d, $00
SHIFT_FONT_VERT_LOOP_FONT:
    inc c
    ld a, c
    cp $10
    jr z, SHIFT_FONT_VERT_LJUST

    ld hl, VAR_ARG_ARR
    ld e, $00
SHIFT_FONT_VERT_LOOP_SEARCH:
    ld a, c
    cp (hl)
    jr z, SHIFT_FONT_VERT_LOOP_FONT
    inc hl
    inc e
    ld a, b
    cp e
    jr nz, SHIFT_FONT_VERT_LOOP_SEARCH

    push bc
    ld hl, VAR_ARG_FONT
    ld b, $00
    add hl, bc
    ld a, (hl)
    ld hl, VAR_TEMP_ARR
    ld b, $00
    ld c, d
    add hl, bc
    ld (hl), a
    inc d
    pop bc
    jp SHIFT_FONT_VERT_LOOP_FONT
SHIFT_FONT_VERT_LJUST:
    ld b, $00
    ld c, d
SHIFT_FONT_VERT_LOOP_LJUST:
    ld hl, VAR_TEMP_ARR
    add hl, bc
    ld (hl), $00
    inc c
    ld a, c
    cp $10
    jr nz, SHIFT_FONT_VERT_LOOP_LJUST
SHIFT_FONT_END:
    ld de, VAR_ARG_FONT
    ld hl, VAR_TEMP_ARR
    ld bc, $0010
    ldir

    pop hl
    pop de
    pop bc
    pop af
    ret

HOOK_B08D_RET:
    ld a, $0A
    ld ($B2DC), a
    pop hl
    pop de
    pop bc
    pop af
    call PRINT_FONT
    jp $B090
HOOK_B08D:
    push af
    push bc
    push de
    push hl
    cp HANGUL_CODE_START
    jr c, HOOK_B08D_RET
    cp HANGUL_CODE_END+1
    jr nc, HOOK_B08D_RET
    sub HANGUL_CODE_START
    ld (VAR_CODE+1), a
    call GET_TEXT_CHAR
    pop hl
    inc hl
    push hl
    ld (VAR_CODE), a

    ld bc, (VAR_CODE)
    ld de, $024C
    call DIV16
    ld a, c
    ld (VAR_CHO), a
    ld bc, hl
    ld de, $1C
    call DIV16
    ld a, c
    ld (VAR_JUNG), a
    ld a, l
    ld (VAR_JONG), a

    ld a, (VAR_CHO)
    ld hl, TABLE_CONV_CHO
    ld d, $00
    ld e, a
    add hl, de
    ld a, (hl)
    ld (VAR_CHO), a

    ld a, (VAR_JUNG)
    ld hl, TABLE_CONV_JUNG
    ld e, a
    add hl, de
    ld a, (hl)
    ld (VAR_JUNG), a


    ld hl, HANGUL_FONT_CHO
    ld a, (VAR_CHO)
    sla a
    sla a
    sla a
    ld b, $00
    ld c, a
    add hl, bc
    ld de, VAR_ARG_FONT
    ld bc, $08
    ldir
    ld hl, CONST_ZERO
    ld bc, $08
    ldir

    ld a, (VAR_JUNG)
    cp 16
    jp nc, COND1
    ld a, (VAR_CHO)
    cp 11
    jr nc, COND2_ELIF1
; cho < 11
    ld a, SHIFT_HORZ
    ld b, $03
    ld hl, VAR_ARG_ARR
    ld (hl), $02
    inc hl
    ld (hl), $05
    inc hl
    ld (hl), $07
    call SHIFT_FONT
    jp COND3
COND2_ELIF1:
    cp 13
    jr nc, COND2_ELIF2
; cho < 13
    ld a, SHIFT_HORZ
    ld b, $02
    ld hl, VAR_ARG_ARR
    ld (hl), $03
    inc hl
    ld (hl), $07
    call SHIFT_FONT
    jp COND3
COND2_ELIF2:
    cp 18
    jr nc, COND2_ELSE
; cho < 18
    ld a, SHIFT_HORZ
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $07
    call SHIFT_FONT
    jp COND3
COND2_ELSE:
; else
    ld hl, HANGUL_FONT_CHO
    ld a, (VAR_CHO)
    inc a
    sla a
    sla a
    sla a
    ld b, $00
    ld c, a
    add hl, bc
    ld de, VAR_ARG_FONT
    ld bc, $08
    ldir
    ld hl, CONST_ZERO
    ld bc, $08
    ldir

    ld a, SHIFT_HORZ
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $07
    call SHIFT_FONT
COND3:
    ld de, VAR_FONT
    ld hl, VAR_ARG_FONT
    ld bc, $0010
    ldir
    ld e, $00

    ld a, (VAR_JUNG)
    cp 9
    jp c, COND_END
    ld a, (VAR_JONG)
    cp 0
    jp z, COND_END
; jung > 8 and jong != 0
    ld a, SHIFT_VERT
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $00
    call SHIFT_FONT

    ld de, VAR_FONT
    ld hl, VAR_ARG_FONT
    ld bc, $0010
    ldir

    ld hl, HANGUL_FONT_JUNG
    ld a, (VAR_JUNG)
    sla a
    sla a
    sla a
    sla a
    ld b, a
    ld a, $00
    adc a, $00
    ld c, b
    ld b, a
    add hl, bc
    ld de, VAR_ARG_FONT
    ld bc, $10
    ldir

    ld a, SHIFT_VERT
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $00
    call SHIFT_FONT

    ld e, $01
    jp COND_END
COND1:
; jung >= 16
    ld a, SHIFT_VERT
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $00
    call SHIFT_FONT

    ld de, VAR_FONT
    ld hl, VAR_ARG_FONT
    ld bc, $0010
    ldir
    ld e, $00

    ld a, (VAR_JUNG)
    cp 18
    jr c, COND_END
    ld a, (VAR_JONG)
    cp $00
    jr z, COND_END

    ld hl, HANGUL_FONT_JUNG
    ld a, (VAR_JUNG)
    sla a
    sla a
    sla a
    sla a
    ld b, a
    ld a, $00
    adc a, $00
    ld c, b
    ld b, a
    add hl, bc
    ld de, VAR_ARG_FONT
    ld bc, $10
    ldir

    ld a, SHIFT_VERT
    ld b, $02
    ld hl, VAR_ARG_ARR
    ld (hl), $00
    inc hl
    ld (hl), $0A
    call SHIFT_FONT
    ld e, $01
COND_END:
    ld a, e
    or a
    jr nz, MERGE

    ld hl, HANGUL_FONT_JUNG
    ld a, (VAR_JUNG)
    sla a
    sla a
    sla a
    sla a
    ld b, a
    ld a, $00
    adc a, $00
    ld c, b
    ld b, a
    add hl, bc
    ld de, VAR_ARG_FONT
    ld bc, $10
    ldir
MERGE:
    ld hl, VAR_FONT
    ld de, VAR_ARG_FONT
    ld b, $10
MERGE_LOOP1:
    ld a, (de)
    or (hl)
    ld (hl), a
    inc hl
    inc de
    djnz MERGE_LOOP1

    ld hl, HANGUL_FONT_JONG
    ld a, (VAR_JONG)
    sla a
    sla a
    sla a
    ld b, $00
    ld c, a
    add hl, bc
    ld d, h
    ld e, l

    ld hl, VAR_FONT
    ld bc, $08
    add hl, bc
    ld b, $08
MERGE_LOOP2:
    ld a, (de)
    or (hl)
    ld (hl), a
    inc hl
    inc de
    djnz MERGE_LOOP2

    ld de, VAR_ARG_FONT
    ld hl, VAR_FONT
    ld bc, $0010
    ldir
    ld a, SHIFT_VERT
    ld b, $01
    ld hl, VAR_ARG_ARR
    ld (hl), $00
    call SHIFT_FONT
    ld de, VAR_FONT
    ld hl, VAR_ARG_FONT
    ld bc, $000C
    ldir

FINAL:
    call VDP_LMMC
    ld a, $0C
    ld ($B2DC), a

    pop hl
    pop de
    pop bc
    pop af
; PRINT_FONT
    ld de, $B090
    push de
    push hl
    push bc
    rlca
    rlca
    rlca
    ld e, a
    and %11111000
    ld a, $60 ; hangul sx
    ld ($B2D2), a
    xor e
    ld e, a
    add a, a
    add a, a
    add a, e
    add a, a
    ld h, $00
    cp 40
    jr c, loc_B257
    sub 40
    ld h, $02
loc_B257:
    add a, $D4
    ld l, a
    ld hl, $02DE ; hangul sy
    ld ($B2D4), hl
    jp $B25D

HOOK_B14B_RET:
    ld ($DD58), a
    jp $B14E
HOOK_B14B:
    cp $14
    jr nz, HOOK_B14B_RET
    inc a
    jp HOOK_B14B_RET

HOOK_A9F7_RET:
    pop de
    pop bc
    pop af
    ld hl, $DD56
    jp $A9FA
HOOK_A9F7:
    push af
    push bc
    push de
    ld a, ($DD56)
    ld b, a
    ld a, ($DD58)
    ld c, a
    ld a, ($DD5A)
    ld d, a
    ld a, ($DD5C)
    add a, d
    cp $0A
    jr z, HANDLE2
    add a, b
    add a, c
    cp $B9
    jr z, START_MENU_RECT
    cp $94
    jr z, START_MENU_RECT2
    jp HOOK_A9F7_RET
START_MENU_RECT:
    ld a, $3C
    ld ($DD5C), a
    jp HOOK_A9F7_RET
START_MENU_RECT2:
    ld a, $F
    ld ($DD5C), a
    jp HOOK_A9F7_RET
HANDLE2:
    ld a, $34
    ld ($DD5C), a
    jp HOOK_A9F7_RET

HOOK_AA1B_RET:
    pop de
    pop bc
    pop af
    ld hl, $DD56
    jp $AA1E
HOOK_AA1B:
    push af
    push bc
    push de
    ld a, ($DD56)
    ld b, a
    ld a, ($DD58)
    ld c, a
    ld a, ($DD5A)
    ld d, a
    ld a, ($DD5C)
    ld a, c
    add a, d
    cp $1A
    jr z, HANDLE
    cp $14
    jr z, HANDLE3
    ld a, ($DD5C)
    add a, c
    add a, d
    cp $4A
    jr z, START_MENU_LINE
    add a, b
    cp $B8
    jr z, START_MENU_LINE2
    jp HOOK_AA1B_RET
START_MENU_LINE:
    ld a, $3C
    ld ($DD5C), a
    jp HOOK_AA1B_RET
START_MENU_LINE2:
    ld a, $53
    ld ($DD58), a
    jp HOOK_AA1B_RET
HANDLE3:
    ld a, $34
    ld ($DD5C), a
    jp HOOK_AA1B_RET
HANDLE:
    ld a, $46
    ld ($DD58), a
    jp HOOK_AA1B_RET

; Test
T: equ $D531+O
S1: db $C9,$00,$BE,$44,$00 ; 준비
S2: db $C2,$DC,$C7,$91,$00 ; 시작
S3: db $06,$3F,$01,$B4,$F1,$B8,$5D,$00 ; 등록
S4: db $06,$3F,$01,$C0,$AD,$C8,$1C,$00 ; 삭제
S5: db $06,$3F,$01,$C1,$20,$D0,$DD,$00 ; 선택
S6: db $D5,$0C,$B8,$08,$c7,$74,$C5,$B4,$20,$00 ; 플레이어
S7: db $C0,$C1,$D0,$DC,$00 ; 상태
END: