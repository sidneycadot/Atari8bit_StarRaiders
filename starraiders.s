
; Star Raiders
; Copyright (c) 1979 Atari
; Programmed by ...

; Macros

; this macro replaces spaces in a string by zero bytes
.macro space_to_zero str
  .repeat .strlen(str),i
    .if .strat(str, i) = ' '
      .byte 0
    .else
      .byte .strat(str, i)
    .endif
  .endrep
.endmacro

; this macro emits a string, where the first character has its sign bit set
.macro message str
  .byte   .strat(str, 0) | $80
  .repeat .strlen(str)-1,i
    .byte   .strat(str, i+1)
  .endrep
.endmacro

; Private vars

LEVEL           = $62  ; level of play; 0--3 -> novice/pilot/warrior/commander
MY_CONSOL       = $63  ; stores the value read from CONSOL in the previous loop
MY_BITS         = $64  ; a seemingly important bitfield...

VBL_DONE        = $67  ; nonzero if VBL just finished
MY_KEY          = $CA  ; stores the keystroke
STATUS_LINE     = $D1F ; status line holding text

; for top of screen

TOP_COLPM0  = $EE
TOP_COLPM1  = $EF
TOP_COLPM2  = $F0
TOP_COLPM3  = $F1
TOP_COLPF0  = $F2
TOP_COLPF1  = $F3
TOP_COLPF2  = $F4
TOP_COLPF3  = $F5
TOP_COLBK   = $F6

; for bottom of screen

BOT_COLPF0 = $F7
BOT_COLPF1 = $F8
BOT_COLPF2 = $F9
BOT_COLPF3 = $FA
BOT_COLBK  = $FB

MY_DLIST = $280

MY_HPOSP0 = $C2A
MY_HPOSP1 = $C2B
MY_HPOSP2 = $C2C
MY_HPOSP3 = $C2D
MY_HPOSM3 = $C2E

; OS registers

VDSLST   =  $200 ; vector for Display List Interrupt
VIMIRQ   =  $216 ; vector for IRQ Interrupt
VVBLKI   =  $222 ; vector for Vertical Blank Interrupt

; CTIA/GTIA registers

CTIA     = $D000;

HPOSP0   = CTIA + $00 ; (W) Horizontal position of player 0
HPOSP1   = CTIA + $01 ; (W) Horizontal position of player 1
HPOSP2   = CTIA + $02 ; (W) Horizontal position of player 2
HPOSP3   = CTIA + $03 ; (W) Horizontal position of player 3

HPOSM0   = CTIA + $04 ; (W) Horizontal position of missile 0
HPOSM1   = CTIA + $05 ; (W) Horizontal position of missile 1
HPOSM2   = CTIA + $06 ; (W) Horizontal position of missile 2
HPOSM3   = CTIA + $07 ; (W) Horizontal position of missile 3

M0PL     = CTIA + $08 ; (R) Missile 0 to Player collissions
M1PL     = CTIA + $09 ; (R) Missile 1 to Player collissions
M2PL     = CTIA + $0A ; (R) Missile 2 to Player collissions
M3PL     = CTIA + $0B ; (R) Missile 3 to Player collissions

P0PL     = CTIA + $0C ; (R) Player 0 to Player collisions
P1PL     = CTIA + $0D ; (R) Player 1 to Player collisions
P2PL     = CTIA + $0E ; (R) Player 2 to Player collisions
P3PL     = CTIA + $0F ; (R) Player 3 to Player collisions

TRIG0    = CTIA + $10 ; (R) Read joystick trigger #0
TRIG1    = CTIA + $11 ; (R) Read joystick trigger #1
TRIG2    = CTIA + $12 ; (R) Read joystick trigger #2
TRIG3    = CTIA + $13 ; (R) Read joystick trigger #3

COLPM0   = CTIA + $12 ; (W) Color of player and missile #0
COLPM1   = CTIA + $13 ; (W) Color of player and missile #1
COLPM2   = CTIA + $14 ; (W) Color of player and missile #2
COLPM3   = CTIA + $15 ; (W) Color of player and missile #3
COLPF0   = CTIA + $16 ; (W) Color of playfield #0
COLPF1   = CTIA + $17 ; (W) Color of playfield #1
COLPF2   = CTIA + $18 ; (W) Color of playfield #2
COLPF3   = CTIA + $19 ; (W) Color of playfield #3
COLBK    = CTIA + $1A ; (W) Color of background
PRIOR    = CTIA + $1B ; (W) Player/Missile Priority selection register
GRACTL   = CTIA + $1D ; (W) Trigger Latch & P/M control
HITCLR   = CTIA + $1E ; (W) Reset P/M collision registers
CONSOL   = CTIA + $1F ; (R) Console key readout

; POKEY registers

POKEY = $D200

KBCODE   = POKEY + $09 ; (R) keyboard code
RANDOM   = POKEY + $0A ; (R) random number generator
IRQEN    = POKEY + $0E ; (W) IRQ enable register
SKCTL    = POKEY + $0F ; (W) serial port control

; PIA (6520) registers

PIA = $D300

PORTA    = PIA + $00 ; (R/W) Joystick Port #1
PORTB    = PIA + $01 ; (R/W) Joystick Port #2
PACTL    = PIA + $02 ; (R/W) Port A Controller
PBCTL    = PIA + $03 ; (R/W) Port B Controller

; ANTIC registers

ANTIC = $D400

DMACTL   = ANTIC + $00 ; (W) DMA control
CHACTL   = ANTIC + $01 ; (W) Character mode control
DLISTL   = ANTIC + $02 ; (W) Display List pointer (LSB)
DLISTH   = ANTIC + $03 ; (W) Display List pointer (MSB)
PMBASE   = ANTIC + $07 ; (W) Player/Missile Base Address (MSB)
CHBASE   = ANTIC + $09 ; (W) Character Set base address (MSB)
WSYNC    = ANTIC + $0A ; (W) wait for horizontal synchronization
VCOUNT   = ANTIC + $0B ; (R) Vertical line counter
NMIEN    = ANTIC + $0E ; (W) NMI interrupt enable mask

; ROM addresses

CHARSET1 = $E000 ; standard Atari character set

; ----------------------------------------------------------------------------

        .segment "cartridge"
        .org $A000

; ----------------------------------------------------------------------------

; character set and display list

MY_CHARSET:     .byte   $00,$7F,$47,$47,$47,$47,$47,$7F
                .byte   $00,$30,$10,$10,$10,$38,$38,$38
                .byte   $00,$78,$08,$08,$78,$40,$40,$78
                .byte   $00,$78,$08,$08,$7C,$0C,$0C,$7C
                .byte   $00,$60,$60,$60,$6C,$7C,$0C,$0C
                .byte   $00,$78,$40,$40,$78,$08,$08,$78

                .byte   %00000000       ; 6-ish
                .byte   %01111000
                .byte   %01001000
                .byte   %01000000
                .byte   %01000000
                .byte   %01111110
                .byte   %01000010
                .byte   %01111110

                .byte   %00000000       ; question mark
                .byte   %01111100
                .byte   %01000100
                .byte   %00000100
                .byte   %00011100
                .byte   %00010000
                .byte   %00010000
                .byte   %00010000

                .byte   %00000000       ; b (?)
                .byte   %00111000
                .byte   %00101000
                .byte   %00101000
                .byte   %01111100
                .byte   %01101100
                .byte   %01101100
                .byte   %01111100

                .byte   %00000000       ; q (?)
                .byte   %01111100
                .byte   %01000100
                .byte   %01000100
                .byte   %01111100
                .byte   %00001100
                .byte   %00001100
                .byte   %00001100

                .byte   %00000000       ; space
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000

                .byte   %00111000       ; colon?
                .byte   %00111000
                .byte   %00111000
                .byte   %00000000
                .byte   %00000000
                .byte   %00111000
                .byte   %00111000
                .byte   %00111000

                .byte   %10000000       ; empty square?
                .byte   %10000000
                .byte   %10000000
                .byte   %10000000
                .byte   %10000000
                .byte   %10000000
                .byte   %10000000
                .byte   %11111111

                .byte   %00000000       ; 3-piece squadron
                .byte   %00111100
                .byte   %00100000
                .byte   %00100000
                .byte   %01111000
                .byte   %01100000
                .byte   %01100000
                .byte   %01111100

                .byte   %00000000       ; infinity sign
                .byte   %01100110
                .byte   %10011001
                .byte   %10011001
                .byte   %10011001
                .byte   %01100110
                .byte   %00000000
                .byte   %00000000

                .byte   %00000000       ; '-'
                .byte   %00000000
                .byte   %00000000
                .byte   %01111110
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000
                .byte   %00000000

                .byte   $00,$18,$18,$18,$7E,$18,$18,$18
                .byte   $00,$18,$7E,$DB,$99,$DB,$7E,$18
                .byte   $66,$66,$66,$66,$66,$2C,$38,$30
                .byte   $00,$7C,$44,$44,$7C,$68,$6C,$6C
                .byte   $00,$1C,$3E,$63,$5D,$63,$3E,$1C
                .byte   $00,$46,$46,$44,$7C,$64,$66,$66
                .byte   $FE,$92,$10,$18,$18,$18,$18,$18
                .byte   $FC,$8C,$8C,$80,$80,$80,$84,$FC
                .byte   $00,$00,$00,$00,$00,$00,$00,$FF
                .byte   $80,$80,$80,$80,$80,$80,$80,$80
                .byte   $00,$00,$00,$00,$00,$00,$00,$80
                .byte   $80,$AA,$9C,$BE,$9C,$AA,$80,$FF
                .byte   $80,$98,$80,$B6,$80,$8C,$80,$FF
                .byte   $80,$8E,$80,$B8,$80,$9C,$80,$FF
                .byte   $80,$B0,$98,$BE,$98,$B0,$80,$FF

LA0F8:          space_to_zero "  long range scan"
LA109:          space_to_zero "      aft view   "
LA11A:          space_to_zero "   galactic chart   "

LA12E:          .byte   $60 ; blank(7)
                .byte   $46 ; mode (6) + LMS
                .word   LA11A
                .byte   $F0 ; blank(8) + DLI
                .byte   $47 ; mode (7) + LMS
                .word   $D35
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $07 ; mode (7)
                .byte   $80 ; blank(1) + DLI
                .byte   $46 ; mode (6) + LMS
                .word   $D1F
                .byte   $46 ; mode (6) + LMS
                .word   $971
                .byte   $06 ; mode (6)
                .byte   $06 ; mode (6)
                .byte   $41 ; jump-vblank
                .word   MY_DLIST

; ----------------------------------------------------------------------------

; the /big/ main routine!

main:           lda     #0
                sta     SKCTL
                sta     $66
                sta     LEVEL
                sta     MY_CONSOL

                lda     #3
                sta     SKCTL

main_1:         ldy     #$2F
main_2:         lda     #$FF
main_3:         sty     $65
                sta     MY_BITS

                ; zero the following memory regions:
                ;   $D000 - $D0FF
                ;   $D300 - $D3FF
                ;   $D400 - $D4FF
                ;   $D200 - $D20F
                ;   $0067 - $016E

                lda     #0
                tax
@1:             sta     $D000,x
                sta     $D400,x
                cpx     #$0F
                bcs     @2
                sta     $D200,x
@2:             sta     $D300,x
                sta     a:VBL_DONE,x
                inx
                bne     @1

                ; set stack pointer to top-of-stack

                dex
                txs

                ; clear decimal mode

                cld

                ; clear memory range $200..$1FFF

                lda     #>$0200
                jsr     clear_range_up_to_1fff

                ; initialize IRQ/DLI/VBLANK interrupt vectors

                lda     #<IRQ_HANDLER
                sta     VIMIRQ
                lda     #>IRQ_HANDLER
                sta     VIMIRQ+1
                lda     #<VBLANK_HANDLER
                sta     VVBLKI
                lda     #<DLIST_HANDLER
                sta     VDSLST
                lda     #>VBLANK_HANDLER
                sta     VVBLKI+1
                lda     #>DLIST_HANDLER
                sta     VDSLST+1

                ;

                lda     #4
                sta     PACTL
                lda     #$11
                sta     PRIOR
                lda     #3
                sta     GRACTL
                jsr     SUB71
                ldx     #$0A
                jsr     SUB62
                lda     MY_BITS
                and     #$80
                tay
                ldx     #$5F
                lda     #8
                jsr     SUB87
                lda     #$20
                sta     $71
                lda     #<MY_DLIST
                sta     DLISTL
                lda     #>MY_DLIST
                sta     DLISTH
                lda     #$3E
                sta     DMACTL
                lda     #0
                sta     PMBASE
                lda     #$10
                sta     $79
                ldx     LEVEL
                ldy     LBF0C,x
                jsr     SUB68
                lda     #$40
                sta     $D20E

                ; enable IRQ, DLI, and VBLANK interrupts

                cli
                lda     #$C0
                sta     NMIEN

vblank_loop:

                ; busy-wait for VBL notification

                lda     VBL_DONE
                beq     vblank_loop
                lda     #0
                sta     VBL_DONE

                ;

                lda     $7A
                beq     skip_1

loop_1:         ldx     #4
@loop:          inx
                ldy     $0C5B,x
                lda     $0800,y
                sta     $68
                lda     $0864,y
                sta     $69
                ldy     $0C8C,x
                lda     $0CBD,x
                sta     ($68),y
                cpx     $7A
                bcc     @loop

                ;

                lda     #0
                sta     $7A

skip_1:         lda     $C0
                bmi     skip_2

loop_2:         ldx     $79
                stx     $7A
@loop:          lda     $0BF9,x
                sta     $0C5B,x
                tay
                lda     $0800,y
                sta     $68
                lda     $0864,y
                sta     $69
                lda     $0C2A,x
                lsr     a
                lsr     a
                sta     $0C8C,x
                tay
                lda     ($68),y
                sta     $0CBD,x
                ora     $0CEE,x
                sta     ($68),y
                dex
                cpx     #4
                bne     @loop

skip_2:         lda     $66
                bpl     loop_3

                ;

                lda     #0
                sta     $17E3
                sta     $17E4
                sta     $17BC
                sta     $17BB

                ;

loop_3:         lda     #0
                ldy     $0C5F
                ldx     $0CC1
@loop:          sta     $0300,y
                iny
                dex
                bpl     @loop

                ;

loop_4:         ldy     $0C5E
                ldx     $0CC0
@loop:          sta     $0700,y
                iny
                dex
                bpl     @loop

                ;

loop_5:         ldy     $0C5D
                ldx     $0CBF
@loop:          sta     $0600,y
                iny
                dex
                bpl     @loop

                ;

loop_6:         ldy     $0C5C
                ldx     $0CBE
@loop:          sta     $0500,y
                iny
                dex
                bpl     @loop

                ;

loop_7:         ldy     $0C5B
                ldx     $0CBD
@loop:          sta     $0400,y
                iny
                dex
                bpl     @loop

                ;

                lda     $0C90
                cmp     #1
                ldy     $E8
                ldx     $0BFD
                stx     $0C5F
                lda     $0CF2
                sta     $6A
                sta     $0CC1

                ;

@11:            lda     LB8E4,y
                bcs     @12
                and     RANDOM
@12:            sta     $0300,x
                iny
                inx
                dec     $6A
                bpl     @11

                ;

                lda     $0C8F
                cmp     #1
                ldy     $E7
                ldx     $0BFC
                stx     $0C5E
                lda     $0CF1
                sta     $6A
                sta     $0CC0

                ; loop

@13:            lda     LB8E4,y
                bcs     @14
                and     RANDOM
@14:            sta     $0700,x
                inx
                iny
                dec     $6A
                bpl     @13

                ;

                lda     $0C8E
                cmp     #1
                ldy     $E6
                ldx     $0BFB
                stx     $0C5D
                lda     $0CF0
                sta     $6A
                sta     $0CBF

                ; loop

@15:            lda     LB8E4,y
                bcs     @16
                and     RANDOM
@16:            sta     $0600,x
                inx
                iny
                dec     $6A
                bpl     @15

                ;

                ldy     $E5
                ldx     $0BFA
                stx     $0C5C
                lda     $0CEF
                sta     $6A
                sta     $0CBE

                ; loop

@17:            lda     LB9B1,y
                sta     $0500,x
                inx
                iny
                dec     $6A
                bpl     @17

                ;

                ldy     $E4
                ldx     $0BF9
                stx     $0C5B
                lda     $0CEE
                sta     $6A
                sta     $0CBD

                ; loop

@18:            lda     LB9B1,y
                sta     $0400,x
                inx
                iny
                dec     $6A
                bpl     @18

                ; copy Player positions

                lda     MY_HPOSP0
                sta     HPOSP0
                lda     MY_HPOSP1
                sta     HPOSP1
                lda     MY_HPOSP2
                sta     HPOSP2
                lda     MY_HPOSP3
                sta     HPOSP3
                lda     MY_HPOSM3
                sta     HPOSM3
                clc
                adc     #2
                sta     HPOSM2
                adc     #2
                sta     HPOSM1
                adc     #2
                sta     HPOSM0

                ;

                bit     $D0
                bmi     @22
                lda     $C8
                beq     @20
                sta     $6D

                ; loop

                ldy     $79
@19:            sty     $6E
                clc
                tya
                tax
                adc     #$31
                tay
                jsr     SUB65
                tya
                tax
                ldy     $6E
                jsr     SUB65
                dey
                bpl     @19

                ;

@20:            lda     $C9
                beq     @22
                sta     $6D

                ; loop

                ldy     $79
@21:            sty     $6E
                clc
                tya
                tax
                adc     #$62
                tay
                jsr     SUB65
                tya
                tax
                ldy     $6E
                jsr     SUB65
                dey
                bpl     @21

                ; loop

@22:            ldx     $79
@23:            cpx     #5
                bcs     @24
                lda     $0C8C,x
                beq     @25
@24:            sec
                lda     $0AD3,x
                sbc     $70
                sta     $0AD3,x
                lda     $0A40,x
                sbc     $C1
                sta     $0A40,x
                lda     $09AD,x
                sbc     #0
                sta     $09AD,x
@25:            dex
                bpl     @23

                ; loop

                ldx     $79
@26:            cpx     #$10
                bne     @27
                ldx     #4
@27:            txa
@28:            tay
                lda     #0
                sta     $6B
                lda     $0B66,y
                bpl     @29
                eor     #$7F
                clc
                adc     #1
                bcs     @29
                dec     $6B
@29:            clc
                adc     $0AD3,y
                sta     $0AD3,y
                lda     $0A40,y
                adc     $6B
                sta     $0A40,y
                lda     $09AD,y
                adc     $6B
                sta     $09AD,y
                tya
                clc
                adc     #$31
                cmp     #$90
                bcc     @28
                dex
                bpl     @26

                ; loop

                ldy     #4
@30:            tya
                tax
                lda     #2
                sta     $6A
@31:            lda     $09AD,x
                cmp     #2
                bcc     @33
                asl     a
                lda     #0
                sta     $09AD,x
                bcs     @32
                inc     $09AD,x
                eor     #$FF
@32:            sta     $0A40,x
@33:            txa
                clc
                adc     #$31
                tax
                dec     $6A
                bpl     @31
                dey
                bpl     @30

                ;

                lda     $D0
                cmp     #2
                bcs     @40

                ; loop

                ldx     $79
@34:            lda     #$FF
                ldy     $09AD,x
                cpy     $D0
                beq     @39
                lda     $0A0F,x
                bne     @35
                sec
                lda     #0
                sbc     $0B35,x
                sta     $6A
                lda     #0
                sbc     $0AA2,x
                sta     $6B
                jmp     @36
@35:            lda     $0B35,x
                sta     $6A
                lda     $0AA2,x
                sta     $6B
@36:            jsr     SUB73
                jsr     SUB92
                lda     $09DE,x
                bne     @37
                sec
                lda     #0
                sbc     $0B04,x
                sta     $6A
                lda     #0
                sbc     $0A71,x
                sta     $6B
                jmp     @38
@37:            lda     $0B04,x
                sta     $6A
                lda     $0A71,x
                sta     $6B
@38:            jsr     SUB73
@39:            jsr     SUB64
                dex
                bpl     @34

@40:            jsr     SUB56
                bit     $D0
                bvc     @44
                ldx     #$31
                jsr     SUB52
                bit     $0996
                bvs     @44

                ; loop

                ldx     $79
@41:            lda     $0A40,x
                ldy     $09AD,x
                bne     @42
                eor     #$FF
@42:            tay
                lda     $0DE9,y
                jsr     SUB92
                lda     $0A71,x
                ldy     $09DE,x
                bne     @43
                eor     #$FF
@43:            tay
                lda     $0DE9,y
                jsr     SUB64
                dex
                bpl     @41

                ; loop

@44:            ldx     #5
@45:            dex
                bpl     @46
                jmp     @54
@46:            lda     #0
                sta     $E4,x
                sta     $0CEE,x
                bit     $D0
                bpl     @48
                cpx     #3
                bcc     @45
@47:            lda     RANDOM
                ldy     #$F2
                bmi     @50
@48:            cmp     $E9,x
                beq     @45
                bvs     @47
                ldy     $0A40,x
                bit     $7B
                bvc     @50
                cpx     #2
                bcs     @49
                lda     $0C2C
                clc
                adc     LBEDB,x
                sta     $0C2A,x
                lda     $0BFB
                clc
                adc     #4
                sta     $0BF9,x
                ldy     $0A42
@49:            lda     $76
                and     #$0F
@50:            sta     $6B
                tya
                ldy     $0BF9,x
                cpy     #$CC
                bcs     @45
                ldy     $D0
                beq     @51
                eor     #$FF
@51:            cmp     #$20
                bcs     @45
                cmp     #$10
                bcc     @52
                lda     #$0F
@52:            sta     $6A
                ora     $0C8C,x
                lsr     a
                tay
                lda     LBE2F,y
                sta     $E4,x
                lda     LBE7F,y
                sta     $0CEE,x
                tya
                lsr     a
                lsr     a
                lsr     a
                tay
                lda     LBFD1,y
                cpy     #8
                bne     @53
                eor     RANDOM
@53:            ldy     $6A
                eor     LBFDB,y
                eor     $6B
                ldy     data2,x
                sta     $EE,y
                jmp     @45

                ;

@54:            ldy     #$AF
                ldx     $81
                lda     $8B
                beq     @55
                dec     $8B
                ldy     #$4F
                and     #$20
                beq     @55
                ldx     #$42
                ldy     #$60
@55:            sty     $F4
                stx     TOP_COLBK

                ; loop

                ldx     $79
@56:            lda     $0A40,x
                ldy     $D0
                cpy     #1
                bne     @58
                cmp     #$F0
                bcs     @57
                jsr     SUB94
@57:            eor     #$FF
@58:            cmp     #$10
                bcc     @59
                lda     #$0F
@59:            asl     a
                and     #$1C
                ora     $72
                tay
                lda     LBA90,y
                sta     $6A
                lda     $0C2A,x
                and     #3
                tay
                lda     LBAB0,y
                and     $6A
                sta     $0CEE,x
                dex
                cpx     #5
                bcs     @56

                ;

                bit     MY_BITS
                bvc     @60
                jmp     @66

                ; if

@60:            jsr     SUB79
                lda     $D300
                tay
                and     #3
                tax
                lda     LBAF5,x
                sta     $C9
                tya
                lsr     a
                lsr     a
                and     #3
                tax
                lda     LBAF5,x
                sta     $C8
                jsr     SUB80
                jsr     SUB84
                bit     $0995
                bvs     @64
                lda     $7E
                beq     @64
                lda     $D0
                bne     @61
                jsr     SUB66
@61:            ldx     $095C
                lda     $BF
                bmi     @62
                tax
                ora     #$80
                sta     $BF
@62:            lda     $E9,x
                bne     @63
                txa
                eor     #1
                tax
                lda     $E9,x
                bne     @63
                ldx     $095C
@63:            stx     $095C
                lda     $7C
                beq     @64
                lda     $D0
                cmp     #2
                bcs     @64
                eor     #1
                cmp     $09AD,x
                beq     @64
                tax
                lda     LBECF,x
                sta     MY_KEY
@64:            jsr     SUB88
                jsr     SUB72
                lda     $7B
                bne     @66
                lda     $EB
                beq     @66
                ldy     $0A42
                iny
                cpy     #2
                bcs     @66
                ldy     $0A73
                iny
                cpy     #2
                bcs     @66
                ldy     $0AA4
                iny
                cpy     #2
                bcs     @66
                jsr     SUB81
                ldy     #2
                jsr     SUB89
                ldx     #$7F
                lda     $81
                bne     @65
                ldx     #$0A
                jsr     SUB62
                ldy     #$23
                ldx     #8
                jsr     SUB58
                ldx     #$5F
                ldy     #$80
                lda     #8
                jsr     SUB87
                jsr     clear_range_1000_to_1fff
                ldx     #$40
                stx     $E3
                ldx     #$FF
@65:            stx     $8A
                lda     #0
                sta     $EB
                lda     #2
                sta     $BE
                ldx     #1
                jsr     SUB98
                ldx     #$0A
                jsr     SUB83

                ; endif

                ; handle console commands...

@66:            ldy     MY_CONSOL
                lda     CONSOL
                eor     #$FF
                and     #3
                sta     MY_CONSOL
                beq     @68
                dey
                bpl     @68
                sta     $66
                cmp     #2
                bcs     @67
                lda     #0
                tay
                jmp     main_3

@67:            inc     LEVEL
                lda     LEVEL
                and     #3
                sta     LEVEL
                jmp     main_1

@68:            jsr     vbl_loop_end_1
                jsr     vbl_loop_end_2
                jsr     vbl_loop_end_3
                jsr     vbl_loop_end_4
                jmp     vblank_loop

; ----------------------------------------------------------------------------

; Vertical Blank Interrupt handler

VBLANK_HANDLER: ; notify main loop that VBLANK processing is done,
                ; when we are finished.

                lda     #$FF
                sta     VBL_DONE

                ; prepare correct character set

                lda     #>CHARSET1
                sta     CHBASE

                ; handle background color

                ldx     TOP_COLBK
                lda     RANDOM
                bit     $8A
                bvc     @2
                bmi     @1
                and     #$72
                ora     #$40
@1:             tax
@2:             lda     $D0
                cmp     #3
                bcc     @3
                ldx     #$A0
@3:             stx     TOP_COLBK

                ; copy colors for the top of the screen

                ldx     #8
@cloop:         lda     TOP_COLPM0,x
                sta     COLPM0,x
                dex
                bpl     @cloop

                ; reset Player/Missile collisions

                sta     HITCLR

                ; ???

                jsr     SUB54
                inc     $77
                bne     @5
                lda     $66
                bmi     @5
                inc     $66
                bpl     @5
                ldy     #0
                jmp     main_2
@5:             jmp     doRTI

; ----------------------------------------------------------------------------

                ; Display List Interrupt Handler

DLIST_HANDLER:  pha
                txa
                pha
                tya
                pha

                ; set correct character set

                lda     #>CHARSET1
                ldy     VCOUNT
                cpy     #$60
                beq     @1
                lda     #>MY_CHARSET
@1:             sta     CHBASE

                ; set color of playfield 0--3 and background.

                ldx     #4
                sta     WSYNC
@2:             lda     BOT_COLPF0,x
                sta     COLPF0,x
                dex
                bpl     @2

                lda     M0PL
                ora     M1PL
                ora     M2PL
                ora     M3PL
                sta     $83
                lda     P3PL
                sta     $82

doRTI:          pla
                tay
                pla
                tax
                pla
                rti

; ----------------------------------------------------------------------------

                ; IRQ Interrupt Handler

IRQ_HANDLER:    pha
                lda     #0
                sta     IRQEN
                lda     #$40
                sta     IRQEN
                lda     KBCODE
                ora     #$C0
                sta     MY_KEY
                pla
                rti

; ----------------------------------------------------------------------------

loop:           sta     $A4,y
                inx
                dey
                bpl     priv52
                jsr     SUB49

SUB52:          lda     #5
                sta     $A2
                bit     $0995
                bvs     RTS52
                ldy     #2

priv52:         lda     LBAF9,x
                cmp     #$FE
                bne     loop
RTS52:          rts

; ----------------------------------------------------------------------------

SUB49:          lda     #$55

; ----------------------------------------------------------------------------

SUB48:          sta     $6B
                lda     $A4
                sta     $6E
                and     #$7F
                sta     $A4
@1:             ldy     $A5
                lda     $0800,y
                sta     $68
                lda     $0864,y
                sta     $69
                lda     $A6
                lsr     a
                lsr     a
                sta     $6A
                lda     $A6
                and     #3
                tay
                lda     LBAB0,y
                and     $6B
                ldy     $6A
                ora     ($68),y
                sta     ($68),y
                bit     $6E
                bpl     @2
                inc     $A5
                bne     @3
@2:             inc     $A6
@3:             dec     $A4
                bne     @1
                rts

; ----------------------------------------------------------------------------

SUB66:          ldx     $095C
                ldy     $A2
                cpy     #5
                bcs     @4
                lda     $A0
                sta     $A6
                lda     LBF6E,y
@1:             asl     a
                sta     $6C
                bcc     @2
                lda     #$81
                sta     $A4
                lda     $A1
                sta     $A5
                lda     #$AA
                jsr     SUB48
@2:             inc     $A6
                lda     $6C
                bne     @1
                inc     $A1
@3:             inc     $A2
                rts

@4:             cpy     #$0A
                bcc     @3
                lda     $E9,x
                beq     @9
                lda     $0A71,x
                ldy     $09DE,x
                beq     @5
                cmp     #$0C
                bcc     @6
                lda     #$0B
                bpl     @6
@5:             cmp     #$F5
                bcs     @6
                lda     #$F5
@6:             clc
                adc     #$83
                sta     $A0
                lda     $0AA2,x
                eor     #$FF
                ldy     $0A0F,x
                bne     @7
                cmp     #5
                bcc     @8
                lda     #4
                bpl     @8
@7:             cmp     #$FA
                bcs     @8
                lda     #$FA
@8:             clc
                adc     #$4D
                sta     $A1
                lda     #0
                sta     $A2
@9:             lda     #$36
                sta     $68
                lda     #$1B
                sta     $69
                ldx     #$0E
@10:            ldy     #6
@11:            lda     ($68),y
                and     #$55
                sta     ($68),y
                dey
                bpl     @11
                clc
                lda     $68
                adc     #$28
                sta     $68
                bcc     @12
                inc     $69
@12:            dex
                bpl     @10
                ldx     $095C
                iny
                lda     $88
                beq     @13
                dec     $88
                bne     @14
@13:            lda     $A0
                cmp     #$81
                bcc     @14
                cmp     #$85
                bcs     @14
                lda     #$AA
                sta     $1BFE
                sta     $1C04
                lda     $A1
                cmp     #$4B
                bcc     @14
                cmp     #$4F
                bcs     @14
                lda     #$AA
                sta     $1C9E
                sta     $1CA4
                lda     $0A40,x
                cmp     #$0C
                bcs     @14
                ldy     #$A0
                sty     $1D40
                sty     $1D68
                sty     $1D42
                sty     $1D6A
@14:            sty     $A3
                rts

; ----------------------------------------------------------------------------

vbl_loop_end_2: ldy     $C0
                beq     @rts1
                lda     $70
                cmp     #$FE
                bcs     @4
                cmp     #$80
                bcc     @1
                jsr     vbl_loop_end_2_sub_A
@1:             lda     #3
                sta     $095C
                lda     #$90
                sta     $0C8F
                sta     $EC
                lda     #$1F
                sta     $0A43
                sec
                lda     $0BFC
                sbc     #$77
                clc
                adc     $C5
                and     #$7F
                sta     $8E
                sec
                lda     $0C2D
                sbc     #$7D
                clc
                adc     $C4
                and     #$7F
                sta     $8F
                lda     LEVEL
                beq     @3
                lda     RANDOM
                ldy     $D0
                beq     @2
                sta     $0C2D
                sta     $0BFC
@2:             cmp     #$10
                bcs     @rts1
@3:             lda     RANDOM
                ora     #$10
                and     $C6
                sta     $0B9A
                lda     RANDOM
                ora     #$10
                and     $C6
                sta     $0BCB
@rts1:          rts

@4:             tya
                bmi     @5
                lda     #$FF
                sta     $C0
                ldx     #0
                jsr     SUB53
                jsr     SUB55
                ldy     #$1B
                jmp     SUB75

@5:             dec     $91
                beq     @6
                ldx     #2
                jmp     SUB98

@6:             ldy     #$19
                jsr     SUB76
                lda     $8F
                sta     $8D
                lda     $8E
                sta     $8C
                lsr     a
                and     #7
                tax
                lda     LBFB3,x
                sta     $C7
                ldy     $92
                sty     $90
                lda     #0
                sta     $7B
                ldx     $08C9,y
                bpl     @8
                lda     #$FF
                sta     $7B
                ldy     #0
@7:             lda     #0
                sta     $0B68,y
                lda     #1
                sta     $09AF,y
                lda     RANDOM
                and     $C7
                sta     $0A42,y
                tya
                clc
                adc     #$31
                tay
                cmp     #$93
                bcc     @7
                lda     $0A42
                ora     #$71
                sta     $0A42
                ldx     #2
                jmp     SUB93

@8:             beq     @rts2
                lda     #$FF
                sta     $8B
                ldx     #6
                jsr     SUB53
                ldy     #$75
                jsr     SUB68
@rts2:          rts

; ----------------------------------------------------------------------------

SUB77:          ldx     #1
                jsr     SUB98
                ldy     #$17

; ----------------------------------------------------------------------------

SUB76:          lda     #0
                sta     $71
                sta     $C0

; ----------------------------------------------------------------------------

SUB75:          lda     #$10
                sta     $79
                lda     #0
                sta     $C1
                sta     $73
                sta     $8A
                sta     $0C8F
                sta     $80
                cpy     #$17
                beq     @1
                sta     $E9
                sta     $EA
@1:             sta     $EB
                sta     $EC
                sta     $ED
                sta     $75
                sta     $095C
                jmp     SUB68

; ----------------------------------------------------------------------------

vbl_loop_end_2_sub_A:

                dec     $C2
                bpl     @rts
                lda     #1
                sta     $C1
                lda     #$30
                sta     $79
                lda     #3
                sta     $C2
                ldx     $C3
                lda     #$12
                sta     $69
                lda     RANDOM
                and     #3
                tay
                lda     LBB3A,y
                sta     $0A71,x
                lda     LBB3E,y
                sta     $0AA2,x
                jsr     SUB93
                txa
                tay
                lda     #5
                sta     $6E
@loop:          clc
                lda     $68
                adc     #$50
                sta     $68
                sta     $0AD3,x
                lda     $69
                adc     #0
                sta     $69
                sta     $0A40,x
                lda     #0
                sta     $0B66,x
                sta     $0B97,x
                sta     $0BC8,x
                lda     #1
                sta     $09AD,x
                lda     #$63
                sta     $0BF9,x
                sta     $0C2A,x
                jsr     SUB90
                dex
                cpx     #$11
                bcs     @skip
                ldx     #$30
@skip:          dec     $6E
                bpl     @loop
                stx     $C3
@rts:           rts

; ----------------------------------------------------------------------------

SUB73:          lda     #0
                sta     $6D
                lda     #7
                sta     $6E
                lsr     $6B
                ror     $6A
                lda     $D0
                bne     @1
                lda     $0A40,x
                lsr     a
                sta     $69
                lda     $0AD3,x
                ror     a
                sta     $68
                jmp     @2
@1:             sec
                lda     #0
                sbc     $0AD3,x
                sta     $68
                lda     #0
                sbc     $0A40,x
                lsr     a
                sta     $69
                ror     $68
@2:             asl     $6D
                sec
                lda     $6A
                sbc     $68
                tay
                lda     $6B
                sbc     $69
                bcc     @3
                sta     $6B
                sty     $6A
                inc     $6D
@3:             asl     $6A
                rol     $6B
                bcc     @4
                lda     #$FF
                rts

@4:             dec     $6E
                bpl     @2
                ldy     $6D
                lda     $0DE9,y
RTS73:          rts

; ----------------------------------------------------------------------------

SUB72:          lda     $C0
                ora     $7B
                bne     RTS73
                lda     $86
                beq     @2
                ldx     $89
                sec
                lda     $0BF9,x
                sbc     $0BFC
                bcc     @1
                lda     #0
@1:             jsr     SUB82
                sta     $0BCB
                sta     $0BCC
                sec
                lda     $0C2D
                sbc     $0C2A,x
                jsr     SUB82
                sta     $0B9A
                sec
                lda     $0C2E
                sbc     $0C2A,x
                jsr     SUB82
                sta     $0B9B
@2:             ldx     #3
@loop:          dec     $BA,x
                bpl     @inc
                txa
                lsr     a
                tay
                lda     $C8,y
                ldy     $D0
                beq     @3
                eor     #$FF
                clc
                adc     #1
@3:             clc
                adc     $B4,x
                bpl     @4
                lda     #0
@4:             cmp     #$10
                bcc     @5
                lda     #$0F
@5:             sta     $B4,x
                cmp     #8
                bcc     @6
                eor     #$0F
@6:             asl     a
                sta     $BA,x
@inc:           dex
                bpl     @loop

                lda     $0C8E
                bne     @9
                ldy     LEVEL
                lda     LBF85,y
                ldx     $0AA4
                bpl     @7
                and     #$7F
@7:             sta     $0BCA
                ora     #$80
                ldx     $0A73
                bpl     @8
                and     #$7F
@8:             sta     $0B99
@9:             lda     $76
                and     #3
                beq     @12
@10:            lda     $E6
                beq     @11
                lda     $EB
                bne     @rts1
@11:            lda     RANDOM
                cmp     #4
                bcs     @rts1
                lda     #$60
                sta     $0C8E
                ldx     #2
                jsr     SUB94
                lda     #$3C
                sta     $EB
                lda     #$88
                sta     $0B68
                lda     #0
                sta     $0C2C
                sta     $0B99
                sta     $0BCA
@rts1:          rts

@12:            lda     $A7
                eor     #1
                sta     $A7
                tax
                lda     $E9,x
                bne     @14
                lda     $E9
                ora     $EA
                and     #1
                ldy     $90
                cmp     $08C9,y
                bcs     @10
                lda     #$FF
                sta     $E9,x
                lda     RANDOM
                and     #7
                tay
                lda     LBF89,y
                sta     $0C8C,x
                lda     LEVEL
                beq     @13
                lda     LBF91,y
@13:            sta     $A8,x
                lda     #1
                sta     $AA,x
                sta     $09AD,x
                lda     RANDOM
                and     $C7
                sta     $0AA2,x
                adc     #$13
                sta     $0A71,x
                ora     #$71
                sta     $0A40,x
                jsr     SUB93
@14:            lda     $0A40,x
                cmp     #$20
                bcs     @16
                lda     $09AD,x
                beq     @15
                lda     $E4,x
                beq     @16
                cmp     #$29
                beq     @16
@15:            lda     #0
                sta     $A8,x
@16:            dec     $AA,x
                bpl     @20
                lda     #$78
                sta     $AA,x
                lda     LEVEL
                ldy     RANDOM
                cpy     #$30
                bcc     @17
                lsr     a
@17:            lsr     a
                sta     $B8,x
                lda     $A8,x
@18:            bit     RANDOM
                bpl     @19
                eor     #$0F
@19:            sta     $AC,x
                inx
                inx
                cpx     #6
                bcc     @18

                ldx     $A7
@20:            lda     $A8,x
                bne     @27
                ldy     $A7
@21:            cpy     #$31
                bcs     @23
                lda     $B8,y
                lsr     a
                lda     $0A40,y
                bcs     @22
                cmp     #$0A
                bcc     @25
                bcs     @23

@22:            cmp     #$F5
                bcs     @24
@23:            lda     $09AD,y
                lsr     a
@24:            lda     #$0F
                bcs     @26
@25:            lda     #0
@26:            sta     $AC,x
                clc
                tya
                adc     #$31
                tay
                inx
                inx
                cpx     #6
                bcc     @21
                ldx     $A7
@27:            ldy     $A7
@28:            lda     $B2,x
                cmp     $AC,x
                beq     @30
                bcs     @29
                inc     $B2,x
                bcc     @30
@29:            dec     $B2,x
@30:            stx     $6A
                tax
                lda     LBF99,x
                ldx     $6A
                sta     $0B66,y
                tya
                clc
                adc     #$31
                tay
                inx
                inx
                cpx     #6
                bcc     @28
                ldx     $A7
                lda     $0C8E
                bne     @31
                lda     $EB
                bne     @rts2
                lda     $BE
                beq     @31
                dec     $BE
@rts2:          rts

@31:            clc
                lda     $0AA2,x
                adc     #2
                cmp     #5
                bcs     @rts2
                ldy     #$D0
                lda     $09AD,x
                lsr     a
                lda     $0A40,x
                bcs     @32
                eor     #$FF
                ldy     LEVEL
                beq     @rts2
                ldy     #$50
@32:            cmp     #$20
                bcs     @rts2
                sty     $0B68
                lda     #0
                sta     $0C8E
                sta     $0C2C
                lda     #$3E
                sta     $EB
                ldx     #2
                ldy     $A7
                sty     $BF
                jmp     SUB91

; ----------------------------------------------------------------------------

SUB89:          lda     #$80
                sta     $73
                ldx     #$30
                stx     $79
@loop:          lda     RANDOM
                and     #$0F
                adc     $0C2A,y
                sbc     #$30
                sta     $0C2A,x
                lda     RANDOM
                and     #$0F
                adc     $0BF9,y
                lsr     a
                sbc     #$10
                sta     $0BF9,x
                jsr     SUB91
                lda     RANDOM
                and     #$87
                sta     $0B66,x
                lda     RANDOM
                and     #$87
                sta     $0B97,x
                lda     RANDOM
                and     #$87
                sta     $0BC8,x
                dex
                cpx     #$10
                bne     @loop
                rts

; ----------------------------------------------------------------------------

SUB91:          lda     $09AD,y
                sta     $09AD,x
                lda     $0A40,y
                sta     $0A40,x
                lda     $0AD3,y
                sta     $0AD3,x

; ----------------------------------------------------------------------------

SUB90:          lda     $09DE,y
                sta     $09DE,x
                lda     $0A71,y
                sta     $0A71,x
                lda     $0A0F,y
                sta     $0A0F,x
                lda     $0AA2,y
                sta     $0AA2,x
                lda     $0B04,y
                sta     $0B04,x
                lda     $0B35,y
                sta     $0B35,x
RTS90:          rts

; ----------------------------------------------------------------------------

SUB88:          lda     $7B
                beq     RTS90
                lda     $D0
                bne     @1
                lda     #$14
                sta     PRIOR
@1:             lda     #2
                sta     $095C
                lda     #$30
                sta     $0C8E
                lda     #$20
                sta     $0C8D
                lda     #$40
                sta     $0C8C
                lda     #$FF
                ldx     $90
                ldy     $08C9,x
                bmi     @2
                lda     #0
@2:             sta     $E9
                sta     $EA
                sta     $EB
                sta     $7B
                bmi     @3
                ldy     #2
                jsr     SUB89
                ldx     #$0A
                jmp     SUB83

@3:             lda     $0A42
                bne     @4
                lda     $0AD5
                cmp     #$20
                bcs     @4
                inc     $0AD5
@4:             lda     $0C2C
                sec
                sbc     #$78
                cmp     #$10
                bcs     @5
                lda     $0BFB
                sec
                sbc     #$68
                cmp     #$10
                bcs     @5
                lda     $0A42
                cmp     #2
                bcs     @5
                lda     $09AF
                and     $0A11
                eor     #1
                ora     $70
                ora     $0AA4
                ora     $71
                beq     @7
@5:             lda     $75
                cmp     #2
                bcc     @6
                ldy     #$1F
                jsr     SUB68
@6:             lda     #0
                sta     $75
@rts1:          rts

@7:             bit     $75
                bvs     @8
                bmi     @10
                lda     $75
                bne     @rts1
                dec     $75
                ldy     #$1C
                jmp     SUB68

@8:             ldx     #0
                stx     $65
                ldy     $D1
                bne     @rts1
                lda     #$50
                sta     $0C90
                lda     #1
                sta     $09B1
                sta     $09E2
                sta     $0A13
                sta     $0AA6
                sta     $0B9B
                lda     #$10
                sta     $0A44
                lda     #0
                sta     $0A75
                lda     #$87
                sta     $0B6A
                lda     #$81
                sta     $75
                sta     $0BCC
                sta     $ED
@rts2:          rts

@10:            lda     $09B1
                bne     @rts2
                ldx     #$0C
                jsr     SUB53
                ldy     #$21
                jsr     SUB68
                ldx     #5
@11:            lda     LBB8B,x
                sta     $0992,x
                dex
                bpl     @11
                lda     #$89
                ldx     #3
@12:            sta     $0955,x
                dex
                bpl     @12
                lda     #7
                sta     $0B6A
                lda     #$81
                sta     $0B9B
                lda     #1
                sta     $0BCC
                sta     $75
                jmp     SUB60

; ----------------------------------------------------------------------------

SUB87:          sei
                sta     $6A

@1:             lda     VCOUNT
                cmp     #$7C
                bcc     @1

@loop:          lda     LBA62,y
                iny
                bpl     @2
                lda     #$0D
@2:             sta     $0280,x
                inx
                dec     $6A
                bne     @loop
                cli
                rts

; ----------------------------------------------------------------------------

        clear_range_1000_to_1fff:

                lda     #>$1000

        clear_range_up_to_1fff:

                ; clears all memory pages from A to $1FFF
                ; As a side effect, $A3 and $7A are also cleared.

                sta     $69
                lda     #0
                tay
                sta     $68
                sta     $A3
                sta     $7A
@loop:          sta     ($68),y
                iny
                bne     @loop
                inc     $69
                ldy     $69
                cpy     #$20
                tay
                bcc     @loop
                rts

; ----------------------------------------------------------------------------

SUB84:          lda     $84
                ldy     TRIG0
                sty     $84
                bne     @rts
                sty     $66
                ldx     $C0
                bne     @rts
                ldx     $87
                cmp     #1
                beq     @1
                bcs     @3
@rts:           rts

@1:             lda     $EC,x
                cmp     #$E8
                bcs     @rts
                ldy     $095C
                sty     $89
                lda     #$0C
                ldy     $A3
                sty     $86
                beq     @2
                lda     #0
@2:             sta     $88
@3:             sty     $84
                bit     $0992
                bvs     @rts
                bmi     @4
                txa
                eor     #1
                sta     $87
@4:             txa
                sta     $09E1,x
                lda     LBF73,x
                sta     $0A74,x
                lda     #$FF
                sta     $EC,x
                sta     $0AA5,x
                lda     #0
                sta     $0C8F,x
                sta     $0A43,x
                sta     $0B07,x
                sta     $0A12,x
                sta     $0B38,x
                lda     #1
                sta     $09B0,x
                sta     $0AD6,x
                lda     $D0
                lsr     a
                ror     a
                ora     #$66
                sta     $0B69,x
                lda     #0
                sta     $0B9A,x
                sta     $0BCB,x
                ldx     #2
                jsr     SUB98
                ldx     #0

; ----------------------------------------------------------------------------

SUB83:          txa
                bne     @1
                lda     $E1
                cmp     #$18
                bcs     @rts
@1:             ldy     #7
@loop:          lda     LBF20,x
                sta     $DA,y
                inx
                dey
                bpl     @loop
                lda     LBF20,x
                sta     $D208
                lda     LBF21,x
                sta     $D204
@rts:           rts

; ----------------------------------------------------------------------------

        SUB82:  ldy     #$80
                bcs     @1
                eor     #$FF
                ldy     #0
        @1:     sty     $6A
                cmp     #8
                bcc     @2
                lda     #7
        @2:     tay
                lda     $6A
                ora     LBFC9,y
                rts

; ----------------------------------------------------------------------------

SUB81:          bit     MY_BITS
                bmi     @rts
                ldx     LEVEL
@1:             lda     RANDOM
                cmp     LBF10,x
                bcs     @rts
                and     #7
                cmp     #6
                bcs     @rts
                tax
                lda     $0992,x
                asl     a
                bmi     @1
                lda     $EB
                cmp     #$1E
                lda     #$80
                ldy     LBF14,x
                bcc     @4
                cpx     #3
                bne     @2
                bit     $0996
                bvs     @4
@2:             cpx     #4
                bne     @3
                bit     $0995
                bvs     @4
@3:             lda     #$C0
                ldy     LBF1A,x
@4:             ora     $0992,x
                sta     $0992,x
                sty     $65
                bit     $0995
                bvc     @5
                lda     #0
                sta     $7E
                jsr     clear_range_1000_to_1fff
@5:             ldy     #$52
                jsr     SUB68
                ldx     #$12
                jsr     SUB53
@rts:           rts

; ----------------------------------------------------------------------------

SUB80:          ldx     #2
@1:             dex
                bpl     @2
                rts

@2:             lda     $0C8F,x
                bne     @1
                lda     $EC,x
                beq     @1
                lda     $82,x
                and     #7
                beq     @1
                lsr     a
                cmp     #3
                bne     @3
                lsr     a
@3:             tay
                lda     $E9,y
                beq     @1
                lda     $D0
                beq     @4
                lda     #$FF
@4:             sta     $6C
                eor     $0A40,y
                cmp     #$10
                bcc     @5
                lda     #$0F
@5:             lsr     a
                sty     $6B
                tay
                lda     $6C
                eor     $0A43,x
                cmp     LBF75,y
                bcs     @1
                cmp     LBF7D,y
                bcc     @1
                ldy     $6B
                sec
                lda     #$FF
                sbc     $EC,x
                sta     $E2
                cmp     #$0F
                bcc     @6
                lda     $0C8C,y
                cmp     #$80
@6:             lda     #0
                sta     $88
                sta     $EC,x
                bcs     @9
                sta     $E9,y
                lda     $0C8C,y
                beq     @9
                cmp     #$60
                beq     @9
                lda     #0
                sta     $86
                ldx     $90
                dec     $08C9,x
                bpl     @7
                lda     #0
                sta     $08C9,x
                sec
                lda     $CB
                sbc     #3
                sta     $CB
                lda     $CC
                sbc     #0
                sta     $CC
                rts

@7:             clc
                lda     $CB
                adc     #6
                sta     $CB
                lda     $CC
                adc     #0
                sta     $CC
                ldx     #1
@8:             inc     $0950,x
                lda     $0950,x
                cmp     #$4A
                bcc     @9
                lda     #$40
                sta     $0950,x
                dex
                bpl     @8
@9:             jsr     SUB89
                ldx     #$7F
@10:            lda     $08C9,x
                bmi     @11
                bne     @rts
@11:            dex
                bpl     @10
                ldy     #$3F
                ldx     #0
                jsr     SUB57
@rts:           rts

; ----------------------------------------------------------------------------

SUB79:          lda     MY_KEY
                beq     RTS79
                ldx     #$14
                sta     $6A
                lda     #0
                sta     $66
                sta     MY_KEY
                lda     #$11
                sta     PRIOR
@loop:          lda     LBABE,x
                cmp     $6A
                beq     @1
                dex
                bpl     @loop

                ldy     #$10
                jmp     SUB68

@1:             cpx     #$0A
                bcs     skpRTS
                lda     $C0
                beq     @2
                jmp     SUB77

@2:             bit     $0993
                bvc     @3
                cpx     #6
                bcc     @3
                ldx     #5
@3:             lda     LBAD3,x
                sta     $80
                lda     LBAB4,x
                sta     $71
RTS79:          rts

skpRTS:         cpx     #$0E
                bcs     SUB61

; ----------------------------------------------------------------------------

SUB62:          lda     LBE23-11,x
                sta     $D0
                ldy     LBA82,x
                ldx     #2
                lda     #8
                jsr     SUB87
                ldx     #$10
@1:             jsr     SUB94
                dex
                cpx     #5
                bcs     @1
                bcc     SUB60

; ----------------------------------------------------------------------------

SUB61:          cpx     #17
                bcs     SUB59
                ldy     LBE23-11,x
                lda     $6E,x
                eor     LBE23-8,x
                sta     $6E,x
                beq     @1
                ldy     LBE23-5,x
@1:             jsr     SUB68
                ldx     #$0C
                jsr     SUB53

; ----------------------------------------------------------------------------

SUB60:          ldx     #$16
                ldy     $7C
                beq     @1
                inx
@1:             stx     $095A
                jsr     clear_range_1000_to_1fff
                lda     $7E
                beq     RTS79
                ldx     $D0
                beq     @2
                cpx     #1
                bne     RTS79
                ldx     #$2A
@2:             jmp     SUB52

; ----------------------------------------------------------------------------

SUB59:          cpx     #$11
                bne     @2
                lda     $C0
                bne     @rts
                lda     #$7F
                sta     $C0
                lda     #$FF
                sta     $71
                lda     #$1E
                sta     $80
                lda     #$30
                sta     $C3
                lda     #0
                sta     $C2
                sta     $0A74
                sta     $0B07
                sta     $0B38
                sta     $0B69
                lda     #1
                sta     $09B0
                sta     $09E1
                sta     $0A12
                sta     $0AA5
                lda     $8F
                sta     $C4
                lda     $8E
                sta     $C5
                lda     LEVEL
                beq     @1
                lda     $91
                rol     a
                rol     a
                rol     a
                and     #3
                tay
                lda     LBED7,y
@1:             sta     $C6
                ldy     #$11
                jmp     SUB68

@2:             cpx     #$13
                bcs     @3
                lda     $095C
                eor     #1
                and     #1
                sta     $095C
@rts:           rts

@3:             bne     @4
                lda     $D300
                cmp     #$FF
                beq     @3
                rts

@4:             ldy     #$76
                ldx     #4

; ----------------------------------------------------------------------------

SUB58:          lda     #0
                sta     $EC
                sta     $D6
                sta     $D1
                sta     $8B
                sta     $D207
                sta     $71
                sta     $81
                sta     $7D
                sta     $C0
                sta     $C1

; ----------------------------------------------------------------------------

SUB57:          lda     #$FF
                sta     MY_BITS
                sty     $65
                txa
                ora     LEVEL
                tax
                lda     LBEDD,x
                clc
                adc     $CB
                tax
                lda     #0
                sta     $C9
                sta     $C8
                adc     $CC
                bmi     @rts
                lsr     a
                txa
                ror     a
                lsr     a
                lsr     a
                lsr     a
                cmp     #$13
                bcc     @1
                lda     #$12
                ldx     #$0F
@1:             sta     $CD
                tay
                txa
                cpy     #0
                beq     @3
                cpy     #$0B
                bcc     @2
                cpy     #$0F
                bcc     @3
@2:             lsr     a
                eor     #8
@3:             and     #$0F
                sta     $CE
@rts:           rts

; ----------------------------------------------------------------------------

SUB56:          lda     $C0
                bne     @rts
                lda     $D0
                bmi     @1
@rts:           rts

@1:             bit     $0997
                bmi     @2
                jsr     SUB70
@2:             lda     $72
                and     #1
                bne     SUB55
                clc
                lda     $8F
                adc     $C8
                and     #$7F
                sta     $8F
                clc
                adc     #$3D
                sta     $0C2E
                clc
                lda     $8E
                adc     $C9
                and     #$7F
                sta     $8E
                clc
                adc     #$3F
                sta     $0BFD
                lda     $8C
                clc
                adc     #$3F
                sta     $0BFC
                lda     $8D
                clc
                adc     #$3D
                sta     $0C2D

; ----------------------------------------------------------------------------

SUB55:          lda     $8F
                lsr     a
                lsr     a
                lsr     a
                sta     $6A
                lda     $8E
                and     #$70
                ora     $6A
                sta     $92
                tax
                lda     $08C9,x
                bpl     @1
                lda     #0
@1:             ora     #$90
                bit     $0997
                bvs     @2
                sta     $098D
@2:             sec
                lda     $8F
                sbc     $8D
                bcs     @3
                eor     #$FF
                adc     #1
@3:             sta     $6A
                sec
                lda     $8E
                sbc     $8C
                bcs     @4
                eor     #$FF
                adc     #1
@4:             lsr     a
                clc
                adc     $6A
                tay
                lsr     a
                lsr     a
                lsr     a
                tax
                tya
                and     #3
                clc
                adc     LBADD,x
                sta     $91
                tay
                lda     #$10
                sta     $097D
                sta     $097E
                sta     $097F
@5:             ldx     #2
@6:             inc     $097D,x
                lda     $097D,x
                cmp     #$1A
                bcc     @7
                lda     #$10
                sta     $097D,x
                dex
                bpl     @6
@7:             dey
                bne     @5
                rts

; ----------------------------------------------------------------------------

vbl_loop_end_3: lda     $D1
                beq     @1
                dec     $CF
                beq     show_status
@rts:           rts

@1:             ldy     $65
                beq     @rts

; ----------------------------------------------------------------------------

SUB68:          sty     $D1
                ldy     #$23
                ldx     #$0F
                lda     #7
                jsr     SUB87

; ----------------------------------------------------------------------------

show_status:    ; clear status line and set $6B to zero.

                ldx     #19
                lda     #0
                sta     $6B
@1:             sta     STATUS_LINE,x
                dex
                bpl     @1

@2:             ldx     $D1
                inc     $D1
                bne     @3
                ldx     #$0F
                ldy     #$80
                lda     #7
                jmp     SUB87

@3:             lda     LBBAA,x
                cmp     #$FC
                bne     @4
                ldy     $CE
                lda     LBEFC,y
                ldx     $6B
                sta     STATUS_LINE,x
                lda     #$3C
                sta     $CF
                rts

@4:             cmp     #$FD
                bne     @5
                ldy     $CD
                lda     LBEE9,y
@5:             sta     $6C
                and     #$3F
                sta     $6A

                ; point to string table.

                lda     #<(strings-1)
                sta     $68
                lda     #>(strings-1)
                sta     $69

                ; find the correct string
                ; at this point, $6A contains the string index

@loop:          inc     $68     ; increment ($68)
                bne     @7
                inc     $69
@7:             ldy     #0
                lda     ($68),y
                bpl     @loop
                dec     $6A
                bne     @loop

                ; copy the string contents

@8:             and     #$3F
                eor     #$A0
                ldx     $6B
                inc     $6B
                sta     STATUS_LINE,x
                iny
                lda     ($68),y
                bpl     @8

                inc     $6B
                lda     #$3C
                bit     $6C
                bpl     @9
                bvc     @10
                lda     #$FE
@9:             bvc     @2
                ldy     #$FF
                sty     $D1
@10:            sta     $CF
                rts

; ----------------------------------------------------------------------------

SUB54:          lda     $D6
                beq     @3
                dec     $D8
                bpl     @3
                lda     $D9
                beq     @1
                lda     $D5
                bmi     @1
                sta     $D8
                ldy     #0
                beq     @2
@1:             lda     $D4
                sta     $D8
                ldx     $D2
                inc     $D2
                lda     LBF5C,x
                sta     $D206
                ldy     #$A8
                cmp     #$FF
                bne     @2
                lda     $D7
                sta     $D2
                dec     $D3
                bpl     @1
                ldy     #0
                sty     $D6
@2:             sty     $D207
                sty     $D9
@3:             lda     $E2
                beq     @4
                dec     $E2
                bne     @4
                ldx     #$14
                jsr     SUB83
@4:             ldx     $70
                txa
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                cmp     $E1
                bcc     @5
                lda     #0
                sta     $E1
                inx
                txa
                eor     #$FF
                sta     $D204
                tax
                asl     a
                asl     a
                asl     a
                asl     a
                asl     a
                sta     $D200
                txa
                lsr     a
                lsr     a
                lsr     a
                sta     $D202
                lsr     a
                eor     #$8F
                sta     $D203
                and     #$87
                sta     $D205
                lda     #$70
                sta     $D208
                rts

@5:             lda     $DB
                beq     @6
                dec     $DB
                bne     @6
                lda     #$8F
                sta     $DC
@6:             ldx     $DA
                beq     @8
                dec     $DA
                bne     @7
                lda     #$AF
                sta     $DC
                lda     #2
                sta     $DE
                sta     $DF
@7:             lda     LBFEA,x
                sta     $DD
                lda     LBFF2,x
                sta     $D204
                sta     $D209
@8:             lda     $E3
                beq     @9
                dec     $E3
                lda     RANDOM
                sta     $D204
                and     #$20
                eor     $DD
                sta     $DD
@9:             clc
                lda     $DE
                adc     $E0
                sta     $DE
                sta     $D200
                lda     $DF
                adc     #0
                sta     $DF
                sta     $D202
                ldx     $DC
                ldy     $DD
                lda     $72
                lsr     a
                bcc     @11
                lda     $E1
                beq     @11
                dec     $E1
                cmp     #$11
                bcs     @11
                txa
                and     #$0F
                beq     @10
                dex
                stx     $DC
@10:            tya
                and     #$0F
                beq     @11
                dey
                sty     $DD
@11:            stx     $D203
                sty     $D205
                rts

; ----------------------------------------------------------------------------

SUB53:          lda     LBF3E,x
                cmp     $D6
                bcc     @rts
                ldy     #5
@1:             lda     LBF3E,x
                sta     $D2,y
                inx
                dey
                bpl     @1
@rts:           rts

; ----------------------------------------------------------------------------

SUB71:          ; setup display list

                ldx     #89
@1:             lda     #$0D
                sta     MY_DLIST+5,x
                cpx     #10
                bcs     @2
                lda     LBFA9,x
                sta     $F2,x
@2:             dex
                bpl     @1
                lda     #$70
                sta     MY_DLIST+0
                sta     MY_DLIST+1
                lda     #$41
                sta     MY_DLIST+103
                lda     #<MY_DLIST
                sta     MY_DLIST+104
                lda     #>MY_DLIST
                sta     MY_DLIST+105

                ldx     #0
                stx     $68
                stx     $69
                stx     $6A
                stx     $6B
@3:             clc
                lda     $68
                adc     #$51
                sta     $68
                lda     $69
                sta     $0DE9,x
                adc     #0
                sta     $69
                clc
                lda     $6A
                adc     #$64
                sta     $6A
                lda     $6B
                sta     $0EE9,x
                sed
                adc     #0
                cld
                sta     $6B
                inx
                bne     @3

                ldx     #0
                stx     $68
                lda     #$10
                sta     $69
@4:             clc
                lda     $68
                sta     $0800,x
                adc     #$28
                sta     $68
                lda     $69
                sta     $0864,x
                adc     #0
                sta     $69
                lda     LBB42,x
                sta     $0949,x
                inx
                cpx     #$64
                bcc     @4

                dex
                stx     $78
                ldx     #3
                stx     $0911
@5:             lda     LBBA6,x
                sta     $6A
                ldy     LEVEL
                iny
                iny
                sty     $6B
@6:             lda     RANDOM
                and     #$7F
                tay
                lda     $08C9,y
                bne     @6
                lda     $6A
                bpl     @7
                cpy     #$10
                bcc     @6
                cpy     #$70
                bcs     @6
                tya
                and     #$0F
                beq     @6
                cmp     #$0F
                beq     @6
                lda     $08C8,y
                ora     $08CA,y
                ora     $08D9,y
                ora     $08B9,y
                bne     @6
                lda     $6A
@7:             sta     $08C9,y
                dec     $6B
                bpl     @6
                dex
                bpl     @5

                ; loop

                ldx     #$B4
@8:             lda     #$0A
                sta     $0D34,x
                dex
                bne     @8

                ; loop

                ldx     #$0F
@9:             lda     #$18
                sta     $0D37,x
                dex
                bpl     @9

                lda     #$1A
                sta     $0D47
                lda     #0
                sta     $0911
                lda     #$48
                sta     $90
                lda     #$43
                sta     $8D
                sta     $8F
                lda     #$47
                sta     $8E
                sta     $8C
                lda     #$EA
                sta     $0FE8

; ----------------------------------------------------------------------------

; this is only called from within SUB56 (1 reference), and
; accessed through fall-through from SUB71

SUB70:          ldy     #0
                sty     $6A
@1:             ldx     $6A
                lda     $08C9,x
                bpl     @2
                lda     #5
@2:             tax
                lda     LBED1,x
                sta     $0D4B,y
                iny
                inc     $6A
                lda     $6A
                and     #$0F
                bne     @1
                lda     #$19
                sta     $0D4B,y
                iny
                iny
                iny
                iny
                cpy     #$A0
                bcc     @1
                rts

; ----------------------------------------------------------------------------

vbl_loop_end_4: inc     $76
                ldx     #$90
                lda     $76
                bpl     @1
                ldy     $0955
                cpy     #$80
                bne     @1
                ldx     #$44
@1:             and     #3
                sta     $72
                bne     @5
                ldy     $7D
                beq     @4
                ldy     #$A0
                bit     $0994
                bpl     @3
                bvs     @2
                lda     RANDOM
                cmp     #$C8
                bcc     @4
@2:             ldy     #0
@3:             tya
                bne     @4
                ldx     #$26
@4:             sty     $81
                stx     $FB
@5:             ldx     #2
@6:             lda     $0C8E,x
                bne     @7
                lda     $EB,x
                beq     @7
                dec     $EB,x
@7:             dex
                bpl     @6
                lda     $73
                beq     @10
                dec     $73
                bne     @8
                ldx     #$11
                stx     $79
@8:             cmp     #$70
                bcs     @9
                ldx     #0
                stx     $8A
@9:             cmp     #$18
                bcs     @10
                dec     $79
@10:            dec     $74
                bpl     @rts1
                lda     #$28
                sta     $74
                ldx     #4
@11:            inc     $09A3,x
                lda     $09A3,x
                cmp     #$DA
                bcc     @13
                lda     #$D0
                sta     $09A3,x
                cpx     #3
                bne     @12
                dex
@12:            dex
                bpl     @11
@13:            dec     $78
                bmi     @14
@rts1:          rts

@14:            lda     #$31
                sta     $78
                lda     $CB
                bne     @15
                dec     $CC
@15:            dec     $CB
                ldx     MY_BITS
                bne     @rts1
                stx     $6A
@16:            lda     $08C9,x
                bpl     @17
                jsr     SUB95
                beq     @17
                lda     #2
                sta     $08C9,x
                sta     $6A
                sec
                lda     $CB
                sbc     #$12
                sta     $CB
                lda     $CC
                sbc     #0
                sta     $CC
@17:            inx
                bpl     @16
                lda     $6A
                beq     @18
                bit     $0997
                bvs     @18
                ldy     #$15
                jsr     SUB68
                ldx     #$18
                jsr     SUB53
@18:            dec     $9F
                bmi     @19
                ldx     $93
                lda     $08C9,x
                bmi     @22
@19:            lda     #7
                sta     $9F
                ldy     #$7F
@20:            lda     RANDOM
                and     #$7F
                tax
                lda     $08C9,x
                bmi     @22
                dey
                bpl     @20
                ldx     #$7F
@21:            lda     $08C9,x
                bmi     @22
                dex
                bpl     @21
                rts

@22:            stx     $93
                txa
                and     #$0F
                sta     $94
                txa
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                sta     $95
                ldx     #$FF
@23:            inx
                bpl     @27
                ldx     #0
@24:            lda     $08C9,x
                and     #$DF
                sta     $08C9,x
                inx
                bpl     @24
                bit     $0997
                bvs     @rts2
                ldx     #0
@25:            lda     $08C9,x
                bpl     @26
                jsr     SUB95
                beq     @26
                lda     #$63
                sta     $78
                ldy     #$13
                jsr     SUB68
                ldx     #$18
                jmp     SUB53

@26:            inx
                bpl     @25
@rts2:          rts

@27:            ldy     $08C9,x
                cpy     #$0A
                bcs     @23
                lda     RANDOM
                cmp     LBFBB,y
                bcs     @23
                cpx     $90
                beq     @23
                ldy     #8
@28:            clc
                txa
                adc     LBFC0,y
                sta     $6A
                and     #$0F
                sec
                sbc     $94
                bcs     @29
                eor     #$FF
                adc     #1
@29:            sta     $6B
                lda     $6A
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                sec
                sbc     $95
                bcs     @30
                eor     #$FF
                adc     #1
@30:            clc
                adc     $6B
                sta     $96,y
                dey
                bpl     @28
                lda     #1
                sta     $6B
@31:            ldy     #7
@32:            lda     $96,y
                cmp     $9E
                bcs     @34
                clc
                txa
                adc     LBFC0,y
                bmi     @34
                sty     $6A
                tay
                lda     $08C9,y
                bne     @33
                lda     $08C9,x
                cpy     $90
                beq     @33
                ora     #$20
                sta     $08C9,y
                lda     #0
                sta     $08C9,x
                beq     @35
@33:            ldy     $6A
@34:            dey
                bpl     @32
                inc     $9E
                dec     $6B
                bpl     @31
@35:            jmp     @23

; ----------------------------------------------------------------------------

; this is only called from within the main vblank-synced loop (4 references)
; depends on: NONE

SUB65:          lda     $09AD,x
                eor     #1
                beq     @1
                lda     #$FF
@1:             sta     $6B
                sta     $6C
                lda     $0A40,x
                sta     $6A
                lda     RANDOM
                ora     #$BF
                eor     $0AD3,x
                asl     a
                rol     $6A
                rol     $6B
                asl     a
                rol     $6A
                rol     $6B
                lda     $6D
                eor     #$FF
                sta     $6D
                bmi     @2
                clc
                lda     $0AD3,y
                adc     $6A
                sta     $0AD3,y
                lda     $0A40,y
                adc     $6B
                sta     $0A40,y
                lda     $09AD,y
                adc     $6C
                sta     $09AD,y
                rts

@2:             sec
                lda     $0AD3,y
                sbc     $6A
                sta     $0AD3,y
                lda     $0A40,y
                sbc     $6B
                sta     $0A40,y
                lda     $09AD,y
                sbc     $6C
                sta     $09AD,y
                rts

; ----------------------------------------------------------------------------

; this is only called from within the main vblank-synced loop (2 references)

SUB64:          cmp     #$50
                bcs     SUB47
                sta     $6D
                lda     #$50
                cpx     #5
                bcs     @1
                lda     #$7D
@1:             ldy     $09DE,x
                bne     @2
                sec
                inc     $6D
                sbc     $6D
                sta     $0C2A,x
                rts

@2:             clc
                adc     $6D
                sta     $0C2A,x
                rts

; ----------------------------------------------------------------------------

; this is only called from within the main vblank-synced loop (2 references)

SUB92:          cmp     #$32
                bcs     SUB47
                sta     $6D
                lda     #$32
                cpx     #5
                bcs     @1
                asl     $6D
                lda     #$7A
@1:             bit     $D0
                bvc     @3
                bit     $0996
                bpl     @2
                bit     RANDOM
                bvc     @4
                bvs     @5
@2:             ldy     $09AD,x
                bne     @4
                beq     @5
@3:             ldy     $0A0F,x
                beq     @5
@4:             sec
                inc     $6D
                sbc     $6D
                sta     $0BF9,x
                rts

@5:             clc
                adc     $6D
                sta     $0BF9,x
                rts

; ----------------------------------------------------------------------------

; this is only called from within SUB64 and SUB92 (2 references)

SUB47:          cpx     #5
                bcs     SUB94
                lda     #$FB
                sta     $0BF9,x
RTS47:          rts

; ----------------------------------------------------------------------------

SUB94:          lda     #$63
                sta     $0BF9,x
                sta     $0C2A,x
                cpx     #$11
                bcs     RTS47
                lda     RANDOM
                and     #$0F
                sta     $6A
                sta     $0AA2,x
                lda     RANDOM
                and     #$0F
                cmp     $6A
                bcc     @1
                sta     $6A
@1:             sta     $0A71,x
                lda     #$0F
                sta     $0A40,x
                lda     $D0
                eor     #1
                and     #1
                sta     $09AD,x
                bne     @2
                sta     $0B04,x
                sta     $0B35,x
                sec
                sbc     $6A
                sta     $0A40,x
                lda     #$80
                sta     $0AD3,x
@2:             bit     $D0
                bvc     SUB93
                lda     RANDOM
                sta     $0A71,x
                lda     RANDOM
                sta     $0A40,x
                and     #1
                sta     $09AD,x

; ----------------------------------------------------------------------------

; depends on: NONE

SUB93:          lda     RANDOM
                and     #1
                sta     $0A0F,x
                bne     @1
                sec
                sbc     $0B35,x
                sta     $0B35,x
                lda     #0
                sbc     $0AA2,x
                sta     $0AA2,x
@1:             lda     RANDOM
                and     #1
                sta     $09DE,x
                bne     @rts
                sec
                sbc     $0B04,x
                sta     $0B04,x
                lda     #0
                sbc     $0A71,x
                sta     $0A71,x
@rts:           rts

; ----------------------------------------------------------------------------

; This is only called from within vbl_loop_end_4 (2 references)

SUB95:          lda     $08C8,x
                beq     @rts
                lda     $08CA,x
                beq     @rts
                lda     $08B9,x
                beq     @rts
                lda     $08D9,x
@rts:           rts

; ----------------------------------------------------------------------------

vbl_loop_end_1: ldx     $70
                cpx     $71
                beq     @2
                bcc     @1
                dec     $70
                bcs     @3
@1:             inc     $70
@2:             lda     $C0
                bne     @3
                bit     $0993
                bpl     @3
                lda     $71
                and     RANDOM
                sta     $70
@3:             ldy     #1
                jsr     vbl_loop_end_1_sub_B
                bit     $0995
                bmi     @4
                lda     #$31
                ldy     #$17
                jsr     vbl_loop_end_1_sub_A
                lda     #$62
                ldy     #$1D
                jsr     vbl_loop_end_1_sub_A
                lda     #0
                ldy     #$23
                jsr     vbl_loop_end_1_sub_A
                lda     $096E
                sta     $096F
                cmp     #$0A
                bcs     @4
                ldx     $095C
                lda     $0AD3,x
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                tax
                lda     $0EE9,x
                sta     $096F
@4:             clc
                lda     $7F
                adc     $7D
                adc     $80
                adc     $7E
                adc     #1
                cmp     $7F
                sta     $7F
                bcs     RTS98
                ldx     #3

                ; fallthrough to SUB98

; ----------------------------------------------------------------------------

; depends on: SUB58, SUB62
; fallthrough from vbl_loop_end_1

SUB98:          bit     MY_BITS
                bvs     RTS98
                dec     $0955,x
                lda     $0955,x
                cmp     #$80
                bcs     RTS98
                lda     #$89
                sta     $0955,x
                cpx     #2
                bne     @2
                lda     $CB
                bne     @1
                dec     $CC
@1:             dec     $CB
@2:             dex
                bpl     SUB98
                ldx     #$0A
                txa
                ldy     #3
@3:             sta     $0955,y
                dey
                bpl     @3
                jsr     SUB62
                ldy     #$31
                ldx     #4
                jsr     SUB58
RTS98:          rts

; ----------------------------------------------------------------------------

; This is only referenced from within vbl_loop_end_1 (3 references)
; input parameters: A, Y

vbl_loop_end_1_sub_A:

                clc
                adc     $095C
                tax
                lda     #$10
                sta     $6A
                lda     $09AD,x
                lsr     a
                lda     $0A40,x
                bcs     @1
                eor     #$FF
                dec     $6A
@1:             tax
                lda     $6A
                sta     $0949,y
                tya
                and     #$10
                beq     vbl_loop_end_1_sub_B
                cpx     #$FF
                bne     vbl_loop_end_1_sub_B
                dex

; referenced once directly, from within vbl_loop_end_1

vbl_loop_end_1_sub_B:

                lda     $0EE9,x
                tax
                and     #$0F
                sta     $094B,y
                txa
                lsr     a
                lsr     a
                lsr     a
                lsr     a
                sta     $094A,y
                rts

; ----------------------------------------------------------------------------

data2:          .byte   $00,$01,$02,$03,$07
LB8E4:          .byte   $00,$18,$3C,$7E,$7E,$76,$F7,$DF
                .byte   $DF,$FF,$FF,$F7,$76,$7E,$7E,$3C
                .byte   $18,$10,$38,$7C,$7C,$FE,$DE,$DA
                .byte   $FA,$EE,$EE,$7C,$7C,$38,$10,$18
                .byte   $3C,$3C,$7E,$6E,$7A,$7E,$76,$7E
                .byte   $3C,$3C,$18,$10,$38,$38,$7C,$74
                .byte   $7C,$6C,$38,$38,$10,$10,$18,$3C
                .byte   $2C,$3C,$3C,$18,$08,$10,$38,$38
                .byte   $28,$38,$10,$3C,$3C,$24,$3C,$7E
                .byte   $7E,$7E,$5A,$FF,$FF,$42,$42,$42
                .byte   $42,$42,$42,$1C,$1C,$14,$3E,$3E
                .byte   $3E,$2A,$7F,$7F,$22,$22,$22,$22
                .byte   $22,$18,$18,$3C,$3C,$3C,$3C,$7E
                .byte   $24,$24,$24,$24,$10,$10,$38,$38
                .byte   $38,$7C,$28,$28,$28,$18,$18,$3C
                .byte   $18,$18,$10,$10,$38,$10,$18,$7E
                .byte   $FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF
                .byte   $FF,$FF,$FF,$FF,$7E,$7E,$00,$18
                .byte   $3C,$7E,$FF,$FF,$FF,$E7,$66,$FF
                .byte   $FF,$FF,$FF,$7E,$7E,$00,$18,$3C
                .byte   $7E,$FF,$FF,$E7,$66,$FF,$FF,$FF
                .byte   $FF,$3C,$18,$3C,$FF,$FF,$E7,$66
                .byte   $FF,$FF,$7E,$3C,$00,$18,$3C,$FF
                .byte   $FF,$FF,$3C,$18,$18,$3C,$FF,$3C
                .byte   $18,$28,$28,$28,$28,$EE,$00,$00
                .byte   $EE,$28,$28,$28,$28
LB9B1:          .byte   $00,$81,$81,$81,$81,$BD,$FF,$FF
                .byte   $BD,$81,$81,$81,$81,$82,$82,$BA
                .byte   $FE,$FE,$BA,$82,$82,$42,$5A,$7E
                .byte   $7E,$5A,$42,$44,$54,$7C,$7C,$54
                .byte   $44,$24,$3C,$3C,$24,$28,$38,$38
                .byte   $28,$18,$18,$10,$10,$E0,$F8,$F8
                .byte   $FE,$57,$FE,$F8,$F8,$C0,$C0,$F0
                .byte   $C0,$F0,$F0,$FC,$BE,$FC,$F0,$80
                .byte   $80,$C0,$C0,$F0,$BC,$F0,$C0,$07
                .byte   $1F,$1F,$7F,$EA,$7F,$1F,$1F,$03
                .byte   $03,$0F,$03,$0F,$0F,$3F,$7D,$3F
                .byte   $0F,$01,$01,$03,$03,$0F,$3D,$0F
                .byte   $03,$18,$3C,$7E,$7E,$DB,$C3,$81
                .byte   $81,$81,$10,$38,$7C,$7C,$D6,$C6
                .byte   $82,$82,$18,$3C,$3C,$66,$66,$42
                .byte   $42,$10,$38,$38,$6C,$44,$44,$18
                .byte   $3C,$24,$24,$10,$38,$28,$18,$3C
                .byte   $7E,$FF,$18,$18,$FF,$7E,$3C,$18
                .byte   $10,$38,$7C,$FE,$38,$38,$FE,$7C
                .byte   $38,$10,$18,$3C,$7E,$18,$7E,$3C
                .byte   $18,$10,$38,$7C,$10,$7C,$38,$10
                .byte   $18,$3C,$18,$3C,$18,$10,$38,$38
                .byte   $10
LBA62:          .byte   $8D,$00,$46,$49,$09,$20,$06,$00
                .byte   $01,$2E,$A1,$00,$00,$46,$F8,$A0
                .byte   $4D,$C8,$10,$00,$00,$46,$09,$A1
                .byte   $4D,$C8,$10,$4D,$00,$10,$0D,$0D
LBA82:          .byte   $0D,$0D,$0D,$30,$46,$1F,$0D,$4D
                .byte   $A8,$12,$1B,$13,$0B,$08
LBA90:          .byte   $FF,$FF,$FF,$FF,$AA,$FF,$AA,$FF
                .byte   $AA,$AA,$AA,$FF,$AA,$AA,$AA,$AA
                .byte   $AA,$AA,$AA,$55,$55,$AA,$55,$AA
                .byte   $55,$55,$55,$AA,$55,$55,$55,$55
LBAB0:          .byte   $C0,$30,$0C,$03
LBAB4:          .byte   $00,$01,$02,$04,$08,$10,$20,$40
                .byte   $60,$70
LBABE:          .byte   $F2,$DF,$DE,$DA,$D8,$DD,$DB,$F3
                .byte   $F5,$F0,$F8,$FF,$C0,$FD,$ED,$FE
                .byte   $D2,$F9,$E5,$CA,$E7
LBAD3:          .byte   $00,$04,$06,$08,$0A,$0C,$0E,$1E
                .byte   $2D,$3C
LBADD:          .byte   $0A,$0D,$10,$14,$17,$32,$46,$50
                .byte   $5A,$78,$7D,$82,$87,$8C,$9B,$AA
                .byte   $B8,$C8,$D0,$D8,$DF,$E8,$F1,$FA
LBAF5:          .byte   $00,$01,$FF,$00
LBAF9:          .byte   $50,$28,$87,$50,$36,$87,$77,$46
                .byte   $1E,$77,$56,$1E,$77,$46,$91,$94
                .byte   $46,$91,$78,$4E,$06,$7E,$4B,$0F
                .byte   $7E,$51,$0F,$8D,$4E,$07,$85,$47
                .byte   $84,$7E,$4C,$85,$8C,$4C,$85,$85
                .byte   $52,$84,$3E,$32,$0F,$54,$32,$0F
                .byte   $FE,$4E,$35,$82,$4F,$34,$82,$50
                .byte   $32,$85,$51,$34,$82,$52,$35,$82
                .byte   $FE
LBB3A:          .byte   $04,$04,$03,$02
LBB3E:          .byte   $02,$03,$04,$04
LBB42:          .byte   $12,$0B,$00,$00,$0A,$55,$4B,$40
                .byte   $40,$0A,$8D,$8B,$89,$89,$89,$89
                .byte   $0A,$16,$0B,$00,$0A,$14,$0B,$0F
                .byte   $00,$00,$0A,$51,$4B,$0F,$00,$00
                .byte   $0A,$93,$8B,$0F,$00,$00,$00,$0A
                .byte   $37,$21,$32,$30,$00,$25,$2E,$25
                .byte   $32,$27,$39,$1A,$00,$00,$00,$10
                .byte   $00,$00,$00,$00,$B4,$A1,$B2,$A7
                .byte   $A5,$B4,$B3,$9A,$00,$00,$24,$23
                .byte   $1A
LBB8B:          .byte   $30,$25,$33,$23,$2C,$32,$00,$F3
                .byte   $F4,$E1,$F2,$00,$E4,$E1,$F4,$E5
                .byte   $DA,$D0,$D0,$CE,$D0,$D0,$00,$00
                .byte   $00,$00,$00
LBBA6:          .byte   $CF,$04,$03,$02

LBBAA:          .byte   $00,$05,$06,$42,$05,$06,$43,$04
                .byte   $42,$04,$43,$06,$07,$42,$07,$43
                .byte   $48,$09,$4A,$0B,$CD,$0B,$CC,$09
                .byte   $4E,$09,$4F,$D0,$11,$92,$56,$13
                .byte   $4E,$15,$4F,$B8,$97,$99,$98,$8C
                .byte   $9D,$1E,$9F,$FD,$25,$FC,$78,$9B
                .byte   $60,$B8,$97,$98,$1A,$8E,$1C,$94
                .byte   $24,$9F,$FD,$25,$FC,$A7,$68,$B8
                .byte   $97,$98,$1A,$8F,$24,$9F,$FD,$25
                .byte   $FC,$66,$2C,$5A,$2E,$5A,$31,$5A
                .byte   $33,$5A,$B8,$34,$76,$37,$B5,$78
                .byte   $37,$8C,$78,$23,$B5,$78,$23,$8C
                .byte   $78,$04,$B5,$78,$04,$8C,$78,$06
                .byte   $B5,$78,$06,$8C,$78,$A2,$75,$A2
                .byte   $4C,$A1,$75,$A1,$4C,$C1,$B8,$97
                .byte   $98,$1A,$8E,$24,$9F,$FD,$25,$FC
                .byte   $66

strings:        message "     RED ALERT"
                message "ON"
                message "OFF"
                message "SHIELDS"
                message "ATTACK"
                message "COMPUTER"
                message "TRACKING"
                message "WHATS WRONG?"
                message "HYPERWARP"
                message "ENGAGED"
                message "STARBASE"
                message "DESTROYED"
                message "SURROUNDED"
                message "ABORTED"
                message "COMPLETE"
                message "HYPERSPACE"
                message "ORBIT"
                message "ESTABLISHED"
                message "DOCKING"
                message "ENERGY"
                message "TRANSFER"
                message "STANDBY"
                message "STAR FLEET TO"
                message "STAR CRUISER 7"
                message "ALL UNITS"
                message "MISSION"
                message "    STAR RAIDERS"
                message "ZERO"
                message "BY ZYLON FIRE"
                message "POSTHUMOUS"
                message "RANK IS:"
                message "COPYRIGHT ATARI 1979"
                message "SUB-SPACE RADIO"
                message "SECTOR SCAN"
                message "ENGINES"
                message "NEW"
                message "CLASS"
                message "CONGRATULATIONS"
                message "REPORT TO BASE"
                message "FOR TRAINING"
                message "GALACTIC COOK"
                message "GARBAGE SCOW CAPTAIN"
                message "ROOKIE"
                message "NOVICE"
                message "ENSIGN"
                message "PILOT"
                message "ACE"
                message "LIEUTENANT"
                message "WARRIOR"
                message "CAPTAIN"
                message "COMMANDER"
                message "DAMAGE"
                message "DAMAGED"
                message "CONTROL"
                message "PHOTONS"
                message " "
                message "STAR COMMANDER"

                .byte   $80,$00

LBE23:          .byte   $01,$40,$80
                .byte   $0E,$09,$04
                .byte   $FF,$08,$02
                .byte   $0B,$07,$01

LBE2F:          .byte   $01,$11,$1F,$2B,$35,$3D,$75,$7A
                .byte   $01,$0D,$15,$1B,$21,$25,$29,$2B
                .byte   $2D,$38,$41,$36,$36,$00,$00,$00
                .byte   $7E,$8E,$9D,$AA,$B4,$BC,$7B,$7A
                .byte   $47,$52,$5B,$50,$50,$00,$00,$00
                .byte   $43,$53,$61,$6C,$75,$7A,$75,$7A
                .byte   $01,$11,$1F,$2B,$35,$3D,$75,$7A
                .byte   $61,$6A,$72,$79,$7F,$83,$29,$2B
                .byte   $86,$90,$9A,$A1,$A8,$AD,$29,$2B
                .byte   $C1,$C1,$C1,$C1,$C1,$C1,$75,$C1
LBE7F:          .byte   $0F,$0D,$0B,$09,$07,$05,$01,$01
                .byte   $0B,$07,$05,$05,$03,$03,$01,$01
                .byte   $09,$08,$05,$02,$00,$00,$00,$00
                .byte   $0F,$0E,$0C,$09,$07,$04,$02,$01
                .byte   $09,$08,$05,$02,$00,$00,$00,$00
                .byte   $0F,$0D,$0A,$08,$04,$03,$01,$01
                .byte   $0F,$0D,$0B,$09,$07,$05,$01,$01
                .byte   $08,$07,$06,$05,$03,$02,$01,$01
                .byte   $09,$09,$06,$06,$04,$03,$01,$01
                .byte   $0B,$0B,$0B,$0B,$0B,$0B,$01,$0B
LBECF:          .byte   $F8,$FF
LBED1:          .byte   $0C,$1E,$1E,$1D,$1C,$1B
LBED7:          .byte   $9F,$BF,$DF,$FF
LBEDB:          .byte   $F8,$08
LBEDD:          .byte   $50,$4C,$3C,$6F,$3C,$3C,$32,$64
                .byte   $28,$32,$28,$5A
LBEE9:          .byte   $A9,$AA,$AA,$AB,$AB,$AC,$AC,$AD
                .byte   $AD,$AE,$AE,$AF,$B0,$B1,$B2,$B3
                .byte   $B3,$B9,$B9
LBEFC:          .byte   $95,$95,$95,$94,$94,$94,$94,$93
                .byte   $93,$93,$92,$92,$92,$91,$91,$91

LBF0C:          .byte   $4A,$4C,$4E,$50 ; indexed by level (0--3)
LBF10:          .byte   $00,$50,$B4,$FE ; indexed by level (0--3)

LBF14:          .byte   $55,$5B,$61,$67,$6D,$71
LBF1A:          .byte   $58,$5E,$64,$6A,$6F,$73
LBF20:          .byte   $18
LBF21:          .byte   $FF,$02,$00,$8A,$A0,$00,$08,$50
                .byte   $00,$40,$40,$01,$03,$88,$AF,$08
                .byte   $00,$50,$04,$30,$40,$01,$03,$84
                .byte   $A8,$04,$00,$50,$04
LBF3E:          .byte   $02,$02,$02,$03,$0C,$02,$04,$03
                .byte   $FF,$10,$07,$04,$07,$04,$02,$02
                .byte   $00,$07,$0B,$05,$FF,$20,$02,$0B
                .byte   $0E,$06,$08,$20,$00,$0E
LBF5C:          .byte   $10,$FF,$18,$FF,$40,$60,$FF,$10
                .byte   $10,$10,$FF,$40,$20,$FF,$48,$40
                .byte   $51,$FF
LBF6E:          .byte   $84,$B4,$FC,$B4,$84
LBF73:          .byte   $FF,$01
LBF75:          .byte   $0C,$0C,$0C,$0C,$0E,$0E,$0E,$20
LBF7D:          .byte   $00,$00,$00,$02,$04,$06,$08,$0C

LBF85:          .byte   $81,$84,$88,$94 ; indexed by level (0--3)

LBF89:          .byte   $80,$10,$10,$10,$70,$70,$70,$10
LBF91:          .byte   $04,$04,$00,$00,$00,$01,$00,$00
LBF99:          .byte   $3E,$1E,$10,$08,$04,$02,$01,$00
                .byte   $00,$81,$82,$84,$88,$90,$9E,$BE
LBFA9:          .byte   $A6,$AA,$AF,$00,$00,$B8,$5A,$FC
                .byte   $5E,$90
LBFB3:          .byte   $FF,$FF,$3F,$0F,$3F,$7F,$FF,$FF
LBFBB:          .byte   $00,$FF,$FF,$C0,$20
LBFC0:          .byte   $F0,$EF,$FF,$0F,$10,$11,$01,$F1
                .byte   $00
LBFC9:          .byte   $00,$08,$10,$18,$28,$30,$38,$40
LBFD1:          .byte   $50,$00,$20,$20,$20,$00,$A0,$00
                .byte   $00,$9F
LBFDB:          .byte   $0E,$0E,$0E,$0C,$0C,$0C,$0A,$0A
                .byte   $0A,$08,$08,$08,$06,$06,$04
LBFEA:          .byte   $04,$8A,$8F,$8D,$8B,$89,$87,$85
LBFF2:          .byte   $83,$00,$04,$01,$04,$01,$04,$01

; ----------------------------------------------------------------------------

cart_start:         .word   $704
cart_not_present:   .byte   $00
cart_options:       .byte   $80
cart_init:          .word   main
