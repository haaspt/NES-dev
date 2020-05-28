.include "constants.inc"
.include "header.inc"

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_hander
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTI
.endproc

.import reset_handler

.export main
.proc main
  LDX PPUSTATUS
  ; write palette
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$04
  BNE load_palettes
  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$28
  BNE load_sprites

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
  LDA #%10011000 ; turn on NMI, sprites use first palette table
  STA PPUCTRL
  LDA #%00011110 ; turn on screen
  STA PPUMASK
forever:
  JMP forever
.endproc

.segment "RODATA"
palettes:
.byte $ff, $19, $09, $0f
sprites:
.byte $70, $0b, $00, $72
.byte $70, $08, $00, $7a
.byte $70, $0f, $00, $82
.byte $70, $0f, $00, $8a
.byte $70, $12, $00, $92
.byte $70, $12, $00, $92

.byte $78, $04, $00, $72
.byte $78, $0f, $00, $7a
.byte $78, $0f, $00, $82
.byte $78, $1c, $00, $8a
 


.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "graphics.chr"