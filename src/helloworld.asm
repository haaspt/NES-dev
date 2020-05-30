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
  CPX #$20
  BNE load_palettes

  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$30
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
.incbin "./graphics/bg_palette.pal"
.incbin "./graphics/sp_palette.pal"

sprites:
.byte $70, $0b, $01, $72
.byte $70, $08, $01, $7a
.byte $70, $0f, $01, $82
.byte $70, $0f, $01, $8a
.byte $70, $12, $01, $92
.byte $70, $2D, $01, $9a

.byte $79, $04, $03, $72
.byte $79, $0f, $03, $7a
.byte $79, $0f, $03, $82
.byte $79, $1c, $03, $8a
.byte $79, $28, $03, $92
 


.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "./graphics/graphics.chr"