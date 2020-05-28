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
  LDX #$29
  STX PPUDATA
  LDX #$19
  STX PPUDATA
  LDX #$09
  STX PPUDATA
  LDX #$0f
  STX PPUDATA
  ; write sprite data
  LDA #$70
  STA $0200 ; Y-coord of first sprite
  LDA #$05
  STA $0201 ; tile number of first sprite
  LDA #$00
  STA $0202 ; arrtibute of first sprite
  LDA #$80
  STA $0203 ; X-coord of first sprite

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
  LDA #%10010000 ; turn on NMI, sprites use first palette table
  STA PPUCTRL
  LDA #%00011110 ; turn on screen
  STA PPUMASK
forever:
  JMP forever
.endproc

.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "graphics.chr"