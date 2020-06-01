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
  ; Reset gamestate update flag
  LDA #$01
  EOR GAMESTATE
  STA GAMESTATE
  RTI
.endproc

.import reset_handler

.export main
.proc main
  ; Gamestate status flags
  ; ---- ---U
  ; |||| ||||
  ; |||| |||+- Gamestate updated
  ; |||| ||+-- Undef
  ; |||| |+--- Undef
  ; |||| +---- Undef
  ; ||++------ Undef
  ; |+-------- Undef
  ; +--------- Undef
  LDA #%00000000
  STA GAMESTATE

  LDY #$00
load_sprites:
  LDA sprites, X
  STA SPRITETAB, X
  INX
  CPX #$10
  BNE load_sprites

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

  LDA #$00
  STA $20
  STA $21

  LDX PPUSTATUS
  ; write background
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDY #$00
  LDX #$00
  LDA #<nametable
  STA $20
  LDA #>nametable
  STA $21
load_background:
  LDA ($20), Y
  STA PPUDATA
  INY
  CPY #$00
  BNE load_background
  INC $21
  INX
  CPX #$04
  BNE load_background

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
  LDA #%10010000 ; turn on NMI, sprites use first palette table
  STA PPUCTRL
  LDA #%00011110 ; turn on screen
  STA PPUMASK
forever:
  LDA GAMESTATE
  ADC #$01
  BCC player_pos_update
  JMP forever

player_pos_update:
  LDA #$01
  EOR GAMESTATE
  STA GAMESTATE
  LDY #$00
  LDX #$00
pos_loop:
  INC SPRITETAB, X
  TXA
  ADC #$04
  TAX
  INY
  CPY #$04
  BNE pos_loop
  JMP forever
.endproc

.segment "RODATA"
sprites:
.byte $80, $05, $06, $80
.byte $80, $06, $06, $88
.byte $88, $07, $06, $80
.byte $88, $08, $06, $88

palettes:
.incbin "./graphics/bg_palette.pal"
.incbin "./graphics/sp_palette.pal"
 
nametable:
.incbin "./graphics/bg_nametable.nam"

.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "./graphics/graphics.chr"