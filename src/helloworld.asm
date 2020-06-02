.include "constants.inc"
.include "header.inc"

.zeropage
gamestate: .res 1
buttons: .res 1
; playerstates:
; 1 = Facing Up
; 2 = Facing Down
; 3 = Facing Right
; 4 = Facing Left
playerstate: .res 1

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
  EOR gamestate
  STA gamestate
  RTI
.endproc

.import reset_handler

.export main
.proc main
  ; Gamestate status flags
  ; ---- --IU
  ; |||| ||||
  ; |||| |||+- Gamestate update pending
  ; |||| ||+-- Input registered
  ; |||| |+--- Undef
  ; |||| +---- Undef
  ; ||++------ Undef
  ; |+-------- Undef
  ; +--------- Undef
  LDA #%00000000
  STA gamestate

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
  LDA gamestate
  AND #%00000001
  BEQ no_update
  JSR player_pos_update
no_update:
  JMP forever

read_controller:
  LDA #$01
  STA JOYPAD1 ; begin contoller polling
  STA buttons ; set to 1 for ring counter
  LSR A ; shift #$01->#$00
  STA JOYPAD1 ; stop controller polling
@loop:
  LDA JOYPAD1
  LSR A ; bit 0 -> carry
  ROL buttons ; carry -> bit 0; bit 7 -> carry
  BCC @loop
  RTS

player_pos_update:
  JSR read_controller
  LDA buttons
  ORA #%00000000 ; determine if any button pressed
  BNE update
  RTS
update:
  LDA #$01
  EOR gamestate ; flip gamestate update flag off
  STA gamestate 
  LDY #$00
  LDX #$00
  LDA buttons
  AND #%00001000 ; bitmask all but Up
  BNE move_up
  LDA buttons
  AND #%00000100 ; bitmask all but Down
  BNE move_down
  RTS

move_down:
@loop:
  CLC
  INC PLAYERLOC, X
  TXA
  ADC #$04
  TAX
  INY
  CPY #$04
  BNE @loop
  RTS

move_up:
@loop:
  CLC
  DEC PLAYERLOC, X
  TXA
  ADC #$04
  TAX
  INY
  CPY #$04
  BNE @loop
  RTS
.endproc

.segment "RODATA"
sprites:
.byte $80, $05, $06, $76
.byte $80, $06, $06, $7E
.byte $88, $07, $06, $76
.byte $88, $08, $06, $7E

palettes:
.incbin "./graphics/bg_palette.pal"
.incbin "./graphics/sp_palette.pal"
 
nametable:
.incbin "./graphics/bg_nametable.nam"

.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "./graphics/graphics.chr"
