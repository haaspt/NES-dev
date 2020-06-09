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
; playerstate: .res 1
scroll_y_pos: .res 1
cooldown: .res 1

; dynamic object stuff
; Entity table
; 16 x 3 bytes
; Byte 0: Entity type
; -- 00: inactive
; -- 01: bullet
; -- 02: enemy
; Byte 1: Sprite mem lobyte
; Byte 2: Sprite mem hibyte
objectAddressLookup: .res 48
freeObjectAddress: .res 2
despawnIndex: .res 1


.segment "CODE"

.proc irq_handler
  RTI
.endproc

.proc nmi_hander
  ; PPU Scroll
  LDA #$00
  STA PPUSCROLL
  LDA scroll_y_pos
  STA PPUSCROLL
  CMP #$00
  BNE @continue
  LDA #$F0
  STA scroll_y_pos
@continue:
  DEC scroll_y_pos

  ; Perform DMA
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  ; Reset gamestate update flag
  LDA #$01
  EOR gamestate
  STA gamestate

  ; Cooldown tick
  LDY cooldown
  CPY #$00
  BEQ @finish
  DEY
  STY cooldown

@finish:
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
  STA scroll_y_pos

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

  JSR initialize_object_table

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
  JSR bullet_pos_update
  LDA #$00
  STA gamestate
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

bullet_pos_update:
  LDX #$00
@loop:
  LDA objectAddressLookup, X
  CMP #$01 ; check if bullet
  BEQ @found
@iterate:
  TXA
  CLC
  ADC #$03
  TAX
  CPX #$30 ; size of object table
  BCC @loop
  RTS
@found:
  INX
  LDA (objectAddressLookup, X)
  ; check if bullet needs to be despawned
  CLC
  CMP #$03
  BCS @continue
  DEX
  JSR despawn_entity
  LDA #$00
  CMP #$00
  BEQ @iterate
@continue:
  CLC
  SBC #$02
  STA (objectAddressLookup, X)
  DEX
  LDA #$00
  CMP #$00
  BEQ @iterate

player_pos_update:
  JSR read_controller
  LDA buttons
  ORA #%00000000 ; determine if any button pressed
  BNE @continue
  RTS
@continue:
  LDY #$00
  LDX #$00

  LDA buttons
  AND #%11000000 ; bitmask all but A or B
  BEQ shootNotPressed
  JSR handle_shoot
shootNotPressed:  
  LDA buttons
  AND #%00001000 ; bitmask all but Up
  BEQ upNotPressed
  JSR move_up
upNotPressed:
  LDA buttons
  AND #%00000100 ; bitmask all but Down
  BEQ downNotPressed
  JSR move_down
downNotPressed:
  LDA buttons
  AND #%00000010 ; bitmask all but Left
  BEQ leftNotPressed
  JSR move_left
leftNotPressed:
  LDA buttons
  AND #%00000001 ; bitmask all but Right
  BEQ rightNotPressed
  JSR move_right
rightNotPressed:
  RTS

move_down:
  LDX #$00
  LDY #$00
  ; Prevent screen wrap
  LDA PL_Y
  CMP #$DE  ; Effective bottom of screen ($06) + #$08px
  BNE @loop
  RTS
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
  LDX #$00
  LDY #$00
  ; Prevent screen wrap
  LDA PL_Y
  CMP #$0E  ; Effective top of screen ($06) + #$08px
  BNE @loop
  RTS
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

move_right:
  LDX #$03
  LDY #$00
  ; Prevent screen wrap
  LDA PL_X
  CMP #$F8 ; Right edge of screen ($FF) - #$08px
  BNE @loop
  RTS
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

move_left:
  LDX #$03
  LDY #$00
  ; Prevent screen wrap
  LDA PL_X
  CMP #$08 ; Left edge of screen ($00) + #$08px
  BNE @loop
  RTS
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

handle_shoot:
  LDA cooldown
  CMP #$00
  BEQ @continue
  RTS
@continue:
  LDA #$10
  STA cooldown
  JSR spawn_bullet
  RTS

despawn_entity:
  ; X register set with table offset before subroutine call
  STX despawnIndex ; store original index to tmp
  LDY #$00
  ; change entity status
  LDA #$00
  STA objectAddressLookup, X
  INX
  LDA objectAddressLookup, X
  STA freeObjectAddress
  INX
  LDA objectAddressLookup, X
  STA freeObjectAddress + 1
  LDY #$00
  LDA #$00
@loop:
  STA (freeObjectAddress), Y
  INY
  CPY #$04
  BNE @loop
  LDA #$00
  STA freeObjectAddress
  STA freeObjectAddress + 1
  LDX despawnIndex ; reload original index from tmp
  RTS

spawn_bullet:
  LDY #$01 ; seeking to spawn a bullet
  JSR find_free_object_slot
  LDA freeObjectAddress
  CMP #$00
  BNE @continue
  RTS
@continue:
  LDY #$00
  LDA PL_Y
  SBC #$0A ; offset from player Y by 10px
  STA (freeObjectAddress), Y
  INY
  LDA #$03 ; bullet sprite
  STA (freeObjectAddress), Y
  INY
  LDA #$06 ; bullet pallet
  STA (freeObjectAddress), Y
  INY
  LDA PL_X
  SBC #$04 ; offset from player X by 4px
  STA (freeObjectAddress), Y
  RTS

find_free_object_slot:
  LDX #$00
@loop:
  LDA objectAddressLookup, X
  CMP #$00 ; check if free entity
  BEQ @found
  TXA
  CLC
  ADC #$03
  TAX
  CPX #$30 ; size of object table
  BCC @loop
  LDA #$00
  STA freeObjectAddress
  RTS
@found:
  STY objectAddressLookup, X ; desired object type stored in Y register
  INX
  LDA objectAddressLookup, X
  STA freeObjectAddress
  INX
  LDA objectAddressLookup, X
  STA freeObjectAddress + 1
  RTS

initialize_object_table:
  ; initialize object address lookup
  LDX #$00
  LDA #$10 ; lowbyte
@loop:
  LDY #$00 ; all entities start inactive
  STY objectAddressLookup, x
  INX
  STA objectAddressLookup, X
  INX
  LDY #$02 ; sprite table hibyte
  STY objectAddressLookup, X
  INX
  CLC
  ADC #$04
  CPX #$30
  BNE @loop

  RTS


.endproc

.segment "RODATA"
sprites:
.byte $C0, $05, $06, $7F
.byte $C0, $06, $06, $87
.byte $C8, $07, $06, $7F
.byte $C8, $08, $06, $87

palettes:
.incbin "./graphics/bg_palette.pal"
.incbin "./graphics/sp_palette.pal"
 
nametable:
.incbin "./graphics/bg_starfield.nam"

.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "./graphics/graphics.chr"
