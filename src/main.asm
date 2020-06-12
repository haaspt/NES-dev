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
gun_cooldown_timer: .res 1
enemy_spawn_timer: .res 1

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

collisionEntityIndexes: .res 2 ; offsets i and j for scaning object table
collisionEntityAPointer: .res 2 ; OAM pointer for entity "A"
collisionEntityBPointer: .res 2 ; OAM pointer for entity "B"
colXa: .res 2 ; X1 and X2 coords for entity A
colXb: .res 2 ; X1 and X2 coords for entity B
colYa: .res 2 ; Y1 and Y2 coords for entity A
colYb: .res 2 ; Y1 and Y2 coords for entity B
;; TODO: combine collision flag into gamestate byte?
entitiesAreColliding: .res 1 ; Binary flag set by detect_collision subroutine

hitCount: .res 1 ;; tmp variable for collision detection development

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

  ;; COOLDOWN TIMER TICKS
timer_updates:
  LDY gun_cooldown_timer
  CPY #$00
  BEQ @next
  DEY
  STY gun_cooldown_timer
@next:
  LDY enemy_spawn_timer
  CPY #$00
  BEQ @finish
  DEY
  STY enemy_spawn_timer

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

  LDA #$7F
  STA enemy_spawn_timer

  LDY #$00
load_player_sprites:
  LDA player_sprites, X
  STA SPRITETAB, X
  INX
  CPX #$10
  BNE load_player_sprites

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
  LDA enemy_spawn_timer
  BNE @no_enemy_spawn
  JSR spawn_enemy
  LDA #$7F
  STA enemy_spawn_timer
@no_enemy_spawn:
  JSR handle_player_input
  JSR bullet_pos_update
  JSR enemy_pos_update
  JSR scan_for_bullet_collisions
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

enemy_pos_update:
  LDX #$00
@loop:
  LDA objectAddressLookup, X
  CMP #$02 ; check if enemy
  BEQ @found
@iterate:
  TXA 
  CLC
  ADC #$03 ; size of table object
  TAX
  CPX #$30 ; size of object table
  BCC @loop
  RTS
@found:
  INX
  LDA (objectAddressLookup, X)
  ; check if enemy needs to be despawned
  CLC
  CMP #$DE
  BCC @continue
  DEX 
  JSR despawn_entity
  SEC
  BCS @iterate
@continue:
  CLC
  ADC #$02
  STA (objectAddressLookup, X)
  DEX
  SEC
  BCS @iterate

bullet_pos_update:
  LDX #$00
@loop:
  LDA objectAddressLookup, X
  CMP #$01 ; check if bullet
  BEQ @found
@iterate:
  TXA
  CLC
  ADC #$03 ; size of table object
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
  SEC
  BCS @iterate
@continue:
  CLC
  SBC #$02
  STA (objectAddressLookup, X)
  DEX
  SEC
  BCS @iterate

handle_player_input:
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
  LDA gun_cooldown_timer
  BEQ @continue
  RTS
@continue:
  LDA #$10
  STA gun_cooldown_timer
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

spawn_enemy:
  LDY #$02 ; seeking to spawn an enemy
  JSR find_free_object_slot
  LDA freeObjectAddress
  BNE @continue
  RTS
@continue:
  LDY #$00
  LDA #$00 ; spawn enemy at top of screen
  STA (freeObjectAddress), Y
  INY
  LDA enemy_sprite + 1
  STA (freeObjectAddress), Y
  INY
  LDA enemy_sprite + 2
  STA (freeObjectAddress), Y
  INY
  LDA PL_X ; spawn enemy at player X (for now)
  STA (freeObjectAddress), Y
  RTS

spawn_bullet:
  LDY #$01 ; seeking to spawn a bullet
  JSR find_free_object_slot
  LDA freeObjectAddress
  BNE @continue
  RTS
@continue:
  LDY #$00
  LDA PL_Y
  SBC #$0A ; offset from player Y by 10px
  STA (freeObjectAddress), Y
  INY
  LDA bullet_sprite + 1 ; bullet sprite
  STA (freeObjectAddress), Y
  INY
  LDA bullet_sprite + 2 ; bullet pallet
  STA (freeObjectAddress), Y
  INY
  LDA PL_X
  SBC #$04 ; offset from player X by 4px
  STA (freeObjectAddress), Y
  RTS

find_free_object_slot:
  LDX #$00
@loop:
  LDA objectAddressLookup, X ; check if free entity
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

scan_for_bullet_collisions:
  ;; scan setup
  LDA #$00
  LDX #$00
  STA collisionEntityIndexes
  STA collisionEntityIndexes + 1
@outerLoop: ; scan table for bullets
  LDA objectAddressLookup, X
  CMP #$01
  BEQ @foundA
@outerIterate:
  TXA
  CLC
  ADC #$03
  TAX
  CPX #$30
  BNE @outerLoop
  RTS
@foundA:
  STX collisionEntityIndexes
  LDY #$00
@innerLoop:
  LDA objectAddressLookup, Y
  CMP #$02
  BEQ @foundB
@innerIterate:
  TYA
  CLC
  ADC #$03
  TAY
  CPY #$30
  BNE @innerLoop
  JMP @outerIterate
@foundB:
  STY collisionEntityIndexes + 1
  LDY #$00
  LDA collisionEntityIndexes
  TAX
  INX
  LDA objectAddressLookup, X
  STA collisionEntityAPointer
  INX
  LDA objectAddressLookup, X
  STA collisionEntityAPointer + 1
  LDA collisionEntityIndexes + 1
  TAX
  INX
  LDA objectAddressLookup, X
  STA collisionEntityBPointer
  INX
  LDA objectAddressLookup, X
  STA collisionEntityBPointer + 1

  ;; Load entity coords into memory
  LDY #$00
  LDA (collisionEntityAPointer), Y
  ADC #$01
  STA colYa
  CLC
  ADC  #$06
  STA colYa + 1
  LDA (collisionEntityBPointer), Y
  ADC #$01
  STA colYb
  CLC
  ADC #$06
  STA colYb + 1
  LDY #$03
  LDA (collisionEntityAPointer), Y
  ADC #$01
  STA colXa
  CLC
  ADC #$06
  STA colXa + 1
  LDA (collisionEntityBPointer), Y
  ADC #$01
  STA colXb
  CLC
  ADC #$06
  STA colXb + 1

  JSR detect_collision
  LDA entitiesAreColliding
  BEQ @noCollision
  JSR handle_collision
@noCollision:
  LDX collisionEntityIndexes
  JMP @outerIterate

handle_collision:
  LDX collisionEntityIndexes
  JSR despawn_entity
  LDX collisionEntityIndexes + 1
  JSR despawn_entity
  INC hitCount
  RTS

detect_collision:
  ;; Rect. overlap checking
  LDA colYb
  CMP colYa + 1
  BCS @no_overlap ; yA2 <= yB1
  LDA colYa
  CMP colYb + 1
  BCS @no_overlap ; yB2 <= yA1
  LDA colXb
  CMP colXa + 1
  BCS @no_overlap ; xA2 <= xB1
  LDA colXa
  CMP colXb + 1
  BCS @no_overlap ; xB2 <= xA1
  ;; Object are overlapping
  LDA #$01
  STA entitiesAreColliding
  RTS
@no_overlap:
  LDA #$00
  STA entitiesAreColliding
  RTS

.endproc

.segment "RODATA"
player_sprites:
.byte $C0, $05, $06, $7F
.byte $C0, $06, $06, $87
.byte $C8, $07, $06, $7F
.byte $C8, $08, $06, $87

bullet_sprite:
.byte $00, $0B, $05, $00

enemy_sprite:
.byte $00, $0C, $07, $00

big_enemy_sprites:
.byte $00, $0D, $07, $00
.byte $08, $0F, $07, $00
.byte $10, $0E, $07, $00

palettes:
.incbin "./graphics/bg_palette.pal"
.incbin "./graphics/sp_palette.pal"
 
nametable:
.incbin "./graphics/bg_starfield.nam"

.segment "VECTORS"
.addr nmi_hander, reset_handler, irq_handler

.segment "CHR"
.incbin "./graphics/graphics.chr"
