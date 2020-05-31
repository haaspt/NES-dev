.include "constants.inc"

.segment "CODE"
.import main
.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$00
  STX PPUCTRL
  STX PPUMASK
  ; Clear zerotable
  LDA #$00
clear_loop:
  STA $000, X
  STA $100, X
  STA $200, X
  STA $300, X
  STA $400, X
  STA $500, X
  STA $600, X
  STA $700, X
  INX
  CPX #$00
  BNE clear_loop

  LDX #$FF  ; reset stack pointer
  TXS
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
  JMP main
.endproc