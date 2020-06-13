PPUCTRL     = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
PPUSCROLL   = $2005
PPUADDR     = $2006
PPUDATA     = $2007
OAMADDR     = $2003
OAMDMA      = $4014
OAMTAB      = $0200

JOYPAD1     = $4016
JOYPAD2     = $4017

SPRITETAB   = $0200
PLAYERLOC   = SPRITETAB

PL_Y       = $020C ; player sprite center pt y
PL_X       = $020F ; player sprite center pt x

;; GLOBALS
Y_SCROLL_LIMIT      = $F0
BULLET_Y_LIMIT      = $03
ENEMY_Y_LIMIT       = $DE

GUN_COOLDOWN        = $10
ENEMY_SPAWN_RATE    = $7F
ENEMY_OBJ_TIMER     = $10
EXPL_OBJ_TIMER      = $04

OBJECT_SIZE         = $04
OBJECT_TABLE_LEN    = $40