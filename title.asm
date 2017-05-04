;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.asm"
include "matrix.asm"
include "crosshairs.asm"
include "dma.asm"
include "math.asm"
include "debug.asm"
; holds methods for displaying the title-screen


	IF !DEF(title_asm)
title_asm	set	1


; generate text on fly
screen_write: MACRO
	jr	.past_letters\@
.letters\@
	DB	\3
.past_letters\@
	mat_GetYX	_SCRN0, \1, \2
	ldpair	de, hl	; destination is screen
	ld	hl, .letters\@	; source is screen letters
	ld	bc, .past_letters\@ - .letters\@; # of bytes to write
	call	mem_CopyVRAM
	ENDM

EASY_Y		SET	8
MEDIUM_Y	SET	10
HARD_Y		SET	12

EASY_DIFFICULTY	SET	30
EASY_RAMP	SET	5
MEDIUM_DIFFICULTY	SET	42
MEDIUM_RAMP	SET	10
HARD_DIFFICULTY	SET	55
HARD_RAMP	SET	15

; displays startscreen and waits for user input before returning
; runs a tight loop so that when the user presses a button,
; it initializes the random-value generator (not tied to vblank)
title_LevelSelect:
	screen_write	5, 5, "Level Select"
	screen_write	EASY_Y, 5, "Easy"
	screen_write	MEDIUM_Y, 5, "Medium"
	screen_write	HARD_Y, 5, "Hard"
	lda	8 + 2*8	; offset + X*grid
	ld	[rPlayerX], a
	ld	[rCrosshairX], a
	lda	EASY_Y * 8
	ld	[rPlayerY], a
	ld	[rCrosshairY], a
	call	crosshairs_update
	call	DMACODELOC
	irq_CallFromVBLANK	title_vblank
	irq_EnableVBLANK
.loop_for_input
	if_not	jpad_EdgeA, jr .loop_for_input
	; we get here if the user pressed A
	call	set_difficulty_chosen
	call	jpad_GetKeys	; read keys again so that A is not "just"
				; pressed. Prevents actions from happening
	ld	a, 8*8		; place player coordinates directly in middle
	ld	[rPlayerY], a
	ld	a, 9*8
	ld	[rPlayerX], a
	ret	; return to main execution

; handle moving cursor up or down
title_vblank:
	call	jpad_GetKeys
	if_	jpad_EdgeDown, call MoveDownLevels
	if_	jpad_EdgeUp, call MoveUpLevels
	call	crosshairs_update
	call	DMACODELOC
	ret

MoveDownLevels:
	ld	hl, rPlayerY
	lda	[hl]
	add	16
	ld	[hl], a
	ret
MoveUpLevels:
	ld	hl, rPlayerY
	lda	[hl]
	sub	16
	ld	[hl], a
	ret

Player@Easy:
	lda	[rPlayerY]
	math_Div	a, 8
	ifa	==, EASY_Y, ret_true
	ret_false
Player@Medium:
	lda	[rPlayerY]
	math_Div	a, 8
	ifa	==, MEDIUM_Y, ret_true
	ret_false
Player@Hard:
	lda	[rPlayerY]
	math_Div	a, 8
	ifa	==, HARD_Y, ret_true
	ret_false


; set difficulty based on player Y's position (which corresponds to difficulty)
; default of easy is chosen if it couldn't match any
set_difficulty_chosen:
	if_	Player@Easy, jr .easy
	if_	Player@Medium, jr .medium
	if_	Player@Hard, jr .hard
.default
	bug_message	"didn't match any difficulty"
.easy
	lda	EASY_DIFFICULTY
	ld	[rDifficulty], a
	lda	EASY_RAMP
	ld	[rDifficultyRamp], a
	ret
.medium
	lda	MEDIUM_DIFFICULTY
	ld	[rDifficulty], a
	lda	MEDIUM_RAMP
	ld	[rDifficultyRamp], a
	ret
.hard
	lda	HARD_DIFFICULTY
	ld	[rDifficulty], a
	lda	HARD_RAMP
	ld	[rDifficultyRamp], a
	ret


	ENDC	; end title define
