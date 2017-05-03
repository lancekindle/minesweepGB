;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.asm"
include "matrix.asm"
include "crosshairs.asm"
include "dma.asm"
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
; displays startscreen and waits for user input before returning
; runs a tight loop so that when the user presses a button,
; it initializes the random-value generator (not tied to vblank)
title_LevelSelect:
	screen_write	5, 5, "Level Select"
	screen_write	8, 5, "Easy"
	screen_write	10, 5, "Medium"
	screen_write	12, 5, "Hard"
	lda	8 + 2*8	; offset + X*grid
	ld	[rPlayerX], a
	ld	[rCrosshairX], a
	lda	16 + 6*8	; offset + Y*grid
	ld	[rPlayerY], a
	ld	[rCrosshairY], a
	call	crosshairs_update
	call	DMACODELOC
	irq_CallFromVBLANK	title_vblank
	irq_EnableVBLANK
.loop_for_input
	if_not	jpad_EdgeA, jr .loop_for_input
	; we get here if the user pressed A
	ret	; return to main execution

; handle moving cursor up or down
title_vblank:
	call	jpad_GetKeys
	if_	jpad_EdgeDown, call MoveDownn
	if_	jpad_EdgeUp, call MoveUpp
	call	crosshairs_update
	call	DMACODELOC
	ret

MoveDownn:
	ld	hl, rPlayerY
	lda	[hl]
	add	16
	ld	[hl], a
	ret
MoveUpp:
	ld	hl, rPlayerY
	lda	[hl]
	sub	16
	ld	[hl], a
	ret


	ENDC	; end title define
