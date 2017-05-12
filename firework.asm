;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(FIREWORK_ASM)
FIREWORK_ASM	SET	1

include "syntax.asm"
include "vars.asm"
include "irq.asm"
include "gbhw.inc"
include "cgbhw.inc"
include "random.asm"
include "smoke.asm"	; for its randomly_alter_tile
include "tileGraphics.asm"

TILE_SIZE	  =	16
FIREWORK_HTILE	  =	3
FIREWORK_VTILE	  =	4
FIREWORK_CTILE	  =	5
FIREWORK_PALETTE  =	7

	var_LowRamByte	rFW_CenterY
	var_LowRamByte	rFW_CenterX
	var_LowRamByte	rFW_Tick
	var_LowRamByte	rFW_VY
	var_LowRamByte	rFW_VX
	var_LowRamByte	rFW_Offset

	var_LowRamByte	rFireworkRandByte	; holds onto byte # to change
	var_LowRamWord	rFireworkMask	; holds onto mask address
	var_MidRamBytes	rFW_horizontal, TILE_SIZE
	var_MidRamBytes	rFW_vertical, TILE_SIZE
	var_MidRamBytes	rFW_corner, TILE_SIZE
	; declare 8 sprites: firework balls of light
	SpriteAttr	Spr_FW_Vert_U	; has attributes for sprite
	SpriteAttr	Spr_FW_Vert_D
	SpriteAttr	Spr_FW_Horiz_L
	SpriteAttr	Spr_FW_Horiz_R
	SpriteAttr	Spr_FW_Corner_UR ; corner in axis 1 (upper right)
	SpriteAttr	Spr_FW_Corner_UL ; corner in axis 1 (upper left)
	SpriteAttr	Spr_FW_Corner_DR ; corner in axis 1 (down right)
	SpriteAttr	Spr_FW_Corner_DL ; corner in axis 1 (down right)

; load three firework tiles into ram.
; 1) verticals 2) horizontals 3) corners
; during vblank their changes will be loaded into vram
; set up sprites to point to those 3 vram spots
; sprites are flipped (vertically and horizontally) according to their
; position.
firework_create_particles:
	; copy over firework particles to ram (not VRAM)
	ld	bc, TILE_SIZE
	ld	hl, mine_gfx + 4 * TILE_SIZE
	ld	de, rFW_horizontal
	preserve2 HL, BC,	call	mem_Copy	; copy to ram
	ld	de,rFW_vertical
	preserve2 HL, BC,	call	mem_Copy	; copy to ram
	ld	de, rFW_corner
	call	mem_Copy	; copy to ram
	; setup sprite images so that they point to particle
	ld	a, FIREWORK_HTILE
	sprite_PutTile	Spr_FW_Horiz_L, a	; sprite3
	sprite_PutTile	Spr_FW_Horiz_R, a	; sprite3
	lda	FIREWORK_VTILE
	sprite_PutTile	Spr_FW_Vert_U, a	; sprite4
	sprite_PutTile	Spr_FW_Vert_D, a	; sprite4
	lda	FIREWORK_CTILE
	sprite_PutTile	Spr_FW_Corner_UR, a	; sprite5
	sprite_PutTile	Spr_FW_Corner_UL, a	; sprite5
	sprite_PutTile	Spr_FW_Corner_DR, a	; sprite5
	sprite_PutTile	Spr_FW_Corner_DL, a	; sprite5
	; now lets set the Horizontal, Vertical flip flags
	; Upper Left are normal, we flip for right & down
	ld	a, FIREWORK_PALETTE
	sprite_PutFlags	Spr_FW_Horiz_L, a
	set	5, a	; set horizontal flip flag
	sprite_PutFlags	Spr_FW_Horiz_R, a
	; set vertcal flip flags
	ld	a, FIREWORK_PALETTE
	sprite_PutFlags	Spr_FW_Vert_U, a	; sprite4
	set	6, a	; set vertical flip flag
	sprite_PutFlags	Spr_FW_Vert_D, a	; sprite4
	; set corner flags
	ld	a, FIREWORK_PALETTE
	sprite_PutFlags	Spr_FW_Corner_UL, a	; sprite5
	set	5, a	; set horizontal flip flag
	sprite_PutFlags	Spr_FW_Corner_UR, a	; sprite5
	set	6, a	; set vertical flip flag
	sprite_PutFlags	Spr_FW_Corner_DR, a	; sprite5
	res	5, a	; undo horizontal flip
	sprite_PutFlags	Spr_FW_Corner_DL, a	; sprite5
	; SETUP FIREWORK VARIABLES
	ld	bc, firework_font
	; set FireworkMask to be full firework
	var_SetWord	b,c, rFireworkMask	; holds onto mask address
	xor	a
	ld	[rFW_Tick], a
	ld	[rFW_Offset], a
	ret


; fires once per frame. 60x per second
firework_HandleLCDC:
	call	firework_cycle
	call	firework_alter_tiles
	ret


; cycle fireworks. Explode, expand, disappear, repeat
; does not handle color of fireworks, nor tile changes
firework_cycle:
	lda	[rFW_Tick]
	inc	a
	ld	[rFW_Tick], a
	ifa	<=, 1, jp .explosion_start
	ifa	<, 10, jp .explosion_expand_fast
	ifa	<, 15, jp .explosion_expand_medium
	ifa	<, 30, jp .explosion_expand_slow
.explosion_hang_in_air
	; set explosion mask to minimal
	ld	bc, firework_font + 3*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
	ld	hl, rFW_Tick
	lda	[hl]
	ifa	<, 80, jp .done 	; don't need to move fireworks at all
	; if we get here, fireworks have finished and need to "disappear"
	lda	200
	ld	[rFW_CenterX], a
	ld	[rFW_CenterY], a
	xor	a
	ld	[rFW_Offset], a
	lda	[hl]
	ifa	>, 120, ld	[hl], 0
	; reset firework mask to full
	ld	bc, firework_font
	var_SetWord	b,c, rFireworkMask
	jp .done
.explosion_start
	rand_A	; get a random number
	ifa	>, 128, math_Div a, 2	; keep value <= 128
	ld	[rFW_CenterY], a
	ld	d, a
	rand_A	; get 2nd random number for X
	ifa	>, 128, math_Div a, 2
	ld	[rFW_CenterX], a
	ld	e, a
	; bundle fireworks all at the center
	xor	a
	ld	[rFW_Offset], a
	lda	[rFW_Tick]
	inc	a
	ld	[rFW_Tick], a	; increment firework so that it won't remain
				; in explosion start for too long
	ld	bc, firework_font
	var_SetWord	b, c, rFireworkMask
	; now call the function to arrange the fireworks
	jp	.done
.explosion_expand_fast
	ld	hl, rFW_Offset
	inc	[hl]
	lda	[rFW_Tick]
	RRCA
	if_flag	c, inc [hl]	; offset moves at 1.5 pixels per tick (average)
	; set firework mask to diminished firework
	ld	bc, firework_font
	var_SetWord	b, c, rFireworkMask
	jp	.done
.explosion_expand_medium
	ld	hl, rFW_Offset
	inc [hl]	; offset moves at 0.5 pixels per tick (average)
	; set firework mask to diminished firework
	ld	bc, firework_font + 1*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
	jp	.done
.explosion_expand_slow
	ld	hl, rFW_Offset
	lda	[rFW_Tick]
	RRCA
	if_flag	c, inc [hl]	; offset moves at 0.5 pixels per tick (average)
	; set firework mask to vastly diminished firework
	ld	bc, firework_font + 2*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
.done
	call	fireworks_center_offset
	ret


; given offset, position each firework around the center
; the horizontal and vertical will only adjust x, y (respectively) from
; the center. The corner case will move 75% of the offset in both x & y.
; moving 71% would nearly perfectly approximate a circle's distance in
; comparison to the x & y, but 75% is very easy to approximate:
; 25% offset + 50% offset
; this'll allow you to simply set an integer offset, and call this function
; to get a circle of fireworks around rFW_CenterX & rFW_CenterY at an offset
; distance of rFW_Offset
fireworks_center_offset:
	lda	[rFW_Offset]
	ld	b, a	; preserve offset in B
	math_Div	a, 2
	ld	c, a	; preserve 25% offset
	lda	b	; restore offset
	math_Div	a, 4
	add	c	; get 75% offset
	ld	c, a	; store 75% offset in c
	; change Y coordinates of all fireworks
	lda	[rFW_CenterY]
	ld	E, a	; store Y in D
	; horizontals
	PutSpriteYAddr	Spr_FW_Horiz_L, a
	PutSpriteYAddr	Spr_FW_Horiz_R, a
	; verticals
	add	b	; Y += offset
	PutSpriteYAddr	Spr_FW_Vert_D, a
	lda	E
	sub	b	; Y -= offset
	PutSpriteYAddr	Spr_FW_Vert_U, a
	; corners
	lda	E
	add	c	; Y += 75% offset
	PutSpriteYAddr	Spr_FW_Corner_DR, a
	PutSpriteYAddr	Spr_FW_Corner_DL, a
	lda	E
	sub	c	; y -= 75% offset
	PutSpriteYAddr	Spr_FW_Corner_UR, a
	PutSpriteYAddr	Spr_FW_Corner_UL, a
	; change X coordinates of fireworks
	lda	[rFW_CenterX]
	ld	E, a	; store X in E
	; verticals
	PutSpriteXAddr	Spr_FW_Vert_U, a
	PutSpriteXAddr	Spr_FW_Vert_D, a
	; horizontals
	add	b	; x += offset
	PutSpriteXAddr	Spr_FW_Horiz_R, a
	lda	E
	sub	b	; x -= offset
	PutSpriteXAddr	Spr_FW_Horiz_L, a
	; corners
	lda	E
	add	c	; x += 75% offset
	PutSpriteXAddr	Spr_FW_Corner_DR, a
	PutSpriteXAddr	Spr_FW_Corner_UR, a
	lda	E
	sub	c	; x -= 75% offset
	PutSpriteXAddr	Spr_FW_Corner_DL, a
	PutSpriteXAddr	Spr_FW_Corner_UL, a
	ret


; update each firework particle-image such that it changes over time
firework_alter_tiles:
	; DE points to RAM tile to update
	; HL points to mask (with which it'll AND)
	; A holds tile # in VRAM
	var_GetWord	h,l, rFireworkMask
	ld	de, rFW_horizontal
	ld	a, FIREWORK_HTILE
	push	hl
	call	randomly_alter_tile	; update a progressive row in tile
	pop	hl
	; edit and re-load 2nd particle
	ld	de, rFW_vertical
	ld	a, FIREWORK_VTILE
	push	hl
	call	randomly_alter_tile
	pop	hl
	; edit and re-load 3rd particle
	ld	de, rFW_corner
	ld	a, FIREWORK_CTILE
	call	randomly_alter_tile
	ret


	ENDC	; end firework.asm defines
