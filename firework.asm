;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(FIREWORK_ASM)
FIREWORK_ASM	SET	1

include "syntax.inc"
include "gbhw.inc"
include "cgbhw.inc"
include "vars.asm"
include "irq.asm"
include "rgb.asm"
include "random.asm"
include "smoke.asm"	; for its randomly_alter_tile
include "tileGraphics.asm"
include "movement.asm"

TILE_SIZE	  =	16
FIREWORK_HTILE	  =	3
FIREWORK_VTILE	  =	4
FIREWORK_CTILE	  =	5
FIREWORK_PALETTE  =	7

	var_LowRamByte	rFW_CenterY
	var_LowRamByte	rFW_CenterX	; center of firework explosion
	var_LowRamByte	rFW_Radius	; radius of explosion
	var_LowRamByte	rFW_DestY
	var_LowRamByte	rFW_DestX	; where explosion will occur
	var_LowRamByte	rFW_Tick	; timing tick. Firework repeats if<=1

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
	; INIT firework TICK variable
	call	firework_cycle_init
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
	rand_A
	and	rgb_PALETTE_MASK
	or	1	; set palette to >= 1 (don't use boring palette 0)
	ld	b, a	; store palette in b
	sprite_PutFlags	Spr_FW_Horiz_L, a
	set	5, a	; set horizontal flip flag
	sprite_PutFlags	Spr_FW_Horiz_R, a
	; set vertcal flip flags
	lda	b	; restore palette
	sprite_PutFlags	Spr_FW_Vert_U, a	; sprite4
	set	6, a	; set vertical flip flag
	sprite_PutFlags	Spr_FW_Vert_D, a	; sprite4
	; set corner flags
	lda	b	; restore palette
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
	ret

; AND mask C, then OR mask B (in that order)
firework_apply_palette_masks: MACRO
	sprite_GetFlags	\1
	and	c
	or	b
	sprite_PutFlags	\1, a
	ENDM

firework_change_palette:
	rand_A
	and	rgb_PALETTE_MASK
	ifa	==, 0, lda 1	; do not use boring palette 0 (greyscale)
	ld	b, a	; load palette into B
	ld	a, %11111111 ^ rgb_PALETTE_MASK
	ld	c, a	; load anti-palette mask into C
	firework_apply_palette_masks	Spr_FW_Horiz_L
	firework_apply_palette_masks	Spr_FW_Horiz_R
	firework_apply_palette_masks	Spr_FW_Vert_U
	firework_apply_palette_masks	Spr_FW_Vert_D
	firework_apply_palette_masks	Spr_FW_Corner_UL
	firework_apply_palette_masks	Spr_FW_Corner_UR
	firework_apply_palette_masks	Spr_FW_Corner_DL
	firework_apply_palette_masks	Spr_FW_Corner_DR
	ret


; fires once per frame. 60x per second
firework_HandleLCDC:
	call	firework_cycle
	call	firework_alter_tiles
	ret

; call this ONCE to initiate fireworks and get ready to launch
firework_cycle_init:
	lda	89
	ld	[rFW_Tick], a
	xor	a
	ld	[rFW_Radius], a
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
	ifa	<, 60, jp .explosion_expand_no_more
	ifa	<, 80, jp .explosion_hang_in_air
	ifa	<, 90, jp .hide_firework
	ifa	==, 90, jp .calculate_destination
	ifa	<, 200, jp .launch_firework
.explosion_start
	; bundle fireworks all at the center
	xor	a
	ld	[rFW_Radius], a
	lda	[rFW_Tick]
	inc	a		; increment tick so that it won't remain
	ld	[rFW_Tick], a	; in explosion start for too long
	ld	bc, firework_font
	var_SetWord	b, c, rFireworkMask
	; now call the function to arrange the fireworks
	jp	.done
.explosion_expand_fast
	ld	hl, rFW_Radius
	inc	[hl]
	lda	[rFW_Tick]
	RRCA
	if_flag	c, inc [hl]	; offset moves at 1.5 pixels per tick (average)
	; (given that we move 1 pixel every tick + .5 from the above command)
	jp	.done
.explosion_expand_medium
	ld	hl, rFW_Radius
	inc [hl]	; offset moves at 1 pixel per tick
	jp	.done
.explosion_expand_slow
	ld	hl, rFW_Radius
	lda	[rFW_Tick]
	RRCA
	if_flag	c, inc [hl]	; move an additional pixel if tick is odd
	; set firework mask to diminished firework
	ld	bc, firework_font + 1*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
.explosion_expand_no_more
	; set firework mask to vastly diminished firework
	ld	bc, firework_font + 2*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
.explosion_hang_in_air
	; set explosion mask to minimal
	ld	bc, firework_font + 3*TILE_SIZE
	var_SetWord	b, c, rFireworkMask
	jp	.done
.hide_firework
	; if we get here, fireworks have finished and need to "disappear"
	lda	200
	ld	[rFW_CenterX], a
	ld	[rFW_CenterY], a
	xor	a
	ld	[rFW_Radius], a
	; reset firework mask to full-brightness
	ld	bc, firework_font
	var_SetWord	b,c, rFireworkMask
	call	firework_change_palette
	jp	.done
.calculate_destination
	rand_A	; get a random number
	; corrall firework in bounds of 30>= Y =<90
	ifa	>, 100, math_Div a, 2	; keep value <= 90
	; may have to divide twice... to get it below 80
	ifa	>, 100, math_Div a, 2	; keep value <= 90
	ifa	<, 30, add 60		; center firework if < 30
	ld	[rFW_DestY], a
	rand_A	; get 2nd random number for X
	ifa	>, 100, math_Div a, 2
	ifa	>, 100, math_Div a, 2
	ifa	<, 25, add 50		; center firework if < 20
	ld	[rFW_DestX], a
	jp	.done
.launch_firework
	; firework has been hidden. Now we need to launch firework into air
	; from below screen. Once firework reaches destination, trigger
	; explosion (tick=0)
	; load source coordinates
	lda	[rFW_CenterY]
	ld	b, a
	lda	[rFW_CenterX]
	ld	c, a
	; load destination coordinates
	lda	[rFW_DestY]
	ld	d, a
	lda	[rFW_DestX]
	ld	e, a
	; load updated coordinates
	call	move_BC_sixteenth_to_DE
	lda	b
	ld	[rFW_CenterY], a
	lda	c
	ld	[rFW_CenterX], a
	; reset tick (and start explosion) if firework at destination
	lda	b
	ifa	==, d, jp .restart_firework_explosion
	lda	c
	ifa	==, e, jp .restart_firework_explosion
	jp	.done
.restart_firework_explosion
	; we get here if BC == DE
	xor	a
	ld	[rFW_Tick], a
	jp	firework_cycle	; start firework explosion immediately
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
; distance of rFW_Radius
fireworks_center_offset:
	lda	[rFW_Radius]
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
