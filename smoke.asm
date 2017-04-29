;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(SMOKE_ASM)
SMOKE_ASM	SET	1

include	"syntax.asm"
include "tileGraphics.asm"
include "memory.asm"
include "sprite.inc"


	var_LowRamByte	rSmokeRandByte	; holds onto byte # to change
	var_MidRamBytes	rSmokeTile1, 16
	var_MidRamBytes	rSmokeTile2, 16
	var_MidRamBytes	rSmokeTile3, 16
	; learn SpriteAttr from sprite.inc
	SpriteAttr	Spr_Smoke1	; has attributes Spr_Smoke1TileNum, etc
	SpriteAttr	Spr_Smoke2
	SpriteAttr	Spr_Smoke3

; create three smoke particles over the bomb.
create_smoke_particles:
	; copy over smoke particles to ram (not VRAM)
	ld	de, rSmokeTile1
	ld	bc, smoke_font_end - smoke_font
	ld	hl, smoke_font
	preserve2 HL, BC,	call	mem_Copy	; copy to ram
	ld	de, rSmokeTile2
	preserve2 HL, BC,	call	mem_Copy	; copy to ram
	ld	de, rSmokeTile3
	call	mem_Copy	; copy to ram
	; setup sprite images so that they point to smoke particles
	ld	a, 3
	sprite_PutTile	Spr_Smoke1, a	; sprite3
	inc	a
	sprite_PutTile	Spr_Smoke2, a	; sprite4
	inc	a
	sprite_PutTile	Spr_Smoke3, a	; sprite5
	; get player coordinates (Actual pixel #) in de
	lda	[rPlayerY]
	ld	d, a
	lda	[rPlayerX]
	ld	e, a
	; update coordinates of smoke particles to be 'nearby' explosion
	; place smoke 1 @ (X-3,Y-3)
	lda	d
	sub	3
	PutSpriteYAddr	Spr_Smoke1, a
	lda	e
	sub	3
	PutSpriteXAddr	Spr_Smoke1, a
	; place smoke 2 @ (X + 2, Y)
	lda	d
	PutSpriteYAddr	Spr_Smoke2, a
	lda	e
	add	5
	PutSpriteXAddr	Spr_Smoke2, a
	; place smoke 3 @ (X, Y + 2)
	lda	d
	add	5
	PutSpriteYAddr	Spr_Smoke3, a
	lda	e
	PutSpriteXAddr	Spr_Smoke3, a
	ret


; move smoke particles in a random direction, 1 pixel at a time. Movement will
; be slow and each particle should move 1 after another, with delays between
; each particles movement. Additionally, update (slightly faster) each smoke
; particle-image such that it changes over time
update_smoke_particles:
	ld	hl, smoke_font
	ld	de, rSmokeTile1
	ld	a, 3; tile right after select-box tile
	; update a progressive row in tile
	call	randomly_alter_tile
	; edit and re-load 2nd smoke particle
	ld	hl, smoke_font
	ld	de, rSmokeTile2
	ld	a, 4; tile right after select-box tile
	call	randomly_alter_tile
	; edit and re-load 3rd smoke particle
	ld	hl, smoke_font
	ld	de, rSmokeTile3
	ld	a, 5; tile right after select-box tile
	call	randomly_alter_tile
	ret
	;jp	move_smoke_particles


; REG:	DE points to ram address for tile to update
;	HL points to mask to apply. This gives tile a consistent shade / shape
;	offset for tile and mask will be calculated on a rolling basis
;	A holds tile # in VRAM onto which this byte-update will apply
; USES:	AF, BC, DE, HL
randomly_alter_tile:
	; tile is made of 16 bytes. change 1 byte of tile each update
	; interestingly, the gameboy uses one bit from both bytes to define a
	; pixel shade. Meaning, if you change 1 byte of a row's word at a time,
	; it'll look more like a shading difference than an actual pixel
	; change. I make use of this by changing every other byte starting with
	; byte 0, then on the 2nd loop change every-other byte starting with
	; byte 1.
	ld	b, a	; preserve tile # in B
	lda	[rSmokeRandByte]
	add	2
	; load a with odd or even starting value to complement the previous
	; loop on which it was operating. Since this loop resets values >= 17,
	; rRandTileByte does not need to be initialized (though it'd be best
	; practice to do so)
	ifa	==, 16, lda 1
	ifa	>=, 17, lda 0
	ld	[rSmokeRandByte], a
	ld	c, a	; preserve pixel # (aka tile offset)
	; add offset to random-tile address
	; HL => HL + A
	add	l
	ld	l, a
	lda	0
	adc	h
	ld	h, a
	; add offset to mask address
	; DE => DE + A
	lda	c	; restore tile offset
	add	e
	ld	e, a
	lda	0
	adc	d
	ld	d, a
	rand_A		; get random #
	and	[hl]	; apply mask
	ld	[de], a	; write masked random byte to tile byte in ram
	lda	b	; restore tile #
	add	a, a	; x2
	add	a, a	; x4
	add	a, a	; x8
	add	a, a	; x16	(for 16 bytes per tile)
	add	c	; add byte offset within tile
	ld	bc, _VRAM
	add	c
	ld	c, a
	lda	0
	adc	b
	ld	b, a	; BC => _VRAM + tile byte address
	lda	[de]	; load byte back into A
	stack_Push	VRAMBytesToLoad, C,B,A
	ret




	ENDC	; end smoke defines
