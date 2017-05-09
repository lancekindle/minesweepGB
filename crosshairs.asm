;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

; handle movement of the crosshairs. They visually indicate whch cell
; the player is targetting. To make movement smooth, the crosshairs do
; NOT mirror player-x,y but instead move half-way to the player-x,y each
; time crosshairs_update is called. This makes player movement appear
; smooth. crosshairs_update should be done once per Vblank


include "vars.asm"
include "sprite.inc"
include "dma.inc"

	IF !DEF(CROSSHAIRS_ASM)
CROSSHAIRS_ASM	SET	1
	var_LowRamByte	rCrosshairY
	var_LowRamByte	rCrosshairX
	; declare sprites for crosshairs
	SpriteAttr	Spr_UpperLeft
	SpriteAttr	Spr_UpperRight
	SpriteAttr	Spr_LowerLeft
	SpriteAttr	Spr_LowerRight


; initialize crosshairs in OAM
crosshairs_setup:
	; set crosshairs to tile#2
	ld	a, 2
	sprite_PutTile	Spr_UpperLeft, a
	sprite_PutTile	Spr_LowerLeft, a
	sprite_PutTile	Spr_UpperRight, a
	sprite_PutTile	Spr_LowerRight, a
	; flip crosshairs so they all have a corresponding orientation
	ld	a, %00000000
	sprite_PutFlags	Spr_UpperLeft, a
	set	5, a	; set horizontal flip flag
	sprite_PutFlags	Spr_UpperRight, a
	set	6, a	; set vertical flip flag
	sprite_PutFlags	Spr_LowerRight, a
	res	5, a	; undo horizontal flip
	sprite_PutFlags	Spr_LowerLeft, a
	; push sprite positions to vram
	call	crosshairs_update
	call	DMACODELOC
	ret


; update crosshairs indicating player origin
; player's coordinates are on top-left of cell
; places crosshairs around the location
; USES: AF
crosshairs_update:
	call	crosshairs_move_halfway_to_player
	ld	a, [rCrosshairY]
	push	af
	sub	3
	PutSpriteYAddr	Spr_UpperLeft, a
	PutSpriteYAddr	Spr_UpperRight, a
	pop	af
	add	2
	PutSpriteYAddr	Spr_LowerLeft, a
	PutSpriteYAddr	Spr_LowerRight, a
	; update X position
	ld	a, [rCrosshairX]
	push	af
	sub	2
	PutSpriteXAddr	Spr_UpperLeft, a
	PutSpriteXAddr	Spr_LowerLeft, a
	pop	af
	add	3
	PutSpriteXAddr	Spr_UpperRight, a
	PutSpriteXAddr	Spr_LowerRight, a
	ret


; moves the crosshairs halfway to the player. Each call moves it halfway closer
; until it's fully moved into the player's position
; barely perceptable, but it makes movement feel so smooth. mmmmmm yeah.
; try rocking back and forth to see what I mean
; USES: AF, B
crosshairs_move_halfway_to_player:
	; uncomment these lines to see the instant 'jerky' moves
;	ld	a, [rPlayerY]
;	ld	[rCrosshairY], a
;	ld	a, [rPlayerX]
;	ld	[rCrosshairX], a
	; update Y position
	ld	a, [rCrosshairY]
	ld	b, a
	ld	a, [rPlayerY]
	sub	b	; calculate player_y - crosshar_y
	jp	z, .update_X	; skip updating Y if it's already equal
	jp	c, .negativeY	; handle case where cross_Y > player_Y
	; A is positive here. We add half that to crosshair_y
	srl	a	; divide offset by 2
	; if A=0 now, then before A / 2 (srl A) it was 1. Reload 1 so that
	; crosshairs will reach destination
	if_flag	z, ld	a, 1
	jp .add_half_y_offset
.negativeY
	sra	a	; divide by 2 (for signed/negative numbers)
	; unlike unsigned #s, -1 never goes away when divided by 2. $FF stays
	; $FF when using sra (which keeps the same bit 7)
	; so it's totally cool to use the exact result as the offset
.add_half_y_offset
	add	b	; apply offset to crosshair_y
	ld	[rCrosshairY], a
	; update X position
.update_X
	ld	a, [rCrosshairX]
	ld	b, a
	ld	a, [rPlayerX]
	sub	b	; calculate player_X - crosshar_X
	ret 	z	; skip updating X if it's already equal
	jp 	c, .negativeX	; handle case where cross_X > player_X
	srl	a	; divide offset by 2
	; if A=0, then our offset was one. Load 1 as offset
	if_flag	z, ld	a, 1
	jp .add_half_x_offset
.negativeX
	sra	a	; divide by 2 (for signed/negative numbers)
.add_half_x_offset
	add	b	; apply offset to crosshair_x
	ld	[rCrosshairX], a
	ret


; call this to "disappear" the crosshairs by setting the sprite to " " (blank)
; and setting their coordinates to (0,0) offscreen
; NOTE: This won't be permanent. It'll be invisible, but if you continue
; updating positions of the crosshairs, those sprites will invisibly
; return to the player's position
crosshairs_disappear:
	; position at 0,0
	xor	a
	ld	[rCrosshairX], a
	ld	[rCrosshairY], a	; set crosshairs at 0,0
	ld	[Spr_UpperLeft_XAddr], a
	ld	[Spr_UpperLeft_YAddr], a
	ld	[Spr_UpperRight_XAddr], a
	ld	[Spr_UpperRight_YAddr], a
	ld	[Spr_LowerLeft_XAddr], a
	ld	[Spr_LowerLeft_YAddr], a
	ld	[Spr_LowerRight_XAddr], a	; set all 4 sprites to 0,0
	ld	[Spr_LowerRight_YAddr], a	; (which puts it offscreen)
	; change graphic to space
	lda	" "
	sprite_PutTile	Spr_UpperLeft, a
	sprite_PutTile	Spr_UpperRight, a
	sprite_PutTile	Spr_LowerLeft, a
	sprite_PutTile	Spr_LowerRight, a
	ret


	ENDC	; end defining crosshairs
