;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

; Movement handles the movement of the player. It's responsible for handling
; the joypad and moving (or not moving) the player appropriately depending
; on if the joypad was pressed or held. It's main goal is to make movement
; simple and intuitive such that quickly pressing right will move the player
; the expected number, whereas holding down a direction will initially pause
; (like a keyboard) then repeat.

	IF !DEF(MOVEMENT_ASM)
MOVEMENT_ASM	SET	1

include "vars.asm"
include "sprite.inc"


	; hold onto player coordinates
	var_LowRamByte	rPlayerY
	var_LowRamByte	rPlayerX

	; hold onto last-pressed buttons. If it changes, we want to respond
	; to user input immediately.
	var_LowRamByte	rJPAD_LastButtons
	; how long after buttons-release before button settings reset:
	; Charge, RepeatRate, RepeatInc, etc.
	; (so that user can switch from left to down-left to down seamlessly)
	var_LowRamByte	rJPAD_Cooldown
JPAD_Cooldown_Init	SET	2 ; time of x/60 seconds before button-release
				  ; resets joypad settings
	; triggers buttons on overflow (is set to overflow on first press)
	var_LowRamByte	rJPAD_Charge
	; how fast jpad_charge builds up
	var_LowRamByte	rJPAD_RepeatRate
JPAD_RepeatRate_Init	SET	5
JPAD_MaxRepeatRate	SET	60
; set initial charge to 255 so that any amount added overflows and immediately
; triggers a button press.
JPAD_Charge_Init	SET	255
	; RepeatRate increases by this amount every time jpad_charge overflows
	var_LowRamByte	rJPAD_RepeatInc	
JPAD_RepeatInc_Init	SET	15


move_InitJpadVariables:
	xor	a
	; joypad repeat variables
	ld	a, JPAD_Cooldown_Init
	ld	[rJPAD_Cooldown], a
	ld	a, JPAD_Charge_Init
	ld	[rJPAD_Charge], a
	ld	a, JPAD_RepeatRate_Init
	ld	[rJPAD_RepeatRate], a
	ld	a, JPAD_RepeatInc_Init
	ld	[rJPAD_RepeatInc], a
	ret


; responsible for moving the player within borders. The selection box is
; separate that needs to be called with crosshairs_move_halfway_to_player
; will also control repeat rate of direction(s) if joypad held down.
; each time we trigger a keypress, we increase the rate of repeat
move_player_within_screen_bounds:
	; Only move if NOT on borders
	lda	[jpad_rKeys]
	and	jpad_dpad_mask	; will set result to non-zero if a directional
				; key is active
	if_flag	z, jp .Dpad_not_pressed
.held_key
	; reg. A holds mask for buttons currently pressed
	; increment joypad-press variables and trigger movement if appropriate
	lda	[rJPAD_RepeatRate]
	ld	hl, rJPAD_Charge
	add	a, [hl]	; get new jpad_charge
	ld	[hl], a	; store new jpad_charge
	if_flag	nc, jp .skip_button_press
	; if we get here, button is pressed and we need to trigger it
.increase_repeat_rate
	ld	hl, rJPAD_RepeatRate
	lda	[rJPAD_RepeatInc]
	add	a, [hl]
	ifa	>, JPAD_MaxRepeatRate, ld a, JPAD_MaxRepeatRate
	ld	[hl], a	; increase repeat rate each time we trigger a key-press
	; make sure cooldown is reset once a key gets pressed
	ld	hl, rJPAD_Cooldown
	ld	[hl], JPAD_Cooldown_Init
.move_player
	ld	a, [rPlayerX]
	push	af	; store X value for later
	ifa	<, SCRN_X - 8,	call move_if_right
	pop	af
	ifa	>, 0,		call move_if_left
	ld	a, [rPlayerY]
	push	af
	ifa	<, SCRN_Y - 8,	call move_if_down
	pop	af
	ifa	>, 0,		call move_if_up
	jr	.done
.Dpad_not_pressed
	; cooldown variable allows a brief lapse between button presses while
	; still maintaining the same repeat rate. If cooldown expires, we reset
	ld	hl, rJPAD_Cooldown
	dec	[hl]	; sets 0-flag if cooldown == 0
	jr	nz, .done
	; if cooldown == 0, reset jpad vars
.reset_jpad_variables
	ld	hl, rJPAD_Charge
	ld	[hl], JPAD_Charge_Init
	ld	hl, rJPAD_RepeatRate
	ld	[hl], JPAD_RepeatRate_Init
	ld	hl, rJPAD_RepeatInc
	ld	[hl], JPAD_RepeatInc_Init
	jr	.done
.skip_button_press
	; we get here if buttons are pressed, but aren't triggering a repeat
	; so lets check if there's a newly-pressed button and trigger that
	; (if it does trigger, it'll reset the jpad charge to ensure we don't
	; quickly repeat a key-press)
	lda	[jpad_rEdge]
	and	jpad_dpad_mask	; sets zero-flag if no newly pressed dpad keys
	jp	nz, move_player_direction_just_pressed
	; we get here if buttons are held down with no changes
.done
	ret


; moves player direction just pressed. AND resets
; jpad charge so that the key can't get instantly triggered (Again) the next
; vblank
move_player_direction_just_pressed:
	xor	a
	; set charge to 0 so that it'll wait the full time before repeating
	ld	[rJPAD_Charge], a
	; now move player according to newly-pressed direction
	ld	a, [rPlayerX]
	push	af	; store X value for later
	ifa	<, SCRN_X - 8,	call move_once_if_right
	pop	af
	ifa	>, 0,		call move_once_if_left
	ld	a, [rPlayerY]
	push	af
	ifa	<, SCRN_Y - 8,	call move_once_if_down
	pop	af
	ifa	>, 0,		call move_once_if_up
	ret


move_if_right:
	if_not	jpad_ActiveRight, ret
	ld	a, [rPlayerX]
	add	8
	ld	[rPlayerX], a
	ret

move_if_left:
	if_not	jpad_ActiveLeft, ret
	ld	a, [rPlayerX]
	sub	8
	ld	[rPlayerX], a
	ret

move_if_up:
	if_not	jpad_ActiveUp, ret
	ld	a, [rPlayerY]
	sub	8
	ld	[rPlayerY], a
	ret

move_if_down:
	if_not	jpad_ActiveDown, ret
	ld	a, [rPlayerY]
	add	8
	ld	[rPlayerY], a
	ret

move_once_if_right:
	if_not	jpad_EdgeRight, ret
	ld	a, [rPlayerX]
	add	8
	ld	[rPlayerX], a
	ret

move_once_if_left:
	if_not	jpad_EdgeLeft, ret
	ld	a, [rPlayerX]
	sub	8
	ld	[rPlayerX], a
	ret

move_once_if_up:
	if_not	jpad_EdgeUp, ret
	ld	a, [rPlayerY]
	sub	8
	ld	[rPlayerY], a
	ret

move_once_if_down:
	if_not	jpad_EdgeDown, ret
	ld	a, [rPlayerY]
	add	8
	ld	[rPlayerY], a
	ret


; moves BC (source) halfway to the DE (desination). B,C, are coordinates
; respectively moving towards D,E. I usually put Y in B, D and X in C, E
; but it doesn't matter, so long as DE contains the destination coordinates.
; If called repeatedly, this will ensure that BC eventually matches DE.
; USES: AF, BC, DE
move_BC_halfway_to_DE:
	lda	d
	sub	b	; calculate Destination_B - source_B
	jp	z, .update_C	; skip updating B if it's already equal
	jp	c, .negativeB	; handle case where source_B > Destination_B
	; regiter A is positive here. We add half that to source_B
	srl	a	; divide offset by 2. Sets Zero-flag if a==0
	; if A=0 now, then before A / 2 (srl A) it was 1. Reload 1 so that
	; source will reach destination
	if_flag	z, ld	a, 1
	jp .add_half_B_offset
.negativeB
	sra	a	; divide by 2 (for signed/negative numbers)
	; unlike unsigned #s, -1 never goes away when divided by 2. $FF stays
	; $FF when using sra (which keeps the same bit 7)
	; so it's correct to use the result of "sra a" as the offset
.add_half_B_offset
	add	b	; apply offset to source_B
	ld	b, a	; B now contains updated coordinate
.update_C
	lda	e
	sub	c	; calculate Destination_C - source_C
	ret 	z	; skip updating C if it's already equal
	jp 	c, .negativeC	; handle case where source_C > Destination_C
	srl	a	; divide offset by 2
	; if A=0, then our offset was one. Load 1 as offset
	if_flag	z, ld	a, 1
	jp .add_half_C_offset
.negativeC
	sra	a	; divide by 2 (for signed/negative numbers)
.add_half_C_offset
	add	c	; apply offset to crosshair_x
	ld	c, a	; C now contains updated coordinate
	ret


; moves BC (source) one-quarter of the way to the DE (desination).
; B,C, are coordinates respectively moving towards D,E.
; I usually put Y in B, D and X in C, E but it doesn't matter, so long
; as DE contains the destination coordinates.
; once distance between coordinates <=3, the distance will change by 1
; each time.
; If called repeatedly, this will ensure that BC eventually matches DE.
; USES: AF, BC, DE
move_BC_quarterway_to_DE:
	lda	d
	sub	b	; calculate Destination_B - source_B
	jp	z, .update_C	; skip updating B if it's already equal
	jp	c, .negativeB	; handle case where source_B > Destination_B
	; regiter A is positive here. We add half that to source_B
	srl	a
	srl	a	; divide offset by 4
	; if A=0 now, then before A / 4 (2x'srl A') it was <=3. Reload 1 so
	; that source will reach destination in <=3 calls
	if_flag	z, ld	a, 1
	jp .add_half_B_offset
.negativeB
	sra	a
	sra	a	; divide by 4 (for signed/negative numbers)
	; unlike unsigned #s, -1 never goes away when divided by 2. $FF stays
	; $FF when using sra (which keeps the same bit 7)
	; so it's correct to use the result of "sra a" as the offset
.add_half_B_offset
	add	b	; apply offset to source_B
	ld	b, a	; B now contains updated coordinate
.update_C
	lda	e
	sub	c	; calculate Destination_C - source_C
	ret 	z	; skip updating C if it's already equal
	jp 	c, .negativeC	; handle case where source_C > Destination_C
	srl	a
	srl	a	; divide offset by 4
	; if A=0, then our offset was one. Load 1 as offset
	if_flag	z, ld	a, 1
	jp .add_half_C_offset
.negativeC
	sra	a
	sra	a	; divide by 4 (for signed/negative numbers)
	; (the smallest a negative number can get when calling "SRA A" is
	;  -1, so we don't need to perform any checks on # post-division)
.add_half_C_offset
	add	c	; apply offset to crosshair_x
	ld	c, a	; C now contains updated coordinate
	ret


; moves BC (source) 1/16th of the way to the DE (desination).
; B,C, are coordinates respectively moving towards D,E.
; I usually put Y in B, D and X in C, E but it doesn't matter, so long
; as DE contains the destination coordinates.
; once distance between coordinates <=3, the distance will change by 1
; each time.
; If called repeatedly, this will ensure that BC eventually matches DE.
; USES: AF, BC, DE
move_BC_sixteenth_to_DE:
	lda	d
	sub	b	; calculate Destination_B - source_B
	jp	z, .update_C	; skip updating B if it's already equal
	jp	c, .negativeB	; handle case where source_B > Destination_B
	; regiter A is positive here. We add half that to source_B
	srl	a
	srl	a	; divide offset by 4
	srl	a
	srl	a	; / 16
	; if A=0 now, then before A / 4 (2x'srl A') it was <=3. Reload 1 so
	; that source will reach destination in <=3 calls
	if_flag	z, ld	a, 1
	jp .add_B_offset
.negativeB
	sra	a
	sra	a	; divide by 4 (for signed/negative numbers)
	sra	a
	sra	a
	; unlike unsigned #s, -1 never goes away when divided by 2. $FF stays
	; $FF when using sra (which keeps the same bit 7)
	; so it's correct to use the result of "sra a" as the offset
.add_B_offset
	add	b	; apply offset to source_B
	ld	b, a	; B now contains updated coordinate
.update_C
	lda	e
	sub	c	; calculate Destination_C - source_C
	ret 	z	; skip updating C if it's already equal
	jp 	c, .negativeC	; handle case where source_C > Destination_C
	srl	a
	srl	a	; divide offset by 4
	srl	a
	srl	a
	; if A=0, then our offset was one. Load 1 as offset
	if_flag	z, ld	a, 1
	jp .add_C_offset
.negativeC
	sra	a
	sra	a	; divide by 4 (for signed/negative numbers)
	sra	a
	sra	a
	; (the smallest a negative number can get when calling "SRA A" is
	;  -1, so we don't need to perform any checks on # post-division)
.add_C_offset
	add	c	; apply offset to crosshair_x
	ld	c, a	; C now contains updated coordinate
	ret




	ENDC	; end movement of player
