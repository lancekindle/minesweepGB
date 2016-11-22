; these are methods for joypads taken from gameboy examples

include "gbhw.inc"
include "vars.asm"	; for setting jpad variables

	IF  !DEF(JOYPAD_INC)
JOYPAD_INC  SET     1

; gbhw.inc defines the down, up, left, right, start, select, B, etc. as...
; PADF_DOWN, PADF_UP, PADF_LEFT, PADF_RIGHT, PADF_START, PADF_SELECT, PADF_B..

; define our own joypad variables for keeping track of when a button was last
; pressed
	var_LowRamByte var_DOWN
	var_LowRamByte var_UP
	var_LowRamByte var_LEFT
	var_LowRamByte var_RIGHT
	var_LowRamByte var_START
	var_LowRamByte var_SELECT
	var_LowRamByte var_B
	var_LowRamByte var_A
	var_LowRamByte jpad_rKeys		; holds current value of keys
	var_LowRamByte jpad_rEdge	; holds which buttons were pressed
									; since last time this GetKeys was used

; gets currently pressed keys. Register A will hold keys in the following
; order: MSB --> LSB (Most Significant Bit --> Least Significant Bit)
; Down, Up, Left, Right, Start, Select, B, A
jpad_GetKeys:
	; get action buttons: A, B, Start / Select
	ld	a, JOYPAD_BUTTONS; choose bit that'll give us action button info
	ld	[rJOYPAD], a; write to joypad, telling it we'd like button info
	ld	a, [rJOYPAD]; gameboy will write (back in address) joypad info
	ld	a, [rJOYPAD]
	cpl		; take compliment
	and	$0f	; look at first 4 bits only  (lower nibble)
	swap	a	; place lower nibble into upper nibble
	ld	b, a	; store keys in b
	; get directional keys
	ld	a, JOYPAD_ARROWS
	ld	[rJOYPAD], a ; write to joypad, selecting direction keys
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]	; delay to reliablly read keys
	cpl			; take compliment
	and	$0f		; keep lower nibble
	or	b		; combine action & direction keys (result in a)

	; now calculate edge (which keys have been pressed since last time
	ld	b, a			; store current keys in b
	ld	a, [jpad_rKeys]		; load keys from last time
	xor	a, b
	and	a, b
	ld	[jpad_rEdge], a		; store newly calculated edges
	ld	a, b
	ld	[jpad_rKeys], a		; store keys
	push	af
	
	ld	a, P1F_5|P1F_4
	ld	[rP1], a		; does this... reset joypad???

	ld	a, [jpad_rEdge]
	ld	b, a
	pop af
	; done calculating edges
	ret


; code to wait for keypress (of any kind)
jpad_WaitForKeypress:	MACRO
	push	bc
	ld	b,20
.WaitForKeypress\@
	call	GetKeys
	jr	z,.WaitForKeypress\@
	dec	b
	jr	nz,.WaitForKeypress\@
	pop	bc
	ENDM

; wait until all keys are released. yikes.
jpad_WaitForKeyrelease:	MACRO
	push	bc
	ld	b,20
.WaitForKeyrelease\@
	call	GetKeys
	jr	nz,.WaitForKeyrelease\@
	dec	b
	jr	nz,.WaitForKeyrelease\@
	pop	bc
	ENDM

        ENDC  ; end defining JOYPAD STUFF
