; these are methods for joypads taken from gameboy examples

include "gbhw.inc"

    IF  !DEF(JOYPAD_INC)
JOYPAD_INC  SET     1

; gbhw.inc defines the down, up, left, right, start, select, B, etc. as...
; PADF_DOWN, PADF_UP, PADF_LEFT, PADF_RIGHT, PADF_START, PADF_SELECT, PADF_B


; gets currently pressed keys. Register A will hold keys in the following
; order: A, B, Start, Select, Up, Down, Left, Right  (unsure)
jpad_GetKeys:
	; get action buttons: A, B, Start / Select
	ld a, JOYPAD_BUTTONS		; choose bit that'll give us action button info
	ld [rJOYPAD], a		; write to joypad, telling it we'd like button info
	ld a, [rJOYPAD]		; gameboy will write (back in address) joypad info
	ld a, [rJOYPAD]
	cpl				; take compliment
	and $0f			; look at first 4 bits only  (lower nibble)
	swap a			; place lower nibble into upper nibble
	ld b, a			; store keys in b
	; get directional keys
	ld a, JOYPAD_ARROWS
	ld [rJOYPAD], a ; write to joypad, selecting direction keys
	ld a, [rJOYPAD]
	ld a, [rJOYPAD]
	ld a, [rJOYPAD]
	ld a, [rJOYPAD]
	ld a, [rJOYPAD]
	ld a, [rJOYPAD]		; delay to reliablly read keys
	cpl				; take compliment
	and $0f			; keep lower nibble
	or b			; combine action and direction keys  (result is in a)
	ret

    ENDC  ; end defining JOYPAD STUFF
