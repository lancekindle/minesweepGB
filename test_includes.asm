;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; this holds files that'll be used by testing procedures
; particularly look at TestResult
	IF	!DEF(TEST_INCLUDES_ASM)
TEST_INCLUDES_ASM	SET	1

include "memory.asm"


get_true:
	ret_true
get_false:
	ret_false

SetRegs:
	ld	a, 1
	ld	b, 2
	ld	c, 3
	ld	d, 4
	ld	e, 5
	ld	h, 6
	ld	l, 7
	ret

TrashRegs:
	ld a, 7
	cp 9
	ld b, 31
	ld c, 13
	ld d, 22
	ld e, 55
	ld h, 66
	ld l, 25
	ret

Letters:  ; using (:) will save ROM address so that you can reference it in code
	DB	"-123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"



;======================= [Tests (newest on bottom) ]=======================
; register A indicates success status.   ==0, fails.    >0, pass
; if the test did NOT pass, a=0
; \1 is actual test #, regardless of pass

; call this macro to essentially print to screen the test ID (if it passed)
; or it will print "-" to the screen if it failed. You must pass one thing
; to this macro: the test # (integer only).
; If Register A > 0, success! your test ID is printed on-screen
; if Register A ==0, Booo! "-" is printed where that test ID should have been
TestResult: MACRO
	; sets background tiles to empty space
	cp	0	; compare a to 0. Z is set if test failed
	ld	hl, Letters + \1
	ld	a, [hl]		; load test letter/# into a
	jr	nz, .passed\@
	ld	hl, Letters	; test failed here, so load "-" into a
	ld	a, [hl]
.passed\@
	ld	hl, _SCRN0 -1 + \1	; set location to test# position
	ld	bc, $0001		; on-screen
	call	mem_SetVRAM
	ret	; cause test function to return
	ENDM




	ENDC	; end defining test_includes_asm
