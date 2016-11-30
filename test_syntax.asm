;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; Newest Test is on bottom of file
include "gbhw.inc"
include "ibmpc1.inc"

section "Vblank", HOME[$0040]
	reti
section "LCDC", HOME[$0048]
	reti
section "Timer_Overflow", HOME[$0050]
	reti
section "Serial", HOME[$0058]
	reti
section "joypad_p1_p4", HOME[$0060]
	reti
section "start", HOME[$0100]
	nop
	jp begin

	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT

include "joypad.asm"
include "memory.asm"
include "lcd.asm"
include "syntax.asm"

begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	call	lcd_Stop
	call	lcd_ScreenInit	; set up pallete and (x,y)=(0,0)
	call	LoadFont
	call	ClearBackground
	call	lcd_On
	call	lcd_ShowBackground
	call	test_01_lda
	call	test_02_preserve
	call	test_03_preserve2
	call	test_04_if
	call	test_05_if_not
	call	test_06_if_flag
	call	test_07_if_not_flag
	call	test_08_truefalse
	call	test_09_ifa
	call	test_0A_test_flags_testing
.mainloop:
	halt
	nop
	jr	.mainloop
	


ClearBackground:
	; sets background tiles to empty space
	ld	a, 32
	ld	hl, _SCRN0
	ld	bc, SCRN_VX_B * SCRN_VY_B
	call	mem_SetVRAM
	ret


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



; makes use of include "ibmpc1.inc"
ASCII_TILES_LOC:
	chr_IBMPC1 1,8  ; arguments 1,8 cause all 256 characters to be loaded
ASCII_TILES_END:

LoadFont:
	ld	hl, ASCII_TILES_LOC
	ld	de, _VRAM
	ld	bc, ASCII_TILES_END - ASCII_TILES_LOC
	call	mem_CopyMono  ; copy a Monochrome font to ram. (our is monochrome?)
	ret


Letters:  ; using (:) will save ROM address so that you can reference it in code
	DB	"-123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"





;======================= [Tests (newest on bottom) ]=======================
; a indicates success. ==0, fails. >0, pass
; if the test did NOT pass, a=0
; \1 is actual test #, regardless of pass
TestResult: MACRO
	; sets background tiles to empty space
	cp	0	; compare a to 0. Z is set if test failed
	ld	hl, Letters + \1
	ld	a, [hl]
	jr	nz, .passed\@
	ld	hl, Letters
	ld	a, [hl]
.passed\@
	ld	hl, _SCRN0 -1 + \1
	ld	bc, $0001
	call	mem_SetVRAM
	ret	; cause test function to return
	ENDM


; test lda
test_01_lda:
	ld	a, 0
	lda	1  ; test if macro will load a with 1
	TestResult	1

; test preserve2
test_03_preserve2:
	ld	a, 1
	ld	b, 3
	preserve2	af, bc, call TrashRegs
	cp	1
	jp	nz, .failed03
	lda	b
	cp 3
	jp	nz, .failed03
	TestResult	3
.failed03
	lda	0
	TestResult	3

;test preserve
test_02_preserve:
	ld	a, 1
	preserve	af, ld a, 0
	cp	1
	jp	nz, .failed02
	call	SetRegs
	preserve	all, call TrashRegs
	cp	1  ; verify a
	jr	nz, .failed02
	lda	b
	cp	2 ; verify b
	jr	nz, .failed02
	lda	c
	cp	3 ; verify c
	jr	nz, .failed02
	lda	d
	cp	4 ; verify d
	jr	nz, .failed02
	lda	e
	cp	5 ; verify e
	jr	nz, .failed02
	lda	h
	cp	6 ; verify h
	jr	nz, .failed02
	lda	l
	cp	7 ; verify l
	jr	nz, .failed02
	TestResult	2
.failed02
	lda	0
	TestResult	2


; test the if_ syntax
test_04_if:
	ld	a, 0
	if_	get_true, ld a, 1
	if_	get_false, ld a, 0
	if_	get_true, preserve af, preserve bc, call TrashRegs  ; test arg unpacking
	TestResult	4

; test the if_not syntax
test_05_if_not:
	ld	a, 0
	if_not	get_false, ld a, 1
	if_not	get_true, ld a, 0
	if_not	get_false, preserve af, preserve bc, call TrashRegs  ; test arg unpacking
	TestResult	5


; test the if_flag syntax
test_06_if_flag:
	lda	0
	cp	0
	if_flag	z, lda 2
	cp	1
	if_flag	c, lda 0
	if_flag	z, lda 0
	TestResult	6

; test the if_not_flag syntax
test_07_if_not_flag:
	lda	0
	cp	0
	if_not_flag	c, lda 2
	cp	3
	if_not_flag	c, lda 0
	cp	2
	if_not_flag	z, lda 0
	TestResult	7


; test true/false macros for setting and returning
test_08_truefalse:
	ld	a, 1
	ld	hl, .tf1
	push	hl
	ret_true
.tf1
	jr	nc, .failed08
	ld	hl, .tf2
	push	hl
	ret_false
.tf2
	jr	c, .failed08
	TestResult	8
.failed08
	ld	a, 0
	TestResult	8

; test ifa macro (which compares numbers to register a)
test_09_ifa:
	lda	1
	ifa	>=, 2, jp .failed_09 
	ifa	<=, 0, jp .failed_09
	ifa	<, 1, jp .failed_09
	ifa	>, 1, jp .failed_09
	ifa	==, 0, jp .failed_09
	ifa	<>, 1, jp .failed_09
	ifa	!=, 1, jp .failed_09
	; now test positive matches
	lda	5
.ifa0
	ifa	>=, 4, jp .ifa1		; test >  (part of >=)
	jp	.failed_09
.ifa1
	ifa	>=, 5, jp .ifa2		; test =  (part of >=)
	jp	.failed_09
.ifa2
	ifa	<=, 6, jp .ifa3		; test <  (part of <=)
	jp	.failed_09
.ifa3
	ifa	<=, 5, jp .ifa4		; test =  (part of <=)
	jp	.failed_09
.ifa4
	ifa	>, 4, jp .ifa5		; test > (but only >)
	jp	.failed_09
.ifa5
	ifa	<, 6, jp .ifa6		; test < (but only <)
	jp	.failed_09
.ifa6
	ifa	==, 5, jp .ifa7		; test ==
	jp	.failed_09
.ifa7
	ifa	<>, 6, jp .ifa8		; test <>
	jp	.failed_09
.ifa8
	ifa	!=, 4, jp .ifa9		; test !=
	jp	.failed_09
.ifa9
	TestResult	9
.failed_09
	lda	0
	TestResult	9

; test if_flags and if_not_flags syntax
test_0A_test_flags_testing:
	lda	5
	cp	5	; C=0, Z=1
	if_flags	nc, nz, jp .failed_0A
	if_flags	c, z, jp .failed_0A
	SCF	; set carry flag (c=1 and z=1)
	if_flags	c, nz, jp .failed_0A
	if_flags	z, c, jp .ifflags0
	jp .failed_0A
.ifflags0
	lda	5
	cp	5	; C=0, Z=1
	if_not_flags	c, z, jp .failed_0A
	if_not_flags	nz, nc, jp .failed_0A
	if_not_flags	z, z, jp .failed_0A
	SCF	; set carry flag (now C=1 and Z=1)
	if_not_flags	c, z, jp .failed_0A
	if_not_flags	nc, nz, jp .ifflags1
	jp .failed_0A
.ifflags1
	TestResult	10
.failed_0A
	lda	0
	TestResult	10



