;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; Newest Test is on bottom of file

include "test_includes.asm"
include "syntax.asm"



;======================= [Tests (newest on bottom) ]=======================
; register A indicates success status.   ==0, fails.    >0, pass
; if the test did NOT pass, a=0
; \1 is actual test #, regardless of pass



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
	call	SetRegs		; sets a,b,c,d,e,h,l   to    1,2,3,4,5,6,7
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
test_0A_if_flags:
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


; shift_left and shift_right should shift one register into the next,
; in the direction specified. For example, "shift_left a, b" will treat
; a & b as a 16bit register, ab. Then it shifts it left. So the 8th bit
; from b would arrive in the 1st bit in a. The last bit in a is
; discarded. "shift_right a,b" will shift ab to the right. "a" gets
; shifted first, and its 1st bit gets shifted into B's 8th bit-place.
test_0B_shifts:
	ld	a, 0
	ld	b, %10101010
	inc	a	; x= %00000001
	shift_right	a, b
	ifa	>, 0, jr	.failed_0B
	shift_left	a, b
	ifa	<>, 1, jr	.failed_0B
	shift_left	a, b
	ifa	<>, %00000011, jr	.failed_0B
	shift_left	a, b
	ifa	<>, %00000110, jr	.failed_0B
	ld	c, 0
	shift_right	a, c	; a=00000011, c=00000000
	shift_right	a, c	; a=00000001, c=10000000
	shift_right	a, c	; a=00000000, c=11000000
	ifa	>, 0, jr	.failed_0B
	lda	c
	ifa	<>, %11000000, jr	.failed_0B
	TestResult	11	; will print B if a >0
.failed_0B
	ld	a, 0
	TestResult	11	; will print B if a > 0
