;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; Newest Test is on bottom of file

include "test_includes.asm"
include "syntax.inc"



;======================= [Tests (newest on bottom) ]=======================
; register A indicates success status.   ==0, fails.    >0, pass
; if the test did NOT pass, a=0
; \1 is actual test #, regardless of pass



; test lda
test_01_lda:
	ld	a, 0
	lda	1  ; test if macro will load a with 1
	TestResult	1

test_02_arg_unpacking:	; don't have a test for this yet

test_03_ldpair:
	ld	BC, $0101
	ld	DE, $BBCC
	ldpair	bc, de
	; bc should contain $ABCD
	ld	a, b
	cp	$BB
	jp	nz, .test_03_fail
	ld	a, c
	cp	$CC
	jr	nz, .test_03_fail
.test_03_pass
	ld	a, 1
	jp	.test_03_end
.test_03_fail
	ld	a, 0
.test_03_end
	TestResult	3

; test the if_ syntax
test_04_if:
	ld	a, 0
	if_	get_true, ld a, 1
	if_	get_false, ld a, 0
	TestResult	4

; test the if_not syntax
test_05_if_not:
	ld	a, 0
	if_not	get_false, ld a, 1
	if_not	get_true, ld a, 0
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


; test true/false macros for setting CY and returning
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
	TestPassed	8
.failed08
	TestFailed	8

; test ifa macro (which compares numbers to register a)
test_09_ifa:
	; test failing matches first
	lda	1
	ifa	>=, 2, jp .failed 
	ifa	<=, 0, jp .failed
	ifa	<, 1, jp .failed
	ifa	>, 1, jp .failed
	ifa	==, 0, jp .failed
	ifa	==, 2, jp .failed
	ifa	<>, 1, jp .failed
	ifa	!=, 1, jp .failed
	; now test positive matches
	lda	5
.ifa0
	ifa	>=, 4, jp .ifa1		; test >  (part of >=)
	jp	.failed
.ifa1
	ifa	>=, 5, jp .ifa2		; test =  (part of >=)
	jp	.failed
.ifa2
	ifa	<=, 6, jp .ifa3		; test <  (part of <=)
	jp	.failed
.ifa3
	ifa	<=, 5, jp .ifa4		; test =  (part of <=)
	jp	.failed
.ifa4
	ifa	>, 4, jp .ifa5		; test > (but only >)
	jp	.failed
.ifa5
	ifa	<, 6, jp .ifa6		; test < (but only <)
	jp	.failed
.ifa6
	ifa	==, 5, jp .ifa7		; test ==
	jp	.failed
.ifa7
	ifa	<>, 6, jp .ifa8		; test <>
	jp	.failed
.ifa8
	ifa	!=, 4, jp .ifa9		; test !=
	jp	.failed
.ifa9
	TestPassed	9
.failed
	TestFailed	9

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
	TestPassed	10
.failed_0A
	TestFailed	10


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
	TestPassed	11
.failed_0B
	TestFailed	11	; will print B if a > 0


; test increment and decrement. Pretty obvious. This macro is designed to
; work only on hl
test_increment: MACRO
	ld	hl, \1
	increment	hl
	IF _NARG == 2
		if_not_hl	\2, .failed
	ELSE
		if_not_hl	\1 + 1, .failed
	ENDC
	ENDM

test_decrement: MACRO
	ld	hl, \1
	decrement	hl
	IF _NARG == 2
		if_not_hl	\2, .failed
	ELSE
		if_not_hl	\1 - 1, .failed
	ENDC
	ENDM

; test and verify that increment / decrement correctly rolls over the
; under/overflow into the MSB (more significant byte)
test_0C_increment_decrement:
	ld	bc, $00FF
	increment	bc
	ld	h, b
	ld	l, c	; move bc to hl
	if_not_hl	$0100, .failed
	ld	de, $0FFF
	increment	de
	ld	h, d
	ld	l, c	; move de to hl
	if_not_hl	$1000, .failed
	test_increment	$00FE, $00FF
	test_increment	$F0FF, $F100
	test_increment	$FFFF, $0000
	test_increment	$2525, $2526
	test_increment	$34FF, $3500
	test_increment	$0000, $0001
	test_increment	$0002, $0003
	; test decrements
	ld	bc, $0100
	decrement	bc
	ld	h, b
	ld	l, c	; move bc to hl
	if_not_hl	$00FF, .failed
	ld	de, $1000
	decrement	de
	ld	h, d
	ld	l, c	; move de to hl
	if_not_hl	$0FFF, .failed
	test_decrement	$0000, $FFFF
	test_decrement	$FF00, $FEFF
	test_decrement	$0025
	test_decrement	$0026
	test_decrement	$F3A9
	test_decrement	$FE10
	test_decrement	$0001, $0000
.passed_0C
	TestPassed	12
.failed
	TestFailed	12


; test that ifa_not works as expected.
test_0D_ifa_not:
	; test failing matches first (where A IS >=2, but we're testing that
	; ifa_not will reverse that logic)
	jp .skip
.skip
	lda	1
	ifa_not	<=, 2, jp .failed
	ifa_not	>=, 0, jp .failed
	ifa_not <=, 1, jp .failed
	ifa_not	>=, 1, jp .failed
	ifa_not	<, 2, jp .failed
	ifa_not	>, 0, jp .failed
	ifa_not	==, 1, jp .failed
	ifa_not	<>, 2, jp .failed
	ifa_not	<>, 0, jp .failed
	ifa_not	!=, 2, jp .failed
	; now test positive matches (where A ISN'T >=2, but we're testing
	; that ifa_not will reverse that logic)
	lda	5
.ifa0
	ifa_not	<=, 4, jp .ifa1		; test >
	jp	.failed
.ifa1
	ifa_not	>=, 6, jp .ifa2		; test >=
	jp	.failed
.ifa2
	ifa_not	>, 5, jp .ifa3		; test >
	jp	.failed
.ifa3
	ifa_not	<>, 5, jp .ifa4		; test ==
	jp	.failed
.ifa4
	ifa_not	>, 6, jp .ifa5		; test >
	jp	.failed
.ifa5
	ifa_not	<, 5, jp .ifa6		; test <
	jp	.failed
.ifa6
	ifa_not	<, 4, jp .ifa7		; test <
	jp	.failed
.ifa7
	ifa_not	!=, 5, jp .passed_0D
.failed
	TestFailed	13
.passed_0D
	TestPassed	13


; call with register, and value to load then negate
; test_negate	a, 5   --OR--   test_negate	bc, $F0C2
test_negate: MACRO
	IF STRLEN("\1") == 1
		ld	\1, \2
		negate	\1
		ld	a, \1
		ifa	<>, \3, jp .failed_0E
		PRINTT	"\1"
	ELSE
		IF STRIN("BCDEHL",STRUPR("\1")) == 0
			FAIL	"\1 needs to be a register pair"
		ENDC
		; assume it's a register pair
		ld	\1, \2
		negate	\1
		; move negated register pair to DE
		IF STRCMP("HL",STRUPR("\1")) == 0
			ldpair	de, hl
		ENDC
		IF STRCMP("BC",STRUPR("\1")) == 0
			ldpair	de, bc
		ENDC
		; we'll store \3 in HL
		ld	hl, \3
		; now DE, HL should be equal
		ld	a, d
		ifa	<>,h, jp .failed_0E
		ld	a, e
		ifa	<>,l, jp .failed_0E
	ENDC
	ENDM

; test that negate returns -register or -register_pair
; also verify that "trash AF" will force the macro to skip preserving A
test_0E_negate:
	test_negate	a, 4, -4
	test_negate	b, 0, 0
	test_negate	c, 8, -8
	test_negate	d, 99, -99
	;test_negate	e, 255, -255	; signed 8bit range: $7F, -$80
	test_negate	h, 127, -127	; aka 		     127, -128
	test_negate	l, 67, -67
	; test negate register pairs
	test_negate	bc, $0A38, -$0A38
	test_negate	DE, $0000, -$0000
	;test_negate	HL, $FFFF, -$FFFF  ; signed 16bit range: $7FFF, -$8000
	test_negate	HL, $7FFF, -$7FFF  ; aka		 32767, -32768
	; test "trash AF" argument
	ld	a, 0
	ld	b, 88
	negate	b, trash AF
	ifa	==, 0, jp .failed_0E	; a should not be preserved
	ld	a, 0
	negate	b
	ifa	<>, 0, jp .failed_0E	; a is preserved
	; test "trash AF" arg when negating register pair
	ld	a, 0
	ld	hl, $F92F
	negate	hl, trash AF
	ifa	==, 0, jp .failed_0E
	ld	a, 0
	negate	hl
	ifa	<>, 0, jp .failed_0E
.passed_0E
	TestPassed	14
.failed_0E
	TestFailed	14

