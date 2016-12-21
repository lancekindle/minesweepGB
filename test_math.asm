;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.asm"
include "test_includes.asm"
include "math.asm"


; call this macro with # to compare, and 
; jump address that'll get used IF HL != #  (which is loaded in BC)
if_not_hl: MACRO
	ld	bc, \1
	ld	a, b
	cp	h	; compare B to H, and jump if they don't equal
	jp	nz, \2
	ld	a, c
	cp	l	; compare C to L and jump if not equal
	jp	nz, \2
	ENDM

; call this macro with the two numbers to be multiplied.
; the first two arguments will be loaded into a and c, respectively.
; Then the result in HL will be compared with the computated value of
; the first two arguments multiplied together
; make sure both arguments are less than 256
math_MultiplyAC_Test: MACRO
	load	a, \1
	load	c, \2
	call	math_MultiplyAC
	if_not_hl	\1 * \2, .failed_0C
	ENDM

; test a whole lotta different numbers multiplied together
test_0C_math_MultiplyAC:
	math_MultiplyAC_Test	8, 8
	math_MultiplyAC_Test	25, 25
	math_MultiplyAC_Test	199, 17
	math_MultiplyAC_Test	1, 3
	math_MultiplyAC_Test	0, 3
	math_MultiplyAC_Test	5, 0
	math_MultiplyAC_Test	127, 40
	math_MultiplyAC_Test	255, 255
.passed_0C
	ld	a, 1	; so that test passes
	TestResult	12
.failed_0C
	ld	a, 0	; so that test fails
	TestResult	12


; calls math_Mult macro with first two arguments.
; compare to pre-calculated multiplication, and jump to .failed_0D
; if they are not equal
math_Mult_Test: MACRO
	math_Mult	\1, \2
	if_not_hl	\1 * \2, .failed_0D
	ENDM

; test that fast-compute methods work as expected
; 1, 2, 4, 8, 16, 32, 64, 128, 256, 512	; powers of 2
; 3, 5, 6, 9, 10, 12, 17, 18, 20, 24, 33,	; complicated powers of 2
; 34, 36, 40, 48, 65, 66, 68, 72, 80, 96,
; these methods will print to console during compile to notify you
; that "PowerOf2" or "ComplexPowerOf2" fast-compute methods will be used
test_0D_math_Mult_Shortcuts:
	math_Mult_Test	50, 1
	math_Mult_Test	50, 2
	math_Mult_Test	50, 4
	math_Mult_Test	50, 8
	math_Mult_Test	50, 16
	math_Mult_Test	50, 32
	math_Mult_Test	50, 64
	math_Mult_Test	50, 128
	math_Mult_Test	50, 256
	math_Mult_Test	50, 512
	; done with powers of 2, now to use complicated (But still fast)
	; multiplication shortcuts
	math_Mult_Test	50, 3
	math_Mult_Test	50, 5
	math_Mult_Test	50, 6
	math_Mult_Test	50, 9
	math_Mult_Test	50, 10
	math_Mult_Test	50, 12
	math_Mult_Test	50, 17
	math_Mult_Test	50, 18
	math_Mult_Test	50, 20
	math_Mult_Test	50, 24
	math_Mult_Test	50, 33
	math_Mult_Test	50, 34
	math_Mult_Test	50, 36
	math_Mult_Test	50, 40
	math_Mult_Test	50, 48
	math_Mult_Test	50, 65
	math_Mult_Test	50, 66
	math_Mult_Test	50, 68
	math_Mult_Test	50, 72
	math_Mult_Test	50, 80
	math_Mult_Test	50, 96
.passed_0D
	ld	a, 1
	TestResult	13
.failed_0D
	ld	a, 0
	TestResult	13
