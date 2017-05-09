;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.asm"
include "test_includes.asm"
include "math.asm"


; call this macro with the two numbers to be multiplied.
; the first two arguments will be loaded into a and c, respectively.
; Then the result in HL will be compared with the computated value of
; the first two arguments multiplied together
; make sure both arguments are less than 256
; an optional 3rd arguments specifies the expected result
math_MultiplyAC_Test: MACRO
	load	a, \1
	load	c, \2
	call	math_MultiplyAC
	IF _NARG == 3
		if_not_hl	\3, .failed_0C
	ELSE
		if_not_hl	\1 * \2, .failed_0C
	ENDC
	ENDM

; test a whole lotta different numbers multiplied together
test_11_math_MultiplyAC:
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
	TestResult	1, 1
.failed_0C
	ld	a, 0	; so that test fails
	TestResult	1, 1


; calls math_Mult macro with first two arguments.
; compare to pre-calculated multiplication, and jump to .failed_0D
; if they are not equal
; an optional 3rd argument allows you to specify the expected result
math_Mult_Test: MACRO
	math_Mult	\1, \2
	IF _NARG == 3
		if_not_hl	\3, .failed_0D
	ELSE
		if_not_hl	\1 * \2, .failed_0D
	ENDC
	ENDM

; test that fast-compute methods work as expected
; 1, 2, 4, 8, 16, 32, 64, 128, 256, 512	; powers of 2
; 3, 5, 6, 9, 10, 12, 17, 18, 20, 24, 33,	; complicated powers of 2
; 34, 36, 40, 48, 65, 66, 68, 72, 80, 96,
; these methods will print to console during compile to notify you
; that "PowerOf2" or "ComplexPowerOf2" fast-compute methods will be used
test_12_math_Mult_Shortcuts:
	math_Mult_Test	50, 1
	math_Mult_Test	50, 2
	math_Mult_Test	50, 4, 200
	math_Mult_Test	50, 8, 400
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
	math_Mult_Test	50, 10, 500
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
	math_Mult_Test	50, 80, 4000
	math_Mult_Test	50, 96
.passed_0D
	ld	a, 1
	TestResult	1, 2
.failed_0D
	ld	a, 0
	TestResult	1, 2



; preload \1 to be divided by \2 and call Divide fxn.
; then verify that result is expected (given rounding down)
; optional 3rd argument to specify expected result
math_Divide_Test: MACRO
	ld	a, \1
	ld	c, \2
	call	math_Divide_A_by_C
	IF _NARG == 3
		ifa	<>, \3, jp .failed_13
	ELSE
		ifa	<>, \1 / \2, jp .failed_13
	ENDC
	ENDM

; divide \1 by \2
; compare to \3  (or if not supplied, (\1) / (\2)
; This differs from math_Divide_Test in that it may use shortcuts
; and may not include a remainder
math_Div_Test: MACRO
	math_Div	\1, \2
	IF _NARG == 3
		ifa	<>, \3, jp .failed_13
	ELSE
		ifa	<>, \1 / \2, jp .failed_13
	ENDC
	ENDM



; test that division fxn behaves as expected. It should divide and
; return an integer result, rounded down
; currently only 8bit division is supported, and the result is in D
test_13_math_Divide_A_by_C:
	math_Divide_Test	25, 5
	math_Divide_Test	30, 6
	math_Divide_Test	40, 8
	math_Divide_Test	5, 1
	math_Divide_Test	1, 5, 0
	math_Divide_Test	15, 8, 1
	math_Divide_Test	16, 8, 2
	math_Divide_Test	255, 2, 127
	math_Divide_Test	255, 1, 255
	math_Divide_Test	255, 240, 1
	math_Divide_Test	240, 250, 0
	; test shortcut division >= 1
	math_Div_Test	8, 8
	math_Div_Test	19, 16
	math_Div_Test	250, 32
	math_Div_Test	250, 64
	math_Div_Test	250, 128
	; test just barely < 2
	math_Div_Test	15, 8
	math_Div_Test	31, 16
	math_Div_Test	63, 32
	math_Div_Test	127, 64
	math_Div_Test	255, 128
	; test optional 3rd argument (result)
	math_Div_Test	32, 16, 2
.passed_13
	ld	a, 1
	TestResult	1, 3
.failed_13
	ld	a, 0
	TestResult	1, 3
