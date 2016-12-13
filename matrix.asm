;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "syntax.asm"
include "vars.asm"


; max matrix dimension allowed is 255

; create a matrix named `arg1` of Y, X size (arg2 and arg3, respectively)
; the matrix name will also have two other variables created, which simply
; attach _Y and _X to the name of the matrix
mat_Create: MACRO
\1_Y = \2
\1_X = \3
	var_MidRamBytes	\1, (\2 * \3)	; allocate memory for matrix
	; \1 now equals the starting memory address of its matrix
	ENDM


; place the memory address for matrix (arg1) @ Y, X into HL
; calculates it as	Matrix_Address + X + (Y * width)
; Y in register A, X in register E
; because register E doesn't get squashed by math_Mult
; this macro squashes all registers (sorry)
; this can be optimized later. The idea is that the matrix X dimension
; \1_X can tell us if the multiply operation can be optimized
; that can offer speed benefits for an optimized matrix width
mat_AddressYX: MACRO
	load	a, \2
	load	d, 0
	load	e, \3	; \3 is X addr. So we setup DE == $0X
	push	de
	math_Mult	a, \1_X	; calculate Y mem offset (into HL)
	pop	de
	add	hl, de	; add X mem offset to HL
	ld	bc, \1
	add	hl, bc	; add matrix address to HL offset
	ENDM

; usage:
; mat_GetYX	matrix_name, Y, X
; Y is a # or register D
; X is a # or register E
; mat_GetYX squashes all registers (sorry)
; make sure you use the matrix variable name here
mat_GetYX: MACRO
	load	d, \2
	ld	a, d	; swap Y into register A
	mat_AddressYX	\1, a, \3
	ld	a, [HL]	; load matrix@YX into a
	ENDM

; usage:
; mat_SetYX	matrix_name, Y, X, value
; where Y is either a # or register D
; X is either a # or register E
; value is either a # or register A
mat_SetYX: MACRO
	load	a, \4	;this MUST happen first (aka... what if we pass a?)
	load	d, \2
	push af
	ld	a, d	; swap Y into register A
	mat_AddressYX	\1, a, \3
	pop af
	ld	[HL], a
	ENDM


