;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "syntax.asm"
include "vars.asm"
include "math.asm"


	IF !DEF(MATRIX_ASM)
MATRIX_ASM	SET	1
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
mat_AddressYX: MACRO
	load	a, \2, "mat_* arg2 is Y-coordinate"
	load	d, 0
	; \3 is X addr. So we setup DE equal to $0X  (where X is X-coordinate)
	load	e, \3, "mat_* arg3 is X-coordinate"
	push	de
	math_Mult	a, \1_X	; calculate Y mem offset (into HL)
	pop	de
	add	hl, de	; add X mem offset to HL
	load	bc, \1
	add	hl, bc	; add matrix address to HL offset
	ENDM

; usage:
; mat_GetYX	matrix_name, Y, X
; Y is a # or register D
; X is a # or register E
; mat_GetYX squashes all registers (sorry)
; make sure you use the matrix variable name here
mat_GetYX: MACRO
	load	d, \2, "mat_GetYX arg2 is Y-coordinate in matrix"
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
	; load'ing of vars MUST happen first (aka... what if we pass a?)
	load	a, \4, "arg4 of mat_SetYX is value to set in matrix"
	load	d, \2, "mat_SetYX arg2 is Y-coordinate in matrix"
	push af
	ld	a, d	; swap Y into register A
	mat_AddressYX	\1, a, \3
	pop af
	ld	[HL], a		; load value (a) into matrix@Y,X
	ENDM


