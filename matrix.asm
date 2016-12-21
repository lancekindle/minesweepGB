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
; In order to allow treating other ram-space and VRAM as a matrix, I also
; detect if the user is passing an already-defined variable. In that case,
; I append _Y and _X to the variable name, but leave the value of the
; variable unchanged. This allows one to pass in _SCRN0 (with a value of $9800)
; into mat_Create and begin using all the mat_* macros to address _SCRN0 as
; a matrix.
mat_Create: MACRO
\1_Y = \2
\1_X = \3
	IF !DEF(\1)	; this will raise "syntax error" if \1 is just a #
		PRINTT "\nCreating Matrix called \1 with (H, W) = (\2, \3)\n"
		var_MidRamBytes	\1, (\2 * \3)	; allocate memory for matrix
		; \1 now equals the starting memory address of its matrix
	ELSE
		PRINTT "\nCreating Matrix \1 @ "
		PRINTV	\1
		PRINTT	" and (H, W) = (\2, \3)"
		; we actually don't need to do anything else here. Matrix is
		; "set up" in that \1_Y and \1_X are now defined. mat_GetYX
		; and others will now just work.
	ENDC
	ENDM



mat_GetIndex: MACRO
	load	bc, \1, "arg1 of mat_GetIndex is matrix-name or address"
	load	hl, \2, "arg2 of mat_GetIndex is address-offset or HL"
	add	hl, bc		; compute location of matrix @ offset
	ld	a, [HL]	; load value from matrix @ offset into a
	ENDM

; usage:
; mat_GetYX	matrix_name, Y, X
; Y is a # or register D
; X is a # or register E
; mat_GetYX squashes all registers (sorry)
; make sure you use the matrix variable name here
mat_GetYX: MACRO
	load	d, \2, "mat_GetYX arg2 is Y-coordinate in matrix"
	load	e, \3, "mat_GetYX arg3 is X-coordinate in matrix"
	mat_IndexYX	\1, d, e	; place index@YX in HL
	ld	bc, \1	; load base address of matrix
	add	hl, bc	; add base addr of matrix to Index
	ld	a, [HL]	; load matrix@YX into a
	ENDM


; set value @ memory offset specified. the offset must be hard-coded or
; in register HL. Value to set is in register A (or a hard-coded #)
; mat_SetIndex		matrix_name, $00ff, 32
mat_SetIndex: MACRO
	load	bc, \1, "arg1 of mat_SetIndex is matrix-name"
	load	hl, \2, "arg2 of mat_SetIndex is address-offset"
	load	a, \3, "arg3 of mat_SetIndex is value to place in matrix"
	add	hl, bc		; compute location of matrix @ Index
	ld	[HL], a		; load value into matrix @ Index
	ENDM

; usage:
; mat_SetYX	matrix_name, Y, X, value
; where Y is either a # or register D
; X is either a # or register E
; value is either a # or register A
mat_SetYX: MACRO
	; load'ing of vars MUST happen first (aka... what if we pass a?)
	load	bc, \1, "mat_SetYX arg1 is name or base-address of matrix"
	load	d, \2, "mat_SetYX arg2 is Y-coordinate in matrix"
	load	e, \3, "mat_SetYX arg3 is X-coordinate in matrix"
	load	a, \4, "arg4 of mat_SetYX is value to set in matrix"
	push	af
	push	bc
	mat_IndexYX	\1, d, e	; place index@YX in HL
	pop	bc
	add	hl, bc	; add base addr of matrix to Index
	pop	af
	ld	[HL], a		; load value (a) into matrix@Y,X
	ENDM


; use this macro to calculate the address Index. Useful for manipulating
; several matrices at once by pre-calculating the Index / offset
; (And then just adding the address of the matrix itself to begin the
; set/get operation)
; mat_GetAddressIndex	matrix_name, Y, X
; offset will be stored in HL
mat_IndexYX: MACRO
	load	d, \2, "mat_* arg2 is Y-coordinate in matrix"
	; \3 is X addr. So we setup DE == $0X  (X is x-coordinate)
	load	e, \3, "mat_* arg3 is X-coordinate in matrix"
	ld	a, d	; push Y-coordinate into a for multiplication
	ld	d, 0	; DE is now just X-coordinate
	push	de
	math_Mult	a, \1_X	; calculate Y mem offset (into HL)
	pop	de
	add	hl, de	; add X mem offset to HL
	ENDM


	ENDC	; end matrix define
