;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.asm"
include "test_includes.asm"
include "matrix.asm"




; Test that a matrix is declared in free ram-space (that they don't squash
; each other's variables).
; Test that a matrix can be initialized with a single value, or filled with
; the values defined at a location.
; Test that partially filling a matrix in Init (supplied data is not big
; enough) will fail (well, I'll test that it passes)
test_21_matrix_DeclareInit:
	if_not	matrix_initiated_with_single_value, jp .failed_0F
	if_not	matrix_initated_with_preset_values, jp .failed_0F
.passed_0F
	lda	1
	TestResult	2, 1	; TestResult is a macro from test_includes.asm
.failed_0F
	lda	0
	TestResult	2, 1


; initialize a 4x4 matrix with the value 5. Iterate through all 9
; bytes and confirm that they all equal 5
matrix_initiated_with_single_value:
	mat_Declare	matrix, 4, 4
	lda	5
	mat_Init	matrix, a
	ld	hl, matrix	; load address of matrix into hl
	ld	bc, matrix_size - 1	; test full matrix's data
	;lda	2	; enable to fail test
	jp	compare_array_value


matrix123_start:
DB	1,2,3
DB	4,5,6
DB	7,8,9	; represent a 3x3 matrix increasing in value
matrix123_end:

matrix_initated_with_preset_values:
	mat_Declare	matrix, 3, 3
	; initialize matrix with values defined above
	mat_Init	matrix,matrix123_start, matrix123_end - matrix123_start
	ld	de, matrix
	ld	hl, matrix123_start
	ld	bc, matrix_size
	;ld	hl, matrix123_values + 2	; enable to fail test
	jp	compare_arrays


; compare a range of bytes to A, and return true if each byte == A
; HL = source address
; A = compare_byte
; BC = count (# of bytes)
compare_array_value:
.loop
	cp	[hl]	; compare value to A (with which you Init'ed matrix)
	jp	nz, .failed	; each value of [hl] should be equal to A
	increment	hl	; advance to next address in array
	dec	c
	jr	nz, .loop
.passed:
	ret_true
.failed:
	ret_false


; compare two arrays and verify that values are the same in each
; HL = Array1 source address
; DE = Array2 source address
; BC = byte-count (how large matrix / array is)
compare_arrays:
	inc	b	; our for-loop requires initial byte-count to be
	inc	c	; incremented first (because we decrement immediately)
	jr	.skip
.loop	
	ld	a, [de]
	cp	[hl]	; compare [HL] with [DE] (now stored in a)
	jp	nz, .failed	; each value of [hl] should be equal to A
	increment	hl	; advance to next address in array
	increment	de	; advance to next address in array
.skip
	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
.passed:
	ret_true
.failed:
	ret_false
