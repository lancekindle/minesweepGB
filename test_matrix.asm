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
	ld	bc, matrix_size	; test full matrix's data
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
	inc	b
	inc	c		; initially increment b & c
	jr	.decrement	; immediately decrement to make up for offset
.loop
	cp	[hl]	; compare value to A (with which you Init'ed matrix)
	jp	nz, .failed	; each value of [hl] should be equal to A
	increment	hl	; advance to next address in array
.decrement
	dec	c
	jr	nz, .loop
	dec	b
	jr	nz, .loop
.passed
	ret_true
.failed
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
	dec	c	 ; decrement bc. Start with C. If C doesn't decrement
	jr	nz,.loop ; to 0, then continue loop. Otherwise,
	dec	b	 ; decrement B. Again, we check if B reaches 0
	jr	nz,.loop ; and then exit loop entirely if so.
	; Technically, this exits the loop 1B early. That's why we incremented
	; both b and c before beginning the for-loop; to offset for our
	; early exit
.passed
	ret_true
.failed
	ret_false



; test that iterator initializes with correct values
test_22_matrix_IterDeclareInit:
	mat_Declare	matrix, 5, 5
	mat_Init	matrix, 32	; initialize 5x5 with 32
	mat_IterDeclare	matrix
	mat_IterInit	matrix	; setup full matrix iteration
	mat_IterExhaust	matrix	; fully iterate matrix
	ifa	<>, 25, jp .failed	; verify iterated 25 bytes
	mat_IterInit	matrix
	mat_IterCount	matrix, ==, 32
	ifa	<>, 25, jp .failed	; verify all 25 bytes == 32
	PRINTV	matrix
.passed
	lda	1
	TestResult	2, 2
.failed
	lda	0
	TestResult	2, 2

mat5x5_begin:
DB	 1, 2, 3, 4, 5
DB	 6, 7, 8, 9,10
DB	11,12,13,14,15
DB	16,17,18,19,20
DB	21,22,23,24,25	; represent a 5x5 matrix increasing in value
mat5x5_end:

submat5x5:
DB	 1, 2, 3
DB	 6, 7, 8
DB	11,12,13

submat5x5_1_4:
DB	 7, 8, 9
DB	12,13,14
DB	17,18,19


;verify_submatrix iterator, hand-crafted-submatrix
; arg1 is iterator (submatrix)
; arg2 is start address of hand-crafted submatrix
verify_submatrix: MACRO
	ld	hl, \2 - 1 ; don't worry. we +1 before comparing
.loop\@
	push	hl	; preserve hand-crafted submatrix address
	mat_IterNext	\1 ; get next submatrix value
	pop	hl
	jr	nc, .done\@	; finished iteration
	increment	hl ; advance to next byte in hand-crafted submatrix
	cp	[hl]	; compare hand-crafted submatrix to iterated value
	jp	nz, .failed	; fail if the aren't equal
	jr	.loop\@
.done\@
	ENDM


test_23_matrix_SubmatrixIter:
	mat_Declare	m3, 5, 5
	mat_Init	m3, mat5x5_begin, mat5x5_end - mat5x5_begin
	mat_IterDeclare	m3
	mat_IterInit	m3, 0,3, 0,3	; 3x3 submatrix iterator
	verify_submatrix	m3, submat5x5
	; verify iterator iterates over same values as hand-crafted submatrix
	; now let's test that the count is correct
	mat_IterInit	m3, 1,4, 1,4	; setup another 3x3 submatrix
	mat_IterExhaust	m3
	ifa	<>, 9, jr .failed
	; now verify values not just from 0,0 to 3,3
	mat_IterInit	m3, 1,4, 1,4
	verify_submatrix	m3, submat5x5_1_4
.passed
	lda	1
	TestResult	2, 3
.failed
	lda	0
	TestResult	2, 3


; hl holds address to hand-crafted submatrix values
compare_iter_to_array:
	
