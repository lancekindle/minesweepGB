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

; declare a matrix named `arg1` of H, W size (arg2 and arg3, respectively)
; the matrix name will also have two other variables declared, which simply
; attach _H and _X to the name of the matrix
; In order to allow treating other ram-space and VRAM as a matrix, I also
; detect if the user is passing an already-defined variable. In that case,
; I append _H and _X to the variable name, but leave the value of the
; variable unchanged. This allows one to pass in _SCRN0 (with a value of $9800)
; into mat_Declare and begin using all the mat_* macros to address _SCRN0 as
; a matrix.
mat_Declare: MACRO
\1_H = \2
\1_W = \3
\1_size = \2 * \3
	IF !DEF(\1)	; this will raise "syntax error" if \1 is just a #
		PRINTT "\nCreating Matrix called \1 with (H, W) = (\2, \3)\n"
		var_MidRamBytes	\1, (\2 * \3)	; allocate memory for matrix
		; \1 now equals the starting memory address of its matrix
	ELSE
		PRINTT "\nCreating Matrix \1 @ "
		PRINTV	\1
		PRINTT	" and (H, W) = (\2, \3)"
		; we actually don't need to do anything else here. Matrix is
		; "set up" in that \1_H and \1_W are now defined. mat_GetYX
		; and others will now just work.
		; DONT FORGET TO INITIALIZE VALUES IN MATRIX
	ENDC
	ENDM


; initialize matrix fully with a particular value (hard-coded or in A)
; OR
; initialize a matrix from a hard-coded matrix defined in the ROM.
; it's most easily done by applying a pre and post label like so:
; matrix_start:
; DB	1, 2, 3
; DB	4, 5, 6
; DB	7, 8, 9
; matrix_end:
; then you can initialize a matrix (called tictac) to those values like so:
; mat_Declare	tictac, 3, 3
; mat_Init	tictac, matrix_start, matrix_end - matrix_start
; tictac will now hold the values 1,2,3,4,5,6,7,8,9 in ram (in matrix format)
mat_Init: MACRO
	IF _NARG == 2
		load	hl, \1		; address of matrix
		load	a, \2		; value to set
		load	bc, \1_H * \1_W	; size of matrix
		IF (\1 < $A000) && (\1 + (\1_H * \1_W) > $8000)
		; if inside VRAM space ($8000 - $A000), use SetVRAM to prevent
		; access outside of vblank (else it will trash video display)
			call	mem_SetVRAM
		ELSE
			call	mem_Set
		ENDC
	ENDC
	; matrix-name, start addr of values, byte-count of values (end-start)
	IF _NARG == 3	; if user passes start address & size of values to fill
		; make pre-compile checks only
		; if passed argument 3 isn't a dynamic value in a register pair
		PRINTT "\n[===     \1, \2, \3     ===]\n"
		IF STRIN("afbcdehlAFBCDEHL", "\3") == 0
		; verify matrix will be fully filled by supplied values
		IF (\3) != (\1_H * \1_W)
			PRINTT	"\nfilling matrix requires all values to be "
			PRINTT	"specified. In this case, \1 is of size "
			PRINTV	\3
			PRINTT	" but you only provided values up to size "
			PRINTV	\1_H * \1_W
			FAIL	"\n"
		ENDC
		ENDC	; end pre-compile checks
		load	de, \1, "matrix address is destination"
		load	hl, \2, "start address of values is source"
		load	bc, \3, "number of bytes to fill (end - start)"
		PRINTV	\3
		IF (\1 < $A000) && (\1 + (\1_H * \1_W) > $8000)
		; if inside VRAM space ($8000 - $A000), use SetVRAM to prevent
		; access outside of vblank (else it will trash video display)
			PRINTT "copying into VRAM"
			call	mem_CopyVRAM
		ELSE
			PRINTT "copying outside vram"
			call	mem_Copy
		ENDC
	ENDC
	ENDM

; declare (set aside) variables in ram for iteration of a matrix.
; variables are set up one-after another in ram so that they can be read
; from a pointer, increment the pointer, then read next variable
mat_IterDeclare: MACRO
	var_LowRamByte	\1_iter_ptrL	; pointer to (sub)matrix index
	var_LowRamByte	\1_iter_ptrH	; msb of matrix pointer
	var_LowRamByte	\1_iter_steps_remain	; steps remaining in column
	var_LowRamByte	\1_iter_step		; iterator step in x
	var_LowRamByte	\1_iter_rowsteps_remain	; row steps remaining
	var_LowRamByte	\1_iter_rowstep		; iterator step in y (from end of submatrix)
	var_LowRamByte	\1_iter_W	; W of submatrix we're iterating
					; (used to reset \1_iter_step_remain
	ENDM				; after we advance a row)


; initialize iterator. Save variables to ram
; if you pass in just the matrix name, the iterator will be set up to
; iterate the entire matrix
; USES AF, BC, DE, HL
mat_IterInit: MACRO
	IF _NARG == 1
	ld	hl, \1 - 1	; load address of matrix - 1
				; (we increment before sampling)
	lda	l	; load LSB of matrix address
	ld	[\1_iter_ptrL], a
	lda	h	; load MSB of matrix address
	ld	[\1_iter_ptrH], a
	; store steps-remain as a way to count down until we reach end of row
	; in this case, the width of the matrix
	lda	\1_W + 1	; FIRST ROW ITERATION needs an additional step
	ld	[\1_iter_steps_remain], a
	; store width of matrix as iter_W. If we were iterating a submatrix
	; the width would be smaller than the full matrix. This number is used
	; to restore _iter_step_remain when we advance to the next row
	; (it's also equal to the initial value of \1_iter_steps remain)
	lda	\1_W		; all proceeding row iterations require just
				; the width  (not +1)
	ld	[\1_iter_W], a
	; step is amount that we increment index-address (iter_ptr) each time
	; we advance to the next element
	lda	1
	ld	[\1_iter_step], a	; store step (in x-direction) as 1
	; store rowstep remain as a way to count down # of rows to traverse
	; (in this case, the height of the matrix)
	lda	\1_H
	ld	[\1_iter_rowsteps_remain], a
	; rowstep is extra amount to increment to return to start of next row
	; for a whole-matrix iteration, rowstep is 0, as iter_step will get us
	; from the last element of row 1 to the first element of row2,
	; for example. When dealing with a submatrix, this will be > 0
	lda	1
	ld	[\1_iter_rowstep], a	; store rowstep of 1
	ENDC
	; 5 args: name, startY, endY, startX, endX  (assumes x,y steps are 1)
	; bounds are inclusive start, exclusive end:
	; [y1:y2, x1:x2)	meaning: up-to, but NOT including y2 and x2
	IF _NARG == 5
		load	b, \2, "arg2 mat_IterInit: Start Y"
		load	c, \3, "arg3 mat_IterInit: End Y"
		load	d, \4, "arg4 mat_IterInit: Start X"
		load	e, \5, "arg5 mat_IterInit: End X"
		ld	a, 1
		ld	[\1_iter_step], a
		lda	e
		sub	d	; calculate endX - startX
		;inc	a	; +1 to account for loop quirks
		inc	a	; +1 for first row
		ld	[\1_iter_steps_remain], a	; aka # of cols in row
		dec	a ; -1 rest of row-iterations should use normal width
		ld	[\1_iter_W], a		; submatrix width
		;dec	a	; remove 1 since we added 1 for loop-correction
		; get two's complement of width (aka get -width)
		cpl
		inc	a	; get negative width (-width) of submatrix
		add	\1_W + 1; rowstep = matrix_width - submatrix_width + 1
		; +1 because we want to get first byte of next row, 
		; not last byte of the current row
		ld	[\1_iter_rowstep], a
		lda	c
		sub	b	; calculate endY - startY (aka # of rows)
		; there used to be a `dec a` here, but I removed it
		; due to our new iterstep method, which stops one row early
		; (decrements rowsteps_remain and then exits if it equals zero
		; rather than checking if it equals zero first)
		ld	[\1_iter_rowsteps_remain], a
		ld	e, d	; move Start X to e
		ld	d, b	; move Start Y to d
		mat_IndexYX	\1, d, e	; get start index in HL
						; (which trashes all registers)
		ldhard	de, \1 - 1	; we get matrix address - 1 because
			; iterator steps 1 over before sampling byte
		add	hl, de	; submatrix_start = index + matrix_address
		ld	a, l
		ld	[\1_iter_ptrL], a
		ld	a, h
		ld	[\1_iter_ptrH], a ; store starting address in ram
	ENDC
	ENDM


; call this macro to get the next element in an initiated matrix iterator
; the next-in-line byte will be in A when this completes, and HL will contain
; the matrix index, the address where the byte A was retrieved.
; Carry flag will be reset (to 0) when the iterator has finished. The element
; in A once StopIteration is signalled is a repeat of the last element.
; (since technically, you've finished iterating the matrix BEFORE you get the
; StopIteration signal).
mat_IterNext: MACRO
	; load pointer to current index within matrix
	ld	de, \1_iter_ptrL	; de points to LSB of address ptr to submatrix
	call	mat_IterNext_fxn
	ENDM


; iterate matrix, returning next-in-line element in A.
; (retrieves iteration variables starting from HL)
; ALWAYS returns with valid element in A
; and HL will contain memory address at which element was taken
; if user attempts to call IterNext and the iterator has already
; reached the end, it will reset the carry-flag (to 0) and return the last
; element once again. This is known as the IterStop Signal.
; If IterNext_fxn is called again after it has already reached the end,
; it will return the last valid element from iteration,
; and reset carry flag (to 0), as if it had just reached the end.
; this is NOT the same thing as "Index". If you want to alter the value
; at address HL, simply use	ld [HL], xxx
; where xxx can be a register or a hard-coded value. Just be very careful
; that you verify it's not within VRAM range. (If it is, you just need to wait
; for V-Blank before writing, or else you risk garbling the display).

mat_IterNext_fxn:
	ld	b, 0	; setup for adding bc to HL later ;)
	push	de	; store "start of variables" address
	; retrieve index address from ram at de+1, de => H,L
	ld	a, [de]
	ld	l, a
	increment	de
	ld	a, [de]
	ld	h, a	; now hl points to curent index in (sub)matrix
.iter_next_step
	increment	de	; now DE points to \1_iter_steps_remain
	ld	a, [de]		; get iter_steps_remain
	dec	a
	jr	z, .iter_next_row	; if zero, skip iter-col to begin iter-row
	ld	[de], a		; update value of \1_iter_steps_remain in ram
	increment	de	; point to \1_iter_step
	ld	a, [de]
	ld	c, a		; Assume b=0. So BC=step-amount
	add	hl, bc			; step to next element in iterator
	jr	.iter_get_element
.iter_next_row	;we get here if we reached end of matrix row.
	increment	de	; point to \1_iter_step (was skipped above)
	increment	de	; point to \1_iter_rowsteps_remain
	ld	a, [de]
	dec	a ; sets Z flag if rows remaining is zero (we just decremented
	; to zero, but we had previously incremented to offset this)
	; if no rows remain, we haven't got any more elements to read
	; which means we already got last element in the last iteration
	jr	z, .iter_ended	; hl points to last element
	ld	[de], a	; store remaining rows
	increment	de	; points to \1_iter_rowstep
	ld	a, [de]		; get rowstep
	ld	c, a		; assume b=0. So BC=rowstep-amount
	increment	de	; points to \1_iter_W
	ld	a, [de]		; grab reset # for \1_iter_steps_remain
	add	hl, bc	; step to next row's element in iterator
	; now we need to reset column-steps-remaining
	pop	de
	push	de	; retrieve (and re-store) address of \1_iter_ptrL
	increment	de	; point to \1_iter_ptrH
	increment	de	; point to \1_iter_steps_remain
	ld	[de], a		; reset \1_iter_steps_remain in ram
	jr	.iter_get_element
.iter_ended
	pop	de ; retrieve pushed (now unnecessary) address of variables
	ld	a, [hl]		;get element @ index
	ret_false		; indicate end of iteration.
				; No new byte was retrieved this iteration
				; The last byte is repeated here
.iter_get_element
	ld	c, [hl]			; get element @ index
	pop	de		; retrieve "start of variables" address
	ld	a, l
	ld	[de], a		; store LSB of index pointer
	ld	a, h
	increment	de
	ld	[de], a		; store MSB. Finishes storing new index in ram
	ld	a, c		; place element in A
	ret_true		; indicate there are still more elements


; mat_IterCount    matrix, >=, 5
; uses AF, BC, HL, DE
; stops iteration short if count reaches 255
; EXIT: count in A
;	last-used-address within matrix in HL
mat_IterCount: MACRO
	ld	c, 1	; our count (start at 1 to detect overflow. -1 at end)
	load	b, \3	; number to compare stored in B. (BC is only register
			; pair that we preserve)
.loop\@
	push	bc	; store compare# and count
	mat_IterNext	\1	; gets next byte
	pop	bc	; restore compare# and count
	jr	nc,	.done\@	; CY=0 when we've gone past end of matrix
	ifa_not	\2, b, jr 	.loop\@	; begin loop again if comparison failed
	inc	c			; else, increment our count
	jr	nz, .loop\@		; (and then begin loop again)
	; we get here if C overflowed ($FF > $00)
.done\@
	dec	c	; -1 to compensate for +1 in beginning
			; (this ensures that if we overflowed, we'll return FF)
	ld	a, c	; place count in a
	ENDM



; call this, and the iterator will be exhausted, and count of iterated
; bytes returned.
; Useful for debugging; otherwise there's much easier ways to get the
; size of a submatrix
; (can handle a maximum of 255 bytes). Will return 255 and stop exhausting
; iterator
mat_IterExhaust: MACRO
	ld	c, 1	; our count (start at 1) (we'll compensate at end)
	; this is so that if we hit max count (255), C will actually have
	; rolled over to 0, which triggers the Zero-flag, and we can
	; detect that and stop before we sample 256 values
	; (which is one greater than we can handle)
.loop\@
	push	bc	; store count
	mat_IterNext	\1	; gets next byte
	pop	bc	; restore count
	jr	nc,	.done\@	; CY=0 when we've gone past end of matrix
	inc	c	; count += 1
	jr	nz, .loop\@		; (and then begin loop again)
	; we get here if C overflowed ($FF > $00)
.done\@
	dec	c	; this compensates for the ld c, 1 above
	ld	a, c	; place count in a
	ENDM


; mat_Count	name, >=, #
; Count all bytes in matrix that match conditional comparison
; such as >=, 3   (or >=, B   if B contains the value 3)
; will stop counting if count maxes out at 255, and finish with 255 immediately
; HL will contain the address of the last-compared byte in the matrix
mat_Count8bit: MACRO
	load	hl, \1	; load address of matrix
	load	b, \3	; comparator value  (\2 is comparison type: ==, >...)
	ld	c, 0	; our counter for # of instances
	ld	de, \1_size	; loop count
	inc	d ; we increment both D&E because our for-loop decrements
	inc	e ; in a non-std way. (we decrement first, then loop)
	jr	.countdown\@
.loop\@
	ld	a, [hl]		; load byte from matrix
	ifa_not	\2, b, jr	.countdown\@
	inc	c		; increment C
	jr	z,	.maxed_count\@	; C just rolled over from FF to 0
.countdown\@
	increment	hl	; advance to next byte in matrix
	dec	e	; countdown. (when combined with the below dec d)
	jr	nz, .loop\@ ; if e isn't zero, we don't yet have to dec d
	dec	d	; sets Zero flag if DE==0
	jr	nz, .loop\@	; continue looping until DE == 0
	inc	c	; to offset the below dec c
.maxed_count\@
	dec	c	; If we got here by jr z, .maxed_count,
	; then C just maxed out in values (FF) and was incremented to zero.
	; we decrement, then, to restore the max value, 255
	ld	a, c	; place count in A
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
	math_Mult	a, \1_W	; calculate Y mem offset (into HL)
	pop	de
	add	hl, de	; add X mem offset to HL
	ENDM

; from an index, calculate the Y, X coordinates in D,E respectively
; USES:	BC, DE, HL
; EXIT: D,E holds Y,X
mat_YX_from_Index: MACRO
	load	hl, \2, "index from which to calculate y,x"
	ld	b, 0
	ld	c, \1_W	; BC == 16-bit value of width
	cpl	b
	cpl	c		; calculate two's complement so that:
	increment	bc	; BC = -width
	ld	d, 0	; count (Y coordinate)
.sub	add	hl, bc
	jr	c, .y_found ; check if we've gone past 0. D = Y-value
	inc	d	; y+=1
	jr	.sub
.y_found
	decrement	bc
	cpl	b
	cpl	c	; get original width
	add	hl, bc	; HL previously held negative number after subtraction
			; of one too many rows. Now we add a row back to get
			; index remaining after valid Y-coordinate subtracted
	; HL should contain $00XX, where L contains XX, the X coordinate
	ld	e, l	; place X coordinate in E
	ENDM



	ENDC	; end matrix define
