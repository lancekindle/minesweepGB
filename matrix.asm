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
	IF _NARG == 3	; if user passes start & end address of values to fill
		; make pre-compile checks only
		; if passed argument 3 isn't a dynamic value in a register pair
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
		IF (\1 < $A000) && (\1 + (\1_H * \1_W) > $8000)
		; if inside VRAM space ($8000 - $A000), use SetVRAM to prevent
		; access outside of vblank (else it will trash video display)
			call	mem_CopyVRAM
		ELSE
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
; TRASHES ALL REGISTERS
mat_IterInit: MACRO
	IF _NARG == 1
	ld	hl, \1 - 1	; load address of matrix - 1
				; (we increment before sampling)
	lda	l	; load LSB of matrix address (-1)
	ld	[\1_iter_ptrL], a
	lda	h	; load MSB of matrix address (-1)
	ld	[\1_iter_ptrH], a
	; store steps-remain as a way to count down until we reach end of row
	; in this case, the width of the matrix -1
	lda	\1_W - 1
	ld	[\1_iter_steps_remain], a
	; store width of matrix as iter_W - 1. If we were iterating a submatrix
	; the width would be smaller than the full matrix. This number is used
	; to restore _iter_step_remain when we advance to the next row
	; (it's also equal to the initial value of \1_iter_steps remain)
	ld	[\1_iter_W], a
	; step is amount that we increment index-address (iter_ptr) each time
	; we advance to the next element
	lda	1
	ld	[\1_iter_step], a	; store step (in x-direction) as 1
	; store rowstep remain as a way to count down # of rows to traverse
	; (in this case, the height of the matrix - 1)
	lda	\1_H - 1
	ld	[\1_iter_rowsteps_remain], a
	; rowstep is extra amount to increment to return to start of next row
	; for a whole-matrix iteration, rowstep is 0, as iter_step will get us
	; from the last element of row 1 to the first element of row2,
	; for example. When dealing with a submatrix, this will be > 0
	lda	1
	ld	[\1_iter_rowstep], a	; store rowstep of 1
	ENDC
	; 5 args: name, startY, endY, startX, endX  (assumes x,y steps are 1)
	IF _NARG == 5
		load	b, \2, "arg2 mat_IterInit: Start Y"
		load	c, \3, "arg3 mat_IterInit: End Y"
		load	d, \4, "arg4 mat_IterInit: Start X"
		load	e, \5, "arg5 mat_IterInit: End X"
		ld	a, 1
		ld	[\1_iter_step], a
		lda	e
		sub	d	; calculate endX - startX
		ld	[\1_iter_steps_remain], a	; aka # of cols
		ld	[\1_iter_W], a		; submatrix width
		cpl
		inc	a	; get negative width (-width) of submatrix
		add	\1_W	; matrix_width - submatrix_width = rowstep
		ld	[\1_iter_rowstep], a
		lda	c
		sub	b	; calculate endY - startY
		dec	a	; rowsteps = # of rows -1
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

; setup iterator to iterate in a 3x3 square centered around given coordinates
; currenty doesn't account for edges
mat_IterInitRelativeNeighbors: MACRO
	load	d, \2
	load	e, \3	; D,E should contain Y, X, respectively
	mat_IterInit	\1	; lazy setup of all variables
			; WARNING: overwrite A & HL
	dec	d
	dec	e	; get address @ (Y-1, X-1)
	mat_IndexYX	\1, d, e	; calculate index @ (Y-1, X-1)
	load	bc, \1	; base address of matrix
	add	hl, bc	; full address of matrix@(Y-1,X-1)
	lda	l
	ld	[\1_iter_ptrL], a
	lda	h
	ld	[\1_iter_ptrH], a	; store pointer to start of submatrix
	ld	b, 3	; size of submatrix (both width and height)
	lda	b	; size of submatrix
	ld	[\1_iter_W], a
	ld	[\1_iter_steps_remain], a
	dec	a	; rows to traverse are 1 less than height of submatrix
	ld	[\1_iter_rowsteps_remain], a
	lda	\1_W
	sub	b	; rowstep = (matrix width - submatrix width)
	ld	[\1_iter_rowstep], a
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
	ld	a, [de]
	and	a	; set Z flag if steps (in column) remaining is zero
	jr	z, .iter_next_row	; skip iter-col to begin iter-row
	dec	a
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
	and	a	; set Z flag if rows remaining is zero
	; if no rows remain, we haven't got any more elements to read
	; which means we already got last element in the last iteration
	jr	z, .iter_ended	; hl points to last element
	dec	a	; ah, good. continue to next row's element
	ld	[de], a	; store remaining rows
	increment	de	; points to \1_iter_rowstep
	ld	a, [de]		; assume b=0. So BC=rowstep-amount
	ld	c, a
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
	set_false		; indicate end of iteration.
	ret			; No new element was retried this iteration
				; The last element is repeated here
.iter_get_element
	ld	c, [hl]			; get element @ index
	pop	de		; retrieve "start of variables" address
	ld	a, l
	ld	[de], a		; store LSB of index pointer
	ld	a, h
	increment	de
	ld	[de], a		; store MSB. Finishes storing new index in ram
	ld	a, c		; place element in A
	set_true		; indicate there are still more elements
	ret


	

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


	ENDC	; end matrix define
