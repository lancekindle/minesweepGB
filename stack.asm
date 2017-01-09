;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "syntax.asm"
include "vars.asm"
include "math.asm"

	IF !DEF(STACK_ASM)
STACK_ASM	SET	1

stack_Declare: MACRO
; add 1 to stack-size (arg \2) so that it holds the expected number of bytes
\1_stack_size = \2 + 1			; create compiler-level constant vars
	var_MidRamBytes	\1, \2 + 1	; reserve stack's size (+1) of ram
\1_stack_end = \1 + \2			; keep track of end of stack
	IF !DEF(\1)
		PRINTT "\nCreating Stack called \1 with size \2\n"
		var_MidRamBytes \1, \2
	ELSE
		PRINTT	"\nCreating Matrix \1 @"
		PRINTV	\1
		PRINTT	" and size \2"
		; we don't do anything since \1 is already a defined name.
		; likely it points to a VRAM address
	ENDC
	var_LowRamByte	\1_stack_topL	;holds address of top of stack
	var_LowRamByte	\1_stack_topH
	ENDM


; USES: A, HL
; loads start address of stack in ram
stack_Init: MACRO
	ld	hl, \1
	ld	a, l
	ld	[\1_stack_topL], a
	ld	a, h
	ld	[\1_stack_topH], a
	ENDM


; push A onto stack
stack_Push: MACRO
	load	a, \2, "value to push into \1 stack"
	ld	hl, \1_stack_topL
	ld	de, \1_stack_end	; load end of stack address-space
	call	stack_PushA
	ENDM


; INPT: A contains value to push onto stack
;	HL contains address to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (A was pushed onto stack)
; returns false if no room left on stack
stack_PushA:
	ld	c, [hl]	; load LSB of stack top pointer
	increment	HL	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	push	af	; preserve A value
	lda	c
	ifa	<>, e, jr .push	; it's more likely that LSB's won't match
	lda	b		; (but if it does, compare to MSB)
	ifa	<>, d, jr .push
	; we get here if BC (front of stack) == DE (last stack address)
	; which means we must exit, throwing false
	pop	af
	ret_false
.push
	pop	af
	increment	bc	; advance to next open position
	ld	[bc], a
	ld	[hl], b	; load MSB of stack top pointer
	decrement	HL
	ld	[hl], c	; load LSB of stack top pointer
	ret_true


; pop A from stack
; returns true and with byte in A if there's an available byte to read
; else return false, with A holding 0
; USES:	AF, BC, DE, HL
stack_Pop: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1		; load beginning of stack address-space
	call	stack_PopA
	ENDM


stack_PopA:
	ld	c, [hl]	; load LSB of stack top pointer
	increment	hl	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	lda	c
	ifa	<>, e, jr .pop
	lda	b
	ifa	<>, d, jr .pop
	; we get here if the top of the stack is the beginning of the stack
	; there's no bytes to read from, so return false
	ret_false
.pop
	ld	a, [bc]	; load byte from stack
	decrement	bc	; advance to previous byte on stack
	ld	[hl], b	; store MSB of stack top pointer
	decrement	hl	; hl -> \1_stack_topL
	ld	[hl], c	; store LSB of stack top pointer
	ret_true

; returns with HL containing stack size
; returns true if HL > 0
; returns false if HL == 0
; USES:	AF, BC, DE, HL
stack_Size: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1		; load start of stack address
	call	stack_SizeA
	ENDM

stack_SizeHL:
	ld	c, [hl]	; load LSB of stack top pointer
	increment	hl
	ld	b, [hl]	; load MSB of stack top pointer

	lda	e	; begin loading -DE into HL
	cpl
	inc	a	; get two's complement of LSB of start of stack
	; zero flag is set if we need to add 1 to MSB
	ld	l, a
	lda	d
	cpl		; get one's complement of MSB of start of stack
	if_flag	z, inc	a	; add one to MSB if LSB rolled over
				; (part of creating two's complement)
	ld	h, a	; DE contained start-of-stack address
			; HL now contains -(DE)
	add	hl, bc		; HL = BC - HL (aka "end-start" aka size)
	ld	a, h
	or	l	; sets zero flag if HL == 0
	ret	z	; return if zero (carry flag will be false, too)
	; ok we got here meaning HL > 0
	ret_true	; return true indicating stack size > 0




	ENDC	; end stack.asm define
