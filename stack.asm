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
	ld	hl, \1_stack_topL
	ld	de, \1_stack_end	; load end of stack address-space
	IF _NARG == 3
		load	C, \2, "When pushing 2 bytes, 1st byte to push in \1 stack will be in C"
		load	B, \3, "When pushing 2 bytes, 2nd to push into \1 stack will be in B"
		call	stack_PushBC
	ENDC
	IF _NARG == 2
		load	a, \2, "value to push into \1 stack"
		call	stack_PushA
	ENDC
	IF (_NARG == 1) || (_NARG > 3)
		FAIL "stack_Push requires stack-name and 1 or 2 values to push"
	ENDC
	ENDM


; INPT: A contains value to push onto stack
;	HL contains address to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (A was pushed onto stack)
; returns false if no room left on stack
; USES:	AF, BC, DE, HL
stack_PushA:
	ld	c, [hl]	; load LSB of stack top pointer
	increment	HL	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	push	af	; preserve A value
	lda	b
	ifa	<, d, jr .push	; top-of-stack MSB < end-of-stack MSB
				; which means we have plenty of space to push
	lda	c
	ifa	<, e, jr .push	; top-of-stack LSB < end-of-stack LSB
				; which means we have space to push
	; we get here if BC (front of stack) >= DE (last stack address)
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

; INPT:	B & C contains values to push onto stack
;	HL contains addres to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (C & B was pushed onto stack -- in that order)
; returns false if not enough room left on stack to push both bytes
; will not push either value 
; USES:	AF, BC, DE, HL
stack_PushBC:
	push	bc	; preserve B, C values
	ld	c, [hl]	; load LSB of stack top pointer
	increment	HL	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack address. DE holds end of stack address
	; verify that BC + 1 < DE
	increment	bc	; compensate that we'll be pushing 2 onto stack
	lda	b
	ifa	<, d, jr .push	; top-of-stack MSB < end-of-stack MSB
				; which means we have plenty of space to push
	lda	c
	ifa	<, e, jr .push	; top-of-stack LSB < end-of-stack LSB
				; which means we have space to push
	; we get here if BC (front of stack) >= DE (last stack address)
	; which means we must exit, throwing false
	pop	de
	ret_false
.push
	; remember, we already incremented BC above. So our stack pointer
	; [BC] is already pointing to an open space in ram
	pop	de	; de contains B, C, respectively
	lda	e	; load value originally in C
	ld	[bc], a	; push C onto stack
	increment	bc	; advance to next open position
	lda	d	; load value originally in B
	ld	[bc], a	; push B onto stack
	; now we store our current pointer in ram
	ld	[hl], b	; load MSB of stack top pointer
	decrement	HL
	ld	[hl], c	; load LSB of stack top pointer
	ret_true


; pop byte(s) from stack
; if just stack-name passed, will pop value into A. Can also pass arg "A"
; if arg "BC" passed, will pop two bytes from stack into B,C (in that order)
; if arg "DE" passed, will pop two bytes from stack into D,E (in that order)
; if arg "HL" passed, will pop two bytes from stack into H,L (in that order)
; returns true if available byte(s) were read
; else return false
; USES:	AF, BC, DE, HL
stack_Pop: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1		; load beginning of stack address-space
	IF _NARG == 1
		call	stack_PopA
	ENDC
	IF _NARG == 2
		IF STRIN("aA","\2") >= 1
			call	stack_PopA
		ENDC
		IF STRIN("bcBC","\2") >= 1
			call	stack_PopBC
		ENDC
		IF STRIN("deDE","\2") >= 1
			call	stack_PopBC
			ldpair	d,e,	b,c
		ENDC
		IF STRIN("hlHL","\2") >= 1
			call	stack_PopBC
			ldpair	h,l,	b,c
		ENDC
	ENDC
	ENDM


; read a value from current stack-address, then back up one byte to point to
; next available (and unread) byte. Save that ptr back into ram
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


; pops B & C values from stack (in that order)
; stack-ptr points to current value B (supposedly).
; Will verify that we have space to read both B & C, and then do so
; returns true if bytes read (and they'll be in BC),
; or returns False, without having read either byte
stack_PopBC:
	ld	c, [hl]	; load LSB of stack top pointer
	increment	hl	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack addr. DE holds beginning of stack addr
	; verify that BC - 1 > DE   (since we need to pop 2 values)
	; (same thing as verifying that BC - 2 >= DE)
	decrement	BC
	lda	b
	ifa	>, d, jr .pop
	lda	c
	ifa	>, e, jr .pop
	; we get here if (top-of-stack - 2) <= start-of-stack
	; we don't have two bytes available to read, so return false
	ret_false
.pop
	; remember, we decremented BC above already. So [BC] points to value C.
	increment	BC	; [BC] points back to top-of-stack
	ld	a, [bc]	; load byte B from stack
	ld	d, a	; move B value
	decrement	BC	; back up to next unread byte (C)
	ld	a, [bc]	; load C byte from stack
	ld	e, a	; move C value
	decrement	BC	; advance to next unread byte
	ld	[hl], b	; store MSB of stack top pointer
	decrement	hl	; hl -> \1_stack_topL
	ld	[hl], c	; store LSB of stack top pointer
	ldpair	b,c,	d,e	; move values into BC
	ret_true


; returns with HL containing stack size (as opposed to max size)
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
