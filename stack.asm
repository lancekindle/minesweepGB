;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "syntax.inc"
include "vars.asm"
include "math.asm"

; STACK.ASM is a module designed to mimick the stack on the gameboy.
; It does so by declaring and blocking out a chunk of ram equal to the
; desired size + 1. This +1 is because, like the gameboy SP, it does
; not write to the first byte of its stack. On push, the stack increments
; it's pointer FIRST, then writes the byte. The pointer then sits on a
; filled value. On pop, the stack reads the byte where its sitting, THEN
; decrements. If pushing multiple values, the push macro is set up so
; that you must type `push stack-name c,b` as a reminder that the values
; get pushed in reverse order and then is popped as b,c
; the push and pop routines also check if there's enough space to
; push/pop the desired bytes. If there is not enough space, the routine
; aborts without altering the stack data and returns false (carry-flag=0)
; in the case of a push abort, the registers that were going to be pushed
; will also be preserved on exit (but the other registers will be trashed)


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


; push the register(s) onto the stack. If not enough space to push register(s),
; will return false and leave the pushable registers unaltered (but trashes
; all other registers)
; if arg "A" passed, will push register A onto stack (space-permitting)
; if args "C,B" passed, will push registers C,B onto stack (in that order)
; if args "C,B,A" passed, will push registers C,B,A onto stack (in that order)
stack_Push: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1_stack_end	; load end of stack address-space
thread_safe	set	0
	IF _NARG == 5
		IF STRCMP("THREAD_SAFE", STRUPR("\5")) == 0
thread_safe	set	1
		ENDC
	ENDC
	IF thread_safe
		di	; avoid one thread messing with stack while another accesses it
	ENDC
	IF _NARG >= 4
		load	C, \2, "When pushing 2 bytes, 1st byte to push in \1 stack will be in C"
		load	B, \3, "When pushing 2 bytes, 2nd to push into \1 stack will be in B"
		load	A, \4, "When pushing 3 bytes, 3rd byte to push into \1 stack will be A"
		call	stack_PushABC
	ENDC
	IF _NARG == 3
		load	C, \2, "When pushing 2 bytes, 1st byte to push in \1 stack will be in C"
		load	B, \3, "When pushing 2 bytes, 2nd to push into \1 stack will be in B"
		call	stack_PushBC
	ENDC
	IF _NARG == 2
		load	a, \2, "value to push into \1 stack"
		call	stack_PushA
	ENDC
	IF thread_safe
		ei	; re-enable interrupts. Stack is safe to modify
	ENDC
	IF (_NARG == 1) || ((_NARG > 4) && (thread_safe == 0))
		FAIL "stack_Push requires stack-name and 1, 2, or 3 values to push"
	ENDC
	ENDM


; macro that takes a macro that YOU DEFINE, and registers in which to place
; values. The Registers you define are loaded (in listed order) from top of
; stack. Your macro is embedded to run right after the registers are loaded
; with values. After your macro, BatchRead loops and begins again. It repeats
; until it reads past the beginning of its stack. At that point it skips past
; the macro and sets the pointer to the beginning of the stack
; one thing: DE and HL are used by this macro. Do not pop stack values into
; H,L,D,E, and if you use those registers in your defined macro, be sure to
; push / pop the register to preserve its contents. If you do not, this may
; never finish processing, and may load completey wrong data.
stack_BatchRead: MACRO
;need to know which registers are open. 0 = open, 1 = taken
A\@	SET	0
.preload\@
	load	HL, \1_stack_topL
	ld	c, [hl]	; load low-byte of stack-top address
	inc	hl
	ld	b, [hl]	; load high-byte of stack-top address directly to H
	; start of stack address in bc
	; hl -> MSB of top-of-stack
	load	DE, \1	; aka stack-start
	ld	[hl], D
	dec	hl
	ld	[hl], E	; Reset stack so that it's top-of-stack is the start
	ldpair	hl, bc
	; HL points to original stack-top. DE contains stack-start
.read_stack_bytes\@
	ld	\3, [hl]
		IF STRCMP(STRUPR("\3"), "A") == 0
A\@	SET	1
		ENDC
	dec	hl
		IF _NARG >= 4
	ld	\4, [hl]
	dec	hl
			IF STRCMP(STRUPR("\4"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
		IF _NARG >= 5
	ld	\5, [hl]
	dec	hl
			IF STRCMP(STRUPR("\5"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
		IF _NARG >= 6
	ld	\6, [hl]
	dec	hl
			IF STRCMP(STRUPR("\6"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
		IF _NARG >= 7
	ld	\7, [hl]
	dec	hl
			IF STRCMP(STRUPR("\7"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
		IF _NARG >= 8
	ld	\8, [hl]
	dec	hl
			IF STRCMP(STRUPR("\8"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
		IF _NARG >= 9
	ld	\9, [hl]
	dec	hl
			IF STRCMP(STRUPR("\9"), "A") == 0
A\@	SET	1
			ENDC
		ENDC
.verify_not_past_start\@
	; if H > D, we still have plenty of stack left
	IF A\@ == 1
		push	af
	ENDC
	lda	h
	ifa	>, d,   jr .run_user_macro\@
	; if L < E, we went past stack-start
	lda	L
	ifa	<, E,   jp .BatchRead_done\@
.run_user_macro\@
	IF A\@ == 1
		pop	af
	ENDC
	; if you get a syntax error here... check a couple things
	; 1) is first argument name of stack?
	; 2) is 2nd argument a macro and valid? Try calling it within own file
	\2	; run arg-2
	jp	.read_stack_bytes\@
.BatchRead_done\@
	pop	af
.done_noPop\@
	ENDM

;a1		EQUS	"IF _NARG>=2\n \\2\nENDC\n"
;run_arg2_macro	EQUS	"{a1}"


; a stringified macro to set a variable
if_last_arg	equs	""
; use like so:
; if_last_arg	==, thread_safe, thread_safe set 1


; INPT: A contains value to push onto stack
;	HL contains address to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (A was pushed onto stack)
; returns false if no room left on stack
; USES:	AF, BC, DE, HL
stack_PushA:
	ld	c, [hl]	; load LSB of stack top pointer
	inc	HL	; hl -> \1_stack_topH
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
	pop	af	; restore A value
	ret_false
.push
	pop	af
	inc	bc	; advance to next open position
	ld	[bc], a
	ld	[hl], b	; load MSB of stack top pointer
	dec	HL
	ld	[hl], c	; load LSB of stack top pointer
	ret_true

; INPT:	B & C contains values to push onto stack
;	HL contains addres to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (C & B was pushed onto stack -- in that order)
; returns false if not enough room left on stack to push both bytes
; stack will be unaltered if false returned.
; pushing registers will also be unaltered if false returned
; USES:	AF, BC, DE, HL
stack_PushBC:
	push	bc	; preserve B, C values
	ld	c, [hl]	; load LSB of stack top pointer
	inc	HL	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack address. DE holds end of stack address
	; verify that BC + 1 < DE
	inc	bc	; compensate that we'll be pushing 2 onto stack
	lda	b
	ifa	<, d, jr .push	; top-of-stack MSB < end-of-stack MSB
				; which means we have plenty of space to push
	lda	c
	ifa	<, e, jr .push	; top-of-stack LSB < end-of-stack LSB
				; which means we have space to push
	; we get here if BC (front of stack) >= DE (last stack address)
	; which means we must exit, throwing false
	pop	bc	; restore B, C values
	ret_false
.push
	; remember, we already incremented BC above. So our stack pointer
	; [BC] is already pointing to an open space in ram
	pop	de	; de contains B, C, respectively
	lda	e	; load value originally in C
	ld	[bc], a	; push C onto stack
	inc	bc	; advance to next open position
	lda	d	; load value originally in B
	ld	[bc], a	; push B onto stack
	; now we store our current pointer in ram
	; HL already points to where we'd loaded MSB (aka register B)
	ld	[hl], b	; load MSB of stack top pointer
	dec	HL
	ld	[hl], c	; load LSB of stack top pointer
	ret_true


; INPT:	A & B & C contains values to push onto stack
;	HL contains addres to ram holding stack top pointer
;	DE contains end of stack address
; returns true if succeeded. (C & B & A was pushed onto stack -- in that order)
; returns false if not enough room left on stack to push all three bytes
; stack will be unaltered if false returned.
; pushing registers will also be unaltered if false returned
; USES:	AF, BC, DE, HL
stack_PushABC:
	push	af	; preserve A value
	push	bc	; preserve B, C values
	ld	c, [hl]	; load LSB of stack top pointer
	inc	HL	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack address. DE holds end of stack address
	; verify that BC + 2 < DE
	inc	bc
	inc	bc	; compensate that we'll be pushing 3 onto stack
	lda	b
	ifa	<, d, jr .push	; top-of-stack MSB < end-of-stack MSB
				; which means we have plenty of space to push
	lda	c
	ifa	<, e, jr .push	; top-of-stack LSB < end-of-stack LSB
				; which means we have space to push
	; we get here if BC (front of stack) >= DE (last stack address)
	; which means we must exit, throwing false
	pop	bc	; restore B, C values
	pop	af	; restore A value
	ret_false
.push
	dec	bc	; decrement once so that [BC] (stack pointer) is
			;  pointing to first open space in ram
	pop	de	; de contains B, C, respectively
	lda	e	; load value originally in C
	ld	[bc], a	; push C onto stack
	inc	bc	; advance to next open position
	lda	d	; load value originally in B
	ld	[bc], a	; push B onto stack
	inc	bc	; advance to next open position
	pop	af	; restore A register
	ld	[bc], a	; push A onto stack
	; now we store our current pointer in ram
	; HL already points to where we'd loaded MSB (aka register B)
	ld	[hl], b	; load MSB of stack top pointer
	dec	HL
	ld	[hl], c	; load LSB of stack top pointer
	ret_true



; pop byte(s) from stack
; if just stack-name passed, will pop value into A. Can also pass arg "A"
; if arg "BC" passed, will pop two bytes from stack into B,C (in that order)
; if arg "DE" passed, will pop two bytes from stack into D,E (in that order)
; if arg "HL" passed, will pop two bytes from stack into H,L (in that order)
; if arg "ABC",   will pop three bytes from stack into A,B,C (in that order)
; returns true if available byte(s) were read
; else return false (and will not alter the stack)
; USES:	AF, BC, DE, HL
stack_Pop: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1		; load beginning of stack address-space
thread_safe	set	0
	IF _NARG == 3
		IF STRCMP("THREAD_SAFE", STRUPR("\3")) == 0
thread_safe	set	1
		ENDC
	ENDC
	IF thread_safe
		di	; avoid multi-thread manipulation of stack
	ENDC
	IF _NARG == 1
		call	stack_PopA
	ENDC
	IF _NARG >= 2
		IF STRIN("A",STRUPR("\2")) >= 1	;+A+
			call	stack_PopA
		ELSE
		IF STRIN("BC",STRUPR("\2")) >= 1	;+BC+
			call	stack_PopDE
			ldpair	bc, de
		ELSE
		IF STRIN("DE",STRUPR("\2")) >= 1		;+DE+
			call	stack_PopDE
		ELSE
		IF STRIN("HL",STRUPR("\2")) >= 1			;+HL+
			call	stack_PopDE
			ldpair	hl, de
		ELSE
		IF STRIN("ABC",STRUPR("\2")) >= 1			;+ABC+
			call	stack_PopABC
		ELSE
			FAIL	"\nexpected A,BC,DE,HL, or ABC. Got \2\n"
		ENDC							;-HL-
		ENDC						;-DE-
		ENDC					;-BC-
		ENDC				;-A-
		ENDC
	ENDC; end if _narg=2
	IF thread_safe
		ei	; re-enable interrupts. Stack is now safe to manipulate
	ENDC
	IF _NARG >= 4
		FAIL	"\nstack_Pop expects at most 2 arguments. Got more\n"
	ENDC
	ENDM


; read a value from current stack-address, then back up one byte to point to
; next available (and unread) byte. Save that ptr back into ram
stack_PopA:
	ld	c, [hl]	; load LSB of stack top pointer
	inc	hl	; hl -> \1_stack_topH
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
	dec	bc	; advance to previous byte on stack
	ld	[hl], b	; store MSB of stack top pointer
	dec	hl	; hl -> \1_stack_topL
	ld	[hl], c	; store LSB of stack top pointer
	ret_true


; pops B & C values from stack (in that order)
; stack-ptr points to current value B (supposedly).
; Will verify that we have space to read both B & C, and then do so
; returns true if bytes read (and they'll be in BC),
; or returns False, without having read either byte
stack_PopDE:
	ld	c, [hl]	; load LSB of stack top pointer
	inc	hl	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack addr. DE holds beginning of stack addr
	; verify that BC - 1 > DE   (since we need to pop 2 values)
	; (same thing as verifying that BC - 2 >= DE)
	dec	BC
	lda	b
	ifa	>, d, jr .pop
	lda	c
	ifa	>, e, jr .pop
	; we get here if (top-of-stack - 2) <= start-of-stack
	; we don't have two bytes available to read, so return false
	ret_false
.pop
	; remember, we decremented BC above already. So [BC] points to value B
	inc	BC	; [BC] points back to top-of-stack
	ld	a, [bc]	; load byte B from stack
	ld	d, a	; move B value
	dec	BC	; back up to next unread byte (C)
	ld	a, [bc]	; load C byte from stack
	ld	e, a	; move C value
	dec	BC	; advance to next unread byte
	ld	[hl], b	; store MSB of stack top pointer
	dec	hl	; hl -> \1_stack_topL
	ld	[hl], c	; store LSB of stack top pointer
	ret_true


; pops A & B & C values from stack (in that order)
; stack-ptr points to current value B (supposedly).
; Will verify that we have space to read A & B & C, and then do so
; returns true if bytes read (and they'll be in A,BC),
; or returns False, without having read any bytes
stack_PopABC:
	ld	c, [hl]	; load LSB of stack top pointer
	inc	hl	; hl -> \1_stack_topH
	ld	b, [hl]	; load MSB of stack top pointer
	; BC now holds current stack addr. DE holds beginning of stack addr
	; verify that BC - 2 > DE   (since we need to pop 3 values)
	; (same thing as verifying that BC - 3 >= DE)
	dec	BC
	dec	BC
	lda	b
	ifa	>, d, jr .pop
	lda	c
	ifa	>, e, jr .pop
	; we get here if (top-of-stack - 3) <= start-of-stack
	; we don't have three bytes available to read, so return false
	ret_false
.pop
	push	hl
	ld	hl, 2	; set HL = BC + 2, which undoes the above x2 dec
	add	hl, bc	; [HL] now points to top of stack, to read byte A
	ld	a, [hl]	; pop value A from stack
	dec	HL	; [HL] => B
	ld	b, [hl]	; pop value B from stack
	dec	HL	; [HL] points to next value. (C)
	ld	c, [hl]	; pop value C from stack
	dec	HL	; advance to next unread byte
	ldpair	de, hl	; move stack pointer to [DE]
	pop	hl	; [HL] points to ramspace where we store DE, the
			; pointer to top-of-stack
	ld	[hl], d	; store MSB of stack-top-pointer
	dec	hl	; back up to point to LSB
	ld	[hl], e	; store LSB of stack-top-pointer
	ret_true


; returns with HL containing stack size (filled amount, as opposed to max size)
; returns true if HL > 0
; returns false if HL == 0
; USES:	AF, BC, DE, HL
stack_Size: MACRO
	ld	hl, \1_stack_topL
	ld	de, \1		; load start of stack address
	call	stack_SizeHL
	ENDM

stack_SizeHL:
	ld	c, [hl]	; load LSB of stack top pointer
	inc	hl
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
