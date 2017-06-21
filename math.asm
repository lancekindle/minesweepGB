;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; module to hold math procedures. Included macros are to help set up registers
; and call the procedure.

include "syntax.asm"

	IF	!DEF(MATH_ASM)
MATH_ASM	SET	1


; multiply two 8-bit registers together
; final result will reside in HL. (a 16-bit register is required)
; The two numbers should be in registers A & C (B will be set to 0)
; COST: Fastest (44 Cycles) vs Slowest (52 Cycles)
math_MultiplyAC:
	ld	b, 0
	ld	l, b
	ld	h, a	; set HL to $A0
	REPT	8	; (do the following 8 times)
; The trick here is to combine shifting bits out of A, and shifting BC
; appropriately into one step: add HL, HL
; Previously we RIGHT-shifted A and added C (as BC) to HL if CY=1, then
; LEFT-shifted BC unconditionally
; By LEFT-shifting A, we must start with BC = $XX00, where XX=multiplier.
; we then add BC to HL if CY=1, then RIGHT-shift BC unconditionally.
; this LEFT-shifting of A and (by left-shifting HL) RIGHT-shifting of BC
; can happen in one step: ADD HL, HL .... if H=A,L=0,B=0,C=C
		add	hl, hl
		if_flag	c,	add	hl, bc
	ENDR
	ret

; A is number to multiply by a power of 2
; C is the power. The result (in HL) will be A * 2^C
; TO BE CLEAR: if C = 8, the result will be A * 2^8, or A * 256
math_PowerA2C:
	ld	h, 0
	ld	l, a	; store a in L. Now HL holds value of A
	inc	c
	dec	c	; quickly check if C is 0
	ret	z	; return if power (C) is 0. (which means HL == A * 1)
.double_HL_per_C
	add	hl, hl	; double value of HL
	dec	c
	jr	nz, .double_HL_per_C
	ret

; A is number to multiply by a power of 2. B is the power.
; HL = A * 2^B.    Store this result (HL) in stack.
; then continue multiplying the current result HL by 2*C
; then add the previously calculated result (A * 2^B)
; Basically this is a VERY fast way to multiply A by a wide range of #'s
; HL = (A * 2^B) + (A * 2^(B+C))
; with this, you can get 3A, 5A, 6A, 9A, 10A, 12A, 17A, 18A, 20A, etc.
math_PowerA2B_Plus_A2BC:
	ld	h, 0
	ld	l, a	; store a in L. Now HL holds value of A
	inc	b
	dec	b	; to check if B > 0
	jr	z, .second_step_A2BC	;if B=0, move to 2nd step (HL == A)
.double_HL_per_B
	add	hl, hl	; HL = HL*2
	dec	b
	jr	nz, .double_HL_per_B
; at this point, HL = A * 2^B
.second_step_A2BC
	inc	c
	dec	c	; to check if C > 0
	ret	z	; if C=0, return with current result (A * 2^B) in HL
	push	hl	; store A * 2^B
.double_HL_per_C
	add	hl, hl	; HL = HL*2
	dec	c
	jr	nz, .double_HL_per_C
	; at this point, HL = A * 2^(B+C)
	pop	bc	; get previously stored result: BC = A * 2^B
	add	hl, bc	; HL = (A * 2^B) + (A * 2^(B+C))
	ret


; call this to write more readable "multiply" code
; math_Mult	a, 8
; result will reside in HL
; if you want to pass in register values, arg1 must be A (or a #),
; and arg2 must be C (or a #)
; if you want the benefit of fast-multiplication (based on a hard-coded #)
; then pass that # as 2nd argument
math_Mult: MACRO
	load	a, \1, "first byte of multiplication"; preload arg1
	; now we just need to check what kind of argument \2 is, and perform
	; the correct (and preferably fast) procedure
	IF STRIN("AFBCDEHL", STRUPR("\2")) >= 1
	; looks like the user passed a register / register pair
		PRINTT	"performing register axc multiplication"
		load	c, \2, "second byte of multiplication"
		call	math_MultiplyAC
	ELSE
	; check if \2 is a multiplier that can be done fast computationallly
	; these multipliers are in the form 2^B + 2^(B+C), where B,C = 0-8
	; basically, check if \2 is one of these numbers:
	; 3, 5, 6, 9, 10, 12, 17, 18, 20, 24, 33, 34, 36, 40, 48, 65, 66,
	; 68, 72, 80, 96,
	IF (\2 == 3) || (\2 == 5) || (\2 == 6) || (\2 == 9) || (\2 == 10) || (\2 == 12) || (\2 == 17) || (\2 == 18) || (\2 == 20) || (\2 == 24) || (\2 == 33) || (\2 == 34) || (\2 == 36) || (\2 == 40) || (\2 == 48) || (\2 == 65) || (\2 == 66) || (\2 == 68) || (\2 == 72) || (\2 == 80) || (\2 == 96) || (\2 == 129) || (\2 == 130) || (\2 == 132) || (\2 == 136) || (\2 == 144) || (\2 == 160)
		PRINTT "\nUsing ComplexPowerOf2 multiplication for \1 * \2\n"
		math_MultiplyComplicatedPowerOf2	a, \2
	ELSE
		; check if arg2 is a power of 2 for shortcut multiplication
		IF (\2 == 512) ||(\2 == 256) || (\2 == 128) || (\2 == 64) || (\2 == 32) || (\2 == 16) || (\2 == 8) || (\2 == 4) || (\2 == 2) || (\2 == 1)
			PRINTT "\nUsing PowerOf2 multiplication for \1 * \2\n"
			math_MultiplyPowerOf2	a, \2
		; below ELSE is for if \2 is not a quickly-calculable #
		; then we call the general multiplication form
		ELSE
			PRINTT "\nUsing general multiplication for \1 * \2\n"
			load	c, \2, "second byte of multiplication"
			call	math_MultiplyAC
		ENDC
	ENDC
	ENDC
	ENDM


; multiply HL by 8
; COST: (6 Cycles / 3 Bytes)
math_Multiply8HL: MACRO
	add	hl, hl	; x2
	add	hl, hl	; x4
	add	hl, hl	; x8. Done
	ENDM


; run this macro if arg2 is a power of 2 (only works with hard-coded #'s)
; 1, 2, 4, 8, 16, 32, 64, 128, 256, 512
math_MultiplyPowerOf2: MACRO
	IF STRIN("AFBCDEHL", STRUPR("\2")) >= 1
		FAIL	"\n cannot pass register into this macro. Got \2\n"
	ENDC
	load	a, \1	; already assume A is loaded
	IF \2 == 1	; 2^0 = 1x  (so it technically is a power of 2)
		ld	h, 0
		ld	l, a	; HL = A. DONE.
	ENDC
	IF \2 == 2	; COST: 5 / 4
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL = A*2. DONE.
	ENDC
	IF \2 == 4	; COST: 7 / 5
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL = A*2
		add	hl, hl	; HL = A*4. DONE.
	ENDC
	IF \2 == 8	; COST: 9 / 6
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8. Done.
	ENDC
	IF \2 == 16	; COST: 11 / 7
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		add	hl, hl		; HL = A*16. DONE.
	ENDC
	IF \2 == 32	; COST: 13 / 8
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		add	hl, hl		; HL = A*16
		add	hl, hl		; HL = A*32. Done
	ENDC
	IF \2 == 64	; COST: 15 / 9
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		math_Multiply8HL	; HL = A*64. Done
	ENDC
	IF \2 == 128	; COST: 17 / 10
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		math_Multiply8HL	; HL = A*64
		add	hl, hl		; HL = A*128. Done
	ENDC
	IF \2 == 256	; COST: 19 / 11
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		math_Multiply8HL	; HL = A*64
		add	hl, hl		; HL = A*128
		add	hl, hl		; HL = A*256. Done
	ENDC
	IF \2 == 512	; COST: 21 / 12
		ld	h, 0
		ld	l, a		; HL = A
		math_Multiply8HL	; HL = A*8
		math_Multiply8HL	; HL = A*64
		math_Multiply8HL	; HL = A*512. Done
	ENDC
	IF \2 == 1024	; This isn't used. It's an example of set-up
		ld	c, 10	;2^10 = 1024
		call	math_PowerA2C_2
	ENDC
	ENDM


; use this macro if arg2 is a hard-coded number that can be formed by this:
; arg2 = 2^B + 2^(B+C)
; where B and C are any integer 0-8
; some examples:
; B=0, C=2.... arg2=5		(2^0 + 2^(0+2)) => (2^0 + 2^2) => (1 + 4)
; B=1, C=2.... arg2=10		(2^1 + 2^(1+2)) => (2^1 + 2^3) => (2 + 8)
; B=3, C=2.... arg2=40		(2^3 + 2^(3+2)) => (2^3 + 2^5) => (8 + 32)
; B=1, C=5.... arg2=66		(2^1 + 2^(1+5)) => (2^1 + 2^6) => (2 + 64)
; use this to quickly multiply a register by a non-standard, hard-coded #
; a list of hard-coded #'s to which this will apply:
; 3, 5, 6, 9, 10, 12, 17, 18, 20, 24, 33,
; 34, 36, 40, 48, 65, 66, 68, 72, 80, 96,
; 129, 130, 132, 136, 144, 160, 192	<= I haven't short-coded last #
math_MultiplyComplicatedPowerOf2: MACRO
	load	a, \1		; we already assume A is loaded
	IF (\2 == 3)	; COST: 9 / 7
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		add	hl, hl	; HL=A*2
		add	hl, bc	; HL+A = A*3
	ENDC
	IF (\2 == 5)	; COST: 11 / 8
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		add	hl, hl	; HL=A*2
		add	hl, hl	; HL=A*4
		add	hl, bc	; HL+A = A*5
	ENDC
	IF (\2 == 6)	; COST: 11 / 8
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		ldpair	bc, hl	; store value of 2A
		add	hl, hl	; HL=4A
		add	hl, bc	; HL+2A = 6A
	ENDC
	IF (\2 == 9)	; COST: 13 / 9
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		math_Multiply8HL; HL=8A
		add	hl, bc	; HL+A = 9A
	ENDC
	IF (\2 == 10)	; COST: 13 / 9
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		ldpair	bc, hl	; store value of 2A
		add	hl, hl	; HL=4A
		add	hl, hl	; HL=8A
		add	hl, bc	; HL+2A = 10A
	ENDC
	IF (\2 == 12)	; COST: 13 / 9
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		add	hl, hl	; HL=4A
		ldpair	bc, hl	; store value of 4A
		add	hl, hl	; HL=8A
		add	hl, bc	; HL+4A = 12A
	ENDC
	IF (\2 == 17)	; COST: 15 / 10
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL=16A
		add	hl, bc	; HL+A = 17A
	ENDC
	IF (\2 == 18)	; COST: 15 / 10
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		ldpair	bc, hl	; store value of 2A
		math_Multiply8HL; HL=16A
		add	hl, bc	; HL+2A = 18A
	ENDC
	IF (\2 == 20)	; COST: 15 / 10
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		add	hl, hl	; HL=4A
		ldpair	bc, hl	; store value of 4A
		add	hl, hl	; HL=8A
		add	hl, hl	; HL=16A
		add	hl, bc	; HL+4A = 20A
	ENDC
	IF (\2 == 24)	; COST: 15 / 10
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		ldpair	bc, hl	; store value of 8A
		add	hl, hl	; HL=16A
		add	hl, bc	; HL+8A = 24A
	ENDC
	IF (\2 == 33)	; COST: 17 / 11
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL=16A
		add	hl, hl	; HL=32A
		add	hl, bc	; HL+A = 33A
	ENDC
	IF (\2 == 34)	; COST: 17 / 11
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		ldpair	bc, hl	; store value of 2A
		math_Multiply8HL; HL=16A
		add	hl, hl	; HL=32A
		add	hl, bc	; HL+2A = 33A
	ENDC
	IF (\2 == 36)	; COST: 17 / 11
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		add	hl, hl	; HL=4A
		ldpair	bc, hl	; store value of 4A
		math_Multiply8HL; HL=32A
		add	hl, bc	; HL+4A = 36A
	ENDC
	IF (\2 == 40)	; COST: 17 / 11
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		ldpair	bc, hl	; store value of 8A
		add	hl, hl	; HL=16A
		add	hl, hl	; HL=32A
		add	hl, bc	; HL+8A = 40A
	ENDC
	IF (\2 == 48)	; COST: 17 / 11
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL=16A
		ldpair	bc, hl	; store value of 16A
		add	hl, hl	; HL=32A
		add	hl, bc	; HL+16A = 48A
	ENDC
	IF (\2 == 65)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		math_Multiply8HL; HL=8A
		math_Multiply8HL; HL=64A
		add	hl, bc	; HL+A = 65A
	ENDC
	IF (\2 == 66)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		ldpair	bc, hl	; store value of 2A
		math_Multiply8HL; HL=16A
		add	hl, hl	; HL=32A
		add	hl, hl	; HL=64A
		add	hl, bc	; HL+2A = 66A
	ENDC
	IF (\2 == 68)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL=2A
		add	hl, hl	; HL=4A
		ldpair	bc, hl	; store value of 4A
		math_Multiply8HL; HL=32A
		add	hl, hl	; HL=64A
		add	hl, bc	; HL+4A = 68A
	ENDC
	IF (\2 == 72)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		ldpair	bc, hl	; store value of 8A
		math_Multiply8HL; HL=64A
		add	hl, bc	; HL+8A = 72A
	ENDC
	IF (\2 == 80)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL=16A
		ldpair	bc, hl	; store value of 16A
		add	hl, hl	; HL=32A
		add	hl, hl	; HL=64A
		add	hl, bc	; HL+16A = 80A
	ENDC
	IF (\2 == 96)	; COST: 19 / 12
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL=16A
		add	hl, hl	; HL=32A
		ldpair	bc, hl	; store value of 32A
		add	hl, hl	; HL=64A
		add	hl, bc	; HL+32A = 96A
	ENDC
	IF (\2 == 129)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		ldpair	bc, hl	; store value of A
		math_Multiply8HL; HL=8A
		math_Multiply8HL; HL=64A
		add	hl, hl	; HL = 128A
		add	hl, bc	; HL+A = 129A
	ENDC
	IF (\2 == 130)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL = 2A
		ldpair	bc, hl	; store value of 2A
		math_Multiply8HL; HL=16A
		math_Multiply8HL; HL=128A
		add	hl, bc	; HL+2A = 130A
	ENDC
	IF (\2 == 132)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		add	hl, hl	; HL = 2A
		add	hl, hl	; HL = 4A
		ldpair	bc, hl	; store value of 4A
		math_Multiply8HL; HL=32A
		add	hl, hl	; HL = 64A
		add	hl, hl	; HL = 128A
		add	hl, bc	; HL+4A = 132A
	ENDC
	IF (\2 == 136)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		ldpair	bc, hl	; store value of 8A
		math_Multiply8HL; HL=64A
		add	hl, hl	; HL = 128A
		add	hl, bc	; HL+8A = 136A
	ENDC
	IF (\2 == 144)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL = 16A
		ldpair	bc, hl	; store value of 16A
		math_Multiply8HL; HL = 128A
		add	hl, bc	; HL+16A = 144A
	ENDC
	IF (\2 == 160)	; COST: 21 / 13
		ld	h, 0
		ld	l, a	; HL = A
		math_Multiply8HL; HL=8A
		add	hl, hl	; HL = 16A
		add	hl, hl	; HL = 32A
		ldpair	bc, hl	; store value of 32A
		add	hl, hl	; HL = 64A
		add	hl, hl	; HL = 128A
		add	hl, bc	; HL+32A = 160A
	ENDC
	IF (\2 == 192)
		ld	b, 6	;   Ax64
		ld	c, 1	; + Ax128
		call	math_PowerA2B_Plus_A2BC_2	; the procedure way
	ENDC
	ENDM
; 17/8   ==>   17 is NUMERATOR. 8 is DENOMINATOR
; division requires sampling the MSB (one extra bit at a time) from the
; numerator, and subtracting the denominator from the current sample only
; once the sample is >= denominator.
; we continue this process, adding MSBs to the remainder, until we've
; fully-divided the numerator.
; INPT:	A holds numererator
;	C holds denominator
; EXIT:	A holds integer result (rounded down)
;	B holds remainder
;	C still holds denominator
; USES:	AF, BC, DE
math_Divide_A_by_C:
	ld	b, a	; our algorithm actually performs B / C
			; (but we want conformity)
	ld	a, 0	; we will be shifting MSB's of B into A
	ld	d, 0	; we will store division result in D
	ld	e, 9	; # of times I will divide + 1. Rounded-Down Integer
.start_divide_C_by_A:
	dec	e
	jr	z, .done_dividingCB
	shift_left	a, b	; take first MSB sample from B, place in A
	ifa	>=, c, jr .subtract_B_from_A
	SLA	d	; sampled bits still too small for C, we have not yet
			; divided the remainder by B
	jr .start_divide_C_by_A
.subtract_B_from_A:
	sub	c	; remainder is in A
	SCF
	RL	d	; sampled bits were larger than B, so we have divided
			; a portion of the register.
			; To indicate we've done this, shift and store 1 in D
	jr .start_divide_C_by_A
.done_dividingCB:
	ld	b, a	; store remainder in B
	ld	a, d	; store result (from D) in A
	ret


; macro to set-up variables and call math_Divide. Will opt for speed-shortcuts
; wherever possible.
; if denominator is static, power-of-2 number, it will opt for the speed
; shortcut, meaning that only registers A & F will be altered (result in A).
math_Div: MACRO
	load	a, \1, "byte to be divided"
	; now we just need to check what kind of argument \2 is, and perform
	; the correct (and preferably fast) procedure
	IF STRIN("AFBCDEHL", STRUPR("\2")) >= 1
	; looks like the user passed a register / register pair
		PRINTT	"performing register A/C division"
		load	c, \2, "divider byte"
		call	math_Divide_A_by_C
	ELSE
	; check if \2 is a divisor that can be done fast computationallly
	; these divisors are in the form 2^C
	; basically, check if \2 is one of these numbers:
	; 2, 4, 8 (yep that's all for now)
	IF (\2 == 2) || (\2 == 4) || (\2 == 8) || (\2 == 16) || (\2 == 32) || (\2 == 64) || (\2 == 128)
		PRINTT "\nUsing fast division for \1 / \2\n"
		math_DivFast	a, \2
	ENDC
	ENDC
	ENDM


; DivFast divides A by a power of 2: 2,4,8,16,32,64,128
; The result remains in A, no other registers (except F) are altered
math_DivFast: MACRO
	load	a, \1, "byte to be divided"
	; SRL shifts 0 into bit 7. Other bits shift right
	IF (\2 == 2)
		SRL	a	; A/2
	ENDC
	IF (\2 == 4)
		SRL	a	; A/2
		SRL	a	; A/4
	ENDC
	IF (\2 == 8)
		RRA		; A/2    (except bit 7 is carried from bit 0)
		RRA		; A/4
		RRA		; A/8
		AND	%00011111	; mask out top 3 bits which were
	ENDC				; rotated in
	IF (\2 == 16)
		swap	a	; place top 4 bits into lower 4 bits (& vice versa)
		and	%00001111	; mask out top 4 bits
	ENDC
	IF (\2 == 32)
		; multiply, overflow, & masking trick == /32
		RLCA
		RLCA
		RLCA	; move bits 5-7 into bits 0-2
		and	%00000111	; save only bits 0-2
	ENDC
	IF (\2 == 64)
		RLCA
		RLCA	; move bits 6-7 into bits 0-1
		and	%00000011	; save only bits 0-1
	ENDC
	IF (\2 == 128)
		RLCA	; move bit 7 into bit 0
		and	%00000001	; save only bit 0
	ENDC
	ENDM






	ENDC	; end math.asm defines
