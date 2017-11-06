;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; this code defines a set of macros. A LOT of macros.
; these macros are designed for one thing: WRITING READABLE ASSEMBLY
; as such, I consider these macros a part of my assembly syntax
; Generally with my macros, if a 2nd argument is required,
; it is expected to be a full command.
; Meaning if you want to call jpad_GetKeys, but preserve the register a,
; you'd use this:
; preserve	af, call jpad_GetKeys
; See how the second argument is `call jpad_GetKeys`; it's a full command.
; You can also use a macro (with a max of 8 args) in the 2nd argument.
; If you know macros well, you understand that technically
;>>> preserve	af, MoveXY 1, 1
; has three arguments: \1 == `af`. \2 == `MoveXY 1`, and \3 == `1`
; Nonetheless, arguments 2-9 are treated as one command such that the macro
; MoveXY is called with both arguments like so:
;>>> 	MoveXY 1, 1

; some abbreviations I may use:
; arg = argument  (passed parameter)
; fxn = function


	IF	!DEF(SYNTAX_INC)
SYNTAX_INC	SET	1

include "macro_syntax.inc"
include "assert_macro_args.inc"
include "if_conditionals.inc"


lda: MACRO
	ld	a, \1
	ENDM


; simple macros to set/return true or false. Use these everywhere!
; can include a conditional (C, NC, Z, NZ) as 1st arg. If the conditional
; does not pass, then the true/false portion of (re)setting the carry-flag
; will not happen either
ret_true: MACRO
	IF _NARG == 0
		set_true
		ret
	ENDC
	IF _NARG == 1
		IF STRCMP("C",STRUPR("\1")) == 0
			ret	c	; just return if we wanted carry-flag
			; (because CY is already set to true, then)
		ELSE
			jr_inverse	\1, .skip_ret_true\@
			ret_true
		ENDC
	ENDC
.skip_ret_true\@
	ENDM

ret_false: MACRO
	IF _NARG == 0
		set_false
		ret
	ENDC
	IF _NARG == 1
		IF STRCMP("NC",STRUPR("\1")) == 0
			ret	nc	; just return false if we wanted !CY
			; (because CY (carry-flag) is already set to false)
		ELSE
			jr_inverse	\1, .skip_ret_false\@
			ret_false
		ENDC
	ENDC
.skip_ret_false\@
	ENDM


set_true: MACRO
	SCF     ; set carry flag
	ENDM

set_false: MACRO
	SCF
	CCF     ; compliment carry flag. (toggle it's value)
	ENDM

; if_* MACROs take 2 arguments.
; 1) a function to call. Depending on if it returns true or not,
; 2) a command to run depending on return condition of function
; technically, the 2nd arg can be many arguments. They'll all get appended
; to the end of the 2nd arg run. (see unpack_arg2_cmd for details)
if_: MACRO
	call \1
	jr nc, .end_if_\@
	unpack_arg2_cmd		; from macro_syntax.inc
.end_if_\@
	ENDM

if_not: MACRO
	call \1
	jr c, .end_if_not\@
	unpack_arg2_cmd
.end_if_not\@
	ENDM

; macro for comparing contents of a to another value.
; can compare to: another register (b, c, d, e, h, l), a hard-coded value,
; or [HL]. [HL] means the byte to which HL points
; in assembly, the command `cp x` compares the register / value x to A:
; if A == x, Z=1, C=0
; if A > x, Z=0, C=0
; if A < x, Z=0, C=1
; this macro smartly determines what type of comparison you want, and uses only
; those instructions. So it's a sleek, fast and readable way to compare #s
; just remember that ifa doesn't work with negative #'s or those >= 256
ifa: MACRO
	cp	\2
	IF (STRCMP("\1", "==") == 0) ; Z=1
	jr	nz, .skip_ifa\@
	ENDC
	IF (STRCMP("\1", ">=") == 0) ; C=0
	jr	c, .skip_ifa\@
	ENDC
	IF (STRCMP("\1", ">") == 0) ; C=0 & Z=0
	jr	c, .skip_ifa\@
	jr	z, .skip_ifa\@
	ENDC
	IF (STRCMP("\1", "<=") == 0) ; C=1 | Z=1
	jr	c, .exec_ifa_cmd\@  ; immediately exec if C=1
	jr	nz, .skip_ifa\@  ; fallthrough if Z=1. Otherwise, skip
	ENDC
	IF (STRCMP("\1", "<") == 0) ; C=1
	jr 	nc, .skip_ifa\@
	ENDC
	IF (STRCMP("\1", "!=") == 0) ; Z=0
	jr	z, .skip_ifa\@
	ENDC				; support both != and <>
	IF (STRCMP("\1", "<>") == 0) ; Z=0
	jr	z, .skip_ifa\@
	ENDC
.exec_ifa_cmd\@
	unpack_arg3_cmd
.skip_ifa\@
	; we've completed ifa logic. Now we verify arguments are valid.
	; hint: I've seen that passing a 16-bit value to ifa doesn't cause an
	; error. We should detect that, and warn / fail
	;IF (STRCMP("[hl]","\2")!=0) && (STRCMP("[HL]","\2")!=0) ; \2 != [hl]
	;IF STRIN("ahflbdceAHFLBDCE", "\2") == 0	; \2 not an 8bit register
	;PRINTT	"\2"
	;IF ROUND(\2) > 255	; finally we check to see if value is > 8 bits
	;	PRINTT	"\n\2 is not an 8-bit number. We can only compare 8bit"
	;	PRINTT	" values, a single register, or [HL] to A. The value "
	;	PRINTT	"of \2 is "
	;	PRINTV	\2
	;	PRINTT	"\n"
	;ENDC
	;ENDC
	;ENDC
	ENDM

; just like ifa but reversed logic
; ifa_not >=, 4, inc C
; will increment C only if A is 3, 2, 1, 0 (basically if A < 4)
ifa_not: MACRO
	cp	\2
	IF (STRCMP("\1", "==") == 0) ; Z = 1 if A==\2
	; treat as ifa != x
	jr	z, .skip_ifa_not\@; since this is reverse logic, we skip if Z=1
	ENDC
	IF (STRCMP("\1", ">=") == 0) ; C=0 if A>=\2
	; ifa < x
	jr	nc, .skip_ifa_not\@	; skip if C=0
	ENDC
	IF (STRCMP("\1", ">") == 0) ; C=0 & Z=0 if A>\2
	; ifa <= x
	jr	c, .exec_ifa_not_cmd\@	; reversed logic, execute if C=1
	jr	nz, .skip_ifa_not\@		; reversed logic, skip if Z=0
	ENDC
	IF (STRCMP("\1", "<=") == 0) ; C=1 | Z=1
	; ifa > x
	jr	c, .skip_ifa_not\@  ; skip if C=1
	jr	z, .skip_ifa_not\@  ; skip if Z=1
	ENDC
	IF (STRCMP("\1", "<") == 0) ; C=1 if A<\2
	; ifa >= x
	jr 	c, .skip_ifa_not\@
	ENDC
	IF (STRCMP("\1", "!=") == 0) ; Z=0
	; ifa == x
	jr	nz, .skip_ifa_not\@
	ENDC				; support both != and <>
	IF (STRCMP("\1", "<>") == 0) ; Z=0
	; ifa == x
	jr	nz, .skip_ifa_not\@
	ENDC
.exec_ifa_not_cmd\@
	unpack_arg3_cmd
.skip_ifa_not\@
	ENDM


; first argument is a flag condition, such as z, nz, c, nc
; second argument is a command to run if flag is NOT set
if_not_flag: MACRO
	jr \1, .end_if_not_flag\@
	unpack_arg2_cmd
.end_if_not_flag\@
	ENDM

; first argument is a flag condition, such as z, nz, c, nc
; second argument is a command to run if flag IS SET
if_flag: MACRO
	jr_inverse	\1, .end_if_flag\@
	unpack_arg2_cmd
.end_if_flag\@
	ENDM


; first two arguments are flag condition, 3rd argument is command to run
if_not_flags: MACRO
	jr \1, .end_if_not_flags\@
	jr \2, .end_if_not_flags\@
	unpack_arg3_cmd
.end_if_not_flags\@
	ENDM

if_flags: MACRO
	jr_inverse	\1, .end_if_flags\@
	jr_inverse	\2, .end_if_flags\@
	unpack_arg3_cmd
.end_if_flags\@
	ENDM


; 1st argument is a bit-position. (bit 0-7).
; 2nd argument is register a,b,c,d,e,h,l or (hl)
; if the bit position of 2nd argument is set (to 1), execute arg3+
if_bit: MACRO
	bit	\1, \2
	jr	z, .end_if_bit\@
	unpack_arg3_cmd
.end_if_bit\@
	ENDM

; same thing but reversed. If bit \1 of \2 is 0, execute arg3+
if_not_bit: MACRO
	bit	\1, \2
	jr	nz, .end_if_not_bit\@
	unpack_arg3_cmd
.end_if_not_bit\@
	ENDM
	

if_bits: MACRO
	bit	\1, \3
	jr	z, .end_if_bits\@
	bit	\2, \3
	jr	z, .end_if_bits\@
	unpack_arg4_cmd
.end_if_bits\@
	ENDM

if_not_bits: MACRO
	bit	\1, \3
	jr	nz, .end_if_not_bits\@
	bit	\2, \3
	jr	nz, .end_if_not_bits\@
	unpack_arg4_cmd
.end_if_not_bits\@
	ENDM


; pushall   pushes all registers
pushall: MACRO
	push	AF
	push	BC
	push	DE
	push	HL
	ENDM

; popall    pops all registers (in the right order to undo pushall)
popall: MACRO
	pop	HL
	pop	DE
	pop	BC
	pop	AF
	ENDM


; load a register pair from another register pair, for example:
; ldpair	bc, hl
ldpair: MACRO
	ASSERT_IS_REG_PAIR \1, "must supply register pair as arg1. Got \1."
	ASSERT_IS_REG_PAIR \2, "must supply register pair as arg2. Got \2."
	ASSERT_NOT_EQUAL \1, \2, "ldpair requires two different register pairs"
	ld	LOW(\1), LOW(\2)
	ld	HIGH(\1), HIGH(\2)
	ENDM


; used for explicitly negating two registers as a pair
; (used by other macros only). Specify two registers like so: H,L
; always trashes AF. Be sure to push/pop af as necessary
; this is the fastest negation available. 6 cycles, 6 bytes
negate_pair: MACRO
	xor	a	; zero out A
	sub	\2	; subtract LSB of register pair (sets Carry if \2 > 0)
	; A = -LSB   (aka two's complement of LSB)
	ld	\2, a
	sbc	a, a	; if CY==1 (and \2 > 0), A = 255
			; if CY==0 (and \2 == 0), A = 0
	sub	\1	; A = -MSB - Carry  (aka A = complement(MSB) if CY = 1)
			; A = two's complement of MSB if CY = 0
	; they say that the easiest way to calculate two's complement of a
	; register (aka negate a register) is to complement, then increment.
	; so if A = %0000,0001. (01), then we complement: A = %1111,1110 (FE)
	; & increment: A = %1111,1111 (FF).    FF is two's complement to 01.
	; Or subtracting the value from zero is same as two's complement:
	; xor a
	; sub b		; gets two's complement of b.
	; When negating a register pair, we complement and store each register,
	; then increment the register pair as a whole. Meaning: increment LSB
	; and only increment MSB if LSB overflowed. And LSB only overflows if
	; it increments from FF to 0. LSB only increments to 0 if it's original
	; value (before complement) was 0. So in other words, we only
	; increment MSB if LSB == 0.
	ld	\1, a
	ENDM

; calculate two's complement of a register / pair
; completely safe, preserves all other registers (unless negating a pair and
; a third [optional] argument "trash AF" is supplied)
; e.g.: negate	bc
; or  : negate	bc, trash AF <- indicate you don't care about preserving AF
; the formulate for negating a # is to:
; 1) complement it (0->1, 1->0)
; 2) increment
; OR
; 1) subtract it from 0
; in the case of a register pair, the full register must be incremented,
; not necessarily incrementing both registers. In fact, the higher-byte
; of a register pair is ONLY incremented if the lower byte was 0 originally
; accumulator (A) is the only register capable of complementing. Hence why
; for anything other than `negate a`, a `push af; pop af` preserves AF
negate: MACRO
trash\@	set	0
	IF _NARG == 2
		IF STRCMP("TRASH AF", STRUPR("\2")) == 0
trash\@	set	1
		ELSE
			PRINTT	"\n you supplied a 2nd arg to negate: \2. "
			FAIL "2nd arg to negate should be `trash AF` only\n"
		ENDC
	ENDC
	IF arg1_IS_REGISTER	; it's a single register to negate
		IF STRCMP("A", STRUPR("\1")) == 0
			; negate a only
			cpl
			inc	a	; get -A
		ELSE
			IF (trash\@ == 0)
				push	af
			ENDC
			xor	a	; set A=0
			sub	\1	; get -(\1) in A
			ld	\1, a
			IF (trash\@ == 0)
				pop	af
			ENDC
		ENDC
	ENDC
	IF arg1_IS_REG_PAIR
		IF (trash\@ == 0)
			push	af
		ENDC
		IF STRCMP("BC", STRUPR("\1")) == 0
			negate_pair	b,c
		ENDC
		IF STRCMP("DE", STRUPR("\1")) == 0
			negate_pair	d,e
		ENDC
		IF STRCMP("HL", STRUPR("\1")) == 0
			negate_pair	h,l
		ENDC
		IF (trash\@ == 0)
			pop	af
		ENDC
	ENDC
	ENDM


; exchange register_pair1 with register_pair2
; allows functionality similar to what you might see in normal Z80 opcodes,
; (but with any registers) with one exception: cannot exchange with AF
; example:	exch	bc, hl
; COST:	9/4
exch: MACRO
	ASSERT_IS_REG_PAIR \1, "arg1 should be register pair. Got \1 instead."
	ASSERT_IS_REG_PAIR \2, "arg2 should be register pair. Got \2 instead."
	IF STRCMP(STRUPR("\1"),STRUPR("\2")) == 0
		WARN	"exch requested for the same register pair. Won't do"
	ELSE
	; recent RGBDS addition is the HIGH() and LOW() functions
	push	\1
	ld	HIGH(\1), HIGH(\2)
	ld	LOW(\1), LOW(\2)
	pop	\2
	ENDC
	ENDM


; shift register(s) left, with carry-over to preceding register
; essentially treating 1,2,3, or 4 registers as one register when shifting.
; extremely useful for multiplication whereby a register pair can hold
; a 16-bit number
; shift_left	a	; a * 2 with no overflow
; shift_left	b, c	; B*2, then C*2, overflowing into B
; shift_left	a, b, c	; A*2,B*2 overflows into A,C*2 overflows into B
shift_left: MACRO
	IF _NARG >= 5	; I could support more. I don't think I'll ever need to
		FAIL "shift_left supports max of 4 registers"
	ENDC
	IF _NARG == 4
		SLA	\4	; shift left, into carry-flag. Bit 0 becomes 0
		RLfast	\3	; rotate left, bit7 into carry-flag.
		RLfast	\2	; carry-flag rotates into bit0
		RLfast	\1
	ENDC
	IF _NARG == 3
		SLA	\3
		RLfast	\2
		RLfast	\1
	ENDC
	IF _NARG == 2
		SLA	\2	; 2, 2
		RLfast	\1	; 2, 2  (or 1,1 if A)
	ENDC
	IF _NARG == 1
		SLA	\1
	ENDC
	ENDM


; same thing as shift left, just in the opposite direction
; register 1 overflows into register 2, which overflows into register 3, etc.
; You can pass 1-4 registers to this macro
shift_right: MACRO
	IF _NARG >= 5	; I could support more. I don't think I'll ever need to
		FAIL "shift_right supports max of 4 registers"
	ENDC
	SRL	\1	; shift right, into carry-flag. Bit 7 becomes 0
	IF _NARG == 4
		RRfast	\2	; rotate right, bit0 into carry-flag.
		RRfast	\3	; carry-flag rotates into bit7
		RRfast	\4
	ENDC
	IF _NARG == 3
		RRfast	\2
		RRfast	\3
	ENDC
	IF _NARG == 2
		RRfast	\2
	ENDC
	ENDM


; macro to increment a register pair.
; we can't just call "inc hl" because the original gameboy trashes its
; sprite ram if inc hl/de/bc is run while hl points to the sprite ram
; if we increment the lower byte to zero, then we know we need to increment
; the higher byte
; increment sets the flags in the same fashion as INC
; (Zero flag is set if register = 0, and doesn't change the carry-flag)
increment: MACRO
	IF arg1_IS_REGISTER
		FAIL "\nincrement requires register pair. Got \1\n"
	ENDC
	IF STRCMP("AF",STRUPR("\1")) == 0
		FAIL "\nincrement requires register pair, but NOT AF\n"
	ENDC
	IF arg1_NOT_REG_PAIR
		FAIL "\nincrement requires register pair. Got \1, a single\n"
	ENDC
	IF STRCMP("BC",STRUPR("\1")) == 0
		inc	C
		if_flag	z, inc	B
	ENDC
	IF STRCMP("DE",STRUPR("\1")) == 0
		inc	E
		if_flag	z, inc	D
	ENDC
	IF STRCMP("HL",STRUPR("\1")) == 0
		inc	L
		if_flag	z, inc	H
	ENDC
	ENDM


; decrementing a register pair is a little trickier. We won't know if we've
; just decremented from zero, because the carry flag doesn't get set by
; a decrement operation. Instead, we check to see if the register is currently
; zero by incrementing then decrementing, setting the value back to (possibly)
; zero. If it is indeed zero, the Z flag will be set, and we'll know that
; our next decrement will underflow the lower byte. So we decrement the higher
; byte if our lower byte equals zero. Then, no matter what, we decrement
; the lower byte.
; decrement sets the flags in the same fashion as DEC
; (Zero flag is set if register = 0, and doesn't change the carry-flag)
decrement: MACRO
	IF arg1_IS_REGISTER
		FAIL "\ndecrement requires register pair. Got \1\n"
	ENDC
	IF STRCMP("AF",STRUPR("\1")) == 0
		FAIL "\ndecrement requires register pair, but NOT AF\n"
	ENDC
	IF arg1_NOT_REG_PAIR
		FAIL "\ndecrement requires register pair. Got \1, a single\n"
	ENDC
	IF STRCMP("BC",STRUPR("\1")) == 0
		inc	C
		dec	C ; check to see if we are about to decrement from 0
		if_flag	z, dec	B
		dec	C
	ENDC
	IF STRCMP("DE",STRUPR("\1")) == 0
		inc	E
		dec	E ; check to see if we are about to decrement from 0
		if_flag	z, dec	D
		dec	E
	ENDC
	IF STRCMP("HL",STRUPR("\1")) == 0
		inc	L
		dec	L ; check to see if we are about to decrement from 0
		if_flag	z, dec	H
		dec	L
	ENDC
	ENDM


; CASE 5, .end, roll_ball
; evaluates A against 5. If equal, it runs the command roll_ball and then
; breaks out of the switch statement by jumping to .end
; otherwise it skips roll_ball and moves onto the line below (which is usually
; another CASE statement)
; there is no switch macro, only case. So for redability, prefix the set of
; case statements with a .switch_<label> to explain what the switch-case is
; doing.
case: MACRO
	cp	\2
	IF (STRCMP("\1", "==") == 0) ; Z=1
	jr	nz, .skip_case\@
	ENDC
	IF (STRCMP("\1", ">=") == 0) ; C=0
	jr	c, .skip_case\@
	ENDC
	IF (STRCMP("\1", ">") == 0) ; C=0 & Z=0
	jr	c, .skip_case\@
	jr	z, .skip_case\@
	ENDC
	IF (STRCMP("\1", "<=") == 0) ; C=1 | Z=1
	jr	c, .exec_case_cmd\@  ; immediately exec if C=1
	jr	nz, .skip_case\@  ; fallthrough if Z=1. Otherwise, skip
	ENDC
	IF (STRCMP("\1", "<") == 0) ; C=1
	jr 	nc, .skip_case\@
	ENDC
	IF (STRCMP("\1", "!=") == 0) ; Z=0
	jr	z, .skip_case\@
	ENDC				; support both != and <>
	IF (STRCMP("\1", "<>") == 0) ; Z=0
	jr	z, .skip_case\@
	ENDC
.exec_case_cmd\@
	unpack_arg4_cmd
	jp	\3	; BREAK. jump to end-of-switch label
	; (yes, this is unlike standard C which requires a break. A break here
	; is implicit)
.skip_case\@
	ENDM



	ENDC  ; end syntax file
