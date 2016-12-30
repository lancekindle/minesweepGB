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


	IF	!DEF(SYNTAX_ASM)
SYNTAX_ASM	SET	1


; simple macros to set/return true or false. Use these everywhere!
ret_true: MACRO
	set_true
	ret
	ENDM
ret_false: MACRO
	set_false
	ret
	ENDM

set_true: MACRO
	SCF     ; set carry flag
	ENDM
set_false: MACRO
	SCF
	CCF     ; compliment carry flag. (toggle it's value)
	ENDM

lda: MACRO
	ld	a, \1
	ENDM

; if_* MACROs take 2 arguments.
; 1) a function to call. Depending on if it returns true or not,
; 2) a command to run depending on return condition of function
; technically, the 2nd arg can be many arguments. They'll all get appended
; to the end of the 2nd arg run. (see unpack_arg2_cmd for details)
if_: MACRO
	call \1
	jr nc, .end_if_\@
	unpack_arg2_cmd
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
	cp \2
	IF (STRCMP("\1", "==") == 0) ; Z=1
	jr	nz, .skip_if_a\@
	ENDC
	IF (STRCMP("\1", ">=") == 0) ; C=0
	jr	c, .skip_if_a\@
	ENDC
	IF (STRCMP("\1", ">") == 0) ; C=0 & Z=0
	jr	c, .skip_if_a\@
	jr	z, .skip_if_a\@
	ENDC
	IF (STRCMP("\1", "<=") == 0) ; C=1 | Z=1
	jr	c, .exec_if_a_cmd\@  ; immediately exec if C=1
	jr	nz, .skip_if_a\@  ; fallthrough if Z=1. Otherwise, skip
	ENDC
	IF (STRCMP("\1", "<") == 0) ; C=1
	jr 	nc, .skip_if_a\@
	ENDC
	IF (STRCMP("\1", "!=") == 0) ; Z=0
	jr	z, .skip_if_a\@
	ENDC				; support both != and <>
	IF (STRCMP("\1", "<>") == 0) ; Z=0
	jr	z, .skip_if_a\@
	ENDC
.exec_if_a_cmd\@
	unpack_arg3_cmd
.skip_if_a\@
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


; first of all, let me apologize that this mess below is nearly the first thing
; you see in this file. But it plays an integral part to my macros. The a2-a9
; strings are one-line macros that get joined into the overall one-line macro
; `unpack_arg2_cmd`. This is responsible for handling all command arguments
; passed into other macros. In general, this means it'll handle arguments
; \2 - \9 (if that many exist). This allows the user to specify some pretty
; complex commands, like calling another macro with multiple arguments if an
; if_ macro passes, or nest multiple if_ macros before running a
; final command.
; so.. again.. why is this necessary? Say you want to run this small code:
;
;>>> if_ keboard_pressed, call moveCharacter
;
; this is simple. My if_ macro sees the 2nd argument ( \2 ) as
; `call moveCharacter` and so it simply
; runs `\2` which calls the function moveCharacter.
; But what if you want to run a macro, instead of call a function?
;
;>>> if_ left_pressed, MoveLeft 1
;
; Which is fine so long as you only need to pass one argument into the macro
;
;>>> if_ A_pressed, MoveXY 1, 2
;
; notice how arg2 is `MoveXY 1` and arg3 is `2`. Obviously, we want to call
; arg2 and pass it the 3rd argument. Which would look like:
;>>> \2, \3
; This is where _NARG comes into play.
; _NARG tells us Number of ARGuments passed into a macro. So we have to check
; for a matching set of arguments and then run the appropriate command.
; For clarity, a2 looks like this in it's exanded form:
; IF _NARG == 3
; 	\2, \3
; ENDC
a1	EQUS	"IF _NARG==2\n \\2\nENDC\n"
a2	EQUS	"IF _NARG==3\n \\2,\\3\nENDC\n"
a3	EQUS	"IF _NARG==4\n \\2,\\3,\\4\nENDC\n"
a4	EQUS	"IF _NARG==5\n \\2,\\3,\\4,\\5\nENDC\n"
a5	EQUS	"IF _NARG==6\n \\2,\\3,\\4,\\5,\\6\nENDC\n"
a6	EQUS	"IF _NARG==7\n \\2,\\3,\\4,\\5,\\6,\\7\nENDC\n"
a7	EQUS	"IF _NARG==8\n \\2,\\3,\\4,\\5,\\6,\\7,\\8\nENDC\n"
a8	EQUS	"IF _NARG==9\n \\2,\\3,\\4,\\5,\\6,\\7,\\8,\\9\nENDC\n"

unpack_arg2_cmd	EQUS	"{a1}{a2}{a3}{a4}{a5}{a6}{a7}{a8}"

; and of course, some macros need to expand arg3+, so here it is
b2	EQUS	"IF _NARG==3\n \\3\nENDC\n"
b3	EQUS	"IF _NARG==4\n \\3,\\4\nENDC\n"
b4	EQUS	"IF _NARG==5\n \\3,\\4,\\5\nENDC\n"
b5	EQUS	"IF _NARG==6\n \\3,\\4,\\5,\\6\nENDC\n"
b6	EQUS	"IF _NARG==7\n \\3,\\4,\\5,\\6,\\7\nENDC\n"
b7	EQUS	"IF _NARG==8\n \\3,\\4,\\5,\\6,\\7,\\8\nENDC\n"
b8	EQUS	"IF _NARG==9\n \\3,\\4,\\5,\\6,\\7,\\8,\\9\nENDC\n"

unpack_arg3_cmd	EQUS	"{b2}{b3}{b4}{b5}{b6}{b7}{b8}"

; and of course, some macros need to expand arg4+, so here it is
c2	EQUS	"IF _NARG==4\n \\4\nENDC\n"
c3	EQUS	"IF _NARG==5\n \\4,\\5\nENDC\n"
c4	EQUS	"IF _NARG==6\n \\4,\\5,\\6\nENDC\n"
c5	EQUS	"IF _NARG==7\n \\4,\\5,\\6,\\7\nENDC\n"
c6	EQUS	"IF _NARG==8\n \\4,\\5,\\6,\\7,\\8\nENDC\n"
c7	EQUS	"IF _NARG==9\n \\4,\\5,\\6,\\7,\\8,\\9\nENDC\n"

unpack_arg4_cmd	EQUS	"{c2}{c3}{c4}{c5}{c6}{c7}"


; jr_inverse is just like jr, except it tests for the opposite flag
; i.e. jr_inverse c, rDMA ==> jr nc, rDMA
; this is most useful in the if_flag macro
jr_inverse: MACRO
	IF (STRCMP("\1", "c") == 0)
	jr	nc, \2
	ENDC
	IF (STRCMP("\1", "nc") == 0)
	jr	c, \2
	ENDC
	IF (STRCMP("\1", "z") == 0)
	jr	nz, \2
	ENDC
	IF (STRCMP("\1", "nz") == 0)
	jr	z, \2
	ENDC
	IF (STRCMP("\1", "z") != 0) && (STRCMP("\1", "c") != 0)
		IF (STRCMP("\1", "nc") != 0) && (STRCMP("\1", "nz") != 0)
			FAIL "\n\1 is not a valid flag\n"
		ENDC
	ENDC
	ENDM


; preserve & restore register pair after cmd. \1 can be: af, bc, de, hl, or all
; if \1 is "all", then all registers will be preserved
; \2 is FULL command. such as CALL xyz or a macro. 
preserve: MACRO
	IF (STRCMP("\1", "all") == 0)
		push af
		push bc
		push de
		push hl
	ELSE
		push \1
	ENDC
	unpack_arg2_cmd
	IF (STRCMP("\1", "all") == 0)
		pop hl
		pop de
		pop bc
		pop af
	ELSE
		pop \1
	ENDC
	ENDM

preserve2: MACRO
	push \1
	push \2
	unpack_arg3_cmd
	pop \2
	pop \1
	ENDM


; load is designed to be used by other macros for enforcing arguments.
; calling load yyy, xxx
; will fail at compile time if xxx isn't register yyy, or a hard-coded #.
; (if arg2 matches arg1, this macro will create no instructions, since the
; value is already in the desired register)
; an optional arg3 will be printed before failure if necessary
; loading allows argument enforcement with the flexibility to pass either
; the desired register or a #. An example may be:
; load	a, \2, "move_Character arg2"
; The optional error message should be used to specify which argument is
; failing (as that may not be always obvious), or what the argument is
; supposed to contain conceptually.
; If it does error-out, a default error message is always included that
; tells the user which register should have been used
load:	MACRO
	IF STRIN("abcdehlABCDEHL","\1")==0
		PRINTT "\n====================================\n"
		PRINTT	"first argument must be register (a, b, c...)"
		FAIL	"\n====================================\n"
	ENDC
	IF STRIN("abcdehlABCDEHL","\2")>=1
		IF STRCMP("\1", "\2") != 0
			PRINTT "\n====================================\n"
			IF _NARG == 3
				PRINTT	"\3"
			ENDC
			PRINTT "\narg must be \1 or hard-coded #. Got \2 \n"
			FAIL	"\n====================================\n"
		ENDC
	ENDC
	IF STRIN("[hl][HL]", "\2") && STRIN("\2", "[") && STRIN("\2", "]")
		WARN	"I don't think \2 will work. You should just pass hl"
	ENDC
	IF STRCMP("\1", "\2") != 0	; only load register / value IF
		ld	\1, \2		; register 1 != register 2
	ENDC				; (it'd be silly to   ld a, a
	ENDM				; and a syntax error to   ld hl, hl)


; use with macro to enforce the loading of a hard-coded #.
; why use this? Say that you've created a macro that sets up a few registers,
; then calls a procedure which trashes all the registers in its computation.
; after that point, it needs to load a few more numbers and call another
; procedure. If any arguments are loaded into a register at this point, keep
; in mind that none of the registers contain any known value. You HAVE to
; load a hard-coded #. For example:
;do_stuff: MACRO
; ld	b, \1
; call	replace_all_registers_with_9
; ld	a, \2				; if \2 is a register such as c....
; ENDM					; c will hold 9, not the value that
;					; c previously held at start of macro
; at this point, you cannot "ld	a, \4" and expect it to work if \4 is any
; register. Hence, ldhard gives you peace of mind that the programmer will
; be unable to compile if he passes in a register where you've specified
; a hard-coded # must be supplied
ldhard: MACRO
	IF STRIN("afbcdehlAFBCDEHL", "\1") == 0
		IF _NARG == 3
			PRINTT	"\3"
		ENDC
		FAIL "arg1 should be a register / pair. Got \1 instead"
	ENDC
	IF STRIN("afbcdehlAFBCDEHL", "\2") >= 1
		IF _NARG == 3
			PRINTT	"\3"
		ENDC
		FAIL "arg2 should be a hard-coded #. Got \2 instead"
	ENDC
	ld	\1, \2
	ENDM


; load a register pair from another register pair
; ldpair	b,c,	h,l	; for example
; parameters are dest1, dest2,     src1, src2
; where dest1 & 2 must combine to form a register pair, and same with src1 & 2
ldpair: MACRO
	IF STRIN("afbcdehlAFBCDEHL", STRCAT("\1","\2")) == 0
		PRINTT "\nldpair only takes register pairs separated by commas"
		FAIL ".Got \1,\2 as first two arguments instead."
	ENDC
	IF STRIN("afbcdehlAFBCDEHL", STRCAT("\3","\4")) == 0
		PRINTT "\nldpair only takes register pairs separated by commas"
		FAIL ".Got \3,\4 as last two arguments instead."
	ENDC
	ld	\1, \3
	ld	\2, \4
	ENDM

; setvar variable, value
; uses  registers. So preserve if necessary
setvar: MACRO
	ld a, \2
	ld [\1], a
	ENDM
;getvar is simply ld a, [var]

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
		RL	\3	; rotate left, bit7 into carry-flag.
		RL	\2	; carry-flag rotates into bit0
		RL	\1
	ENDC
	IF _NARG == 3
		SLA	\3
		RL	\2
		RL	\1
	ENDC
	IF _NARG == 2
		SLA	\2
		RL	\1
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
		RR	\2	; rotate right, bit0 into carry-flag.
		RR	\3	; carry-flag rotates into bit7
		RR	\4
	ENDC
	IF _NARG == 3
		RR	\2
		RR	\3
	ENDC
	IF _NARG == 2
		RR	\2
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
	IF STRIN("abfcdhelABFCDHEL", "\1") >= 1	; it's just a single register
		FAIL "\nincrement requires register pair. Got \1\n"
	ENDC
	IF STRIN("afbcdehlAFBCDEHL", "\1") == 0	; didn't get a register pair
		FAIL "\nincrement requires register pair. Got \1, a single\n"
	ENDC
	IF STRIN("bcBC", "\1") >= 1
		inc	C
		if_flag	z, inc	B
	ENDC
	IF STRIN("deDE", "\1") >= 1
		inc	E
		if_flag	z, inc	D
	ENDC
	IF STRIN("hlHL", "\1") >= 1
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
	IF STRIN("abfcdhelABFCDHEL", "\1") >= 1	; it's just a single register
		FAIL "\ndecrement requires register pair. Got \1\n"
	ENDC
	IF STRIN("afbcdehlAFBCDEHL", "\1") == 0	; didn't get a register pair
		FAIL "\ndecrement requires register pair. Got \1, a single\n"
	ENDC
	IF STRIN("bcBC", "\1") >= 1
		inc	C
		dec	C ; check to see if we are about to decrement from 0
		if_flag	z, dec	B
		dec	C
	ENDC
	IF STRIN("deDE", "\1") >= 1
		inc	E
		dec	E ; check to see if we are about to decrement from 0
		if_flag	z, dec	D
		dec	E
	ENDC
	IF STRIN("hlHL", "\1") >= 1
		inc	L
		dec	L ; check to see if we are about to decrement from 0
		if_flag	z, dec	H
		dec	L
	ENDC
	ENDM





	ENDC  ; end syntax file
