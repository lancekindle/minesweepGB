;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(MACRO_SYNTAX_INC)
MACRO_SYNTAX_INC = 1

; Contains macros designed mostly for use by other macros

include "assert_macro_args.inc"
include "if_conditionals.inc"


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


; load is designed to be used by other macros for enforcing arguments.
; calling load yyy, xxx
; will fail at compile time if xxx isn't a hard-coded-# OR register yyy.
; (if arg2 matches arg1, this macro will create no instructions, since the
; value is already in the desired register)
; an optional arg3 will be printed before failure if necessary
; loading allows argument enforcement with the flexibility to pass either
; the desired register or a #. An example may be:
; load	a, \2, "movement speed of player (arg2)"
; The optional error message should be used to specify which argument is
; failing (as that may not be always obvious), and what the argument is
; supposed to contain conceptually.
; If it does error-out, a default error message is always included that
; tells the user which register should have been used
load:	MACRO
	IF arg1_IS_NUMBER
		PRINTT "\n====================================\n"
		PRINTT	"first argument must be register or pair (a, b, hl,... )"
		FAIL	"\n====================================\n"
	ENDC
	IF arg2_NOT_NUMBER
		IF STRCMP(STRUPR("\1"), STRUPR("\2")) != 0
			PRINTT "\n====================================\n"
			IF _NARG == 3
				PRINTT	"\3"
			ENDC
			PRINTT "\narg must be \1 or hard-coded #. Got \2 \n"
			FAIL	"\n====================================\n"
		ENDC
	ENDC
	IF STRIN("\2", "[") && STRIN("\2", "]") && STRIN("[BC][DE][HL][C][$FF00+C][$FF00 +C][$FF00+ C][$FF00 + C]",STRUPR("\2"))
		; this means the user did something like `load b, [hl]`
		; NOTE THAT WE STILL ALLOW load a, [$FFFF]
		FAIL	"ld \1, \2 will NOT work"
	ENDC
	IF STRCMP(STRUPR("\1"), STRUPR("\2")) != 0 ; only load register / value
		ld	\1, \2		; IF register 1 != register 2
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
; at this point, you cannot "ld	a, \2" and expect it to work if \2 is any
; register. Hence, ldhard gives you peace of mind that the programmer will
; be unable to compile if he passes in a register where you've specified
; a hard-coded # must be supplied
ldhard: MACRO
	IF arg1_IS_NUMBER
		PRINTT	"\n====================================\n"
		IF _NARG == 3
			PRINTT	"\3"
		ENDC
		PRINTT	"arg1 should be a register / pair. Got \1 instead"
		FAIL	"\n====================================\n"
	ENDC
	IF arg2_NOT_NUMBER
		PRINTT	"\n====================================\n"
		IF _NARG == 3
			PRINTT	"\3"
		ENDC
		PRINTT	"arg2 should be a hard-coded #. Got \2 instead"
		FAIL	"\n====================================\n"
	ENDC
	ld	\1, \2
	ENDM



; this is mostly used by other macros to ensure fast rotation
; used to call RL [register], but calls RLA (faster) if it's just register A
RLfast: MACRO
	IF _NARG != 1
		FAIL "RLfast takes only 1 argument"
	ENDC
	IF STRCMP("A", STRUPR("\1")) == 0
		RLA
	ELSE
		RL	\1
	ENDC
	ENDM

; used to call RR [register], but calls RRA (faster) if it's just register A
RRfast: MACRO
	IF _NARG != 1
		FAIL "RRfast takes only 1 argument"
	ENDC
	IF STRCMP("A", STRUPR("\1")) == 0
		RRA
	ELSE
		RR	\1
	ENDC
	ENDM


; jr_inverse is just like jr, except it tests for the opposite flag
; i.e. jr_inverse c, rDMA ==> jr nc, rDMA
; this is most useful in the if_flag macro
jr_inverse: MACRO
	IF (STRCMP("C", STRUPR("\1")) == 0)
	jr	nc, \2
	ENDC
	IF (STRCMP("NC", STRUPR("\1")) == 0)
	jr	c, \2
	ENDC
	IF (STRCMP("Z", STRUPR("\1")) == 0)
	jr	nz, \2
	ENDC
	IF (STRCMP("NZ", STRUPR("\1")) == 0)
	jr	z, \2
	ENDC
	IF (STRCMP("Z", STRUPR("\1")) != 0) && (STRCMP("C", STRUPR("\1")) != 0)
		IF (STRCMP("NC", STRUPR("\1")) != 0) && (STRCMP("NZ", STRUPR("\1")) != 0)
			FAIL "\n\1 is not a valid flag\n"
		ENDC
	ENDC
	ENDM




	ENDC	; end macro-syntax.inc defines
