;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(IF_CONDITIONALS_INC)
IF_CONDITIONALS_INC	SET	1

; if_conditionals.inc defines a set of string equates for use in your macros.
; use these right after an IF. RGBD's string expansion will take it from there.
; --
; Say you want to verify that argument \2 within your macro is a register
; you could write
; IF STRIN("ABCDEHL", "\2") == 0
; 	FAIL "argument two should be a register. Got \2 instead."
; ENDC
; and then wherever you need to verify more arguments, use this same string.
; but if there's an error, now you have to change all versions of this string.
; OOPS - we should probably use STRUPR("\2") or else a lowercase 'a' will not
; get detected as a register. OOPS - we should probably separate those
; characters like so: "A.B.C.D.E.H.L" or else we may end up allowing a
; register pair like 'HL', instead of only allowing a single register like 'C'
; here's where IF_CONDITIONALS come in handy:
; --
; IF arg2_NOT_REGISTER
;	FAIL "argument 2 should be register. Got \2 instead"
; ENDC
; --
; It standardizes the typical use-cases for macro argument checking, and is
; READABLE! arg2_NOT_REGISTER is much easier to understand than
; !STRIN("A.B.C.D.E.H.L",STRUPR("\2"))
;
; the following if_conditionals exist for arguments 1 - 9:
; argX_IS_REGISTER		; for \X == A, B, C, D, E, H, L
; argX_IS_REG_PAIR		; for \X == BC, DE, HL
; argX_IS_ANY_REG_PAIR		; for \X == AF, BC, DE, HL
; argX_IS_NUMBER		; basically, if \X != any register / pair
; argX_NOT_REGISTER
; argX_NOT_REG_PAIR
; argx_NOT_NUMBER		; basically, if \X == any register / pair


arg1_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\1\"))>=1)"
arg1_NOT_REGISTER	EQUS	"(!{arg1_IS_REGISTER})"
arg1_IS_REG_PAIR	EQUS	"({arg1_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\1\"))>=1)"
arg1_NOT_REG_PAIR	EQUS	"(!{arg1_IS_REG_PAIR})"

; notice that any comparison of arg1 to register(s) never includes F flag reg.
; We only check for AF in the arg1_NOT or IS_NUMBER
; Because no macro that I've written has accepted AF as an argument
; and it's unlikely that most people want AF when verifying \X == register/pair
; argX_IS_ANY_REG_PAIR addresses that shortcoming by including AF
arg1_IS_ANY_REG_PAIR	EQUS	"({arg1_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\1\"))>=1)"

; -----
; if arg1 isn't any reg./pair, then it's a # or a defined constant (also a #)
; this would be the equivalent of argX_NOT_REGISTER && argX_NOT_REG_PAIR
arg1_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\1\"))==0)"

; argX_NOT_NUMBER basically means argX MUST be a register/pair INCLUDING AF
arg1_NOT_NUMBER		EQUS	"(!{arg1_IS_NUMBER})"

;========================
;-- Arg 2 conditionals
;========================
arg2_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\2\"))>=1)"
arg2_NOT_REGISTER	EQUS	"(!{arg2_IS_REGISTER})"
arg2_IS_REG_PAIR	EQUS	"({arg2_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\2\"))>=1)"
arg2_NOT_REG_PAIR	EQUS	"(!{arg2_IS_REG_PAIR})"
; --
arg2_IS_ANY_REG_PAIR	EQUS	"({arg2_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\2\"))>=1)"
; -----
arg2_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\2\"))==0)"
arg2_NOT_NUMBER		EQUS	"(!{arg2_IS_NUMBER})"
;========================
;-- Arg 3 conditionals
;========================
arg3_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\3\"))>=1)"
arg3_NOT_REGISTER	EQUS	"(!{arg3_IS_REGISTER})"
arg3_IS_REG_PAIR	EQUS	"({arg3_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\3\"))>=1)"
arg3_NOT_REG_PAIR	EQUS	"(!{arg3_IS_REG_PAIR})"
; --
arg3_IS_ANY_REG_PAIR	EQUS	"({arg3_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\3\"))>=1)"
; -----
arg3_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\3\"))==0)"
arg3_NOT_NUMBER		EQUS	"(!{arg3_IS_NUMBER})"
;========================
;-- Arg 4 conditionals
;========================
arg4_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\4\"))>=1)"
arg4_NOT_REGISTER	EQUS	"(!{arg4_IS_REGISTER})"
arg4_IS_REG_PAIR	EQUS	"({arg4_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\4\"))>=1)"
arg4_NOT_REG_PAIR	EQUS	"(!{arg4_IS_REG_PAIR})"
; --
arg4_IS_ANY_REG_PAIR	EQUS	"({arg4_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\4\"))>=1)"
; -----
arg4_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\4\"))==0)"
arg4_NOT_NUMBER		EQUS	"(!{arg4_IS_NUMBER})"
;========================
;-- Arg 5 conditionals
;========================
arg5_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\5\"))>=1)"
arg5_NOT_REGISTER	EQUS	"(!{arg5_IS_REGISTER})"
arg5_IS_REG_PAIR	EQUS	"({arg5_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\5\"))>=1)"
arg5_NOT_REG_PAIR	EQUS	"(!{arg5_IS_REG_PAIR})"
; --
arg5_IS_ANY_REG_PAIR	EQUS	"({arg5_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\5\"))>=1)"
; -----
arg5_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\5\"))==0)"
arg5_NOT_NUMBER		EQUS	"(!{arg5_IS_NUMBER})"
;========================
;-- Arg 6 conditionals
;========================
arg6_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\6\"))>=1)"
arg6_NOT_REGISTER	EQUS	"(!{arg6_IS_REGISTER})"
arg6_IS_REG_PAIR	EQUS	"({arg6_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\6\"))>=1)"
arg6_NOT_REG_PAIR	EQUS	"(!{arg6_IS_REG_PAIR})"
; --
arg6_IS_ANY_REG_PAIR	EQUS	"({arg6_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\6\"))>=1)"
; -----
arg6_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\6\"))==0)"
arg6_NOT_NUMBER		EQUS	"(!{arg6_IS_NUMBER})"
;========================
;-- Arg 7 conditionals
;========================
arg7_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\7\"))>=1)"
arg7_NOT_REGISTER	EQUS	"(!{arg7_IS_REGISTER})"
arg7_IS_REG_PAIR	EQUS	"({arg7_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\7\"))>=1)"
arg7_NOT_REG_PAIR	EQUS	"(!{arg7_IS_REG_PAIR})"
; --
arg7_IS_ANY_REG_PAIR	EQUS	"({arg7_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\7\"))>=1)"
; -----
arg7_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\7\"))==0)"
arg7_NOT_NUMBER		EQUS	"(!{arg7_IS_NUMBER})"
;========================
;-- Arg 8 conditionals
;========================
arg8_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\8\"))>=1)"
arg8_NOT_REGISTER	EQUS	"(!{arg8_IS_REGISTER})"
arg8_IS_REG_PAIR	EQUS	"({arg8_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\8\"))>=1)"
arg8_NOT_REG_PAIR	EQUS	"(!{arg8_IS_REG_PAIR})"
; --
arg8_IS_ANY_REG_PAIR	EQUS	"({arg8_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\8\"))>=1)"
; -----
arg8_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\8\"))==0)"
arg8_NOT_NUMBER		EQUS	"(!{arg8_IS_NUMBER})"
;========================
;-- Arg 9 conditionals
;========================
arg9_IS_REGISTER	EQUS	"(STRIN(\"A.B.C.D.E.H.L\",STRUPR(\"\\9\"))>=1)"
arg9_NOT_REGISTER	EQUS	"(!{arg9_IS_REGISTER})"
arg9_IS_REG_PAIR	EQUS	"({arg9_NOT_REGISTER} && STRIN(\"BC-DE-HL\",STRUPR(\"\\9\"))>=1)"
arg9_NOT_REG_PAIR	EQUS	"(!{arg9_IS_REG_PAIR})"
; --
arg9_IS_ANY_REG_PAIR	EQUS	"({arg9_NOT_REGISTER} && STRIN(\"AF-BC-DE-HL\",STRUPR(\"\\9\"))>=1)"
; -----
arg9_IS_NUMBER		EQUS	"(STRIN(\"AF.BC.DE.HL\",STRUPR(\"\\9\"))==0)"
arg9_NOT_NUMBER		EQUS	"(!{arg9_IS_NUMBER})"


	ENDC	; end if_conditionals file
