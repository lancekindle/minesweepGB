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
; nonetheless, arguments 2-9 are treated as one command such that the macro
; MoveXY is called with both arguments.

; some abbreviations I may use:
; arg = argument  (passed parameter)
; fxn = function


	IF	!DEF(SYNTAX_ASM)
SYNTAX_ASM	SET	1


ret_true: MACRO
	SCF     ; set carry flag
	ret
	ENDM

ret_false: MACRO
	SCF
	CCF     ; compliment carry flag. (toggle it's value)
	ret
	ENDM


; functions to get true/false, rather than the macros
; this allows you to CALL get_true, whereas you'd just run ret_true
get_true:
	ret_true

get_false:
	ret_false


; if_* MACROs take 2 arguments.
; 1) a function to call. Depending on if it returns true or not,
; 2) a command to run depending on return condition of function
if_: MACRO
	call \1
	jr nc, .end_if_\@
	run_2nd_arg_cmd
.end_if_\@
	ENDM

if_not: MACRO
	call \1
	jr c, .end_if_not\@
	run_2nd_arg_cmd
.end_if_not\@
	ENDM


; first of all, let me apologize that this mess below is nearly the first thing
; you see in this file. But it plays an integral part to my macros. The args2-9
; are one-line macros that get joined into the overall one-line macro
; `run_2nd_arg_cmd`. This is responsible for handling all command arguments
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
; For clarity, I'll show you what args2 looks like in it's exanded form
; IF _NARG == 3
; 	\2, \3
; ENDC
arg1	EQUS	"IF _NARG==2\n \\2\nENDC\n"
arg2	EQUS	"IF _NARG==3\n \\2,\\3\nENDC\n"
arg3	EQUS	"IF _NARG==4\n \\2,\\3,\\4\nENDC\n"
arg4	EQUS	"IF _NARG==5\n \\2,\\3,\\4,\\5\nENDC\n"
arg5	EQUS	"IF _NARG==6\n \\2,\\3,\\4,\\5,\\6\nENDC\n"
arg6	EQUS	"IF _NARG==7\n \\2,\\3,\\4,\\5,\\6,\\7\nENDC\n"
arg7	EQUS	"IF _NARG==8\n \\2,\\3,\\4,\\5,\\6,\\7,\\8\nENDC\n"
arg8	EQUS	"IF _NARG==9\n \\2,\\3,\\4,\\5,\\6,\\7,\\8,\\9\nENDC\n"

run_2nd_arg_cmd	EQUS	"{arg1}{arg2}{arg3}{arg4}{arg5}{arg6}{arg7}{arg8}"


; preserve & restore register pair after fxn call. \1 can be: af, bc, de, hl
; \2 is FULL command. such as CALL xyz or a macro. BUT you can only pass 1
; argument into macro due to a limitation (cannot use commas (,) in 2nd arg)
preserve: MACRO
	push \1
	run_2nd_arg_cmd
	pop \1
	ENDM




	ENDC  ; end syntax file
