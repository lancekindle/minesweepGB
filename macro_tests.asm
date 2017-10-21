include "gbhw.inc"

;gb_header
section "Vblank", HOME[$0040]
	reti
section "LCDC", HOME[$0048]
	reti
section "Timer_Overflow", HOME[$0050]
	reti
section "Serial", HOME[$0058]
	reti
section "joypad_p1_p4", HOME[$0060]
	reti
section "start", HOME[$0100]
	nop
	jp begin

; apparently the difference between defining a macro and calling one is
; the indent. If indented, we are calling the macro. If not, we are defining it
; UGH. After any macro call / command, DO NOT INCLUDE A COMMA! Commas only
; separate 2+ arguments. First argument doesn't have a comma separating it.
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT

; include .asm files here (asm includes actual code. inc just defines stuff)
; we need to add it here after all the critical address-specific code
; has been laid out
include "syntax.inc"
; write the rom header



;-----------------
string_compare_example: MACRO
	IF (STRCMP("c", "\1") == 0)  ; strcmp returns 0 if it matches
	if_not_flag	nc, \2
	ENDC
	ENDM

blah: MACRO
	IF (strcmp(strsub("\1", 1, 4), "nc") == 0 )
	\2
	ENDC
	ENDM

newline: MACRO
	PRINTT "\n"
	ENDM
; arg1 should be a flag
; arg2 should be two flags separated by and
test: MACRO
	PRINTT "\1"
	PRINTT "\2"
	PRINTT "\3"
	;IF STRCMP("and", strsub("\2", 1, 4)) = 0
	; strsub(<string>, start, #of characters) ???
	newline
	PRINTT "`\2` substring @ 2 for 3 chars:"
	newline
	PRINTT STRSUB("\2", 2, 3)  ; THIS gives "and". because 3 is # of chars
	newline
	PRINTT STRSUB("\2", 2, STRLEN("\2") - 2)
	newline
	PRINTV STRLEN("\2")
	newline

	IF STRCMP(STRSUB("\2", 2, 3), "and") == 0
	PRINTT "it's a match\n"
	ELSE
	PRINTT "\2 ... uh.."
	ENDC
	ENDM

test_chars_allowed: MACRO
	newline
	IF (_NARG >= 1)
	PRINTT "\1"
	ENDC
	newline
	IF (_NARG >= 2)
	PRINTT "\2"
	ENDC
	newline
	IF (_NARG >= 3)
	PRINTT "\3"
	ENDC
	newline
	ENDM











begin:
	di    ; disable interrupts
	nop
	stop
	test this, is, a test
	test_chars_allowed	a, <>, b
