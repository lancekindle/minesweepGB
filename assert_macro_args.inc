;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
	IF !DEF(ASSERTIONS_INC)
ASSERTIONS_INC	SET	1

include "if_conditionals.inc"

ASSERT_IS_REGISTER: MACRO
; assert that passed argument(s) is/are a single register. Fails if the
; argument is either a number or defined constant. Each argument MUST
; be a register, NOT a register pair
	IF !arg1_IS_REGISTER
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should be register. Got \1 instead."
	ENDC
	ENDM

ASSERT_IS_REG_PAIR: MACRO
; assert that passed argument(s) is/are a register pair.
; Fails if the argument is a #, defined constant, or a single register.
; WARNING: fails if argument is AF   because AF is never used except
; for pushing and popping
	IF !arg1_IS_REG_PAIR
		IF STRIN("-AF-",STRUPR("\1"))>=1
			WARN "arg was AF. This macro fails on AF"
		ENDC
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should be a register pair. Got \1."
	ENDC
	ENDM

ASSERT_IS_NUMBER: MACRO
; assert that passed argument(s) is a #. (basically not a register/pair)
	IF !arg1_IS_NUMBER
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should be a number. Got \1."
	ENDC
	ENDM

ASSERT_NOT_REGISTER: MACRO
; assert that passed argument(s) is/are a number or defined constant.
; Fails if the argument is a register. WARNING: WILL NOT FAIL IF arg
; is a register pair like HL, BC, DE
	IF !arg1_NOT_REGISTER
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should be #, NOT a register. Got \1."
	ENDC
	ENDM

ASSERT_NOT_REG_PAIR: MACRO
; assert that passed argument(s) is/are a #, constant, or single register.
; Fails if the argument is a register pair. WARNING: also fails if it's AF
; because when we're checking for NOT register pairs, getting AF would be
; bad.
	IF arg1_IS_ANY_REG_PAIR
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should NOT be a register pair. Got \1."
	ENDC
	ENDM

ASSERT_NOT_NUMBER: MACRO
; assert that passed argument(s) is NOT a #. (basically any register/pair)
	IF !arg1_NOT_NUMBER
		IF _NARG == 2
			FAIL "\2"
		ENDC
		FAIL "Arg should NOT be a number. Got \1."
	ENDC
	ENDM

ASSERT_EQUAL: MACRO
; assert that two passed arguments are equal
	IF _NARG <= 1
		FAIL "ASSERT_EQUAL requires 2 arguments"
	ENDC
	IF STRCMP(STRUPR("\1"), STRUPR("\2")) != 0
		IF _NARG == 3
			FAIL "\3"
		ENDC
		FAIL "Arguments should be equal. Got \1\nand\n\2\n"
	ENDC
	ENDM

ASSERT_NOT_EQUAL: MACRO
; assert that two passed arguments are NOT equal
	IF _NARG <= 1
		FAIL "ASSERT_NOT_EQUAL requires 2 arguments"
	ENDC
	IF STRCMP(STRUPR("\1"), STRUPR("\2")) == 0
		IF _NARG == 3
			FAIL "\3"
		ENDC
		FAIL "Arguments should not be equal. Got \1\nand\n\2\n"
	ENDC
	ENDM



	ENDC	; end assertions.inc define
