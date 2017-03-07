;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
include "test_includes.asm"
include "vars.asm"
include "syntax.asm"
include "stack.asm"

; verify that pointer Inited in ram points to the stack
; exactly equal to our hard-coded value of "stack"
verify_stack_address_at_beginning: MACRO
	ld	hl,	\1_stack_topL	; HL points to LSB in ram
	ld	c, [hl]
	increment	hl
	ld	b, [hl]		; load BC with stack_pointer from ram
	ldpair	h,l,	b,c	; place stack_pointer (from ram) into hl
	if_not_hl	\1, .failed	; verify that HL (ptr from ram)
					; equals stack (ptr from compiler)
	ENDM

verify_stack_address_at_end: MACRO
	ld	hl,	\1_stack_topL	; HL points to LSB in ram
	ld	c, [hl]
	increment	hl
	ld	b, [hl]		; load BC with stack_pointer from ram
	ldpair	h,l,	b,c	; place stack_pointer (from ram) into hl
	; verify that HL (ptr from ram) == stack_end (ptr from compiler)
	if_not_hl	\1_stack_end, .failed	
	ENDM


; verify that stack declare doesn't throw syntax errors
; verify stack init sets stack ptr in ram
test_31_stack_DeclareInit:
	stack_Declare	stack, 5
	stack_Init	stack
	verify_stack_address_at_beginning	stack
.passed
	TestPassed	3, 1
.failed
	TestFailed	3, 1


; we use the previously Declared and Inited stack
; to test Push and Pop. Verify that values Pushed on are returned FILO-order
; First In, Last Out (FILO)
test_32_stack_PushPop:
	stack_Push	stack, 11	; push 11 onto stack
	stack_Pop	stack
	ifa	<>, 11, jp .failed	; verify 11 was popped from stack
	; verify ptr is back @ start
	verify_stack_address_at_beginning	stack
	stack_Push	stack, 1
	stack_Push	stack, 2
	stack_Push	stack, 3
	stack_Pop	stack
	ifa	<>, 3, jp .failed
	stack_Pop	stack
	ifa	<>, 2, jp .failed
	stack_Pop	stack
	ifa	<>, 1, jp .failed
.passed
	TestPassed	3, 2
.failed
	TestFailed	3, 2


; verify that stack won't go past start or end of defined ram-limits
; also verify that it throws false (Carry-flag=0) when popping or pushing
; past it's limits
; Finally, verify that pushing additional items on an already-full stack
; does not modify the stack
test_33_stack_Boundaries:
	verify_stack_address_at_beginning	stack
	stack_Pop	stack		; attempt to Pop from an empty stack
	if_flag	c, jp .failed		; pop from empty should throw CY=0
	verify_stack_address_at_beginning	stack
	stack_Push	stack, 5
	if_flag	nc, jp .failed		; carry flag == 1 for successful op
	stack_Push	stack, 4
	stack_Push	stack, 3
	stack_Push	stack, 2
	if_flag	nc, jp .failed
	stack_Push	stack, 1	; this should fill up stack
	if_flag	nc, jp .failed
	verify_stack_address_at_end	stack
	stack_Push	stack,	99	; attempt to Push to a full stack
	if_flag c, jp .failed		; CY=0 since we failed to push
	verify_stack_address_at_end	stack
	; attempt to trash stack by pushing values even though we're at
	; the end of the stack
	stack_Push	stack, 87
	stack_Push	stack, 23
	stack_Push	stack, 52
	; now we verify that values are untouched
	stack_Pop	stack
	if_flag	nc, jp .failed		; verify successful operation (CY=1)
	ifa	<>, 1, jp .failed
	stack_Pop	stack
	ifa	<>, 2, jp .failed
	stack_Pop	stack
	ifa	<>, 3, jp .failed
	stack_Pop	stack
	ifa	<>, 4, jp .failed
	stack_Pop	stack
	if_flag	nc, jp .failed		; again verify that a successful
					; operation throws true (carry-flag=1)
	ifa	<>, 5, jp .failed
	stack_Pop	stack
	if_flag	c, jp .failed		; pop empty should throw CY=0
.passed
	TestPassed	3, 3
.failed
	TestFailed	3, 3


; verify that pushing and popping a word succeeds or fails in chunks of 2 bytes
; and that you can pop into BC, DE, or HL
test_34_stack_PushPop_Word:
	verify_stack_address_at_beginning	stack
	; test that you can pass B,C
	ld	bc, $BBCC
	stack_Push	stack, c, b
	ld	bc, $0000	; zero out bc
	stack_Pop	stack, bc
	lda	b
	ifa	<>, $BB, jp .failed
	lda	c
	ifa	<>, $CC, jp .failed
	ld	bc, $DDEE
	; verify you can pop into DE
	stack_Push	stack, c, b
	stack_Pop	stack, de
	lda	d
	ifa	<>, $DD, jp .failed
	lda	e
	ifa	<>, $EE, jp .failed
	; verify pop into HL
	ld	bc, $1122
	stack_Push	stack, c, b
	stack_Pop	stack, hl
	lda	h
	ifa	<>, $11, jp .failed
	lda	l
	ifa	<>, $22, jp .failed
	; we've confirmed that stack successfully pushes & pops BC
	verify_stack_address_at_beginning	stack
	stack_Push	stack, $11, $22	; push a word
	stack_Push	stack, $33	; push a single value
	stack_Pop	stack, bc	; pop off a word. Should get 33,22
	lda	b
	ifa	<>, $33, jp .failed
	lda	c
	ifa	<>, $22, jp .failed
	stack_Pop	stack	; pop off that last word
	verify_stack_address_at_beginning	stack
.passed
	TestPassed	3, 4
.failed
	TestFailed	3, 4


; verify that pushing a word succeeds only if both bytes are pushed onto
; stack. Similarly, verify that popping succeeds only if it can pop 2 bytes
test_35_stack_PushPop_WordBoundaries:
	verify_stack_address_at_beginning	stack
	stack_Pop	stack, bc
	jp	c, .failed	; if true-flag set, then it thinks it can pop2
				; from an empty stack
	stack_Push	stack, $AA	; push a single value
	stack_Pop	stack, bc	; again try to pop 2 bytes
	jp	c, .failed ; fail (again) if stack pops2 from size-1 stack
	stack_Push	stack, $22	; push a second value
	stack_Pop	stack, bc
	jp	nc, .failed	; fail this time if Pop2 doesn't work
	;BC should contain $22AA
	lda	B
	ifa	<>, $22, jp .failed
	lda	C
	ifa	<>, $AA, jp .failed
	; We've now confirmed that stack_Pop won't pop until at least 2 bytes
	; are on the stack if BC is passed as an argument
	stack_Push	stack, $AA, $BB		; 2/5 filled
	stack_Push	stack, $CC, $DD		; 4/5 filled
	if_flag	nc, jp .failed	; flag should indicate success
	stack_Push	stack, $EE, $FF	; this one should fail
	if_flag	c, jp .failed	; flag should indicate failure
	stack_Push	stack, $00	; 5/5 filled
	if_flag	nc, jp .failed	; previous operation should have succeeded
	; now pop values
	stack_Pop	stack, bc
	lda	b
	ifa	<>, $00, jp .failed
	lda	c
	ifa	<>, $DD, jp .failed
	stack_Pop	stack, bc
	lda	b
	ifa	<>, $CC, jp .failed
	lda	c
	ifa	<>, $BB, jp .failed
	stack_Pop	stack, bc	;1/5 filled. should fail
	if_flag	c, jp .failed
	stack_Pop	stack, a
	ifa	<>, $AA, jp .failed
.passed
	TestPassed	3, 5
.failed
	TestFailed	3, 5


; verify that pushing and popping a 3 bytes succeeds or fails in 3-byte chunks
test_36_stack_PushPop_3bytes_and_Boundaries:
	verify_stack_address_at_beginning	stack
.test_pop_boundary
	; first test that 1, 2-filled values cannot be popped until 3 bytes
	stack_Pop	stack, abc
	if_flag	c, jp .failed	; can't pop from an empty stack
	stack_Push	stack, $00
	stack_Pop	stack, abc	; can't pop 3bytes from a 1-byte stack
	if_flag	c, jp .failed
	stack_Push	stack, $00
	stack_Pop	stack, abc	; can't pop 3bytes from a 2-byte stack
	if_flag	c, jp .failed
	stack_Push	stack, $00
	stack_Pop	stack, abc	; can't pop 3bytes from a 2-byte stack
	if_flag	nc, jp .failed		; that should pass
	ld	a, $AA
	ld	bc, $BBCC
	stack_Push	stack, c, b, a
	ld	bc, 0	; zero out bc
	ld	a, 0	; zero out a
	stack_Pop	stack, abc
	ifa	<>, $AA, jp .failed
	lda	b
	ifa	<>, $BB, jp .failed
	lda	c
	ifa	<>, $CC, jp .failed
	; we've confirmed that stack successfully pushes & pops ABC
	; verify that pushes in chunks can combine into one pop
	verify_stack_address_at_beginning	stack
	stack_Push	stack, $11, $22	; push a word
	stack_Push	stack, $33	; push a single value
	stack_Pop	stack, ABC	; should get 33, 22, 11
	ifa	<>, $33, jp .failed
	lda	b
	ifa	<>, $22, jp .failed
	lda	c
	ifa	<>, $11, jp .failed
	verify_stack_address_at_beginning	stack
	stack_Pop	stack, ABC	; should fail
	if_flag	c, jp .failed
.fillup
	stack_Push	stack, a	; I don't care what value I push here.
	if_flag	c, jp .fillup
	; we get here when the stack is totally full
.test_push_boundary
	stack_Pop	stack, ABC	; make room for one push
	; TEST PUSH BOUNDARIES (1,2 remaining bytes should fail)
	stack_Push	stack, $33	; push a single value
	if_flag	nc, jp .failed
	stack_Push	stack, c,b,a
	if_flag	c, jp .failed		; pushing 3 fails when there's only 2 bytes left
	; test with only 1 byte of space available
	stack_Push	stack, $33	; push a single value
	if_flag	nc, jp .failed
	stack_Push	stack, c,b,a
	if_flag	c, jp .failed		; pushing 3 fails when there's only 1 byte left
	; test with 0 bytes of space available
	stack_Push	stack, $33	; push a single value
	if_flag	nc, jp .failed
	stack_Push	stack, c,b,a
	if_flag	c, jp .failed		; pushing 3 fails when there's only 0 bytes left
	stack_Push	stack, $33	; push a single value
	if_flag	c, jp .failed	; verify that stack is completely full
.test_pushes_and_pops
	stack_Pop	stack, ABC	; make room for one push
	ld	a, $AA
	ld	bc, $BBCC
	stack_Push	stack, c,b,a
	if_flag	nc, jp .failed	; that push should succeed
	stack_Push	stack, c,b,a	; this push should fail
	if_flag	c, jp .failed
	stack_Pop	stack	; remove A value
	ld	a, $11
	stack_Push	stack, a
	ld	a, $99	; overwrite A value
	ld	bc, $FFFF	; overwrite BC value (just in case)
	stack_Pop	stack, ABC	; ABC => $11,BBCC
	ifa	<>, $11, jp .failed
	lda	b
	ifa	<>, $BB, jp .failed
	lda	c
	ifa	<>, $CC, jp .failed
.cleanup
	; need to empty stack for next test
	stack_Pop	stack
	if_flag	c, jp .cleanup
.passed
	TestPassed	3, 6
.failed
	TestFailed	3, 6


; verify that registers are restored upon push fail. This is important
; because if a push fails, we may want to try again with the same values
; It'll add dramatic overhead if we have to constantly preserve those
; registers prior to attempting to push. Instead, we just detect a push
; fail, and then we can push those values onto SP and work to clear up
; space
test_37_stack_PushFail_PreservesPushingRegisters:
	verify_stack_address_at_beginning	stack
.pushA
	ld	a, $AA
	stack_Push	stack, A
	if_flag	c, jp .pushA
	; we get here when the stack-push fails
	; verify register A has kept its value
	ifa	<>, $AA, jp .failed
	stack_Pop	stack, BC	; make some room
	stack_Pop	stack, BC	; for next pushing test
.pushBC
	ld	bc, $BBCC
	stack_Push	stack, C,B
	if_flag	c, jp .pushBC
	; we get here when the stack-push fails
	; verify registers B,C remain unaltered
	lda	b
	ifa	<>, $BB, jp .failed
	lda	c
	ifa	<>, $CC, jp .failed
	stack_Pop	stack, BC	; make some room
	stack_Pop	stack, BC	; for next pushing test
.pushABC
	ld	a, $AA
	ld	bc, $BBCC
	stack_Push	stack, C,B,A
	if_flag	c, jp .pushABC
	; we get here when the stack-push fails
	; verify registers A,B,C remain unaltered
	ifa	<>, $AA, jp .failed
	lda	b
	ifa	<>, $BB, jp .failed
	lda	c
	ifa	<>, $CC, jp .failed
.cleanup
	; need to empty stack for next test
	stack_Pop	stack
	if_flag	c, jp .cleanup
.passed
	TestPassed	3, 7
.failed
	TestFailed	3, 7



