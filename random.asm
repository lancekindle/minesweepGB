	IF	!DEF(random_asm)
random_asm	SET	1


include "syntax.asm"
include "vars.asm"

	var_LowRamByte	random_LoByte	; declare holding space for random#

; load a new random # in A
; COST: 44/30
rand_A: MACRO
	push	BC
	call	random_xorshift_update
	pop	BC
	ENDM

; xorshift generates random #'s, mixing in with carry-flag and rDIV
; rDIV is a 0-255 value that gets auto-incremented at 16kHz by the CPU
; USES:	AF, BC
; EXIT:	A contains random #
; performs the following calculation (^ = xor):
; x = previous-random# + [rDIV] + carry-flag 
; y = x ^ x<<7
; z = y ^ y>>5
; R = z ^ z<<3		; R is the final random #
; COST:	31/25 (outdated)
; call this function at end of each mainloop if you want to ensure that
; a random # is generally random once a user presses a button
random_xorshift_update:
	lda	[random_LoByte]
	adc	a	; Ax2 + 1. Basically a shift left by 1  (x<<1)
	ld	b, a
	RRCA
	RRCA
	RRCA	; rotate right 3. (y>>3)
	xor	b
	ld	b, a
	lda	[rDIV]	; load divider register (time-based number that inc's)
	add	b
	ld	[random_LoByte], a	; store newly-generated random #
	ret



	ENDC	; end random.asm definition
