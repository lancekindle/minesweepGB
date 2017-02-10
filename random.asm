	IF	!DEF(random_asm)
random_asm	SET	1


include "syntax.asm"
include "vars.asm"

	var_LowRamByte	random_LoByte1	; declare holding space for random#
	var_LowRamByte	random_LoByte2

; load a new random # in A
; COST: 44/30
rand_A: MACRO
	push	BC
	call	random_xorshift_update
	pop	BC
	ENDM

; trashes AF. Injects the value of A into random vars
rand_seed: MACRO
	push	hl
	ld	hl, random_LoByte1
	adc	a, [hl]
	ld	[hl], a
	pop	hl
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
	lda	[random_LoByte1]
	adc	a	; Ax2 + 1. Basically a shift left by 1  (x<<1)
	ld	b, a
	swap	a	; swap lower and upper nibble. Same as y>>4
	xor	b	; xor's R ^ R>>3
	ld	b, a	; hold rand#1
	ld	a, [random_LoByte2]
	ld	c, a	; hold rand#2
	add	b	; rand#1 = rand#2 + xor-shifted-rand#1
	ld	[random_LoByte2], a	; store rand#1  (in rand#2's ram spot)
	lda	[rDIV]	; load divider register (time-based number that inc's)
	adc	b	; add rand#1 (pre-addition) + CY
	sub	c	; rand#2 = [rDIV + rand#1(pre-add)] - rand#2
	ld	[random_LoByte1], a	; store rand#2  (in rand#1's ram spot)
	ret



	ENDC	; end random.asm definition
