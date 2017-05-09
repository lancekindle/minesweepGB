;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; holds interrupt routines
include	"syntax.asm"
include "vars.asm"

	IF !DEF(IRQ_ASM)
IRQ_ASM	SET	1

; IRQ is responsible for handling all interrupts on the GameBoy


	var_HighRamByte	irq_Vblank_JumpAddrLSB
	var_HighRamByte	irq_Vblank_JumpAddrMSB
	var_HighRamByte	irq_LCDC_JumpAddrLSB
	var_HighRamByte	irq_LCDC_JumpAddrMSB

; set lcd vblank irq (but don't actually enable interrupts)
irq_EnableVBLANK: MACRO
	push	af
	ldh	a, [rIE]	; grab current irq
	or	IEF_VBLANK	; add vblank to current interrupts
	ldh	[rIE], a
	pop	af
	ENDM

; set lcd vblank irq (but don't actually enable interrupts)
irq_DisableVBLANK: MACRO
	push	af
	ldh	a, [rIE]	; grab current irq
	and	%11111111 ^ IEF_VBLANK	; remove vblank from current interrupts
				; (and it with xor'ed VBLANK flag)
	ldh	[rIE], a
	pop	af
	ENDM

; used to disable all known interrupts (IRQ's).
irq_DisableAll: MACRO
	push	af
	xor	a
	ldh	[rIE], a
	pop	af
	ENDM

; enable LCD interrupt when at a specific line
; requires line # from 0-153. (lines 140-153 are during vblank, so avoid those)
irq_EnableLCDC_AtLine: MACRO
	push	af
	load	a, \1
	; select line-interrupt to occur on line \1 (first argument)
	ldh	[rLYC], a
	; set LCD to throw line# interrupt
	ldh	a, [rSTAT]
	or	STATF_LYC	; enable LY-compare
	ldh	[rSTAT], a		; set lcd to throw line-interrupt
	; set interrupt flag to accept lcd line-interrupt
	ldh	a, [rIE]
	or	IEF_LCDC	; enable lcd line-interrupt
	ldh	[rIE], a
	pop	af
	ENDM

; disable LCD line-interrupt
irq_DisableLCDC: MACRO
	push	af
	; disable LCD from throwing interrupt
	ldh	a, [rSTAT]
	and	%11111111 ^ STATF_LYC	; disable LY-compare
	ldh	[rSTAT], a	; disable lcd from throwing line-interrupt
	; disable gameboy from responding to LCDC interrupt
	ldh	a, [rIE]	; grab current irq
	and	%11111111 ^ IEF_LCDC	; remove lcdc from current interrupts
	ldh	[rIE], a
	pop	af
	ENDM

; handle vblank interrupt. Pushes all registers and calls the routine address
; stored in memory. Once that function returns, HandleVBLANK restores all
; registers and then resumes the interrupted execution
irq_HandleVBLANK:
	pushall
	ld	hl, irq_Vblank_JumpAddrLSB
	ld	c, [hl]
	inc	hl
	ld	h, [hl]	; load MSB in H
	ld	l, c	; move LSB to L
	call	irq_Jump2HL
	popall
	reti


; handles LCDC interrupt. (handles them same as vblank)
irq_HandleLCDC:
	pushall
	ld	hl, irq_LCDC_JumpAddrLSB
	ld	c, [hl]
	inc	hl
	ld	h, [hl]	; load MSB in H
	ld	l, c	; move LSB to L
	call	irq_Jump2HL
	popall
	reti


; sets which function will handle the vblank (as delegated by irq_HandleVBLANK)
; The important thing is that you call this macro with the handling address
; as the first argument.
; The Handling function should return but NOT enable interrupts.
; The Handling function does not need to preserve registers
; Both reti and push/pop all is handled by irq_HandleVBLANK
irq_CallFromVBLANK: MACRO
	push	bc
	push	hl
	ld	bc, \1
	ld	hl, irq_Vblank_JumpAddrLSB
	ld	[hl], c
	inc	hl
	ld	[hl], b
	pop	hl
	pop	bc
	ENDM


; sets address of function to call when handling LCDC (line-interrupt on lcd)
; this is often used for pre-vblank prep code OR line-specific code such as
; enabling / disabling window or cool screen effects
; call this macro with the address of routine as argument
irq_CallFromLCDC: MACRO
	push	bc
	push	hl
	ld	bc, \1
	ld	hl, irq_LCDC_JumpAddrLSB
	ld	[hl], c
	inc	hl
	ld	[hl], b
	pop	hl
	pop	bc
	ENDM


; push BC onto the stack, then ret, thus effectively jumping to BC
; you call this routine from another function from which you wish to
; return. Thus, this jumps to [BC] and the function at [BC] returns
; to the orginal calling function when it's done
irq_Jump2BC:
	push	bc
	ret	; which jumps to bc

; call irq_Jump2HL to effectively call HL
; because this'll jump to HL, and the target function can return back to the
; caller of jump2HL
irq_Jump2HL:
	jp	[hl]


	ENDC	; end IRQ defines
