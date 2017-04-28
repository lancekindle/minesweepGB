;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; holds interrupt routines
include	"syntax.asm"

	IF !DEF(IRQ_ASM)
IRQ_ASM	SET	1

; set lcd vblank irq (but don't actually enable interrupts)
irq_EnableVBLANK: MACRO
	push	af
	ld	[rIE], a	; grab current irq
	or	IEF_VBLANK	; add vblank to current interrupts
	ld	[rIE], a
	pop	af
	ENDM

; enable LCD interrupt when at a specific line
; requires line # from 0-153. (lines 140-153 are during vblank, so avoid those)
irq_EnableLCDC_AtLine: MACRO
	push	af
	push	hl
	load	a, \1
	; select line-interrupt to occur on line \1 (first argument)
	ld	[rLYC], a
	; set LCD to throw line# interrupt
	ld	hl, rSTAT
	lda	[hl]
	or	STATF_LYC	; enable LY-compare
	ld	[hl], a		; set lcd to throw line-interrupt
	; set interrupt flag to accept lcd line-interrupt
	ld	hl, rIE
	lda	[hl]	; get state of interrupts
	or	IEF_LCDC	; enable lcd line-interrupt
	ld	[hl], a
	pop	hl
	pop	af
	ENDM



	ENDC	; end IRQ defines
