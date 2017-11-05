
	IF ! DEF(DMA_INC)
DMA_INC SET	1

DMACODELOC	EQU	$ff80
; end location was experimentally determined
DMA_END_LOC	EQU	$ff80 + 13

; copies the dmacode to HIRAM. dmacode will get run each Vblank,
; and it is resposible for copying sprite data from ram to vram.
; dma_Copy2HRAM trashes all registers
; actual dma code preserves all registers
dma_Copy2HRAM: MACRO
; we include memory.asm INSIDE this macro call because dma.inc by design,
; generates no code in the ROM when included.
; This is important because DMACODELOC is referenced before we can load the
; actual dmacode. So we must use only macros & defines in this file
include "memory.asm"
	jr	.dmaend\@
.dmacode\@
	push	af
	ld	a, OAMDATALOCBANK	; OAMDATA... from sprite.inc
	ldh	[rDMA], a
	ld	a, $28 ; countdown until DMA is finishes, then exit
.dma_wait\@			;<-|
	dec	a		;  |	keep looping until DMA finishes
	jr	nz, .dma_wait\@ ; _|
	pop	af
	ret	; if this were jumped to by the v-blank interrupt, we'd
		; want to reti (re-enable interrupts) instead.
.dmaend\@
.copy_dma_into_memory\@
	ld	de, DMACODELOC
	ld	hl, .dmacode\@
	ld	bc, .dmaend\@ - .dmacode\@
	call	mem_Copy
	ENDM
; =============================================================
; ----------- (In Depth) So what does DMA do? -----------------
; =============================================================
; DMA (on the original gameboy) is a specific memory-copying routine
; that is 2x+ faster at copying memory from a specified source to a
; hard-coded destination of $FE00-$FE9F (where OAM resides).
; because sprites / objects depend on OAM manipulation, we write to
; OAM all throughout game logic. Except there's a catch: OAM is
; inaccessible a lot due to the LCD drawing routine. So almost every
; game sets aside 160 bytes in normal RAM that they write to. Then,
; every Vertical-Blank (when OAM is accessible), we initiate DMA to
; copy the 160 bytes from RAM into OAM. The value we write to [rDMA]
; is the MSB version of the source address. If we write $80,
; then DMA copies 160 bytes from $8000 into OAM.
; while DMA performs it's copying routine, the cpu is still active,
; but unable to access the ROM, or any other memory aside from HRAM.
; That's why this routine gets copied into HRAM and performs a
; for-loop before exiting. It's timed so that DMA is complete when
; it exits. In this case, DMA takes 162 Machine Cycles.
; The CGB has additional DMA routines, such as HDMA which copies 16
; bytes every H-blank


	ENDC	; end dma.inc define
