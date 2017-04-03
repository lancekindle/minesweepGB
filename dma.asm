
        IF ! DEF(DMA_ASM)
DMA_ASM SET     1

DMACODELOC	EQU	$ff80

; copies the dmacode to HIRAM. dmacode will get run each Vblank,
; and it is resposible for copying sprite data from ram to vram.
dma_Copy2HRAM: MACRO
include "memory.asm"
.copy_dma_into_memory\@
        ld      de, DMACODELOC
        ld	hl, dmacode\@
        ld	bc, dmaend\@ - dmacode\@
        call	mem_CopyVRAM
        jr      dmaend\@
dmacode\@:
        push	af
        ld	a, OAMDATALOCBANK       ; OAMDATA... from sprite.inc
        ldh	[rDMA], a
        ld	a, $28
dma_wait\@:
        dec	a
        jr	nz, dma_wait\@
        pop	af
        ret	;no longer reti.because dma is now handled by a vblank routine
dmaend\@:
        ENDM



        ENDC    ; end dma.asm define
