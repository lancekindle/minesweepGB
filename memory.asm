;*
;* Memory Manipulation Code
;*
;*   Started 16-Aug-97
;*
;* Initials: JF = Jeff Frohwein, CS = Carsten Sorensen
;*
;* V1.0 - 16-Aug-97 : Original Release - JF, most code from CS
;* V1.1 - 29-Nov-97 : Added monochrome font copy. - JF
;*                    Fixed bug in mem_SetVRAM. - JF
;*
;* Library Subroutines:
;*
;* lcd_WaitVRAM -
;*   Macro that pauses until VRAM available.
;*
;* mem_Set -
;*   Set a memory region.
;*    Entry: a = value, hl = start address, bc = length
;*
;* mem_Copy -
;*   Copy a memory region.
;*    Entry: hl = start address, de = end address, bc = length
;*
;* mem_SetVRAM -
;*   Set a memory region in VRAM.
;*    Entry: a = value, hl = start address, bc = length
;*
;* mem_CopyVRAM -
;*   Copy a memory region to or from VRAM.
;*    Entry: hl = start address, de = end address, bc = length
;*

;If all of these are already defined, don't do it again.

        IF      !DEF(MEMORY1_ASM)
MEMORY1_ASM  SET  1

rev_Check_memory1_asm: MACRO
;NOTE: REVISION NUMBER CHANGES MUST BE ADDED
;TO SECOND PARAMETER IN FOLLOWING LINE.
        IF      \1 > 1.1      ; <---- NOTE!!! PUT FILE REVISION NUMBER HERE
        WARN    "Version \1 or later of 'memory.asm' is required."
        ENDC
        ENDM

        INCLUDE "gbhw.inc"

; Make sure include files are useable revisions.

        rev_Check_hardware_inc   1.5

; Macro that pauses until VRAM available.

lcd_WaitVRAM: MACRO
        ldh     a,[rSTAT]       ; <---+
        and     STATF_BUSY      ;     |
        jr      nz,@-4          ; ----+
        ENDM

        SECTION "Memory1 Code",HOME

;***************************************************************************
;*
;* mem_Set - "Set" a memory region
;*
;* input:
;*    a - value
;*   hl - pMem
;*   bc - bytecount
;*
;***************************************************************************
mem_Set::
	inc	b
	inc	c
	jr	.skip
.loop	ld	[hl+],a
.skip	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
	ret

;***************************************************************************
;*
;* mem_Copy - "Copy" a memory region
;*
;* input:
;*   hl - pSource
;*   de - pDest
;*   bc - bytecount
;*
;***************************************************************************
mem_Copy::
	inc	b
	inc	c
	jr	.skip
.loop	ld	a,[hl+]
	ld	[de],a
	inc	de
.skip	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
	ret

;***************************************************************************
;*
;* mem_Copy - "Copy" a monochrome font from ROM to RAM
;*
;* input:
;*   hl - pSource
;*   de - pDest
;*   bc - bytecount of Source
;*
;***************************************************************************
mem_CopyMono::
	inc	b
	inc	c
	jr	.skip
.loop	ld	a,[hl+]
	ld	[de],a
	inc	de
        ld      [de],a
        inc     de
.skip	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
	ret


;***************************************************************************
;*
;* mem_SetVRAM - "Set" a memory region in VRAM
;*
;* input:
;*    a - value
;*   hl - pMem
;*   bc - bytecount
;*
;***************************************************************************
mem_SetVRAM::
	inc	b
	inc	c
	jr	.skip
.loop   push    af
        di
        lcd_WaitVRAM
        pop     af
        ld      [hl+],a
        ei
.skip	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
	ret

;***************************************************************************
;*
;* mem_CopyVRAM - "Copy" a memory region to or from VRAM
;*
;* input:
;*   hl - pSource
;*   de - pDest
;*   bc - bytecount
;*
;***************************************************************************
mem_CopyVRAM::
	inc	b
	inc	c
	jr	.skip
.loop   di
        lcd_WaitVRAM
        ld      a,[hl+]
	ld	[de],a
        ei
	inc	de
.skip	dec	c
	jr	nz,.loop
	dec	b
	jr	nz,.loop
	ret

        ENDC    ;MEMORY1_ASM

;***************************************************************************
;*
;* mem_InitDMA - Copy DMA code to high-memory. You can call / initiate the
;*				 DMA by calling dmacode, which is the address of the dma
;*				 routine in memory
;*
;* input:
;*	 None
;*
;***************************************************************************
DMACODE EQU $ff80
DMA_DATA_SRC 	EQU		_RAM/$100   ;oam data location / $100
; BECAUSE the DMA code takes a byte XX and uses it as shorthand for $XX00
; hence why we take _RAM and divide by $100.  $8000 -> $80
; DMA copies 160 bytes from $XX00 to $fe00 thru $fe9f
; in more Explicit terms, it copies 160 bytes to the OAM (Object Attribute
; Memory) area. It always copies 160 bytes, fully overwriting the 
; from where you store the variables of your sprite and puts it into the
; oam data bank. The destination is a hard-coded $fe00  (or _OAMRAM)
; does it always overwrite OAMRAM?
mem_InitDMA:
	ld	de, DMACODE
	ld	hl, dma_src
	ld	bc, dma_src_end-dma_src
	call	mem_CopyVRAM			; copy when VRAM is available
	ret
dma_src:
	push	af
	ld	a, DMA_DATA_SRC		; bank where OAM DATA is stored
	ldh	[rDMA], a		; Start DMA  (ldh == LD High: $ff00 is added to rDMA)
	ld	a, $28				; 160ns
.dma_wait:
	dec	a
	jr	nz, .dma_wait
	pop	af
	ret
dma_src_end:
