	IF	!DEF(RGB_ASM)	; prevent multiple includes of this file
RGB_ASM	SET	1

include "gbhw.inc"
include "cgbhw.inc"
include "syntax.asm"
include "memory.asm"

; library to help with colors on the gameboy color

;*
;* Set GameBoy Colo(u)r palettes
;*
;* v1.0 - Original
;* v1.1 - Fixed interrupt bugs
;*

rgb_PALETTE_MASK	=	%00000111


;	This RGBSet Macro uses RGB values from 0 to 255.
; Even though this is a greater color range than the
; GBC supports it allows you to directly read color
; values from your favorite paint program using the
; eye dropper tool and drop them directly into your
; program.
rgb_Set: MACRO
	DW	((\3 >> 3) << 10) + ((\2 >> 3) << 5) + (\1 >> 3)
	ENDM


; this contains the standard palette for which to set background and sprites
; that'll look like standard gameboy graphics (using palette 0)
rgb_StandardPalette:
	rgb_Set	255, 255, 255	; white
	rgb_Set	192, 192, 192	; light grey
	rgb_Set	127, 127, 127	; dark grey
	rgb_Set	0, 0, 0		; black


rgb_InvertedPalette:
	rgb_Set	0, 0, 0		; black
	rgb_Set	127, 127, 127	; dark grey
	rgb_Set	192, 192, 192	; light grey
	rgb_Set	255, 255, 255	; white


; USES: A
rgb_LoadVRAMColorBank: MACRO
	ld	a, $01		; select bank 1 (color bank)
	ldh	[rVRAM_BANK], a		; aka ld [$FF4F], a
	ENDM

; USES: A
rgb_LoadVRAMTileBank: MACRO
	ld	a, $00		; select bank 0 (tile-data bank)
	ldh	[rVRAM_BANK], a		; aka ld [$FF4F], a
	ENDM

; initializes the tiles with color \1 (a hard-coded #)
; this assumes the LCD is stopped and/or safe to write to
rgb_InitVRAMColorBank: MACRO
	rgb_LoadVRAMColorBank
	ldhard	a, \1
	ld	hl, _SCRN0
	ld	bc, SCRN_VX_B * SCRN_VY_B ;256 tiles in a 32x32 screen area
	call	mem_Set
	rgb_LoadVRAMTileBank
	ENDM


; Note:
;	Testing of register STAT is done to allow setting
; palettes while the screen is on or off. Without this
; test you can only reliably set the palettes with
; the screen off. There are probably many other methods
; for setting palettes with the screen on that are not
; covered here.
;
; Example for setting background palettes:
;
;	ld	hl,BGPaletteTable
;	call	SetAllCGB_BGP


; *** Set a single background palette ***
; Entry: HL = pntr to data for 1 palette
;	 A = palette number (0-7)
rgb_SetSingleBGP:
	add	a,a		; a = pal # * 2
	add	a,a		; a = pal # * 4
	add	a,a		; a = pal # * 8
	set	7, a		; enable auto-increment
	ldh	[rBCPS],a
	ld	bc,$0869	; b = 8, c = rBCPD
.loop1:
	di
.loop2:
	ldh	a,[rSTAT]
	and	2
	jr	nz,.loop2
	ld	a,[hl+]
	ld	[c],a
	ei
	dec	b
	jr	nz,.loop1
	ret


; *** Set all background palettes ***
; Entry: HL = pntr to data for 8 palettes
rgb_SetAllBGP:
	ld	a,%10000000	; bit 7 = auto-increment. Bits 0-6 = index 0
	ldh	[rBCPS],a
	ld	bc,$4069	; b = 64, c = rBCPD	(when using ld [c], a)
.loop1:
	di
.loop2:
	ldh	a,[rSTAT]
	and	2
	jr	nz,.loop2
	ld	a,[hl+]
	ld	[c],a
	ei
	dec	b
	jr	nz,.loop1
	ret


; *** Set a single object (sprite) palette ***
; Entry: HL = pntr to data for 1 palette
;	 A = palette number (0-7)
rgb_SetSingleOBJP:
	add	a,a		; a = pal # * 2
	add	a,a		; a = pal # * 4
	add	a,a		; a = pal # * 8
	set	7, a		; enable auto-increment
	ldh	[rOCPS],a
	ld	bc,$086b	; b = 8, c = rOCPD
.loop1:
	di
.loop2:
	ldh	a,[rSTAT]
	and	2
	jr	nz,.loop2
	ld	a,[hl+]
	ld	[c],a
	ei
	dec	b
	jr	nz,.loop1
	ret


; *** Set all object (sprite) palettes ***
; Entry: HL = pntr to data for 8 palettes
rgb_SetAllOBJP:
	ld	a,$80
	ldh	[rOCPS],a
	ld	bc,$406b	; b = 64, c = rOCPD
.loop1:
	di
.loop2:
	ldh	a,[rSTAT]
	and	2
	jr	nz,.loop2
	ld	a,[hl+]
	ld	[c],a
	ei
	dec	b
	jr	nz,.loop1
	ret

; ** Example colo(u)r palettes **

; don't use this macro. It's merely an example for how to define a block of 8
; color palettes. You'd then set HL to point to the start of this block
; and call rgb_SetAllBGP	--or-- call rgb_SetAllOBJP for background and
; sprite palettes, respectively
rgb_Setup_OBJPaletteTable_Example: MACRO
;							- Palette #1
	RGBSet	0,0,0		; Transparent (not used, value not important)
	RGBSet	191,191,191	; Light Grey
	RGBSet	127,127,127	; Dark Grey
	RGBSet	0,0,0		; Black

	RGBSet	0,0,0		; Transparent		- Palette #2
	RGBSet	0,255,0		; Green
	RGBSet	0,0,255		; Blue
	RGBSet	0,0,0		; Black

	RGBSet	0,0,0		; Transparent		- Palette #3
	RGBSet	255,0,0		; Red
	RGBSet	255,105,180	; Pink
	RGBSet	0,0,0		; Black
;	...........

	RGBSet	0,0,0		; Transparent		- Palette #8
	RGBSet	255,0,255	; Purple
	RGBSet	255,255,0	; Yellow
	RGBSet	0,0,0		; Black
	ENDM




	ENDC	; end RGB_ASM defines
