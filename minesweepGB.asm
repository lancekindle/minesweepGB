;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "gbhw.inc"
include "ibmpc1.inc"



DMACODELOC	EQU	$ff80
;IRQs
; whenever one of these IRQs is triggered, three things happen:
; 1) SP is loaded with the address of the interrupted instruction
; 2) interrupts are disabled (You can return and enable IRQs with "reti")
; 3) execution jumps here, to the appropriate section.
;
; you can control which IRQs are enabled by writing the appropriate bits
; to rIE  (register Interrupt Enable). Search gbhw.inc for interrupt
; to see what flags are available  (i.e. IEF_SERIAL, IEF_VBLANK, IEF_TIMER)
section "Vblank", HOME[$0040]
		; trickery. Since dma returns and enables interrupts
		; we can just jp to the dma code immediately
		; this saves on number of returns (and cpu cycles)
	jp DMACODELOC	; DMACODE copies data from _RAM / $100 to OAMDATA
section "LCDC", HOME[$0048]
	reti
section "Timer_Overflow", HOME[$0050]
	reti
section "Serial", HOME[$0058]
	reti
section "joypad_p1_p4", HOME[$0060]
	reti

;gb_header

section "start", HOME[$0100]
	nop
	jp begin

; apparently the difference between defining a macro and calling one is
; the indent. If indented, we are calling the macro. If not, we are defining it
; UGH. After any macro call / command, DO NOT INCLUDE A COMMA! Commas only
; separate 2+ arguments. First argument doesn't have a comma separating it.
; write the rom header
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT

; include .asm files here (asm includes actual code. inc just defines stuff)
; we need to add it here after all the critical address-specific code
; has been laid out

include "sprite.inc"
; create Sprite0
	SpriteAttr	Sprite0
include "joypad.asm"
	PRINTT	"jpad_rKeys @"
	PRINTV	jpad_rKeys
	PRINTT	"\njpad_rEdge @"
	PRINTV	jpad_rEdge
	PRINTT	"\n"
include "memory.asm"
include "lcd.asm"
include "syntax.asm"
include "math.asm"
include "vars.asm"
include "matrix.asm"


	mat_Create	blah, 6, 8


ClearSpriteTable:
	ld	a, 0
	ld	hl, OAMDATALOC
	ld	bc, OAMDATALENGTH
	call	mem_Set
	ret

ClearBackground:
	; sets background tiles to empty space
	ld	a, 32
	ld	hl, _SCRN0
	ld	bc, SCRN_VX_B * SCRN_VY_B
	call	mem_SetVRAM
	ret

LoadWords:
	ld	hl, Title
	ld	de, _SCRN0 + SCRN_VX_B * 5
	ld	bc, TitleEnd - Title
	call	mem_CopyVRAM
	ret

SpriteSetup:
	PutSpriteYAddr	Sprite0, 0
	PutSpriteXAddr	Sprite0, 0
	ld	a, 1
	ld	[Sprite0TileNum], a
	ld	a, %00000000
	ld	[Sprite0Flags], a
	call	DMACODELOC   ; we should make sure interrupts are disabled before this
	ret

begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	call	initdma
	call	lcd_ScreenInit		; set up pallete and (x,y)=(0,0)
	call	lcd_Stop
	call	LoadFont
	call	ClearSpriteTable
	call	ClearBackground
	call	lcd_On
	call	LoadWords
	call	SpriteSetup
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	call	lcd_EnableVBlankInterrupt
.mainloop:
	call	lcd_Wait4VBlank
	mat_SetYX	blah, 1, 3, 67
	mat_SetYX	blah, 3, 3, 75
	mat_SetYX	blah, 0, 3, 90
	mat_GetYX	blah, 0, 3
	ifa	==, 90, call	jpad_GetKeys
	mat_GetYX	blah, 1, 3
	ifa	==, 67, call	move_sprite_within_screen_bounds
	;call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	;call	move_sprite_within_screen_bounds
.past_operation:
	lda	[jpad_rEdge]
	and	PADF_B
	if_flag	nz,	call toggle_flag

	jp	.mainloop; jr is Jump Relative (it's quicker than jp)


get_true:
	ret_true


; press keyboard_A to toggle flag
; this places a character @ location of sprite (x, y)
toggle_flag:
	; store (x, y) @ (b, c)
	; sprite coordinates are (0,0) up to (160,144)
	; background coordinates are (0,0) up to (20,18)
	; 160 / 8 == 20
	; so to translate from sprite coordinates to background coordinates,
	; we divide by 8.
	; divide by 8 easily gets us memory offset from _SCRN0 for X
	; but Y is technicallly SCRN_VX_B * Y, as far as location in VRAM
	; is concerned. So to get memory offset for Y, we must divide by 8,
	; then multiply by 32 (SCRN_VX_B). So, we really need to multiply
	; by 4
	GetSpriteYAddr	Sprite0		; Y is loaded in a
	math_Mult	a, 4	; Y * 4 result is in HL
	; essentially we just got SCRN_VX_B * Y in register hl
	ld	bc, _SCRN0
	add	hl, bc		; add (0,0) _SCRN0 address to hl

	GetSpriteXAddr	Sprite0		; get X component
	; we want to divide by 8 b shifting bits
	ld	c, a
	ld	b, 8
	call math_Divide_C_by_B
	ld	c, d	; result in D
	ld	b, 0
	; now bc holds x-component
	add	hl, bc	; add x-component to screen address
	; place flag graphic at location hl
	ld	c, 1	; set bc to $0001. only set one VRAM byte
	ld	a, 2	; flag icon
	call	mem_SetVRAM	; set character (reg_a) at sprite position
	ret


move_sprite_within_screen_bounds:
	; MoveIf* are macros from sprite.inc
	; Only move if NOT on borders
	GetSpriteXAddr	Sprite0
	push af
	ifa	<, SCRN_X - 8,  MoveOnceIfRight Sprite0, 8
	pop af
	push af
	ifa	>, 0,		MoveOnceIfLeft	Sprite0, 8
	pop af
	GetSpriteYAddr	Sprite0
	push af
	ifa	<, SCRN_Y - 8,  MoveOnceIfDown	Sprite0, 8
	pop af
	push af
	ifa	>, 0,		MoveOnceIfUp	Sprite0, 8
	pop af
	ret


; makes use of include "ibmpc1.inc"
ASCII_TILES_LOC:
	chr_IBMPC1 1,8  ; arguments 1,8 cause all 256 characters to be loaded
ASCII_TILES_END:

LoadFont:
	ld	hl, ASCII_TILES_LOC
	ld	de, _VRAM
	ld	bc, ASCII_TILES_END - ASCII_TILES_LOC
	call	mem_CopyMono  ; copy a Monochrome font to ram. (our is monochrome?)
	ret


Title:  ; using (:) will save ROM address so that you can reference it in code
	DB	"abcdefghijklmnopqrstuvwxyz"
TitleEnd:

initdma:
	ld	de, DMACODELOC
	ld	hl, dmacode
	ld	bc, dmaend - dmacode
	call	mem_CopyVRAM
	ret
dmacode:
	push	af
	ld	a, OAMDATALOCBANK
	ldh	[rDMA], a
	ld	a, $28
dma_wait:
	dec	a
	jr	nz, dma_wait
	pop	af
	reti
dmaend:
