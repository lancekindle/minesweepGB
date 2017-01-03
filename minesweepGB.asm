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


	mat_Declare	_SCRN0, SCRN_VY_B, SCRN_VX_B	; setup screen matrix
	; we'll now be able to use mat_GetYX and mat_SetYX on _SCRN0
	; meaning we can address the background tiles like a 32x32 matrix
	; SCRN_VY_B == 32 == SCRN_VX_B

Blank	SET	" "
Mine	SET	"*"
Flag	SET	"/"	; Blank, Mine, Flag are now variable constants


ClearSpriteTable:
	ld	a, 0
	ld	hl, OAMDATALOC
	ld	bc, OAMDATALENGTH
	call	mem_Set
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
	mat_Init	_SCRN0, Blank	; initialize screen background with " "
	call	LoadFont
	call	ClearSpriteTable
	call	lcd_On
	call	LoadWords
	call	SpriteSetup
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	call	lcd_EnableVBlankInterrupt
.mainloop:
	call	lcd_Wait4VBlank
	call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	call	move_sprite_within_screen_bounds
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
	; sprite coordinates are (0,0) up to (160,144)
	; background coordinates are (0,0) up to (20,18)
	; 160 / 8 == 20
	; so to translate from sprite coordinates to background coordinates,
	; we divide by 8.
	GetSpriteYAddr	Sprite0		; Y is loaded in a
	shift_right	a
	shift_right	a
	shift_right	a	; A>>3 == A/8
	ld	d, a	; Y coordinate should be in D
	GetSpriteXAddr	Sprite0		; X is loaded in a
	shift_right	a
	shift_right	a
	shift_right	a	; A>>3 == A/8
	ld	e, a	; X coordinate should be in E
	mat_SetYX	_SCRN0, d, e, Flag
	ret

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
