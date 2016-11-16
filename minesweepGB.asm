;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "gbhw.inc"
include "ibmpc1.inc"
include "sprite.inc"

; create Sprite0
	SpriteAttr	Sprite0



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
	jp DMACODELOC		; DMACODE copies data from _RAM / $100 to OAMDATA
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
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT

; include .asm files here (asm includes actual code. inc just defines stuff)
; we need to add it here after all the critical address-specific code
; has been laid out
include "joypad.asm"
include "memory.asm"
include "lcd.asm"
; write the rom header


; align screen to X, Y of (0, 0)
screen_init:
	ld a, 0
	ld [rSCX], a
	ld [rSCY], a
	; setup pallette colors
	ld a, %11100100
	ld [rBGP], a  ; set background pallet
	ld [rOBP0], a ; set sprite/obj pallete 0  (can choose 1 or 0)
	ld [rOBP1], a ; set sprite/ obj pallete 1
	; lh   vs.   ldh.    ldh sets address to $ff00 and then adds (?) nn
	; OR ldh sets address to nn BUT sets 2nd byte to ff
	ret

ClearSpriteTable:
	ld a, 0
	ld hl, OAMDATALOC
	ld bc, OAMDATALENGTH
	call mem_Set
	ret

ClearBackground:
	; sets background tiles to empty space
	ld a, 32
	ld hl, _SCRN0
	ld bc, SCRN_VX_B * SCRN_VY_B
	call mem_SetVRAM
	ret

LoadWords:
	ld hl, Title
	ld de, _SCRN0 + SCRN_VX_B * 5
	ld bc, TitleEnd - Title
	call mem_CopyVRAM
	ret

SpriteSetup:
	PutSpriteYAddr	Sprite0, 0
	PutSpriteXAddr	Sprite0, 0
	ld a, 1
	ld [Sprite0TileNum], a
	ld a, %00000000
	ld [Sprite0Flags], a
	call DMACODELOC   ; we should make sure interrupts are disabled before this
	ret

begin:
	di    ; disable interrupts
	ld sp, $ffff  ; init stack pointer to be at top of memory
	call initdma
	call screen_init
	call lcd_Stop
	call LoadFont
	call ClearSpriteTable
	call ClearBackground
	call lcd_Begin
	call LoadWords
	;call mem_InitDMA ; this'll need to be revised later (it's hard-coded to
					 ; copy from a specific location)
	call SpriteSetup
	call lcd_ShowBackground
	call lcd_ShowSprites
	call lcd_EnableVBlankInterrupt
.mainloop:
	call jpad_GetKeys  ; loads keys into register a
	; MoveIf* are macros from sprite.inc
	MoveIfLeft Sprite0, 1
	MoveIfRight Sprite0, 1
	MoveIfDown Sprite0, 1
	MoveIfUp Sprite0, 1
	call lcd_Wait4VBlank
	jr .mainloop; jr is Jump Relative (it's quicker than jp)


; makes use of include "ibmpc1.inc"
ASCII_TILES_LOC:
	chr_IBMPC1 1,8  ; arguments 1,8 cause all 256 characters to be loaded
ASCII_TILES_END:

LoadFont:
	ld hl, ASCII_TILES_LOC
	ld de, _VRAM
	ld bc, ASCII_TILES_END - ASCII_TILES_LOC
	call mem_CopyMono  ; copy a Monochrome font to ram. (our is monochrome?)
	ret


Title:  ; using (:) will save ROM address so that you can reference it in code
	DB "abcdefghijklmnopqrstuvwxyz"
TitleEnd:

initdma:
	ld de, DMACODELOC
	ld hl, dmacode
	ld bc, dmaend - dmacode
	call	mem_CopyVRAM
	ret
dmacode:
	push af
	ld a, OAMDATALOCBANK
	ldh [rDMA], a
	ld a, $28
dma_wait:
	dec a
	jr nz, dma_wait
	pop af
	reti
dmaend:
