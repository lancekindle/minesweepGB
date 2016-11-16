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
section "Vblank", HOME[$0040]
						; trickery. Since dma returns and enables interrupts
						; we can just jp to the dma code immediately
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

clear_sprite_table:
	ld a, 0
	ld hl, OAMDATALOC
	ld bc, OAMDATALENGTH
	call mem_Set
	ret

clear_background:
	; sets background tiles to empty space
	ld a, 32
	ld hl, _SCRN0
	ld bc, SCRN_VX_B * SCRN_VY_B
	call mem_SetVRAM
	ret

load_words:
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
	call DMACODELOC
	ret

begin:
	di    ; disable interrupts
	ld sp, $ffff  ; init stack pointer to be at top of memory
	call initdma
	call screen_init
	call StopLCD
	call load_font
	call clear_sprite_table
	call clear_background
	call BeginLCD
	call load_words
	;call mem_InitDMA ; this'll need to be revised later (it's hard-coded to
					 ; copy from a specific location)
	call SpriteSetup
	call EnableSprites
	call EnableVBlankInterrupt
.mainloop:
	call jpad_GetKeys  ; loads keys into register a
	; MoveIf* are macros from sprite.inc
	MoveIfLeft Sprite0, 1
	MoveIfRight Sprite0, 1
	MoveIfDown Sprite0, 1
	MoveIfUp Sprite0, 1
	call wait4vblank
	jr .mainloop; jr is Jump Relative (it's quicker than jp)


; makes use of include "ibmpc1.inc"
ASCII_TILES_LOC:
	chr_IBMPC1 1,8  ; arguments 1,8 cause all 256 characters to be loaded
ASCII_TILES_END:

load_font:
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

wait4vblank
	ld a, [rLY]
	cp 145			; are we at line 145 yet?  (finished drawing screen then)
	jr nz, wait4vblank
	ret

StopLCD:
	ld a, [rLCDC]  ; LCD-Controller
	rlca		; rotate. IF LCD is On, bit 7 will carry (into carry flag)
	ret nc		; return if LCD is already off (carry-flag was 0)
	call wait4vblank
.stopLCD
	ld a, [rLCDC]
	xor LCDCF_ON	; XOR lcd-on bit with lcd control bits. (toggles LCD off)
	ld [rLCDC], a   ; `a` holds result of XOR operation
	ret

; AUGH. Objects were NOT turned on within the LCD, so my object never showed up
BeginLCD:
	ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON
	ld [rLCDC], a	; we really go all out here....turning on so much stuff
	ret

; modify sprite lcd options to enable sprites of 8bit size
; LCDCF_OBJ16 and LCDCF_OBJ8 control which objects get displayed
; LCDCF_OBJON / LCDCF_OBJOFF control if objects get displayed at all
EnableSprites:
	ld a, [rLCDC]  ; load current LCD config|LCDCF_OBJ8|LCDCF_OBJON
	or LCDCF_OBJON   ; add the "OBJECTS ON" option
	or LCDCF_OBJ8    ; add the "Obj 8-bit" option
	ld [rLCDC], a  ; push the new LCD config
	ret

EnableVBlankInterrupt:
	ld a, IEF_VBLANK
	ld [rIE], a             ; config to only allow V-blank interrupts
	ei						; actually enable interrupts
	ret
