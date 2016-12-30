;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------
; Newest Test is on bottom of file
include "gbhw.inc"
include "ibmpc1.inc"

section "Vblank", HOME[$0040]
	reti
section "LCDC", HOME[$0048]
	reti
section "Timer_Overflow", HOME[$0050]
	reti
section "Serial", HOME[$0058]
	reti
section "joypad_p1_p4", HOME[$0060]
	reti
section "start", HOME[$0100]
	nop
	jp begin

	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT

include "joypad.asm"
include "memory.asm"
include "lcd.asm"
include "syntax.asm"
include "test_includes.asm"
include "test_syntax.asm"
include "test_math.asm"
include "test_matrix.asm"

begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	call	SetupGameboy
	call	test_01_lda			; begin test_syntax.asm
	call	test_02_preserve
	call	test_03_preserve2
	call	test_04_if
	call	test_05_if_not
	call	test_06_if_flag
	call	test_07_if_not_flag
	call	test_08_truefalse
	call	test_09_ifa
	call	test_0A_if_flags
	call	test_0B_shifts
	call	test_0C_increment_decrement
	call	test_11_math_MultiplyAC		; begin test_math.asm
	call	test_12_math_Mult_Shortcuts
	call	test_13_math_Divide_C_by_B
	call	test_21_matrix_DeclareInit	; begin test_matrix.asm
; ===============================[ End calling tests ]====================
.mainloop:
	halt
	nop
	jr	.mainloop
	

SetupGameboy:
	call	lcd_Stop
	call	lcd_ScreenInit	; set up pallete and (x,y)=(0,0)
	call	LoadFont
	call	ClearBackground
	call	lcd_On
	call	lcd_ShowBackground
	ret

LoadFont:
	ld	hl, ASCII_TILES_LOC
	ld	de, _VRAM
	ld	bc, ASCII_TILES_END - ASCII_TILES_LOC
	call	mem_CopyMono  ; copy a Monochrome font to ram. (our is monochrome?)
	ret

ClearBackground:
	; sets background tiles to empty space
	ld	a, 32
	ld	hl, _SCRN0
	ld	bc, SCRN_VX_B * SCRN_VY_B
	call	mem_SetVRAM
	ret

; makes use of include "ibmpc1.inc"
ASCII_TILES_LOC:
	chr_IBMPC1 1,8  ; arguments 1,8 cause all 256 characters to be loaded
ASCII_TILES_END:
