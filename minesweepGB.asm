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
include "stack.asm"


	mat_Declare	_SCRN0, SCRN_VY_B, SCRN_VX_B	; setup screen matrix
	mat_Declare	mines, SCRN_VY_B, SCRN_VX_B	; setup mines matrix
	mat_Declare	flags, SCRN_VY_B, SCRN_VX_B	; setup flags matrix
	mat_Declare	probed, SCRN_VY_B, SCRN_VX_B	; keep track of which
		; coordinates have already been explored
	stack_Declare	toExplore, 255	; just a random stack size
		; will hold a temporary storage of searchable cells
	; we'll now be able to use mat_GetYX and mat_SetYX on _SCRN0
	; meaning we can address the background tiles like a 32x32 matrix
	; SCRN_VY_B == 32 == SCRN_VX_B
	mat_IterDeclare	mines	; declar ram variables for mines iterator

Blank	SET	" "
Mine	SET	"*"
Flag	SET	"/"	; Blank, Mine, Flag are now variable constants



populate_minefield: MACRO
 DB   1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0
 DB   0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0
 DB   0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
 DB   0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0
 DB   0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0
 DB   0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
 DB   0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0
 DB   0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
 DB   0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0
 DB   0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0
 DB   0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1
 DB   0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,1
 DB   0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0
 DB   0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB   0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0
 DB   0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0
	ENDM

mines_prefill:
	populate_minefield	; call macro paste minefield
mines_prefill_end:


ClearSpriteTable:
	ld	a, 0
	ld	hl, OAMDATALOC
	ld	bc, OAMDATALENGTH
	call	mem_Set
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
	call	SpriteSetup
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	call	lcd_EnableVBlankInterrupt
	mat_Init	mines, mines_prefill, mines_prefill_end - mines_prefill
	mat_Init	flags, Blank
	mat_Init	probed, 0
	stack_Init	toExplore
.mainloop:
	call	lcd_Wait4VBlank
	call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	call	move_sprite_within_screen_bounds
	if_	jpad_EdgeB, call	toggle_flag
	if_	jpad_EdgeA, call	probe_cell
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
	math_Div	a, 8	; normally overwrites registers. But quick way
				; doesn't
	ld	d, a	; Y coordinate should be in D
	GetSpriteXAddr	Sprite0		; X is loaded in a
	math_Div	a, 8
	ld	e, a	; X coordinate should be in E
	mat_SetYX	_SCRN0, d, e, Flag
	ret

; USES: A, DE
; EXIT: D,E holds Y,X coordinates of player (if we assume screen is 20x16 grid)
; uses fast math_Div so that rest of registers aren't overwritten
get_sprite_yx_in_de:
	GetSpriteYAddr	Sprite0		; Y is loaded in a
	math_Div	a, 8
	ld	d, a	; Y coordinate should be in D
	GetSpriteXAddr	Sprite0		; X is loaded in a
	math_Div	a, 8
	ld	e, a	; X coordinate should be in E
	ret

probe_cell:
	call	get_sprite_yx_in_de
	; probe cell @ current location
	mat_IndexYX	mines, d, e	; get address in HL
	push	hl	; store current matrix index. We'll use it 3x
	mat_GetIndex	flags, hl	; result in a
	pop	hl
	ifa	==, Flag, ret	; ignore probe if Y,X is flagged
.check_explode
	push	hl	; store Y,X index for writing # to screen later
	mat_GetIndex	mines, hl	; HL equals current index
	pop	hl
	ifa	==, 1, ret	; return (do nothing) if we exploded
	mat_SetIndex	probed, hl, 1	; indicate we've probed this cell
	call	get_sprite_yx_in_de
.add_DE_to_stack	; push Y,X (D,E) onto stack
	ld	a, d
	preserve	de, stack_Push	toExplore, a ; store Y coordinate
	ld	a, e
	stack_Push	toExplore, a	; store X coordinate
.explore_stack	; pop X,Y into E,D
	stack_Pop	toExplore	; get X
	ret	nc	; stack is empty. No more cells to probe. Return.
	ld	e, a	; store X
	preserve	de, stack_Pop	toExplore	; get Y
	ld	d, a	; store Y
	push	de	; store Y,X
	; D,E now holds Y,X, respectively
.count_nearby_mines		; now we need to probe (y-1, x-1):(y+1,x+1)
	call	get_neighbor_corners_within_bounds
	pop	HL	; pop Y,X into H,L
	push	bc
	push	de	; preserve neighbor corners
	push	HL	; preserve Y,X on top of stack
	; setup iterator @mines, from (y-1, x-1) to (y+1, x+1)
	; aka	[y-1:y+2,x-1:x+2)    <==  [inclusive start, exclusive end)
	; where (y,x) is the coordinates of the player
	; b, c  = (y-1, y+2).   d, e == (x-1, x+2)
	mat_IterInit	mines, b, c, d, e
	mat_IterCount	mines, ==, 1; counts mines surrounding player
	add	"0"		; set A to string version of count
	preserve	af, call lcd_Wait4VBlank ; need to wait for VRAM access
	pop	de	; retrieve Y, X
	push	af
	mat_IndexYX	_SCRN0, d,e	; calculate index
	pop	af
	push	hl	; store index
	push	af	; store value of a
	mat_SetIndex	_SCRN0, hl, a	; write count to screen background
	pop	hl
	mat_SetIndex	probed, hl, 1	; show that it's been explored
	pop	af	; restore value of nearby mines
	pop	de
	pop	bc	; restore neighbor corners in d,e,  b,c
	; only explore neighbors if all of them are empty
	ifa	==,"0", jr	.push_nearby_into_stack
	; else continue to explore current stack
	jp	.explore_stack
.push_nearby_into_stack
	mat_IterInit	mines, b, c, d, e	; iterate neighbors
.loop_push_neighbors
	mat_IterNext	mines	; we only iter to get next HL address / Y,X
	jp	nc, .explore_stack		; finished adding to stack.
						; now let's explore it
	push	hl
	mat_GetIndex	probed, hl
	ifa	==, 1, jr .donot_explore_already_explored
	pop	hl
	push	hl
	mat_SetIndex	probed, hl	; NOW we set cell to probed
	pop	hl			; before we add it to stack
	call	mat_YX_from_Index
	ld	a, d	; D holds Y address
	preserve	de, stack_Push	toExplore	; push Y coordinate
	ld	a, e	; E holds X address
	stack_Push	toExplore	; push X coordinate
	jr	.loop_push_neighbors
.donot_explore_already_explored
	pop	hl
	jr .loop_push_neighbors



; USES:	A, BC, DE
; INPT:	Y,X in D,E, respectively
; EXIT:	Y-1,Y+2 in B,C.  X-1,X+2 in D,E, respectively
get_neighbor_corners_within_bounds:
	; assume D,E holds Y, X
	lda	-1
	add	d	; get Y - 1
	ifa	==, $FF, inc	a	; correct for underflow
					; keeps y >= 0
	ld	b, a	; store start Y
	lda	2
	add	d	; get Y + 2
	ifa	>, SCRN_Y_B, dec	a	; keeps y <= screen height
	ld	c, a	; store End Y
	; Y-1:Y+2 is now loaded in B:C, respectively
	lda	-1
	add	e	; get X - 1
	ifa	==, $FF, inc	a	; correct for underflow
					; keeps x >= 0
	ld	d, a	; store start X
	lda	2
	add	e	; get X + 2
	ifa	>, SCRN_X_B, dec	a	; keeps x <= screen width
	ld	e, a	; store end X
	; X-1:X+2 is now loaded in D:E, respectively
	ret


move_sprite_within_screen_bounds:
	; MoveIf* are macros from sprite.inc
	; Only move if NOT on borders
	GetSpriteXAddr	Sprite0
	push	af	; store X value for later
	ifa	<, SCRN_X - 8,  MoveOnceIfRight Sprite0, 8
	pop	af
	ifa	>, 0,		MoveOnceIfLeft	Sprite0, 8
	GetSpriteYAddr	Sprite0
	push	af
	ifa	<, SCRN_Y - 8,  MoveOnceIfDown	Sprite0, 8
	pop	af
	ifa	>, 0,		MoveOnceIfUp	Sprite0, 8
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

; copies the dmacode to HIRAM. dmacode will get run each Vblank,
; and it is resposible for copying sprite data from ram to vram.
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
