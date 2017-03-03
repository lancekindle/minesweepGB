;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "gbhw.inc"
include "cgbhw.inc"
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
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBIT, COLOR_COMPATIBLE

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
include "random.asm"
include "rgb.asm"

; declare some variables
	var_LowRamByte	rGBC	; set to > 0 if color gameboy present
	var_LowRamByte	rGBA	; set to > 0 if running on gameboy advance
	var_LowRamByte	rNearbyCount
	var_LowRamByte	rCellY
	var_LowRamByte	rCellX
	var_LowRamByte	rFirstProbe
	var_LowRamByte	rMinesCount
	var_LowRamWord	rCellsRemaining
	var_LowRamByte	rCorrectFlags
	var_LowRamByte	rWrongFlags

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
	mat_IterDeclare	_SCRN0	; declare ram vars for screen iterator

Blank	SET	" "
Mine	SET	"*"
Flag	SET	$2B	; Blank, Mine, Flag are now variable constants


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


init_colorgb_variables:
	ld	a, $FF
	; at the very least, set the fact that color is supported
	ld	[rGBC], a
	; check if gameboy color or gameboy advance
	bit	0, b	; check bit 0 of B. It's gba if 1
	if_flag	nz,	ld [rGBA], a
	; now enable Color Gameboy double-speed
	ld	[rKEY1], a
	stop	
	; stop normally halts cpu (in a bad way). But since we've just
	; enabled double-speed mode, the cpu will pick up (After a screen
	; flicker) in double-speed mode
	xor	a
	ld	hl, rgb_StandardPalette
	call	rgb_SetSingleBGP  ; set BKGND palette 0 (reg. A) to greyscale
	xor	a
	ld	hl, rgb_StandardPalette
	call	rgb_SetSingleOBJP  ; set sprite palette 0 (reg. A) to greyscale
	ret

; set low-ram variables to 0
init_variables:
	xor	a	; set A=0
	ld	[rNearbyCount], a
	ld	[rCellY], a
	ld	[rCellX], a
	ld	[rMinesCount], a
	ld	[rCorrectFlags], a
	ld	[rWrongFlags], a
	ld	bc, SCRN_X_B * SCRN_Y_B	; number of cells (20x18 == 360)
	var_SetWord	b,c,	rCellsRemaining
	ld	a, 1
	ld	[rFirstProbe], a
	ret

begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	push	af	; save color-gameboy indicator
	xor	a
	ld	[rGBC], a	; zero gb-color indicator
	ld	[rGBA], a	; zero gb-advance indicator
	pop	af
	; register a contains 11 in a gameboy color
	ifa	==, $11, call init_colorgb_variables
	call init_colorgb_variables
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
	call	init_variables
	mat_Init	mines, 0
	mat_Init	flags, 0
	mat_Init	probed, 0
	stack_Init	toExplore
	call	display_startscreen	; and wait for user input
	mat_Init	_SCRN0, Blank	; initialize screen background with " "
	call	fill_mines
	call	remove_dense_mines
.mainloop:
	lcd_Wait4VBlank
	call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	call	move_sprite_within_screen_bounds
	if_	jpad_EdgeB, call	toggle_flag
	if_	jpad_EdgeA, call	probe_cell
	if_	only_mines_left, call reveal_mines
	jp	.mainloop; jr is Jump Relative (it's quicker than jp)


; start screen text
startscreen:
	DB	"Press any Button"
startscreen_end:

; displays startscreen and waits for user input before returning
; runs a tight loop so that when the user presses a button,
; it initializes the random-value generator (not tied to vblank)
display_startscreen:
	mat_GetYX	_SCRN0, 5, 2
	; HL contains address @ 5,5
	ldpair	d,e,	h,l	; destination is screen
	ld	hl, startscreen	; source is screen letters
	ld	bc, startscreen_end - startscreen	; # of bytes to write
	call	mem_CopyVRAM
	; now we run in a tight loop waiting for user input
.wait4input
	call	jpad_GetKeys
	or	a	; set zero-flag if no user input
	jr	z, .wait4input
	; now we've got the keypress in a
	rand_seed	; seed random#-generator with keypress
	rand_A	; immediately calculate random#. This has the effect
		; of starting the randomizer based on when user pressed a key
	ret


get_true:
	ret_true

; press keyboard_A to toggle flag
; this places a character @ location of sprite (x, y)
toggle_flag:
	call	get_sprite_yx_in_de
	mat_IndexYX	_SCRN0, d, e
	; hl now holds Index, the offset we can use
	push	hl
	mat_GetIndex	probed, hl
	pop	hl
	; if cell is already probed, do nothing
	ifa	==, 1, ret	;jr .turn_off_flag_logically
	push	hl
	mat_GetIndex	flags, hl
	pop	hl
	ifa	>, 0, jr .toggle_off
	push	hl
	mat_SetIndex	flags, hl, Flag
	pop	hl
	push	hl
	mat_GetIndex	mines, hl
	ifa	==, 0, jr .wrongly_flagged   ; user flagged a non-mine
.correctly_flagged	; we get here if user correctly flagged a mine
	ld	hl, rCorrectFlags
	inc	[hl]
	jr	.display_flag_on_screen
.wrongly_flagged
	ld	hl, rWrongFlags
	inc	[hl]
.display_flag_on_screen
	pop	hl
	mat_SetIndex	_SCRN0, hl, Flag
	ret
.toggle_off
	push	hl
	mat_GetIndex	mines, hl
	ifa	==, 0, jr .unflag_wrongly_flagged
.unflag_correctly_flagged	; we get here if we just un-flagged a mine
	ld	hl, rCorrectFlags
	dec	[hl]
	jr .disable_flag_logically_and_visually
.unflag_wrongly_flagged
	ld	hl, rWrongFlags
	dec	[hl]
.disable_flag_logically_and_visually
	pop	hl
	push	hl
	mat_SetIndex	_SCRN0, hl, Blank
	pop	hl
.turn_off_flag_logically
	mat_SetIndex	flags, hl, 0
	ret


; fill mines randomly in the minefield
fill_mines:
	mat_IterInit	mines, 0, SCRN_Y_B,   0, SCRN_X_B
.iterate
	mat_IterNext	mines
	ret	nc	; return if mines iterator done
	push	hl
	rand_A
	pop	hl
	ifa	>, 30, jr .iterate
	ld	a, 1
	ld	[hl], a		; place mine
	ld	hl, rMinesCount
	inc	[hl]		; add to count of mines
	jr	.iterate


; re-iterates current mine count and potentially removes a mine @ a location
; where there are >= 3 mines nearby. Should help remove dense clusters of mines
remove_dense_mines:
	mat_IterInit	_SCRN0, 0, SCRN_Y_B,   0, SCRN_X_B
.iterate
	mat_IterNext	_SCRN0
	ret	nc	; return if iteration done
	ld	bc, _SCRN0
	negate	bc
	add	hl, bc	; Index = @YX_address - beginning of matrix
	; HL now holds Index
	mat_GetIndex	mines, hl
	ifa	==, 0, jr .iterate	; skip if there's no mine here
	; we get here if there's a mine in the center.
	mat_IterYX	_SCRN0	; Y,X from current iteration in D,E
	call	get_neighbor_corners_within_bounds
	; BC now holds [y-1:y+2), DE holds [x-1:x+2)
	mat_IterInit	mines, b,c,   d,e
	mat_IterCount	mines, ==, 1
	ifa	>=,3, jp .maybe_remove_mine
	jp .iterate
.maybe_remove_mine
	rand_A
	ifa	>,200, jp .iterate
	mat_IterYX	_SCRN0; get Y,X in DE
	mat_GetYX	mines, d, e
	ifa	==, 0, jp .iterate	; there's no mine. Go back to iteration
	; [HL] (from above GetYX) holds the address for the current mine
	ld	[hl], 0		; remove mine
	ld	hl, rMinesCount
	dec	[hl]		; lower count of mines
	jp .iterate


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

; for the first probe, clear all mines in a 3x3 square around the player
; makes the first probe better, since it'll "open up" a foothold from which
; to begin probing in an educated manner
; assume that D,E already holds player Y,X position
first_probe:
	call	get_sprite_yx_in_de
	call	get_neighbor_corners_within_bounds
	; b, c holds (y-1, y+2), d, e holds (x-1, x+2)
	push	bc
	push	de
	mat_IterInit	mines, b, c, d, e	; setup iterate neighbors
	; subtract the # of mines nearby from the total mine-count
	mat_IterCount	mines, >, 0
	negate	a	; get -(count of nearby mines)
	ld	hl, rMinesCount
	add	a, [hl]
	ld	[hl], a		; store lowered count of mines
	; now zero-out the nearby mines
	pop	de
	pop	bc
	mat_IterInit	mines, b, c, d, e	; setup iterate neighbors
	mat_IterSet	mines, 0
	ld	a, 0
	ld	[rFirstProbe], a	; ensure first probe only runs once
	ret

probe_cell:
	call	get_sprite_yx_in_de
	; probe cell @ current location
	mat_IndexYX	mines, d, e	; get address in HL
	push	hl	; store current matrix index. We'll use it 3x
	mat_GetIndex	flags, hl	; result in a
	pop	hl
	ifa	==, Flag, ret	; ignore probe if Y,X is flagged
	push	hl
	mat_GetIndex	probed, hl
	pop	hl
	ifa	>, 0, ret	; ignore probe if it's already been probed
	ld	a, [rFirstProbe]	; is this our first probe?
	push	hl
	ifa	==, 1, call	first_probe ; remove nearby mines on 1st probe
	pop	hl
.check_explode
	push	hl	; store Y,X index for writing # to screen later
	mat_GetIndex	mines, hl
	pop	hl
	ifa	==, 1, jp reveal_mines	; we probed a mine :(
	; we get here if we didn't explode
	mat_SetIndex	probed, hl, 1	; indicate we've probed this cell
	call	get_sprite_yx_in_de
.add_DE_to_stack
	ldpair	b,c,	d,e	; Y,X in BC now
	stack_Push	toExplore, c,b	; store X,Y in stack
.explore_stack	; pop X,Y into E,D
	stack_Pop	toExplore, de
	ret	nc	; stack is empty. No more cells to probe. Return.
	lda	d
	ld	[rCellY], a	; store Y in ram
	lda	e
	ld	[rCellX], a	; store X in ram
	; D,E now holds Y,X, respectively
	call	count_and_display_nearby_mines; writes # to screen, # ret in A
	ifa	>, 0, jp .explore_stack	; nearby mines exist, so explore stack
	; we get here if mine-count==0, so now we'll explore nearby
.push_nearby_into_stack
	lda	[rCellY]
	ld	d, a
	lda	[rCellX]
	ld	e, a
	; DE now holds YX
	call	get_neighbor_corners_within_bounds
	; b, c holds (y-1, y+2), d, e holds (x-1, x+2)
	mat_IterInit	mines, b, c, d, e	; setup iterate neighbors
.loop_push_neighbors
	mat_IterNext	mines	; we only iter to add each cell's Y,X to stack
	jp	nc, .explore_stack	; finished adding cells to stack.
					; now let's explore next cell on stack
	; HL contains address of cell within mines. We need to calculate Index,
	; which is HL - mines-start-address
	ld	bc, mines	; get two's complement to:
	negate	bc		; calculate  -(mines)
	add	hl, bc		; stack-start (HL) - addr (BC) == Index
	push	hl		; store index@matrix
	mat_GetIndex	probed, hl	; get value @ index in A
	pop	hl
	ifa	==, 1, jr .loop_push_neighbors	; this cell's already been
						; explored. Move to next cell.
	mat_SetIndex	probed, hl, 1	; NOW we set cell to probed
					; before we add it to stack
	mat_IterYX	mines	; get Y,X of current iteration in D,E
	ldpair	b,c,	d,e	; move Y,X into BC
	stack_Push	toExplore, c, b	; push X, Y
	jr	.loop_push_neighbors


; call this with coordinates Y,X loaded in D,E
; This will write the number of nearby mines on-screen as well
; EXIT:	count of nearby mines in A
count_and_display_nearby_mines:   ; now we need to probe (y-1, x-1):(y+1,x+1)
	push	de
	call	get_neighbor_corners_within_bounds
	; setup iterator @mines, from (y-1, x-1) to (y+1, x+1)
	; aka	[y-1:y+2,x-1:x+2)    <==  [inclusive start, exclusive end)
	; where (y,x) is the coordinates of the player
	; b, c  = (y-1, y+2).   d, e == (x-1, x+2)
	mat_IterInit	mines, b, c, d, e
	mat_IterCount	mines, ==, 1; counts mines surrounding player
	ld	[rNearbyCount], a	; store minecount
	pop	de	; retrieve Y, X
	mat_IndexYX	_SCRN0, d,e	; calculate index (in HL)
	; index is in HL
	call	unflag_non_mine_cell	; preserves hl
	push	hl
	var_WordDecrement	rCellsRemaining
	lcd_Wait4VBlank ; need to wait for VRAM access
	lda	[rNearbyCount]
	add	"0"	; create string equivalent of count
	mat_SetIndex	_SCRN0, hl, a	; write count to screen background
	pop	hl	; pop matrix index
	mat_SetIndex	probed, hl, 1	; show that it's been explored
	; only explore neighbors if all of them are empty
	lda	[rNearbyCount]
	ret


; call this to logically unflag a location (index in HL) that is about to be
; probed. We assume that the location is NOT a mine
; preserves HL throughout this routine
unflag_non_mine_cell:
	push	hl
	mat_GetIndex	flags, hl
	ifa	>, 0, jr .removeWrongFlag
	pop	hl
	ret
.removeWrongFlag
	ld	hl, rWrongFlags
	dec	[hl]
	pop	hl
	push	hl
	mat_SetIndex	flags, hl, 0
	pop	hl
	ret


; call this with coordinates Y,X loaded in D,E
; this will NOT display # on screen, NOR mark cell as probed. It simply counts
; EXIT:	count of nearby mines in A
; USES:	AF, BC, DE, HL
count_only_nearby_mines:   ; now we need to probe (y-1, x-1):(y+1,x+1)
	call	get_neighbor_corners_within_bounds
	; setup iterator @mines, from (y-1, x-1) to (y+1, x+1)
	; aka	[y-1:y+2,x-1:x+2)    <==  [inclusive start, exclusive end)
	; where (y,x) is the coordinates of the player
	; b, c  = (y-1, y+2).   d, e == (x-1, x+2)
	mat_IterInit	mines, b, c, d, e
	mat_IterCount	mines, ==, 1; counts mines surrounding player
	ld	[rNearbyCount], a	; store minecount
	ret

; USES:	AF, BC, DE
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

; returns true/false if all mines have been accounted for and flagged
all_mines_flagged:
	lda	[rWrongFlags]
	or	a
	; return false if 1+ flags are incorrectly set
	ret_false	nz
	lda	[rCorrectFlags]
	ld	hl, rMinesCount
	; return true if # of correct-flags == # of mines
	ifa	==, [hl], ret_true
	ret_false


; returns true/false if all non-mine cells have been probed
only_mines_left:
	var_GetWord	b,c,	rCellsRemaining
	ld	a, [rMinesCount]
	sub	c	; Will set zero-flag
	or	b	; if 0,A == B,C
	ret_true	z
	ret_false


; iterate through and reveal mines onscreen
reveal_mines:
	mat_IterInit	mines, 0, SCRN_Y_B,	0, SCRN_X_B
.loop
	mat_IterNext	mines
	ret	nc	; iteration finished
	ifa	==, 0, jr .loop		; keep looping until we find a mine
	; HL contains address @ mines
	ld	de, mines
	negate	de
	add	hl, de
	; HL now contains Index / offset
	mat_SetIndex	_SCRN0, hl, Mine
	jr	.loop
.done
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
