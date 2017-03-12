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
; 3) execution jumps here (below), to the appropriate section.
;
; you can control which IRQs are enabled by writing the appropriate bits
; to rIE  (register Interrupt Enable). Search gbhw.inc for interrupt
; to see what flags are available  (i.e. IEF_SERIAL, IEF_VBLANK, IEF_TIMER)
section "Vblank", HOME[$0040]
		; trickery. Since dma returns and enables interrupts
		; we can just jp to the dma code immediately
		; this saves on number of returns (and cpu cycles)
	jp handle_vblank
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
	; set sprite variables
	SpriteAttr	Spr_UpperLeft
	SpriteAttr	Spr_UpperRight
	SpriteAttr	Spr_LowerLeft
	SpriteAttr	Spr_LowerRight
	; set ram variables
	var_LowRamByte	rGBC	; set to > 0 if color gameboy present
	var_LowRamByte	rGBA	; set to > 0 if running on gameboy advance
	var_LowRamByte	rPlayerY
	var_LowRamByte	rPlayerX
	var_LowRamByte	rPlayerHasMoved
	var_LowRamByte	rCrosshairY
	var_LowRamByte	rCrosshairX
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
	; toReveal holds coordinates and value to place on cells
	stack_Declare	toReveal, 42
	stack_Declare	minesToReveal, 30
	; toFlag holds coordinates of cells to flag.
	; (but will not flag it if it's marked as probed)
	stack_Declare	toFlag, 2	; enough to queue 1 flag only
	stack_Declare	toUnflag, 2
	; we'll now be able to use mat_GetYX and mat_SetYX on _SCRN0
	; meaning we can address the background tiles like a 32x32 matrix
	; SCRN_VY_B == 32 == SCRN_VX_B
	mat_IterDeclare	mines	; declar ram variables for mines iterator
	mat_IterDeclare	_SCRN0	; declare ram vars for screen iterator

Blank	SET	" "
Mine	SET	"*"
Cell	SET	$00	; Cell == graphics index to display a normal cell
Flag	SET	Cell + $01	; Blank, Mine, Flag are now variable constants


ClearSpriteTable:
	ld	a, 0
	ld	hl, OAMDATALOC
	ld	bc, OAMDATALENGTH
	call	mem_Set
	ret

SpriteSetup:
	ld	a, 8*8
	ld	[rPlayerY], a
	ld	[rCrosshairY], a
	ld	a, 9*8
	ld	[rPlayerX], a
	ld	[rCrosshairX], a
	; set crosshairs to tile#2
	ld	a, 2
	ld	[Spr_UpperLeftTileNum], a
	ld	[Spr_LowerLeftTileNum], a
	ld	[Spr_UpperRightTileNum], a
	ld	[Spr_LowerRightTileNum], a
	; flip crosshairs to they all have a corresponding orientation
	ld	a, %00000000
	ld	[Spr_UpperLeftFlags], a
	set	5, a	; set horizontal flip flag
	ld	[Spr_UpperRightFlags], a
	set	6, a	; set vertical flip flag
	ld	[Spr_LowerRightFlags], a
	res	5, a	; undo horizontal flip
	ld	[Spr_LowerLeftFlags], a
	; push sprite positions to vram
	call	update_crosshairs
	call	DMACODELOC
	ret

; update crosshairs indicating player origin
; player's coordinates are on top-left of cell
; places coordinates around the location
; USES: AF
update_crosshairs:
	call	move_crosshairs_halfway_to_player
	ld	a, [rCrosshairY]
	push	af
	sub	3
	PutSpriteYAddr	Spr_UpperLeft, a
	PutSpriteYAddr	Spr_UpperRight, a
	pop	af
	add	2
	PutSpriteYAddr	Spr_LowerLeft, a
	PutSpriteYAddr	Spr_LowerRight, a
	; update X position
	ld	a, [rCrosshairX]
	push	af
	sub	2
	PutSpriteXAddr	Spr_UpperLeft, a
	PutSpriteXAddr	Spr_LowerLeft, a
	pop	af
	add	3
	PutSpriteXAddr	Spr_UpperRight, a
	PutSpriteXAddr	Spr_LowerRight, a
	ret


; moves the crosshairs halfway to the player. Each call moves it halfway closer
; until it's fully moved into the player's position
; barely perceptable, but it makes movement feel so smooth. mmmmmm yeah.
; try rocking back and forth to see what I mean
; USES: AF, B
move_crosshairs_halfway_to_player:
	; uncomment these lines to see the instant 'jerky' moves
;	ld	a, [rPlayerY]
;	ld	[rCrosshairY], a
;	ld	a, [rPlayerX]
;	ld	[rCrosshairX], a
	; update Y position
	ld	a, [rCrosshairY]
	ld	b, a
	ld	a, [rPlayerY]
	sub	b	; calculate player_y - crosshar_y
	jp z, .update_X	; skip updating Y if it's already equal
	if_flag	c, jp .negativeY
	; A is positive here. We add half that to crosshair_y
	srl	a	; divide offset by 2
	; if A=0, then before divide by 2 it was 1. Reload 1
	if_flag	z, ld	a, 1
	jp .add_half_y_offset
.negativeY
	sra	a	; divide by 2 (for signed/negative numbers)
	; unlike unsigned #s, -1 never goes away when divided by 2. $FF stays
	; $FF when using sra (which keeps the same bit 7)
	; so it's totally cool to use the exact result as the offset
.add_half_y_offset
	add	b	; apply offset to crosshair_y
	ld	[rCrosshairY], a
	; update X position
.update_X
	ld	a, [rCrosshairX]
	ld	b, a
	ld	a, [rPlayerX]
	sub	b	; calculate player_X - crosshar_X
	ret z	; skip updating X if it's already equal
	if_flag	c, jp .negativeX
	srl	a	; divide offset by 2
	; if A=0, then our offset was one. Load 1 as offset
	if_flag	z, ld	a, 1
	jp .add_half_x_offset
.negativeX
	sra	a	; divide by 2 (for signed/negative numbers)
.add_half_x_offset
	add	b	; apply offset to crosshair_x
	ld	[rCrosshairX], a
	ret


bg_color_palettes:
	; greyscale
	rgb_Set 255, 255, 255	; white
	rgb_Set	192, 192, 192	; light grey
	rgb_Set	127, 127, 127	; dark grey
	rgb_Set	  0,   0,   0	; black
	; green on white (good indicator)
	rgb_Set 255, 255, 255	; white
	rgb_Set	  0, 127,   0	; light green
	rgb_Set	  0, 255,   0	; green
	rgb_Set	  0, 100,   0	; dark green
	; blue on white
	rgb_Set 255, 255, 255	; white
	rgb_Set	0,   0,   127	; light blue
	rgb_Set	0,   0,   192	; darkish blue
	rgb_Set	0,   0,   255	; blue
	; purple on white
	rgb_Set 255, 255, 255	; white
	rgb_Set 127,   0, 127	; light magenta
	rgb_Set	$7D, $05, $52	; dark orchid
	rgb_Set	255,   0, 255	; magenta
	; red on white (bad indicator)
	rgb_Set 255, 255, 255	; white
	rgb_Set	127,   0,   0	; light red
	rgb_Set	255,   0,   0	; red
	rgb_Set	128,   0,   0	; dark red
	; yellow on white
	rgb_Set 255, 255, 255	; white
	rgb_Set	127, 127,   0	; light yellow
	rgb_Set	192, 192,   0	; bolder yellow
	rgb_Set	255, 255,   0	; yellow
	; pink on white
	rgb_Set 255, 255, 255	; white
	rgb_Set	$FA, $AF, $BE	; pink
	rgb_Set	$F6, $60, $AB	; hot pink
	rgb_Set	$F5, $28, $87	; deep pink
	; orange on red (flames...)
	rgb_Set 255,   0,   0	; red
	rgb_Set	$FF, $24,   0	; scarlet
	rgb_Set	$F8, $72, $17	; pumpkin orange
	rgb_Set	255, $A5,   0	; orange


; check for gameboy color and gameboy advance hardware. Enable color features
; if possible
check_hardware:
	push	af	; save color-gameboy indicator
	xor	a
	ld	[rGBC], a	; zero gb-color indicator
	ld	[rGBA], a	; zero gb-advance indicator
	ld	a, [$143]	; this is the ROM's GBC support indicator
	; it doesn't matter what hardware we're running if we accidentally
	; compiled the program to be non-color compatible
	ifa	<, $80, jp .rom_incompatible_with_color
	pop	af
	; register a contains $11 in a gameboy color
	; AND bit 1 of register b is set in a gameboy advance
	ifa	==, $11, call init_colorgb_variables
	ret
.rom_incompatible_with_color
	; we get here if rom does not tell the gbc that it's color compatible
	; so even though we may detect gbc, gba hardware, we must NOT use any
	; color features. Pretend we're on the original gameboy
	pop	af
	ret

init_colorgb_variables:
	ld	a, 1
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
	nop	; nop after a halt/stop is good practice
	xor	a
	ld	hl, bg_color_palettes
	call	rgb_SetAllBGP
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


get_cell_font: MACRO
	PUSHO	; push options so that I can change the meaning of .-oX
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; - = 01
	; o = 10
	; X = 11
	OPT	g.-oX

	; Cell Graphic
	DW	`o.......
	DW	`o------.
	DW	`o------.
	DW	`o------.
	DW	`o------.
	DW	`o------.
	DW	`o------.
	DW	`oooooooo

	; Flagged Cell Graphic
	DW	`o.......
	DW	`o--X---.
	DW	`o--X---.
	DW	`oXXXXX-.
	DW	`o--X---.
	DW	`o--X---.
	DW	`o------.
	DW	`oooooooo

	; 1/4th of crosshairs. We flip (H and V) to create a surrounding box
	DW	`X-X-X-X-
	DW	`-.......
	DW	`X.......
	DW	`-.......
	DW	`X.......
	DW	`-.......
	DW	`X.......
	DW	`-.......

	POPO	; restore default options (aka undo g.-oX)
	ENDM

get_number_font: MACRO
	PUSHO	; push options so that I can change the meaning of . & X
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; X = 01
	OPT	g.X-o

	; 0 number, completely removed from being visible
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........

	OPT	g.-Xo	; realign X to be dark (but not darkest) shade
	; numbers 1-9, shaded dark, but not black
        DW      `..XX....
        DW      `.XXX....
        DW      `..XX....
        DW      `..XX....
        DW      `..XX....
        DW      `..XX....
        DW      `XXXXXX..
        DW      `........

        DW      `.XXXX...
        DW      `XX..XX..
        DW      `....XX..
        DW      `..XXX...
        DW      `.XX.....
        DW      `XX..XX..
        DW      `XXXXXX..
        DW      `........

        DW      `.XXXX...
        DW      `XX..XX..
        DW      `....XX..
        DW      `..XXX...
        DW      `....XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `........

        DW      `...XXX..
        DW      `..XXXX..
        DW      `.XX.XX..
        DW      `XX..XX..
        DW      `XXXXXXX.
        DW      `....XX..
        DW      `...XXXX.
        DW      `........

        DW      `XXXXXX..
        DW      `XX......
        DW      `XXXXX...
        DW      `....XX..
        DW      `....XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `........

        DW      `..XXX...
        DW      `.XX.....
        DW      `XX......
        DW      `XXXXX...
        DW      `XX..XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `........

        DW      `XXXXXX..
        DW      `XX..XX..
        DW      `....XX..
        DW      `...XX...
        DW      `..XX....
        DW      `..XX....
        DW      `..XX....
        DW      `........

        DW      `.XXXX...
        DW      `XX..XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `XX..XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `........

        DW      `.XXXX...
        DW      `XX..XX..
        DW      `XX..XX..
        DW      `.XXXXX..
        DW      `....XX..
        DW      `...XX...
        DW      `.XXX....
        DW      `........


	POPO	; restore default options (aka undo g.-oX)
	ENDM

cell_gfx:
	get_cell_font
end_cellgfx:

number_gfx:
	get_number_font
end_numbergfx

load_graphics:
	; copy cell, flagged-cell, and crosshairs graphics
	ld	hl, cell_gfx
	ld	de, _VRAM + Cell	; (Cell == 0. That part adds nothing)
	ld	bc, end_cellgfx - cell_gfx
	call	mem_CopyVRAM
	; copy #0 graphics
	ld	hl, number_gfx
	ld	de, _VRAM + 16*"0"
	ld	bc, end_numbergfx - number_gfx
	call	mem_CopyVRAM
	ret

begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	call	check_hardware
	call	initdma
	call	lcd_ScreenInit		; set up pallete and (x,y)=(0,0)
	call	lcd_Stop
	mat_Init	_SCRN0, Blank	; initialize screen background with " "
	mat_Init	mines, 0	; setup variables
	mat_Init	flags, 0
	mat_Init	probed, 0
	stack_Init	toExplore
	stack_Init	toReveal
	stack_Init	minesToReveal
	stack_Init	toFlag
	stack_Init	toUnflag
	call	LoadFont
	call	load_graphics
	call	ClearSpriteTable
	call	lcd_On
	call	SpriteSetup
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	call	init_variables
	call	display_startscreen	; and wait for user input
	mat_Init	_SCRN0, 0	; initialize screen background with cells
	call	fill_mines
	call	remove_dense_mines
	call	lcd_EnableVBlankInterrupt
.mainloop:
	halt	; should get interrupted every vblank...
	nop
	if_	jpad_EdgeA, call	probe_cell
	if_	only_mines_left, call queue_color_mines_reveal
	jp	.mainloop; jr is Jump Relative (it's quicker than jp)


; this gets called v-blank. It has three purposes:
;  1) move sprite data
;  2) reveal all awaiting #'d cells: cells that have been queued up in probe
;  3) read joypad
;  4) move character according to movement keys.
;  5) queue up probe and flag operations according to other keys
;  3) queue task-handler operation to read joypad and do appropriate actions
;	IF it doesn't already have that queued. It's important that the task
;	only gets run once
; This 3rd task is another function that has a small list of to-run functions
handle_vblank:
	pushall
	call	DMACODELOC ; DMACODE copies data from _RAM / $100 to OAMDATA
	lda	[rGBC]
	ifa	==, 0, call	reveal_queued_probed_cells
	lda	[rGBC]
	ifa	>=, 1, call	reveal_color_queued_probed_cells
	call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	call	move_sprite_within_screen_bounds
	if_	jpad_EdgeB, call	toggle_flag
	call	reveal_queued_mines
	call	reveal_queued_flags
	popall
	reti


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
; toggles on/off flag and keeps track of whether you've flagged a correct
; or incorrect flag
toggle_flag:
	call	get_player_yx_in_de
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
.set_logical_flag
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
	pop	bc
	stack_Push	toFlag, C,B
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
	pop	bc
	push	bc
	stack_Push	toUnflag, C,B
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
; but leave edge-mines in place. This'll make it more likely that there will
; stragglers around the edges
remove_dense_mines:
	mat_IterInit	_SCRN0, 1, SCRN_Y_B - 1,   1, SCRN_X_B - 1
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
get_player_yx_in_de:
	ld	a, [rPlayerY]
	math_Div	a, 8	; convert from pixels to grid
	ld	d, a
	ld	a, [rPlayerX]
	math_Div	a, 8	; convert from pixel coordinates to grid coords
	ld	e, a
	ret

; for the first probe, clear all mines in a 3x3 square around the player
; makes the first probe better, since it'll "open up" a foothold from which
; to begin probing in an educated manner
; assume that D,E already holds player Y,X position
first_probe:
	call	get_player_yx_in_de
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
	call	get_player_yx_in_de
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
	push	hl	; store index for writing # to screen later
	mat_GetIndex	mines, hl
	pop	hl
	ifa	==, 1, jp mine_probed	; we probed a mine :(
	; we get here if we didn't explode
	mat_SetIndex	probed, hl, 1	; indicate we've probed this cell
	call	get_player_yx_in_de
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
; this will add this coordinate to the list of #s to be displayed on screen
; (which will get displayed every v-blank)
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
	mat_SetIndex	probed, hl, 1	; show that it's been explored
	var_WordDecrement	rCellsRemaining
	lda	[rNearbyCount]
	pop	hl	; pop matrix index
	ldpair	b,c,	h,l	; move Index to b,c
	add	"0"	; create string equivalent of count
.pushing
	stack_Push	toReveal, C,B,A	; push Index and mine-count for later
	jr nc, .pushing	; keep trying to push onto stack until it succeeds
	lda	[rNearbyCount]
	ret


; from queue of (count, index), write out count to index + _SCRN0.
; since we assume this is called from within vblank, don't make safety checks
reveal_queued_probed_cells:
.reveal_loop
	stack_Pop	toReveal, ABC
	; if stack_Pop returns false (no more to pop), then we are done
	ret	nc
	; we assume this is during v-blank. Just do it
	ldpair	h,l,	b,c
	mat_SetIndex	_SCRN0, hl, a, vblank unsafe	; write count to screen background
	jr .reveal_loop


; Reveal numbers indicating mine-count on background grid. In the gameboy
; color, the same grid location (but in VRAM-bank-1) is used to set the color
reveal_color_queued_probed_cells:
.reveal_loop
	stack_Pop	toReveal, ABC
	; if stack_Pop returns false (no more to pop), then we are done
	ret	nc
	; we assume this is during v-blank. Just do it
	ld	hl, _SCRN0
	add	hl, bc	; get address of #'d tile (and color) in VRAM
	ld	[hl], a	; write number to screen
	ifa	==, "0", jr .reveal_loop ; don't need to change palette for empty cell
.change_color
	ldpair b,c,	h,l	; move VRAM address to bc
	ld	hl, rVBK
	ld	[hl], 1	; change to VRAM bank 1 (color bank)
	sub	"0"	; get integer palette # corresponding to string count
	ifa	>, 4, ld a, 4	; use palettes 0-4
	ld	[bc], a	; set palette
	xor	a
	ld	[hl], a	; change back to VRAM bank 0 (tile-data bank)
	jr .reveal_loop


; reveal_queued_flags only flags and unflags once per iteration.
; We shouldn't expect more than that # of flags, and this puts a limit
; on how long this routine will take to run.
reveal_queued_flags:
.flag_loop
	stack_Pop	toFlag, HL
	; if stack_Pop returns false (no more to pop), then we are done
	jr	nc, .unflag_loop
	push	hl
	mat_GetIndex	probed, hl
	ifa	==, 1, jr .bad_flag
	pop	hl
	; we assume this is during v-blank. Just do it
	mat_SetIndex	_SCRN0, hl, Flag, vblank unsafe
	jr .flag_loop
.bad_flag
	; skip this flag
	pop	hl
.unflag_loop
	stack_Pop	toUnflag, HL
	; if stack_Pop returns false (no more to pop), then we are done
	ret	nc
	push	hl
	mat_GetIndex	probed, hl
	ifa	==, 1, jr .bad_unflag
	pop	hl
	mat_SetIndex	_SCRN0, hl, Cell, vblank unsafe
	ret
.bad_unflag	; attempted to unflag a probed cell
	pop	hl
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


move_once_if_right:
	if_not	jpad_EdgeRight, ret
	ld	a, [rPlayerX]
	add	8
	ld	[rPlayerX], a
	ld	a, $FF
	ld	[rPlayerHasMoved], a	; place a non-zero value here
	ret

move_once_if_left:
	if_not	jpad_EdgeLeft, ret
	ld	a, [rPlayerX]
	sub	8
	ld	[rPlayerX], a
	ld	a, $FF
	ld	[rPlayerHasMoved], a	; place a non-zero value here
	ret

move_once_if_up:
	if_not	jpad_EdgeUp, ret
	ld	a, [rPlayerY]
	sub	8
	ld	[rPlayerY], a
	ld	a, $FF
	ld	[rPlayerHasMoved], a	; place a non-zero value here
	ret

move_once_if_down:
	if_not	jpad_EdgeDown, ret
	ld	a, [rPlayerY]
	add	8
	ld	[rPlayerY], a
	ld	a, $FF
	ld	[rPlayerHasMoved], a	; place a non-zero value here
	ret


move_sprite_within_screen_bounds:
	; MoveIf* are macros from sprite.inc
	; Only move if NOT on borders
	xor	a	; A=0
	ld	[rPlayerHasMoved], a	; if player has moved this'll be updated
	ld	a, [rPlayerX]
	push	af	; store X value for later
	ifa	<, SCRN_X - 8,	call move_once_if_right
	pop	af
	ifa	>, 0,		call move_once_if_left
	ld	a, [rPlayerY]
	push	af
	ifa	<, SCRN_Y - 8,	call move_once_if_down
	pop	af
	ifa	>, 0,		call move_once_if_up
	jp	update_crosshairs
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
	ret_true	z	; return true if 0-flag has been set
	ret_false


; call this directly from probe_cell if we probed a mine. At start,
; HL = index corresponding to Y,X of probed mine
; A = 1  (indicating yes, it's a mine)
mine_probed:
	push	hl	; store index
	call	queue_color_mines_reveal
	pop	hl	; restore index
	lda	[rGBC]
	ifa	==, 0, ret
.color_exploded_mine
	ld	de, _SCRN0
	add	hl, de	; get VRAM address for mine
	ldpair	b,c,	h,l
	ld	a, 4	; set color of probed mine to red (palette 4)
	stack_Push	minesToReveal, C,B,A
	ret


queue_color_mines_reveal:
	mat_IterInit	mines, 0, SCRN_Y_B,	0, SCRN_X_B
.loop
	mat_IterNext	mines
	ret	nc	; iteration finished
	ifa	==, 0, jr .loop		; keep looping until we find a mine
	; HL contains address @ mines
	ld	de, mines
	negate	de
	add	hl, de	; HL = HL - DE  (aka get index)
	push	hl
	; indicate it's been probed (Prevent ability to flag revealed mine)
	mat_SetIndex	probed, hl, 1
	pop	hl	; HL now contains Index / offset
	push	hl
	mat_GetIndex	flags, hl
	; A=1 if flagged, A=0 if not. This perfectly corresponds with the
	; desired palette, where palette 1 = green, 0 = greyscale
	pop	hl
	ld	de, _SCRN0
	add	hl, de	; HL = VRAM address of tile
	ldpair	b,c,	h,l
.push2stack
	stack_Push	minesToReveal, C,B,A
	jr	nc, .push2stack	; keep trying until stack is open
	jr	.loop


; pop from stack a mine (and corresponding color). Set color of tile and draw
; mine. This should be run in vblank
reveal_queued_mines:
.loop
	stack_Pop	minesToReveal, ABC, thread unsafe
	ret	nc	; return if stack_Pop came up empty
	; A contains palette data, BC contains VRAM address of mine
	; we need to write the mine, and change the color
	ld	d, a	; move palette to d
	lda	[rGBC]
	ifa	==, 0, jr .write_mine
.write_color
	lda	d	; load palette back into reg. A
	ld	hl, rVBK
	ld	[hl], 1	; switch to VRAM color bank
	ld	[bc], a	; write palette to VRAM
	ld	[hl], 0	; switch back to VRAM tile-data bank
.write_mine
	lda	Mine	; load mine
	ld	[bc], a	; write mine to VRAM
	jr	.loop	; continue revealing mines until all are gone


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
	ret	;no longer reti.because dma is now handled by a vblank routine
dmaend:
