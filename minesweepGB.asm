;---------------------------------------------------------------------------
;   Copyright (C) 2017 Lance Kindle
;   Original from <github.com/lancekindle/minesweepGB>
;   Licensed under GNU GPL v3 <http://www.gnu.org/licenses/>
;---------------------------------------------------------------------------

include "gbhw.inc"
include "cgbhw.inc"
include "ibmpc1.inc"
include "dma.inc"



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
	jp	irq_HandleVBLANK
section "LCDC", HOME[$0048]
	jp	irq_HandleLCDC
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

include "syntax.asm"
include "irq.asm"
include "memory.asm"
include "vars.asm"
include "sprite.inc"
include "joypad.asm"
include "lcd.asm"
include "math.asm"
include "matrix.asm"
include "stack.asm"
include "random.asm"
include "rgb.asm"
include "tileGraphics.asm"
include "movement.asm"
include "crosshairs.asm"

; declare some variables
	; set ram variables
	var_LowRamByte	rGBC	; set to > 0 if color gameboy present
	var_LowRamByte	rGBA	; set to > 0 if running on gameboy advance
					
	var_LowRamByte	rDifficulty	; 0-255 = chance to lay mine. 0=easy
	var_LowRamByte	rDenseDifficulty
	var_LowRamByte	rDifficultyRamp	; increase after winning
	var_LowRamByte	rNearbyCount
	var_LowRamByte	rCellY
	var_LowRamByte	rCellX
	var_LowRamByte	rFirstProbe
	var_LowRamByte	rMinesCount
	var_LowRamByte	rGameOver
	var_LowRamByte	rWinner
	var_LowRamWord	rCellsRemaining
	var_LowRamByte	rCorrectFlags
	var_LowRamByte	rWrongFlags
	var_LowRamByte	rShakeScreen
	var_LowRamWord	rShakeCounter

	mat_Declare	_SCRN0, SCRN_VY_B, SCRN_VX_B	; setup screen matrix
	mat_Declare	mines, SCRN_VY_B, SCRN_VX_B	; setup mines matrix
	mat_Declare	flags, SCRN_VY_B, SCRN_VX_B	; setup flags matrix
	mat_Declare	probed, SCRN_VY_B, SCRN_VX_B	; keep track of which
		; coordinates have already been explored
	stack_Declare	toExplore, 255	; just a random stack size
		; will hold a temporary storage of searchable cells
	; toReveal holds coordinates and value to place on cells
	stack_Declare	toReveal, 10*3	; reveal up to 10 squares per vblank
	stack_Declare	minesToReveal, 4*3	; reveal +4 mines per vblank
	stack_Declare	VRAMBytesToLoad, 5*3	; hold 5 bytes to load
; smoke and tile need VRAMBytesToLoad stack pre-declared
include "smoke.asm"
include "title.asm"
include "firework.asm"
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
	ld	a, 8*8		; place player coordinates directly in middle
	ld	[rPlayerY], a
	ld	[rCrosshairY], a
	ld	a, 9*8
	ld	[rPlayerX], a
	ld	[rCrosshairX], a
	call	crosshairs_setup
	ret


bg_color_palettes:
	; greyscale
	rgb_Set 255, 255, 255	; white
	rgb_Set	192, 192, 192	; light grey
	rgb_Set	127, 127, 127	; dark grey
	rgb_Set	  0,   0,   0	; black
	; green on white (good indicator)
	rgb_Set 255, 255, 255	; white
	rgb_Set	  0, 255,   0	; green
	rgb_Set	  0, 127,   0	; light green
	rgb_Set	  0, 100,   0	; dark green
	; blue on white
	rgb_Set 255, 255, 255	; white
	rgb_Set	0,   0,   192	; darkish blue
	rgb_Set	0,   0,   127	; light blue
	rgb_Set	0,   0,   255	; blue
	; purple on white
	rgb_Set 255, 255, 255	; white
	rgb_Set 127,   0, 127	; light magenta
	rgb_Set	$7D, $05, $52	; dark orchid
	rgb_Set	255,   0, 255	; magenta
	; red on white (bad indicator)
	rgb_Set 255, 255, 255	; white
	rgb_Set	255, 100, 100	; light red
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
	; we get here if rom tells the gbc that it's NOT color compatible
	; so even though we may detect gbc, gba hardware, we must NOT use any
	; color features (because they won't be enabled in-hardware)
	; Pretend we're on the original gameboy
	pop	af
	ret

init_colorgb_variables:
	ld	a, 1
	; at the very least, set the fact that color is supported
	ld	[rGBC], a
	; check if gameboy color or gameboy advance
	bit	0, b	; check bit 0 of B. It's gba if 1
	if_flag	nz,	ld [rGBA], a
	call	cpu_doublespeed_switch
	xor	a
	ld	hl, bg_color_palettes
	call	rgb_SetAllBGP
	xor	a
	ld	hl, bg_color_palettes
	call	rgb_SetAllOBJP	; set sprite palettes to mirror bg
	ret

; this properly switches the cpu to double speed mode (cbg-only).
; this avoids the issue where if a key is held down during the switch, the
; gameboy will hang. We avoid it by disabling interruption by the keypad
; (see Cycle-Accurate Game Boy Docs by AntonioND)
cpu_doublespeed_switch:
	ldh	a, [rIE]
	ld	b, a	; save Interrupt-enable's for later
	xor	a, a
	ldh	[rIE], a	; clear Interrupt-enables
	ld	a, $30
	ld	[rP1], a  ; briefly disable joypad from triggering interrupts
	ld	a, $01
	ldh	[rKEY1], a	; tell cpu "prepare for hammer-time!"
	stop ; Switch speed.
	; stop normally halts cpu (in a bad way). But since we've just
	; enabled double-speed mode, the cpu will pick up (After a screen
	; flicker) in double-speed mode
	ld a,b
	ldh [rIE],a		; restore Interrupt-enable's
	ret ; Restore IE.<Paste>


; set low-ram variables to 0
init_variables:
	xor	a	; set A=0
	ld	[rNearbyCount], a
	ld	[rCellY], a
	ld	[rCellX], a
	ld	[rMinesCount], a
	ld	[rGameOver], a
	ld	[rCorrectFlags], a
	ld	[rWrongFlags], a
	ld	[rShakeScreen], a
	; set jpad variables
	call	move_InitJpadVariables
	; others
	ld	bc, SCRN_X_B * SCRN_Y_B	; number of cells (20x18 == 360)
	var_SetWord	b,c,	rCellsRemaining
	ld	a, 1
	ld	[rFirstProbe], a
	ret
init_startup_variables:
	lda	30
	ld	[rDifficulty], a	; store default difficulty of 30
	ld	[rDifficultyRamp], a
	lda	100
	ld	[rDenseDifficulty], a	; 200 used to be default. Small=easy
	lda	0
	ld	[rWinner], a
	ret


begin:
	di    ; disable interrupts
	ld	sp, $ffff  ; init stack pointer to be at top of memory
	call	check_hardware	; where we check for, and set up GBC & GBA vars
	call	init_startup_variables
	dma_Copy2HRAM
reset:		; also called after player loses and is resetting
	di
	ld	sp, $ffff	; reset stack to top
	irq_DisableAll	; disable all interrupts. So irq won't mess w/ state
	call	init_variables
	call	lcd_ScreenInit		; set up pallete and (x,y)=(0,0)
	call	lcd_Stop
	mat_Init	_SCRN0, Blank	; initialize screen background with " "
	lda	[rGBC]
	ifa	>=, 1, rgb_InitVRAMColorBank	0
	mat_Init	mines, 0	; setup variables
	mat_Init	flags, 0
	mat_Init	probed, 0
	stack_Init	toExplore
	stack_Init	toReveal
	stack_Init	minesToReveal
	stack_Init	VRAMBytesToLoad
	stack_Init	toFlag
	stack_Init	toUnflag
	call	LoadFont
	call	tileGraphics_Load
	call	ClearSpriteTable
	call	lcd_On
	call	SpriteSetup
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	lda	[rWinner]
	ifa	==, 0, call title_LevelSelect
	irq_DisableVBLANK
	mat_Init	_SCRN0, 0	; initialize screen background with cells
	call	fill_mines
	call	remove_dense_mines
	; redirect vlbank to our main logic handler (updating graphics)
	irq_CallFromVBLANK	handle_vblank
	irq_EnableVBLANK
	; lcdc handles joypad, crosshairs, flags,& queueing graphics changes
	irq_CallFromLCDC	handle_lcdc_main_game
	irq_EnableLCDC_AtLine	100
; mainloop handles probing cells and endgame logic
.mainloop:
	; TEMPORARY to trigger wingame
	call	win_game
	if_	jpad_EdgeA, call	probe_cell
	; queue_color_mines_reveal is dmg compatible. The actual reveal
	; loop is the one that needs to be color-sensitive
	if_	only_mines_left, call win_game
	lda	[rGameOver]
	ifa	>=, 1, jr .won_lost_mainloop
	jr	.mainloop; jr is Jump Relative (it's quicker than jp)
; won/lost mainloop handles game over logic (waiting for keypress to reset)
.won_lost_mainloop
	; exploded mainloop monitors for keypress, then resets gameboy
	call	jpad_GetKeys
	if_	jpad_EdgeA, jp	reset
	jr .won_lost_mainloop


; a macro that gets called in the middle of reading from the toReveal stack
; the rules here are we must preserve HL from start to end,
; and in this case, A & BC are loaded with values.
; BC contains the Index of tile, and A contains the # to print
writeColorCells2VRAM: MACRO
	; preserve HL (stack-pointer) and DE (we don't use DE)
	push	hl
	; data is in A,BC
.display_number\@
	ld	hl, _SCRN0
	add	hl, bc	; get address of #'d tile (and color) in VRAM
	ld	[hl], a	; write number to screen
	ifa	==, "0", jr .done\@ ; don't need to change palette for empty cell
.change_color\@
	ldpair	bc, hl	; move VRAM address to bc
	ld	hl, rVBK
	ld	[hl], 1	; change to VRAM bank 1 (color bank)
	sub	"0"	; get integer palette # corresponding to string count
	ifa	>, 4, ld a, 4	; use palettes 0-4
	ld	[bc], a	; set palette
	xor	a
	nop
	ld	[hl], a	; change back to VRAM bank 0 (tile-data bank)
.done\@
	pop	hl
	ENDM

; this macro write cell #'s to vram. It needs to preserve HL & DE (it does)
writeCells2VRAM: MACRO
	push	hl
	mat_SetIndex	_SCRN0, bc, a, vblank unsafe
	pop	hl
	ENDM


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
	;pushall is handled by irq_HandleVBLANK
	call	DMACODELOC ; DMACODE copies data from _RAM / $100 to OAMDATA
	lda	[rGBC]
	ifa	==, 0, jr .dmg_probe_display
.cgb_probe_display
	stack_BatchRead		toReveal, writeColorCells2VRAM, A,B,C
	jr .reveal_flag
.dmg_probe_display
	stack_BatchRead		toReveal, writeCells2VRAM, A,B,C
.reveal_flag
	call	reveal_queued_flags
.load_vram_bytes
	; for loading specific byte-changes (tile-alteration or otherwise)
	call	load_bytes_into_vram
.reveal_mines
	call	reveal_queued_mines ; (which only happens during game over)
.done
	;popall is handled by irq_HandleVBLANK
	reti

; handle interrupts thrown when lcd screen is at a certain point
; this will be set to just before VBLANK occurs. So this means the lcd-line
; interrupt will handle all per-frame logic not needed during vblank, which
; includes:
;	* move player & crosshairs
;	* queue flag / unflag
;	* queue byte changes within vram
handle_lcdc_main_game:
	;pushall is handled by irq handler
.move
	call	jpad_GetKeys  ; loads keys into register a, and jpad_rKeys
	call	move_player_within_screen_bounds	; & move crosshairs
	call	crosshairs_move_halfway_to_player
.flag
	if_	jpad_EdgeB, call	toggle_flag
.done
	;popall is handled by irq handler
	ret

; handle smoke particles and screen shake. mainloop will handle joypad
handle_lcdc_exploded:
	call	update_smoke_particles
.shake_screen
	lda	[rShakeScreen]
	ifa	==, 0, jr .done
	call	shake_screen_interrupt
.done
	ret


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
	lda	[rDifficulty]
	ld	b, a	; store difficulty in B (default was 30)
	rand_A
	ifa	>, b, jr .iterate
	; we place mine below this point
	; [HL] (from abover iterNext of mines) will place mine @ location
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
	lda	[rDenseDifficulty]	; default value of 200
	ld	b, a	; small DenseDifficulty = easy: mine removed more often
	rand_A
	ifa	>, b, jp .iterate
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
	ldpair	bc, de	; Y,X in BC now
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
	ldpair	bc, de	; move Y,X into BC
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
	ldpair	bc, hl	; move Index to b,c
	add	"0"	; create string equivalent of count
.pushing
	; push Index and mine-count for later reveal during vblank
	stack_Push	toReveal, C,B,A, thread_safe
	jr nc, .pushing	; keep trying to push onto stack until it succeeds
	lda	[rNearbyCount]
	ret




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
	ldpair	bc, hl	; move VRAM address to bc
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
	; we assume this is during v-blank. Just do it
	mat_SetIndex	_SCRN0, hl, Flag, vblank unsafe
.unflag_loop
	stack_Pop	toUnflag, HL
	; if stack_Pop returns false (no more to pop), then we are done
	ret	nc
	mat_SetIndex	_SCRN0, hl, Cell, vblank unsafe
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


win_game:
	di
	irq_DisableAll
	irq_EnableVBLANK
	call	firework_create_particles
	irq_CallFromLCDC	firework_HandleLCDC
	irq_EnableLCDC_AtLine	100
	ei
	lda	$FF
	ld	[rWinner], a	; both winner and GameOver need to be set
	ld	[rGameOver], a
	ld	a, [rDifficultyRamp]
	ld	hl, rDifficulty
	add	[hl]
	ld	[hl], a ; difficulty = difficulty + ramp
	call	crosshairs_disappear
	call	queue_color_mines_reveal
	ret


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
; this function returns back to mainloop (so not necessary to preserve HL
mine_probed:
	push	hl	; store index of mine
	; change lcdc handler to shake screen & update smoke particles
	irq_CallFromLCDC	handle_lcdc_exploded
	call	shake_screen_init
	call	create_smoke_particles
	call	crosshairs_disappear
	lda	$FF
	ld	[rGameOver], a
	xor	a
	ld	[rWinner], a
	call	queue_color_mines_reveal
.wait_for_empty_stack
	stack_Size	minesToReveal
	jr	c, .wait_for_empty_stack
	; loop until no more mines to draw, then draw red & exploded-mine
	; Why? Because usually the red mine
	; gets drawn over by the black mine that we reveal. To prevent that,
	; I wait until the minesToReveal is empty, indicating that all mines
	; have been drawn and then push it to the stack again so that it'll
	; FOR SURE not get drawn over.
	pop	hl	; restore index
.color_exploded_mine
	ld	bc, _SCRN0
	add	hl, bc	; get VRAM address for mine
	ldpair	bc, hl
	ld	a, "*" + 4	; set color of probed mine to red (palette 4)
	stack_Push	minesToReveal, C,B,A, thread_safe
	ret


; use to shake screen around after exploding on mine
; change x,y once per vblank, essentially. Blocking: meaning no other
; valuable computation may occur during this shake effect
; USES:	af, bc
shake_screen_init:
	; set up shake-counter so that effect ends after ... 700ms?
	ld	bc, $001F
	inc	c
	inc	b	; increment so that loop counter works
	var_SetWord	b,c, rShakeCounter, trash AF
	; enable shaking of screen
	lda	$FF
	ld	[rShakeScreen], a
	ret


shake_screen_interrupt:
	var_GetWord	b,c, rShakeCounter, trash AF
.bump_x_frame
	rand_A
	ld	hl, rSCX
	ifa	>, 155, inc [hl]	; bump right
	ifa	<, 100, dec [hl]	; bump left
	lda	[hl]
.x_within_bounds
	ifa	>, 250, jr .negative_x
.positive_x
	ifa	>, 2, ld [hl], 2
	jr	.bump_y_frame
.negative_x
	ifa	<, 254, ld [hl], 254
.bump_y_frame
	rand_A
	ld	hl, rSCY
	ifa	>, 155, inc [hl]	; bump down
	ifa	<, 100, dec [hl]	; bump up
	lda	[hl]
.y_within_bounds
	ifa	>, 250, jr .negative_y
.positive_y
	ifa	>, 2, ld [hl], 2
	jr	.bump_done
.negative_y
	ifa	<, 254, ld [hl], 254
.bump_done	; now we decrement counter. Completely Done if counter == 00
	dec	c
	jr	nz, .frame_done
	dec	b
	jr	z, .fully_done
.frame_done
	var_SetWord	b,c, rShakeCounter, trash AF
	ret
.fully_done	; shake effect complete.
	xor	a
	; disable shake variable
	ld	[rShakeScreen], a
	; Now we restore x,y to (0,0)
	ldh	[rSCX], a
	ldh	[rSCY], a
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
	add	"*"	; add mine. If flagged, it'll equal a flagged-mine
	pop	hl
	ld	de, _SCRN0
	add	hl, de	; HL = VRAM address of tile
	ldpair	bc, hl
.push2stack
	stack_Push	minesToReveal, C,B,A, thread_safe
	jr	nc, .push2stack	; keep trying until stack is open
	jr	.loop


; pop from stack a mine (and corresponding color). Set color of tile and draw
; mine. This should be run in vblank
reveal_queued_mines:
.loop
	stack_Pop	minesToReveal, ABC
	ret	nc	; return if stack_Pop came up empty
	; A contains palette data, BC contains VRAM address of mine
	; we need to write the mine, and change the color
	ld	d, a	; move mine-graphic / palette into d
	lda	[rGBC]
	ifa	==, 0, jr .write_mine
.write_color
	lda	d	; load palette back into reg. A
	sub	"*"	; subtract mine-graphic gets palette (0=normal, 1=green)
	ld	hl, rVBK
	ld	[hl], 1	; switch to VRAM color bank
	ld	[bc], a	; write palette to VRAM
	ld	[hl], 0	; switch back to VRAM tile-data bank
.write_mine
	lda	d	; reload mine-graphic into a
	ld	[bc], a	; write mine to VRAM
	jr	.loop	; continue revealing mines until all are gone


copyByte2VRAM: MACRO
	; REG:	A holds byte
	; 	BC holds VRAM address to which to write the byte
	; HL, DE must be preserved
	ld	[bc], a
	ENDM

; read from stack VRAMBytesToLoad and load tiles into VRAM.
load_bytes_into_vram:
	stack_BatchRead	VRAMBytesToLoad, copyByte2VRAM, A,B,C
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
	; Yes, ours is monochrome. An interesting thing about the mem_CopyMono
	; fxn: it copies each byte from rom font TWICE. Each time to a
	; sequential destination. But why? Because the gameboy takes 1 bit
	; from each of two bytes to determine a pixel's shade in a row.
	; To see RGBDS's syntax for handling this in a human-readable graphic,
	; see my tileGraphics.asm. (Hint: preface each row with ` character)
