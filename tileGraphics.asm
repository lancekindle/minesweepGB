

        IF !DEF(TILE_GRAPHICS_ASM)
TILE_GRAPHICS_ASM       SET     1
; tiles 0,1 are cell & flagged-cell.
; tile 2 is the crosshair section
; tile "0"-"9" is overwritten by custom-shaded graphics
tileGraphics_Load:
	; copy cell, flagged-cell, and crosshairs graphics
	ld	hl, cell_gfx
	ld	de, _VRAM       ; copy starting at tile 0
	ld	bc, end_cellgfx - cell_gfx ;
	call	mem_CopyVRAM
	; copy #0 graphics
	ld	hl, number_gfx
	ld	de, _VRAM + 16 * "0" ; start copying over "0"-"9"
	ld	bc, end_numbergfx - number_gfx
	call	mem_CopyVRAM
        ; copy Mine and flagged-Mine graphics
        ld      hl, mine_gfx
        ld      de, _VRAM + 16 * "*" ; start copying over *
        ld      bc, end_minegfx - mine_gfx
        call    mem_CopyVRAM
	ret


get_cell_font: MACRO
	PUSHO	; push options so that I can change the meaning of .-*@
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; - = 01
	; * = 10
	; @ = 11
	OPT	g.~*@

	; Cell Graphic
	DW	`*.......
	DW	`*~~~~~~.
	DW	`*~~~~~~.
	DW	`*~~~~~~.
	DW	`*~~~~~~.
	DW	`*~~~~~~.
	DW	`*~~~~~~.
	DW	`********

	; Flagged Cell Graphic
	DW	`*.......
	DW	`*@~~~~@.
	DW	`*~@~~@~.
	DW	`*~~@@~~.
	DW	`*~~@@~~.
	DW	`*~@~~@~.
	DW	`*@~~~~@.
	DW	`********

	; 1/4th of crosshairs. We flip (H and V) to create a surrounding box
	DW	`@~@~@~@~
	DW	`~~@~@~@~
	DW	`@@......
	DW	`~~......
	DW	`@@......
	DW	`~~......
	DW	`@@......
	DW	`~~......

	POPO	; restore default options (aka undo g.~*@)
	ENDM

get_mine_font: MACRO
	PUSHO	; push options so that I can change the meaning of .~o@
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; ~ = 01
	; * = 10
	; @ = 11
	OPT	g.~*@

        DW      `........
        DW      `.@@.@@..
        DW      `..@@@...
        DW      `@@@@@@@.
        DW      `..@@@...
        DW      `.@@.@@..
        DW      `........
        DW      `........

        ; graphic for flagged mine
        ; (corresponds to palette 1 ~ green)
        DW      `*......*
        DW      `.*~..~*.
        DW      `..*~~*..
        DW      `~~~**~~~
        DW      `..~**~..
        DW      `.~*~~*~.
        DW      `.*....*.
        DW      `*......*

        ; graphic for untouched mine
        DW      `........
        DW      `.@@.@@..
        DW      `..@@@...
        DW      `@@@@@@@.
        DW      `..@@@...
        DW      `.@@.@@..
        DW      `........
        DW      `........

        ; graphic for untouched mine
        DW      `........
        DW      `.@@.@@..
        DW      `..@@@...
        DW      `@@@@@@@.
        DW      `..@@@...
        DW      `.@@.@@..
        DW      `.........
        DW      `.........

        ; Exploded Mine
        ; (corresponds to palette 4 - Red)
        DW      `...@....
        DW      `..@*@@..
        DW      `.@****@.
        DW      `@**~***@
        DW      `.@*~~*@.
        DW      `.****@@.
        DW      `..**@@..
        DW      `....@...

        ; blank (to be altered by smoke.asm)
        ; will be mixed with above Exploded Mine to produce a flame effect
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........

        POPO    ; restore options (before OPT g.~0@)
        ENDM


; set # fonts such that original gameboy (non-color) shows #'s in increasingly
; dark shades depending on what # it is. Color Gameboy also sets the color
; of the tile...
; 1-2 are light
;  3 is medium.
; 4-9 are DARK
get_number_font: MACRO
	PUSHO	; push options so that I can change the meaning of . & @
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; @ = 01
        ; this means that @ is not very dark...
	OPT	g.@~*

	; 0 number, completely removed from being visible
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........
	DW	`........

	OPT	g.@~*	; realign @ to be light-ish shade
        ; numbers 1 & 2 are lightest (but still visible) shades
        ; this makes it such that they appear LIGHT on the original gameboy
        ; but can otherwise be colored any color (and as dark as desired)
        ; on the gameboy color
        DW      `..@@....
        DW      `.@@@....
        DW      `..@@....
        DW      `..@@....
        DW      `..@@....
        DW      `..@@....
        DW      `@@@@@@..
        DW      `........

        DW      `.@@@@...
        DW      `@@..@@..
        DW      `....@@..
        DW      `..@@@...
        DW      `.@@.....
        DW      `@@..@@..
        DW      `@@@@@@..
        DW      `........

	OPT	g.~@*	; realign @ to be dark (but not darkest) shade
        ; #3 is darkish
        DW      `.@@@@...
        DW      `@@..@@..
        DW      `....@@..
        DW      `..@@@...
        DW      `....@@..
        DW      `@@..@@..
        DW      `.@@@@...
        DW      `........

	OPT	g.~*@
	; any cell 4 or greater should be colored as black as possible
        DW      `...@@@..
        DW      `..@@@@..
        DW      `.@@.@@..
        DW      `@@..@@..
        DW      `@@@@@@@.
        DW      `....@@..
        DW      `...@@@@.
        DW      `........

        DW      `@@@@@@..
        DW      `@@......
        DW      `@@@@@...
        DW      `....@@..
        DW      `....@@..
        DW      `@@..@@..
        DW      `.@@@@...
        DW      `........

        DW      `..@@@...
        DW      `.@@.....
        DW      `@@......
        DW      `@@@@@...
        DW      `@@..@@..
        DW      `@@..@@..
        DW      `.@@@@...
        DW      `........

        DW      `@@@@@@..
        DW      `@@..@@..
        DW      `....@@..
        DW      `...@@...
        DW      `..@@....
        DW      `..@@....
        DW      `..@@....
        DW      `........

        DW      `.@@@@...
        DW      `@@..@@..
        DW      `@@..@@..
        DW      `.@@@@...
        DW      `@@..@@..
        DW      `@@..@@..
        DW      `.@@@@...
        DW      `........

        DW      `.@@@@...
        DW      `@@..@@..
        DW      `@@..@@..
        DW      `.@@@@@..
        DW      `....@@..
        DW      `...@@...
        DW      `.@@@....
        DW      `........


	POPO	; restore default options (aka undo g.~*@)
	ENDM

get_smoke_font: MACRO
	PUSHO	; push options so that I can change the meaning of .~*@
	OPT	g.~*@
        ; smoke MASK
        ; apply this mask to a randomly-generated tile to get
        DW      `....@...
        DW      `.@.@.@..
        DW      `..@.@.@.
        DW      `.@.@.@.@
        DW      `@.@.@.@.
        DW      `.@.@.@..
        DW      `..@.@.@.
        DW      `...@....

        POPO    ; restore options (before OPT g.~0@)
        ENDM

get_firework_font: MACRO
	PUSHO	; push options so that I can change the meaning of .~*@
	OPT	g.~*@

        ; full firework
        DW      `....@...
        DW      `.@@@@@..
        DW      `..@@@@@.
        DW      `.@@@@@@@
        DW      `@@@@@@@.
        DW      `.@@@@@..
        DW      `..@@@@@.
        DW      `...@....

        ; half firework
        DW      `....@...
        DW      `.@.@.@..
        DW      `..@.@.@.
        DW      `.@.@.@.@
        DW      `@.@.@.@.
        DW      `.@.@.@..
        DW      `..@.@.@.
        DW      `...@....

        ; quarter firework
        DW      `........
        DW      `...@....
        DW      `....@.@.
        DW      `.@.@.@..
        DW      `..@.@.@.
        DW      `.@.@....
        DW      `....@...
        DW      `........

        ; barely firework
        DW      `........
        DW      `...@....
        DW      `......@.
        DW      `...@....
        DW      `....@...
        DW      `.@......
        DW      `....@...
        DW      `........


        POPO    ; restore options (before OPT g.~0@)
        ENDM

get_blank_font: MACRO
	PUSHO	; push options so that I can change the meaning of .~*@
	OPT	g.~*@

        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........
        DW      `........

        POPO    ; restore options (before OPT g.~0@)
        ENDM

cell_gfx:
	get_cell_font
end_cellgfx:

number_gfx:
	get_number_font
end_numbergfx

mine_gfx:
        get_mine_font
end_minegfx

smoke_font:
        get_smoke_font
smoke_font_end:

firework_font:
        get_firework_font
firework_font_end:

blank_font:
        get_blank_font
blank_font_end:


        ENDC    ; end tiles_gfx_asm defines
