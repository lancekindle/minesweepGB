

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
	DW	`oX----X.
	DW	`o-X--X-.
	DW	`o--XX--.
	DW	`o--XX--.
	DW	`o-X--X-.
	DW	`oX----X.
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

get_mine_font: MACRO
	PUSHO	; push options so that I can change the meaning of .-oX
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; - = 01
	; o = 10
	; X = 11
	OPT	g.-oX

        DW      `........
        DW      `.XX..XX.
        DW      `..XXXX..
        DW      `XXXXXXXX
        DW      `..XXXX..
        DW      `.XX..XX.
        DW      `........
        DW      `........

        ; graphic for flagged mine
        DW      `........
        DW      `.Xo..oX.
        DW      `..XooX..
        DW      `oooXXooo
        DW      `..oXXo..
        DW      `.oX..Xo.
        DW      `.X....X.
        DW      `........

        ; graphic for untouched mine
        DW      `........
        DW      `.XX..XX.
        DW      `..XXXX..
        DW      `XXXXXXXX
        DW      `..XXXX..
        DW      `.XX..XX.
        DW      `........
        DW      `........

        ; graphic for untouched mine
        DW      `........
        DW      `.XX..XX.
        DW      `..XXXX..
        DW      `XXXXXXXX
        DW      `..XXXX..
        DW      `.XX..XX.
        DW      `........
        DW      `........

        ; graphic for exploded mine
        ; needed so that when setting a palette of 4 (red) there's a
        ; corresponding mine graphic
        DW      `........
        DW      `.oX..oX.
        DW      `..XXoX..
        DW      `XoXoXoXo
        DW      `..XXoX..
        DW      `.Xo..Xo.
        DW      `........
        DW      `........



        POPO    ; restore options (before OPT g.-0X)
        ENDM

; set # fonts such that original gameboy (non-color) shows #'s in increasingly
; dark shades depending on what # it is. Color Gameboy also sets the color
; of the tile...
; 1-2 are light
;  3 is medium.
; 4-9 are DARK
get_number_font: MACRO
	PUSHO	; push options so that I can change the meaning of . & X
	; change graphics characters. Start the line with ` (for graphics)
	; . = 00
	; X = 01
        ; this means that X is not very dark...
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

	OPT	g.X-o	; realign X to be dark (but not darkest) shade
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

	OPT	g.-Xo	; realign X to be dark (but not darkest) shade

        DW      `.XXXX...
        DW      `XX..XX..
        DW      `....XX..
        DW      `..XXX...
        DW      `....XX..
        DW      `XX..XX..
        DW      `.XXXX...
        DW      `........

	OPT	g.-oX	; realign X to be dark (but not darkest) shade
	; any cell 4 or greater should be colored as black as possible
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

mine_gfx:
        get_mine_font
end_minegfx



        ENDC    ; end tiles_gfx_asm defines
