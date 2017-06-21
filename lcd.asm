; contains a number of defines and code to be used

include "gbhw.inc"

	; prevent re-loading lcd.asm if it's already loaded
	IF	!DEF(LCD_ASM)
LCD_ASM		SET	1

;================== FROM GBHW.INC  (FOR REFERENCE)===========================
; --
; -- LCDC ($FF40)
; -- LCD Control (R/W)
; --
;rLCDC EQU $FF40

;LCDCF_OFF     EQU  %00000000 ; LCD Control Operation
;LCDCF_ON      EQU  %10000000 ; LCD Control Operation
;LCDCF_WIN9800 EQU  %00000000 ; Window Tile Map Display Select
;LCDCF_WIN9C00 EQU  %01000000 ; Window Tile Map Display Select
;LCDCF_WINOFF  EQU  %00000000 ; Window Display
;LCDCF_WINON   EQU  %00100000 ; Window Display
;LCDCF_BG8800  EQU  %00000000 ; BG & Window Tile Data Select
;LCDCF_BG8000  EQU  %00010000 ; BG & Window Tile Data Select
;LCDCF_BG9800  EQU  %00000000 ; BG Tile Map Display Select
;LCDCF_BG9C00  EQU  %00001000 ; BG Tile Map Display Select
;LCDCF_OBJ8    EQU  %00000000 ; OBJ Construction
;LCDCF_OBJ16   EQU  %00000100 ; OBJ Construction
;LCDCF_OBJOFF  EQU  %00000000 ; OBJ Display
;LCDCF_OBJON   EQU  %00000010 ; OBJ Display
;LCDCF_BGOFF   EQU  %00000000 ; BG Display
;LCDCF_BGON    EQU  %00000001 ; BG Display
;
;======================= (end reference) ====================================


; macro to safely wait for V-Blank. At that point, VRAM is available to write
; if user specifies argument "trash AF", then AF will not be preserved
; (which makes access just a tiny bit faster)
lcd_Wait4VBlank: MACRO
trash\@ = 0
	IF _NARG == 1
		IF STRCMP("TRASH AF", STRUPR("\1")) != 0
trash\@ = 1
		ENDC
	ENDC
	IF trash\@ == 0
		push	af
	ENDC
.wait4vblank\@
	ldh	a, [rLY]
	cp	145	; are we at line 145 yet?  (finished drawing screen then)
	jr	nz, .wait4vblank\@
	IF trash\@ == 0
		pop	af
	ENDC
	ENDM

lcd_Stop:
	ldh	a, [rLCDC]  ; LCD-Controller
	rlca		; rotate. IF LCD is On, bit 7 will carry (into carry flag)
	ret	nc		; return if LCD is already off (carry-flag was 0)
.wait4vblank
	ldh	a, [rLY]
	cp	145	; are we at line 145 yet?  (finished drawing screen then)
	jr	nz, .wait4vblank
.stopLCD
	ldh	a, [rLCDC]
	xor	LCDCF_ON	; XOR lcd-on bit with lcd control bits. (toggles LCD off)
	ldh	[rLCDC], a   ; `a` holds result of XOR operation
	ret

; minimally turn on lcd. Preserve any Previous settings turned on
; AUGH. Objects were NOT turned on within the LCD, so my object never showed up
lcd_On:
	ldh	a, [rLCDC]
	or	LCDCF_ON
	ldh	[rLCDC], a
	ret

; beginner-style lcd-on command. Enables everything in the lcd and initializes
; the pallette for background and sprites. Does NOT enable VBlank Interrupt,
; however. That one's important to enable once you're ready
lcd_Begin:
	ld	a, LCDCF_ON
	ldh	[rLCDC], a
	call	lcd_ShowBackground
	call	lcd_ShowSprites
	call	lcd_ScreenInit
	ret

; enable all backgrounds
lcd_ShowBackground:
	ldh	a, [rLCDC]
	or	LCDCF_BG8000
	or	LCDCF_BG9800
	or	LCDCF_BGON
	ldh	[rLCDC], a
	ret

; modify sprite lcd options to enable sprites of 8bit size
; LCDCF_OBJ16 and LCDCF_OBJ8 control which objects get displayed
; LCDCF_OBJON / LCDCF_OBJOFF control if objects get displayed at all
lcd_ShowSprites:
	ldh	 a, [rLCDC]  ; load current LCD config|LCDCF_OBJ8|LCDCF_OBJON
	or	LCDCF_OBJON   ; add the "OBJECTS ON" option
	or	LCDCF_OBJ8    ; add the "Obj 8-bit" option
	ldh	[rLCDC], a  ; push the new LCD config
	ret

lcd_EnableVBlankInterrupt:
	ld	a, IEF_VBLANK
	ldh	[rIE], a             ; config to only allow V-blank interrupts
	ei						; actually enable interrupts ??? I left this out
							; and it still worked
	ret

; init screen. (x,y) == (0,0) and setup pallette to standard
lcd_ScreenInit:
	ld	a, 0		; set (x,y) to (0,0)
	ldh	[rSCX], a
	ldh	[rSCY], a
	ld	a, %11100100	; setup pallette colors
	ldh	[rBGP], a	;  set background pallet
	ldh	[rOBP0], a	;  set sprite/obj pallete 0
	ldh	[rOBP1], a	;  set sprite/ obj pallete 1
	; lh   vs.   ldh.    ldh sets address to $ff00 and then adds (?) nn
	; OR ldh sets address to nn BUT sets 2nd byte to ff
	ret


	ENDC	; end lcd-defines
