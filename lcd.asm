; contains a number of defines and code to be used

include "gbhw.inc"

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


lcd_Wait4Vblank
	ld a, [rLY]
	cp 145			; are we at line 145 yet?  (finished drawing screen then)
	jr nz, lcd_Wait4Vblank
	ret

lcd_Stop:
	ld a, [rLCDC]  ; LCD-Controller
	rlca		; rotate. IF LCD is On, bit 7 will carry (into carry flag)
	ret nc		; return if LCD is already off (carry-flag was 0)
.wait4vblank
	ld a, [rLY]
	cp 145			; are we at line 145 yet?  (finished drawing screen then)
	jr nz, .wait4vblank
.stopLCD
	ld a, [rLCDC]
	xor LCDCF_ON	; XOR lcd-on bit with lcd control bits. (toggles LCD off)
	ld [rLCDC], a   ; `a` holds result of XOR operation
	ret

; AUGH. Objects were NOT turned on within the LCD, so my object never showed up
lcd_Begin:
	ld a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON
	ld [rLCDC], a	; we really go all out here....turning on so much stuff
	ret

; modify sprite lcd options to enable sprites of 8bit size
; LCDCF_OBJ16 and LCDCF_OBJ8 control which objects get displayed
; LCDCF_OBJON / LCDCF_OBJOFF control if objects get displayed at all
lcd_ShowSprites:
	ld a, [rLCDC]  ; load current LCD config|LCDCF_OBJ8|LCDCF_OBJON
	or LCDCF_OBJON   ; add the "OBJECTS ON" option
	or LCDCF_OBJ8    ; add the "Obj 8-bit" option
	ld [rLCDC], a  ; push the new LCD config
	ret

lcd_EnableVBlankInterrupt:
	ld a, IEF_VBLANK
	ld [rIE], a             ; config to only allow V-blank interrupts
	ei						; actually enable interrupts ??? I left this out
							; and it still worked
	ret
