; hello-sprite.inc
; definitions used in hello-sprite.asm
; updates:
;  2008-03-23: added conditional so it is not interpreted by the assembler 2x

        IF      !DEF(HELLO_SPRITE_INC)
HELLO_SPRITE_INC  SET  1

SPEED		EQU	$0fff

DMACODELOC	EQU	$ff80
OAMDATALOC	EQU	_RAM				; must be multiple of $100
OAMDATALOCBANK	EQU	OAMDATALOC/$100 
OAMDATALENGTH	EQU	$A0

LoRamBase       SET     OAMDATALOC + OAMDATALENGTH	; low area of RAM for storing variables

LoByteVar:	MACRO
\1		EQU	LoRamBase
LoRamBase	SET	LoRamBase+1
		ENDM

		printt  "From Sprite.inc\n"
		printv	OAMDATALOCBANK
		printt	"TEXT!!!!\n"
		
		RSSET	OAMDATALOC

; RSSET and RB are macros / compiler stuff that increments a value called __RS
; so let's talk about __RS. It's a compiler-tracked byte. 
; RSSET 100        ; sets the __RS value to 100
; X		RB  25     ; sets X to 100 (current value of __RS) then adds 25 to __RS
; Y		RB  14     ; sets Y to 125 (current value of __RS) then adds 14 to __RS

; since SpriteAttr is called right after __RS is set to OAMDATALOC,
; then RB 1 is assigning an OAMDATA location to each value and then
; incrementing __RS, Thus giving Y, X Addr, Tilenum, & Flags a unique OAM data
; location.
SpriteAttr:	MACRO
\1YAddr		RB	1
\1XAddr		RB	1
\1TileNum	RB	1
\1Flags		RB	1
		ENDM

;set XAddr of sprite.
;format:
;	PutSpriteXAddr	SpriteName,r8
;	PutSpriteXAddr	SpriteName,n8
PutSpriteXAddr:	MACRO
		push	af
		ld	a,\2
		add	8	
		ld	[\1XAddr],a
		pop	af
		ENDM

;set YAddr of sprite.
;format:
;	PutSpriteYAddr	SpriteName,r8
;	PutSpriteYAddr	SpriteName,n8
PutSpriteYAddr:	MACRO
		push	af
		ld	a,\2
		add	16	
		ld	[\1YAddr],a
		pop	af
		ENDM

GetSpriteXAddr:	MACRO
		ld	a,[\1XAddr]
		sub	8
		ENDM

GetSpriteYAddr:	MACRO
		ld	a,[\1YAddr]
		sub	16
		ENDM

MoveLeft: MACRO
	; move sprite (\1) to the left by x (\2)
	GetSpriteXAddr \1  ; loads x into a
	sub \2
	PutSpriteXAddr \1, a
	ENDM

MoveIfLeft: MACRO
	; load keys in a, and pass spriteName as argument
	; IF Left key was pressed, will move left by second argument
	push af
	and PADF_LEFT
	jr z, .skipLeft\@  ; return (do nothing) if left was not pressed
	MoveLeft \1, \2
.skipLeft\@
	pop af
	ENDM

MoveOnceIfLeft: MACRO
	; only moves once per depressed key. Waits for key to be released
	; and then re-pressed to move again
	push af
	and PADF_LEFT
	jr z, .resetOnceLeft\@  ; return (do nothing) if left was not pressed
	jr nz, .leftHeld\@  ; reset variable that determines if left pushed
.resetOnceLeft\@
	ld a, 0
	ld [VAR_LEFT], a
	jr .skipOnceLeft\@
.leftHeld\@
	add [VAR_LEFT], $100
	ld a, [VAR_LEFT]
	cp $100
	jr nz, .skipOnceLeft\@   ; if [VAR_LEFT] != $100, then we already pressed LEFT
	MoveLeft \1, \2
.skipOnceLeft\@
	pop af
	ENDM

MoveIfRight: MACRO
	; load keys in a, and pass spriteName as argument
	; IF Right key was pressed, will move right by second argument
	push af
	and PADF_RIGHT
	jr z, .skipRight\@  ; return (do nothing) if key was not pressed
	GetSpriteXAddr \1
	add \2
	PutSpriteXAddr \1, a
.skipRight\@
	pop af
	ENDM

MoveIfUp: MACRO
	; load keys in a, and pass spriteName as argument
	; IF Up key was pressed, will move right by second argument
	push af
	and PADF_UP
	jr z, .skipUp\@  ;do nothing if key was not pressed
	GetSpriteYAddr \1
	sub \2
	PutSpriteYAddr \1, a
.skipUp\@
	pop af
	ENDM

MoveIfDown: MACRO
	; load keys in a, and pass spriteName as argument
	; IF Down key was pressed, will move right by second argument
	push af
	and PADF_DOWN
	jr z, .skipDown\@ ; return (do nothing) if key was not pressed
	GetSpriteYAddr \1
	add \2
	PutSpriteYAddr \1, a
.skipDown\@
	pop af
	ENDM

		ENDC			; HELLO_SPRITE_INC
