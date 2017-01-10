; vars.asm
; definitions and macros useful for creating variables pre-compiled,
; or variables residing in the gameboy ram

	IF !DEF(VARS_ASM)
VARS_ASM	SET     1

include "gbhw.inc"

LowRamBase      SET     _RAM + $A0      ; available ram after OAM data
; (OAM data is the $A0 added to _RAM)
LowRamLimit     SET     LowRamBase + $1FF ; 512 bytes (variables) available
MidRamBase      SET     LowRamLimit
MidRamLimit     SET     MidRamBase + $0FFF ; 4096 bytes (var+space) available
; OAM data is just a holding place where you can modify it. The OAM data
; will then get copied to OAM location in Video-Ram [$FE00 - $FEA0)

; LowRamByte is a macro you can call to assign a variable a byte-address in ram
; there's a LOT of available bytes: $C0A0 - $C29F (512)
; each time you call LowRamByte, it assigns your variable an address, then
; increments. You should have no fear of running out of space
; I've set an arbitrary limit so that I can define other sections of memory
; for other purposes
var_LowRamByte:     MACRO
\1      EQU     LowRamBase      ; NOTICE I can't put a space before \1
LowRamBase       Set     LowRamBase + 1
	IF (LowRamBase >= LowRamLimit)
		PRINTT "\n\1 was last LowRamByte declared. "
		FAIL    "too many variables declared"
	ENDC
	ENDM

var_LowRamWord: MACRO
\1	EQU	LowRamBase
LowRamBase	Set	LowRamBase + 2	; \1 points to two bytes
	IF (LowRamBase >= LowRamLimit)
		PRINTT	"\n\1 was last LowRamWord declared. "
		FAIL    "too many variables declared"
	ENDC
	ENDM


; call var_MidRamBytes to allocate memory space for a variable.
; argument 1 is the variable name
; argument 2 is the size of the variable
var_MidRamBytes: MACRO
\1      EQU     MidRamBase
	IF _NARG == 1
		FAIL    "var_MidRamBytes requires a size argument"
	ENDC
MidRamBase      Set     MidRamBase + \2
	IF (MidRamBase >= MidRamLimit)
		PRINTT  "\n$"
		PRINTV  MidRamBase
		FAIL    "\nMidRam space depleted"
	ENDC
	ENDM


; use this macro to load a word from ram. It loads the word into the specified
; register without trashing any other registers (it will push / pop to do so)
; var_GetLowRamWord	register_pair, variable_name
; e.g. var_GetLowRamWord	b,c, player_coordinates
; following Z80 stack-pointer convention, the LSB is stored @address,
; and the MSB is stored @address + 1.
; this means that the example above is psuedo-code for
; ld	c, [player_coordinates]
; ld	b, [player_coordinates + 1]
; register A or F is not allowed because we make use of A
; FAST:	17/14	17 cycles, 14 bytes
var_GetLowRamWord: MACRO
	IF STRIN("bcdehlBCDEHL", "\1") == 0
		FAIL	"require a register (but not A). Got \1"
	ENDC
	IF STRIN("bcdehlBCDEHL", "\2") == 0
		FAIL	"require a register (but not A). Got \2"
	ENDC
	push	af	; save AF (we have to use A)
	lda	[\3]
	ld	\2, a		; load LSB from address
	lda	[\3 + 1]
	ld	\1, a		; load MSB from address+1
	pop	af
	ENDM

; same deal as var_GetLowRamWord. Stores register pair in memory
; at specified address in ram. Sets LSB @address, MSB @address+1
; doesn't trash registers
; register A or F is not allowed because we make use of A
; FAST:	17/14	17 cycles, 14 bytes
var_SetLowRamWord: MACRO
	IF STRIN("bcdehlBCDEHL", "\1") == 0
		FAIL	"require a register (but not A). Got \1"
	ENDC
	IF STRIN("bcdehlBCDEHL", "\2") == 0
		FAIL	"require a register (but not A). Got \2"
	ENDC
	push	af	; save AF (we have to use A)
	lda	\2
	ld	[\3], a		; store LSB at address
	lda	\1
	ld	[\3 + 1], a	; store MSB at address+1
	pop	af
	ENDM



	ENDC    ; end vars.asm definitions
