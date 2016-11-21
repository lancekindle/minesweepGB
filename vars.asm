; vars.asm
; definitions and macros useful for creating variables pre-compiled,
; or variables residing in the gameboy ram

        IF !DEF(VARS_ASM)
VARS_ASM        SET     1

include "gbhw.inc"

LowRamBase      SET     _RAM + $A0      ; available ram after OAM data
LowRamLimit     SET     LowRamBase + $ff ; 256 bytes (variables) available
; OAM data is just a holding place where you can modify it. The OAM data
; will then get copied to OAM location in Video-Ram [$FE00 - $FEA0)

; LowRamByte is a macro you can call to assign a variable a byte-address in ram
; there's a LOT of available bytes: $C0A0 - $DFFF
; each time you call LowRamByte, it assigns your variable an address, then
; increments. You should have no fear of running out of space
; I've set an arbitrary limit so that I can define other sections of memory
; for other purposes
var_LowRamByte:     MACRO
\1      EQU     LowRamBase      ; NOTICE I can't put a space before \1
LowRamBase       Set     LowRamBase + 1
        IF (LowRamBase == LowRamLimit)
                FAIL    "too many variables declared"
        ENDC
        ENDM

        ENDC    ; end vars.asm definitions
