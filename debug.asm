
; this is a macro that stores in memory a debug message. This message will
; be skipped by the actual hardware, but in BGB or No$GMB it will display
; in their "debug messages" window
; ( I have not yet tried this one, but I see gists that show this. Should work)
bug_message: MACRO
	ld	d, d
	jr .end\@
	DW $6464
	DW $0000
	DB \1
.end\@
	ENDM

; call this macro to automatically break / halt execution when this instruction
; is reached. This macro uses the bug_message macro, so you must pass a
; message into this macro as well
; ( I have not yet tried this one )
bug_break: MACRO
	bug_message	\1
	ld	b, b
	; I'm not sure if the below instructions are required, but I figure
	; it's important to try
	jr .end\@
	DW $6464
	DW $0000
.end\@
	ENDM

; debug file is meant as a way to debug (LIVE!) running programs
; pass bug_ macros the text you wish to display, and it'll be written
; (in serial fashion) to rDIV, aka the address $FF04. Since most people don't
; ever write to this register, I figure it's safe to use for debugging purposes

; take a snapshot of the system registers
; in order: af bc de hl  (yes, we also write the flags reg.)
; finally, SP and PC
; all of these register are written (one at a time, MSB first) to rDIV.
; if you use hardware or software to monitor writes to this address, you
; can capture the debug information.
; This routine preserves all registers, AND preserves (as best as possible)
; current interrupt and timer state.
; This writes heavily to rDIV, which has the side effect of incrementing other
; timers extremely quickly. To counteract this, I preserve interrupt flags
; and the timer and restore them at the end.
bug_snapshot:
	di	; disable interrupts since we'll be doing some SP manipulation
	push	af
	ld	a, [rTIMA]	; preserve timer counter. We'll restore
	push	af		; at end of snapshot
	ld	a, [rIF]	; preserve interrupt flags. We'll restore
	push	af		; at end of snapshot
	push	hl
	add	sp, 6		; backtrack past pushed HL & rIF & rTIMA values
	pop	af		; get original AF
	ld	[rDIV], a	; write A
	add	sp, -1		; go forward 1 byte. To align SP with F (flags)
				; (remember SP starts at top of memory)
	pop	af		; now we pop off F? into AF
	ld	[rDIV], a	; write F
	; at this point, SP is +1 (1 byte before our pushed values began).
	; it also is 1 byte into the return address (pushed when calling
	; bug_snapshot)
	; we've pushed a total of 8 bytes. So SP is 9 bytes away from top
	; of where we want to be
	add	SP, -1	; align SP to where it was before we pushed 8 bytes
			; (so SP is 8 bytes from top). From here we can Pop the
			; return address -- aka PC before snapshot was called
	ld	a, b
	ld	[rDIV], a	; write b
	ld	a, c
	ld	[rDIV], a	; write c
	ld	a, d
	ld	[rDIV], a	; write d
	ld	a, e
	ld	[rDIV], a	; write e
	ld	a, h
	ld	[rDIV], a	; write h
	ld	a, l
	ld	[rDIV], a	; write l
	ld	hl, sp+2	; get SP address (before fxn call)
				; +2 basically offsets so that HL now holds
				; where SP would have pointed right before this
				; routine was called
	ld	a, h
	ld	[rDIV], a	; write S
	ld	a, l
	ld	[rDIV], a	; write P
			; (remember, we left SP ready to pop off return addr)
	pop	hl	; return address == PC before bug_snapshot was called
			; SP is now 10 bytes away from top (PC,AF,AF,AF,HL)
	add	sp, -10	; return SP to top of stack, ready to pop
	ld	a, h
	ld	[rDIV], a	; write P
	ld	a, l
	ld	[rDIV], a	; write C
	; that's it! We've written to rDIV our entire register stack, in order.
	; AF, BC, DE, HL, SP, PC
	pop	hl
	pop	af		; restore original interrupt flags (so that we
	ld	[rIF], a	; interrupts to pre-snapshot state)
	pop	af
	ld	[rTIMA], a	; restore timer to pre-snapshot state
	pop	af	;get original AF values
	reti		; return and enable interrupts
