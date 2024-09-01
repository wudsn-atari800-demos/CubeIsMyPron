; >>> Cube is my Pron - 256-byte vector cube <<<
;
; (c) 2011-05-24 by JAC! of WUDSN
;
; Based on the awesome code by Skate / Plush. Man you rule!
; Related to: Rottoprojo64 Coder Pron
; http://www.pouet.net/prod.php?which=56981
;
; After fighting the RottoprojoXL down to 256 bytes, 
; I decided to approach the final frontier that has
; always been there in my mind for over 20 years:
;
; >> The mother of all objects in 256 bytes on an 8-bit machine from 1983 <<
;
; The result is a real-time rotated, projected, and
; drawn wire cube running at about 3-5 FPS. 
;
; The demo is available in low and high resolution.
; The screen is double buffered to avoid flickering.
; The rotation is toggling between 1 and 2 axis at a time.
; The speed of the rotation can be adjusted in 8 steps in the source.
; The colors are alternating every 8 seconds and become nicer over time.
;
; Load from MyPicoDos without BASIC and with Atari XL OS Revision 2.
; This version uses undocumented OS entry points which
; will be different in a different OS version.
; If you need it for a different OS, just contact me.
;
; Created using WUDSN IDE, visit https://www.wudsn.com for more.


;Constants
GRAPHICS	= 6		;Use 6 or 8

	.if GRAPHICS=6		;Low-res
PAGES		= $09
CUBE_SIZE	= $3f
OFFSET		= $2f
OFFSET_X	= $20
dllmshi		= $b77d
	.elif GRAPHICS=8	;High-res
PAGES		= $0f
CUBE_SIZE	= $3f
OFFSET		= $2f-4
OFFSET_X	= $70+4
dllmshi		= $a03b
	.endif

DISTANCE	= 137
POINTS		= 4
HIGH		= POINTS
SPEED		= $80	;2^n, n=0 to 7
TOGGLE		= $04

; Zeropage Addresses
cnt		= $14
vertexX		= $80	;8 bytes for 4x16-bit vertex coordinates
vertexY		= $88	;8 bytes for 4x16-bit vertex coordinates
vertexZ		= $90	;8 bytes for 4x16-bit vertex coordinates
object		= $98	;Object counter
rotatedVertex	= $99	;Low part of 16-bit vertex coordinate
storage1	= $ba	;Multiplication temporary avlue
storage2	= $bb	;Multiplication temporary value
draw_cnt	= $bc	;Line drawing counter
temp_x		= $bd	;Projection temporary value

rowcrs_tab	= $e0	;8 bytes of point rows
colcrs_tab	= $e8	;8 bytes of point columns

; OS Zeropage Address
rowcrs		= $54	;Row of cursor, 1 byte
colcrs		= $55	;Column of cursor, 2 bytes

; OS Illegal Entry Points (undocumented and version dependent)
; These are the values for the standard 800 XL REV 2 OS
os_gfx		= $ef9c
os_clear	= $f1a4
os_draw		= $f9c2

	opt l+
	org $2000

;
; Rotate Vertex
; -------------
; Description:
; Rotates a vertex on 1 or 2 axes.
; First rotation is on z-axis
; Second rotation is on y-axis (optional, depends on the state flag)
; IN: X
; OUT: X, unchanged
; DESTROYS: A,Y

	.macro m_rotate_vertex
	; Rotate on z-axis
	ldy vertexY,x
	lda vertexY+HIGH,x
	jsr rotate_single_axis
	sta vertexY,x
	sty vertexY+HIGH,x

	lda cnt-1		;Frame based axis toggle
	and #TOGGLE
	beq skip
	; Rotate on y-axis
	ldy vertexZ,x
	lda vertexZ+HIGH,x
	jsr rotate_single_axis
	sta vertexZ,x
	sty vertexZ+HIGH,x
skip				;Must not be in the ".endm" line, otherwise MADS loops forever
	.endm

;===============================================================

	.proc main

	lda #GRAPHICS
	jsr os_gfx		;Returns with A=0, X=6, Y=1

	ldx #CUBE_SIZE		;Warning! Loop counter equals to cube size
	txa
init_loop
	sta vertexX,x		;Set positive size values as default vertex
	dex
	bpl init_loop
	stx $4d			;Force attract mode for color changes

	lda #$ff-CUBE_SIZE	;Inverted cube size = $c0
	sta vertexZ+1+HIGH	;Vertex2 Z coordinate high-byte
	sta vertexY+3+HIGH	;Vertex3 Y coordinate high-byte
	sta vertexY+2+HIGH	;Vertex4 Y coordinate high-byte
	sta vertexZ+2+HIGH	;Vertex4 Z coordinate high-byte

	.proc vertex_loop
	dec object
	bpl no_clear

	.proc clear
	lda 89
	sta dllmshi		;DL LMS HI of $b7e0-bfff if BASIC is OFF
	eor #$20		;Toggle 8k as double buffering
	sta 89			;Start address for screen memory
	ora #PAGES
	sta $6a			;End address for screen memory (RAMTOP)
	lda #125
	jsr os_clear		;Sets color to <>0 and clear screens, return with A=125, X=6, Y=1

	.proc print
loop	ldy draw_cnt
	ldx index,y
	ldy colcrs_tab,x
	sty colcrs
	lda rowcrs_tab,x
;	sta rowcrs
	jsr os_draw+2		;First statement is LDA ROWCRS, returns with A=<color>, X=3, Y=1
 	dec draw_cnt
 	bpl loop
	.endp

;	lda cnt			;Frame counting
;	sta $600
;time = *-2
;	inc time
	.endp
no_clear
	lda #15
	sta draw_cnt
	lsr
	and object
	sta object
	cmp #4			;We don't need to rotate all 8 vertexes
	and #3
	tax
	bcs mirrors		;We can use mirroring, right?
	lda #SPEED
loop	pha
	m_rotate_vertex		;Does not change X
	pla
	lsr
	bne loop
;	lda #$00		;Do not invert
	.byte $2c		;Skip
mirrors
	lda #$ff		;Invert
positive
	pha			;X=0..3, A=$00/$ff
	pha
	eor vertexZ+HIGH,x
	adc #DISTANCE		;\
	lsr			; \
	lsr			;  \
	sta storage1		;   Magical Linear Projection
	lsr			;  /
	adc storage1		; /
	adc #$26		;/
	sta storage1

	pla
	eor vertexX+HIGH,x
	jsr calculate_coordinate
	adc #OFFSET_X
	sta temp_x

	pla
	eor vertexY+HIGH,x
	jsr calculate_coordinate

	ldx object
	sta rowcrs_tab,x
	lda temp_x
	sta colcrs_tab,x

	jmp vertex_loop
	.endp

; Calculate Coordinate
; --------------
; Description:
; Multiplies rotated coordinate with perspective
; value and adds a constant value to move the cube
; in the visible screen area. This routine is used
; for both X and Y coordinates.
;
	.proc calculate_coordinate
	php
	bpl *+4
	eor #$ff		;For negative values invert

; Multiply
; --------
; Description:
; Modified unsigned 8-bit multiplication routine.
; Only high byte of the 16-bit result is used.
; Returns with Y=0 and X unchanged

	sta storage2
	lda #8
    	ldy #9
loop	lsr
	ror storage2
	bcc not_set
	adc storage1
not_set	dey
	bne loop

	plp
	bpl *+4
	eor #$ff		;Invert back
	adc #OFFSET		;Add offset
	rts	
	.endp

; Rotate On Single Axis
; ---------------------
; Description:
; Rotates a vertex on a single axis
; to rotate on x-axis inputs should be y and z coordinates
; to rotate on y-axis inputs should be x and z coordinates
; to rotate on z-axis inputs should be x and y coordinates
;
; Input:
; vertexCoordinate1 : signed 16-bit coordinate (x,y or z)
; vertexCoordinate2 : signed 16-bit coordinate (x,y or z)
;
; Formula:
; vertexCoordinate1 -= vertexCoordinate2 / 256
; vertexCoordinate2 += vertexCoordinate1 / 256
; p.s: "/ 256" means getting the high byte
;
; Comment:
; Skate can rotate vertexes by addition and subtraction
; without needing any tables or lame trigonometric stuff. ;)
;

; IN: A=Value for rotatedVertex, Y = Value for rotatedVertex+1
; OUT: A=Value of rotatedVertex, Y = Value of rotatedVertex+1
	.proc rotate_single_axis
	sty rotatedVertex
	tay
	; vertexCoordinate1 -= vertexCoordinate2 / 256
;	clc			;Optional, you won't see the difference
	eor #$ff
	bmi skip1

	adc vertexX,x
	bcc skip2
	inc vertexX+HIGH,x
	bcs skip2

skip1	adc vertexX,x
	bcs skip2
	dec vertexX+HIGH,x

skip2	sta vertexX,x

	; vertexCoordinate2 += vertexCoordinate1 / 256
	lda vertexX+HIGH,x
	clc
	bpl skip3

	eor #$ff
	adc rotatedVertex
	bcc skip4
	dey
;	bcs skip4
	rts

skip3	adc rotatedVertex
	bcc skip4
	iny

skip4	rts
	.endp

index	.byte 0,1,7,6,5,4,2,3
	.byte 0,1,2,3,5,4,7,6
	.endp

	.print "PROC main:	", main, " - ", main+.len main-1, " (", .len main, " bytes)"
