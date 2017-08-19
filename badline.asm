// -- FLD (Flexible line distance) Example --
//
// Code: Jesder / 0xc64 / Hokuto Force
// Platform: C64
// Compiler: win2c64 (http://www.aartbik.com)
// About: Simple FLD effect to bounce a bitmap logo
//

BasicUpstart2(begin)				// <- This creates a basic sys line that can start your program

// common register definitions
.const REG_INTSERVICE_LOW  = $0314              // interrupt service routine low byte
.const REG_INTSERVICE_HIGH = $0315              // interrupt service routine high byte
.const REG_SCREENCTL_1     = $d011              // screen control register #1
.const REG_RASTERLINE      = $d012              // raster line position
.const REG_SCREENCTL_2     = $d016              // screen control register #2
.const REG_MEMSETUP        = $d018              // memory setup register
.const REG_INTFLAG         = $d019              // interrupt flag register
.const REG_INTCONTROL      = $d01a              // interrupt control register
.const REG_BORCOLOUR       = $d020              // border colour register
.const REG_BGCOLOUR        = $d021              // background colour register
.const REG_INTSTATUS_1     = $dc0d              // interrupt control and status register #1
.const REG_INTSTATUS_2     = $dd0d              // interrupt control and status register #2

// constants
.const C_SCREEN_RAM = $0400              // screen RAM
.const C_COLOUR_RAM = $d800              // colour ram

// Koala resource
.var picture = LoadBinary("defeest-fullscreen.kla", BF_KOALA)
*=$0C00	"ScreenRam"; 			.fill picture.getScreenRamSize(), picture.getScreenRam(i)
*=$1C00	"ColorRam:"; colorRam: 	.fill picture.getColorRamSize(), picture.getColorRam(i)
*=$2000	"Bitmap";				.fill picture.getBitmapSize(), picture.getBitmap(i)

* = $1000 "Main Program"    
// Let the code begin
begin:
	// create initial interrupt
	sei                     // set up interrupt
	lda #$7f
	sta REG_INTSTATUS_1     // turn off the CIA interrupts
	sta REG_INTSTATUS_2
	and REG_SCREENCTL_1     // clear high bit of raster line
	sta REG_SCREENCTL_1

	ldy #000
	sty REG_RASTERLINE

	lda #<sync_intro        // load interrupt address
	ldx #>sync_intro
	sta REG_INTSERVICE_LOW
	stx REG_INTSERVICE_HIGH

	// Initial screen memory setup
	lda #%00010110								  // $1800-1ffff char mem, $0400-07ff screen mem
	sta REG_MEMSETUP

	// Setup background sprites
	lda #$20				// Using block 32 for Sprite 0-6
	sta $7f8
	sta $ff8
	sta $7f9
	sta $ff9
	sta $7fa
	sta $ffa
	sta $7fb
	sta $ffb
	sta $7fc
	sta $ffc
	sta $7fd
	sta $ffd
	sta $7fe
	sta $ffe
	sta $7ff
	sta $fff

	ldx #0					// Copy sprite into sprite memory
!loop:
	lda checkboard_sprite, x
	sta $0800, x
	inx 
	cpx #63
	bne !loop-

	ldy #11					// Sprite colors to gray
	sty $D027
	sty $D028
	sty $D029
	sty $D02A
	sty $D02B
	sty $D02C
	sty $D02D
	sty $D02E

	ldy #80					// Set Y position of sprite
	sty $D001				// Set y for sprite 0
	sty $D003				// Set y for sprite 1
	sty $D005				// Set y for sprite 2
	sty $D007				// Set y for sprite 3
	sty $D009				// Set y for sprite 4
	sty $D00B				// Set y for sprite 5
	sty $D00D				// Set y for sprite 6
	sty $D00F				// Set y for sprite 6
	
	ldx #0					// Set X position of sprite 0
	stx $D000
	ldx #48					// Set X position of sprite 1
	stx $D002			
	ldx #96				// Set X position of sprite 2
	stx $D004
	ldx #124				// Set X position of sprite 3
	stx $D006
	ldx #192				// Set X position of sprite 4
	stx $D008
	ldx #240				// Set X position of sprite 5
	stx $D00A
	ldx #%11000000
	stx $D010
	ldx #032				// Set X position of sprite 6
	stx $D00C
	ldx #080				// Set X position of sprite 6
	stx $D00E

	lda #%11111111
	sta $D015				// Enable sprites
	lda #$ff
	sta $D01B			  // Sprites behind graphics sprites
	sta $D01D				// Double X
	sta $D017				// Double Y

	lda #$ff	// Enable sprite 0-6


	lda #$01                // enable raster interrupts
	sta REG_INTCONTROL
	cli

	// forever loop
	jmp *


// helper routines -----------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
apply_interrupt:
	sta REG_RASTERLINE              // apply next interrupt
	stx REG_INTSERVICE_LOW
	sty REG_INTSERVICE_HIGH
	jmp $ea81


// intro sync ----------------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
sync_intro:
	inc REG_INTFLAG                 // acknowledge interrupt
	ldx #000                        // relocate bitmap colour/screen data

// Load Koala image-----------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

	lda #BLACK
	sta REG_BORCOLOUR
	lda #picture.getBackgroundColor()
//	lda #BLUE
	sta REG_BGCOLOUR
	ldx #0

	loop1:		
	.for (var i=0; i<4; i++) {
		lda colorRam+i*$100,x
		sta $d800+i*$100,x
	}
	inx
	bne loop1

	lda #$ff                        // init video garbage
	sta $3fff                       // fill to highlight fld for debugging


// Logo Header and Footer-----------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
	ldx #40													// Make sure the text color is set to white
	lda #$01
!loop:
	sta $D800, x
	sta $DA7F, x
	dex
	bne !loop-

	lda #%00000001
	sta $0291

	ldx #00													// Copy header text to screen
!loop:
	lda header_text, x	
	beq !over+	
	sta $0400, x	
	lda footer_text, x
	sta $680, x
	inx
	jmp !loop-
!over:

//	sta $D800
//	lda #001                        // test characters
//	sta $400                        // used to verify character positions before / after fld
//	sta $770
//	sta $680

	jmp hook_init_frame_fld


// init frame fld state ------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_init_frame_fld:
	lda #015
	ldx #<init_frame_fld
	ldy #>init_frame_fld
	jmp apply_interrupt


init_frame_fld:
	inc REG_INTFLAG

	lda #$1b                        // restore register to default
	sta REG_SCREENCTL_1

	jmp hook_bitmap_start


// begin rendering bitmap ----------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_bitmap_start:
	lda #057
	ldx #<render_bitmap_start
	ldy #>render_bitmap_start
	jmp apply_interrupt


render_bitmap_start:
	ldx #000                        // apply fld effect to top of logo (Self modifying)
	beq bitmap_top_fld_done         // no fld? then skip past this
wait_bitmap_top_fld:
	lda REG_RASTERLINE
!loop:
	cmp REG_RASTERLINE							// Busy wait for raster line to change
	beq !loop-
	lda REG_SCREENCTL_1
	adc #001                        // delay next bad scan line
	and #007
	ora #$18
	sta REG_SCREENCTL_1
	dex
	bne wait_bitmap_top_fld

bitmap_top_fld_done:
	ldx #012                        // wait for raster to get into position for bitmap
!loop:
	dex
	bne !loop-

	inc REG_INTFLAG                 // acknowledge interrupt

	clc
	lda REG_SCREENCTL_1             // switch bitmap mode on
	and #007
	adc #$38												// %00111000
	sta REG_SCREENCTL_1
	lda #%00111000									// $2000-$3fff char mem, $0c00-$0fff screen mem
	sta REG_MEMSETUP
	lda #$d8                        // switch multi colour mode on
	sta REG_SCREENCTL_2

	jmp hook_sprite_multi

// multiplex sprites half way ------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_sprite_multi:
	lda #115
	ldx #<sprite_multi	
	ldy #>sprite_multi
	jmp apply_interrupt

sprite_multi:
	inc REG_INTFLAG                 // acknowledge interrupt
	ldy #122				// Set Y position of sprite
  sty $D001				// Set y for sprite 0
  sty $D003				// Set y for sprite 1
  sty $D005				// Set y for sprite 2
  sty $D007				// Set y for sprite 3
  sty $D009				// Set y for sprite 4
  sty $D00B				// Set y for sprite 5
  sty $D00D				// Set y for sprite 6
  sty $D00F				// Set y for sprite 6
	jmp hook_bitmap_end

// complete rendering bitmap -------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_bitmap_end:
	lda #121
	ldx #<render_bitmap_end
	ldy #>render_bitmap_end
	jmp apply_interrupt


render_bitmap_end:
	ldx #015
wait_bitmap_bot_fld:
	lda REG_RASTERLINE
	cmp REG_RASTERLINE
	beq wait_bitmap_bot_fld + 3
	lda REG_SCREENCTL_1
	adc #001
	and #007
	ora #056
	sta REG_SCREENCTL_1
	dex
	bpl wait_bitmap_bot_fld

	ldx #008
latch_final_bitmap_line:
	dex
	bne latch_final_bitmap_line

	inc REG_INTFLAG                 // acknowledge interrupt

	clc
	lda REG_SCREENCTL_1             // bitmap off
	and #007                        // maintain current vertical scroll bits
	adc #024
	sta REG_SCREENCTL_1
	lda #%00010110								  // $1800-1ffff char mem, $0400-07ff screen mem
	sta REG_MEMSETUP
	lda #200                        // multi colour mode off
	sta REG_SCREENCTL_2

	// Sprites back to top
	ldy #80				// Set Y position of sprite
	sty $D001				// Set y for sprite 0
	sty $D003				// Set y for sprite 1
	sty $D005				// Set y for sprite 2
	sty $D007				// Set y for sprite 3
	sty $D009				// Set y for sprite 4
	sty $D00B				// Set y for sprite 5
	sty $D00D				// Set y for sprite 6
	sty $D00F				// Set y for sprite 6

	jmp hook_update_logo_fld


// update fld effect ---------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_update_logo_fld:
	lda #250
	ldx #<update_logo_fld
	ldy #>update_logo_fld
	jmp apply_interrupt


update_logo_fld:
	inc REG_INTFLAG

	dec logo_bounce_delay                   // smooth logo bounce effect
	bne update_logo_fld_done
	lda #002																// The bounce speed devider
	sta logo_bounce_delay

	ldx logo_bounce_index                   // advance bounce height index
	inx
	txa
	and #015                                // loop bounce index to start
	sta logo_bounce_index

	clc
	tax
	lda logo_bounce_heights, x              // grab next height
	tay
	adc #177                                // adjust bitmap ending interrupt
	sta hook_bitmap_end + 1
	sty render_bitmap_start + 1             // set number of fld lines before the bitmap
	clc
	lda #016                                // set number of fld lines after the bitmap
	sbc render_bitmap_start + 1							// 16 - start-lines > end lines to shift.
	sta render_bitmap_end + 1

	jsr update_x_sprite_anim								// Update X sprite position

update_logo_fld_done:
	jmp hook_init_frame_fld

// Update the X sprite animation----------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
update_x_sprite_anim:
	// Count back from 0 to 48 in loop
	ldx back_run_pos												// Get animation position
	inx 
	txa
	cmp #48																	// loop at 47 range is 0-47
	bne !over+
	ldx #00																	// Reset					
!over:
	stx back_run_pos												// Store animation position

//	lda #48																	// Invert counter
//	sbc back_run_pos
//	tax

	// The last sprite always has bit 8 set
	lda #%10000000
	sta $D010

	// Set the initial seed number to the sprite register
	txa
	clc
	sbc #28												// The sprite should move from (-28 to 20)
	bcs !cs+
	clc
	sta temp_run_pos
	sbc #7
	sta $D000
	lda $d010
	ora #%00000001
	sta $d010
	lda temp_run_pos
!cs:
	sta temp_run_pos							// Else
	clc
	sbc #38
	bcs !cc+
	lda temp_run_pos
	sta $D000											// Sprite 0 -20 - 28
!cc:
	lda temp_run_pos	
	clc
	adc #47					// 48 pixels futher
	sta $D002				// Sprite 1	28 - 56
	clc
	adc #47					// again 
	sta $D004				// Sprite 2 56 - 104
	clc
	adc #47					// again
	sta $D006				// Sprite 3 104 - 152
	clc
	adc #47					// again
	sta $D008				// Sprite 4 152 - 200
	clc
	adc #47					// again
	sta $D00A				// Sprite 5	200 - 248
	clc
	adc #47					// again
	bcc !cc+
	pha
	lda $d010
	ora #%01000000	
	sta $d010
	pla
!cc:
	sta $D00C				// sprite 6 248 - 296
	clc
	adc #47					// again
	sta $D00E				// sprite 7	296 - 344
	
	rts							// Return


// variables -----------------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

logo_bounce_heights:
	.byte 000, 000, 001, 001, 003, 005, 008, 013, 015, 012, 009, 006, 003, 001, 001, 000
logo_bounce_index:
	.byte 000
logo_bounce_delay:
	.byte 002
back_run_pos:
	.byte 000
temp_run_pos:
	.byte 000
header_text:
	.text "=-=-=-=-=> deFEEST logo below <=-=-=-=-="
	.byte 0
footer_text:
	.text "=-=-=-=-=> deFEEST logo above <=-=-=-=-="
	.byte 0

// Data
checkboard_sprite:
.byte 255,255,255
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 255,255,255
.byte 255,255,255
.byte 255,255,255
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 128,24,1
.byte 255,255,255
