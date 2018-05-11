// -- FLD (Flexible line distance) Example --
//
// Code: Jesder / 0xc64 / Hokuto Force
// Platform: C64
// Compiler: win2c64 (http://www.aartbik.com)
// About: Simple FLD effect to bounce a bitmap logo
//

// Raster debug ?
.const DEBUG = 0

BasicUpstart2(begin)        // <- This creates a basic sys line that can start your program

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
.const REG_ZERO_FE         = $fe                // Zero page cache
.const REG_ZERO_FD         = $fd                // Zero page cache

// constants
.const C_SCREEN_BANK       = $4000              // screen bank base address
.const C_SCREEN_RAM        = C_SCREEN_BANK + $0400 // screen RAM
.const C_COLOUR_RAM        = $d800              // colour ram
.const C_CHARSET           = C_SCREEN_BANK + $3800 // Alternate Character bank
.const C_CHARSET_HIGH      = C_SCREEN_BANK + $3900 // Arternate Character bank HI


// Koala resource
.var picture = LoadBinary("defeest-fullscreen.kla", BF_KOALA)
*=C_SCREEN_BANK + $C00 "ScreenRam";      .fill picture.getScreenRamSize(), picture.getScreenRam(i)
*=C_SCREEN_BANK + $1C00 "ColorRam:"; colorRam:  .fill picture.getColorRamSize(), picture.getColorRam(i)
*=C_SCREEN_BANK + $2000 "Bitmap";       .fill picture.getBitmapSize(), picture.getBitmap(i)

* = $2000 "Main Program"
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

	// Switch SID bank to $8000-$bfff
	lda #%00000010
	sta $DD00 // SPINDLE: For Spindle change to $DD02

  // Setup background sprites
  lda #$20                // Using block 32 for Sprite 0-6
  sta C_SCREEN_BANK + $7f8							  // offset $0400
  sta C_SCREEN_BANK + $ff8								// offset $0C00
  sta C_SCREEN_BANK + $7f9
  sta C_SCREEN_BANK + $ff9
  sta C_SCREEN_BANK + $7fa
  sta C_SCREEN_BANK + $ffa
  sta C_SCREEN_BANK + $7fb
  sta C_SCREEN_BANK + $ffb
  sta C_SCREEN_BANK + $7fc
  sta C_SCREEN_BANK + $ffc
  sta C_SCREEN_BANK + $7fd
  sta C_SCREEN_BANK + $ffd
  sta C_SCREEN_BANK + $7fe
  sta C_SCREEN_BANK + $ffe
  sta C_SCREEN_BANK + $7ff
  sta C_SCREEN_BANK + $fff

  ldx #0                  // Copy sprite into sprite memory
!loop:
  lda checkboard_sprite, x
  sta C_SCREEN_BANK + $800, x
  lda scroller_back_top, x
  sta C_SCREEN_BANK + $800 + 64, x
  inx
  cpx #63
  bne !loop-

  ldy #$0E                // Sprite colors to gray
  sty $D027
  sty $D028
  sty $D029
  sty $D02A
  sty $D02B
  sty $D02C
  sty $D02D
  sty $D02E

  ldy #$0B
  sty $D025

  ldy #$0C
  sty $D026

  ldy #80                 // Set Y position of sprite
  sty $D001               // Set y for sprite 0
  sty $D003               // Set y for sprite 1
  sty $D005               // Set y for sprite 2
  sty $D007               // Set y for sprite 3
  sty $D009               // Set y for sprite 4
  sty $D00B               // Set y for sprite 5
  sty $D00D               // Set y for sprite 6
  sty $D00F               // Set y for sprite 6

  ldx #0                  // Set X position of sprite 0
  stx $D000
  ldx #48                 // Set X position of sprite 1
  stx $D002
  ldx #96                 // Set X position of sprite 2
  stx $D004
  ldx #124                // Set X position of sprite 3
  stx $D006
  ldx #192                // Set X position of sprite 4
  stx $D008
  ldx #240                // Set X position of sprite 5
  stx $D00A
  ldx #%11000000
  stx $D010
  ldx #032                // Set X position of sprite 6
  stx $D00C
  ldx #080                // Set X position of sprite 6
  stx $D00E

  lda #%11111111
  sta $D015               // Enable sprites
  lda #$ff
  sta $D01B               // Sprites behind graphics sprites
  sta $D01D               // Double X
  sta $D017               // Double Y

  lda #$ff                // Enable sprite 0-6


  lda #$01                // enable raster interrupts
  sta REG_INTCONTROL
  cli

  // Wait for space for exit
main_loop:
  jmp *

  // Back to normal text screen
  sei
  lda #$00                // Disable raster interrupt
  sta $d01a
  cli

  lda #%00011011          // Back to text mode
  sta $D011

  lda #$00                // Disable sprites

  jmp $FFFC

  rts                     // Back to basic


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
//  lda #BLUE
  sta REG_BGCOLOUR
  ldx #0

  loop1:
  .for (var i=0; i<4; i++) {
    lda colorRam+i*$100,x
    sta C_COLOUR_RAM+i*$100,x
  }
  inx
  bne loop1

  lda #$ff                        // init video garbage
  sta C_SCREEN_BANK + $3fff                       // fill to highlight fld for debugging

  jsr setup_charset

// Logo Header and Footer-----------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
  ldx #40                         // Make sure the text color is set to white
  lda #$01
!loop:
  sta C_COLOUR_RAM, x
  sta $DA7F, x
  dex
  bne !loop-

  ldx #$00                        // Make sure the scroller is white
!loop:
  //sta C_COLOUR_RAM + $200, x
  sta C_COLOUR_RAM + $2e8, x
  inx
  bne !loop-

  lda #%00000001
  sta $0291

  ldx #00                         // Copy header text to screen
!loop:
  lda header_text, x
  beq !over+
  sta C_SCREEN_BANK + $400, x
  lda footer_text, x
  sta C_SCREEN_BANK + $680, x
  inx
  jmp !loop-
!over:

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
  cmp REG_RASTERLINE              // Busy wait for raster line to change
  beq !loop-
.if (DEBUG==1) {
  lda #$05
  sta $d020
}
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
  adc #$38                        // %00111000
  sta REG_SCREENCTL_1
  lda #%00111000                  // $2000-$3fff bitmap mem, charmem $2000, $0c00-$0fff screen mem
  sta REG_MEMSETUP
  lda #$d8                        // switch multi colour mode on
  sta REG_SCREENCTL_2

  lda #$20                        // Set sprites back to position 32
  sta C_SCREEN_BANK + $7f8
  sta C_SCREEN_BANK + $ff8
  sta C_SCREEN_BANK + $7f9
  sta C_SCREEN_BANK + $ff9
  sta C_SCREEN_BANK + $7fa
  sta C_SCREEN_BANK + $ffa
  sta C_SCREEN_BANK + $7fb
  sta C_SCREEN_BANK + $ffb
  sta C_SCREEN_BANK + $7fc
  sta C_SCREEN_BANK + $ffc
  sta C_SCREEN_BANK + $7fd
  sta C_SCREEN_BANK + $ffd
  sta C_SCREEN_BANK + $7fe
  sta C_SCREEN_BANK + $ffe
  sta C_SCREEN_BANK + $7ff
  sta C_SCREEN_BANK + $fff

.if (DEBUG==1) {
  lda #$00
  sta $d020
}
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
  ldy #122                        // Set Y position of sprite
  sty $D001                       // Set y for sprite 0
  sty $D003                       // Set y for sprite 1
  sty $D005                       // Set y for sprite 2
  sty $D007                       // Set y for sprite 3
  sty $D009                       // Set y for sprite 4
  sty $D00B                       // Set y for sprite 5
  sty $D00D                       // Set y for sprite 6
  sty $D00F                       // Set y for sprite 6

.if (DEBUG==1) {
  lda #$0A
  sta $d020
}
  jsr update_scroller             // Update the scroller state
.if (DEBUG==1) {
  lda #$00
  sta $d020
}

  jsr wait_space                  // Test if the space bar is pressed

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
.if (DEBUG==1) {
  lda #$04
  sta $d020
}
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
  lda #%00011110                  // $1800-1ffff char mem, $0400-07ff screen mem
  sta REG_MEMSETUP
  lda #200                        // multi colour mode off
  sta REG_SCREENCTL_2

  // Set sprites to the scroller background
  lda #$21                        // Set sprites back to position 33
  sta C_SCREEN_BANK + $7f8
  sta C_SCREEN_BANK + $ff8
  sta C_SCREEN_BANK + $7f9
  sta C_SCREEN_BANK + $ff9
  sta C_SCREEN_BANK + $7fa
  sta C_SCREEN_BANK + $ffa
  sta C_SCREEN_BANK + $7fb
  sta C_SCREEN_BANK + $ffb
  sta C_SCREEN_BANK + $7fc
  sta C_SCREEN_BANK + $ffc
  sta C_SCREEN_BANK + $7fd
  sta C_SCREEN_BANK + $ffd
  sta C_SCREEN_BANK + $7fe
  sta C_SCREEN_BANK + $ffe
  sta C_SCREEN_BANK + $7ff
  sta C_SCREEN_BANK + $fff

  // Enable multicolor mode sprite 0-7
  ldy #$ff
  sty $D01C

  ldy #$0F                        // Sprite colors to gray
  sty $D027
  sty $D028
  sty $D029
  sty $D02A
  sty $D02B
  sty $D02C
  sty $D02D
  sty $D02E

  // Set scroller background
  ldy #210                        // Set Y position of sprite
  sty $D001                       // Set y for sprite 0
  sty $D003                       // Set y for sprite 1
  sty $D005                       // Set y for sprite 2
  sty $D007                       // Set y for sprite 3
  sty $D009                       // Set y for sprite 4
  sty $D00B                       // Set y for sprite 5
  sty $D00D                       // Set y for sprite 6
  sty $D00F                       // Set y for sprite 7

.if (DEBUG==1) {
  ldy #$00
  sty $d020
}
  jmp hook_apply_hw_scroll

// apply hardware scroll -------------------------------------------------------------------------------------------]
// -----------------------------------------------------------------------------------------------------------------]
hook_apply_hw_scroll:
  lda #202
  ldx #<apply_hardware_scroll
  ldy #>apply_hardware_scroll
  jmp apply_interrupt

apply_hardware_scroll:
  lda #$c0                   // 38 column
scroller_amount:
  ora #007                   // + hardware scroll Self modifying
  ldy #202                   // wait first scroller scanline
wait_scroller_start:
  cpy REG_RASTERLINE
  bne wait_scroller_start
  sta REG_SCREENCTL_2        // apply hardware scroll value
  jmp hook_reset_hw_scroll

// reset hardware scroll -------------------------------------------------------------------------------------------]
// -----------------------------------------------------------------------------------------------------------------]
hook_reset_hw_scroll:
  lda #244
  ldx #<reset_hardware_scroll
  ldy #>reset_hardware_scroll
  jmp apply_interrupt

reset_hardware_scroll:
  lda #$c8                   // 40 column mode + no scroll
  ldy #250                   // wait for final scanline done
wait_scroller_end:
  cpy REG_RASTERLINE
  bne wait_scroller_end
  sta REG_SCREENCTL_2        // reset scroll & column mode

  jmp hook_update_logo_fld



// update fld effect ---------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]

hook_update_logo_fld:
  lda #254
  ldx #<update_logo_fld
  ldy #>update_logo_fld
  jmp apply_interrupt


update_logo_fld:
  inc REG_INTFLAG

.if (DEBUG==1) {
  lda #$01
  sta $d020
}

  jsr update_x_sprite_anim                // Update X sprite position, the jumps are to far, so subroutine

  dec logo_bounce_delay                   // smooth logo bounce effect
  bne update_logo_fld_done
  lda #002                                // The bounce speed devider
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
  sbc render_bitmap_start + 1             // 16 - start-lines > end lines to shift.
  sta render_bitmap_end + 1

update_logo_fld_done:
  // Disable multicolor mode Sprite 0-7
  ldy #$00
  sty $D01C


  // Set logo background Sprites back to top
  ldy #80                         // Set Y position of sprite
  sty $D001                       // Set y for sprite 0
  sty $D003                       // Set y for sprite 1
  sty $D005                       // Set y for sprite 2
  sty $D007                       // Set y for sprite 3
  sty $D009                       // Set y for sprite 4
  sty $D00B                       // Set y for sprite 5
  sty $D00D                       // Set y for sprite 6
  sty $D00F                       // Set y for sprite 7

  ldy #$0E                        // Sprite colors to blue
  sty $D027
  sty $D028
  sty $D029
  sty $D02A
  sty $D02B
  sty $D02C
  sty $D02D
  sty $D02E


.if (DEBUG==1) {
  lda #$00
  sta $d020
}
  jmp hook_init_frame_fld


// Update the X sprite animation----------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
update_x_sprite_anim:
  // Count back from 0 to 48 in loop
  ldx back_run_pos                        // Get animation position
  inx
  txa
  cmp #48                                 // loop at 47 range is 0-47
  bne !over+
  ldx #00                                 // Reset
!over:
  stx back_run_pos                        // Store animation position
  lda x_sprite_cos_pos, x
  tax


  // The last sprite always has bit 8 set
  lda #%10000000
  sta $D010

  // Set the initial seed number to the sprite register
  txa
  clc
  sbc #28                                 // The sprite should move from (-28 to 20)
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
  sta temp_run_pos                        // Else
  clc
  sbc #38
  bcs !cc+
  lda temp_run_pos
  sta $D000                               // Sprite 0 -20 - 28
!cc:
  lda temp_run_pos
  clc
  adc #48                                 // 48 pixels futher
  sta $D002                               // Sprite 1 28 - 56
  clc
  adc #48                                 // again
  sta $D004                               // Sprite 2 56 - 104
  clc
  adc #48                                 // again
  sta $D006                               // Sprite 3 104 - 152
  clc
  adc #48                                 // again
  sta $D008                               // Sprite 4 152 - 200
  clc
  adc #48                                 // again
  sta $D00A                               // Sprite 5 200 - 248
  clc
  adc #48                                 // again
  bcc !cc+
  pha
  lda $d010
  ora #%01000000
  sta $d010
  pla
!cc:
  sta $D00C                               // sprite 6 248 - 296
  clc
  adc #48                                 // again
  sta $D00E                               // sprite 7 296 - 344

  rts

// Setup alternative Character set -------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
setup_charset:
  ldx #$00                                // relocate character set data
!loop:
  lda font_data, x
  sta C_CHARSET, x
  lda font_data + $100, x
  sta C_CHARSET + $100, x
  lda font_data + $180, x
  sta C_CHARSET + $180, x
  inx
  bne !loop-

  rts


// update & render scroller ----------------------------------------------------------------------------------------]
// -----------------------------------------------------------------------------------------------------------------]

update_scroller:
  ldx scroller_amount + 1
  dex                             // advance hardware scroll
  dex
  dex
  bmi shift_scroller_data         // detect if time to shift screen ram on the scroller
  stx scroller_amount + 1         // not time to advance scroller, so just update the hardware scroll value
  rts                             // This frame no update is needed

shift_scroller_data:
  stx REG_ZERO_FE                 // cache scroll amount for later use

  ldy #000                        // shift screen ram to the left
scroller_shift_loop:
  lda C_SCREEN_RAM + $2f9, y      // shift all 4 rows
  sta C_SCREEN_RAM + $2f8, y
  lda C_SCREEN_RAM + $321, y
  sta C_SCREEN_RAM + $320, y
  lda C_SCREEN_RAM + $349, y
  sta C_SCREEN_RAM + $348, y
  lda C_SCREEN_RAM + $371, y
  sta C_SCREEN_RAM + $370, y
  iny
  cpy #039
  bne scroller_shift_loop

  ldx scroller_char_step + 1      // grab step into rendering current scroller character
  bpl render_next_scroll_colm     // detect if we need to render a new character, or are still rendering current character (each letter is 4 chars wide)

scroller_message_index:
  ldx #000                        // time to render a new character, so set up some which character to render and the bit mask
read_next_scroller_char:
  lda scroller_message, x         // grab next character to render
  bpl advance_scroller_index      // detect end of message control character
  lda scroller_message
  ldx #001                        // reset index - set to 1 since this update will use first char in message
  ldy #>scroller_message          // reset scroller message read source
  sty read_next_scroller_char + 2
  jmp save_scroller_index

advance_scroller_index:
  inx                             // advance scroller message index
  bne save_scroller_index         // detect if reached 256 offset
  inc read_next_scroller_char + 2 // advance high byte for reading message
save_scroller_index:
  stx scroller_message_index + 1

  ldy #>C_CHARSET                 // determine if character is in the low/high section of the charset
  cmp #031
	bcc calc_scrollchar_src_low
	ldy #>C_CHARSET_HIGH

calc_scrollchar_src_low:
  and #031                        // calculate offset into char set for character bytes
  asl
  asl
  asl

  sty render_scroller_column + 2  // store character high/low pointers for rendering
  sty render_scroller_column2 + 2
  sta render_scroller_column + 1
  sta render_scroller_column2 + 1

  lda #192                        // reset the scroller character mask
  sta scroller_character_mask + 1
  lda #003                        // reset step into new character mask
  sta scroller_char_step + 1

render_next_scroll_colm:
  clc
  lda REG_ZERO_FE                 // reset the hardware scroll value
  adc #008
  tax
  stx scroller_amount + 1         // save hardware scroll index

  ldx #000                        // init character byte loop counter
  stx REG_ZERO_FD                 // reset screen rendering offset and cache on zero page
render_scroller_column:
  lda C_CHARSET, x                // load byte from character ram
scroller_character_mask:
  and #192                        // apply current mask
scroller_char_step:
  ldy #255
  beq skip_shift_1                // dont shift if we are already masking bits 0 and 1
shift_scroll_mask_loop1:
  lsr                             // shift down until bits 0 and 1 are occupied
  lsr
  dey
  bne shift_scroll_mask_loop1
skip_shift_1:
  asl                             // multiply by 4 as a look up into our character matrix
  asl
  sta REG_ZERO_FE                 // cache on zero page to recall shortly

  inx                             // advance to next byte in character ram
render_scroller_column2:
  lda C_CHARSET, x
  and scroller_character_mask + 1 // apply current mask
  ldy scroller_char_step + 1
  beq skip_shift_2                // dont shift if we are already masking bits 0 and 1
shift_scroll_mask_loop2:
  lsr                             // shift down until bits 0 and 1 are occupied
  lsr
  dey
  bne shift_scroll_mask_loop2

skip_shift_2:
  clc                             // calculate characater code to use for this 2x2 block
  adc REG_ZERO_FE                 // grab offset calculated earlier
  adc #064                        // add offset to the character matrix

scroller_render_offset:
  ldy REG_ZERO_FD
  sta C_SCREEN_RAM + $31f, y      // render character to screen
  tya
  adc #040                        // advance rendering offset for next pass of the loop
  sta REG_ZERO_FD

  inx                             // advance to next byte in character ram
  cpx #008                        // detect if entire column now rendered
  bne render_scroller_column

  dec scroller_char_step + 1      // advance scroller character step
  lda scroller_character_mask + 1 // advance scroller character mask for next update
  lsr
  lsr
  sta scroller_character_mask + 1


  rts

// Wait for space ------------------------------------------------------------------------------]
// ---------------------------------------------------------------------------------------------]
wait_space:
  ldx #$7F                        //%01111111, Detect if space is pressed
  stx $DC00
  lda $DC01
  and #$10                        //mask %00010000
  bne !over+
  lda $0C                         // Illegal opcode NOP $FFFF, replace the jump command with a nop, this will end the loop
  sta main_loop
!over:
  rts

// Variables -----------------------------------------------------------------------------------]
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
  .text "=-=-=-=-=> defeest logo below <=-=-=-=-="
  .byte 0
footer_text:
  .text "=-=-=-=-=> defeest logo above <=-=-=-=-="
  .byte 0
scroller_message:
  .text "floating flamingo cup holders, game of beer pong, champagne bottle pool float, a giant f#@%ing tote, a schnoodle, waterproof phone case. "
  .text "greetings to revspace, frack, lag, hack42, bitlair, nurdspace, tkkrlab and ijhack. "
  .text "also geraffel, mononoke, websmurf and last but not least je moeder. "
  .text "this demo was made possible by reichsoverheid.nl and is powered by hoe.re. "
  .text "did you know techinc has cameras in the building? "
  .text "daar in dat kleine cafe op de 5e daar zijn de mensen gelijk en tervree etv! ..... "
  .byte $ff   // EOF

// Data
checkboard_sprite:
// Arrows
.byte 134,24,97
.byte 195,12,48
.byte 97,134,24
.byte 48,195,12
.byte 24,97,134
.byte 12,48,195
.byte 134,24,97
.byte 195,12,48
.byte 97,134,24
.byte 48,195,12
.byte 154,105,166
.byte 48,195,12
.byte 97,134,24
.byte 195,12,48
.byte 134,24,97
.byte 12,48,195
.byte 24,97,134
.byte 48,195,12
.byte 97,134,24
.byte 195,12,48
.byte 134,24,97

// Top scroller background
scroller_back_top:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$50,$1F,$41,$F4,$5F,$55,$F5,$8F



// Cosine to animate sprite background
x_sprite_cos_pos:
.var i=0
.var len=48
bounce:
.while (i++<len) {
  .var x = round(24-(12*cos((i*(PI*2))/len))+(12*cos((i*(PI))/len)))
  .print x
  .byte x
}

// Font data alternative font
font_data:
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $7c, $c6, $de, $c6, $c6, $c6, $c6, $00
  .byte $fc, $c6, $dc, $c6, $c6, $c6, $fc, $00, $7c, $c6, $c0, $c0, $c0, $c6, $7c, $00
  .byte $fc, $c6, $c6, $c6, $c6, $c6, $fc, $00, $7c, $c6, $f8, $c0, $c0, $c6, $7c, $00
  .byte $7c, $c6, $f8, $c0, $c0, $c0, $c0, $c0, $7c, $c0, $ce, $c6, $c6, $c6, $7c, $00
  .byte $c6, $c6, $de, $c6, $c6, $c6, $c6, $c0, $18, $18, $18, $18, $18, $18, $18, $00
  .byte $06, $06, $06, $06, $06, $06, $c6, $7c, $c6, $c6, $dc, $c6, $c6, $c6, $c6, $00
  .byte $c0, $c0, $c0, $c0, $c0, $c6, $7c, $00, $6c, $fe, $c6, $c6, $c6, $c6, $c6, $00
  .byte $7c, $c6, $c6, $c6, $c6, $c6, $c6, $00, $7c, $c6, $c6, $c6, $c6, $c6, $7c, $00
  .byte $fc, $c6, $dc, $c0, $c0, $c0, $c0, $c0, $7c, $c6, $c6, $c6, $c6, $c6, $7c, $06
  .byte $fc, $c6, $dc, $c6, $c6, $c6, $c6, $00, $7c, $c0, $fc, $06, $06, $c6, $7c, $00
  .byte $7e, $18, $18, $18, $18, $18, $18, $00, $c6, $c6, $c6, $c6, $c6, $c6, $7c, $00
  .byte $c6, $c6, $c6, $c6, $c6, $6c, $38, $00, $c6, $c6, $c6, $c6, $c6, $fe, $6c, $00
  .byte $c6, $c6, $7c, $1c, $c6, $c6, $c6, $00, $c6, $c6, $7c, $38, $38, $38, $38, $00
  .byte $fe, $0c, $18, $30, $60, $c0, $fe, $00, $3c, $30, $30, $30, $30, $30, $3c, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $3c, $0c, $0c, $0c, $0c, $0c, $3c, $00
  .byte $00, $18, $3c, $7e, $18, $18, $18, $18, $00, $10, $30, $7f, $7f, $30, $10, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $00
  .byte $66, $66, $66, $00, $00, $00, $00, $00, $66, $66, $ff, $66, $ff, $66, $66, $00
  .byte $18, $3e, $60, $3c, $06, $7c, $18, $00, $62, $66, $0c, $18, $30, $66, $46, $00
  .byte $3c, $66, $3c, $38, $67, $66, $3f, $00, $06, $0c, $18, $00, $00, $00, $00, $00
  .byte $0c, $18, $30, $30, $30, $18, $0c, $00, $30, $18, $0c, $0c, $0c, $18, $30, $00
  .byte $00, $66, $3c, $18, $3c, $66, $00, $00, $00, $18, $18, $7e, $18, $18, $00, $00
  .byte $00, $00, $00, $00, $00, $18, $18, $30, $00, $00, $00, $7e, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $30, $30, $00, $00, $03, $06, $0c, $18, $30, $60, $00
  .byte $7c, $c6, $c6, $c6, $c6, $c6, $7c, $00, $38, $18, $18, $18, $18, $18, $18, $00
  .byte $7c, $06, $7c, $c0, $c0, $c6, $7c, $00, $7c, $c6, $3c, $06, $06, $c6, $7c, $00
  .byte $c6, $c6, $7e, $06, $06, $06, $06, $00, $fe, $c0, $fc, $06, $06, $c6, $7c, $00
  .byte $7c, $c0, $dc, $c6, $c6, $c6, $7c, $00, $7e, $c6, $06, $06, $06, $06, $06, $00
  .byte $7c, $c6, $7c, $c6, $c6, $c6, $7c, $00, $7e, $c6, $76, $06, $06, $06, $06, $00
  .byte $00, $30, $30, $00, $30, $30, $00, $00, $00, $00, $18, $00, $00, $18, $18, $30
  .byte $0e, $18, $30, $60, $30, $18, $0e, $00, $00, $00, $3c, $78, $00, $3c, $78, $00
  .byte $70, $18, $0c, $06, $0c, $18, $70, $00, $3c, $66, $06, $0c, $18, $00, $18, $00

  //; scroller characters (16 chars)
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0e, $0e, $0e, $00
  .byte $00, $00, $00, $00, $e0, $e0, $e0, $00, $00, $00, $00, $00, $ee, $ee, $ee, $00
  .byte $0e, $0e, $0e, $00, $00, $00, $00, $00, $0e, $0e, $0e, $00, $0e, $0e, $0e, $00
  .byte $0e, $0e, $0e, $00, $e0, $e0, $e0, $00, $0e, $0e, $0e, $00, $ee, $ee, $ee, $00
  .byte $e0, $e0, $e0, $00, $00, $00, $00, $00, $e0, $e0, $e0, $00, $0e, $0e, $0e, $00
  .byte $e0, $e0, $e0, $00, $e0, $e0, $e0, $00, $e0, $e0, $e0, $00, $ee, $ee, $ee, $00
  .byte $ee, $ee, $ee, $00, $00, $00, $00, $00, $ee, $ee, $ee, $00, $0e, $0e, $0e, $00
  .byte $ee, $ee, $ee, $00, $e0, $e0, $e0, $00, $ee, $ee, $ee, $00, $ee, $ee, $ee, $00

