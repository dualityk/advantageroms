
; NORTH STAR ADVANTAGE
; ##############################################
; 4K Boot ROM (revision BK)
;
; Disassembly and commentary by ian Butler, Aug 2023, no revisions

	org	0x8000

; Bunch of nice equ's in here to make this easier to read.
include "ports.asm"

; Video stuff where this code puts it.
vdd:			equ 0x00f0	; Video driver data block
cursormask:		equ 0x02f0	; Cursor data

; Variables common with the 2K ROM.
hex_input2:		equ 0x00fc	; Typed-in hex values land here
hex_input1:		equ 0x00fe
sio_absent:		equ 0x02fd	; SIO absent from slot 3
dispbuffer:		equ 0x02fe	; 16 bit value to show with hex16
					; (address for current line in
					; monitor display/edit routine)
keybuffer:		equ 0x03ff	; Last key pressed, for repetition

stack_top:		equ 0x0200	; One of the places the stack goes;
					; this is in VRAM.

; Variables used by the 4K ROM.
post_parity:		equ 0xc000
	; POST sets two bits here:
	; Bit 6: clear if main RAM parity error
	; Bit 5: clear if video RAM parity error
	; Additionally, auto floppy boot errors are saved in the lower
	; nibble if present.

post_status:		equ 0xc001
	; POST uses all the bits here:
	; Bits 4-7: bitmap of bad main RAM banks
	; Bit 3: parity error or incomplete RAM test
	; Bit 2: Timeout waiting for network status bit 5 to cycle
	; Bit 1: Timeout waiting for 8035 or bad response
	; Bit 0: Video RAM OK

fd_error:		equ 0xc432
	; The auto floppy boot sets error bits here:
	; Bit 4: Can't find track 0
	; Bit 3: Can't find sector
	; Bit 2: Bad sector ID
	; Bit 1: Disk not bootable (deviates from manual and also from
        ;        old boot code: boot address must be D0-F8 here)
	; Bit 0: Bad CRC

fd_testval:		equ 0x03fc	; Test byte for disk write diagnostic

current_bank:		equ 0xc002	; Currently selected RAM bank
enumtable_bottom:	equ 0xc003	; Card enumeration jump table
selected_slot:		equ 0xc011	; Currently selected slot
enum_taskptrs:		equ 0xc012	; Pointers to task contexts
current_SP_ptr:		equ 0xc020	; Currently selected slot function
net_buffer:		equ 0xfc00	; Start of network receive buffer


; This first part does some quick checks and then decides whether it wants
; to run the POST and fancier boot routines (as it does on cold start) or
; just run the older routine that waits for input.

cold_start:
	ld sp,0x0002		;8000 POST runs if SP is null (cold start)
init:
	push af			;8003
	ld iy,0x0000		;8004
	add iy,sp		;8008
	ld a,bootprom		;800a Map ROM to 8000 and jump to it
	out (set_bank2),a	;800c
	jp .initlanding		;800e
.initlanding:
	di			;8011 Interrupts are for squares
io_reset:
	ld a,0x28		;8012 Reset I/O processor and boards
	out (ioctl),a		;8014
	xor a			;8016 Think about that for a second
.io_reset_delay:
	ex (sp),hl		;8017
	djnz .io_reset_delay	;8018
	dec a			;801a
	jr nz,.io_reset_delay	;801b
	ld bc,0x0fa0		;801d Delay counter
	ld a,0x3f		;8020 Stop resetting, blank display and set
	out (ioctl),a		;8022 caps lock
check_nmi:
	in a,(iostatus1)	;8024
	and is1_nmi		;8026
	jr nz,io_reset		;8028 Reset harder if NMI is still on
	dec bc			;802a
	ld a,b			;802b
	or c			;802c
	jr nz,check_nmi		;802d
	out (clear_nmi),a	;802f Clear NMI flag
	in a,(slot3id)		;8031 See if we have an SIO in slot 3
	and 0xf8		;8033 (nonzero if absent)
	sub 0xe8		;8035
	ld b,a			;8037 Hang onto that result in B for POST
	jr nz,BEEP		;8038 No: skip setup
	ld a,0x7e		;803a Yes: send 3 setup bytes to USART
	ld hl,sio_ctlsetup_start;803c
	ld bc,0x0331		;803f
	otir			;8042 B is kept clear for loadsystem
	out (slot3|siobps),a	;8044 Baud register, 7E = 9600bps
	ld a,0xce		;8046
	out (slot3|sioctl),a	;8048 Mode register, CE = Async, 8-N-2
	ld a,0x37		;804a
	out (slot3|sioctl),a	;804c Command byte, enables tx/rx, etc.
.check_nmi_delay:
	djnz .check_nmi_delay	;804e Wait, then read a couple junk
	in a,(slot3|siodata)	;8050 bytes from USART
	in a,(slot3|siodata)	;8052
BEEP:
	in a,(sysbeep)		;8054 BEEP!
	ld hl,0x0000		;8056 What is SP at the moment anyway?
	add hl,sp		;8059
	ld a,h			;805a
	or l			;805b
	jp nz,loadsystem	;805c If SP != 0, run old ROM boot
	jp post_entrypoint	;805f Otherwise, run POST and new boot

; Empty space.
	defs 7			;8060 Seven empty bytes
	defb 0x18, 0x98		;8069 'jr init'

; This routine takes a sector number in A and waits for it to come around
; on the disk.  It sets the carry flag if it's found.
find_sector:
	cp 0x0b			;806b Bounds checking?  In *my* ROM?
	ret nc			;806d
	ld d,a			;806e Keep our test in D
.find_sector_loop:
	call current_sector	;806f Retrieve our current sector number
	ret nc			;8072 Fail out if current_sector fails
	cp d			;8073
	jr nz,.find_sector_loop	;8074 Loop if it wasn't found
	scf			;8076 Set carry if found
	ret			;8077

; This routine watches the rise and fall of the sector markers and fetches
; the sector number, with sensible timeouts of 0x2000 tries each.
current_sector:
	ld b,0x20		;8078 Timeout counter
.cs_wait_markhigh:
	dec bc			;807a
	ld a,b			;807b
	or c			;807c Return if time's up
	ret z			;807d
	in a,(iostatus1)	;807e See if marker has gone high
	and is1_mark		;8080
	jr z,.cs_wait_markhigh	;8082
	ld b,0x20		;8084 Reset our counter
.cs_wait_marklow:
	dec bc			;8086
	ld a,b			;8087
	or c			;8088
	ret z			;8089 Return if time's up
	in a,(iostatus1)	;808a See if marker has gone low
	and is1_mark		;808c
	jr nz,.cs_wait_marklow	;808e
	in a,(iostatus2)	;8090 Get sector counter
	and is2_data		;8092
	scf			;8094 Set carry to say we're OK
	ret			;8095

; A valiant effort is made to step slowly around until track 0 presents
; itself.  It returns nonzero if it found track 0.
find_track0:
	ld b,0x0a		;8096 Ten tries to step in
.leave_track0_stepin:
	ld a,0xa0		;8098 Stepping in first in case we're in
	call floppy_step	;809a the minus world.
	in a,(iostatus1)	;809d
	and is1_track0		;809f Are we on track 0? 
	jr z,.find_track0_found	;80a1 No, successfully left track 0
	djnz .leave_track0_stepin;80a3 Yes, step in more
	ret			;80a5 Return zero if out of tries
.find_track0_found:
	ld b,0x28		;80a6 Forty tries to step out
.find_track0_stepout:
	ld a,0x80		;80a8 Now stepping outward to find 0 again.
	call floppy_step	;80aa
	in a,(iostatus1)	;80ad
	and is1_track0		;80af
	ret nz			;80b1 Return happy...
	djnz .find_track0_stepout;80b2
	ret			;80b4 ...or just return a zero

; Ka-chunks the heads one step.  A contains the direction (a0 or 80 and C'
; contains the current drive selection.
floppy_step:
	exx			;80b5 Add our drive from C'
	or c			;80b6
	exx			;80b7
	out (fdd_ctl),a		;80b8 Select drive
	or fdc_step		;80ba
	out (fdd_ctl),a		;80bc Ka-
	xor fdc_step		;80be
	out (fdd_ctl),a		;80c0 -chunk
	ld a,0x28		;80c2 And fall into sleep below for an
				;     exceptionally generous 40ms

; Sleep for 'A' whiles (roughly 1ms for each while)
sleep:
	ld c,0xfa		;80c4
.sleep_delay:
	dec c			;80c6 This inner loop is 3999 T-states
	jr nz,.sleep_delay	;80c7 or 0.00099975 seconds.
	dec a			;80c9
	jr nz,sleep		;80ca
	ret			;80cc

; Once a sector number is found, this routine makes sure there's actually
; sync data in it, and retrieves the sector ID byte.  It sets the carry flag
; if it fails, and clears it if it succeeds.
read_sectorid:
	ld hl,0x03fd		;80cd Not totally sure...
.rsid_wait_marklow:
	in a,(iostatus1)	;80d0 No timeouts here, just wait for
	and is1_mark		;80d2 our sector mark to toggle
	jr nz,.rsid_wait_marklow;80d4
.rsid_wait_markhigh:
	in a,(iostatus1)	;80d6
	and is1_mark		;80d8
	jr z,.rsid_wait_markhigh;80da
	ld a,0x64		;80dc Once it does, wait briefly
.rsid_delay1:
	dec a			;80de
	jr nz,.rsid_delay1	;80df
	ld a,ioNreset|io_motors	;80e1 Clear acquire mode 
	out (ioctl),a		;80e3
	out (fdd_setread),a	;80e5 Let the world know we're reading
	ld a,0x18		;80e7
.rsid_delay2:
	dec a			;80e9
	jr nz,.rsid_delay2	;80ea
	ld a,ionormal|io_motors	;80ec Set acquire mode again and start drive
	out (ioctl),a		;80ee
	ld a,b			;80f0
	ld bc,0x64e0		;80f1
.rsid_wait_data:
	defb 0xed, 0x70		;80f4 'in f,(c)' from iostatus1
	jp m,.rsid_get_preamble	;80f6 Sign bit is set if disk serial is true
	djnz .rsid_wait_data	;80f9
	jr .rsid_ded		;80fb If we don't get it pretty fast, fail
.rsid_get_preamble:
	ld b,a			;80fd Data was detected, so grab sync byte
	in a,(fdd_syncbyte)	;80fe
	cp fdd_goodsync		;8100 
	jr nz,.rsid_ded		;8102 Bail if it's no good
	in a,(fdd_data)		;8104 Our next byte is the sector ID
	ld c,0x00		;8106
	or a			;8108 Make sure the carry flag is clear
	ret			;8109 We're good, bail
.rsid_ded:
	scf			;810a Set carry flag and bail if we failed
	ret			;810b

; Once the sector is found and the ID read and verified, it's time to read
; the 512 data bytes within it.  This routine does that thing, storing into
; a buffer at DE, and calculates the CRC as it goes (but does not check it.)
read_sectordata:
	in a,(fdd_data)		;810c Grab data byte
	ld (de),a		;810e Store it in buffer
	xor c			;810f Do the CRC thing
	rlca			;8110
	ld c,a			;8111
	inc de			;8112 Advance buffer pointer
	in a,(fdd_data)		;8113 Do it all twice per loop (256x2)
	ld (de),a		;8115
	xor c			;8116
	rlca			;8117
	ld c,a			;8118
	inc de			;8119
	djnz read_sectordata	;811a
	ret			;811c

; To initialize the floppy drive from power on, it has to have the data
; separator gently massaged a few times.
init_fdc:
	ld b,0x04		;811d Well, four times
.init_fdc_loop:
	out (fdd_setread),a	;811f Set the read flag
	ld a,0x7d		;8121 125-while delay
	call sleep		;8123
	in a,(fdd_clearread)	;8126 Clear the read flag
	ld a,0x7d		;8128 125-while delay
	call sleep		;812a
	djnz .init_fdc_loop	;812d
	ret			;812f

; Tell the 8035 to do something, then wait for its acknowledgement.  It's
; possible to just wait around for a minute and assume it did it, but this
; routine watches for the bit flip that means the 8035 heard us.
write_ioctl:
	push af			;8130 Hang onto what we wanted to say
	in a,(iostatus2)	;8131 Grab the current state of iostatus2 bit 7
	and is2_ack		;8133
	ld b,a			;8135
	pop af			;8136 Now send the command
	out (ioctl),a		;8137
.write_ioctl_wait:
	in a,(iostatus2)	;8139 And loop til that bit complements
	and is2_ack		;813b
	xor b			;813d
	jr z,.write_ioctl_wait	;813e
	ret			;8140

; Needs no introduction.  In the 2K ROM, it's an explicit length string
; instead of null-terminated, but since the big spenders put three strings
; in the 4K ROM, they added a real string print routine.
herald_start:
	defb "LOAD SYSTEM",0	;8141

; The three bytes here are sent to the USART in slot 3 to reset it.
sio_ctlsetup_start:
	defb 0x80, 0x80, 0x40 	; Attn, attn, reset
; The remaining 4 are sent for the serial boot setup.
	defb 0x0c		; N-8-0 sync mode
	defb 0x10, 0x16, 0xb7	; Sync characters

; This is the challenge string the other system is supposed to send the
; Advantage to negotiate a serial boot (from pressing S.)
	defb 0x10, 0x16, 0x05, 0xff

; And this is the response string the Advantage sends back.
	defb 0x10, 0x16, 0x04, 0x01, 0x05, 0xff

; This is the non-automatic boot prompt, which is normally encountered after
; canceling the auto boot sequence with a keystroke, or by restarting in
; some gentler way that stays out of cold_start.
loadsystem:
	ld a,vram_page0		;815e Set up our boot-time memory map
	out (set_bank0),a	;8160 VRAM page 0 in 0000-3FFF
	ld a,vram_page1		;8162
	out (set_bank1),a	;8164 VRAM page 1 in 4000-7FFF
	ld sp,stack_top		;8166 Make ourselves a nice stack
	ld a,b			;8169
	ld (sio_absent),a	;816a 00 here means SIO is in slot 3
	ld a,ram_page0		;816d
	out (set_bank3),a	;816f Main RAM page 0 in C000-FFFF
	out (video_start),a	;8171 Screen draw starts at line 0 
	out (clear_vsync),a	;8173 Refresh flag is cleared
	jr z,.ls_skipherald	;8175 Restart video if NZ (e.g. from cold or POST)
	call init_terminal	;8177 
	ld hl,herald_start	;817a Display our prompt
	call disp_str		;817d
.ls_skipherald:
	ld a,ionormal		;8180 Turn on display 
	out (ioctl),a		;8182
.ls_getcmd:
	call disp_newline	;8184
	ld hl,.ls_wat		;8187 Any unsuccessful returns will print 
	push hl			;818a a '?' before prompting again
	call get_char		;818b
	cp 'N'			;818e Network boot case
	jp z,loadkeyN		;8190
	cp 'D'			;8193 Alternate disk boot case
	jr z,loadkeyD		;8195
	cp 'S'			;8197 Serial boot case
	jp z,loadkeyS		;8199
	cp 0x01			;819c Ctrl-A hard resets
	jp z,cold_start		;819e
	sub 0x0d		;81a1 Enter clears A, loads from default drive
	jr z,loadkeyRet		;81a3
	ret			;81a5 Anything else gets us a '?'

; Bad entry or failed boots get the question mark; a return from the mini
; monitor just gets us a friendly herald and a hop back to the prompt after
; a short delay.  The 2K ROM just clears the screen every time, but the 4K
; is more civilized and sometimes has to say more things.
.ls_wat:
	ld b,'?'		;81a6 Whaaaaaaa
	call disp_chartx	;81a8
warm_loadsystem:
	call disp_newline	;81ab Carriage return and herald
	ld hl,herald_start	;81ae
	call disp_str		;81b1
	ld a,0x02		;81b4 Wait 2ms or so
	call sleep		;81b6
	jr .ls_getcmd		;81b9

; ### Non-automatic floppy boot

; If return is pressed, the prompt for a drive number is skipped and it just
; loads the default drive 1 into A.
loadkeyRet:
	inc a			;81bb
	jr .lkD_defaultdrive	;81bc

; If D is pressed, an additional prompt is made for a drive number, which
; contrary to the manual can be a number from 1-4.
loadkeyD:
	call get_char		;81be Grab a keystroke
	cp '1'			;81c1
	ret c			;81c3 Too low, bail
	cp '5'			;81c4
	ret nc			;81c6 Too high, bail
	sub 0x30		;81c7 Just right, so knock the ASCII off it
	ld b,a			;81c9 Store it as a counter
	xor a			;81ca Clear A and set carry
	scf			;81cb 
.lkD_storedrive:
	rla			;81cc Roll the carry bit in B times
	djnz .lkD_storedrive	;81cd (drive 1=1, 2=2, 3=4, 4=8)
.lkD_defaultdrive:
	exx			;81cf
	ld c,a			;81d0 Our freshly generated drive mask goes
	exx			;81d1 in C'
	ld a,ionormal|io_motors	;81d2 Start drive
	out (ioctl),a		;81d4
	ld a,parity_clr		;81d6 Clear parity error flag
	out (parity_ctl),a	;81d8
	call find_track0	;81da Try to get track 0
	ret z			;81dd And bail if we can't
	call init_fdc		;81de 
	exx			;81e1
	ld b,0x28		;81e2 B' is retry counter (40 tries)
	exx			;81e4
.lkD_find_sec4:
	in a,(fdd_clearread)	;81e5
	exx			;81e7
	dec b			;81e8 Decrement retries
	exx			;81e9
	ret z			;81ea Bail if we ran out
	ld a,0x03		;81eb Try to find sector 4
	call find_sector	;81ed
	ret nc			;81f0 Fail (probably no disk in drive)
	ld a,0x04		;81f1 A' is how many sectors to grab
	ex af,af'		;81f3 
	ld e,0x00		;81f4 Buffer pointer low byte starts at 0
	ld b,0xff		;81f6 First read loop grabs 510 bytes
	call read_sectorid	;81f8 Now get sector ID byte (discarded)
	jr c,.lkD_find_sec4	;81fb Retry if failed
	in a,(fdd_data)		;81fd First byte is our origin
	cp 0xc0			;81ff Must be between C000 and F800
	ret c			;8201 Too low, bail
	cp 0xf9			;8202
	ret nc			;8204 Too high, bail 
	ld d,a			;8205 Good, so DE is our buffer
	ld (de),a		;8206 Save that byte
	inc de			;8207
	rlca			;8208 CRC goes in C
	ld c,a			;8209
	in a,(fdd_data)		;820a Read extra byte for an even 510 in loop
	ld (de),a		;820c
	inc de			;820d
	xor c			;820e More CRC
	rlca			;820f
	ld c,a			;8210
.lkD_read_next:
	call read_sectordata	;8211 Read rest of sector data
	in a,(fdd_data)		;8214 And our CRC byte
	xor c			;8216 Test it against our thing
	in a,(fdd_clearread)	;8217 
	jr nz,.lkD_find_sec4	;8219 No good, retry
	ex af,af'		;821b Good, decrement A'
	dec a			;821c
	jr z,.lkD_boot		;821d Got all our sectors, try to boot 
	ex af,af'		;821f
	call read_sectorid	;8220 Fetch next sector ID
	jr c,.lkD_find_sec4	;8223 Try again from top if that failed
	jr .lkD_read_next	;8225 Otherwise loop (512 byte reads now)
.lkD_boot:
	ld hl,0xf80a		;8227 This wraps around to ten bytes into
	add hl,de		;822a the buffer, which is where a jump
	ld a,(hl)		;822b should be
	cp 0xc3			;822c Is it a jump?
	ret nz			;822e No, not bootable
	xor a			;822f Yes, so...
	out (set_bank0),a	;8230 Bank first 16K into 0000-3FFF
	out (set_bank1),a	;8232 And also into 4000-7FFF
	jp (hl)			;8234 And boot.

; ### Non-automatic serial boot

; This one will only start from an SIO in slot 3, and it's also nothing to
; do with what's in the manual.  The handshake described therein is not
; actually used here, even though the bytes for it are included elsewhere
; in the ROM.  Maybe they decided, rightly, that it was too clever by half.
loadkeyS:
	in a,(slot3id)		;8235 Check for a board ID of F0-F7 
	and 0xf8		;8237
	cp 0xf0			;8239
	ret nz			;823b Alas, fail
	call get_char		;823c Success, wait for return keypress
	cp 0x0d			;823f
	ret nz			;8241 Bail if it's anything but return
	ld a,0x00		;8242
	call init_sio		;8244 Reset SIO and clear baud register
	ld b,0x04		;8247 Write four more setup bytes to sioctl
	ld d,b			;8249 D is our RX byte count, also 4
	otir			;824a
	in a,(slot3|siodata)	;824c Input two junk bytes
	in a,(slot3|siodata)	;824e
	call sio_write		;8250 And transmit one
	ld c,b			;8253 Clear BC (this is our timeout)
.lkS_handshakerx:
	dec bc			;8254 Handshake receive loop
	ld a,b			;8255
	or c			;8256
	jr nz,.lkS_skipbail	;8257
	ld a,d			;8259 Only time out if we're 0-2 bytes in
	cp 0x03			;825a
	ret nc			;825c
.lkS_skipbail:
	in a,(slot3|sioctl)	;825d See if there's a byte to fetch
	and sio_rxrdy		;825f
	jr z,.lkS_handshakerx	;8261 Loop if not
	in a,(slot3|siodata)	;8263 Fetch if so
	cp (hl)			;8265 And compare it to our expectation
	jr nz,.lkS_handshakerx	;8266 Don't advance if it's no good
	inc hl			;8268 If it is OK, advance to next byte 
	dec d			;8269
	jr nz,.lkS_handshakerx	;826a And loop once more 
	ld b,d			;826c Done, delay for a while
.lkS_delay:
	djnz .lkS_delay		;826d
	dec d			;826f
	jr nz,.lkS_delay	;8270
	ld c,0x06		;8272 Transmit 6-byte response
.lkS_handshaketx:
	ld b,(hl)		;8274
	call sio_write		;8275
	inc hl			;8278
	dec c			;8279
	jr nz,.lkS_handshaketx	;827a
.lkS_getSTX:
	call sio_read		;827c Read til we get STX (0x02)
	cp 0x02			;827f
	jr nz,.lkS_getSTX	;8281 
	call sio_read		;8283 Then read the high byte of our buffer
	ld d,a			;8286
	ld e,0x00		;8287 Buffer start is DE
	ld b,e			;8289
	ld ix,0x000a		;828a DE+10 is our boot address
	add ix,de		;828e
	ld hl,0x0001		;8290 HL is checksum
.lkS_rxdata:
	ld (de),a		;8293 Store byte into buffer
	inc de			;8294
	ld c,a			;8295 Compute checksum based only on stored
	add hl,bc		;8296 bytes (not escaped characters)
.lkS_nostore:
	call sio_read		;8297 Get next byte
	cp 0x10			;829a Check for escape character DLE
	jr nz,.lkS_rxdata	;829c Loop and store if not
	call sio_read		;829e Drop DLE and read another byte if so
	cp 0x16			;82a1 DLE, SYN is dropped
	jr z,.lkS_nostore	;82a3 
	cp 0x03			;82a5 DLE, ETX ends transmission
	jr nz,.lkS_rxdata	;82a7 DLE, DLE stores a single DLE
	call sio_read		;82a9 Read and check low checksum byte
	cp l			;82ac
	ret nz			;82ad Fail on mismatch
	call sio_read		;82ae Read and check high checksum byte
	cp h			;82b1
	ret nz			;82b2 Fail on mismatch
	call sio_read		;82b3 Success: read and discard one more
	xor a			;82b6 
	out (set_bank0),a	;82b7 Bank first 16K into 0000-3FFF
	out (set_bank1),a	;82b9 and also 4000-7FFF
	jp (ix)			;82bb Then boot

; Mini monitor.  The entry point is 'monitor'.
mon_getline:
	ld b,'*'		;82bd Print the * prompt
	call disp_chartx	;82bf
	call get_char		;82c2 See what the user has to say
	cp 'D'			;82c5
	jr z,mon_display	;82c7 D - Display/edit memory
	cp 'J'			;82c9
	jr z,mon_jump		;82cb J - Jump to address
	cp 'I'			;82cd
	jr z,mon_pin		;82cf I - Port input
	cp 'O'			;82d1
	jr z,mon_pout		;82d3 O - Port output
	cp 'Q'			;82d5
	jp z,warm_loadsystem	;82d7 Q - Quit back to load system prompt
	cp 'A'			;82da
	jp z,mon_advanced	;82dc Q - Advanced routines (4K specific)
monitor:
	ld sp,stack_top		;82df Reset stack pointer
	ld b,0x18		;82e2 Set cursor on
	call disp_char		;82e4
	call disp_newline	;82e7 Carriage return
	jr mon_getline		;82ea

; Mini monitor port output routine.
mon_pout:
	call mon_get2hex	;82ec Get two bytes
	out (c),b		;82ef Mail it out
	jr monitor		;82f1

; Mini monitor port input routine.
mon_pin:
	call mon_get1hex	;82f3 Get one byte
	in b,(c)		;82f6 Retrieve data from port
	call disp_hex16		;82f8 Print it as hex
	jr monitor		;82fb

; Mini monitor jump routine.
mon_jump:
	call mon_get2hex	;82fd Get two bytes
	ld hl,monitor		;8300 Tell the callee how to get back
	push bc			;8303 Push our bytes to the stack
	ret			;8304 And pop them into PC

; Mini monitor display/edit routine.
mon_display:
	call mon_get2hex	;8305 Get our start address
	ld (dispbuffer),bc	;8308 Hang onto it
.md_readloop:
	ld hl,(dispbuffer)	;830c Fetch a byte
	ld b,(hl)		;830f
	inc hl			;8310 Increment pointer and store back
	ld (dispbuffer),hl	;8311
	call disp_hex16		;8314 Display next byte
	ld b,'-'		;8317 Display edit prompt
	call disp_chartx	;8319
	call get_char		;831c Get a keystroke
	cp ' '			;831f
	jr nz,.md_writeback	;8321 If it's not space, it's the first nibble
	call disp_chartx	;8323 Otherwise skip edit
	jr .md_readloop		;8326 Next byte please
.md_writeback:
	cp 0x0d			;8328 Return bounces back to monitor
	jr z,monitor		;832a
	call .mgh_getchar2	;832c Get second nibble and pack byte
	ld hl,(dispbuffer)	;832f Back up to the byte we just edited
	dec hl			;8332 
	ld (hl),c		;8333 And store edited value
	jr .md_readloop		;8334 Next byte please

; Get two bytes in hex (4 hex digits).  There's no error checking, so
; characters other than 0-9, capital A-F will produce incorrect results.
mon_get2hex:
	call get_char		;8336 Get first nibble in L
	ld l,a			;8339
	call get_char		;833a Get second nibble in H
	ld h,a			;833d
	ld (hex_input2),hl	;833e Store them in hex_input2
				;     and continue

; Get one byte in hex (2 hex digits).  The full 16-bit number is still
; converted though.
mon_get1hex:
	call get_char		;8341 Get first nibble in L
.mgh_getchar2:
	ld l,a			;8344
	call get_char		;8345 Get second nibble in H
	ld h,a			;8348
	ld (hex_input1),hl	;8349 Store them in hex_input1
	ld hl,hex_input2	;834c Set up to convert all 4 to binary
	ld b,0x04		;834f
.mgh_nextnibble:
	ld a,(hl)		;8351 Grab ASCII value from store
	sub 0x30		;8352 Adjust number down so ASCII 0=0x00
	cp 0x0a			;8354 If number is less than 10, we're good
	jr c,.mgh_noadjust	;8356
	sub 0x07		;8358 >10 means A-F, subtract 7 more
.mgh_noadjust:
	bit 0,b			;835a If counter is odd, it's a low nibble
	jr nz,.mgh_lownibble	;835c
	add a,a			;835e High nibbles get shifted left
	add a,a			;835f
	add a,a			;8360
	add a,a			;8361
	ld c,a			;8362 And stored in C
	jr .mgh_highnibble	;8363
.mgh_lownibble:
	add a,c			;8365 Low nibbles get combined with high
	ld (hl),a		;8366 And stored back in place
.mgh_highnibble:
	inc hl			;8367 Increment pointer and decrement count
	djnz .mgh_nextnibble	;8368
	dec hl			;836a Once finished, load conversion into BC
	ld c,(hl)		;836b
	dec hl			;836c
	dec hl			;836d
	ld b,(hl)		;836e
	ret			;836f

; Disp_hex16 actually handles the entirety of the formatting for the mini
; monitor's display routine.  It shows the current address at the beginning
; of each line, and wraps to the next line with a new address when the
; cursor moves far enough along.
disp_hex16:
	ld a,(vdd+vdb_x)	;8370
	cp 0x4b			;8373 Continue line if cursor<column 75
	ld c,b			;8375
	jr c,disp_space		;8376 
	call disp_newline	;8378 Otherwise, new line and
	ld b,' '		;837b two spaces (to line up with '*D')
	call disp_chartx	;837d
	call disp_chartx	;8380
	ld hl,(dispbuffer)	;8383 Load our display pointer 
	dec hl			;8386 Back it up one
	ld e,c			;8387
	ld c,h			;8388
	call disp_hex8		;8389 Then display the address H
	ld c,l			;838c
	call disp_hex8		;838d and address L
	ld c,e			;8390
disp_space:
	ld b,' '		;8391 Display a space and fall into disp_hex8
	call disp_chartx	;8393

; Disp_hex8 is more general purpose and just shows ye olde hex byte in C.
disp_hex8:
	ld d,0x02		;8396 Counter for two digits
	ld a,c			;8398
	and 0xf0		;8399 Relocate the top nibble to the bottom
	rrca			;839b
	rrca			;839c
	rrca			;839d
	rrca			;839e
.dh8_nextdigit:
	add a,0x30		;839f Convert to ASCII 0-9
	cp 0x3a			;83a1
	jr c,.dh8_0to9		;83a3
	add a,0x07		;83a5 If it's A-F, nudge the number over more
.dh8_0to9:
	ld b,a			;83a7 Then print it
	call disp_chartx	;83a8
	ld a,c			;83ab Load our bottom nibble and repeat
	and 0x0f		;83ac
	dec d			;83ae
	jr nz,.dh8_nextdigit	;83af
	ret			;83b1

; Carriage return.  This doesn't implement scrolling, so when it gets to the
; bottom of the screen, it clears it and starts over.  This is because the
; boot code actually stores the video driver block in VRAM just offscreen.
disp_newline:
	ld b,0x1f		;83b2 Press enter
	call disp_chartx	;83b4
	ld a,(vdd+vdb_y)	;83b7 See if we're on line 230 (last row)
	cp 0xe6			;83ba
	ret nz			;83bc If not, we're good
	ld b,0x1e		;83bd Otherwise, home cursor
	call disp_char		;83bf
	ld b,0x0f		;83c2 And clear to end of screen
	jp disp_char		;83c4

; Clear the screen and set up the video driver block in VRAM.
init_terminal:
	xor a			;83c7
	ld h,0x7f		;83c8 Clear all 128 columns for some reason
.it_clearrow:
	ld l,0xf0		;83ca But only 240 rows (all visible ones)
.it_clearcol:
	dec l			;83cc
	ld (hl),a		;83cd
	jr nz,.it_clearcol	;83ce Nexy Y
	dec h			;83d0
	jr nz,.it_clearrow	;83d1 Next X
.it_clearcol0:
	ld (hl),a		;83d3 Clear all 255 rows of column 0
	dec l			;83d4
	jr nz,.it_clearcol0	;83d5
	ld hl,video_font	;83d7 Set pointer to character maps
	ld (vdd+vdb_pixmaps),hl	;83da
	ld hl,cursormask	;83dd Set pointer to cursor
	ld (vdd+vdb_cursptr),hl	;83e0
	ld bc,0x0aff		;83e3 Build a solid green cursor
.it_clearcurs:
	ld (hl),c		;83e6
	inc l			;83e7
	djnz .it_clearcurs	;83e8
	ret			;83ea

; New for the 4K ROM, this is the manually entered network boot routine. 
; Hard to say what this did on the hardware side, but here it is.
loadkeyN:
	in a,(slot5id)		;83eb See if slot 5 has a network board
	cp id_net		;83ed
	jr z,.lkN_boardisDF	;83ef Orrrr...?
	in a,(parity_status)	;83f1 This test is a big mystery because
	and 0x80		;83f3 the only defined thing in parity_status
	ret nz			;83f5 is bit 0.
.lkN_boardisDF:
	xor a			;83f6 Send 0 to the command register
	out (slot5|1),a		;83f7
	ld b,0x32		;83f9 50 tries to do it.
.lkN_waitmore:
	ld a,0x64		;83fb Wait 100ms
	call sleep		;83fd
	in a,(slot5|1)		;8400 See if anything is ready for us
	and 0x01		;8402
	jr nz,.lkN_blockread	;8404 Yes!
	dec b			;8406 No...
	ret z			;8407 Fail if out of tries,
	jr .lkN_waitmore	;8408 or wait more otherwise
.lkN_blockread:
	ld bc,0x0010		;840a 256 bytes from slot5|0 (data register)
	ld hl,net_buffer	;840d Stored into a fixed location
	push hl			;8410 Push that location,
	inir			;8411 fill the buffer,
	ret			;8413 and git.

; The video driver is dropped in here in its entirety.

include "videodriver.asm"

; These two entry points are where plebes are supposed to jump to the video
; driver.
hooks_start:
	jp video_drawcursor	;87fa Redraw cursor
	jp video_driver		;87fd Video driver

; The 4K ROM includes this new POST routine, which at least takes a crack at
; testing memory and seeing if the 8035 is interested in having a chat.  The
; entry point is at the end of the ROM, but the actual tests start here.

; First test for communication with 8035.
post_io:
	xor a			;8800 Clear out DE' to store intermediate
	ld d,a			;8801 results for POST tests
	ld e,a			;8802 B is saved as B' here also
	exx			;8803 
	in a,(iostatus2)	;8804 Put 8035 ack bit in B
	and is2_ack		;8806
	ld b,a			;8808
	ld a,ionormal|io_vblank	;8809 Shut off the screen
	out (ioctl),a		;880b
	ld c,0xb5		;880d Timeout counter
.postio_wait:
	in a,(iostatus2)	;880f Get ack bit again
	and is2_ack		;8811
	xor b			;8813
	jr nz,.postio_ack	;8814 If it toggles, that's good
	dec c			;8816
	jr nz,.postio_wait	;8817 Keep waiting if not
	jr .postio_timeout	;8819 And fail if time runs out
.postio_ack:
	in a,(iostatus2)	;881b Get status again and see if data nibble
	and is2_data		;881d is 0x0e (meaning drive motors are off)
	cp 0x0e			;881f
	jr z,post_net		;8821 If it is, 8035 is good enough for us
.postio_timeout:
	exx			;8823 If not, or if we timed out earlier,
	ld a,d			;8824 set bit 1 of D' to flag it
	or 0x02			;8825
	ld d,a			;8827
	exx			;8828

post_net:
	in a,(slot5id)		;8829 Now let's see if there's a network card
	cp id_net		;882b
	jr nz,post_ram		;882d No, so don't test it
	ld a,0x19		;882f Yes, say hi to card with command 0x19
	out (slot5|1),a		;8831
	ld bc,0x2000		;8833 Longer timeout value here
.postnet_wait:
	in a,(slot5|1)		;8836 Wait for status bit 7 to go high
	and 0x80		;8838
	jr nz,.postnet_got7	;883a Break when it does
	dec bc			;883c
	ld a,b			;883d
	or c			;883e
	jr nz,.postnet_wait	;883f Keep waiting
	jr .postnet_timeout	;8841 Fail if out of time
.postnet_got7:
	in a,(slot5|1)		;8843 Wait for status bit 3 to go high
	and 0x08		;8845
	jr nz,.postnet_timeout	;8847 Keep waiting
	ld a,0x18		;8849 Got it, now do command 0x18
	out (slot5|1),a		;884b
	ld bc,0x2000		;884d Reset our timeout
.postnet_wait2:
	in a,(slot5|1)		;8850 And read status again
	and 0x80		;8852 
	jr z,post_ram		;8854 Success if bit 7 is high
	dec bc			;8856
	ld a,b			;8857
	or c			;8858
	jr nz,.postnet_wait2	;8859 Keep waiting
.postnet_timeout:
	exx			;885b Out of time, so set bit 2 of D' to
	ld a,d			;885c flag the error
	or 0x04			;885d
	ld d,a			;885f
	exx			;8860

; Test main RAM in a basic fashion.  It also spins up the drives and cycles
; the disk read flag, since that needs to be done slowly four times and
; tucking 1/4 of those cycles into each bank test is convenient.  It might
; also be spinning the disks to stress things harder.

post_ram:
	ld a,ionormal|io_vblank|io_motors;8861 Keep screen off but spin disk
	out (ioctl),a		;8863
	ld b,0x04		;8865
.postram_wait:
	dec a			;8867 Then wait around for a bit for 8035 to
	jr nz,.postram_wait	;8868 respond to our thing
	djnz .postram_wait	;886a 
	ld a,ionormal|io_vblank	;886c And remove motors command
	out (ioctl),a		;886e
	ld a,0x80|fdc_in|fdc_drive1;8870 Select drive 1, step in
	out (fdd_ctl),a		;8872 (and a bonus high bit)
	exx			;8874
	ld a,d			;8875 
	or 0x08			;8876 Set D' bit 3
	ld d,a			;8878 (RAM error flag)
	ld a,e			;8879
	or 0x60			;887a Set E' bits 1 and 2
	ld e,a			;887c (Parity error flags)
	exx			;887d
	ld a,parity_clr		;887e Clear hardware parity flag
	out (parity_ctl),a	;8880
	ld b,0x04		;8882 Four pages to test, from the top
.postram_bank:
	ld a,b			;8884 
	dec a			;8885
	out (set_bank0),a	;8886 Bank test page into 0000-3FFF
	out (fdd_setread),a	;8888 Set disk read flag
	ld hl,0x4000		;888a Top of bank 0 (+1)
.postram_write:
	dec hl			;888d
	ld a,(hl)		;888e Write the sum of H+L into (HL) 
	ld a,l			;888f
	add a,h			;8890
	ld (hl),a		;8891
	ld a,l			;8892
	or h			;8893
	jr nz,.postram_write	;8894 Loop while HL>0
	in a,(fdd_clearread)	;8896 Clear disk read flag
	ld c,0x00		;8898 C is our RAM read error flag
	ld hl,0x4000		;889a Top of bank 0 (+1) again
.postram_read:
	dec hl			;889d
	ld a,l			;889e See if we read H+L back from (HL)
	add a,h			;889f
	cp (hl)			;88a0
	jr z,.postram_byteok	;88a1 Good, skip next instruction
	ld c,0x01		;88a3
.postram_byteok:
	ld a,l			;88a5
	or h			;88a6
	jr nz,.postram_read	;88a7 Loop while HL>0
	ld a,c			;88a9
	or a			;88aa
	jr z,.postram_blockok	;88ab C=0 is happy times
	xor a			;88ad Sad times, roll carry flag into
	scf			;88ae high nibble to make a bitmap of
	ld c,b			;88af bad block
.postram_blockbad:
	rra			;88b0
	dec c			;88b1
	jr nz,.postram_blockbad	;88b2 Rollin rollin rollin
	exx			;88b4
	or d			;88b5 Record bitmap into D'
	ld d,a			;88b6
	exx			;88b7
.postram_blockok:
	djnz .postram_bank	;88b8 Loop through all 4 banks
	in a,(parity_status)	;88ba See if we had any parity troubles
	and parity_error	;88bc
	exx			;88be
	jr nz,.postram_paritybad;88bf Yes, flag it
	ld a,e			;88c1
	and 0xbf		;88c2 No, clear bit 6 of E' to
	ld e,a			;88c4 declare success
	ld a,d			;88c5
	and 0xf7		;88c6 And same with bit 3 of D'
	ld d,a			;88c8
.postram_paritybad:
	ld a,parity_clr		;88c9 Clear parity flag again
	out (parity_ctl),a	;88cb
	ld a,d			;88cd Save our bitmap for tryblock
	exx			;88ce
	ld b,0x04		;88cf Now look for an error-free page
.postram_sel_good:
	dec b			;88d1
	jr nz,.postram_tryblock	;88d2 Is it this one?
	xor a			;88d4
	jr .postram_store_status;88d5
.postram_tryblock:
	rlca			;88d7 Roll our bitmap back out
	jr c,.postram_sel_good	;88d8 Keep rolling til we find a 0
				;     or run out of banks (godspeed if so)
	ld a,0x03		;88da The actual bank is 3-B
	sub b			;88dc
.postram_store_status:
	out (set_bank3),a	;88dd Map it into C000-7FFF
	ld (current_bank),a	;88df 
	ld a,vram_page0		;88e2 VRAM page 0 into 0000-3FFF
	out (set_bank0),a	;88e4
	inc a			;88e6 VRAM page 1 into 4000-7FFF
	out (set_bank1),a	;88e7
	ld sp,0xd000		;88e9 Move stack into main RAM
	exx			;88ec
	ld a,d			;88ed Now store POST status bytes into RAM
	ld (post_status),a	;88ee
	ld a,e			;88f1
	ld (post_parity),a	;88f2
	ld a,b			;88f5
	exx			;88f6
	call char_ready_nodebug	;88f7 Skip VRAM test if a key is pressed
	jp nz,.post_skip	;88fa
	ld a,ionormal|io_vblank|io_motors;88fd Start motors again
	out (ioctl),a		;88ff
	ld a,0x03		;8901 And wait for it to happen
	call sleep		;8903

; Video memory test
	ld hl,0x5000		;8906 Only actually test 20KB
postvram_write:
	dec hl			;8909 Same as main RAM, fill with H+L
	ld a,(hl)		;890a 
	ld a,l			;890b
	add a,h			;890c
	ld (hl),a		;890d
	ld a,l			;890e
	or h			;890f
	jr nz,postvram_write	;8910
	ld b,0x00		;8912 B is our error flag this time
	ld hl,0x5000		;8914
.postvram_read:
	dec hl			;8917
	ld a,l			;8918
	add a,h			;8919
	cp (hl)			;891a Make sure we get back what we wrote
	jr z,.postvram_byteok	;891b Yes, skip next instruction
	ld b,0x01		;891d No, set flag
	jp .postvram_storestatus;891f
.postvram_byteok:
	ld a,l			;8922
	or h			;8923
	jr nz,.postvram_read	;8924 Loop until we're done
.postvram_storestatus:
	ld a,b			;8926 See if we had troubles
	cp 0x01			;8927
	jr nz,.postvram_vrambad	;8929 Yes, leave POST error bit set
	ld a,(post_status)	;892b No, update bit 0 of POST status
	or 0x01			;892e
	ld (post_status),a	;8930
.postvram_vrambad:
	in a,(parity_status)	;8933 Now check for parity error
	and parity_error	;8935
	jr nz,post_init_term	;8937 Yes, leave POST parity bit set
	ld a,(post_parity)	;8939 No, clear just the VRAM error bit
	and 0xdf		;893c
	ld (post_parity),a	;893e

; Start video
post_init_term:
	ld a,parity_clr		;8941 And clear parity flag again
	out (parity_ctl),a	;8943
	xor a			;8945 Set our hardware video scanline to 0 
	out (video_start),a	;8946
	call init_terminal	;8948 Clear screen and set up video driver
	exx			;894b
	ld a,b			;894c Fetch cold boot debug SIO test from B'
	exx			;894d
	ld (sio_absent),a	;894e Store it into sio_absent
	ld a,ionormal|io_vblank	;8951
	out (ioctl),a		;8953
	ld a,0x03		;8955 Wait 3ms
	call sleep		;8957
	jp auto_loadsystem	;895a POST passed, advance to auto boot
.post_skip:
	exx			;895d Skipping post drops back into 2K-style
	ld a,0x01		;895e manual load system prompt
	or a			;8960 Clearing Z re-inits video
	jp loadsystem		;8961

; See if a character is waiting for us on the wire or on the keys.
char_ready:
	ld a,(sio_absent)	;8964 Do we have a SIO in slot 3?
char_ready_nodebug:		;     (enter here to only test for keys)
	or a			;8967 
	jr nz,.cr_nodebugsio	;8968 No SIO, see about keys
	in a,(slot3|sioctl)	;896a Yes SIO
	and sio_rxrdy		;896c Return just the RX flag
	ret			;896e
.cr_nodebugsio:
	ld a,(post_status)	;896f Do we have a working 8035?
	and 0x02		;8972
	jr z,.cr_haskeydata	;8974 Probably, so inquire of it
	xor a			;8976 No, return 0
	ret			;8977
.cr_haskeydata:
	in a,(iostatus2)	;8978 See if there's any key data 
	and is2_keydata		;897a and return it
	ret			;897c

; Additional/advanced mini monitor routines, not present in 2K ROM, which do
; manual diagnostics of the floppy drives.
mon_advanced:
	ld b,0x1e		;897d Clear screen
	call disp_char		;897f
	ld b,0x0f		;8982
	call disp_char		;8984
	ld hl,monitorheader	;8987 Show column titles
	call disp_str		;898a
	xor a			;898d Clear out E' and C'
	ld e,a			;898e Track count in BCD
	exx			;898f
	ld c,a			;8990 Drive control byte
	exx			;8991
	ld d,0x81		;8992
	ld b,0x19		;8994 Cursor off
	call disp_char		;8996
	ld a,ionormal|io_motors	;8999 Spin the disks
	call write_ioctl	;899b
	ld a,ionormal		;899e View sector command (unnecessary)
	call write_ioctl	;89a0
	call find_track0	;89a3 Drag heads back to home
	call init_fdc		;89a6 Reset data separator
.ma_getcmd:
	ld a,d			;89a9
	out (fdd_ctl),a		;89aa
	ld a,ionormal|io_motors	;89ac Spin the disks again
	call write_ioctl	;89ae
	ld a,ionormal		;89b1 View sector (still unnecessary)
	call write_ioctl	;89b3
	in a,(iostatus1)	;89b6 See if we're on track 0
	bit 5,a			;89b8
	jr z,.ma_notrack0	;89ba  
	ld e,0x00		;89bc E is our track counter
.ma_notrack0:
	call disp_binary	;89be This is port E0's bitmap
	ld a,d			;89c1
	call disp_binary	;89c2 and port 81's
	ld b,' '		;89c5 Print two spaces
	call disp_chartx	;89c7
	call disp_chartx	;89ca
	ld a,e			;89cd Print high nibble of track
	and 0f0h		;89ce as ASCII
	rrca			;89d0
	rrca			;89d1
	rrca			;89d2
	rrca			;89d3
	add a,030h		;89d4
	ld b,a			;89d6
	call disp_chartx	;89d7
	ld a,e			;89da Print low nibble of track also
	and 00fh		;89db
	add a,030h		;89dd
	ld b,a			;89df
	call disp_chartx	;89e0
	ld b,0x0d		;89e3 Carriage return
	call disp_chartx	;89e5
	call char_ready		;89e8 Is a key waiting? 
	jr z,.ma_loop		;89eb No, loop to keep drives running
	call get_char_noecho	;89ed Yes, fetch it
	cp 'H'			;89f0 H - Head select
	jr z,.ma_keyH		;89f2
	cp 'D'			;89f4 D - Drive select
	jr z,.ma_keyD		;89f6
	cp 'I'			;89f8 I - Step inward
	jr z,.ma_keyI		;89fa
	cp 'O'			;89fc O - Step outward
	jr z,.ma_keyO		;89fe
	cp 'Q'			;8a00 Q - Back to monitor
	jp z,monitor		;8a02
	cp 017h			;8a05 Control-W - write test
	call z,.ma_keyctrlW	;8a07
.ma_loop:
	jr .ma_getcmd		;8a0a

; Toggle head selection
.ma_keyH:
	ld a,d			;8a0c
	xor fdc_side		;8a0d
	ld d,a			;8a0f
	jr .ma_loop		;8a10

; Cycle through drives
.ma_keyD:
	ld a,d			;8a12 Rotate left and mask lower nibble
	add a,a			;8a13
	and 00fh		;8a14
	jr nz,.ma_keyD_set	;8a16 
	inc a			;8a18 Reset to drive 1 if it was >4
.ma_keyD_set:
	ld b,a			;8a19 Write new nibble back into register
	ld a,d			;8a1a
	and 0xf0		;8a1b
	or b			;8a1d
	ld d,a			;8a1e
	jr .ma_loop		;8a1f

; This is the secret revealed unto us when pressing A in the new monitor.
monitorheader:
	defb 0x1f,"port E0H  port 81H  track#", 0x1f, 0x1f, 0

; Step heads inward
.ma_keyI:
	ld a,e			;8a3f
	inc a			;8a40 Increment our track number
	daa			;8a41 And store it as BCD
	ld e,a			;8a42
	ld a,fdc_in		;8a43 Set step in flag
	or d			;8a45
	exx			;8a46
	ld c,0x00		;8a47 Clear C'
	exx			;8a49
	call floppy_step	;8a4a Step.
	jr .ma_loop		;8a4d

; Step heads outward
.ma_keyO:
	ld a,e			;8a4f
	dec a			;8a50 Decrement our track number
	daa			;8a51 And store it as BCD
	ld e,a			;8a52
	ld a,d			;8a53
	and ~fdc_in		;8a54 Clear set in flag
	exx			;8a56
	ld c,0x00		;8a57 Clear C'
	exx			;8a59
	call floppy_step	;8a5a Step.
	jr .ma_loop		;8a5d

; Display a byte in binary
disp_binary:
	ld c,0x08		;8a5f Eight bits to show.
.db_loop:
	rlca			;8a61 Roll left 
	ld b,'0'		;8a62 
	jr nc,.db_is0		;8a64 If there's no carry, stay '0'
	inc b			;8a66 Otherwise become '1'
.db_is0:
	push af			;8a67 Save our byte
	call disp_chartx	;8a68 Print our digit
	pop af			;8a6b Fetch our byte
	dec c			;8a6c
	jr nz,.db_loop		;8a6d and loop.
	ld b,' '		;8a6f Then print two spaces
	call disp_chartx	;8a71
	call disp_chartx	;8a74
	ret			;8a77

; Keystroke read with echo
get_char:
	call get_char_noecho	;8a78 Get it
	call disp_chartx	;8a7b Print it
	ret			;8a7e Done

; Display a character and transmit it too.
disp_chartx:
	call disp_char		;8a7f Always print to screen
	ld a,(sio_absent)	;8a82
	or a			;8a85
	ld a,b			;8a86
	call z,sio_write	;8a87 Also to slot 3 SIO if present
	ret			;8a8a

; Get a character without echoing to the screen.
get_char_noecho:
	ld a,(sio_absent)	;8a8b
	or a			;8a8e
	jr z,.gcn_sio_read	;8a8f Skip to SIO if available
	ld a,0x03		;8a91 Wait 3ms
	call sleep		;8a93
.gcn_key_wait:
	in a,(iostatus2)	;8a96 See if there's a keystroke
	bit 6,a			;8a98
	jr z,.gcn_key_wait	;8a9a Wait til there is one
	ld a,ionormal|io_keyL	;8a9c Get low nibble from 8035
	call write_ioctl	;8a9e
	in a,(iostatus2)	;8aa1
	and is2_data		;8aa3
	ld c,a			;8aa5 
	ld a,ionormal|io_keyH	;8aa6 Get high nibble
	call write_ioctl	;8aa8
	in a,(iostatus2)	;8aab
	add a,a			;8aad Make them a real boy
	add a,a			;8aae
	add a,a			;8aaf
	add a,a			;8ab0
	add a,c			;8ab1
	cp 0xff			;8ab2 If key is FF, issue last pressed key
	jr z,.gcn_key_repeat	;8ab4
	ld (keybuffer),a	;8ab6 Otherwise store this one
.gcn_key_repeat:
	ld a,(keybuffer)	;8ab9 Load last pressed key
.gcn_got_input:
	ld b,a			;8abc
	ld a,ionormal		;8abd Clear keyH command
	out (ioctl),a		;8abf
	ld a,0x03		;8ac1 Snooze
	call sleep		;8ac3
	ld a,b			;8ac6
	cp 0x03			;8ac7 Oh yeah, was that ctrl-C?
	jp z,monitor		;8ac9 If so, back to the monitor
	ret			;8acc
.gcn_sio_read:
	call sio_read		;8acd Grab one off the wire
	jr .gcn_got_input	;8ad0 And continue like we got a key

; Display a character.  Thin wrapper around the ROM video driver.
disp_char:
	ld a,b			;8ad2
	exx			;8ad3 Don't trash quite so many registers
	ld ix,vdd		;8ad4 Put video driver data block in IX
	ld hl,.dc_ret		;8ad8 Make sure driver comes back to us
	ld (vdd+vdb_ret),hl	;8adb
	jp video_driver		;8ade And go
.dc_ret:
	exx			;8ae1 Spin the bookcase upon its return
	ret			;8ae2

; Write B to SIO board in slot 3.
sio_write:
	in a,(slot3|sioctl)	;8ae3 See if the write buffer is open
	and sio_txrdy		;8ae5
	jr z,sio_write		;8ae7 Wait until it is
	ld a,b			;8ae9 
	out (slot3|siodata),a	;8aea Write a byte
	ret			;8aec

; Read from SIO board in slot 3 and return data in A.
sio_read:
	in a,(slot3|sioctl)	;8aed See if a byte is available
	and sio_rxrdy		;8aef
	jr z,sio_read		;8af1 Loop until one is
	in a,(slot3|siodata)	;8af3 Read a byte
	ret			;8af5

; Reset SIO board in slot 3.  Leaves sioctl port in C.
init_sio:
	ld hl,sio_ctlsetup_start;8af6 Load our USART reset sequence
	ld bc,0x0331		;8af9 slot3|sioctl
	otir			;8afc Write 3 bytes
.init_sio_delay:
	djnz .init_sio_delay	;8afe Wait briefly
	out (slot3|siobps),a	;8b00 Write A to baud register
	ret			;8b02

; This loop detects cards in all slots, and builds a list of numbers to dial
; in a RAM-based enumeration table.
enumerate:
	ld de,enumtable_bottom	;8b03 This is a pile of program counters
	ld c,slot6id		;8b06
	ld b,0x06		;8b08 Six slots
.enum_outerloop:
	push bc			;8b0a Hang onto B and C for a minute
	in b,(c)		;8b0b See what port C (slotid) has to say
	ld hl,card_ids_start	;8b0d Start at the top of our ID table
.enum_findcard:
	ld a,(hl)		;8b10 Get ID byte from table
	inc hl			;8b11  (and increment)
	cp 0xff			;8b12 FF is a wildcard, always match it
	jr z,.enum_gotmatch	;8b14
	cp b			;8b16 If ID matches exactly, that's good too
	jr z,.enum_gotmatch	;8b17
	inc hl			;8b19 Otherwise advance to next table row
	inc hl			;8b1a
	jr .enum_findcard	;8b1b Loop to match next row
.enum_gotmatch:
	pop bc			;8b1d Get our counter and slotid port back
	ld a,(hl)		;8b1e Transfer the associated pointer from
	ld (de),a		;8b1f ROM table into enumeration table,
	inc de			;8b20 and advance to next row of both
	inc hl			;8b21
	ld a,(hl)		;8b22
	ld (de),a		;8b23
	inc de			;8b24  
	inc hl			;8b25 (this one not actually necessary)
	inc c			;8b26 Increment slotid port
	djnz .enum_outerloop	;8b27 Loop to detect next card
	ld hl,last_task		;8b29 Our 7th task checks for keypresses
	ex de,hl		;8b2c It's stored into enumtable right here
	ld (hl),e		;8b2d
	inc hl			;8b2e
	ld (hl),d		;8b2f

	ld de,enumtable_bottom	;8b30 
	ld ix,rom_stacks_start	;8b33
	ld iy,enum_taskptrs	;8b37
	ld b,0x07		;8b3b
.enum_makestacks:
	ld l,(ix+0x00)		;8b3d Set up each task's stack pointer
	ld (iy+0x00),l		;8b40 by copying it from ROM to RAM
	inc ix			;8b43
	inc iy			;8b45
	ld h,(ix+0x00)		;8b47
	ld (iy+0x00),h		;8b4a
	inc ix			;8b4d
	inc iy			;8b4f
	ld a,(de)		;8b51 And write the card-specific pointers
	ld (hl),a		;8b52 into the top of each task's stack, so
	inc hl			;8b53 a ret will launch them.
	inc de			;8b54
	ld a,(de)		;8b55
	ld (hl),a		;8b56
	inc de			;8b57
	djnz .enum_makestacks	;8b58 Loop for all 7 tasks.
	xor a			;8b5a Then select task 0
	call .tns_skip		;8b5b and launch.

; The guts of alt_tab_full, which selects the next card to give time
; to.  It's a little hairy, but the gist is there's seven different call
; stacks, one for each task, and the enum_taskptr array contains the current
; stack pointers for each of them.
next_task:
	ld ix,(current_SP_ptr)	;8b5e Current task's SP save spot into IX
	ld hl,0x0000		;8b62
	add hl,sp		;8b65 Current stack pointer into HL
	ld (ix+0x00),l		;8b66 Put our current SP back into the 
	inc ix			;8b69 appropriate taskptr entry
	ld (ix+0x00),h		;8b6b
	ld a,(selected_slot)	;8b6e Increment our slot number
	inc a			;8b71
	cp 0x07			;8b72
	jp c,.tns_skip		;8b74 
	xor a			;8b77 Zero if new slot is >6
.tns_skip:
	ld (selected_slot),a	;8b78 Write back new slot number
	ld hl,enum_taskptrs	;8b7b Select pointer from the task list
	add a,a			;8b7e adding 16 bit
	add a,l			;8b7f  number to an
	ld l,a			;8b80    8 bit number
	adc a,h			;8b81     is super fun
	sub l			;8b82       times I have
	ld h,a			;8b83         heard from friends
	ld (current_SP_ptr),hl	;8b84 Get our new task's SP out of the list
	ld a,(hl)		;8b87 
	inc hl			;8b88
	ld h,(hl)		;8b89
	ld l,a			;8b8a
	ld sp,hl		;8b8b Then point SP to it and git
	ret			;8b8c Which returns to the next task already
				;     in progress

; There is a task running for each card slot, and this routine is called to
; yield to the next task, with a separate stack area for each to hold that
; task's context.  This routine stores the current context and switches to
; the next one.  (Actually there are 6 tasks and an opportunity for rest on
; the 7th, as God intended.)
alt_tab_full:
	push af			;8b8d Store current context
	push bc			;8b8e
	push de			;8b8f
	push hl			;8b90
	call next_task		;8b91 Switch tasks
	pop hl			;8b94 Restore new context
	pop de			;8b95
	pop bc			;8b96
	pop af			;8b97
	ret			;8b98

; This yields without saving registers, for the two routines that don't use
; them.
alt_tab_lite:
	call next_task		;8b99 
	ret			;8b9c

; This is a block of card IDs and jump addresses for auto detection of
; possible boot methods.
card_ids_start:
	defb id_sio		;8b9d SIO card
	defw card_init_serial	;8b9e
	defb id_net		;8ba0 Network card
	defw card_init_net	;8ba1
	defb 0xff		;8ba3 Other card or empty slot
	defw card_init_other	;8ba4

; These are the starting values for each task's stack pointer.
rom_stacks_start:
	defw 0xc0a2		;8ba6
	defw 0xc124		;8ba8
	defw 0xc1a6		;8baa
	defw 0xc228		;8bac
	defw 0xc2aa		;8bae
	defw 0xc32c		;8bb0
	defw 0xc3ae		;8bb2
	defw 0xc430		;8bb4

; After POST but before it starts cycling through looking for expansion
; cards to boot from, it takes a crack at booting straight from the default
; floppy drive, rather than waiting to be asked.
auto_loadsystem:
	ld hl,herald_start	;8bb6 Show load system prompt
	call disp_str		;8bb9
	call disp_newline	;8bbc
	ld a,(post_status)	;8bbf See if we had any POST errors
	or a			;8bc2
	jr z,.auto_try_floppy	;8bc3 Jump straight to floppy boot if none
	ld b,a			;8bc5 Otherwise, mention there is trouble
	call disp_binary	;8bc6
	ld hl,failstring_start	;8bc9 'Hardware failure'
	call disp_str		;8bcc
	call disp_newline	;8bcf
.auto_try_floppy:
	call char_ready		;8bd2 Drop to old prompt if a key is waiting
	jp nz,keypress_abort	;8bd5
	ld a,ionormal|io_motors	;8bd8 Start the motors
	out (ioctl),a		;8bda
	ld a,(post_status)	;8bdc 
	and 0xf2		;8bdf Check for bad RAM or bad 8035
	ld a,(post_parity)	;8be1
	jp nz,.auto_nofloppy	;8be4 Don't try a floppy boot in that case 
	ld (fd_error),a		;8be7 Store POST parity byte back to fd_error
	exx			;8bea
	ld c,fdc_drive1		;8beb Select first drive
	exx			;8bed
	call find_track0	;8bee Try to get track 0
	jr nz,.auto_track0	;8bf1
	ld a,(fd_error)		;8bf3 Failed to get it, so set bit 4 of
	or 0x10			;8bf6 fd_error (but still try to continue)
	ld (fd_error),a		;8bf8
.auto_track0:
	exx			;8bfb
	ld b,0x0a		;8bfc Ten tries to read this disk
	exx			;8bfe
.auto_retry_top:
	ld a,0xc8		;8bff Wait 200ms
	call sleep		;8c01
	call char_ready		;8c04 Bail if a keypress is waiting
	jr z,.auto_find_sector4	;8c07 Otherwise start reading disk
keypress_abort:
	ld a,(sio_absent)	;8c09 Restore our SIO test result to B
	ld b,a			;8c0c
	xor a			;8c0d Clear out A to avoid re-printing herald
	jp loadsystem		;8c0e Depart for manual load system prompt
.auto_find_sector4:
	ld a,ionormal		;8c11 Clear out 8035 command
	call write_ioctl	;8c13
	in a,(fdd_clearread)	;8c16 Clear disk read flag
	ld a,0x03		;8c18 Find sector 4
	call find_sector	;8c1a
	jr c,.auto_start_read	;8c1d If we found it, git it
	ld a,ionormal|io_motors	;8c1f Otherwise keep disks spinning
	call write_ioctl	;8c21
	ld a,(fd_error)		;8c24 But set bit 3 of fd_error
	or 0x08			;8c27
	jr .auto_retry		;8c29 And set up to try again.
.auto_start_read:
	ex af,af'		;8c2b
	ld a,004h		;8c2c A' is the number of sectors to read
	ex af,af'		;8c2e
	ld e,0x00		;8c2f Buffer location is xx00
	ld b,0x00		;8c31 Word read counter
	call read_sectorid	;8c33 Read sector ID byte
	jr nc,.auto_get_bufaddr	;8c36 Jump if we got it
.auto_badsecid:
	ld a,(fd_error)		;8c38 Set bit 2 of fd_error if read failed
	or 0x04			;8c3b 
	jr .auto_retry		;8c3d And retry
.auto_get_bufaddr:
	in a,(fdd_data)		;8c3f First byte of sector 4 is buffer
	cp 0xd0			;8c41 origin high byte
	jr c,.auto_badstart	;8c43 Unlike manual boot, buffer must start
	cp 0f9h			;8c45 between D000-F800, not C000-F800
	jr nc,.auto_badstart	;8c47
	ld d,a			;8c49 We got a good number, set up DE
	ld (de),a		;8c4a 
	inc de			;8c4b
	rlca			;8c4c Store CRC
	ld c,a			;8c4d
	in a,(fdd_data)		;8c4e Second byte in sector has to be 0x76,
	cp 0x76			;8c50 which is not checked in manual boot
	jr z,.auto_get_sector	;8c52 
.auto_badstart:
	ld a,(fd_error)		;8c54 Bad start address, set bit 1 of
	or 0x02			;8c57 fd_error
	jr .auto_retry		;8c59 And retry
.auto_get_sector:
	ld (de),a		;8c5b Store second byte
	inc de			;8c5c
	xor c			;8c5d Update CRC
	rlca			;8c5e
	ld c,a			;8c5f
	dec b			;8c60 B=FF for 254 byte read
.auto_get_nextsector:
	call read_sectordata	;8c61 Get the sector contents
	in a,(fdd_data)		;8c64 Followed by a CRC
	xor c			;8c66 
	in a,(fdd_clearread)	;8c67 Clear read flag
	jr z,.auto_crc_ok	;8c69 And advance if CRC is OK
	ld a,(fd_error)		;8c6b Otherwise set bit 0 of fd_error
	or 0x01			;8c6e
	jr .auto_retry		;8c70 And retry
.auto_crc_ok:
	ex af,af'		;8c72
	dec a			;8c73 Decrement A' for every success
	jr z,.auto_read_complete;8c74 And stop looping if we got them all
	ex af,af'		;8c76
	call read_sectorid	;8c77 Otherwise read next sector header
	jr c,.auto_badsecid	;8c7a and loop or error as appropriate
	jr .auto_get_nextsector	;8c7c 
.auto_read_complete:
	ld hl,0xf80a		;8c7e Move cursor to buffer start+10
	add hl,de		;8c81
	ld a,(hl)		;8c82
	cp 0xc3			;8c83 Is there a jump instruction there? 
	jr z,.auto_strapped	;8c85 Yes, time to bounce 
	ld a,(fd_error)		;8c87 Otherwise set bit 1 of fd_error
	or 0x02			;8c8a
	jr .auto_retry		;8c8c And retry
.auto_strapped:
	xor a			;8c8e Bank main RAM page 0 into
	out (set_bank0),a	;8c8f 0000-3FFF and 4000-7FFF
	out (set_bank1),a	;8c91
	jp (hl)			;8c93 Boot
.auto_retry:
	ld (fd_error),a		;8c94 Store error code
	exx			;8c97
	dec b			;8c98 Decrement tries
	exx			;8c99
	jr z,.auto_nofloppy	;8c9a Out of tries, try to boot from cards
	jp .auto_retry_top	;8c9c 
.auto_nofloppy:
	ld (post_parity),a	;8c9f Store combined parity/floppy error
	jp enumerate		;8ca2 And jump to new card boot routine

; Display a null-terminated string.
disp_str:
	ld a,(hl)		;8ca5
	or a			;8ca6 Is it 0?
	ret z			;8ca7 Yes, bail
	ld b,a			;8ca8
	call disp_chartx	;8ca9 Otherwise display char
	inc hl			;8cac Increment and loop
	jr disp_str		;8cad

; BLOCK 'failstring' (start 0x8caf end 0x8cc2)
failstring_start:
	defb "  Hardware failure", 0

; Mini monitor disk diagnostics write test routine.  This writes a test
; value to sector 2 of the current track and reads it back to verify,
; repeatedly, until a keystroke interrupts it or an error happens.  It
; doesn't advance tracks or sectors, but does write to both sides of the
; disk, and tests a new value with every pass.
.ma_keyctrlW:
	ld a,e			;8cc2
	cp 0x15			;8cc3 See if we're on a track needing precomp
	ld a,d			;8cc5
	jr c,.ma_kcW_precomp_off;8cc6 No, skip ahead
	or fdc_precomp		;8cc8 Yes, set precompensation bit
	jr .ma_kcW_precomp_on	;8cca
.ma_kcW_precomp_off:
	and ~fdc_precomp	;8ccc Clear compensation bit
.ma_kcW_precomp_on:
	ld d,a			;8cce
	ld a,ionormal|io_motors	;8ccf Motors on
	call write_ioctl	;8cd1
	ld a,ionormal		;8cd4 View sector
	call write_ioctl	;8cd6
	ld b,0x19		;8cd9 Cursor off, newline
	call disp_char		;8cdb
	call disp_newline	;8cde
	ld a,ionormal|io_motors	;8ce1 Motors on
	call write_ioctl	;8ce3
	in a,(fdd_clearread)	;8ce6
	xor a			;8ce8 Test value starts at 0
	ld (fd_testval),a	;8ce9 
.ws_breakpoint:
	ld a,0x02		;8cec Wait 2ms
	call sleep		;8cee
	ld a,ionormal		;8cf1 View sector
	out (ioctl),a		;8cf3
	call char_ready		;8cf5 Is there a keypress?
	jr z,write_sector	;8cf8 No, keep going
	call get_char_noecho	;8cfa Yes, is it a Q?
	cp 'Q'			;8cfd
	jr nz,write_sector	;8cff No, keep going
	call disp_newline	;8d01 Yes, bail back to monitor
	ret			;8d04
write_sector:
	in a,(iostatus1)	;8d05 First see if disk is write protected
	and is1_protect		;8d07
	jr z,.ws_find_sec2	;8d09 No, keep going
	ld a,'6'		;8d0b Yes, show error and bail completely
	jp .ws_ded		;8d0d
.ws_find_sec2:
	ld a,0x01		;8d10
	push de			;8d12
	call find_sector	;8d13 Find sector 2
	pop de			;8d16
	jr c,.ws_wait_markhigh	;8d17 Success
	ld a,'4'		;8d19 Fail, no disk probably
	jp .ws_ded		;8d1b
.ws_wait_markhigh:
	in a,(iostatus1)	;8d1e Wait for sector mark high
	and is1_mark		;8d20
	jr nz,.ws_wait_markhigh	;8d22
.ws_wait_marklow:
	in a,(iostatus1)	;8d24 Then wait for sector mark low
	and is1_mark		;8d26
	jr z,.ws_wait_marklow	;8d28
	out (fdd_setwrite),a	;8d2a Set disk write flag
	ld b,0x21		;8d2c
	xor a			;8d2e
.ws_write_preamble:
	out (fdd_data),a	;8d2f Write 33 zeroes
	djnz .ws_write_preamble	;8d31
	ld a,0xfb		;8d33 And a sync byte
	out (fdd_data),a	;8d35
	ld a,0x02		;8d37 Sector ID byte
	out (fdd_data),a	;8d39
	ld b,0x00		;8d3b 512 bytes to write
	ld c,b			;8d3d Clear C also
.ws_write_data:
	ld a,(fd_testval)	;8d3e Write our test value
	out (fdd_data),a	;8d41
	xor c			;8d43 And calculate CRC
	rlca			;8d44
	ld c,a			;8d45
	ld a,(fd_testval)	;8d46 Write our test value
	out (fdd_data),a	;8d49
	xor c			;8d4b And calculate CRC
	rlca			;8d4c
	ld c,a			;8d4d
	djnz .ws_write_data	;8d4e Loop til we're done
	out (fdd_data),a	;8d50 Write CRC
	ld b,'w'		;8d52 And tell the user we did it
	call disp_chartx	;8d54
	ld b,0x0d		;8d57
	call disp_chartx	;8d59
	in a,(fdd_clearread)	;8d5c Clear read flag
	ld a,0x01		;8d5e Look for the sector we just wrote
	push de			;8d60
	call find_sector	;8d61
	pop de			;8d64
	jr c,.ws_read_preamble	;8d65 Did we find it?
	ld a,'4'		;8d67 No, show error
	jr .ws_ded		;8d69
.ws_read_preamble:
	ld b,0x00		;8d6b
	call read_sectorid	;8d6d
	jr nc,.ws_good_preamble	;8d70 Did we get a sector ID?
	ld a,'1'		;8d72 No, show error
	jr .ws_ded		;8d74
.ws_good_preamble:
	ld a,(fd_testval)	;8d76 Put our test byte in H
	ld h,a			;8d79
.ws_read_data:
	in a,(fdd_data)		;8d7a See if we read that byte back
	cp h			;8d7c
	jr nz,.ws_bad_data	;8d7d Nope, error
	xor c			;8d7f Yes, calculate CRC
	rlca			;8d80
	ld c,a			;8d81
	in a,(fdd_data)		;8d82 Read again
	cp h			;8d84 Test again
	jr nz,.ws_bad_data	;8d85 Bail if needed
	xor c			;8d87 Calculate CRC again
	rlca			;8d88
	ld c,a			;8d89
	djnz .ws_read_data	;8d8a
	jr .ws_check_crc	;8d8c Made it, skip past error
.ws_bad_data:
	ld a,'3'		;8d8e Bad data received, show error
	jr .ws_ded		;8d90
.ws_check_crc:
	in a,(fdd_data)		;8d92 Get CRC byte and check it
	xor c			;8d94
	jr z,.ws_good_crc	;8d95 Good
	ld a,'2'		;8d97 No good, show error
	jr .ws_ded		;8d99
.ws_good_crc:
	in a,(fdd_clearread)	;8d9b Clear read flag
	ld b,'r'		;8d9d Write a cheerful character to screen
	call disp_chartx	;8d9f
	ld b,0x0d		;8da2
	call disp_chartx	;8da4
	jr .ws_notded		;8da7 Skip to loop
.ws_ded:
	call .ws_disperror	;8da9 We died, print an error
	ld a,ionormal|io_motors	;8dac Keep disks spinning
	out (ioctl),a		;8dae
.ws_notded:
	ld a,d			;8db0 Switch sides
	xor fdc_side		;8db1
	ld d,a			;8db3
	out (fdd_ctl),a		;8db4
	and fdc_side		;8db6 
	jr nz,.ws_next		;8db8 Now try next side
	ld hl,fd_testval	;8dba
	inc (hl)		;8dbd Increment the test byte to write a
				;     different pattern next time
.ws_next:
	jp .ws_breakpoint	;8dbe And loop
.ws_disperror:
	push de			;8dc1 Save our sundries
	push af			;8dc2
	call disp_newline	;8dc3
	ld b,'T'		;8dc6 T is for trouble
	call disp_chartx	;8dc8
	ld b,' '		;8dcb Space
	call disp_chartx	;8dcd
	pop af			;8dd0
	ld b,a			;8dd1
	call disp_chartx	;8dd2 Particular error character
	ld b,' '		;8dd5 Space
	call disp_chartx	;8dd7
	ld b,d			;8dda XXX 
	call disp_hex16		;8ddb XXX
	ld a,(fd_testval)	;8dde XXX Not sure what this is printing
	ld b,a			;8de1 XXX
	call disp_hex16		;8de2 XXX
	call disp_newline	;8de5
	ld a,0xfa		;8de8 Wait 250ms
	call sleep		;8dea
	pop de			;8ded Recall DE
	ret			;8dee

; Non-detected card task.  This just passes to the next
; task every time.
card_init_other:
	call alt_tab_lite	;8def Just yield, don't bother with registers
	jr card_init_other	;8df2 And loop


; Final task, run after all the card tasks have had a chance to do their
; thing.
last_task:
	call char_ready		;8df4 See if a keystroke is ready
	jp nz,keypress_abort	;8df7 Stop this nonsense if so
	call alt_tab_lite	;8dfa Otherwise yield
	jr last_task		;8dfd (and loop)

; Serial boot task.  One copy of this runs for every SIO in the system, so
; any will work, and this sets up the card in a much friendlier async mode
; vs the 2K ROM or the old serial boot routine (pressing S from old load
; system prompt).
card_init_serial:
	ld hl,sio_ctlsetup_start;8dff Write 3 reset bytes to USART
	ld c,sioctl		;8e02
	call qualify_port	;8e04 Combine current slot and port number
	ld b,0x03		;8e07
	otir			;8e09
.cis_delay:
	djnz .cis_delay		;8e0b Wait a moment
	ld c,siobps		;8e0d
	call qualify_port	;8e0f
	ld a,0x7e		;8e12 Set USART to 9600bps
	out (c),a		;8e14
	ld c,sioctl		;8e16
	call qualify_port	;8e18
	ld a,0xce		;8e1b Set USART to 8-N-2 async
	out (c),a		;8e1d
	ld a,0x37		;8e1f Send command byte
	out (c),a		;8e21
	ld c,siodata		;8e23
	call qualify_port	;8e25
	in a,(c)		;8e28 Read two junk bytes
	in a,(c)		;8e2a
.cis_rx_header:
	call read_next_sio	;8e2c Get a byte for real
.cis_rx_headerretry:
	cp 0x10			;8e2f Is it DLE?
	jr nz,.cis_rx_header	;8e31
	call read_next_sio	;8e33
	cp 0x16			;8e36 Is the next one SYN?
	jr nz,.cis_rx_headerretry;8e38
	call read_next_sio	;8e3a
	cp 0x05			;8e3d Is the next one ENQ?
	jr nz,.cis_rx_headerretry;8e3f
	call read_next_sio	;8e41 The next one is PAD, don't care
.cis_rx_retry:
	ld hl,async_tx_header_start;8e44
	call .tx_string		;8e47 Send our traditional hello,
	ld a,(post_status)	;8e4a and also POST status,
	call write_next_sio	;8e4d
	ld a,(post_parity)	;8e50 parity status,
	call write_next_sio	;8e53
	ld a,(current_bank)	;8e56 current memory block,
	call write_next_sio	;8e59
	ld a,0x05		;8e5c then ENQ,
	call write_next_sio	;8e5e
	ld a,0xff		;8e61 then PAD
	call write_next_sio	;8e63
.cis_bufferinit:
	call read_next_sio	;8e66 Whatcha say about that, pal?
	cp 0x02			;8e69 STX marks the beginning of our thing
	jr nz,.cis_bufferinit	;8e6b
	call read_anysio	;8e6d From here, commit to just this SIO.
	ld d,a			;8e70 Also we just got the high byte for
	ld e,0x00		;8e71 our buffer, to go in DE as usual.
	ld b,e			;8e73
	ld ix,0x000a		;8e74 IX is our boot point again
	add ix,de		;8e78
	ld hl,0x0001		;8e7a And HL is our checksum
.cis_rx_next:
	ld (de),a		;8e7d Store byte into buffer
	inc de			;8e7e
	ld c,a			;8e7f Do the checksum
	add hl,bc		;8e80 
.cis_rx_skip:
	call read_anysio	;8e81 Read next byte
	cp 0x10			;8e84 Drop a single DLE
	jr nz,.cis_rx_next	;8e86
	call read_anysio	;8e88 Read another byte after DLE
	cp 0x16			;8e8b Is it now DLE, SYN?
	jr z,.cis_rx_skip	;8e8d Yes, drop it
	cp 0x03			;8e8f Is it DLE, ETX?
	jr nz,.cis_rx_next	;8e91 No, store and continue reading
	call read_anysio	;8e93 Otherwise we're done, test checksum L
	cp l			;8e96 
	jr nz,.cis_rx_retry	;8e97 Fail, back to the top
	call read_anysio	;8e99 And checksum H
	cp h			;8e9c
	jr nz,.cis_rx_retry	;8e9d Fail, back to the top
	call read_anysio	;8e9f Success: read one more byte
	xor a			;8ea2
	out (set_bank0),a	;8ea3 Main ram bank 0 in 0000-3FFF
	out (set_bank1),a	;8ea5 Main ram bank 1 in 4000-7FFF
	jp (ix)			;8ea7 Boot

; Send a null-terminated string over any SIO.
.tx_string:
	ld a,(hl)		;8ea9
	inc hl			;8eaa
	or a			;8eab Is it 0?
	ret z			;8eac Yes, bail
	call write_next_sio	;8ead No, write character and loop
	jr .tx_string		;8eb0

; Yield to next task, then try to get a serial byte from slot in A.
read_next_sio:
	call alt_tab_full	;8eb2 First let everyone else have a turn
	ld c,sioctl		;8eb5 Then see if there's a byte waiting
	call qualify_port	;8eb7
	in a,(c)		;8eba
	and sio_rxrdy		;8ebc
	jr z,read_next_sio	;8ebe No, yield again
	ld c,siodata		;8ec0 Yes, fetch it and return
	call qualify_port	;8ec2
	in a,(c)		;8ec5
	ret			;8ec7

; Listen more attentively to a single serial port.
read_anysio:
	ld c,sioctl		;8ec8 Get our full port number
	call qualify_port	;8eca
.ras_wait:
	in a,(c)		;8ecd See if there's a byte waiting
	and sio_rxrdy		;8ecf
	jr z,.ras_wait		;8ed1 No, poll some more
	ld c,siodata		;8ed3 Yes, read it and return
	call qualify_port	;8ed5
	in a,(c)		;8ed8
	ret			;8eda

; For the multitasking routines, this fetches the slot number for the
; current context and forms a port number, using the slot as the high nibble
; and returning it combined with whatever was already in C.
qualify_port:
	ld a,(selected_slot)	;8edb Grab current slot number
	add a,a			;8ede Rotate it into the high nibble
	add a,a			;8edf
	add a,a			;8ee0
	add a,a			;8ee1
	or c			;8ee2 Combine with C
	ld c,a			;8ee3 And return the whole thing in C
	ret			;8ee4

; Yield to next task, then write the byte in A to an SIO.
write_next_sio:
	ld b,a			;8ee5 Save TX byte in B
	ld c,sioctl		;8ee6 And qualify our status port
	call qualify_port	;8ee8
.wns_wait:
	call alt_tab_full	;8eeb Yield,
	in a,(c)		;8eee then see if we can send
	and sio_txrdy		;8ef0 
	jr z,.wns_wait		;8ef2 And loop if not
	ld c,siodata		;8ef4 Buffer empty, so write our byte
	call qualify_port	;8ef6
	ld a,b			;8ef9
	out (c),a		;8efa
	ret			;8efc

; This is the first part of the response transmitted over serial when a
; suitable hello has been sent by the other computer.  It's basically the
; same as the 2K ROM version, but sent with a routine that understands null
; terminated strings.
async_tx_header_start:
	defb 0x10, 0x16, 0x04, 0x01, 0x00


; Network boot.  This section's commentary is conjecture, but I think it's
; clear what some of it is doing at least.
net_packet:	equ 0xc433
net_data:	equ slot5|0	; Data port
net_status:	equ slot5|1	; Status port
netstat_rxrdy:	equ 0x01	; Data in receive buffer

card_init_net:
	ld bc,0x0007		;8f02 Copy header into RAM buffer
	ld hl,net_header_start	;8f05
	ld de,net_packet	;8f08
	ldir			;8f0b
	ld a,0x01		;8f0d Assemble some extra items:
	ld (net_packet+7),a	;8f0f One
	ld a,(post_status)	;8f12
	ld (net_packet+8),a	;8f15 Post status
	ld a,(post_parity)	;8f18
	ld (net_packet+9),a	;8f1b Post parity
	ld a,(current_bank)	;8f1e
	ld (net_packet+0xa),a	;8f21 Current RAM bank
	ld a,0x10		;8f24 Now tell the card: "Ten! Hex!"
	call .net_tx_cmd	;8f26
	call .net_packet_ready	;8f29 And see what it has to say
.net_retry:
	jr nz,.net_packet_nak	;8f2c 
	cp 0x01			;8f2e If card originally said FE,
	jr z,.net_packet_ack	;8f30 that's apparently what we need
.net_packet_nak:
	ld a,0x2c		;8f32 Now say "Two cee!" and see
	call .net_tx_cmd	;8f34 Maybe a re-send request?
	call .net_packet_ready	;8f37 And grab another acknowledgement
	jr .net_retry		;8f3a
.net_packet_ack:
	in a,(net_data)		;8f3c Now store back a byte from card
	ld (net_packet+2),a	;8f3e
	in a,(net_data)		;8f41 And another one
	ld (net_packet+4),a	;8f43 
	call .net_clearrx	;8f46 Clear receive buffer
	call .net_send_buffers	;8f49 And send some data
	call .net_packet_ready	;8f4c Did it work?
	jr nz,.net_retry	;8f4f
	cp 0x06			;8f51 Did we get back F9? 
	jr nz,.net_retry	;8f53 No, that's too bad
	ld hl,0xfde6		;8f55 Set up a read from here...
	ld bc,0x021a		;8f58 ...to top of RAM
.net_rx_andbounce:
	in a,(net_data)		;8f5b And read in 538 bytes from data port
	ld (hl),a		;8f5d
	inc hl			;8f5e
	dec bc			;8f5f
	ld a,b			;8f60
	or c			;8f61
	jr nz,.net_rx_andbounce	;8f62
	call .net_clearrx	;8f64 Wait for card to settle
	jp 0xfe00		;8f67 And boot
.net_send_buffers:
	ld a,0x0a		;8f6a Issue command 0x0A
	call .net_tx_cmd	;8f6c
	call .net_rxwait	;8f6f Wait til we get a response
	ld hl,net_packet	;8f72 Send our six bytes
	ld b,0x06		;8f75
	ld c,0x10		;8f77
	otir			;8f79
	call .net_clearrx	;8f7b Throw away the response
	ld a,0x0b		;8f7e Issue command 0x0B
	call .net_tx_cmd	;8f80
	call .net_rxwait	;8f83 Wait again for a response
	ld hl,net_packet+5	;8f86
	ld b,0x0b		;8f89 Send 11 more bytes
	ld c,0x10		;8f8b
	otir			;8f8d
	call .net_clearrx	;8f8f Throw away that response too
	ret			;8f92
.net_packet_ready:
	ld a,0x13		;8f93 "Thirteen?  Hex?" we inquire
	call .net_tx_cmd	;8f95
.npr_wait:
	call .net_getstatus	;8f98
	and 0x04		;8f9b Loiter til bit 2 goes high
	jr z,.npr_wait		;8f9d
	ld a,0x02		;8f9f Issue command 0x02
	call .net_tx_cmd	;8fa1 
	call .net_rxwait	;8fa4
	in a,(net_data)		;8fa7 Get a data byte
	ld b,a			;8fa9
	in a,(net_data)		;8faa Get a data byte
	cpl			;8fac
	cp b			;8fad Set flags for ~A-B
	ret			;8fae
.net_clearrx:
	in a,(net_data)		;8faf Discard data
	in a,(net_status)	;8fb1 And see if buffer is empty
	and netstat_rxrdy	;8fb3
	jr nz,.net_clearrx	;8fb5 Wait until it is
	ret			;8fb7
.net_rxwait:
	call .net_getstatus	;8fb8 Check receive buffer flag
	and netstat_rxrdy	;8fbb
	jr z,.net_rxwait	;8fbd Wait until there's data 
	ret			;8fbf
.net_getstatus:
	call alt_tab_full	;8fc0 Yield to next card
	in a,(net_status)	;8fc3 Then fetch status byte
	ret			;8fc5
.net_tx_cmd:
	push af			;8fc6 
	call .net_getstatus	;8fc7 First get status
	and 0x02		;8fca Just bit 1 please
	ld b,a			;8fcc Save that in B
	pop af			;8fcd
	out (net_status),a	;8fce In either case transmit
.net_tx_wait:
	call .net_getstatus	;8fd0 And check status bit again
	and 0x02		;8fd3
	cp b			;8fd5
	jr z,.net_tx_wait	;8fd6 Wait here for bit to complement
	ret			;8fd8

; This is copied to RAM and later transmitted as part of a larger block of
; data during the network boot sequence.
net_header_start:
	defb 0x06, 0x00, 0x00, 0x01, 0x00, 0x08, 0xf7

; The cold start routine launches the POST here.

post_entrypoint:
	in a,(slot6id)		;8fe0 Check slot 6 for an HDD controller
	xor id_hdc		;8fe2
	jr nz,.post_nohdd	;8fe4 Skip if there isn't one,
	out (hdc_ctl),a		;8fe6 otherwise say hello to it with a 0
.post_nohdd:
	jp post_io		;8fe8 And jump to I/O test.

; BLOCK 'version' (start 0x8feb end 0x9000)
	defs 15
	defb "REV-BK"
