
; NORTH STAR ADVANTAGE
; ##############################################
; Popular ports and nice numbers
;
; Compiled by ian Butler, Aug 2023, no revisions


; Expansion slots.  There are six.
slot6:		equ 0x00
slot5:		equ 0x10
slot4:		equ 0x20
slot3:		equ 0x30
slot2:		equ 0x40
slot1:		equ 0x50

; RAM parity (says main but actually everything.)
parity_status:	equ 0x60	; On input
parity_ctl:	equ 0x60	; On output

; Parity control bits
parity_int:	equ 0x01	; Enable parity interrupt
parity_clr:	equ 0x02	; Clear parity error flag

; Parity status bit
parity_error:	equ 0x01	; Other bits are 'indeterminate'

; Board ID ports.  Bit 3 is ignored, so 76/77 are invalid and 78-7F repeat.
slot6id:	equ 0x70
slot5id:	equ 0x71
slot4id:	equ 0x72
slot3id:	equ 0x73
slot2id:	equ 0x74
slot1id:	equ 0x75

; Board ID values.
id_sio:		equ 0xF7	; Serial card
id_pio:		equ 0xDB	; Parallel card
id_hdc:		equ 0xBF	; Hard disk controller
				; Possibly also BE, but probably not.
id_net:		equ 0xDF	; Network card
id_fpb:		equ 0x7F	; Floating point board (probably apocryphal)

; Floppy control ports.  Bits 2 and 3 are ignored, 84-8F repeat.
fdd_data:	equ 0x80
fdd_syncbyte:	equ 0x81	; On input
fdd_ctl:	equ 0x81	; On output
fdd_clearread:	equ 0x82	; On input
fdd_setread:	equ 0x82	; On output
fdd_setwrite:	equ 0x83	; On output

; Expected sync byte from a floppy read.
fdd_goodsync:	equ 0xfb

; Useful bits for fdd_ctl register.
fdc_side:	equ 0x40	; Disk side select
fdc_in:		equ 0x20	; Step in when 1, out when 0
fdc_precomp:	equ 0x20	; Also precomp when writing
fdc_step:	equ 0x10	; Step pulse
fdc_drive4:	equ 0x08	; Drive 3 select
fdc_drive3:	equ 0x04	; Drive 2 select
fdc_drive2:	equ 0x02	; Drive 1 select
fdc_drive1:	equ 0x01	; Drive 0 select

; Terminal stuff.
sysbeep:	equ 0x83	; Beep on input
video_start:	equ 0x90	; Start line for screen draws
				; 90-9F all valid.
clear_vsync:	equ 0xB0	; Clear display vsync flag.
				; B0-Bf all valid.

; Memory management.  Bits 2 and 3 are ignored, 84-8F repeat.
set_bank0:	equ 0xa0	; 0000-3FFF
set_bank1:	equ 0xa1	; 4000-7FFF
set_bank2:	equ 0xa2	; 8000-BFFF
set_bank3:	equ 0xa3	; C000-FFFF

; Available memory page numbers.
ram_page0:	equ 0x00	; Main memory (0xxxxNNN)
ram_page1:	equ 0x01
ram_page2:	equ 0x02
ram_page3:	equ 0x03
vram_page0:	equ 0x80	; Video RAM (1xxx00N)
vram_page1:	equ 0x81
bootprom:	equ 0xfc	; Boot ROM (1xxxx1xx)

; Interrupt stuff.
clear_nmi:	equ 0xc0	; C0-CF all valid.

; I/O and chatting with the 8035.
iostatus2:	equ 0xd0	; D0-DF all valid.
is2_data:	equ 0x0f	; Low nibble mask for data
is2_repeat:	equ 0x10	; Keyboard repeat flag
is2_overrun:	equ 0x20 	; Keyboard buffer overrun flag
is2_keydata:	equ 0x40	; Keypresses waiting flag
is2_ack:	equ 0x80	; Command acknowledge bit

iostatus1:	equ 0xe0	; E0-EF all valid.

is1_keyint:	equ 0x01	; Keyboard interrupt flag
is1_ioint:	equ 0x02	; I/O interrupt flag
is1_vsync:	equ 0x04	; Vertical scan complete flag
is1_nmi:	equ 0x08	; NMI flag
is1_protect:	equ 0x10	; Disk write protect flag
is1_track0:	equ 0x20	; Track 0 flag
is1_mark:	equ 0x40	; Sector mark flag
is1_dserial:	equ 0x80	; Disk serial data stream

; I/O commands (chiefly for 8035) and a sensible starting point.
ioctl:		equ 0xf8	; Port for these things.  F0-FF all valid.

; Shorthand for popular flags
ionormal:	equ 0x18	; I/O reset and acquire mode high

; Flags
io_vsyncint:	equ 0x80	; Enable vsync interrupt
io_click:	equ 0x40	; Speaker data
io_vblank:	equ 0x20	; Disable display
ioNreset:	equ 0x10	; Not I/O reset
ioNacquire:	equ 0x08	; Not acquire mode

; Commands
io_sector:	equ 0x00	; Show sector counter
io_keyL:	equ 0x01	; Show low nibble of keystroke
io_keyH:	equ 0x02	; Show high nibble of keystroke
io_keyint:	equ 0x03	; Toggle maskable keyboard interrupt
io_crsrlock:	equ 0x04	; Toggle cursor lock
io_motors:	equ 0x05	; Start drive motors
io_keynmi1:	equ 0x06	; Toggle keyboard NMI (aim)
io_keynmi2:	equ 0x07	; Toggle keyboard NMI (fire)


; SIO ports
siodata:	equ 0x00	; USART data port
sioctl:		equ 0x01	; USART command/mode port
siobps:		equ 0x08	; USART baud register

; SIO status bits
sio_txrdy:	equ 0x01	; Transmit buffer empty
sio_rxrdy:	equ 0x02	; Receive buffer full
sio_txe:	equ 0x04	; 
sio_parityerr:	equ 0x08	; Parity error
sio_overrun:	equ 0x10	; Buffer overrun error
sio_frameerr:	equ 0x20	; Framing error (async only)
sio_syndet:	equ 0x40	; Sync detect
sio_dsr:	equ 0x80	; Data set ready

; HDC output ports
hdc_loadsector:	equ 0x05	; Load sector counter (output)
hdc_ctl:	equ 0x06	; Control register (output)
hdc_writebuf:	equ 0x07	; Write data to buffer

; HDC input ports
hdc_readbuf:	equ 0x00	; Read data from buffer
hdc_status:	equ 0x01	; Read status byte
hdc_clrbuf:	equ 0x02	; Clear buffer counter
hdc_clrsec:	equ 0x03	; Clear sector pulse latch
hdc_sync:	equ 0x04	; Start sync
hdc_read:	equ 0x05	; Start read
hdc_write:	equ 0x06	; Start write
hdc_format:	equ 0x07	; Format write

; Video driver data block offsets
vdb_x:		equ 0x00	; Cursor X
vdb_y:		equ 0x01	; Cursor Y (scanline, not row)
vdb_pixmaps:	equ 0x02	; Glyph table address
vdb_scan:	equ 0x04	; Current start scanline
vdb_status:	equ 0x05	; Flags
vdb_ret:	equ 0x06	; Return address
vdb_cursptr:	equ 0x08	; Cursor data address
vdb_inverse:	equ 0x0A	; Normal video if 00, inverse if FF

; Status bits
vdb_curs_isoff:	equ 0		; Set by driver if cursor off
vdb_nowrap:	equ 1		; Set by user to inhibit line wrap
vdb_noscroll:	equ 2		; Set by user to inhibit scroll and auto CR
				; If set, the following two bits apply:
vdb_hit_top:	equ 6		; Set by driver if cursor hits top of screen
vdb_hit_end:	equ 7		; Set by driver if cursor hits end of screen
