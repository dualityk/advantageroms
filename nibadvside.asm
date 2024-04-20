; z80dasm 1.1.6
; command line: z80dasm -alg 0xfc00 a.bin

netdata: equ 0x10
netctl: equ 0x11
timeleft: equ 0xfce7

	org	0fc00h

	di			;fc00
	ld sp,stacktop		;fc01 Local stack
	ld b,07fh		;fc04 127 tries
lfc06h:
	push bc			;fc06 Hang onto our tries
	ld a,080h		;fc07
	sub b			;fc09
	call sub_fc26h		;fc0a
	ld a,(timeleft)		;fc0d Did we time out?
	or a			;fc10
	call nz,sub_fc5ah	;fc11
	ld a,(zap)		;fc14 Timed out, did we get to modding code?
	or a			;fc17
	call z,sub_fc8dh	;fc18 Yes, try to boot
	ld a,0x77		;fc1b
	ld (zap),a		;fc1d
	pop bc			;fc20
	djnz lfc06h		;fc21
	jp 08066h		;fc23

sub_fc26h:
	ld (lfce4h),a		;fc26
	ld a,00ah		;fc29
	call nibcmd		;fc2b
	ld a,001h		;fc2e
	call wait_flagset	;fc30
	ld hl,lfce2h		;fc33
	ld b,006h		;fc36
	ld c,netdata		;fc38
	otir			;fc3a
	ld a,001h		;fc3c
	call wait_flagclear	;fc3e
	ld a,00bh		;fc41
	call nibcmd		;fc43
	ld a,001h		;fc46
	call wait_flagset	;fc48
	ld hl,lfce8h		;fc4b
	ld b,00bh		;fc4e
	ld c,netdata		;fc50
	otir			;fc52
	ld a,001h		;fc54
	call wait_flagclear	;fc56
	ret			;fc59

sub_fc5ah:
	ld a,013h		;fc5a
	call nibcmd		;fc5c
	ld a,004h		;fc5f
	call wait_flagset	;fc61 Wait for bit 2 to set
	ld a,0x02		;fc64 Ask NIB for some data
	call nibcmd		;fc66
	ld a,0x01		;fc69 Wait for transfer ready flag
	call wait_flagset	;fc6b
	ld hl,0xfdfa		;fc6e Set up a read from here
	ld bc,0x0206		;fc71 Read in 518 bytes (to top of RAM)
lfc74h:
	in a,(netdata)		;fc74 Get a byte
zap:	ld (hl),a		;fc76 Store a byte (this gets patched out)
	inc hl			;fc77
	dec bc			;fc78
	ld a,b			;fc79
	or c			;fc7a
	jr nz,lfc74h		;fc7b Loop until fetched
lfc7dh:
	in a,(netdata)		;fc7d Then throw away 256 more reads
	djnz lfc7dh		;fc7f
	ld hl,(0fdfah)		;fc81 See if first two bytes read are zero
	ld a,l			;fc84
	or h			;fc85
	ret nz			;fc86 No, 
	ld hl,zap		;fc87 Yes, write protect the block just stored
	ld (hl),0x00		;fc8a 
	ret			;fc8c

sub_fc8dh:
	call sub_fc5ah		;fc8d
	ld a,(lfce4h)		;fc90
	jp 0xfe00		;fc93 Boot

nibcmd:				;     Invoke NIB command in A
	push af			;fc96
	ld hl,timeleft		;fc97
	ld (hl),0x06		;fc9a Set watchdog for six moments
	ld de,0x0000		;fc9c
	in a,(netctl)		;fc9f Grab and save current command ack
	and 0x02		;fca1 state
	ld b,a			;fca3
	pop af			;fca4
	out (netctl),a		;fca5 Send command
.nc_wait:
	in a,(netctl)		;fca7 Check acknowledgement bit
	and 0x02		;fca9
	cp b			;fcab
	ret nz			;fcac Return as soon as it flips
	call watchdog		;fcad 
	jr .nc_wait		;fcb0

watchdog:			;     Keep calls from blocking indefinitely
	dec de			;fcb2 First decrement 16-bit counter in DE
	ld a,d			;fcb3
	or e			;fcb4
	ret nz			;fcb5
	ld hl,timeleft		;fcb6 If DE=0, decrement 'timeleft'
	dec (hl)		;fcb9
	ret nz			;fcba
	pop hl			;fcbb If timeleft=0, pop this call and
	pop hl			;fcbc previous call
	ret			;fcbd

wait_flagset:			;     Wait for netctl flag in A to set
	ld b,a			;fcbe
	ld hl,timeleft		;fcbf Prepare watchdog for 6 moments
	ld (hl),0x06		;fcc2
	ld de,0x0000		;fcc4
.wfs_wait:
	in a,(netctl)		;fcc7 Grab netctl and see if our flag sets
	and b			;fcc9
	ret nz			;fcca
	call watchdog		;fccb Success
	jr .wfs_wait		;fcce

wait_flagclear:			;     Wait for flag in A to clear
	ld b,a			;fcd0
	ld hl,timeleft		;fcd1 Prepare watchdog for 3 moments
	ld (hl),0x03		;fcd4
	ld de,0x0000		;fcd6
.wfc_wait:
	in a,(netctl)		;fcd9 Grab netctl and see if our flag clears
	and b			;fcdb
	ret z			;fcdc Success
	call watchdog		;fcdd 
	jr .wfc_wait		;fce0

lfce2h:
        defb 00ah               ;fce2
        defb 000h               ;fce3
        defb 000h               ;fce4
        defb 001h               ;fce5
        defb 001h               ;fce6
        defb 000h               ;fce7
        defb 001h               ;fce8
        defb 002h               ;fce9
        defb 000h               ;fcea
	nop			;fceb
	nop			;fcec
	nop			;fced
	nop			;fcee
	nop			;fcef
	nop			;fcf0
	nop			;fcf1
	nop			;fcf2
	nop			;fcf3
	nop			;fcf4
	nop			;fcf5
	nop			;fcf6
	nop			;fcf7
	nop			;fcf8
	nop			;fcf9
	nop			;fcfa
	nop			;fcfb
	nop			;fcfc
	nop			;fcfd
	nop			;fcfe
stacktop:
	nop			;fcff
