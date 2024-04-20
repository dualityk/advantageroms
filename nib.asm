; z80dasm 1.1.6
; command line: z80dasm -alg 0x1000 -b blocks -r frequency -s oursyms -S syms nibprom.bin

	org	0x1000

; 2k RAM: 0000-07ff
; 4k ROM: 1000-1fff

; Ports
dipswitches: equ 0x80 ; DIP switch bank
advstat_port: equ 0x81 ; Advantage status port (to adv)
advcmd_port: equ 0x81 ; Advantage command port (from adv)

advcmd2_port: equ 0x82 ; Advantage status port 2 (to adv?)
; bit 0 checked by part of isr_hunt
; bit 1 selects Advantage if true or server card(?) for command read
; bit 2 must be set before service_adv will try to read command
; bits 4 and 5 set by reset_mpsc

advcmd_reset: equ 0x87	; This probably resets the latch on bit 2 of advcmd2

; MPSC values
mpscA: equ 0x92	; Wire tx port
mpscB: equ 0x93 ; Wire rx port
; Write registers
mpsc_intctl:	equ 0x01	; Interrupt control register
mpsc_busctl:	equ 0x02	; Processor/bus interface control
mpsc_rxctl:	equ 0x03	; Receiver control
mpsc_mode:	equ 0x04	; Mode control
mpsc_txctl:	equ 0x05	; Transmitter control
mpsc_mac:	equ 0x06	; Sync/address character
mpsc_sync:	equ 0x07	; Sync character

; Read registers
mpsc_error:	equ 0x01	; Rx char error/special condition status
mpsc_vector:	equ 0x02	; Interrupt vector (wire rx)

; DMA controller is on 0xa(n)
dma_addr0: equ 0xa0
dma_len0: equ 0xa1
dma_addr1: equ 0xa2
dma_len1: equ 0xa3
dma_addr2: equ 0xa4
dma_len2: equ 0xa5
;dma_addr3: equ 0xa6
;dma_len3: equ 0xa7
dma_status: equ 0xa8 ; (read)
dma_cmd: equ 0xa8 ; (write)
;dma_req: equ 0xa9
dma_mask: equ 0xaa
dma_mode: equ 0xab
dma_clearptr: equ 0xac
;dma_tmp: equ 0xad ; (read)
dma_clearall: equ 0xad ; (write)
dma_clearmask: equ 0xae
dma_maskall: equ 0xaf


servercmd: equ 0xc9 ; Probably server command port

; Variables
int:		equ 0x0038	; Normal interrupt landing spot
nmi:		equ 0x0066	; NMI landing spot
mpscA_en:	equ 0x0181 ; Wire tx enable flag
autorx_off:	equ 0x0182 ; Flag, Wire rx offline
advstat_toggle:	equ 0x0183 ; Adv wants status register A or B
isr_change:	equ 0x0186 ; Flag, ISR change is requested
rx_inflight:	equ 0x0189 ; Flag, packet being received
coldcfg_wait:	equ 0x018a ; Flag, interlock for cold config
b0_flag:	equ 0x018d
intjp_ptr	equ 0x0197 ; Pointer interrupt handler jumps to
int_reason:	equ 0x0199 ; Interrupt code from MPSC
macaddresstwo:	equ 0x046d
number_two:	equ 0x046f
number_one:	equ 0x0472

advstatus1:	equ 0x0713
; bit 7 must be low for POST success on adv side
; bit 6 is rx framing error
; bit 1 toggles for command acknowledge
; bit 0 signals ADV transfer ready (either direction)

advstatus2:	equ 0x0714
; bit 0, 7 set after memory tests on cold boot
; bit 2 is ROM test failure
; bit 3 is RAM test failure
; bit 4 is rx hailing frequencies open
; bit 5 is mpscA ints or something

macaddress:	equ 0x0715

orig_portbase:  equ 0x071b	; Original base port number stored to
				; port_base, loaded from somemore_start

port_base:	equ 0x075e	; Base port number for several things


rom:
	di			;1000 Disable interrupts and cold start
	jp coldstart		;1001

; BLOCK 'dispatch' (start 0x1004 end 0x1054)
dispatch_start:
	defb 0x00
	defw cmd00
	defb 0x01
	defw cmd01
	defb 0x02
	defw cmd02
	defb 0x09
	defw cmd09
	defb 0x0a
	defw cmd_hello
	defb 0x0b
	defw cmd0b
	defb 0x0c
	defw cmd0c
	defb 0x0e
	defw cmd0e
	defb 0x0f
	defw cmd0f
	defb 0x10
	defw cmd10
	defb 0x11
	defw cmd11
	defb 0x12
	defw cmd12
	defb 0x13
	defw cmd13
	defb 0x18
	defw cmd_statusbyteA
	defb 0x19
	defw cmd_statusbyteB
	defb 0x21
	defw cmd21
	defb 0x22
	defw cmd_mpscA_enable
	defb 0x23
	defw cmd23
	defb 0x24
	defw cmd24
	defb 0x25
	defw cmd25
	defb 0x29
	defw cmd29
	defb 0x2a
	defw cmd_mpscA_disable
	defb 0x2b
	defw cmd2b
	defb 0x2c
	defw cmd2c
	defb 0x2d
	defw cmd2d
	defb 0x2e
	defw cmd2e
	defb 0xff
l1053h:
	defb 012h		;1053

coldstart:
	out (087h),a		;1054
	ld a,001h		;1056
	out (0b5h),a		;1058
	ld sp,0x0100		;105a Reset stack pointer
	ld ix,.csramlanding	;105d Set return address for RAM test
	jp ramtest		;1061 And do it
.csramlanding:
	ld sp,0x0100		;1064 Reset stack pointer some more
	call romtest		;1067 CRC(?) check on ROM
	xor a			;106a Clear advstatus1
	ld (advstatus1),a	;106b
	ld a,(advstatus2)	;106e Set bits 0 and 7 of advstatus2
	or 0x81			;1071 
	ld (advstatus2),a	;1073
	in a,(dipswitches)	;1076 Store DIP switch setting
	ld (macaddress),a	;1078
	in a,(advcmd2_port)	;107b Check to see if server card present?
	bit 1,a			;107d Yes?  Then
	jp nz,shadow_rom	;107f over the river and through the woods

warmstart:
	di			;1082
	call clr_tables		;1083
	call reset_mpsc		;1086
	call reset_dma		;1089
	call autorx_start	;108c
	ei			;108f
	in a,(advcmd2_port)	;1090 Check to see if server card present?
	bit 1,a			;1092
	call nz,init_ram	;1094 Build RAM tables if so

service_adv:
	ei			;1097 Enable interrupts
	ld sp,0x0100		;1098 Preemptively reset stack
	ld a,(advstat_toggle)	;109b Update adv-side status register
	or a			;109e  according to A/B toggle
	ld a,(advstatus1)	;109f 
	jr z,.sadv_skip		;10a2
	ld a,(advstatus2)	;10a4
.sadv_skip:
	out (advstat_port),a	;10a7
	in a,(advcmd2_port)	;10a9 Check cmd2:2, which means a new 
	bit 2,a			;10ab  command is waiting
	jr z,service_adv	;10ad No, keep waiting
	xor a			;10af ??? 
	out (0b4h),a		;10b0 ???
	out (087h),a		;10b2 ???
	ld a,(advstatus1)	;10b4 Complement command acknowledge bit
	xor 0x02		;10b7 (adv does not see this immediately)
	ld (advstatus1),a	;10b9
	in a,(advcmd2_port)	;10bc cmd2:1 determines where command
	bit 1,a			;10be  came from
	jr nz,.sadv_readnonlocal;10c0
	in a,(advcmd_port)	;10c2 From the usual Adv port
	jr .sadv_readlocal	;10c4
.sadv_readnonlocal:
	in a,(servercmd)	;10c6 or from far away
.sadv_readlocal:
	ld b,a			;10c8
	jp m,service_adv	;10c9 Commands >127 ignored
	ld hl,dispatch_start	;10cc Otherwise start looking
.sadv_finddispatch:
	ld a,(hl)		;10cf Grab a name
	cp b			;10d0 See if it matches
	jr z,.sadv_dispatch	;10d1  Yes, dispatch
	cp 0xff			;10d3  No, are we at the end?
	jp z,service_adv	;10d5   Yes, give up
	inc hl			;10d8   No, advance to next line
	inc hl			;10d9
	inc hl			;10da
	jr .sadv_finddispatch	;10db And repeat
.sadv_dispatch:
	inc hl			;10dd Found it, grab our jump address
	ld e,(hl)		;10de
	inc hl			;10df
	ld d,(hl)		;10e0
	ex de,hl		;10e1 Put it in HL
	di			;10e2 Disable interrupts
	jp (hl)			;10e3 And begone

reset_mpsc:
	ld a,0x18		;10e4 Reset wire tx
	out (mpscA),a		;10e6
	nop			;10e8 Mandatory loitering
	nop			;10e9
	nop			;10ea
	nop			;10eb
	ld a,mpsc_busctl	;10ec
	out (mpscA),a		;10ee
	ld a,0x02		;10f0 Enable DMA for both channels
	out (mpscA),a		;10f2
	ld a,mpsc_mode		;10f4
	out (mpscA),a		;10f6
	ld a,0x20		;10f8 Set mode to SDLC 
	out (mpscA),a		;10fa
	ld a,mpsc_mac		;10fc 
	out (mpscA),a		;10fe
	ld a,(macaddress)	;1100 Set SDLC secondary address
	out (mpscA),a		;1103
	ld a,mpsc_sync		;1105 
	out (mpscA),a		;1107
	ld a,0x7e		;1109 Set sync byte to SDLC type
	out (mpscA),a		;110b
	ld a,mpsc_rxctl		;110d 
	out (mpscA),a		;110f
	ld a,0xe0		;1111 8 bits/char, auto enable
	out (mpscA),a		;1113
	ld a,mpsc_txctl		;1115 
	out (mpscA),a		;1117 8 bits/char, tx enable,
	ld a,0x6b		;1119 rts low, tx crc enable
	out (mpscA),a		;111b
	ld a,mpsc_intctl	;111d 
	out (mpscA),a		;111f
	ld a,0x07		;1121 Status int enable, tx int enable,
	out (mpscA),a		;1123 status affects vector

	ld a,0x18		;1125 Reset wire rx
	out (mpscB),a		;1127 
	nop			;1129 Mandatory loitering
	nop			;112a
	nop			;112b
	nop			;112c
	ld a,mpsc_busctl	;112d
	out (mpscB),a		;112f
	ld a,0x00		;1131 Clear interrupt vector
	out (mpscB),a		;1133
	ld a,mpsc_mode		;1135
	out (mpscB),a		;1137
	ld a,0x20		;1139 Set mode to SDLC
	out (mpscB),a		;113b
	ld a,mpsc_mac		;113d
	out (mpscB),a		;113f
	ld a,(macaddress)	;1141 Set SDLC secondary address
	out (mpscB),a		;1144
	ld a,mpsc_sync		;1146
	out (mpscB),a		;1148
	ld a,0x7e		;114a Set sync byte to SDLC type
	out (mpscB),a		;114c
	ld a,mpsc_rxctl		;114e
	out (mpscB),a		;1150 Rx 8 bits/char, auto enable, enter
	ld a,0xfd		;1152 hunt, rx crc enable, address search,
	out (mpscB),a		;1154 rx enable
	ld a,mpsc_txctl		;1156
	out (mpscB),a		;1158 Tx 8 bits/char, rts low, tx disable,
	ld a,0x62		;115a tx crc disable
	out (mpscB),a		;115c
	ld a,mpsc_intctl	;115e
	out (mpscB),a		;1160 Int on first rx char only, condition
	ld a,0x0c		;1162 affects vector
	out (mpscB),a		;1164
	call mpsc_clearintA	;1166 Clear status interrupts
	call mpsc_clearintB	;1169
	ld a,0x28		;116c Reset wire tx int
	out (mpscA),a		;116e
	ld a,0x30		;1170 Reset wire rx SRC int 
	out (mpscB),a		;1172
	ld a,0x30		;1174 Reset wire tx SRC int
	out (mpscA),a		;1176
	out (advcmd2_port),a	;1178 advcmd2 = 0x30 ???
	ld a,0x01		;117a
	out (0xb2),a		;117c
	jp .mr_rest		;117e Spaghetti

mpsc_clearintA:
	ld a,0x10		;1181 Reset wire tx status int
	out (mpscA),a		;1183
	ld a,0x10		;1185 Harder...
	out (mpscA),a		;1187
	ld a,0x10		;1189 *harder*
	out (mpscA),a		;118b 
	ret			;118d

mpsc_clearintB:
	ld a,0x10		;118e Reset wire rx status int
	out (mpscB),a		;1190
	ld a,0x10		;1192 Harder...
	out (mpscB),a		;1194
	ld a,0x10		;1196 *harder*
	out (mpscB),a		;1198
	ret			;119a

reset_dma:
	out (dma_clearall),a	;119b Master clear
	xor a			;119d Set default configuration
	out (dma_cmd),a		;119e
	ld a,0x44		;11a0 Channel 4 single mode, write
	out (dma_mode),a	;11a2
	ld a,0x4a		;11a4 Channel 2 single mode, read
	out (dma_mode),a	;11a6
	ld a,0x45		;11a8 Channel 1 single mode, write
	out (dma_mode),a	;11aa
	ld a,0x47		;11ac Channel 3 single mode, write
	out (dma_mode),a	;11ae
	ld a,0xff		;11b0 Disable all channels
	out (dma_maskall),a	;11b2 
	ret			;11b4

.mr_rest:
	xor a			;11b5 Set default configuration
	out (dma_cmd),a		;11b6 
	ld a,0x4a		;11b8 Channel 2 single mode, read
	out (dma_mode),a	;11ba
	ld a,0x45		;11bc Channel 1 single mode, write
	out (dma_mode),a	;11be
	ld a,0x05		;11c0 Disable channel 1
	out (dma_mask),a	;11c2
	ld a,0x06		;11c4 Disable channel 2
	out (dma_mask),a	;11c6
	ret			;11c8

clr_tables:			;     Clear memory and set up interrupts
	ld a,0xc3		;11c9 Jump opcode
	ld (nmi),a		;11cb Written to NMI landing
	ld hl,nmi		;11ce NMI jumps to itself
	ld (nmi+1),hl		;11d1
	ld (int),a		;11d4 Jump opcode on INT landing
	ld hl,inthandler	;11d7 INT jumps to inthandler
	ld (int+1),hl		;11da
	im 1			;11dd Mode 1 (NMI and INT hard jump)
	ld a,001h		;11df
	out (0c0h),a		;11e1
	out (0cah),a		;11e3
	out (0c8h),a		;11e5
	ld a,008h		;11e7
	ld (00718h),a		;11e9
	ld bc,0x058f		;11ec Clear memory from 0182 to 0712
	ld hl,0x0182		;11ef
	ld d,h			;11f2
	ld e,l			;11f3
	ld (hl),0x00		;11f4
	inc de			;11f6
	ldir			;11f7
	xor a			;11f9 
	out (085h),a		;11fa
	out (0b3h),a		;11fc
	out (0b2h),a		;11fe
	inc a			;1200
	out (0b1h),a		;1201
	xor a			;1203
	ld (b0_flag),a		;1204
	out (0b0h),a		;1207
	ld hl,0x0100		;1209 Fill 0100 to 0182 with 0x01
	ld bc,0x0082		;120c
	ld d,h			;120f
	ld e,l			;1210
	ld (hl),0x01		;1211
	inc de			;1213
	ldir			;1214
	ret			;1216

init_ram:
	ld hl,somemore_start	;1217
	ld de,0071bh		;121a
	ld bc,00021h		;121d 33 bytes
	ldir			;1220 Copy block from ROM to RAM
	ld hl,something_start	;1222
	ld de,0073ch		;1225
	ld bc,00040h		;1228 64 bytes
	ldir			;122b Copy block from ROM to RAM
	ld b,006h		;122d
l122fh:
	push bc			;122f
	ld hl,00000h		;1230
	ld de,02000h		;1233
	ld c,000h		;1236
	ld a,010h		;1238
	ld b,00ah		;123a
	call boop		;123c
	ld a,b			;123f
	pop bc			;1240
	or a			;1241
	jr nz,l1260h		;1242
	ld hl,02400h		;1244
	ld bc,0013ch		;1247
	xor a			;124a
l124bh:
	adc a,(hl)		;124b
	inc hl			;124c
	push af			;124d
	dec bc			;124e
	ld a,b			;124f
	or c			;1250
	jr z,l1256h		;1251
	pop af			;1253
	jr l124bh		;1254
l1256h:
	pop af			;1256
	or a			;1257
	jr z,l1273h		;1258
	cp (hl)			;125a
	jr nz,l1273h		;125b
	jp 03000h		;125d
l1260h:
	dec b			;1260
	jr z,l1273h		;1261
	push bc			;1263
	ld b,000h		;1264
	ld de,02ee0h		;1266
l1269h:
	djnz l1269h		;1269
	dec de			;126b
	ld a,d			;126c
	or e			;126d
	jr nz,l1269h		;126e
	pop bc			;1270
	jr l122fh		;1271
l1273h:
	call clr_tables		;1273
	call autorx_start	;1276
	jp service_adv		;1279

shadow_rom:
	ld hl,rom		;127c Copy ROM to itself -- is this
	ld de,rom		;127f shadowing to the server card RAM?
	ld bc,rom		;1282
	ldir			;1285
	ld a,001h		;1287
	out (0b6h),a		;1289
	xor a			;128b
	out (0b5h),a		;128c
	ld ix,.sr_ramdone	;128e Check RAM
	jp ramtest		;1292
.sr_ramdone:
	ld sp,0x0100h		;1295 Reset stack pointer
	xor a			;1298 Clear advstatus1
	ld (advstatus1),a	;1299
	ld a,(advstatus2)	;129c Set bits 0, 7 of advstatus2
	or 0x81			;129f (startup checkpoint)
	ld (advstatus2),a	;12a1
	in a,(dipswitches)	;12a4 Store mac address off switches
	ld (macaddress),a	;12a6
	ld bc,0x0000		;12a9 Copy the entire address space
	ld d,b			;12ac to itself for some reason
	ld e,b			;12ad
	ld h,b			;12ae
	ld l,b			;12af
	ldir			;12b0
	jp warmstart		;12b2

init_variables:
	ld a,(001a1h)		;12b5
	ld (0046ah),a		;12b8
	ld (00470h),a		;12bb
	ld a,(001a2h)		;12be
	ld (0046bh),a		;12c1
	ld a,(001a3h)		;12c4
	ld (0046ch),a		;12c7
	ld (00471h),a		;12ca
	ld a,(macaddress)	;12cd
	ld (macaddresstwo),a	;12d0
	ld a,001h		;12d3
	ld (number_one),a	;12d5
	ld a,002h		;12d8
	ld (number_two),a	;12da
	call .iv_spaghetti	;12dd Spaghetti
	ld (00473h),a		;12e0
	ld a,(00718h)		;12e3 Tries for int_giveup
	ld (00712h),a		;12e6
	ld hl,(0019fh)		;12e9
	ld de,0000ah		;12ec
	add hl,de		;12ef
	ld (00193h),hl		;12f0
	ld a,001h		;12f3
	ld (00195h),a		;12f5
	ld hl,00474h		;12f8
	ld de,(0019fh)		;12fb
	ret			;12ff
.iv_spaghetti:
	ld a,(advstatus1)	;1300 Set status bit 1 (done?)
	or 0x20			;1303
	ld (advstatus1),a	;1305
	ld a,(advstatus1)	;1308 Clear status bit 3
	and 0xef		;130b
	ld (advstatus1),a	;130d
	ld hl,0x0180		;1310 See if 0180 is 0
	ld a,(hl)		;1313
	ld (hl),0x00		;1314 Clear it regardless
	or a			;1316
	ld de,0x0100		;1317
	ld hl,(001a1h)		;131a Load pointer from 01a1
	ld h,0x00		;131d
	add hl,de		;131f Force high byte to 0x01
	jr nz,.iv_skip		;1320 If 0180 was 0,
	inc (hl)		;1322 increment byte at pointer
.iv_skip:
	ld a,(hl)		;1323
	and 0x0f		;1324 
	ret nz			;1326
	ld (hl),a		;1327
	inc (hl)		;1328
	ld a,(hl)		;1329
	ret			;132a

autorx_start:			;     Start receiving packets if stopped
	ld a,(autorx_off)	;132b Prevent re-entry if this is running
	or a			;132e
	ret z			;132f
autorx_force:			;     Start receiving packets regardless
	xor a			;1330 Clear stopped flag
	ld (autorx_off),a	;1331
	ld (rx_inflight),a	;1334 
	ld a,001h		;1337
	ld (b0_flag),a		;1339
	out (0b0h),a		;133c
	ld a,(advstatus1)	;133e Clear advstatus1 bit 3 and update
	and 0xf7		;1341
	ld (advstatus1),a	;1343
	call update_status	;1346
	ld hl,001a4h		;1349
	ld de,002a8h		;134c
	call attnreset		;134f Jiggle port 91
	ld a,0x05		;1352 Disable DMA channel 1
	out (dma_mask),a	;1354
	out (dma_clearptr),a	;1356
	dec de			;1358 Adjust block length
	ld a,l			;1359 Base address from HL
	out (dma_addr1),a	;135a
	ld a,h			;135c
	out (dma_addr1),a	;135d
	ld a,e			;135f Block length in DE
	out (dma_len1),a	;1360 
	ld a,d			;1362
	out (dma_len1),a	;1363
	ld a,0x45		;1365 Channel 1 single write
	out (dma_mode),a	;1367 (Wire rx->NIB)
	ld a,0x01		;1369 Enable channel 1
	out (dma_mask),a	;136b
	ld a,mpsc_rxctl		;136d
	out (mpscB),a		;136f
	ld a,0xfd		;1371 Start hunting on wire rx
	out (mpscB),a		;1373
	ld a,0x30		;1375 Error reset wire rx
	out (mpscB),a		;1377
	ld a,mpsc_txctl		;1379 
	out (mpscB),a		;137b
	ld a,0x60		;137d Disable tx wire rx
	out (mpscB),a		;137f
	call sub_13a4h		;1381 Select correct int handler
	xor a			;1384 Clean up and get out
	ret			;1385

set_defaultisr:			;     Restore isr_hunt (sometimes)
	di			;1386
	ld a,0x01		;1387 Mark change as requested even if we
	ld (isr_change),a	;1389 don't actually do it
	ld a,(rx_inflight)	;138c Don't change handler if flag is set
	or a			;138f
	ret nz			;1390
	ld a,001h		;1391
	out (0b3h),a		;1393
	ld hl,isr_hunt		;1395 Select interrupt handler
	jr set_inthandler	;1398

set_isr_getack:			;     Expect an acknowledgement packet
	ld hl,isr_getack	;139a
	jr set_inthandler	;139d 

set_isr_rxpacket:		;     Expect to finalize receipt
	ld hl,isr_rxpacket	;139f
	jr set_inthandler	;13a2

sub_13a4h:
	di			;13a4
	ld a,0x40		;13a5 Wire rx reset rx crc
	out (mpscB),a		;13a7
	ld a,0x20		;13a9 Wire rx interrupt on next rx char
	out (mpscB),a		;13ab
	ld hl,isr_hunt		;13ad Select interrupt handler 2

set_inthandler:
	ld (intjp_ptr),hl	;13b0 Update pointer for second half of
	ret			;13b3 interrupt handler

inthandler:
	push af			;13b4 Save state
	push bc			;13b5
	push de			;13b6
	push hl			;13b7
	push ix			;13b8
	push iy			;13ba
	in a,(dma_status)	;13bc Has adv-nib DMA reached terminal count?
	bit 0,a			;13be
	jp nz,advdma_end	;13c0 Yes, clean up
	ld a,mpsc_vector	;13c3 No, get MPSC interrupt vector
	out (mpscB),a		;13c5
	in a,(mpscB)		;13c7 
	rrca			;13c9 Right justify the status bits
	rrca			;13ca
	and 0x07		;13cb 
	ld (int_reason),a	;13cd And save it
	ld a,0x38		;13d0 Signal MPSC end of interrupt
	out (mpscA),a		;13d2
	ld hl,(intjp_ptr)	;13d4 Bounce to this pointer
	jp (hl)			;13d7

isr_hunt:			;     Regular hunt mode interrupt handler
	in a,(mpscA)		;13d8 Check for wire tx CTS
	bit 5,a			;13da
	jr nz,.ih_cts		;13dc Yes, CTS?
	jp l1564h		;13de Nope
.ih_cts:
	call mpsc_clearintA	;13e1 Clear tx status interrupt
	ld a,(int_reason)	;13e4 Is it a tx external/status change?
	cp 0x05			;13e7
	jr z,.ih_nottxstat	;13e9 No, skip.
	cp 0x07			;13eb Is it wire tx SRC -------------
	ld a,0x01		;13ed Snag wire rx error status      |
	out (mpscB),a		;13ef                                |
	in a,(mpscB)		;13f1                                |
	jp nz,int_giveup	;13f3 Yes, give up                <--
.ih_nottxstat:
	ld hl,macaddress	;13f6 No, we got something off wire rx
	ld a,(0046ah)		;13f9
	cp (hl)			;13fc
	jp z,l1680h		;13fd
	ld (0018eh),a		;1400
	ld a,(0046ch)		;1403
	ld (0018fh),a		;1406
	xor a			;1409 Clear ISR change request flag
	ld (isr_change),a	;140a
	inc a			;140d
	ld (00188h),a		;140e
	ld hl,0x046b		;1411 Top of block
	ld de,(00193h)		;1414 Packet size
	ld a,0x06		;1418 Disable wire tx DMA
	out (dma_mask),a	;141a
	out (dma_clearptr),a	;141c
	dec de			;141e Adjust block length
	ld a,l			;141f Base address from HL 
	out (dma_addr2),a	;1420
	ld a,h			;1422
	out (dma_addr2),a	;1423
	ld a,e			;1425 Block length from DE
	out (dma_len2),a	;1426
	ld a,d			;1428
	out (dma_len2),a	;1429
	ld a,0x28		;142b Reset wire tx interrupt
	out (mpscA),a		;142d
	ld a,0x4a		;142f Set DMA single read mode
	out (dma_mode),a	;1431 (NIB->Wire tx)
	ld a,0x02		;1433 Enable wire tx DMA
	out (dma_mask),a	;1435
	ld a,0x80		;1437 Reset wire tx CRC generator
	out (mpscA),a		;1439 
	ld a,mpsc_txctl		;143b 
	out (mpscB),a		;143d 
	ld a,0x62		;143f RTS? Disable wire rx (on tx channel)
	out (mpscB),a		;1441
	ld a,(0046ah)		;1443
	out (090h),a		;1446
	ld a,0xc0		;1448 Reset wire tx idle/CRC latch
	out (mpscA),a		;144a 
	xor a			;144c
	ld (0044dh),a		;144d
	ld hl,0044ch		;1450
	ld de,0001eh		;1453 
	call attnreset		;1456 Jiggle port 91
	ld a,0x05		;1459 Disable wire rx DMA
	out (dma_mask),a	;145b
	out (dma_clearptr),a	;145d
	dec de			;145f Adjust block length
	ld a,l			;1460
	out (dma_addr1),a	;1461
	ld a,h			;1463
	out (dma_addr1),a	;1464
	ld a,e			;1466
	out (dma_len1),a	;1467
	ld a,d			;1469
	out (dma_len1),a	;146a
	ld a,0x45		;146c Set DMA single write mode
	out (dma_mode),a	;146e (Wire rx->NIB)
	ld a,0xc0		;1470 Reset wire tx idle/CRC latch
	out (mpscA),a		;1472 
	ld a,0x01		;1474 Enable wire rx DMA
	out (dma_mask),a	;1476
	ld a,mpsc_rxctl		;1478 
	out (mpscB),a		;147a
	ld a,0xfd		;147c Enable wire rx
	out (mpscB),a		;147e
	call set_isr_getack	;1480 Set interrupt handler
	jp intreturn		;1483

isr_getack:			;     Handle acknowledgement of our transmission
	ld a,(int_reason)	;1486 Is it a tx external/status change?
	cp 0x05			;1489
	jp nz,l149fh		;148b Yes, give up (we only want acknowledgement)
	ld a,0x06		;148e Disable tx DMA
	out (dma_mask),a	;1490
	out (dma_clearptr),a	;1492
	in a,(dma_len2)		;1494 Get rx'd length and put in DE
	ld e,a			;1496
	in a,(dma_len2)		;1497
	ld d,a			;1499
	inc de			;149a
	ld a,d			;149b
	or e			;149c
	jr z,l14afh		;149d Only valid if we received full message
l149fh:
	ld a,0x08		;149f Otherwise send abort on wire tx
	out (mpscA),a		;14a1
	xor a			;14a3
	out (0b3h),a		;14a4
	call mpsc_clearintA	;14a6 Reset status interrupts
	call mpsc_clearintB	;14a9
	jp int_giveup		;14ac And give up
l14afh:
	call mpsc_clearintA	;14af
	xor a			;14b2
	out (0b3h),a		;14b3
	ld a,mpsc_rxctl		;14b5 
	out (mpscB),a		;14b7
	ld a,0xf9		;14b9 Disable wire rx search mode
	out (mpscB),a		;14bb
	ld a,0x28		;14bd Reset wire tx interrupt
	out (mpscA),a		;14bf
	ld a,001h		;14c1
	ld (0018bh),a		;14c3
	ld b,0x80		;14c6 128 tries to get to end of frame
l14c8h:
	in a,(mpscA)		;14c8 Select main command/status byte
	ld a,0x10		;14ca Reset wire tx status ints
	out (mpscA),a		;14cc
	ld a,mpsc_error		;14ce Grab wire rx error status
	out (mpscB),a		;14d0
	in a,(mpscB)		;14d2
	bit 7,a			;14d4 End of frame?
	jr z,l14dfh		;14d6
	bit 6,a			;14d8 We're at the end of frame, bad CRC?
	jp nz,int_giveup	;14da Yes, give up
	jr l14e4h		;14dd No, success 
l14dfh:
	djnz l14c8h		;14df Not EOF, keep trying
	jp int_giveup		;14e1 Timeout, give up
l14e4h:				;     Success!
	ld a,(0044fh)		;14e4
	ld b,a			;14e7
	ld a,(0018eh)		;14e8
	cp b			;14eb
	jr nz,int_giveup	;14ec
	ld a,(0044dh)		;14ee
	cp 007h			;14f1
	jp nz,int_giveup	;14f3
	xor a			;14f6
	ld (isr_change),a		;14f7
	ld (00188h),a		;14fa
	ld a,(00451h)		;14fd
	ld (00716h),a		;1500
	ld a,(00452h)		;1503
	ld (00717h),a		;1506
	ld a,(0044fh)		;1509
	ld (00719h),a		;150c
	ld a,001h		;150f
	ld (0018ch),a		;1511
	ld a,(advstatus1)	;1514 Set bit 4 - packet send acknowledged
	or 0x10			;1517
	ld (advstatus1),a	;1519
	ld a,(advstatus1)	;151c Clear bit 5
	and 0xdf		;151f
	ld (advstatus1),a	;1521
	call update_status	;1524 Update status right now
	ld a,0x30		;1527 Error reset wire rx
	out (mpscB),a		;1529
	call reset_mpsc		;152b
	call autorx_force	;152e
	jp intreturn		;1531

int_giveup:			;     Cleanup after error
	xor a			;1534
	out (0b3h),a		;1535
	call reset_mpsc		;1537
	ld hl,00712h		;153a
	dec (hl)		;153d
	jr z,l154eh		;153e
	ld bc,0x0020		;1540 Loiter here for a while
.igu_wait:
	djnz .igu_wait		;1543
	dec c			;1545
	jr nz,.igu_wait		;1546
	call set_defaultisr	;1548 Reset handler
	jp intreturn		;154b Exit interrupt

l154eh:
	ld a,(advstatus1)	;154e
	and 0dfh		;1551
	ld (advstatus1),a	;1553
	ld a,(advstatus1)	;1556
	and 0efh		;1559
	ld (advstatus1),a	;155b
	call autorx_force		;155e
	jp intreturn		;1561

l1564h: ; part of isr_hunt
	call mpsc_clearintA	;1564 Clear all status ints
	call mpsc_clearintB	;1567
	xor a			;156a
	out (0b3h),a		;156b
	in a,(advcmd2_port)	;156d
	bit 0,a			;156f
	jr nz,l157fh		;1571
	ld a,(int_reason)	;1573 Why are we here?
	cp 0x02			;1576 
	jr z,l157fh		;1578 Wire rx character received
	cp 0x03			;157a
	jp nz,rxfail		;157c Wire rx SRC
l157fh:
	ld a,mpsc_txctl		;157f 
	out (mpscB),a		;1581 
	ld a,0x62		;1583 RTS Disable tx for wire rx
	out (mpscB),a		;1585
	out (advcmd2_port),a	;1587 ???
	ld a,0x01		;1589 Set rx packet in flight flag
	ld (rx_inflight),a	;158b
	call set_isr_rxpacket	;158e Change to rx handler
	jp intreturn		;1591

isr_rxpacket:			;     Finish rx and ack of incoming packet
	xor a			;1594 Clear rx packet in flight flag
	ld (rx_inflight),a	;1595
	ld a,0x05		;1598 Disable channel 1
	out (dma_mask),a	;159a
	ld a,mpsc_rxctl		;159c 
	out (mpscB),a		;159e
	ld a,0xfc		;15a0 Disable wire rx
	out (mpscB),a		;15a2
	ld a,(int_reason)	;15a4
	cp 003h			;15a7
	jr z,l15b0h		;15a9
	cp 002h			;15ab
	jp nz,rxfail		;15ad
l15b0h:
	ld a,mpsc_error		;15b0 Grab wire rx error status
	out (mpscB),a		;15b2
	in a,(mpscB)		;15b4
	bit 6,a			;15b6 Framing error?
	jp nz,rxfail		;15b8
	ld a,(advstatus1)	;15bb No, set bit 3???
	or 0x08			;15be
	ld (advstatus1),a	;15c0
	call update_status	;15c3
	ld hl,0x01a8		;15c6 Probably acknowledgement packet
	ld de,0x0014		;15c9 20 bytes out
	ld a,0x06		;15cc Disable channel 2
	out (dma_mask),a	;15ce
	out (dma_clearptr),a	;15d0
	dec de			;15d2 Adjust block length
	ld a,l			;15d3 Base address from HL
	out (dma_addr2),a	;15d4
	ld a,h			;15d6
	out (dma_addr2),a	;15d7
	ld a,e			;15d9
	out (dma_len2),a	;15da Block length from DE
	ld a,d			;15dc
	out (dma_len2),a	;15dd
	ld a,0x4a		;15df Channel 2 single read
	out (dma_mode),a	;15e1 (NIB->Wire tx)
	call mpsc_clearintA	;15e3
	ld b,0x01		;15e6 Wait one jiffy
l15e8h:
	djnz l15e8h		;15e8
	ld a,001h		;15ea
	out (0b3h),a		;15ec
	out (083h),a		;15ee
	call mpsc_clearintA	;15f0
	ld a,007h		;15f3
	ld (001a8h),a		;15f5
	ld a,(macaddress)	;15f8
	ld (001aah),a		;15fb
	ld (001ach),a		;15fe
	ld a,0x30		;1601 Error reset wire rx
	out (mpscB),a		;1603
	ld a,0x80		;1605 Reset wire tx CRC generator
	out (mpscA),a		;1607 
	ld a,0x28		;1609 Reset wire tx interrupt
	out (mpscA),a		;160b
	ld a,0x02		;160d Enable channel 2
	out (dma_mask),a	;160f
	ld a,(001a7h)		;1611
	out (090h),a		;1614
	ld a,0xc0		;1616 Reset wire tx idle/CRC latch
	out (mpscA),a		;1618
	ld b,0x80		;161a Wait 80 jiffies
l161ch:
	djnz l161ch		;161c
	ld a,0x06		;161e Disable channel 2
	out (dma_mask),a	;1620
	xor a			;1622
	out (0b3h),a		;1623
	ld a,0x28		;1625 Reset wire tx interrupt
	out (mpscA),a		;1627
	ld a,0x30		;1629 Error reset wire rx
	out (mpscB),a		;162b
	call sub_1640h		;162d
	call reset_mpsc		;1630
	call autorx_start	;1633
	ld a,(isr_change)	;1636
	or a			;1639
	call nz,set_defaultisr	;163a
	jp intreturn		;163d
sub_1640h: ;part of isr_rxpacket
	xor a			;1640
	ld (rx_inflight),a	;1641
	inc a			;1644
	ld (autorx_off),a	;1645
	ld a,001h		;1648
	ld (b0_flag),a		;164a
	out (0b0h),a		;164d
	ld a,(001a5h)		;164f
	cp 001h			;1652
	ret nz			;1654
	ld a,(00184h)		;1655
	or a			;1658
	jr z,l166dh		;1659
	ld a,(0018eh)		;165b
	ld b,a			;165e
	ld a,(001a7h)		;165f
	cp b			;1662
	ret nz			;1663
	ld a,(0018fh)		;1664
	ld b,a			;1667
	ld a,(001a9h)		;1668
	cp b			;166b
	ret nz			;166c
l166dh:
	ld a,(001a6h)		;166d
	cp 002h			;1670
	ret nz			;1672
	xor a			;1673
	ld (autorx_off),a	;1674
	ld a,(advstatus1)	;1677
	or 004h			;167a
	ld (advstatus1),a	;167c
	ret			;167f

l1680h:
	ld a,0x06		;1680 Disable channel 2
	out (dma_mask),a	;1682
	ld a,(advstatus1)	;1684
	or 010h			;1687
	ld (advstatus1),a	;1689
	ld a,(macaddress)	;168c
	ld (00716h),a		;168f
	ld (00719h),a		;1692
	call update_statusdi	;1695
	xor a			;1698
	ld (00188h),a		;1699
	xor a			;169c
	out (0b3h),a		;169d
	call mpsc_clearintA	;169f
	call mpsc_clearintB	;16a2

rxfail:				;     Receive failure
	call autorx_force	;16a5 Immediately restart receiving
	ld a,(advstatus1)	;16a8
	or 0x40			;16ab Set status flag
	ld (advstatus1),a	;16ad
	ld a,mpsc_error		;16b0 Grab wire rx error status
	out (mpscB),a		;16b2
	in a,(mpscB)		;16b4
	jp warmstart		;16b6 Warm start

advdma_end: 			;     Adv DMA transfer complete
	out (085h),a		;16b9
	ld a,0x04		;16bb Disable channel 0
	out (dma_mask),a	;16bd
	ld a,(advstatus1)	;16bf
	and 0xfe		;16c2 Clear transfer ready flag
	ld (advstatus1),a	;16c4
	ld hl,00195h		;16c7 Clear+check this flag???
	ld a,(hl)		;16ca
	ld (hl),0x00		;16cb
	or a			;16cd
	call nz,set_defaultisr	;16ce
	ld hl,00196h		;16d1
	ld a,(hl)		;16d4
	ld (hl),000h		;16d5
	or a			;16d7
	call nz,autorx_force	;16d8
	call update_status	;16db
	jp intreturn		;16de

intreturn:
	pop iy			;16e1 Restore state
	pop ix			;16e3
	pop hl			;16e5
	pop de			;16e6
	pop bc			;16e7
	pop af			;16e8
	ei			;16e9 Enable interrupts
	reti			;16ea

update_statusei:
	call update_status	;16ec Update status, then enable ints
	ei			;16ef 
	ret			;16f0

update_statusdi:
	di			;16f1 Disable ints, then update status
update_status:
	ld a,(advstat_toggle)	;16f2 Just update status port
	or a			;16f5
	ret nz			;16f6
	ld a,(advstatus1)	;16f7
	out (advstat_port),a	;16fa
	ret			;16fc

attnreset:			;     Whatever this is doing
	in a,(091h)		;16fd
	in a,(091h)		;16ff
	in a,(091h)		;1701
	in a,(091h)		;1703
	ret			;1705

ramtest:
	ld hl,0x0000		;1706 Start at bottom of RAM
.rt_loop:
	ld (hl),0x55		;1709 Write 0x55
	ld a,(hl)		;170b Read it back
	cp 0x55			;170c Still 0x55?
	jr nz,.rt_fail		;170e No, fail and break
	ld (hl),0xaa		;1710 Write 0xaa
	ld a,(hl)		;1712 Read it back
	cp 0xaa			;1713 Still 0xaa?
	jr nz,.rt_fail		;1715 No, fail and break
	ld (hl),0x00		;1717 Clear RAM
	inc hl			;1719 Next byte please
	ld a,h			;171a
	cp 0x08			;171b Loop through 0x07ff
	jr nz,.rt_loop		;171d
.rt_yolo:
	push ix			;171f IX holds our return address
	ret			;1721
.rt_fail:
	ld a,(advstatus2)	;1722 Set advstatus2 bit 3 if ram fail
	or 0x08			;1725 
	ld (advstatus2),a	;1727
	jp .rt_yolo		;172a and return

romtest:
	ld hl,rom		;172d Start at bottom of ROM
	ld a,0xaa		;1730 Initial bit pattern
.romt_loop:
	ld b,(hl)		;1732 Load byte, xor with current pattern
	xor b			;1733
	inc hl			;1734 Next byte
	ld c,a			;1735
	ld a,h			;1736
	cp 0x20			;1737 Continue through 0x1fff
	ld a,c			;1739
	jp nz,.romt_loop	;173a
	cp 0x00			;173d Final result should be zero
	ret z			;173f
	ld a,(advstatus2)	;1740 Set bit 2 of advstatus2 if rom fail
	or 0x04			;1743
	ld (advstatus2),a	;1745
	ret			;1748

cmd00:				;     Send alternate driver to Advantage
	call sub_18c9h		;1749
	call sub_18b2h		;174c
	ld hl,advdriver		;174f Copy driver into expected location in RAM
	ld de,0x0474		;1752
	ld bc,0x0100		;1755
	push bc			;1758
	push de			;1759
	ldir			;175a
	pop hl			;175c Set HL to top of block, DE to length
	pop de			;175d
	jp nibtoadv		;175e And send to Advantage

cmd01:
	ld a,(l1053h)		;1761
	ld (0071ah),a		;1764
	ld hl,macaddress	;1767
	ld de,00006h		;176a
	jp nibtoadv		;176d

cmd02:
	ld a,(advstatus1)	;1770 See if bit 2 is clear
	bit 2,a			;1773 
	jp z,service_adv	;1775 Yes, bail
	ld a,(advstatus1)	;1778 No, clear it
	and 0f3h		;177b
	ld (advstatus1),a	;177d
	ld a,001h		;1780
	ld (00196h),a		;1782
	ld a,001h		;1785
	ld (00196h),a		;1787
	ld hl,001aeh		;178a
	ld de,0029eh		;178d
	jp nibtoadv		;1790

nibtoadv:
	di			;1793
	ld a,001h		;1794
	ld (b0_flag),a		;1796
	out (0b0h),a		;1799
	ld a,001h		;179b
	out (0b1h),a		;179d
	ld a,0x04		;179f Disable channel 0
	out (dma_mask),a	;17a1
	out (dma_clearptr),a	;17a3
	dec de			;17a5 Adjust block length
	ld a,l			;17a6 Base address in HL
	out (dma_addr0),a	;17a7
	ld a,h			;17a9
	out (dma_addr0),a	;17aa
	ld a,e			;17ac Block length in DE
	out (dma_len0),a	;17ad
	ld a,d			;17af
	out (dma_len0),a	;17b0
	ld a,0x48		;17b2 Channel 0 single read
	out (dma_mode),a	;17b4 (NIB->ADV)
	ld a,001h		;17b6
	out (086h),a		;17b8
	ld a,0x00		;17ba Enable channel 0
	out (dma_mask),a	;17bc
	ld a,(advstatus1)	;17be Update adv packet ready flag,
	or 0x01			;17c1 and re-enable interrupts
	ld (advstatus1),a	;17c3
	call update_statusei	;17c6
	jp service_adv		;17c9

cmd09:
	ld hl,00718h		;17cc
	ld de,00003h		;17cf
	jp advtonib		;17d2

cmd_hello:			;     Retrieve cold configuration from adv
	ld a,0x01		;17d5 Interlock with second conf stage
	ld (coldcfg_wait),a	;17d7
	ld hl,0x019f		;17da
	ld de,0x0005		;17dd
	jp advtonib		;17e0

cmd0b:				;     Bring up full configuration from adv
	ld a,(coldcfg_wait)	;17e3 Bail if we're still waiting for hello
	or a			;17e6
	jp z,service_adv	;17e7
	xor a			;17ea Clear hello interlock
	ld (coldcfg_wait),a	;17eb
	call init_variables	;17ee Reset lots of variables
	jp advtonib		;17f1 and retrieve remaining configuration

cmd0c:
	call sub_1966h		;17f4
	call sub_194bh		;17f7
	ld hl,0019ah		;17fa
	ld de,00005h		;17fd
	jp advtonib		;1800

cmd0e:
	ld iy,l1809h		;1803 Return here after these messages
	jr l181ah		;1807
l1809h:
	ld a,(advstatus1)	;1809 Clear adv packet ready flag
	and 0xfe		;180c
	ld (advstatus1),a	;180e
	jp service_adv		;1811

cmd0f:
	ld iy,(0019ch)		;1814
	jr l181ah		;1818

l181ah:
	call sub_18c9h		;181a
	call sub_18b2h		;181d
	ld hl,(0019ch)		;1820
	ld de,(0019ah)		;1823
	ld a,(0019eh)		;1827
	or a			;182a
	jp nz,nibtoadv		;182b
	ld a,0x04		;182e Disable channel 0
	out (dma_mask),a	;1830
	out (dma_clearptr),a	;1832
	dec de			;1834 Adjust block length
	ld a,l			;1835
	out (dma_addr0),a	;1836 Set base address to HL
	ld a,h			;1838
	out (dma_addr0),a	;1839
	ld a,e			;183b
	out (dma_len0),a	;183c And length to DE
	ld a,d			;183e
	out (dma_len0),a	;183f
	ld a,0x44		;1841 Channel 0 single write
	out (dma_mode),a	;1843 (ADV->NIB)
	xor a			;1845
	ld (b0_flag),a		;1846
	out (0b0h),a		;1849
	out (086h),a		;184b ???
	ld a,0x00		;184d Enable channel 0
	out (dma_mask),a	;184f
	ld a,(advstatus1)	;1851 Set adv packet ready flag
	or 0x01			;1854
	ld (advstatus1),a	;1856
	out (advstat_port),a	;1859 Update advstat port immediately
l185bh:
	in a,(dma_status)	;185b Wait here for adv to finish read
	bit 0,a			;185d
	jr z,l185bh		;185f
	xor a			;1861 Clear advstat port entirely
	out (advstat_port),a	;1862
	jp (iy)			;1864 Then bail

advtonib:
	di			;1866 No interruptions
	ld a,001h		;1867
	ld (b0_flag),a		;1869
	out (0b0h),a		;186c
	ld a,001h		;186e
	out (0b1h),a		;1870
	ld a,0x04		;1872 Disable channel 0
	out (dma_mask),a	;1874
	out (dma_clearptr),a	;1876
	dec de			;1878 Adjust block length
	ld a,l			;1879 Set HL as base address
	out (dma_addr0),a	;187a
	ld a,h			;187c
	out (dma_addr0),a	;187d
	ld a,e			;187f
	out (dma_len0),a	;1880 Set DE as byte count
	ld a,d			;1882
	out (dma_len0),a	;1883
	ld a,0x44		;1885 Channel 0 single write
	out (dma_mode),a	;1887 (ADV->NIB)
	xor a			;1889
	out (086h),a		;188a ???
	ld a,0x00		;188c Enable channel 0
	out (dma_mask),a	;188e
	ld a,(advstatus1)	;1890 Set adv packet ready flag
	or 0x01			;1893
	ld (advstatus1),a	;1895
	jp service_adv		;1898

cmd10:
	call sub_18b2h		;189b
	call sub_18c9h		;189e
	ld a,(advstatus1)	;18a1
	and 002h		;18a4
	ld (advstatus1),a	;18a6
	jp warmstart		;18a9

cmd11:
	call sub_18b2h		;18ac
	jp service_adv		;18af

sub_18b2h:
	di			;18b2
	xor a			;18b3 
	ld (isr_change),a	;18b4
	ld a,(advstatus1)	;18b7
	and 0cfh		;18ba
	ld (advstatus1),a	;18bc
	jp update_status	;18bf

cmd12:
	di			;18c2
	call sub_18c9h		;18c3
	jp service_adv		;18c6

sub_18c9h:
	ld a,0x30		;18c9 Wire rx error reset
	out (mpscB),a		;18cb
	ld a,(advstatus1)	;18cd Clear bits 2, 3 of status1
	and 0xf3		;18d0
	ld (advstatus1),a	;18d2
	xor a			;18d5
	ld (00187h),a		;18d6
	call update_status	;18d9
	call autorx_force	;18dc For sure turn on open rx
	ret			;18df

cmd13:
	di			;18e0 Immediately stop interrupts
	call sub_18b2h		;18e1 Clear status 4 and 5 and cancel isr_change
	call sub_18c9h		;18e4 Clear status 2 and 3, open hailing freqs
	jp service_adv		;18e7

; 18
cmd_statusbyteA:
	xor a			;18ea Advantage gets advstatus1
	ld (advstat_toggle),a	;18eb
	jp service_adv		;18ee

; 19
cmd_statusbyteB:
	ld a,0x01		;18f1 Advantage gets advstatus2
	ld (advstat_toggle),a	;18f3
	jp service_adv		;18f6

cmd21:
	call sub_190ah		;18f9
	ld a,(advstatus2)	;18fc Clear bit 4 of advstatus2
	and 0efh		;18ff
	ld (advstatus2),a	;1901
	call autorx_start	;1904
	jp service_adv		;1907
sub_190ah:
	ld a,001h		;190a
	ld (autorx_off),a	;190c
	ret			;190f

; 22 
cmd_mpscA_enable:
	call .mAe_rest		;1910
	jp service_adv		;1913
sub_1916h:
	ld a,(advstatus2)	;1916 Clear bit 5 of advstatus2
	and 0xdf		;1919
	ld (advstatus2),a	;191b
	ld a,mpsc_intctl	;191e 
	out (mpscA),a		;1920
	ld a,0x07		;1922 Enable status int, tx int,
	out (mpscA),a		;1924 status affects vector, for wire tx
	ld a,001h		;1926
	ld (mpscA_en),a		;1928
	ret			;192b

cmd23:
	call sub_190ah		;192c
	call sub_1916h		;192f
	jp service_adv		;1932
cmd24:
	ld a,001h		;1935
	ld (00184h),a		;1937
	jp service_adv		;193a
cmd25:
	ld a,001h		;193d
	ld (00185h),a		;193f
	jp service_adv		;1942

cmd29:
	call sub_194bh		;1945
	jp service_adv		;1948
sub_194bh:
	ld a,mpsc_rxctl		;194b Disable wire rx
	out (mpscB),a		;194d
	ld a,0xfc		;194f
	out (mpscB),a		;1951
	xor a			;1953 Clear flag
	ld (autorx_off),a	;1954
	ld a,(advstatus2)	;1957 Set bit 4 of advstatus2
	or 0x10			;195a
	ld (advstatus2),a	;195c
	ret			;195f

; 2a
cmd_mpscA_disable:
	call .mAd_rest		;1960
	jp service_adv		;1963
.mAd_rest:
	ld a,mpsc_intctl	;1966 Disable all ints on wire tx
	out (mpscA),a		;1968
	ld a,0x00		;196a
	out (mpscA),a		;196c
	xor a			;196e Clear flag
	ld (mpscA_en),a		;196f 
	ld a,(advstatus2)	;1972
	or 0x20			;1975 Set bit 1 of advstatus2
	ld (advstatus2),a	;1977
	ret			;197a

cmd2b:
	call sub_194bh		;197b
	call sub_1966h		;197e
	jp service_adv		;1981

cmd2c:
	xor a			;1984
	ld (00184h),a		;1985
	jp service_adv		;1988

cmd2d:
	xor a			;198b
	ld (00185h),a		;198c
	jp service_adv		;198f
cmd2e:
	ld a,001h		;1992
	ld (00180h),a		;1994
	jp cmd_hello		;1997
boop:
	jp testing		;199a

; BLOCK 'somemore' (start 0x199d end 0x19be)
somemore_start:
	defb 000h		;199d -> 071b (port thing)
	defb 0ffh		;199e 071c
	defb 000h		;199f 071d
	defb 00dh		;19a0 071e
	defb 000h		;19a1 071f
	defb 028h		;19a2 0720
	defb 000h		;19a3 0721
	defb 003h		;19a4 0722
	defb 000h		;19a5 0723
	defb 001h		;19a6 0724
	defb 000h		;19a7 0725
	defb 008h		;19a8 0726
	defb 000h		;19a9 0727
	defb 000h		;19aa 0728
	defb 000h		;19ab 0729
	defb 000h		;19ac 072a
	defb 000h		;19ad 072b
	defb 000h		;19ae 072c
	defb 000h		;19af 072d
	defb 000h		;19b0 072e
	defb 000h		;19b1 072f
	defb 000h		;19b2 0730
	defb 000h		;19b3 0731
	defb 000h		;19b4 0732
	defb 000h		;19b5 0733
	defb 000h		;19b6 0734
	defb 000h		;19b7 0735
	defb 000h		;19b8 0736
	defb 000h		;19b9 0737
	defb 000h		;19ba 0738
	defb 05fh		;19bb 0739
	defb 007h		;19bc 073a
	defb 01dh		;19bd 073b
testing:
	call test1		;19be
	ret nz			;19c1
	jp l19cch		;19c2
test1:
	call test2		;19c5
	ret nz			;19c8
	ld b,000h		;19c9
	ret			;19cb

l19cch:
	call sub_1c65h		;19cc
	ld a,040h		;19cf
	call out_plus6		;19d1
l19d4h:
	xor a			;19d4
	ld (00725h),a		;19d5
l19d8h:
	call sub_1a0ch		;19d8
	jr nz,l19d8h		;19db
	xor a			;19dd
	ld b,a			;19de
l19dfh:
	in e,(c)		;19df
	ld (hl),e			;19e1
	inc hl			;19e2
	add ix,de		;19e3
	in e,(c)		;19e5
	ld (hl),e			;19e7
	inc hl			;19e8
	add ix,de		;19e9
	djnz l19dfh		;19eb
	call sub_1a43h		;19ed
	jr nz,l19feh		;19f0
	push ix		;19f2
	ex (sp),hl			;19f4
	sbc hl,de		;19f5
	jr nz,l19feh		;19f7
	call sub_1c9ch		;19f9
	jr l19d4h		;19fc
l19feh:
	ld a,06eh		;19fe
	ld hl,00744h		;1a00
	push hl			;1a03
	ld hl,00725h		;1a04
	call sub_1d18h		;1a07
	jr l19d8h		;1a0a

sub_1a0ch:
	call sub_1b69h		;1a0c
	call sub_1ba8h		;1a0f
	jr nz,l1a2dh		;1a12
	ld d,a			;1a14
	ld a,(00730h)		;1a15
	cp d			;1a18
	jr z,l1a23h		;1a19
	ld a,070h		;1a1b
	ld hl,00746h		;1a1d
	jp l1d27h		;1a20
l1a23h:
	ld hl,(0074bh)		;1a23
	ld a,(port_base)	;1a26
	ld c,a			;1a29
	xor a			;1a2a
	ld d,a			;1a2b
	ret			;1a2c
l1a2dh:
	cp 003h			;1a2d
	jr nc,l1a3eh		;1a2f
	ld hl,00743h		;1a31
	ld a,06fh		;1a34
	ex (sp),hl		;1a36
	push hl			;1a37
	ld hl,00725h		;1a38
	jp sub_1d18h		;1a3b
l1a3eh:
	ld a,06bh		;1a3e
	jp l1d3bh		;1a40

sub_1a43h:
	in d,(c)		;1a43
	in e,(c)		;1a45
	in a,(c)		;1a47
	cpl			;1a49
	cp d			;1a4a
	ret nz			;1a4b
	in a,(c)		;1a4c
	cpl			;1a4e
	cp e			;1a4f
	ret			;1a50

				;     Orphaned
	ld (0074ah),a		;1a51
	jr l1a5ah		;1a54

l1a56h:
	ld a,(0074ah)		;1a56
	ret			;1a59

l1a5ah:
	push hl			;1a5a
	ld hl,0075fh		;1a5b
	ld a,(0071bh)		;1a5e Port base from 071b
	ld (00779h),hl		;1a61
	ld (port_base),a	;1a64
	pop hl			;1a67
	call in_plus1		;1a68
	bit 5,a		;1a6b
	jr z,l1a73h		;1a6d
	ld b,07dh		;1a6f
	jr l1a56h		;1a71
l1a73h:
	call in_plus1		;1a73
	xor 001h		;1a76
	and 01bh		;1a78
	jr z,l1a56h		;1a7a
	ld b,07eh		;1a7c
	bit 4,a		;1a7e
	jr nz,l1a56h		;1a80
	bit 1,a		;1a82
	ld b,07fh		;1a84
	jr nz,l1a56h		;1a86
	ld b,071h		;1a88
	or b			;1a8a
	jr l1a56h		;1a8b
test2:
	ld (0074ah),a		;1a8d
	push hl			;1a90
	ld hl,(00779h)		;1a91
	push de			;1a94
	inc hl			;1a95
	inc hl			;1a96
	ld (hl),000h		;1a97
	dec hl			;1a99
	ld d,008h		;1a9a
	push bc			;1a9c
	ld b,014h		;1a9d
.t2_loop:
	call in_plus1		;1a9f
	and 004h		;1aa2
	jr nz,l1ac1h		;1aa4
	ld (hl),a			;1aa6
	push bc			;1aa7
	ld bc,00001h		;1aa8
	call sub_1adeh		;1aab
	pop bc			;1aae
	jr nz,l1ab5h		;1aaf
	djnz .t2_loop		;1ab1
l1ab3h:
	ld a,077h		;1ab3
l1ab5h:
	pop bc			;1ab5
	pop de			;1ab6
	ld b,a			;1ab7
	or b			;1ab8
	call z,sub_1b57h		;1ab9
	ld a,(0074ah)		;1abc
	pop hl			;1abf
	ret			;1ac0
l1ac1h:
	ld bc,004e8h		;1ac1
l1ac4h:
	ld (hl),001h		;1ac4
	push bc			;1ac6
	ld bc,00000h		;1ac7
	call sub_1adeh		;1aca
	pop bc			;1acd
	jr nz,l1ab5h		;1ace
	call in_plus1		;1ad0
	and 004h		;1ad3
	jr z,l1ab5h		;1ad5
	dec c			;1ad7
	jr nz,l1ac4h		;1ad8
	djnz l1ac4h		;1ada
	jr l1ab3h		;1adc
sub_1adeh:
	push de			;1ade
	push hl			;1adf
	ld hl,(00779h)		;1ae0
	ld a,(hl)			;1ae3
	sub d			;1ae4
	ld (0075dh),a		;1ae5
	ld (hl),d			;1ae8
	ld a,d			;1ae9
	or 040h		;1aea
	ld (0077bh),a		;1aec
	ld e,a			;1aef
	call out_plus6		;1af0
	ld a,e			;1af3
	inc hl			;1af4
	ld e,(hl)			;1af5
	ld (hl),c			;1af6
	inc hl			;1af7
	ld d,(hl)			;1af8
	ld (hl),b			;1af9
	inc hl			;1afa
	ld l,(hl)			;1afb
	ex de,hl			;1afc
	or 020h		;1afd
	sbc hl,bc		;1aff
	jr z,l1b54h		;1b01
	ld (0075dh),a		;1b03
	ld b,e			;1b06
	jr nc,l1b11h		;1b07
	and 0dfh		;1b09
	ld de,00000h		;1b0b
	ex de,hl			;1b0e
	sbc hl,de		;1b0f
l1b11h:
	ld c,a			;1b11
	call out_plus6		;1b12
	ld a,(0071eh)		;1b15
	add a,007h		;1b18
	jr c,l1b21h		;1b1a
	ld e,a			;1b1c
	ld a,b			;1b1d
	sub e			;1b1e
	jr nc,l1b22h		;1b1f
l1b21h:
	xor a			;1b21
l1b22h:
	inc a			;1b22
	ld b,a			;1b23
	ld de,0ffffh		;1b24
l1b27h:
	add hl,de			;1b27
	jr nc,l1b3ch		;1b28
	ld a,c			;1b2a
	or 010h		;1b2b
	call out_plus6		;1b2d
	ld a,c			;1b30
	call out_plus6		;1b31
	push bc			;1b34
l1b35h:
	ex (sp),hl			;1b35
	ex (sp),hl			;1b36
	djnz l1b35h		;1b37
	pop bc			;1b39
	jr l1b27h		;1b3a
l1b3ch:
	ld de,0ffffh		;1b3c
l1b3fh:
	call in_plus1		;1b3f
	xor 001h		;1b42
	and 01bh		;1b44
	jr z,l1b54h		;1b46
	rra			;1b48
	ld a,071h		;1b49
	jr c,l1b54h		;1b4b
	dec de			;1b4d
	ld a,d			;1b4e
	or e			;1b4f
	jr nz,l1b3fh		;1b50
	or 07ch		;1b52
l1b54h:
	pop hl			;1b54
	pop de			;1b55
	ret			;1b56
sub_1b57h:
	xor a			;1b57
	call sub_1b65h		;1b58
	call sub_1b5eh		;1b5b
sub_1b5eh:
	call in_plus1		;1b5e
	and 040h		;1b61
	jr nz,sub_1b5eh		;1b63
sub_1b65h:
	inc a			;1b65
	jr nz,sub_1b65h		;1b66
	ret			;1b68
sub_1b69h:
	ld ix,00000h		;1b69
	push de			;1b6d
	ld a,(00730h)		;1b6e
	ld e,a			;1b71
	ld a,(00749h)		;1b72
	jr l1b7eh		;1b75

l1b77h:
	in a,(c)		;1b77
	and 040h		;1b79
	ret z			;1b7b
	jr l1b77h		;1b7c

l1b7eh:
	ld a,(port_base)	;1b7e
	inc a			;1b81
	ld c,a			;1b82
	ld a,e			;1b83
	and 00fh		;1b84
	ld b,a			;1b86
	pop de			;1b87
l1b88h:
	in a,(c)		;1b88
	and 040h		;1b8a
	jr z,l1b88h		;1b8c
l1b8eh:
	in a,(c)		;1b8e
	and 040h		;1b90
	jr nz,l1b8eh		;1b92
	inc b			;1b94
	dec b			;1b95
	ret z			;1b96
l1b97h:
	in a,(c)		;1b97
	and 040h		;1b99
	jr z,l1b97h		;1b9b
l1b9dh:
	in a,(c)		;1b9d
	jp p,l1b9dh		;1b9f
	call in_plus3		;1ba2
	djnz l1b9dh		;1ba5
	ret			;1ba7

sub_1ba8h:
	ld a,(port_base)	;1ba8
	inc a			;1bab
	ld c,a			;1bac
	call in_plus3		;1bad
l1bb0h:
	in a,(c)		;1bb0
	jp p,l1bb0h		;1bb2
	inc c			;1bb5
	in a,(c)		;1bb6
	inc c			;1bb8
	in a,(c)		;1bb9
	ld hl,00727h		;1bbb
	ld b,003h		;1bbe
	dec c			;1bc0
	dec c			;1bc1
l1bc2h:
	in a,(c)		;1bc2
	and 040h		;1bc4
	jr z,l1bb0h		;1bc6
	djnz l1bc2h		;1bc8
	inc c			;1bca
	inc c			;1bcb
	inc c			;1bcc
	in a,(c)		;1bcd
	ld b,002h		;1bcf
l1bd1h:
	djnz l1bd1h		;1bd1
	inc c			;1bd3
	in a,(c)		;1bd4
	ld b,00ah		;1bd6
	ld a,c			;1bd8
	sub 004h		;1bd9
	ld c,a			;1bdb
l1bdch:
	in a,(c)		;1bdc
	bit 5,a		;1bde
	jr nz,l1bech		;1be0
	and 040h		;1be2
	jr z,l1bb0h		;1be4
	djnz l1bdch		;1be6
	ld a,002h		;1be8
	or a			;1bea
	ret			;1beb
l1bech:
	ld b,000h		;1bec
l1beeh:
	in a,(c)		;1bee
	inc b			;1bf0
	bit 5,a		;1bf1
	jr nz,l1beeh		;1bf3
	ld a,b			;1bf5
	cp 008h		;1bf6
	jr nc,l1c05h		;1bf8
	ld b,014h		;1bfa
l1bfch:
	in a,(c)		;1bfc
	and 040h		;1bfe
	jp z,l1bb0h		;1c00
	djnz l1bfch		;1c03
l1c05h:
	inc c			;1c05
	in a,(c)		;1c06
	dec c			;1c08
	dec c			;1c09
	ld b,009h		;1c0a
	in a,(c)		;1c0c
	inir		;1c0e
	dec hl			;1c10
	ld a,(hl)			;1c11
	dec hl			;1c12
	xor (hl)			;1c13
	inc a			;1c14
	jr z,l1c1ah		;1c15
l1c17h:
	ld a,001h		;1c17
	ret			;1c19
l1c1ah:
	ld a,(hl)			;1c1a
	ld b,007h		;1c1b
l1c1dh:
	dec hl			;1c1d
	sub (hl)			;1c1e
	djnz l1c1dh		;1c1f
	or a			;1c21
	jr nz,l1c17h		;1c22
	ld a,(00730h)		;1c24
	xor (hl)			;1c27
	and 0f0h		;1c28
	jr nz,l1c35h		;1c2a
	inc hl			;1c2c
	ld bc,(00731h)		;1c2d
	ld a,(hl)			;1c31
	cp c			;1c32
	jr z,l1c38h		;1c33
l1c35h:
	ld a,003h		;1c35
	ret			;1c37
l1c38h:
	inc hl			;1c38
	ld a,(hl)			;1c39
	xor b			;1c3a
	and 007h		;1c3b
	jr nz,setAto4		;1c3d
	ld a,(00727h)		;1c3f
	ret			;1c42
setAto4:
	ld a,004h		;1c43
	ret			;1c45

out_plus6:			;     Saves BC, outputs A XOR 3
	push bc			;1c46 Output to qualified port+6
	xor 0x03		;1c47 
	ld b,a			;1c49
	ld a,(port_base)	;1c4a
	add a,0x06		;1c4d
	ld c,a			;1c4f
	out (c),b		;1c50
	pop bc			;1c52
	ret			;1c53

in_plus1:
	ld a,0x01		;1c54 Input from qualified port+1
.inplus:
	push bc			;1c56
	ld b,a			;1c57
	ld a,(port_base)	;1c58
	add a,b			;1c5b
	ld c,a			;1c5c
	in a,(c)		;1c5d Retrieved byte in A
	pop bc			;1c5f
	ret			;1c60

in_plus3:
	ld a,0x03		;1c61 Input from qualified port+3
	jr .inplus		;1c63

sub_1c65h:
	xor a			;1c65
	ld (0071fh),a		;1c66
	ld (0074bh),de		;1c69
	ld (00758h),de		;1c6d
	ld (0074dh),hl		;1c71
	ld (0075ah),hl		;1c74
	pop bc			;1c77
	pop de			;1c78
	push de			;1c79
	ld ix,(00779h)		;1c7a
	ld hl,00000h		;1c7e
	add hl,sp			;1c81
	ld (0074fh),hl		;1c82
	push bc			;1c85
	xor a			;1c86
	ld (0075ch),a		;1c87
	jr l1d02h		;1c8a

; This is more garbage
	nop			;1c8c
	add hl,bc		;1c8d
	ld (bc),a		;1c8e
	dec bc			;1c8f
	inc b			;1c90
	dec c			;1c91
	ld b,00fh		;1c92
	ex af,af'		;1c94
	ld bc,0030ah		;1c95
	inc c			;1c98
	dec b			;1c99
	ld c,007h		;1c9a

sub_1c9ch:
	ld hl,(00733h)		;1c9c
	inc hl			;1c9f
	ld (00733h),hl		;1ca0
	ld hl,(0074dh)		;1ca3
	ld a,h			;1ca6
	or l			;1ca7
	inc hl			;1ca8
	ld (0074dh),hl		;1ca9
	jr nz,l1cd7h		;1cac
	ld hl,(0074bh)		;1cae
	ld a,(hl)			;1cb1
	dec a			;1cb2
	inc hl			;1cb3
	and (hl)			;1cb4
	inc a			;1cb5
	jr nz,l1cd7h		;1cb6
	ld de,00024h		;1cb8
	add hl,de			;1cbb
	ld a,(hl)			;1cbc
	cp 002h		;1cbd
	jr nz,l1cd7h		;1cbf
	inc hl			;1cc1
	ld a,(hl)			;1cc2
	cp 000h		;1cc3
	jr c,l1cd7h		;1cc5
	ld e,00ah		;1cc7
	add hl,de			;1cc9
	ex de,hl			;1cca
	ld hl,(00779h)		;1ccb
	inc hl			;1cce
	inc hl			;1ccf
	inc hl			;1cd0
	ex de,hl			;1cd1
	ld bc,0000ah		;1cd2
	ldir		;1cd5
l1cd7h:
	ld a,(0074ch)		;1cd7
	add a,002h		;1cda
	ld (0074ch),a		;1cdc
	ld ix,(00779h)		;1cdf
	ld a,(00730h)		;1ce3 Read 0730
	ld l,a			;1ce6 L=xx
	and 0f0h		;1ce7 H=x0
	ld h,a			;1ce9
	ld a,l			;1cea A=L+9
	add a,009h		;1ceb
	and 00fh		;1ced A=x0 or H
	or h			;1cef
	ld hl,00730h		;1cf0 Write back to 0730
	ld (hl),a		;1cf3
	ld a,(0074ah)		;1cf4
	dec a			;1cf7
	ld (0074ah),a		;1cf8
	jr nz,l1d13h		;1cfb
	ld a,000h		;1cfd
	jp z,l1d3bh		;1cff
l1d02h:
	push af			;1d02 Hang onto AF
	ld hl,0x0730		;1d03
	xor a			;1d06
	ld b,0x07		;1d07
l1d09h:
	add a,(hl)		;1d09 Sum everything from 0730-0736
	inc hl			;1d0a
	djnz l1d09h		;1d0b
	ld (hl),a		;1d0d Store sum into 0737
	cpl			;1d0e 
	inc hl			;1d0f
	ld (hl),a		;1d10 Store inverse into 0738
	pop af			;1d11
	ret			;1d12

l1d13h:
	ld a,(hl)			;1d13
	and 00fh		;1d14
	jr l1d02h		;1d16
sub_1d18h:
	ld (00751h),a		;1d18
	inc (hl)			;1d1b
	ld a,(hl)			;1d1c
	inc hl			;1d1d
	cp (hl)			;1d1e
	jr z,l1d23h		;1d1f
	jr nc,l1d38h		;1d21
l1d23h:
	pop hl			;1d23
	ex (sp),hl			;1d24
	jr l1d2ah		;1d25
l1d27h:
	ld (00751h),a		;1d27
l1d2ah:
	inc (hl)			;1d2a
	jr nz,l1d2eh		;1d2b
	dec (hl)			;1d2d
l1d2eh:
	ld hl,0071fh		;1d2e
	inc (hl)			;1d31
	ld a,(hl)			;1d32
	inc hl			;1d33
	cp (hl)			;1d34
	jr nc,l1d38h		;1d35
	ret			;1d37
l1d38h:
	ld a,(00751h)		;1d38
l1d3bh:
	ld hl,(0074fh)		;1d3b
	ld sp,hl			;1d3e
	ld (00751h),a		;1d3f
	ld a,(00751h)		;1d42
	ld b,a			;1d45
	ret			;1d46

; BLOCK 'something' (start 0x1d47 end 0x1f00)
something_start:
	defb 000h		;1d47 -> 073c
	defb 000h		;1d48 073d
	defb 000h		;1d49 073e
	defb 000h		;1d4a 073f
	defb 000h		;1d4b 0740
	defb 000h		;1d4c 0741
	defb 000h		;1d4d 0742
	defb 000h		;1d4e 0743
	defb 000h		;1d4f 0744
	defb 000h		;1d50 0745
	defb 000h		;1d51 0746
	defb 000h		;1d52 0747
	defb 000h		;1d53 0748
	defb 000h		;1d54 0749
	defb 000h		;1d55 074a
	defb 000h		;1d56 074b
	defb 000h		;1d57 074c
	defb 000h		;1d58 074d
	defb 000h		;1d59 074e
	defb 000h		;1d5a 074f
	defb 000h		;1d5b 0750
	defb 000h		;1d5c 0751
	defb 000h		;1d5d 0752
	defb 000h		;1d5e 0753
	defb 000h		;1d5f 0754
	defb 000h		;1d60 0755
	defb 000h		;1d61 0756
	defb 000h		;1d62 0757
	defb 000h		;1d63 0758
	defb 000h		;1d64 0759
	defb 000h		;1d65 075a
	defb 000h		;1d66 075b
	defb 000h		;1d67 075c
	defb 000h		;1d68 075d
	defb 000h		;1d69 075e
	defb 000h		;1d6a 075f
	defb 000h		;1d6b 0760
	defb 000h		;1d6c 0761
	defb 0f0h		;1d6d 0762
	defb 003h		;1d6e 0763
	defb 098h		;1d6f 0764
	defb 000h		;1d70 0765
	defb 040h		;1d71 0766
	defb 000h		;1d72 0767
	defb 080h		;1d73 0768
	defb 000h		;1d74 0769
	defb 098h		;1d75 076a
	defb 000h		;1d76 076b
	defb 000h		;1d77 076c
	defb 000h		;1d78 076d
	defb 000h		;1d79 076e
	defb 0f0h		;1d7a 076f
	defb 003h		;1d7b 0770
	defb 098h		;1d7c 0771
	defb 000h		;1d7d 0772
	defb 040h		;1d7e 0773
	defb 000h		;1d7f 0774
	defb 080h		;1d80 0775
	defb 000h		;1d81 0776
	defb 098h		;1d82 0777
	defb 000h		;1d83 0778
	defb 000h		;1d84 0779
	defb 000h		;1d85 077a
	defb 000h		;1d86 077b

	defs 376,0		;1d87-1eff

advdriver:
	di			;1f00
	ld sp,0fcffh		;1f01
	ld b,07fh		;1f04
l1f06h:
	push bc			;1f06
	ld a,080h		;1f07
	sub b			;1f09
	call 0fc26h		;1f0a
	ld a,(0fce7h)		;1f0d
	or a			;1f10
	call nz,0fc5ah		;1f11
	ld a,(0fc76h)		;1f14
	or a			;1f17
	call z,0fc8dh		;1f18
	ld a,077h		;1f1b
	ld (0fc76h),a		;1f1d
	pop bc			;1f20
	djnz l1f06h		;1f21
	jp 08066h		;1f23
	ld (0fce4h),a		;1f26
	ld a,00ah		;1f29
	call 0fc96h		;1f2b
	ld a,001h		;1f2e
	call 0fcbeh		;1f30
	ld hl,0fce2h		;1f33
	ld b,006h		;1f36
	ld c,010h		;1f38
	otir		;1f3a
	ld a,001h		;1f3c
	call 0fcd0h		;1f3e
	ld a,00bh		;1f41
	call 0fc96h		;1f43
	ld a,001h		;1f46
	call 0fcbeh		;1f48
	ld hl,0fce8h		;1f4b
	ld b,00bh		;1f4e
	ld c,010h		;1f50
	otir		;1f52
	ld a,001h		;1f54
	call 0fcd0h		;1f56
	ret			;1f59
	ld a,013h		;1f5a
	call 0fc96h		;1f5c
	ld a,004h		;1f5f
	call 0fcbeh		;1f61
	ld a,002h		;1f64
	call 0fc96h		;1f66
	ld a,001h		;1f69
	call 0fcbeh		;1f6b
	ld hl,0fdfah		;1f6e
	ld bc,00206h		;1f71
l1f74h:
	in a,(010h)		;1f74
	ld (hl),a			;1f76
	inc hl			;1f77
	dec bc			;1f78
	ld a,b			;1f79
	or c			;1f7a
	jr nz,l1f74h		;1f7b
l1f7dh:
	in a,(010h)		;1f7d
	djnz l1f7dh		;1f7f
	ld hl,(0fdfah)		;1f81
	ld a,l			;1f84
	or h			;1f85
	ret nz			;1f86
	ld hl,0fc76h		;1f87
	ld (hl),000h		;1f8a
	ret			;1f8c
	call 0fc5ah		;1f8d
	ld a,(0fce4h)		;1f90
	jp 0fe00h		;1f93
	push af			;1f96
	ld hl,0fce7h		;1f97
	ld (hl),006h		;1f9a
	ld de,00000h		;1f9c
	in a,(011h)		;1f9f
	and 002h		;1fa1
	ld b,a			;1fa3
	pop af			;1fa4
	out (011h),a		;1fa5
l1fa7h:
	in a,(011h)		;1fa7
	and 002h		;1fa9
	cp b			;1fab
	ret nz			;1fac
	call 0fcb2h		;1fad
	jr l1fa7h		;1fb0
	dec de			;1fb2
	ld a,d			;1fb3
	or e			;1fb4
	ret nz			;1fb5
	ld hl,0fce7h		;1fb6
	dec (hl)			;1fb9
	ret nz			;1fba
	pop hl			;1fbb
	pop hl			;1fbc
	ret			;1fbd
	ld b,a			;1fbe
	ld hl,0fce7h		;1fbf
	ld (hl),006h		;1fc2
	ld de,00000h		;1fc4
l1fc7h:
	in a,(011h)		;1fc7
	and b			;1fc9
	ret nz			;1fca
	call 0fcb2h		;1fcb
	jr l1fc7h		;1fce
	ld b,a			;1fd0
	ld hl,0fce7h		;1fd1
	ld (hl),003h		;1fd4
	ld de,00000h		;1fd6
l1fd9h:
	in a,(011h)		;1fd9
	and b			;1fdb
	ret z			;1fdc
	call 0fcb2h		;1fdd
	jr l1fd9h		;1fe0

; BLOCK 'endmatter' (start 0x1fe2 end 0x2000)
endmatter_start:
	defb 00ah		;1fe2
	defb 000h		;1fe3
	defb 000h		;1fe4
	defb 001h		;1fe5
	defb 001h		;1fe6
	defb 000h		;1fe7
	defb 001h		;1fe8
	defb 002h		;1fe9
	defb 000h		;1fea
	defb 000h		;1feb
	defb 000h		;1fec
	defb 000h		;1fed
	defb 000h		;1fee
	defb 000h		;1fef
	defb 000h		;1ff0
	defb 000h		;1ff1
	defb 000h		;1ff2
	defb 000h		;1ff3
	defb 000h		;1ff4
	defb 000h		;1ff5
	defb 000h		;1ff6
	defb 000h		;1ff7
	defb 000h		;1ff8
	defb 000h		;1ff9
	defb 000h		;1ffa
	defb 000h		;1ffb
	defb 000h		;1ffc
	defb 000h		;1ffd
	defb 000h		;1ffe
	defb 000h		;1fff
