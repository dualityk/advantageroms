
; NORTH STAR ADVANTAGE
; ##############################################
; ADV-16 Boot ROM ("BOOT-1"), last byte 0xCE
;
; Disassembly by ian Butler, Aug 2023, no revisions
; I didn't try to make this one re-assemble, it's for information only.


; This may be an initial revision; it assumes the stack segment is zero and
; could boot somewhere off in the weeds if SS != 0 when it runs.  It also
; freezes on purpose if the initial RAM test fails.

; I'm haven't decided if this is a warm start thing or if it's spurious.  It
; seems like it's filling the first 256 bytes of RAM with the numbers 0-255,
; at least if CS=0, and in that case maybe 0xff00 becomes "cmp ah,[cs:si]"
; and would thus skip the RAM test?  While still pooping all over a decent
; portion of the interrupt vector table.

000FFEF0  32E4              xor ah,ah
000FFEF2  33F6              xor si,si
000FFEF4  B9FF00            mov cx,0xff
000FFEF7  2EAC              cs lodsb
000FFEF9  02E0              add ah,al
000FFEFB  E2FA              loop 0xfef7
000FFEFD  B101              mov cl,0x1
000FFEFF  2E                cs

; ADV16 boots by sending the results of an abbreviated memory test to 88OUT. 
; Once the Z80 reads this byte, ADV16 tosses any waiting data byte from 88IN
; and waits for a header.  The header is:
; Block start offset (16 bits)
; Block start segment (16 bits, but should be zero)
; Block length (16 bits)

; Once it receives the header, it reads n bytes from 88IN into the start
; offset through the length, then does a far return to the start of the
; block to boot.

000FFF00  3A24              cmp ah,[si]		; If AH=[SI], skip RAM test
000FFF02  7532              jnz 0xff36

000FFF04  BA0400            mov dx,0x4		; Repeat test four times
000FFF07  33ED              xor bp,bp		; BP = 0
000FFF09  8EDD              mov ds,bp		; DS = 0
000FFF0B  8EC5              mov es,bp		; ES = 0
000FFF0D  33FF              xor di,di		; DI = 0
000FFF0F  B90080            mov cx,0x8000	; Test first 32K RAM
000FFF12  8B05              mov ax,[di]		; Grab word from [DI]
000FFF14  F715              not word [di]	; Complement word at [DI]
000FFF16  8B1D              mov bx,[di]		; Grab word from [DI] again
000FFF18  AB                stosw		; Record it and advance DI
000FFF19  F7D3              not bx		; Re-complement and compare
000FFF1B  3BC3              cmp ax,bx		; RAM word is good if AX=BX
000FFF1D  E1F3              loope 0xff12	; djnz if it's good
000FFF1F  750A              jnz 0xff2b		; Unequal means failed RAM
000FFF21  81C50010          add bp,0x1000	; Otherwise advance base,
000FFF25  4A                dec dx		; decrement outer counter,
000FFF26  75E1              jnz 0xff09		; and loop.
000FFF28  BF0200            mov di,0x2		; DI = 2 if total success.

						; Parity interrupt would be
						; enabled here in version 2D
000FFF2B  8BCD              mov cx,bp		; High byte of BP into CL
000FFF2D  86E9              xchg ch,cl
000FFF2F  4F                dec di
000FFF30  4F                dec di
000FFF31  7403              jz 0xff36		; Skip next if RAM test pass
000FFF33  80C902            or cl,0x2		; See if failure was pass 2/3

000FFF36  BB00D0            mov bx,0xd000	; DS set for writes to 88OUT
000FFF39  8EDB              mov ds,bx
000FFF3B  E400              in al,0x0		; See if Z80 got our thing
000FFF3D  A801              test al,0x1
000FFF3F  75FA              jnz 0xff3b		; Wait til it does
000FFF41  880C              mov [si],cl
000FFF43  F6C103            test cl,0x3		; Crash here if RAM failure
000FFF46  75FE              jnz 0xff46
000FFF48  33FF              xor di,di		; Clear out DI and ES
000FFF4A  8EC7              mov es,di
000FFF4C  BB00E0            mov bx,0xe000	; DS set for reads from 88IN
000FFF4F  8EDB              mov ds,bx
000FFF51  A880              test al,0x80	; See if Z80 has sent data
000FFF53  7402              jz 0xff57		; No, skip read
000FFF55  8A04              mov al,[si]		; Yes, fetch byte
000FFF57  B90600            mov cx,0x6		; Now fetch 6-byte header
000FFF5A  F3A4              rep movsb
000FFF5C  268B0E0400        mov cx,[es:0x4]	; Block length
000FFF61  26C43E0000        les di,[es:0x0]	; Block start address
000FFF66  F3A4              rep movsb
000FFF68  33E4              xor sp,sp		; Null just stack pointer
000FFF6A  CB                retf		; And boot.

	defb 114(0x90)		; Buncha nops

000FFFE0  EA0000F0FF        jmp 0xfff0:0x0	; x86 cold starts here

	defb 10(0x90)		; More nops
	defb 0xCE		; Revision?