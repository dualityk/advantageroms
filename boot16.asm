
; NORTH STAR ADVANTAGE
; ##############################################
; ADV-16 Boot ROM ("BOOT-16"), last byte 0x2D
;
; Disassembly by ian Butler, Aug 2023, no revisions
; I didn't try to make this one re-assemble, it's for information only.

; This one gives me second revision vibes; it clears the stack segment to
; ensure the boot jump lands in the right place, where the other version
; doesn't, and it won't refuse to start if its initial RAM test fails.

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
000FFF02  7536              jnz 0xff3a

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

000FFF2B  B002              mov al,0x2		; Enable parity interrupt
000FFF2D  E600              out 0x0,al
000FFF2F  8BCD              mov cx,bp		; High byte of BP into CL
000FFF31  86E9              xchg ch,cl
000FFF33  4F                dec di
000FFF34  4F                dec di
000FFF35  7403              jz 0xff3a		; Skip next if RAM test pass
000FFF37  80C902            or cl,0x2		; See if failure was pass 2/3

000FFF3A  BB00D0            mov bx,0xd000	; DS set for writes to Z80
000FFF3D  8EDB              mov ds,bx
000FFF3F  E400              in al,0x0		; See if Z80 got our thing
000FFF41  A801              test al,0x1
000FFF43  75FA              jnz 0xff3f		; Wait til it does
000FFF45  880C              mov [si],cl		; Send our RAM POST status
						; Version CE crashes here if
						; RAM is no good
000FFF47  33FF              xor di,di		; Clear out DI and ES
000FFF49  8EC7              mov es,di
000FFF4B  BB00E0            mov bx,0xe000	; DS set for reads from 88IN
000FFF4E  8EDB              mov ds,bx
000FFF50  A880              test al,0x80	; See if Z80 has sent data
000FFF52  7402              jz 0xff56		; No, skip read 
000FFF54  8A04              mov al,[si]		; Yes, fetch byte
000FFF56  B90600            mov cx,0x6		; Now fetch 6-byte header
000FFF59  F3A4              rep movsb
000FFF5B  268B0E0400        mov cx,[es:0x4]	; Block length
000FFF60  26C43E0000        les di,[es:0x0]	; Block start address
000FFF65  F3A4              rep movsb		; 
000FFF67  33E4              xor sp,sp		; Null stack pointer and seg
000FFF69  8ED4              mov ss,sp		; 
000FFF6B  CB                retf		; And boot.

	defb 115(0x90)		; Buncha nops

000FFFE0  EA0000F0FF        jmp 0xfff0:0	; x86 cold starts here


	defb 0x90, 0x31, 0x2e, 0x30, 0x2e, 0x31, 4(0x90) ; Not sure!
	defb 0x2d		; Revision?
