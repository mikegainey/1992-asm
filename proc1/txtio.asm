comment |

-------------------------------------------------------------------------------
  File:  TXTIO.ASM

  Low-level text mode I/O routines

  Author:  Naba Barkakati, 3/4/89

  p. 573-78 of Microsoft Macro Assembler Bible (The Waite Group)

  Description:  Direct video routines compatible with all popular displays.
------------------------------------------------------------------------------|

VideoStruct     struc
  rows          db      ?
  columns       db      ?
  isCGA         db      ?
  isColor       db      ?
  VideoSegment  dw      ?
  VideoOffset   dw      ?
VideoStruct     ends

BIOSdata        segment at 40h
    org 49h
  VideoMode     db      ?
  ScreenColumns db      ?
    org 4eh
  VMemOffset    dw      ?
    org 63h
  VideoPort     dw      ?
    org 84h
  ScreenRows    db      ?
    org 87h
  EGAinfo       db      ?
BIOSdata        ends

;------------------------------------------------------------------------------
;  Define Macros

ComputeVideoAddress     macro
;  Entry: DS:BX pointer to videodata
;         DH,DL row, column
;  Exit : DI will be offset of byte in video memory
;         ES will be segment address of video memory

        push    ax
        mov     al,[bx].columns
        mul     dh
        xor     dh,dh
        add     ax,dx
        shl     ax,1
        add     ax,[bx].VideoOffset
        mov     di,ax
        pop     ax
        mov     es,[bx].VideoSegment
        endm

SynchWait               macro
                        local   wait1, wait2
;  Entry: none
;  Exit : horizontal retrace begins

        mov     dx,3dah
wait1:  in      al,dx
        test    al,1
        jnz     wait1
wait2:  in      al,dx
        test    al,1
        jz      wait2
        endm

;------------------------------------------------------------------------------
        .model  small
        .code
        public v_getparams
        public v_strout, v_chrout, v_getchattr
;------------------------------------------------------------------------------
;  Determine video information
;  Entry: DS:BX points to VideoStruct structure
;  Exit : Structure is filled with video info

v_getparams     proc
                mov     ax,BIOSdata     ;set up ES to address segment
                mov     es,ax           ;BIOSdata
                assume es:BIOSdata
                push    bx
                mov     ah,12h          ;call EGA BIOS function 12h
                mov     bl,10h          ;by using video interrupt 10h
                int     10h             ;to get info
                mov     cl,bl
                pop     bx
                cmp     cl,10h
                jne     isEGAactive
                cmp     VideoPort,3d4h
                jne     NotCGA1
                mov     [bx].IsCGA,1
                jmp     short skipNotCGA
NotCGA1:
                mov     [bx].IsCGA,0
skipNotCGA:
                mov     [bx].rows,25
                jmp     short checkColor
IsEGAactive:
                test    EGAinfo,8
                je      EGAactive
                cmp     VideoPort,3d4h
                jne     EGAactive
                mov     [bx].isCGA,1
                jmp     short skipEGAactive
EGAactive:
                mov     [bx].IsCGA,0
skipEGAactive:
                mov     al,ScreenRows
                mov     [bx].rows,al
                inc     [bx].rows
CheckColor:
                cmp     VideoPort,3d4h
                jne     Mono
                mov     [bx].IsColor,1
                mov     word ptr [bx].VideoSegment,0b800h
                jmp     short skipMono
Mono:
                mov     [bx].IsColor,0
                mov     word ptr [bx].VideoSegment,0b000h
skipMono:
                mov     al,ScreenColumns
                mov     [bx].columns,al
                mov     ax,VMemOffset
                mov     [bx].VideoOffset,ax
                ret
v_getparams     endp

;------------------------------------------------------------------------------
;  v_strout:  Display a string
;
;  Entry: DS:SI points to ASCIIZ string to be printed
;         DS:BX points to VideoStruct structure
;         DH = row position
;         DL = column position
;         AH = attribute

v_strout        proc
                push    ax
                push    cx
                push    dx
                push    si
                push    di
                push    es
                ComputeVideoAddress
                cld
sout1:          lodsb
                or      al,al
                jz      soutdone
                test    byte ptr [bx].IsCGA,1
                jz      soutnosynch
                mov     cx,ax
                cli
                synchwait
                xchg    ax,cx
                stosw
                sti
                jmp     sout1
soutnosynch:
                stosw
                jmp     sout1
soutdone:
                pop     es
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     ax
                ret
v_strout        endp

;------------------------------------------------------------------------------
;  v_chrout:  Print a single character
;
;  Entry: DS:BX points to VideoStruct structure
;         AL = character to be displayed
;         DH = row position
;         DL = column position
;         AH = attribute

v_chrout        proc
                push    ax
                push    cx
                push    dx
                push    di
                push    es
                ComputeVideoAddress
                test    byte ptr [bx].IsCGA,1
                jz      coutnosynch
                mov     cx,ax
                cli
                SynchWait
                xchg    ax,cx
                stosw
                sti
                jmp     short coutdone
coutnosynch:
                stosw
coutdone:
                pop     es
                pop     di
                pop     dx
                pop     cx
                pop     ax
                ret
v_chrout        endp

;------------------------------------------------------------------------------
;  v_getchattr:  Return character and attribute
;
;  Entry: DS:BX = address of VideoStruct structure
;         DH = row position
;         DL = column position
;  Exit : AL = character at that position
;         AH = attribute

v_getchattr     proc
                push    dx
                push    di
                push    es
                ComputeVideoAddress
                test    byte ptr [bx].IsCGA,1
                jz      gcnosynch
                SynchWait
gcnosynch:
                mov     ax,es:[di]
                pop     es
                pop     di
                pop     dx
                ret
v_getchattr     endp
                end





