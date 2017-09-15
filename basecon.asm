;program to test asc2val.asm and val2asc.asm


;------------------------------------------------------------------------------
display macro   prmpt   ;------------------------------------------------------
        mov     si,offset prmpt         ;print string (m1: input base)
        call    print
        endm

getstr  macro           ;------------------------------------------------------
        local   cont
        mov     ah,10                   ;get keyboard input string
        mov     dx,offset in_buf
        int     21h

        mov     al,in_buf[2]            ;check for 'q' = quit
        and     al,11011111b
        cmp     al,'Q'
        jne     cont
        jmp     exit
cont:
        mov     di,offset in_buf[1]     ;make string asciiz
        mov     al,[di]
        inc     di
        xor     ah,ah
        add     di,ax
        mov     byte ptr [di],0
	endm

;------------------------------------------------------------------------------

        .model  small
        .stack

        .data   ;--------------------------------------------------------------

in_buf  db      40,0,40 dup(10110100b)  ;input buffer (40 character max)
out_buf db      60 dup(01001011b)       ;output buffer

m0      db      1,3,0
m1      db      3,7,0,0,'Input the base (2-16):  ',2,1,0
m2      db      'Input the number     :  ',2,1,0
m3      db      'base ',0
m4      db      'Press <Enter> to continue:  ',2,1,0
inerr   db      '----- input error -----',0
crlf    db      5,0
tab     db      7,0ffh,10,0

number  dw      0
        dw      0

base    dw      0
bas_buf db      3 dup(0)

        .code   ;--------------------------------------------------------------
        extrn   asc2val:proc, val2asc:proc
        extrn   print:proc

entry:
        mov     ax,@data                ;set up DS register
        mov     ds,ax

;----- get base and number from user -----

        display   m0                    ;set video mode and clear screen
start:
        display   m1                      ;get base
        getstr

	mov	bl,10			;asc --> val (base)
        mov     si,offset in_buf[2]
        mov     di,offset number
        call    asc2val
        mov     ax,number[2]            ;get error report (0000=ok, FFFF=error)
        cmp     ax,0ffffh
        je      error1
        cmp     number,2
        jb      error1
        cmp     number,16
        ja      error1
        jmp     short noerr1
error1:
        jmp     error

noerr1:
        display   crlf

        display   m2                      ;get the number
        getstr

	mov	bx,number		;asc --> val (number)
        mov     si,offset in_buf[2]
        mov     di,offset number
        call    asc2val
        mov     ax,number[2]            ;get error report (0000=ok, FFFF=error)
        cmp     ax,0ffffh
        jne     noerr2
        jmp     error
noerr2:

;----- output number in bases 2 through 16 -----

        display   crlf

        mov     cx,16                   ;BL = base counter

next_base:
        mov     base,cx                 ;convert base to ascii
        mov     bl,10
        mov     si,offset base
        mov     di,offset out_buf
        call    val2asc

        display   m3                      ;m3 = 'base '
        display   out_buf
        display   tab

        mov     bl,cl
        mov     si,offset number        
        mov     di,offset out_buf       
        call    val2asc

        display   out_buf

        display   crlf

        dec     cl
        cmp     cl,2
        jge     next_base

        display   crlf
        display   crlf

        display m4                      ;press any key to continue
        getstr
        display crlf

        jmp     start

error:
        display   inerr
        display   crlf
        display m4
        getstr
        jmp     start

exit:
        mov     ah,76
        mov     al,0
        int     21h
        end
