comment *----------------------------------------------------------------------

Program title:  pcgen.asm (procedure)  by Mike Gainey (3/9/92 - 3/9/92)

Modifications:

Description:

  This program generates pc sets given a value in CX from 0 to 4095.
  The correspondance between the presence of pc's and binary counting is used:
  11 10 9  8  7  6  5  4  3  2  1  0  <-- binary digit
  b  bf a  gs g  fs f  e  ef d  cs c  <-- pitch class
  The output, a string of pc's in ascii form, is terminated w/ an FF marker.

Procedures used:  none

Entry:  CX = value used to generate the pc set
        DI = address of output

Exit :  output is placed where DI pointed on entry

--------*----------------------------------------------------------------------

        .model  small

        .data

pc_tab  db      'c  cs d  ef e  f  fs g  gs a  bf b  '  ;pc lookup table

        .code           ;------------------------------------------------------

pc_gen  proc
        public  pc_gen

        push    ax                      ;save register values
        push    bx
        push    cx
        push    dx
        push    si
        push    di

        mov     si,offset pc_tab        ;position source pointer to pc table

loop1:
        mov     dx,cx                   ;CX --> DX for bit processing
        xor     bx,bx                   ;initialize BX as offset pointer
        mov     cx,12                   ;initialize loop counter

loop2:
        shr     dx,1                    ;move a bit to the carry flag
        jc      addpc                   ;bit is present, add pc to output
        add     bx,3                    ;move pc table lookup pointer

loop3:
        loop    short loop2             ;loop back to check next bit
        jmp     short return            ;finished conversion

addpc:
        mov     al,[si+bx]              ;get a pc (byte 1)
        mov     [di],al                 ;store pc (byte 1)
        inc     bx                      ;bump offset pointer
        inc     di                      ;bump destination pointer

        mov     al,[si+bx]              ;get a pc (byte 2)
        mov     [di],al                 ;store pc (byte 2)
        inc     bx                      ;bump offset pointer
        inc     di                      ;bump destination pointer

        mov     al,[si+bx]              ;get a pc (byte 3)
        mov     [di],al                 ;store pc (byte 3)
        inc     bx                      ;bump offset pointer
        inc     di                      ;bump destination pointer

        jmp     short loop3             ;continue the loop

return:
        mov     al,0ffh                 ;FF = end marker
        mov     [di],al                 ;append end marker (FF)

        pop     di                      ;restore register values
        pop     si
        pop     dx
        pop     cx
        pop     bx
        pop     ax


        ret                             ;return to calling routine

pc_gen  endp
        end
