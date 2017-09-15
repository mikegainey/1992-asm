comment *----------------------------------------------------------------------

Program name:  val2asc.asm (procedure) by Mike Gainey (3/10/92 - 3/10/92)

Modif. dates:

Description:

  A procedure to convert a signed word (2 bytes) into an ascii representation
  of the value in any base from 2 to 16.

Entry:  SI = points to procedure input
        DI = points to procedure ouput
        BL = base for output

Exit :  DI = points to an asciiz representation of the input number

--------*----------------------------------------------------------------------

        .model  small

        .data   ;--------------------------------------------------------------

minus   db      0                       ;negative indicator

        .code   ;--------------------------------------------------------------
        public  val2asc
val2asc proc

        push    ax                      ;save registers on entry
        push    bx
        push    cx
        push    dx
        push    si
        push    di

        xor     bh,bh                   ;so BX = BL
        cmp     bl,2                    ;2 is the lowest base allowed
        jl      exit                    ;if <2, abort procedure
        cmp     bl,16                   ;16 is the highest base allowed
        jg      exit                    ;if >16, abort procedure

        mov     ax,[si]                 ;get the numeric value (AX)

        cmp     ax,0                    ;test for negative numbers
        jge     pos1                    ;AX = non-negative
        mov     minus,0ffh              ;set minus flag (FF)
        mov     byte ptr [di],'-'       ;put minus sign in output buffer
        inc     di                      ;bump output pointer
        neg     ax                      ;make AX positive

pos1:
        mov     dx,0ffffh               ;FFFF = end marker
        push    dx                      ;save end marker on the stack

con1:
        xor     dx,dx                   ;initialize DX

        idiv    bx                      ;DX:AX/BX = AX (remainder = DX)

pos2:
        push    dx                      ;save remainer (in DL)

        cmp     ax,0                    ;check (quotient) to see if done
        je      con2                    ;exit inner loop if done

        jmp short con1                  ;loop back until done (inner loop1)

con2:
        pop     ax                      ;get a digit from the stack

        cmp     ax,0ffffh               ;check for end marker (FFFF)
        je      exit                    ;if end marker found, goto next step

        add     al,48                   ;convert to ascii
        cmp     al,57                   ;ascii 57 = '9'
        jle     con4                    ;valid ascii digit, save in output

        add     al,7                    ;for A=10, B=11, etc...

con4:
        mov     [di],al                 ;store digit in output buffer
        inc     di                      ;bump output pointer

        jmp     short con2              ;loop back until done (inner loop2)

exit:
        mov     byte ptr [di],0         ;append end marker (00)

        pop     di                      ;restore registers on exit
        pop     si
        pop     dx
        pop     cx
        pop     bx
        pop     ax

        ret                             ;return to calling procedure
val2asc endp
        end









