comment *----------------------------------------------------------------------

Program name:  asc2val.asm (procedure) by Mike Gainey (3/10/92 - 3/11/92)

Modif. dates:

Description:

  A procedure to convert a number in asciiz format in a base between 2 and 16
  to a word (2 bytes).

Entry:  SI = points to the asciiz string
        BL = base of the input number
        DI = points to the output words (4 bytes)

Exit :  word 1 = value of the asciiz string in base BL
        word 2 = error status :  0000 = no error
                                 FFFF = input error

--------*----------------------------------------------------------------------

        .model  small

        .data   ;--------------------------------------------------------------

base    dw      0                       ;memory variable for base

        .code   ;--------------------------------------------------------------
        public  asc2val
asc2val proc

        push    ax                      ;save registers on entry
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    bp

        xor     bh,bh                   ;so BS = BL
        mov     base,bx                 ;save base in memory variable

;----- push the ascii number on the stack -----
        mov     ax,0ffffh               ;FFFF = end marker
        push    ax                      ;push FFFF end marker on the stack

stk1:
        lodsb                           ;mov al,[si], inc si
        cmp     al,0                    ;check for end marker
        jne     stk3                    ;goto next step
        cmp     ah,0                    ;make sure at least one digit is pushed
        je      next1                   ;if so, go to next step
        jmp     short error
stk3:
        cmp     al,'-'                  ;check for negative sign
        je      valid                   ;don't try to convert to a number

        sub     al,48                   ;convert ascii --> numerical value
        cmp     al,9                    ;check range
        jle     stk2                    ;within range, continue
        and     al,11011111b            ;make sure it's uppercase
        sub     al,7                    ;for (A...F)
stk2:
        cmp     al,byte ptr base        ;check to see if input number is valid
        jl      valid                   ;input is valid, continue
error:
        mov     ax,0ffffh               ;FFFF = error indication
        mov     [di+2],ax               ;return error indication
errloop:
        pop     ax                      ;take a value off the stack
        cmp     ax,0ffffh               ;check for end marker
        jne     errloop                 ;loop until stack is clear
        jmp     short exit              ;return to the calling procedure
valid:
        xor     ah,ah                   ;clear AH (also a flag for valid input)
        push    ax                      ;store digit (in AL) on the stack
        jmp     short stk1              ;loop until all digits have been pushed

;----- process digits in reverse order (from the stack) -----
next1:
        pop     ax                      ;get digit in AL
        mov     [di],ax                 ;store least significant digit

        mov     bp,1                    ;initialize digit counter (exponent)

digit:
        pop     bx                      ;get a digit from the stack
        cmp     bx,0ffffh               ;check for end marker
        je      end_pos                 ;end of the positive number
        cmp     bl,'-'                  ;check for negative sign
        je      end_neg                 ;end of the negative number

        mov     ax,1                    ;initialize AX
        mov     cx,bp                   ;CX = iteration counter for mul
power:  mul     base                    ;multiply base to get power (BP)
        loop    power                   ;multiply until degree is reached

        mul     bx                      ;multiply digit by power
        add     [di],ax                 ;store accumulating result
        inc     bp                      ;power for next digit
        jmp     short digit             ;loop back until all digits are done

end_neg:
        neg     word ptr [di]           ;make BX negative
        pop     ax                      ;take end marker off the stack

end_pos:
        mov     word ptr [di+2],0       ;return 'no error' report

exit:
        pop     bp                      ;restore registers on exit
        pop     di
        pop     si
        pop     dx
        pop     cx
        pop     bx
        pop     ax

        ret                             ;return to calling procedure
asc2val endp
        end






