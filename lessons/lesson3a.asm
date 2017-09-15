;******************************************************************************
;******************************************************************************
;                               lesson3a.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson3.asm
;   by Mike Gainey
; Start date   : 2/5/92 (for Mike)
; Mod. dates   : 2/5/92 (error trapping), 2/6/92 (ascii --> bin)
;
;   Description  :
; Convert an ascii character, input by the user, to binary and display
;
;   New operations:
; 1.  Obtain a keyboard character
; 2.  Convert an ASCII character to binary
; 3.  Test and display bits
;
;******************************************************************************
;                               stack segment
;******************************************************************************
stack   segment stack
        db      1024 dup ('?')  ;default stack is 1K
stack   ends
;******************************************************************************
;                               data segment
;******************************************************************************
data    segment
digit_mess      db      'Type one ascii character: $'
binary_mess     db      0aH,0dH,'Binary value is: $'
data    ends
;******************************************************************************
;                               code segment
;******************************************************************************
code    segment
        assume  cs:code
;**************************************
;       initialization                *
;**************************************
entry_point:
;initialize data segment (DS register)
        mov     ax,data         ;address of data to AX
        mov     ds,ax           ;and to DS
        assume  ds:data         ;assume DS is for the data segment
;**************************************
;       program code                  *
;**************************************
;
; Display 'digit_mess' stored in the data segment
        lea     dx,digit_mess   ;set dx to message text
        mov     ah,9            ;service request number
        int     21H             ;goto DOS
;
; Wait for a keystroke:  ASCII code --> al register
getkey:
        mov     ah,0            ;service request number
        int     16H             ;goto BIOS
;
; Convert to binary
        push    ax              ;save user input on the stack
;
; Display 'binary_mess'
        lea     dx,binary_mess  ;set dx to message text
        mov     ah,9            ;service request number
        int     21H             ;goto DOS
        pop     ax              ;get user input back from the stack
;
; Test bits and compute binary number
        mov     ah,al           ;move number to ah (from al)
        mov     cx,8            ;set up loop counter
test_left_bit:
        test    ah,10000000B    ;test ah, bit 3
        jnz     left_bit_set    ;the bit is set, display a '1'
;
; At this point, bit 3 is not set; display a '0'
        mov     al,'0'          ;character to be displayed
        call    tty             ;procedure to display
        jmp     next_bit        ;loop back and test next bit
;
; Display a '1' because the bit is set
left_bit_set:
        mov     al,'1'          ;character to be displayed
        call    tty             ;goto display procedure
;
; Shift left ah bits
next_bit:
        shl     ah,1            ;shift left one bit position
        loop    test_left_bit   ;continue until cx=0
;
;**************************************
;       exit to DOS                   *
;**************************************
dos_exit:
        mov     ah,76           ;MS-DOS service request code
        mov     al,0            ;no error code returned
        int     21H             ;goto dos
;******************************************************************************
;                               procedures
;******************************************************************************
tty     proc    near
; uses BIOS teletype service number 14 of INT 10H
; on entry: al = ASCII character to be displayed
; on exit : nothing, ax is preserved
;
        push    ax              ;save ax on the stack
        mov     ah,14           ;service request number
        mov     bx,0            ;display page (usually 0)
        int     10H             ;goto BIOS
        pop     ax              ;restore ax from the stack
        ret                     ;end of procedure
tty     endp
;******************************************************************************
code    ends
        end     entry_point     ;reference to label at which execution starts


