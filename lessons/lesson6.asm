;******************************************************************************
;******************************************************************************
;                               lesson6.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson6.asm  (p. 104)
;   by Mike Gainey
; Start date   : 2/9/92 (for Mike)
; Mod. dates   :
;
;   Description:
; The program clears the screen and allows the user to type text at the
; bottom screen line.  The code recognizes and executes the <Tab>, <Backspace>,
; and <Enter> keys.  The <Enter> key sends the bottom screen line to the
; printer and scrolls the text up one line
;
;   New operations:
; 1.  Read the character typed and process according to the character code
; 2.  Store text in a data buffer
; 3.  Print characters
; 4.  Scroll a screen window
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
;
typesim_msg     db      '** A Typewriter Simulation Program **'
                db      0ah,0dh
                db      '   Press:',0ah,0dh
                db      '        <F1> key to end',0ah,0dh
                db      '        <Tab> for 10 spaces',0ah,0dh
                db      '        <Enter> to print line',0ah,0dh
                db      '$'
;
bad_printer     db      '  *** PRINTER ERROR ***$'
;
; Buffer for text in one screen line
line_buffer     db      80 dup (0h)
;
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
; *** clear video display ***
        call    clear_screen    ;local procedure
;
; *** set cursor at screen top ***
        mov     dh,0            ;top screen row
        mov     dl,0            ;leftmost screen column
        call    set_cursor      ;local procedure
;
; *** display greeting ***
        lea     dx,typesim_msg  ;pointer to message text
        call    dos_display     ;local procedure
;
; *** set cursor at screen bottom ***
        mov     dh,24           ;last screen row
        mov     dl,0            ;leftmost screen column
        call    set_cursor      ;local procedure
;
; *** cascade processing of typed characters ***
get_keystroke:
        mov     ah,0            ;service request
        int     16H             ;goto BIOS (get key function)
;
        cmp     ax,3b00h        ;test for <F1> key
        jne     skip1           ;<F1> is not pressed (program mod.)
        jmp     dos_exit        ;end program
skip1:
;
        cmp     al,0dh          ;test for <Enter> key
        je      end_of_line     ;goto end-of-line routine
        cmp     al,09h          ;test for <Tab> key
        je      tab_routine     ;goto tab key routine
;
        call    tty             ;local procedure
        jmp     get_keystroke   ;get next keystroke
;
tab_routine:
        mov     cx,10           ;counter for spaces
tab_10:
        mov     al,' '          ;space character
        call    tty             ;local procedure
        loop    tab_10
        jmp     get_keystroke   ;monitor next keystroke
;
; *** <Enter> pressed ***
end_of_line:
        mov     al,14h          ;paragraph mark to screen
        call    tty             ;local procedure
;
; *** move screen line to buffer ***
        mov     dh,24           ;last screen row
        mov     dl,0            ;first column
        call    set_cursor      ;local procedure
;
        lea     di,line_buffer  ;DI is destination buffer
;
; Get screen character at the current cursor position
get_line:
        push    dx              ;save cursor position
        push    di              ;and buffer pointer
        mov     ah,8            ;service request number
        mov     bh,0            ;page 0
        int     10h             ;goto BIOS
        pop     di              ;restore registers
        pop     dx
; Screen character is returned in AL
;
; Test for end of line
        cmp     al,14h          ;paragraph mark
        je      is_eol          ;end routine on mark
;
; Not end of line, store character in buffer
        mov     [di],al         ;DI --> buffer
        inc     di              ;bump buffer pointer
        inc     dl              ;bump cursor position
        call    set_cursor      ;local procedure
        jmp     get_line        ;continue with next character
;
; *** end of line ***
is_eol:
        mov     al,0dh          ;line feed
        mov     [di],al         ;DI --> buffer
        inc     di              ;bump buffer pointer
        mov     al,0ah          ;carriage return
        mov     [di],al         ;store in buffer
; The character 0Ah marks the end of the text line at print time
;
; *** print buffer line ***
        lea     si,line_buffer  ;SI is source pointer
get_one:
        mov     al,[si]         ;buffer character to AL
        call    print_one       ;local procedure
;
; Test carry flag for printer error
        jnc     printer_ok      ;continue if no carry
;
; *** printer error ***
        lea     dx,bad_printer  ;pointer to message text
        call    dos_display     ;local procedure
        jmp     dos_exit        ;terminate execution
printer_ok:
        inc     si              ;bump buffer pointer
        cmp     al,0ah          ;test for end of text
        jne     get_one         ;continue printing if not 0Ah
;
; *** beep ***
; Use control code 07h to send beep sound to system speaker
        mov     al,07h          ;bell control code
        call    tty             ;local procedure
;
; *** scroll screen ***
        mov     ah,06           ;service request
        mov     al,1            ;code to scroll up one line
        mov     bh,07           ;use normal attribute
        mov     ch,5            ;start at row 5
        mov     cl,0            ;column 0
        mov     dh,24           ;end at row 24
        mov     dl,79           ;column 79
        int     10h             ;goto BIOS
; Move cursor to start of line
        mov     al,0dh          ;carriage return code
        call    tty             ;local procedure
        jmp     get_keystroke
;**************************************
;       exit to DOS                   *
;**************************************
dos_exit:
        mov     ah,76           ;MS-DOS service request code
        mov     al,0            ;no error code returned
        int     21H             ;goto dos
;
;******************************************************************************
;                               procedures
;******************************************************************************
clear_screen    proc    near
        mov     ah,06           ;service request
        mov     al,0            ;code to blank entire window
        mov     bh,07           ;use normal attribute
        mov     cx,0            ;start at row 0, column 0
        mov     dh,24           ;end at row 24
        mov     dl,79           ;column 79
        int     10h             ;BIOS video service
        ret
clear_screen    endp
;******************************************************************************
set_cursor      proc    near
        mov     bh,0            ;display page 0
        mov     ah,02           ;BIOS service request number
        int     10h             ;goto BIOS
        ret
set_cursor      endp
;******************************************************************************
tty             proc    near
        push    ax              ;save AX in stack
        mov     ah,14           ;service request number
        mov     bx,0            ;display page is usually 0
        int     10h             ;BIOS video service interrupt
        pop     ax              ;restore AX from stack
        ret
tty             endp
;******************************************************************************
dos_display     proc    near
        mov     ah,9            ;service request number
        int     21h             ;goto DOS
        ret
dos_display     endp
;******************************************************************************
print_one       proc    near
;
; *** test for ready ***
        push    ax              ;save character
        push    si              ;and pointer buffer
        mov     ah,2            ;BIOS service request
        mov     dx,0            ;printer is number 0
        int     17h             ;BIOS interrupt
; At this point, bit 3 of AH is set if the printer is not ready
        test    ah,00001000b    ;test status bit number 3
        jz      send_chr        ;print character if bit is clear
        pop     si              ;restore registers
        pop     ax
        stc                     ;set carry flag to indicate printer error
        ret
;
; *** send character ***
send_chr:
        mov     ah,0            ;BIOS service request
        mov     dx,0            ;printer is number 0
        int     17h             ;BIOS interrupt
        pop     si
        pop     ax
        ret
print_one       endp
;******************************************************************************
code	ends
        end     entry_point     ;reference to label where execution starts
