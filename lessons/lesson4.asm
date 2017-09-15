;******************************************************************************
;******************************************************************************
;                               lesson4.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson4.asm
;   by Mike Gainey
; Start date   : 2/6/92
; Mod. dates   :
;
;   Description  :
; Display a screen message using a BIOS service
;
;   New operations:
; 1.  Display messages using BIOS service number 14, INT 10H
; 2.  Read byte in BIOS data area
; 3.  Test data byte for auxiliary equipment and display messages
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
prog_message    db      'Program to identify optional equipment'
                db      ' installed in the microcomputer'
                db      0ah,0dh         ;new line
                db      'by examining the BIOS data stored at'
                db      ' address 00410H'
                db      0ah,0dh         ;new line
                db      0               ;message terminator byte
;
; note: 0ah = line-feed, 0dh = carriage-return, 0h = message terminator
;
math_yes        db      'Math coprocessor present',0ah,0dh,0h
math_no         db      'No math coprocessor',0ah,0dh,0h
color_video     db      'Color video hardware',0ah,0dh,0h
mono_video      db      'Monochrome video hardware',0ah,0dh,0h
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
; Set-up for pointer for display procedure
        lea     si,prog_message         ;set si as pointer to message
        call    display_msg             ;local display procedure
;
; Get BIOS data at 0040:0010H (refer to p. 71 for bit assignments)
;
; Set ES to the segment element of the logical address (0040:0010H)
        mov     ax,0040H        ;segment portion of the address
        mov     es,ax           ;ax --> es
;
; Load byte into AL using segment override
        mov     al,es:[0010H]   ;offset portion using ES
;
; Math coprocessor? -- Assume "no," set message pointer accordingly
        lea     si,math_no      ;message pointer 'no math coprocessor'
        test    al,00000010b    ;bit 1 = math coprocessor status
        jz      math_message    ;go if bit is clear (no math coprocessor)
; Math coprocessor detected -- change message pointer
        lea     si,math_yes     ;message pointer 'math coprocessor present'
math_message:
; Display math coprocessor message (pointer is already set)
        push    ax              ;save equipment byte in stack
        call    display_msg     ;local display procedure
        pop     ax              ;get equipment byte back
;
; Video monitor: color or monochrome? (assume color)
        lea     si,color_video  ;message for color system
        mov     ah,al           ;copy equipment byte in AH (save orig. in al)
        and     ah,00110000b    ;logical AND with mask (preserve bits 4,5)
        cmp     ah,00110000b    ;compare with monochrome pattern
        jne     video_message   ;system is in color
; System is monochrome -- change message pointer
        lea     si,mono_video   ;message pointer 'monochrome ...'
video_message:
; Display monitor message(pointer is already set)
        push    ax              ;save equipment byte in stack
        call    display_msg     ;local display procedure
        pop     ax              ;get equipment byte back
;
; Wait for keystroke: BIOS service number 0, INT 16H
        mov     ah,0            ;service request number
        int     16h             ;transfer control to BIOS
; After the user presses any key the program ends by returning to DOS
;
;
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
display_msg     proc    near
;
; Display a message stored in the data segment using
; BIOS service number 14, INT 10H (BIOS teletype write)
;   entry:  SI = start of message to be displayed
;   exit :  nothing
;
; BIOS service requirements:
;   AH = 14 (request number)
;   AL = character to be displayed
;   BH = display page (normally 0)
;   BL = foreground color in the graphics mode (normally 0)
;
        mov     bx,0            ;display page and not graphics mode
        mov     ah,14           ;service request number
display_one:
        mov     al,[si]         ;message character --> AL
        cmp     al,0            ;test for terminator code
        jne     display_char    ;display character if not 0
        ret                     ;end of message/loop
;
; AL = character to be displayed
display_char:
        push    si              ;save SI on the stack (why?)
        int     10H             ;goto BIOS (int 10, #14)
        pop     si              ;restore SI
        inc     si              ;bump pointer to next character
        jmp     display_one     ;repeat processing (get next character)
display_msg     endp
;******************************************************************************
code    ends
        end     entry_point     ;reference to label where execution starts



