;******************************************************************************
;******************************************************************************
;                               lesson5.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson5.asm
;   by Mike Gainey
; Start date   : 2/7/92
; Mod. dates   :
;
;   Description  :
; Display a screen text block showing all the characters of the character set
;
;   New operations:
; 1.  Set video mode
; 2.  Clear the screen
; 3.  Set the cursor at any desired screen position
; 4.  Display a formatted block
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
chars_frame     db      1       ;start row
                db      12      ;start column
                db      7       ;attribute
                db      '                    IBM Character set'
                db      0ffh,0ffh       ;skip two lines
                db      '    0  1  2  3  4  5  6  7 '
                db      '    8  9  A  B  C  D  E  F',0ffh
                db      '  ',0c9h,25 dup (0cdh),0cbh
                db      25 dup (0cdh),0bbh,0ffh
                db      '0 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '1 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '2 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '3 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '4 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '5 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '6 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '7 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '8 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '9 ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'A ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'B ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'C ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'D ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'E ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      'F ',0bah,25 dup (20h),0bah
                db      25 dup (20h),0bah,0ffh
                db      '  ',0c8h,25 dup (0cdh),0cah
                db      25 dup (0cdh),0bch,00h
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
; *** test video hardware ***
; Set ES to segment element of address
        mov     ax,0040h        ;segment portion of address
        mov     es,ax           ;ax --> es
        mov     bl,3            ;assume color mode number 3
; Load byte into AL using segment override
        mov     al,es:[0010h]   ;offset portion using es
; Test bits 4 and 5 of AL to determine video hardware
        mov     ah,al           ;copy equipment byte in AH
        and     ah,00110000b    ;logical AND with mask
        cmp     ah,00110000b    ;compare with monochrome pattern
        jne     video_mode      ;system is in color
        mov     bl,7            ;change to monochrome mode (7)
;
; *** set video mode ***
; Video mode is set using BIOS service number 0, INT 10H
video_mode:
        mov     al,bl           ;display mode to AL
        mov     ah,0            ;service request number
        int     10h             ;goto BIOS
;
; *** clear video display ***
        call    clear_screen    ;local procedure
;
; *** display frame ***
        lea     si,chars_frame  ;pointer to graphics frame
        call    show_block
;
; *** display characters ***
        mov     dh,6            ;row address for cursor
        mov     dl,16           ;column
        mov     al,1            ;first character to display
;
; *** display one column ***
do_column:
        call    set_cursor      ;local procedures to set cursor
        call    show_chr        ;and display one character
        inc     al              ;next character to display
;
; *** check for last character ***
        cmp     al,0ffH         ;last character
        je      exit_char_set   ;end routine if AL = FFh
        inc     dh              ;bump counter to next row
        cmp     dh,21           ;test for last row
        je      next_column     ;go to next column
        jmp     do_column       ;continue along column
;
; *** index to next column ***
next_column:
        cmp     dl,37           ;last column of first area
        je      frame_area_2    ;go to 1st char. of second area in frame
; Index to next column by adding 3 to the column counter
        add     dl,3            ;distance between columns
        mov     dh,5            ;top row of next column
        jmp     do_column       ;column display routine
;
; *** second frame area ***
frame_area_2:
        add     dl,6            ;add six columns to counter
        mov     dh,5            ;row counter to first row
        jmp     do_column       ;to column routine
;
; *** end of display ***
exit_char_set:
; Set cursor at bottom of screen
        mov     dh,22           ;cursor row
        mov     dl,0            ;cursor column
        call    set_cursor      ;local procedure
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
;
clear_screen    proc    near
; Clear the video display using BIOS service 6, INT 10
        mov     ah,06           ;service request
        mov     al,0            ;code to blank entire window
        mov     bh,07           ;normal attribute
        mov     cx,0            ;start at row 0, column 0
        mov     dh,24           ;end at row 24
        mov     dl,79           ;column 79
        int     10h             ;goto BIOS
clear_screen    endp
;******************************************************************************
set_cursor      proc    near
; Procedure to set the system cursor using BIOS int 10h, #2
        push    ax              ;save AX register
        mov     bh,0            ;display page 0
        mov     ah,02           ;BIOS service request number
        int     10h             ;goto BIOS
        pop     ax              ;restore AX
        ret
set_cursor      endp
;******************************************************************************
show_block      proc    near
; display a pre-formatted block message
        mov     dh,[si]         ;get row from header
        inc     si              ;point to start column byte
        mov     dl,[si]         ;column
        mov     cl,dl           ;save start column in CL
        call    set_cursor      ;local procedure to set cursor
        inc     si              ;point to attribute byte
        mov     bl,[si]         ;attribute to BL
        inc     si              ;bump pointer to message text
get_and_show:
        mov     al,[si]         ;text character to AL
; Test for embedded control codes
        cmp     al,00h          ;message terminator code
        je      show_end        ;exit if terminator found
        cmp     al,0ffh         ;test for new-line code
        je      line_end        ;go to new line routine
; At this point, AL holds a character to be displayed
        call    show_chr        ;local procedure
        inc     dl              ;next column
;
; *** bump cursor and text pointer ***
bump_ptrs:
        call    set_cursor      ;local procedure
        inc     si              ;bump message pointer
        jmp     get_and_show
;
; *** end of line ***
line_end:
        inc     dh              ;bump row pointer
        mov     dl,cl           ;reset column to start column
        jmp     bump_ptrs       ;prepare for next character
show_end:
        ret
;
show_block      endp
;******************************************************************************
;
show_chr        proc    near
; Display character in AL, attribute in BL, at cursor
        push    ax              ;save general purpose registers
        push    bx
        push    cx
        push    dx
        mov     ah,9            ;service request number
        mov     bh,0            ;display page
        mov     cx,1            ;repetition factor
        int     10h             ;goto BIOS
        pop     dx              ;restore registers
        pop     cx
        pop     bx
        pop     ax
        ret
show_chr        endp
;******************************************************************************
;
code    ends
        end     entry_point     ;reference to label where execution starts



