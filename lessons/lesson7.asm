;******************************************************************************
;******************************************************************************
;                               lesson7.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson7.asm (p. 123)
;   by Mike Gainey
; Start date   : 2/10/92 (for Mike)
; Mod. dates   :
;
;   Description  :
; Display a screen text block showing all the characters in the IBM extended
; character set using direct access to the video buffer.
;   New operations:
; 1.  Set the ES register to the video buffer base address
; 2.  Clear the screen using direct access
; 3.  Use of a pointer register for direct access operations
; 4.  Display a formatted message using direct screen access
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
; Message block for the show_block procedure (see p. 124)
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
;
        mov     ax,0040h        ;segment portion of address
        mov     es,ax           ;segment to ES
        mov     bl,3            ;assume color mode #3
;
        mov     dx,0b800h       ;base accress of color video
;
        mov     al,es:[0010h]   ;offset portion of color video
        mov     ah,al           ;copy equipment byte into AH
        and     ah,00110000b    ;compare with pattern for monochrome system
        jne     video_mode      ;system is color
;
        mov     bl,7            ;change to monochrome mode
        mov     dx,0b000h       ;base address of monochrome video
;
video_mode:
        push    dx              ;save video base address
        mov     al,bl           ;display mode to AL
        mov     ah,0            ;service request number
        int     10h             ;goto BIOS (video interrupt)
        pop     dx              ;restore base address
;
        mov     es,dx           ;load ES with video base address
;
        call    clear_direct    ;clear screen (local procedure)
;
;
; *** display frame ***
;
        lea     si,chars_frame  ;pointer to graphics frame
        call    show_direct     ;local procedure
;
        mov     dh,6            ;row address for cursor
        mov     dl,16           ;column
        mov     al,1            ;first character to display
;
do_column:
        call    set_di          ;local procedures to set cursor
;
        mov     Byte Ptr es:[di],al     ;store character in video buffer
        inc     al                      ; next character to display
;
        cmp     al,0ffh         ;last character
        je      reset_and_exit  ;end routine if AL = 0FFh
        inc     dh              ;bump counter to next row
        cmp     dh,21           ;test for last row
        je      next_column     ;go to next column
        jmp     do_column       ;continue along column
;
next_column:
        cmp     dl,37           ;last column of first area
        je      frame_area_2    ;go to first column of second area in frame
;
        add     dl,3            ;distance between columns
        mov     dh,5            ;top row of next column
        jmp     do_column       ;column display routine
;
frame_area_2:
        add     dl,6            ;add six columns to counter
        mov     dh,5            ;row counter to first row
        jmp     do_column       ;to column routine
;
reset_and_exit:
        mov     bh,0            ;display page 0
        mov     ah,2            ;service request number
        mov     dh,23           ;cursor row
        mov     dl,0            ;cursor column
        int     10h             ;BIOS video interrupt
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
;   Clear the video display using direct access to the video buffer
;     ES = video buffer base address
clear_direct    proc    near
        push    ax              ;save entry registers
        push    cx
        push    di
        mov     di,0            ;start at offset 0 in buffer
        mov     al,20h          ;blank character
        mov     ah,7            ;normal display attribute
        mov     cx,2000         ;repeat 2000 times (80 x 25 = 2000)
clear_2000:
        mov     es:[di],ax      ;store character and attribute
        inc     di              ;bump buffer pointer (skip character)
        inc     di              ;bump buffer pointer (skip attribute)
        loop    clear_2000
        pop     di              ;restore entry registers
        pop     cx
        pop     ax
        ret
clear_direct    endp
;
;
;   Set DI to the offset address in the video buffer that corresponds with a
;   screen position expressed in terms of a row and column
;     DH = screen row (0 - 24), DL = screen column (0 - 79)
set_di          proc    near
        push    ax              ;save accumulator
        push    dx              ;and DX
        mov     al,160          ;bytes per row
        mul     dh              ;AX = 160 * DH
        mov     di,ax           ;save partial result in DI
        mov     al,2            ;bytes per column
        mul     dl              ;AX = DL * 2
        add     di,ax           ;add previous result
        pop     dx              ;restore DX
        pop     ax              ;restore accumulator
        ret
set_di          endp
;
;
;   Display a formatted block message using direct access to the video buffer
;     see p. 130 for format parameters
show_direct     proc    near
        push    ax              ;save entry registers
        push    cx
        push    dx
        mov     dh,[si]         ;get row
        inc     si              ;point to start column byte
        mov     dl,[si]         ;get column
        mov     cl,dl           ;save start column in CL
        call    set_di          ;set DI to offset in buffer
        inc     si              ;point to attribute byte
        mov     ah,[si]         ;attribute --> AH
        inc     si
get_and_show:
        mov     al,[si]         ;get character
;
        cmp     al,00h          ;terminator (end of message)
        je      show_end
        cmp     al,0ffh         ;end of line
        je      line_end
;
        mov     es:[di],ax      ;store (display) character and attribute
        inc     dl              ;next column
;
bump_ptrs:
        call    set_di          ;next screen position
        inc     si              ;bump message pointer
        jmp     get_and_show
;
line_end:
        inc     dh              ;bump row pointer
        mov     dl,cl           ;reset column to start column
        jmp     bump_ptrs
;
show_end:
        pop     dx              ;restore registers
        pop     cx
        pop     ax
        clc                     ;no error detection
        ret
show_direct     endp
;
;******************************************************************************
;
code    ends
        end     entry_point     ;reference to label where execution starts



