;******************************************************************************
;******************************************************************************
;                               lesson8.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson8.asm
;   by Mike Gainey
; Start date   : 2/17/92 (for Mike)
; Mod. dates   :
;
; Description  :  Simulation of an electronic billboard on the top screen
;                 line using direct access to the video buffer.  Executable
;                 file must be a command-type (COM.) program.
;                 more info on p. 142
;******************************************************************************
;                               code segment
;******************************************************************************

code    segment

        org     0100h           ;COM file forced origin

        assume  cs:code,ds:code,es:code,ss:code

entry:
        jmp     install

;******************************************************************************
;                               code segment data
;******************************************************************************
old1c_add       dw      ?               ;offset of old vector
old1c_seg       dw      ?               ;segment of old vector

video_base      dw      0b000h          ;address of video buffer

; video buffer for holding the message to be displayed
msg_buffer      db      320 dup (00h)
                dw      0ffffh

; text to be moved into the message buffer
bill_msg        db      ' ***** Hamburger $2.35 ** Cheeseburger'
                db      ' $3.00  **  Soft drinks $0.45 ** THANK'
                db      ' YOU ** ',00h

last_pos        dw      0               ;last offset of the video buffer
                                        ;for starting display
toggle_ctrl     db      0               ;on/off control to slow display

;******************************************************************************
;                       interrupt 1CH intercept routine
;******************************************************************************
hex1c_int:                              ;interrupts on
        sti                             ;re-enable interrupts

; save all registers used by the interrupt intercept routine
; including the ES segment register
        push    ax
        push    cx
        push    si
        push    di
        push    es

; the byte at toggle_ctrl is changed to 0 or 1 in each iteration
; display function is bypassed when value is 1, thereby slowing
; down the animation
        mov     al,cs:toggle_ctrl       ;get byte
        cmp     al,1                    ;test for skip value
        je      skip_iter               ;skip display of this iteration

; at this point toggle_ctrl = 0.  change to 1 and execute display operation
        mov     cs:toggle_ctrl,1        ;toggle to 1
        jmp     display_it              ;go to display routine

skip_iter:
        mov     cs:toggle_ctrl,0        ;toggle to 0
        jmp     restore_and_exit

; *** update starting location ***
display_it:
; set DS to video buffer address
        mov     ax,cs:video_base        ;segment address of video buffer
        mov     es,ax
; calculate address of the video buffer where display will start
; using the value stored at last_pos
        inc     cs:last_pos             ;bump start position in buffer
        lea     si,cs:msg_buffer        ;pointer to buffer
        mov     ax,cs:last_pos          ;offset of this display position
        add     ax,ax                   ;double the offset
        cmp     ax,160                  ;test for last position in the buffer
        jne     ok_show                 ;go if not the last byte
        mov     cs:last_pos,0           ;reset counter
ok_show:
        add     si,ax                   ;add offset of this starting
                                        ;position to start of buffer

; *** display buffer ***
        mov     di,0                    ;first screen row and column
        mov     cx,80                   ;80 words to move
display_80:
        mov     ax,word ptr cs:[si]     ;get source word
        mov     word ptr es:[di],ax     ;display it
        add     si,2                    ;bump pointers twice
        add     di,2
        loop    display_80

; *** exit routine ***
restore_and_exit:
        pop     es                      ;restore ES segment
        pop     di                      ;and general registers
        pop     si
        pop     cx
        pop     ax

; exit from new service routine to old service routine
        jmp     dword ptr cs:old1c_add

;******************************************************************************
;                               installation routine
;******************************************************************************
comment |  Installation consists of the following steps:
           1.  Obtain address of original service routine for int 1Ch
               and save in the new handler's data space
           2.  Install the address of the new intercept routine in the
               interrupt vector table
           3.  Save base address of the video buffer in a memory variable
               for use by the service routine
           4.  Initialize the buffer containing the message to be displayed
               by the service routine
           5.  Protect the installed handler and return control to MS-DOS     |

install:
        cli

; *** get address of old handler ***
; uses DOS int 21h #53 to obtain the original address for int 1Ch
        mov     ah,53                   ;service request code
        mov     al,1ch                  ;code of vector desired
        int     21h                     ;goto DOS
; ES --> segment address of installed interrupt handler
; BX --> offset  address of installed interrupt handler
        mov     cs:old1c_add,bx         ;store offset in variable
        mov     cs:old1c_seg,es         ;store segment base

; *** install new handler ***
; take over timer tick at int 1Ch using int 21h #37
        mov     ah,37                   ;service request number
        mov     al,1ch                  ;interrupt to be intercepted
        lea     dx,cs:hex1c_int         ;pointer to handler
        int     21h                     ;goto DOS

; *** store base address of video buffer ***
; default base is B000h.  test system and change base if video is color
        mov     ax,0040h                ;segment base of BIOS data area
        mov     es,ax                   ;to ES
        mov     ax,es:[0010h]           ;get equipment word at offset 0010h
        and     ax,00110000b            ;mask off all bits except 4 and 5
        cmp     al,00110000b            ;test for xx11 xxxx pattern
        je      mono_sys                ;monochrome card installed
; color system - change base address of video buffer (video_base) to B800h
        mov     ax,0b800h               ;screen address of color systems
        mov     cs:video_base,ax        ;store screen address

; *** initialize message buffer ***
; format the buffer msg_buffer with blanks and normal attributes
mono_sys:
        mov     al,20h                  ;character (blank)
        mov     ah,07h                  ;normal display attribute
        mov     di,offset cs:msg_buffer
        mov     cx,160                  ;total words in buffer
to_buffer:
        mov     cs:[di],ax              ;word to buffer
        add     di,2                    ;bump pointer to next address
        loop    to_buffer               ;repeat 160 times
; move 3 copies of the message to be displayed into the display buffer
        lea     di,cs:msg_buffer        ;pointer to buffer
put_msg:
        mov     si,offset cs:bill_msg   ;text message
        lea     si,cs:bill_msg          ;message to be displayed
        mov     cx,80                   ;80 bytes long
put_80:
        mov     al,cs:[si]              ;get message character
        mov     cs:[di],al              ;move character into buffer
        add     di,2                    ;bump video buffer pointer
        inc     si                      ;and message pointer
        cmp     byte ptr cs:[di],0ffh   ;FFh = end of buffer mark
        je      msg_end
        loop    put_80                  ;place 80 characters
        jmp     put_msg

; *** protect routine and exit ***
; exit to DOS protecting the service routine above the label 'install'
; using DOS int27h
msg_end:
        sti
        mov     dx,offset cs:install    ;start protection
        inc     dx                      ;add 1 byte
        int     27h                     ;MS-DOS service
code    ends
        end     entry
