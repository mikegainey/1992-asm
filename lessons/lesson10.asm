;******************************************************************************
;******************************************************************************
;                               lesson10.asm
;******************************************************************************
;******************************************************************************
; Program title:  lesson10.asm  (p. 179-189)
;   by Mike Gainey
; Start date   :  2/27/92 (for Mike)
; Mod. dates   :
;
comment |

Program Description:
  Request a path and filename from the user, open the specified disk file,
  and display its contents.

New Operations:
  1.  Set the disk transfer area (DTA) to the program's memory space
  2.  Open a disk file and obtain the file handle
  3.  Handle errors returned by an MS-DOS service
  4.  Read data from a disk file into a buffer
  5.  Close a disk file using its handle
|
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

;***** text message strings *****
input_msg       db      'Enter path or filename: $'
no_handle       db      0ah,0dh,'ERROR - file not opened'
                db      0ah,0dh,'$'

;***** pathname buffer and data *****
; Buffer formatted for int 21h #10 to read a string of keyboard characters
path_buffer     db      28              ;maximum charcters allowed
                db      0               ;characters actually entered
path_name       db      30 dup(0)       ;buffer storage area

file_handle     dw      0               ;storage for file handle

;***** disk transfer area *****
local_dta       db      128 dup (0)     ;program's disk transfer area

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

;***** set DTA *****
; Set the system's disk transfer area to the program's data space
        lea     dx,local_dta    ;pointer to program's DTA
        call    set_dta         ;local procedure

;***** clear screen and display message *****
        call    clear_screen    ;local procedure

; Set cursor at bottom screen row
        mov     dl,0            ;first column
        mov     dh,24           ;last row
        call    set_cursor      ;local procedure

; Display entry message
        lea     dx,input_msg    ;pointer to message string
        call    dos_display     ;local procedure

;***** open file and get handle *****
        mov     bl,0            ;code for open file function
        lea     dx,path_buffer  ;pointer to buffer area
        call    open_create     ;local procedure

; At this point, the carry flag is set if the open operation failed
        jnc     file_ok         ;go if no carry

;***** ERROR -- file not opened *****
; Display error message and exit
        lea     dx,no_handle    ;pointer to error message
        call    dos_display     ;local procedure
        jmp     short dos_exit  ;end execution

;***** file opened *****
; At this point, the file was successfully opened; AX = file handle
file_ok:
        mov     file_handle,ax  ;store handle in variable (memory)

;***** read one sector *****
new_sector:
        mov     bx,file_handle  ;handle from open function
        lea     dx,local_dta    ;128 byte buffer area
        call    read_128        ;local procedure

; AX hold the number of bytes read into the buffer

; AX = 0 is read operation found the end of file
        cmp     ax,0            ;test for end of file
        jne     not_eof         ;go if not at end

;***** end of file exit *****
; At this point, the end of file was reached.  Close file and end execution.
        mov     bx,file_handle  ;handle from open operation
        call    close_file      ;local procedure
        jmp     short dos_exit

;***** display file data *****
not_eof:
        mov     cx,ax           ;CX counts number of bytes to display
        lea     si,local_dta    ;pointer to buffer area holding disk data

display_data:
        mov     al,[si]         ;get buffer byte

; Test for invalid codes:  < 0Ah
        cmp     al,0ah          ;less than 10h are control codes
        jb      is_control      ;go if character < 10h
        push    cx              ;save byte counter
        push    si              ;save buffer pointer
        call    tty             ;local procedure
        pop     si              ;restore registers
        pop     cx

is_control:
        inc     si              ;bump buffer pointer
        loop    display_data

; At this point, the data in the buffer has been displayed
        jmp     new_sector

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
; Local procedure to display a character using BIOS teletype service
; int 10h #14
        push    ax              ;save AX in stack
        mov     ah,14           ;service request number
        mov     bx,0            ;display page is usually 0
        int     10h             ;BIOS video service interrupt
        pop     AX              ;restore AX from stack
        ret                     ;end of procedure
tty     endp

;******************************************************************************
clear_screen    proc    near
; Clear the video display using BIOS int 10h #6
; On entry:  nothing
        mov     ah,06           ;service request number
        mov     al,0            ;code to blank entire window
        mov     bh,07           ;use normal attribute
        mov     cx,0            ;start at row 0, column 0
        mov     dh,24           ;end at row 24
        mov     dl,79           ;column 79
        int     10h             ;BIOS video service
        ret
clear_screen    endp

;******************************************************************************
dos_display     proc    near
; Display a formatted text string using DOS int 21h #9
; On entry:  DX --> string terminated with '$'
; On exit :  string is displayed at current cursor position
        mov     ah,9            ;service request number
        int     21h             ;MS-DOS service interrupt
        ret
dos_display     endp

;******************************************************************************
set_cursor      proc    near
; Procedure to set the system cursor using BIOS int 10h #2
; assumes text mode is active and screen is 80x25
; On entry:  DH = desired cursor row    (0 - 24)
;            DL = desired cursor column (0 - 79)
; En exit :  system cursor is positioned
        push    ax              ;save AX register
        mov     bh,0            ;display page 0
        mov     ah,02           ;BIOS service request number
        int     10h             ;interrupt for BIOS service
        pop     ax              ;restore AX
        ret
set_cursor      endp

;******************************************************************************
set_dta proc    near
; Set memory area to be used by DOS as disk transfer area
; On entry:  DX --> 128 byte buffer to be used as DTA
; On exit :  carry clear
        mov     ah,26           ;DOS service request
        int     21h
        ret
set_dta endp

;******************************************************************************
open_create     proc    near
; Uses DOS buffered keyboard input service to input a filename, to open a file,
; and to return a 16 bit file handle that can be used to access the file
; refer to p. 186 for more information

;***** buffered input from keyboard *****
        push    bx              ;save exit/create switch
        mov     ah,10           ;DOS service request number
        push    dx              ;save buffer start address
        push    dx              ;twice
        int     21h             ;goto DOS

; Filename now in buffer.  Set a null byte in buffer to create an ASCIIZ string
; for the PATH or FILENAME
        pop     di              ;recover buffer start
        inc     di              ;to input count byte
        mov     al,[di]         ;# characters in buffer
        mov     ah,0            ;prepare for addition
        add     di,ax           ;DX --> CR byte
        inc     di              ;one more
        mov     byte ptr [di],0 ;set null byte terminator

; Recover buffer address and open/create switch
        pop     dx              ;buffer start
        add     dx,2            ;index to path or filename
        pop     bx              ;open/create switch
        cmp     bl,0            ;test for open switch
        je      open_f
        jmp     short create_f

;***** open *****
open_f:
; This routine opens a file for read/write access if the filename is contained
; in the form of an ASCIIZ string pointed to by DX
        mov     ah,61           ;DOS function:  open file (handle mode)
        mov     al,2            ;read/write access
        int     21h

; Carry set if open failed
; If carry clear, file is open and AX = file handle
; BL (in stack) holds exit/create switch

        ret

;***** create *****
create_f:
        mov     cx,0            ;normal access
        mov     ah,60           ;DOS service request
        int     21h

; If carry clear, AX = handle for new file
; If carry set, create function failed

        ret
open_create     endp

;******************************************************************************
close_file      proc    near
; Close file using file handle
; On entry:  BX = file handle
; On exit :  carry clear if operation successful -- file closed
;            carry set if operation failed -- invalid handle or file not open
        mov     ah,62           ;DOS service request
        int     21h
        ret
close_file      endp

;******************************************************************************
read_128        proc    near
; Read 128 bytes from an open file into buffer using the file handle.  This
; procedure assumes that the file has been previously opened or created using
; the procedure 'open_create'
; On entry:  BX = file handle
;            DX = 128 byte user buffer
; On exit :  carry clear if operation successful
;              AX = # bytes read into buffer
;              AX = 0 if end of file
;            carry set if operation failed
;              AX = error code
;                   5 = access denied
;                   6 = invalid handle or file not open
        push    cx              ;save entry CX
        mov     ah,63           ;DOS service request
        mov     cx,128          ;bytes to read
        int     21h
        pop     cx              ;restore
        ret
read_128        endp
;******************************************************************************
;
code    ends
        end     entry_point     ;reference to label where execution starts



