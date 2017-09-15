;******************************************************************************
;******************************************************************************
;                               lesson2.asm
;******************************************************************************
;******************************************************************************
; Program title: lesson2.asm (from IBM Microcomputer Assembly Language ...)
;   by Mike Gainey
; Start date   : 2/6/92 (for Mike)
; Mod. dates   :
;
; Description  : displays a screen message (p. 30-32)
;
; Program operations:
; 1.  Initializes data segment
; 2.  Display a message using DOS service number 9, INT 21H
; 3.  Return control to DOS using service number 76, INT 21H
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
greeting        db      'Hello, I am your computer!$'
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
        lea     dx,greeting     ;set dx to point to text string
        mov     ah,9            ;load ah with DOS function number
        int     21H             ;goto DOS function
;**************************************
;       exit to DOS                   *
;**************************************
dos_exit:
        mov     ah,76           ;MS-DOS service request code
        mov     al,0            ;no error code returned
        int     21H             ;goto dos
;
code    ends
        end     entry_point     ;reference to label where execution starts
