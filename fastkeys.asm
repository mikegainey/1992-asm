comment |----------------------------------------------------------------------

Uses BIOS int 16h #3 to speed up the keyboard response.
  delay = 250ms    repeat rate = 30 chars/sec

------------------------------------------------------------------------------|

code    segment

        assume cs:code

        org     0100h           ;com programs need this

entry:
        jmp     start           ;jump over data

;----- message data -----
;------------------------------------------------------------------------------
m1      db      'FASTKEYS -- a keyboard speed-up program by Mike Gainey '
        db      '(2/26/92)',0dh,0ah
m1_len  equ     $-m1            ;length of 'm1' message

m2      db      '            repeat delay = 250ms    repeat rate = 30.0 '
        db      'chars/sec',0dh,0ah
m2_len  equ     $-m2            ;length of 'm2' message
;------------------------------------------------------------------------------

;----- program code -----
start:
        mov     ah,3            ;int 16h #3
        mov     al,5            ;the book says to do this

        mov     bh,0            ;repeat delay = 250ms
        mov     bl,0            ;repeat rate  = 30 chars/sec

        int     16h             ;goto BIOS (set repeat rate)
                      
        mov     ah,40h          ;DOS function 40h = write file or device
        mov     bx,1            ;BX = standard output handle
        mov     cx,m1_len       ;message 'm1' length
        mov     dx,offset m1    ;set DX register
        int     21h             ;goto DOS function 40h (write file or device)

        mov     ah,40h          ;DOS function 40h = write file or device
        mov     bx,1            ;BX = standard output handle
        mov     cx,m2_len       ;message 'm2' length
        mov     dx,offset m2    ;set DX register
        int     21h             ;goto DOS function 40h (write file or device)

        mov     ah,76           ;exit to DOS
        mov     al,0
        int     21h

code    ends
        end     entry

