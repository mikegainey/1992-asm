;keyboard reading experiment (3/31/92)

        .model small
        .stack

        .data   ;--------------------------------------------------------------
clrscrn db      stm,cls,eom
v2a_in  dw      ?
v2a_out db      20 dup(?)
cr      db      pos,0,0,'   ',pos,0,0,eom
is_ext  db      pos,0,8,'extended    ',eom
not_ext db      pos,0,8,'not extended',eom

        .code   ;--------------------------------------------------------------
        extrn   val2asc:proc, print:proc
        include macs.inc
        include bios.inc

entry:
        mov     ax,@data
        mov     ds,ax
        jmp     exit
        display clrscrn

start:

;        @setcurpos      5,0,0

gkey:
        mov     ah,0                    ;DOS: character input w/ echo
        int     16h
        jmp     short gkey
        display not_ext

        cmp     al,0                    ;check for extended characters
        jne     skipext
        display is_ext

        int     21h                     ;get extended ascii code

skipext:
        cmp     al,'q'
        je      exit
        xor     ah,ah                   ;AX = AL
        mov     v2a_in,ax
        mov     si,offset v2a_in
        mov     di,offset v2a_out
        mov     bl,10
        call    val2asc

        display cr
        display v2a_out
        jmp     short start

exit:
        mov     ah,76
        mov     al,0
        int     21h
        end
