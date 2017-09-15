 comment *----------------------------------------------------------------------

program title:  syslog.asm  by Mike Gainey (4/1/92 - 4/13/92)

modifications:

description:

  Unisys System Log.

Mike Gainey
307 Christina Lane
Friendswood, TX  77546

        *----------------------------------------------------------------------
slmac   macro   inp,outp,len,chr        ;macro form setlen local procedure

        mov     ax,offset inp           ;get input address
        push    ax                      ;push input address on the stack
        push    outp                    ;put output address on the stack
        mov     ah,len                  ;get fixed output length
        mov     al,chr                  ;get output character ('0',' ', or ...)
        push    ax                      ;pass parameters on the stack
        call    setlen                  ;call local procedure
        endm

a2vchk  macro                           ;checks asc2val output for error cond
        local   eomac                   ;local label
        cmp     [v2a_in+2],0ffffh       ;get error condition word
        jne     eomac                   ;end of macro
        jmp     short error             ;error condition
eomac:
        ----- add mov to recbuf -----
        endm

;------------------------------------------------------------------------------

        dosseg                          ;standard segment order
        .model  small
        extrn   print:proc              ;direct screen output routine
        extrn   val2asc:proc            ;converts word values to ascii
        extrn   asc2val:proc            ;converts ascii values to word values
        include macs.inc                ;misc. macros
        .stack
        .data   ;--------------------------------------------------------------

clrscrn db      stm,cls,eom

form    db      attr,3,ctr,1,'Unisys System Log',ctr
        db      ctr,2,'by Mike Gainey',ctr
        db      attr,2,pos,5,3,'System ( )'
        db      pos,5,27,'Start date (  /  /  )'
        db      pos,5,54,'End Date (  /  /  )'
        db      pos,7,3,'Start time (  :  )'
        db      pos,7,27,'End time (  :  )'
        db      pos,7,54,'Total time (   :  )'
        db      pos,9,3,'Mnemonic (        )'
        db      pos,9,27,'DR # (      )'
        db      pos,9,54,'Crash (y/n)    ( )'
        db      pos,11,3,'Cab. type (    -  )'
        db      pos,11,27,'Serial # (          )'
        db      pos,11,54,'Complete (y/n) ( )'
        db      pos,13,3,'CSE (  )'
        db      pos,15,3,'>'
        db      pos,21,27,'( ) Press <Enter> to save entry.'
        db      attr,3,pos,3,0,201,78 dup(205),187      ;top of box
        db      pos,22,0,200,78 dup(205),188            ;bottom of box
        db      pos,4,0,vert,1,18 dup(186)              ;left side of box
        db      pos,4,79,18 dup(186)                    ;right side of box
        db      vert,0, eom     ;----------------------------------------------

instruct        struc                   ;----- input buffer structure -----
  in_sys        db      ' ',0           ;system
  in_sd         db      '  /  /  ',0    ;start date
  in_ed         db      '  /  /  ',0    ;end date
  in_st         db      '  :  ',0       ;start time
  in_et         db      '  :  ',0       ;end time
  in_tt         db      '   :  ',0      ;total time
  in_mn         db      '        ',0    ;mnemonic
  in_dr         db      '      ',0      ;DR number
  in_cr         db      ' ',0           ;crash (y/n)
  in_cb         db      '    -  ',0     ;cabinet type
  in_sr         db      '          ',0  ;serial number
  in_cp         db      ' ',0           ;complete (y/n)
  in_ce         db      '  ',0          ;CSE
  in_ds         db      432 dup(' '),0  ;description
instruct        ends
in_buf          instruct        <>      ;input buffer
in_temp         instruct        <>      ;input template

bufchr_s        equ     <byte ptr in_buf.[bx+si]>       ;text macro
bufchr_d        equ     <byte ptr in_buf.[bx+di]>       ;text macro
tmpchr_s        equ     <byte ptr in_temp.[bx+si]>      ;text macro
tmpchr_d        equ     <byte ptr in_temp.[bx+di]>      ;text macro

disptemp        db      85 dup('Mike')  ;temporary buffer for 'print' proc.
v2a_in          dw      ?               ;val2asc input word
                dw      ?               ;for asc2val output
v2a_out         db      8 dup(?)        ;val2asc output string (asc2val input)

in_data db      5,11,1,in_sys           ;system
        db      5,39,8,in_sd            ;start date
        db      5,64,8,in_ed            ;end date
        db      7,15,5,in_st            ;start time
        db      7,37,5,in_et            ;end time
        db      7,66,6,in_tt            ;total time
        db      9,13,8,in_mn            ;mnemonic
        db      9,33,6,in_dr            ;DR number
        db      9,70,1,in_cr            ;crash (y/n)
        db      11,14,7,in_cb           ;cabinet
        db      11,37,10,in_sr          ;serial number
        db      11,70,1,in_cp           ;complete (y/n)
        db      13,8,2,in_ce            ;CSE
        db      15,5,0ffh,in_ds,0       ;description

ins_flag        db      0ffh            ;insert flag (00=off, FF=on)
f_row           db      ?               ;current field row
f_col           db      ?               ;current field column
f_len           dw      ?               ;current field length
f_buf           db      ?               ;input buffer field offset
f_num           db      1               ;current field number
f_pos           dw      0               ;field cursor position
f_chr           db      ?               ;input character (temp variable)

recstruct       struc                   ;----- record buffer structure -----
  sys           db      ?               ;system
  sdm           db      ?               ;start date month
  sdd           db      ?               ;start date day
  sdy           dw      ?               ;start date year
  edm           db      ?               ;end date month
  edd           db      ?               ;end date day
  edy           dw      ?               ;end date year
  sth           db      ?               ;start time hour
  stmin         db      ?               ;start time minute (stm=set text mode)
  eth           db      ?               ;end time hour
  etm           db      ?               ;end time minute
  tth           dw      ?               ;total time hours
  ttm           db      ?               ;total time minutes
  mne           db      8 dup(?)        ;mnemonic
  dr            db      6 dup(?)        ;DR number
  crash         db      ?               ;crash (y/n)
  cab           db      7 dup(?)        ;cabinet type
  ser           db      10 dup(?)       ;serial number
  comp          db      ?               ;complete (y/n)
  cse           db      ?,?             ;CSE
  desc          db      432 dup(?)      ;description (6 lines of 76 columns)
  recnum        dw      ?               ;record number
  notused       db      27 dup(?)       ;not used
recstruct       ends
recbuf          recstruct       <>      ;buffer for one record (512 bytes)

                                        ;----- non-extended edit keys
e_key   db      08                      ;backspace
        dw      ek_bksp
        db      09                      ;tab
        dw      ek_tab
        db      13                      ;enter
        dw      ek_enter
        db      27                      ;escape
        dw      ek_esc
        db      0ffh                    ;end marker

                                        ;----- extended edit keys -----
ee_key  db      82                      ;insert
        dw      ek_ins
        db      83                      ;delete
        dw      ek_del
        db      71                      ;home
        dw      ek_home
        db      79                      ;end
        dw      ek_end
        db      73                      ;page up
        dw      ek_pgup
        db      81                      ;page down
        dw      ek_pgdn
        db      72                      ;up arrow
        dw      ek_up
        db      80                      ;down arrow
        dw      ek_dn
        db      75                      ;left arrow
        dw      ek_lt
        db      77                      ;right arrow
        dw      ek_rt
        db      15                      ;shift-tab
        dw      ek_shtab
        db      59                      ;F1
        dw      ek_f1
        db      60                      ;F2
        dw      ek_f2
        db      61                      ;F3
        dw      ek_f3
        db      62                      ;F4
        dw      ek_f4
        db      0ffh                    ;end marker

m_menu  db      attr,3,ctr,23,'F1-Create report  F2-Edit records  '
        db      'F3-Change SYSLOG file  Esc-End program',ctr,attr,2,eom

        .code   ;--------------------------------------------------------------

entry:
        mov     ax,@data                ;get data segment
        mov     ds,ax                   ;setup DS register

        display clrscrn                 ;set text mode, clear the screen

start:
        display form                    ;display record entry form
        display m_menu                  ;display main menu

;----- input a record entry -----

        call    datetime                ;local procedure for today's date/time

        mov     f_num,2                 ;display date/time presets
@@:     call    getfdata                ;get field data (screen position, len.)
        mov     cl,0ffh                 ;so buffers will be re-displayed
        call    disp_buf                ;display the buffer
        cmp     f_num,5                 ;last sub-buffer to display
        je      @f                      ;exit loop if done
        inc     f_num                   ;increment field number
        jmp     short @b                ;loop back until done
@@:

        mov     f_num,1                 ;initialize field number

        mov     ins_flag,0              ;set insert off
        call    sizecursor              ;set cursor size (insert on/off)

get_fdata:
        call    getfdata                ;get field data (local procedure)

getkey:
        call    poscursor               ;position hardware cursor

        mov     ah,0                    ;BIOS function
        int     16h                     ;BIOS (read character from keyboard)

        cmp     al,0                    ;if AL = 0, then get scan code
        je      do_eekey                ;process an extended edit key

        mov     si,offset e_key         ;check for a non-extended edit key
@@:
        cmp     [si],al                 ;look for a match in edit key table
        je      e_match                 ;match found
        add     si,3                    ;point to next table entry
        cmp     byte ptr [si],0ffh      ;FF = end marker
        jne     @b                      ;check next table entry
        jmp     short inp_char          ;not an edit key, process a character

do_eekey:
        mov     si,offset ee_key        ;pointer to extended edit key table
@@:
        cmp     [si],ah                 ;look for a match in e-edit key table
        je      e_match                 ;match found
        add     si,3                    ;point to next table entry
        cmp     byte ptr[si],0ffh       ;FF = end marker
        jne     @b                      ;check next table entry
        jmp     getkey                  ;invalid key pressed

e_match:
        mov     bl,f_buf                ;BL = field offset in buffer
        xor     bh,bh                   ;so BX = BL = field offset in buffer

        inc     si                      ;SI points to address of key handler
        jmp     [si]                    ;jump to edit key handler

;----- not an edit key, process a text character -----
inp_char:
        mov     f_chr,al                ;save character input

        cmp     f_num,15                ;check for 'save entry' field
        jne     @f                      ;if not, continue
        jmp     save_chr                ;if so, goto handler
@@:
        xor     cl,cl                   ;flag: display only the input chr

                                        ; ----- an index pointer -----
        mov     bl,f_buf                ;BL = field offset in buffer
        xor     bh,bh                   ;so BX = BL = field offset in buffer

                                        ; f_len = 1:  bypass insert on routine
        cmp     f_len,1                 ;exception: field length = 1
        je      put_fchr                ;type-over character

                                        ; last pos. in field:  bypass insert
        mov     ax,f_len                ;AX = field length
        dec     ax                      ;AX = last field position
        cmp     f_pos,ax                ;check if f_pos = last position
        je      put_fchr                ;type-over character

        cmp     ins_flag,0ffh           ;FF = insert on
        jne     put_fchr                ;insert off, type-over character

;----- insert on:  make room for a character -----
        mov     cl,0ffh                 ;flag: display entire buffer

        mov     ax,f_len                ;AX = field length
        mov     si,ax                   ;position pointer past field
        sub     si,2                    ;SI = next to last char of field
        mov     di,si                   ;DI = SI
        inc     di                      ;SI = just right of SI

makeroom:
        mov     al,byte ptr in_buf.[bx+si]      ;get the character
        mov     byte ptr in_buf.[bx+di],al      ;move the character

        mov     ax,f_pos                ;AX = field position
        cmp     si,ax                   ;check for the end of this loop
        je      put_fchr                ;put f_chr in buffer

        mov     di,si                   ;DI = old SI
@@:
        dec     si                              ;move SI to next char position
        mov     al,byte ptr in_temp.[bx+si]     ;get template character
        cmp     al,' '                          ;check for 'jump-over' chars
        jne     @b                              ;if found, move SI again
        jmp     short makeroom                  ;loop back until done

;----- put input text character in the buffer (in_buf) -----
put_fchr:
        mov     si,f_pos                        ;field position (of cursor)

        mov     al,f_chr                        ;AL = field character input
        mov     byte ptr in_buf.[bx+si],al      ;insert character in buffer

        cmp     f_num,14                        ;field 14 = description field
        jne     @f                              ;skip if not description field

        call    descwrap                        ;auto word-wrap routine
@@:
        call    disp_buf                        ;update screen display
@@:
        inc     si                              ;next character in template
        mov     al,byte ptr in_temp.[bx+si]     ;get character
        cmp     al,0                            ;end of field marker
        je      nextfield                       ;go to next field
        cmp     al,' '                          ;a 'jump over' character
        jne     @b                              ;move SI again

        mov     f_pos,si                ;SI = field position
        jmp     getkey                  ;get next character

nextfield:
        inc     f_num                   ;goto next field
        cmp     f_num,14                ;14 = last field (description)
        jbe     @f                      ;skip if valid field number
        jmp     short save_entry        ;save entry (y/n)
@@:
        jmp     get_fdata               ;get new field data

save_entry:
        mov     f_num,15                ;setup f_variables
        mov     f_row,21                
        mov     f_col,28
        mov     f_pos,0
        jmp     getkey                  ;get keyboard input

save_chr:
        and     al,11011111b            ;make character uppercase
        cmp     al,'Y'                  ;y=yes
        je      @f                      ;(Y)es
        jmp     getkey                  ;not yes
@@:     jmp     make_record             ;goto convert input to record

;-------------------------
ek_bksp:
        dec     f_pos                   ;move one chr position to the left
        cmp     f_pos,0                 ;check for negative value
        jge     @f                      ;if valid, continue
        mov     f_pos,0                 ;lowest valid value
        jmp     getkey                  ;go back to the input loop
@@:
        mov     si,f_pos                ;position source pointer
        cmp     tmpchr_s,' '            ;check for skip-over chr's
        je      @f                      ;no skip-over chr, continue
        dec     f_pos                   ;skip-over character
@@:
        push    f_pos                   ;parameter for the del_chr procedure
        call    del_chr                 ;delete character subroutine
        mov     cl,0ffh                 ;flag to redisplay input buffer
        call    disp_buf                ;redisplay buffer
        jmp     getkey                  ;go back to the input loop

ek_tab:
        inc     f_num                   ;next field number
        cmp     f_num,14                ;check for invalid value
        jbe     @f                      ;valid value
        jmp     save_entry              ;goto 'save entry' routine
@@:     jmp     get_fdata               ;get new field info

ek_enter:
        cmp     f_num,14                ;check for description field
        je      ek_ent1                 ;goto special case handler
        cmp     f_num,15                ;check for 'save entry' field
        jne     ek_tab                  ;enter functions as 'tab'
        jmp     make_record             ;if so, convert input to record

ek_ent1:
        mov     ax,f_pos                ;get field position
        mov     cl,72                   ;divide by 72 (# chars/line)
        div     cl                      ;f_pos/72 = AL, remainder: AH
        inc     al                      ;AL = next line
        mov     cl,72                   ;multiply by 72 (# chars/line)
        mul     cl                      ;(line#)(chars/line) = f_pos
        mov     f_pos,ax                ;update field position
        cmp     ax,f_len                ;check to see if f_pos is too big
        jb      @f                      ;f_pos is valid
        jmp     save_entry              ;goto 'save entry'
@@:     jmp     getkey                  ;go back to keyboard input loop

ek_esc:         jmp exit

ek_ins:
        mov     al,ins_flag             ;get insert flag
        xor     al,0ffh                 ;toggle flag
        mov     ins_flag,al             ;save insert mode
        call    sizecursor              ;change hardware cursor size
        jmp     getkey                  ;go back to the input loop

ek_del:
        mov     ax,f_len                ;get field length
        dec     ax                      ;get last chr position of field
        cmp     ax,f_pos                ;check for last character position
        je      del_last                ;delete last character position

        push    f_pos                   ;parameter for the del_chr procedure
        call    del_chr                 ;delete character by shifting text
        mov     cl,0ffh                 ;flag to redisplay input buffer
        call    disp_buf                ;redisplay input buffer
        jmp     getkey                  ;go back to the input loop

del_last:
        mov     si,f_pos                ;position source pointer
        mov     bufchr_s,' '            ;delete the character
        mov     f_chr,' '               ;chr = blank space
        xor     cl,cl                   ;redisplay just the character changed
        call    disp_buf                ;display the buffer change
        jmp     getkey                  ;go back to the input loop

ek_home:
        mov     f_num,1                 ;field number 1
        jmp     get_fdata               ;get new field info

ek_end:
        jmp     save_entry              ;goto 'save entry' field

ek_pgup:        jmp    getkey
ek_pgdn:        jmp    getkey
ek_up:          jmp    getkey
ek_dn:          jmp    getkey
ek_lt:
        dec     f_pos                           ;decrement field position
        mov     si,f_pos                        ;position index register
        mov     al,byte ptr in_temp.[bx+si]     ;get character at f_pos
        cmp     al,'/'                          ;'skip-over' character
        je      ek_lt                           ;skip the character
        cmp     al,':'                          ;'skip-over' character
        je      ek_lt                           ;skip the character
        cmp     al,'-'                          ;'skip-over' character
        je      ek_lt                           ;skip the character
        cmp     f_pos,0                         ;make sure f_pos is non-neg.
        jge     @f                              ;cursor move is valid
        mov     cl,0f0h                         ;flag used by get_fdata
        jmp     short  ek_shtab                 ;goto previous field
@@:     jmp     getkey                          ;go back to the input loop

ek_rt:
        inc     f_pos                           ;increment field position
        mov     si,f_pos                        ;position index register
        mov     al,byte ptr in_temp.[bx+si]     ;get character at f_pos
        cmp     al,'/'                          ;'skip-over' character
        je      ek_rt                           ;skip the character
        cmp     al,':'                          ;'skip-over' character
        je      ek_rt                           ;skip the character
        cmp     al,'-'                          ;'skip-over' character
        je      ek_rt                           ;skip the character
        cmp     al,0                            ;check for end of field marker
        jne     @f                              ;cursor move is OK
        jmp     ek_tab                          ;goto next field
@@:
        jmp     getkey                          ;go back to the input loop

ek_shtab:
        dec     f_num                   ;decrement field number
        cmp     f_num,1                 ;check for invalid value
        jae     @f                      ;valid value
        mov     f_num,1                 ;lowest valid value = 1
@@:
        jmp     get_fdata               ;get new field info

ek_f1:          jmp    getkey
ek_f2:          jmp    getkey
ek_f3:          jmp    getkey
ek_f4:          jmp    getkey

;----- make record from input buffer: in_buf --> recbuf -----
make_record:
        push    es                      ;save ES register
        mov     ax,ds                   ;get data segment
        mov     es,ax                   ;ES = DS = data segment

        mov     si,offset in_buf        ;position source pointer
        mov     di,offset recbuf        ;position destination pointer
        movsb                           ;copy system field

        a2vmac  in_buf.in_sd,v2a_in,10  ;convert ascii to value (macro/proc)
;        a2vchk                          ;macro to check for valid output


;------------------------------------------------------------------------------
inp_err:
        nop
exit:
        mov     ax,4c00h                ;DOS (exit procedure w/ return code)
        int     21h                     ;goto DOS (exit procedure)

;------------------------------------------------------------------------------
getfdata        proc                    ;get field data

        mov     al,f_num                ;get field number
        dec     al                      ;AL = offset multiplier
        shl     al,1                    ;multiply AL*4 by shifting left 2x
        shl     al,1                    ;AL = offset in in_data
        xor     ah,ah                   ;so AX = AL
        mov     si,ax                   ;SI = offset in in_data
        add     si,offset in_data       ;position source pointer

        lodsb                           ;get field row
        mov     f_row,al                ;store field row
	lodsb				;get field column
        mov     f_col,al                ;store field column
	lodsb				;get field length
        xor     ah,ah                   ;so AX = AL
        cmp     al,0ffh                 ;FF = exception handler (len > 255)
        jne     @f                      ;f_len < 255
        mov     ins_flag,0ffh           ;turn on insert mode
        call    sizecursor              ;set cursor size
        mov     ax,432                  ;FF found
@@:     mov     f_len,ax                ;store field length
        lodsb                           ;get input buffer field offset
        mov     f_buf,al                ;store input buffer field offset

        mov     f_pos,0                 ;initialize field cursor position
        cmp     cl,0f0h                 ;check for left-arrow flag
        jne     @f                      ;if not found,skip and continue
        mov     ax,f_len                ;get field length
        dec     ax                      ;get last character position
        mov     f_pos,ax                ;f_pos = last character position
@@:     xor     cl,cl                   ;clear the flag

        ret                             ;return from this subroutine
getfdata        endp
;------------------------------------------------------------------------------
disp_buf        proc                    ;display buffer on screen

        push    bp                      ;save BP register
        mov     bp,sp                   ;set up stack frame
        sub     sp,4                    ;allocate local (stack) variables
d_line  equ     <byte ptr [bp-3]>       ;line # (stack variable)
d_togo  equ     <word ptr [bp-2]>       ;bytes to go (stack variable)

        push    ax                      ;save registers
        push    dx
        push    si
        push    di
        push    es

        cmp     cl,0ffh                 ;FF=update entire buffer, 00=only 1 chr
        je      disp_entire             ;go display the entire buffer

;----- display only the new character -----
        mov     ax,f_pos                ;AX = field position
        mov     cl,72                   ;CL = 72 = number to be divided by
        div     cl                      ;f_pos/72 = AL, remainder: AH
        mov     dh,f_row                ;DH = field row (beginning)
        add     dh,al                   ;character position (row)
        mov     dl,f_col                ;DL = field column (beginning)
        add     dl,ah                   ;character position (column)

        mov     di,offset disptemp      ;position destination pointer
        mov     byte ptr [di],pos       ;position software cursor code
        inc     di                      ;bump source pointer
        mov     [di],dh                 ;row for character
        inc     di                      ;bump source pointer
        mov     [di],dl                 ;column for character
        inc     di                      ;bump source pointer
        mov     al,f_chr                ;get input character
        mov     [di],al                 ;AL = character (ascii value)
        inc     di                      ;bump source pointer
        mov     byte ptr [di],eom       ;end of message code
        display disptemp                ;display the character
        jmp     short dispexit          ;end this subroutine

;----- display the entire buffer -----
disp_entire:
        mov     d_line,0                ;initialize line # variable
        mov     ax,f_len                ;get field length
        mov     d_togo,ax               ;initialize bytes to go variable

        mov     si,offset in_buf        ;SI = points to input buffer (in_buf)
        mov     al,f_buf                ;get field buffer offset
        xor     ah,ah                   ;so AX = AL
        add     si,ax                   ;SI = points to input field buffer

d_loop:
        mov     di,offset disptemp      ;position destination pointer

        cmp     d_togo,72               ;72 = characters/line
        jbe     displast                ;go to display last line

        mov     dh,f_row                ;field beginning row
        add     dh,d_line               ;current field row to be displayed
        mov     dl,f_col                ;field beginning column

        mov     byte ptr [di],pos       ;position software cursor code
        inc     di                      ;bump source pointer
        mov     [di],dh                 ;row for character
        inc     di                      ;bump source pointer
        mov     [di],dl                 ;column for character
        inc     di                      ;bump source pointer

        mov     cx,ds                   ;CX = data segment
        mov     es,cx                   ;ES = DS for fast string copy
        mov     cx,36                   ;number of word in a line (72 bytes)
        rep     movsw                   ;copy a line to 'disptemp'

        mov     byte ptr [di],eom       ;end of message code
        display disptemp                ;display the line

        inc     d_line                  ;increment line #
        sub     d_togo,72               ;subtract 72 bytes from 'to-go' var.
        jmp     short d_loop            ;loop back until field is displayed

displast:
        mov     di,offset disptemp      ;position destination pointer

        mov     dh,f_row                ;field beginning row
        add     dh,d_line               ;current field row to be displayed
        mov     dl,f_col                ;field beginning column

        mov     byte ptr [di],pos       ;position software cursor code
        inc     di                      ;bump source pointer
        mov     [di],dh                 ;row for character
        inc     di                      ;bump source pointer
        mov     [di],dl                 ;column for character
        inc     di                      ;bump source pointer

        mov     cx,ds                   ;get data segment
        mov     es,cx                   ;ES = DS for fast string move
        mov     cx,d_togo               ;CX = number of bytes to move
        rep     movsb                   ;move string to disptemp

        mov     byte ptr [di],eom       ;end of message code
        display disptemp                ;display the line

dispexit:
        xor     cl,cl                   ;clear flag

        pop     es                      ;restore registers
        pop     di                      
        pop     si
        pop     dx
        pop     ax

        mov     sp,bp                   ;remove stack frame
        pop     bp                      ;restore BP register
        ret                             ;return from procedure
disp_buf        endp
;------------------------------------------------------------------------------
descwrap        proc                    ;auto word-wrap routine for description

        push    bp                      ;save BP register
        mov     bp,sp                   ;set up stack frame
        sub     sp,4                    ;allocate local (stack) variables
w_beg   equ     <word ptr [bp-2]>       ;beginning of text shift
w_flag  equ     <word ptr [bp-4]>       ;wrap flag (FF = wrapped, 00 = didn't)
        mov     w_beg,0ffffh            ;initialize to FFFF
        mov     w_flag,0                ;initialize to 0

        push    ax                      ;save registers
        push    dx
        push    si
        push    di

        mov     si,71                   ;last character position on a line

w_check:
        mov     ax,f_len                ;AX = field length
        dec     ax                      ;AX = last character position

@@:     cmp     si,ax                   ;check if done
        jae     wrapexit                ;leave this subroutine
        cmp     bufchr_s,' '            ;check for space at end of line
        jne     do_wrap                 ;push a word to the next line
        add     si,72                   ;point to next line end
        jmp     @b                      ;loop until all lines done

do_wrap:
        mov     ax,si                   ;AX = SI
        inc     ax                      ;AX = beginning of the next line
        mov     dx,ax                   ;DX = end marker for text shift
        mov     cx,ax                   ;CX = beginning of the next line
        sub     cx,72                   ;CX = beginning of the current line

@@:     dec     si                      ;back up one space
        cmp     si,cx                   ;check if there are any spaces on line
        je      wrapnext                ;exit this loop (don't wrap this line)
        cmp     bufchr_s,' '            ;check for space
        jne     @b                      ;loop back until space is found
        mov     w_flag,0ffh             ;set wrap flag
        mov     cl,0ffh                 ;so 'disp_buf' will re-display buffer
        inc     si                      ;SI points to beginning of a 'word'

        cmp     w_beg,si                ;find the beginning of the text shift
        jbe     @f                      ;skip if w_beg < si
        mov     w_beg,si                ;update w_beg if w_beg > si
@@:
        sub     ax,si                   ;get distance to shift
        mov     di,f_len                ;DI = field length
        dec     di                      ;DI points to last chr of field
        mov     si,di                   ;SI = DI: points to last chr of field
        sub     si,ax                   ;subtract shift distance

@@:     mov     al,bufchr_s             ;get a character
        mov     bufchr_d,al             ;move the character
        cmp     di,dx                   ;check for end of text shift
        je      @F                      ;exit this loop
        dec     si                      ;decrement source pointer
        dec     di                      ;decrement destination pointer
        jmp     short @b                ;loop back until done
@@:
        mov     al,' '                  ;space character
        mov     bufchr_s,al             ;delete old part of word
        inc     si                      ;bump source pointer
        cmp     si,dx                   ;check for end of this loop
        jne     @b                      ;loop back until done

wrapnext:
        mov     si,dx                   ;SI = DX = beginning of next line
        dec     si                      ;SI = end of current line
        add     si,72                   ;SI = end of next line
        jmp     short w_check           ;goto check the next line

wrapexit:
        mov     ax,f_pos                ;AX = field position
        sub     ax,w_beg                ;AX = columns to shift f_pos
        mov     w_beg,0                 ;reset w_beg to zero
        cmp     ax,0                    ;check for positive number
        jle     @f                      ;skip if negative or zero
        cmp     w_flag,0ffh             ;check if word-wrap was done
        jne     @f                      ;skip if word-wrap was not done
        mov     w_beg,ax                ;w_beg = bytes to add to f_pos
        inc     w_beg                   ;to advance to the next chr position
@@:
        pop     di                      ;restore registers
        pop     si
        add     si,w_beg                ;modify f_pos if affected by the shift
        pop     dx
        pop     ax

        mov     sp,bp                   ;remove stack frame
        pop     bp                      ;restore BP register
        ret                             ;return from this subroutine
descwrap        endp

;------------------------------------------------------------------------------
sizecursor      proc                    ;----- set cursor size (insert flag)

        push    ax                      ;save registers
        push    cx

        mov     ah,1                    ;BIOS (set cursor type)
        cmp     ins_flag,0              ;check insert flag (00=off, FF=on)
        je      sm_cur                  ;insert off

        mov     cx,0007h                ;for a big cursor (block)
        jmp     short csize             ;exit macro

sm_cur: mov     cx,0607h                ;for a little cursor (line)
csize:  int     10h                     ;BIOS (set cursor type)

        pop     cx                      ;restore registers
        pop     ax                      ;restore
        ret                             ;return from this subroutine
sizecursor      endp

;------------------------------------------------------------------------------
poscursor       proc                    ;----- position cursor at current field

        push    ax                      ;save registers
        push    bx
        push    dx

        mov     ax,f_pos                ;AL = input field position
        mov     bl,72                   ;divide by 72 (mod 72)
        div     bl                      ;f_pos/72 = AL, remainder = AH
        mov     dh,f_row                ;DH = field row
        add     dh,al                   ;add lines
        mov     dl,f_col                ;DL = field column
        add     dl,ah                   ;add columns

        mov     ah,2                    ;BIOS (set cursor position)
        mov     bh,0                    ;video page 0
        int     10h                     ;BIOS (set cursor position)

        pop     dx                      ;restore registers
        pop     bx
        pop     ax
        ret                             ;return from this subroutine
poscursor       endp

;------------------------------------------------------------------------------
del_chr         proc                    ;delete a chr by shifting text left

        push    bp                      ;save BP register
        mov     bp,sp                   ;set up stack frame
d_pos   equ     <word ptr [bp+4]>       ;position to be deleted

        push    si                      ;save registers
        push    di

        mov     cx,ds                   ;get data segment
        mov     es,cx                   ;ES = DS = data segment

        mov     di,d_pos                ;position destination pointer
        mov     si,di                   ;SI = DI
@@:     inc     si                      ;position source pointer
        cmp     tmpchr_s,' '            ;check for non-space chr's
        jne     @b                      ;skip over them

        mov     cx,f_len                ;get field length
        dec     cx                      ;CX = last character position of field

del_loop:
        mov     al,bufchr_s             ;get character (source)
        mov     bufchr_d,al             ;put character (destination)

        cmp     si,cx                   ;check for end of this loop
        je      del_exit                ;exit if done

        mov     di,si                   ;new DI = old SI

@@:     inc     si                      ;bump source pointer
        cmp     tmpchr_s,' '            ;check for non-space chr's
        jne     @b                      ;skip any 'skip-over' characters

        jmp     short del_loop          ;loop back until done

del_exit:
        mov     si,f_len                ;get field length
        dec     si                      ;get last character position
        mov     bufchr_s,' '            ;delete last character of field

        pop     di                      ;restore registers
        pop     si

        mov     sp,bp                   ;remove stack frame
        pop     bp                      ;restore BP register
        ret                             ;return from this subroutine
del_chr         endp

;------------------------------------------------------------------------------
datetime        proc                    ;get today;s date and time --> in_buf

        push    bp                      ;save BP register
        mov     bp,sp                   ;setup stack frame
        sub     sp,8                    ;space for stack variables
dt_yr   equ     <byte ptr [bp-2]>       ;(word) year
dt_mo   equ     <byte ptr [bp-3]>       ;(byte) month
dt_dy   equ     <byte ptr [bp-4]>       ;(byte) day
dt_hr   equ     <byte ptr [bp-5]>       ;(byte) hour
dt_mn   equ     <byte ptr [bp-6]>       ;(byte) minute
dt_buf  equ     <word ptr [bp-8]>       ;in_buf+field offset address

        push    ax                      ;save registers
        push    bx
        push    cx
        push    dx
        push    si
        push    di

;----- get time and date from DOS -----

        mov     ah,2ah                  ;DOS function number
        int     21h                     ;DOS (get date)
        sub     cx,1900                 ;get only last 2 digits of year
        mov     dt_yr,cl                ;save year
        mov     dt_mo,dh                ;save month
        mov     dt_dy,dl                ;save day
        mov     ah,2ch                  ;DOS function number
        int     21h                     ;DOS (get time)
        mov     dt_hr,ch                ;save hour
        mov     dt_mn,cl                ;save minute

;----- preset start date -----

        mov     ax,offset in_buf        ;get input buffer address
        add     ax,in_sd                ;get start date address
        mov     dt_buf,ax               ;save in stack variable

        mov     al,dt_mo                ;get month
        xor     ah,ah                   ;so AX = AL = month
        mov     v2a_in,ax               ;val2asc input
        v2amac  v2a_in, v2a_out, 10     ;val2asc macro

        slmac   v2a_out,dt_buf,2,'0'    ;macro for setlen local procedure

        add     dt_buf,3                ;point to 'day' subfield
        mov     al,dt_dy                ;get day
        xor     ah,ah                   ;so AX = AL = day
        mov     v2a_in,ax               ;val2asc input
        v2amac  v2a_in,v2a_out,10       ;val2asc macro

        slmac   v2a_out,dt_buf,2,'0'    ;macro for setlen local procedure

        add     dt_buf,3                ;point to 'year' subfield
        mov     al,dt_yr                ;get year (last 2 digits)
        xor     ah,ah                   ;so AX = AL = year (92)
        mov     v2a_in,ax               ;val2asc input
        v2amac  v2a_in,v2a_out,10       ;val2asc macro

        slmac   v2a_out,dt_buf,2,'0'    ;macro for setlen local procedure

;----- copy start date sub-buffer to end date sub-buffer -----

        mov     si,offset in_buf        ;get input buffer address
        add     si,in_sd                ;point to start date sub-buffer
        mov     di,si                   ;DI = SI
        add     di,9                    ;point to end date sub-buffer
        mov     cx,4                    ;8 bytes to copy (4 words)
@@:     lodsw                           ;get a word
        mov     [di],ax                 ;copy the word
        add     di,2                    ;bump destination pointer
        loop    @b                      ;loop for string copy

;----- preset the start time -----

        mov     ax,offset in_buf        ;get input buffer address
        add     ax,in_st                ;get start time address
        mov     dt_buf,ax               ;save in stack variable

        mov     al,dt_hr                ;get hour
        xor     ah,ah                   ;so AX = AL = day
        mov     v2a_in,ax               ;val2asc input
        v2amac  v2a_in,v2a_out,10       ;val2asc macro

        slmac   v2a_out,dt_buf,2,'0'    ;macro for setlen local procedure

        add     dt_buf,3                ;point to 'minute' subfield
        mov     al,dt_mn                ;get minute (last 2 digits)
        xor     ah,ah                   ;so AX = AL = minute
        mov     v2a_in,ax               ;val2asc input
        v2amac  v2a_in,v2a_out,10       ;val2asc macro

        slmac   v2a_out,dt_buf,2,'0'    ;macro for setlen local procedure

;----- copy start time sub-buffer to end time sub-buffer -----

        mov     si,offset in_buf        ;get input buffer address
        add     si,in_st                ;point to start date sub-buffer
        mov     di,si                   ;DI = SI
        add     di,6                    ;point to end date sub-buffer
        mov     cx,3                    ;8 bytes to copy (4 words)
@@:     lodsw                           ;get a word
        mov     [di],ax                 ;copy the word
        add     di,2                    ;bump destination pointer
        loop    @b                      ;loop for string copy

        pop     di
        pop     si
        pop     dx
        pop     cx
        pop     bx
        pop     ax

        mov     sp,bp
        pop     bp
        ret
datetime        endp
;------------------------------------------------------------------------------
setlen          proc                    ;set length for ascii numbers

        push    bp                      ;save BP register
        mov     bp,sp                   ;set up stack frame
sl_in   equ     <word ptr [bp+8]>       ;input address
sl_out  equ     <word ptr [bp+6]>       ;output address
sl_len  equ     <byte ptr [bp+5]>       ;length of output string
sl_chr  equ     <byte ptr [bp+4]>       ;character to use '0' or ' ' or ...

        push    ax                      ;save registers
        push    si
        push    di

        xor     ah,ah                   ;initialize a counter
        mov     si,sl_in                ;set source pointer
@@:     cmp     byte ptr [si],0         ;check for end marker (0)
        je      @f                      ;if end marker found, exit this loop
        inc     ah                      ;increment counter
        inc     si                      ;increment source pointer
        jmp     short @b                ;loop to count string length

@@:
        mov     al,sl_len               ;get output string length (AL)
        sub     al,ah                   ;AL = number of sl_chr's needed
        mov     di,sl_out               ;set destination pointer
        mov     ah,sl_chr               ;AH = character to insert
@@:
        cmp     al,0                    ;check if done (or shouldn't be done)
        jle     @f                      ;exit this loop
        mov     [di],ah                 ;insert a sl_chr character
        inc     di                      ;bump destination pointer
        dec     al                      ;decrement counter
        jmp     short @b                ;loop to insert filler characters
@@:
        mov     si,sl_in                ;set source pointer
@@:
        lodsb                           ;get a character (digit) in AL
        cmp     al,0                    ;0 = end marker
        je      @f                      ;exit loop on end marker
        mov     [di],al                 ;insert character (digit)
        inc     di                      ;bump destination pointer
        jmp     short @b                ;loop to insert characters (digits)
@@:
        pop     di                      ;restore registers
        pop     si
        pop     ax

        mov     sp,bp                   ;remove this stack frame
        pop     bp                      ;restore BP register
        ret                             ;return from this subroutine
setlen          endp
;------------------------------------------------------------------------------

        end
