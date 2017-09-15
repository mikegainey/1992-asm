comment * ---------------------------------------------------------------------

Program title:  pcset.asm  by Mike Gainey (3/2/92 - 3/4/92)

Modification dates:  3/19/92 -- updated version of 'print', added 'val2asc'

Program description:

  This program takes a collection of pitch-classes and finds the prime form
  (pctype), also determining transpositional and inversional symmetry, and
  the interval vector.

Procedures used:
  pctype.asm (main processing procedure)
  print.asm (video display functions)
  val2asc.asm (converts a value (word) to ascii)

----------------------------------------------------------------------------- *
display macro   txt             ;message display macro
        push    si              ;save SI register
        mov     si,offset txt   ;position source pointer
        call    print           ;main display procedure
        pop     si              ;restore SI register
        endm
;------------------------------------------------------------------------------

        .model  small

        .stack

        .data

eom     equ     0                       ;equates for 'print' functions
blkwin  equ     4
crlf    equ     5
pos     equ     7
attr    equ     9
ctr     equ     10
wrap    equ     11


cls     db      1,3,0

t_disp  db      3, attr,9, ctr,1,'----- Pitch-class set type -----',ctr
        db      ctr,2,'----- and Interval vector program -----',ctr
        db      ctr,3,'----- by Mike Gainey, 3/4/92 -----',ctr,0

t_end   db      attr,2, pos,6,0,wrap,0ffh
        db      "     Hi, this is Mike.  I'm interested in "
        db      'computer applications in music theory.  If you have any '
        db      'comments about this program or ideas for other programs, '
        db      "let me know.  If this program didn't make any sense to you, "
        db      'delete it.  It deals with a specialized area (non-serial '
        db      'atonality) of a specialized area (music theory) in "serious" '
        db      '(classical) music.  '
        db      'By the way, this is a free program:  I want its educational '
        db      'possibilities to go unhindered. (and besides, '
        db      'who would pay money for this anyway?)',wrap,0,crlf,crlf
        db      'Mike Gainey',crlf,'307 Christina Lane',crlf
        db      'Friendswood, TX  77546',crlf,crlf, 2,1, 0


t_inst  db      attr,2, pos,5,0,'Use either upper- or lower-case letters:  '
        db      'A B C ...  or a b c ...'
        db      pos,6,0,'s=sharp, f=flat; ds=double sharp, df=double flat:  '
        db      'cs = c-sharp, bf = b-flat'
        db      pos,7,0,'Separate notes with spaces; '
        db      'a blank entry ends the program.'
        db      pos,8,0,80 dup(196),0

input1  db      attr,2, pos,10,0,'Input pitch-classes  (ex: g af b cs)'
        db      pos,12,0,'>', pos,14,0, 80 dup(196),0
b_inp   db      attr,2, blkwin,12,1,12,79,0
inp_err db      attr,12, pos,13,2,'input error -- try again',0
b_inerr db      blkwin,13,1,13,79,0

showset db      attr,2, pos,16,0,'input pitch-class set:     '
        db      54 dup(32), attr,3, pos,16,27,0
inv_sym db      attr,2, pos,18,0,'inversional symmetry:      ',0
tr_sym  db      attr,2, pos,19,0,'transpositional symmetry:  ',0
tr_int  db      attr,2, pos,20,2,'interval(s):             ',pos,20,16,attr,3,0
b_trint db      pos,20,2, 78 dup(32),0
yes     db      attr,3,'yes',0
no      db      attr,3,'no ',0
pr_form db      attr,2, pos,22,0,'prime form:       '
        db      attr,3,'[', 61 dup(32), pos,22,19,0
int_vec db      attr,2, pos,23,0,'interval vector:  '
        db      attr,3,'<', 20 dup(32), pos,23,19,0

number  dw      0                       ;input for val2asc
numout  db      5 dup(0)                ;outupt for val2asc
comma   db      ',',0
cl_brk  db      ']',0
cl_brk1 db      '>',0
comma_flag      db      0               ;so commas display correctly

in_buffer       db      64,?,64 dup(0)  ;buffer for input data
marker_pos      dw      ?               ;marker position for input
out_buffer      db      64 dup(0)       ;buffer for output data
;------------------------------------------------------------------------------

        .code

        extrn   pc_type:proc
        extrn   print:proc, val2asc:proc

        include bios.inc        ;use MASM include file for BIOS functions

entry:
        mov     ax,@data        ;get data segment address
        mov     ds,ax           ;set-up DS register

        display cls             ;set video mode and clear the screen

start:
        display t_disp          ;display the main title display
        display t_inst          ;display instructions

        display input1          ;display input prompt

start1:
        display b_inp           ;blank previous input

        @SetCurPos 2,12,0       ;set cursor position: col=2, row=12, page=2
                                ;  uses dl, dh, bh, ah

;----- get keyboard input -----

        mov     ah,0ah                  ;DOS function #
        mov     dx,offset in_buffer     ;pointer to input buffer
        int     21h                     ;DOS function (buffered keyboard input)

        mov     bl,in_buffer+1          ;get input string length
        cmp     bl,0                    ;blank entry?
        jne     short skip_exit1        ;if not, continue
        jmp     end_program             ;exit to DOS (blank input)

skip_exit1:
        add     bl,2                    ;skip over non-data characters
        mov     bh,0                    ;BX = BL
        mov     cx,offset in_buffer     ;CX = offset of input buffer
        add     cx,bx                   ;find end marker position (input)
        mov     marker_pos,cx           ;save end marker position
        mov     in_buffer[bx],0ffh      ;append an end marker (FFh)

        mov     si,offset in_buffer+2   ;pointer for input data
        mov     di,offset out_buffer    ;pointer for output

        call    pc_type                 ;goto main processing procedure

        display showset                 ;display 'input pitch-class set:  '
        mov     bx,marker_pos           ;BX = end marker position (input)
        mov     byte ptr [bx],0         ;append an end marker (0 for display)
        mov     si,offset in_buffer+2   ;pointer for input data
        display in_buffer+2             ;display input

        cmp     [out_buffer],0f0h       ;check for error indication
        jne     skip_exit2              ;if no error, continue
        display inp_err                 ;display 'input error'
        jmp     short start1            ;input error, try again

skip_exit2:
        display b_inerr                 ;blank 'input error' if displayed

        mov     si,offset out_buffer    ;position source pointer

        display inv_sym                 ;display inv_sym info
        lodsb                           ;get inv_sym flag
        cmp     al,0ffh                 ;FF = inversional symmetry
        je      inv_sym1                ;leading FF marker found

        display no                      ;display 'no'
        jmp     short tr_sym1           ;goto next program step

inv_sym1:
        display yes                     ;display 'yes'

tr_sym1:
        display tr_sym                  ;display trans_sym info
        mov     al,[si]                 ;get trans_sym status
        cmp     al,0ffh                 ;leading FF means no trans symmetry
        jne     tr_sym2                 ;FF marker found

        display no                      ;display 'no'
        display b_trint                 ;blank 'interval(s):  ' line
        jmp     short prime1            ;goto next program step

tr_sym2:
        display yes                     ;display 'yes'
        display tr_int                  ;display '  intervals:'
        call    numbers                 ;display intervals (local procedure)

prime1:
        mov     si,offset out_buffer    ;point to output buffer
        add     si,13                   ;add offset to prime form
        display pr_form                 ;display 'prime form:  ['
        call    numbers                 ;display prime form numbers
        display cl_brk                  ;close the brackets ']'

        mov     si,offset out_buffer    ;point to output buffer
        add     si,26                   ;add offset to interval vector
        display int_vec                 ;display 'interval vector:  <'
        call    numbers                 ;display interval vector
        display cl_brk1                 ;close the brackets '>'

        jmp     start1

end_program:
        display t_disp          ;display title display
        display t_end           ;display ending title display (address, info)

exit_to_dos:
        mov     ah,76                   ;DOS function #
        mov     al,0                    ;return code (OK)
        int     21h                     ;DOS function (exit program)

;------------------------------------------------------------------------------
;procedure to print a string of numbers (set SI pointer; FF = end marker

numbers proc
        mov     ah,0                    ;to reset comma flag
        mov     comma_flag,ah           ;reset comma flag

num_loop:
        lodsb                           ;get interval (or end flag, FF)
        xor     ah,ah                   ;AX = AL
        mov     number,ax               ;store number to be displayed

        cmp     al,0ffh                 ;FF = end marker
        je      exit_numbers            ;end marker found, goto return

        mov     ah,comma_flag           ;so commas display correctly
        cmp     ah,0                    ;check if this is the first number
        je      comma1                  ;if it is, skip the comma

        display comma                   ;display a comma

comma1:
        push    si                      ;save registers used
        push    di
        push    bx
        mov     bl,10                   ;base ten
        mov     si,offset number        ;position source pointer
        mov     di,offset numout        ;position destination pointer
        call    val2asc                 ;convert value to asciiz string
        display numout                  ;display number in 'number_out'
        pop     bx                      ;restore registers used
        pop     di
        pop     si

        mov     ah,0ffh                 ;to set comma flag
        mov     comma_flag,ah           ;set comma flag

        jmp     num_loop                ;loop until all numbers are displayed
exit_numbers:
        ret                             ;return to main procedure
numbers endp
;------------------------------------------------------------------------------
        end     entry
