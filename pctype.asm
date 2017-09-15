comment |----------------------------------------------------------------------

File:  pctype (procedure) by Mike Gainey (2/23/92 - 3/4/92)

Modification dates:  3/9/92 -- make output fields have constant offsets

Description:

  Computes pitch class set types given a list of pitch classes.  Each note
  in the list must be separated with a space (20h, 32d) and the list must
  end with a FFh byte as an end marker.  Upper or lower case letters can
  be used.  Inflection codes:
    double flat  = df
    flat         = f
    sharp        = s
    double sharp = ds

 Entry:  DS:SI = pointer for input data buffer
         DS:DI = pointer for output buffer

 Exit :
           offset
   format:     0   1) inversionally symmetrical flag     (FF=yes, 00=no)
                      *** NOTE: F0 = input error -- procedure aborted
           01-07   2) intervals of transpositional symmetry
                       - interval(s) of transposition /w FF end marker
                       (just FF if not transpositionally symmetrical)
           08-20   3) prime form of input set (w/ FF end marker)
           21-27   4) interval vector

|------------------------------------------------------------------------------

        .model  small

;------------------------------------------------------------------------------
        .data

pc_tble db      9,11,0,2,4,5,7  ;a=9, b=11, c=0, d=2, e=4, f=5, g=7

pcset1  db      25 dup (0)              ;temporary space (2*12) + 1(FF marker)
pcset2  db      25 dup (0)              ;temporary space (2*12) + 1(FF marker)
pcset3  db      13 dup (0)              ;temporary space for a normal order

temp1   struc
        db      12 dup(0)               ;space for pc's
        db      0ffh                    ;space for FFh end marker
temp1   ends
pctemp  temp1   12 dup(<>)              ;temporary space (for set inversions)

ans_addr        dw      0               ;space to save the output address

;------------------------------------------------------------------------------
        .code
pc_type proc
        public  pc_type

        push    ax                      ;save entry registers
        push    bx
        push    cx
        push    dx
        push    si
        push    di

        mov     ans_addr,di             ;store output pointer
        mov     di,ans_addr             ;DI = output buffer
        mov     byte ptr [di],0         ;initialize inv sym area
        inc     di                      ;bump pointer
        mov     byte ptr [di],0ffh      ;initialize inv trans sym area


        mov     al,[si]                 ;check for 0ffh (no data)
        cmp     al,0ffh                 ;FFh = input string length = 0
        jne     initialize              ;if not, continue
        jmp     input_err               ;if so, abort program

initialize:
        mov     di,offset pcset1        ;set DI to sub-module output
        mov     bx,offset pc_tble       ;translate table (c=1, not a)
        cld                             ;clear direction flag (low to high)

;----- convert the input notes into numerical form (c=1, c+=2, etc...) -----
;      SI --> pcset1

get_char:
        lodsb                   ;get character (mov al,[si] ; inc si)
proc_note:
        cmp     al,32           ;asc 32 = space
        je      get_char        ;pass the space character
        cmp     al,255          ;asc 255 = end of data marker
        je      sort_set        ;go to the next processing step

        and     al,11011111b    ;if lowercase, make upper case
        cmp     al,65           ;asc 65 = 'A'
        jl      inp_err1        ;invalid data, abort program
        cmp     al,71           ;asc 71 = 'G'
        jg      inp_err1        ;invalid data, abort program
        sub     al,65           ;a=1, b=2, c=3, etc...
        xlat                    ;c=1, d=3,...a=9, b=11
        xchg    ah,al           ;mov intermediate value to ah

        lodsb                   ;get inflection or delimeter space
        cmp     al,32           ;asc 32 = space
        je      dec_then_add    ;decrement source pointer, then add note
        cmp     al,255          ;asc 255 = end of data marker
        je      dec_then_add    ;decrement source pointer, then add note
        jmp     short proc_infl ;process the inflection code

dec_then_add:
        dec     si              ;so 'add_1' will get the FFh or space char
        jmp     short add_1     ;add the note to the set

proc_infl:
        and     al,11011111b    ;if lowercase, make upper case
        cmp     al,68           ;asc 68 = 'D' for double flat or sharp
        je      df_ds           ;check for double flat or double sharp
        cmp     al,70           ;asc 70 = 'F' (flat)
        je      flat            ;lower pitch one half-step
        cmp     al,83           ;asc 83 = 'S' (sharp)
        je      sharp           ;raise pitch one half-step
inp_err1:
        jmp     input_err       ;invalid data, abort program

flat:
        dec     ah              ;lower pitch one half-step
        jns     add_1           ;add a note to the set
        mov     ah,11           ;b=11
        jmp     short add_1     ;add a note to the set

sharp:
        inc     ah              ;raise pitch one half-step
        cmp     ah,11           ;b=11 is highest valid note
        jle     add_1           ;add a note to the set
        mov     ah,0            ;c=0
        jmp     short add_1     ;add a note to the set

df_ds:
        lodsb                   ;get the 'F' for flat or 'S' for sharp
        and     al,11011111b    ;if lowercase, make upper case
        cmp     al,70           ;asc 70 = 'F'
        je      d_flat          ;lower pitch 2 half-steps
        cmp     al,83           ;asc 83 = 'S'
        je      d_sharp         ;raise pitch 2 half_steps
        jmp     short inp_err1  ;invalid data, abort program

d_flat:
        sub     ah,2            ;lower pitch 2 half_steps
        jns     add_1           ;add a note to the set
        add     ah,12           ;make the pitch valid (between 0 and 11)
        jmp     short add_1     ;add a note to the set

d_sharp:
        add     ah,2            ;raise pitch 2 half_steps
        cmp     ah,11           ;b=11 is highest valid note
        jle     add_1           ;add a note to the set
        sub     ah,12           ;make the pitch valid (between 0 to 11)
        jmp     short add_1     ;add a note to the set

add_1:
        mov     [di],ah                 ;add note to temp processing space
        inc     di                      ;bump pointer
        cmp     di,offset pcset1+25     ;check for end of buffer
        jg      inp_err1                ;too many notes, abort program
        je      sort_set                ;go on to the next step
        lodsb                           ;pass the space (or end marker)
        cmp     al,255                  ;asc 255 = end marker
        je      sort_set                ;go on to the next step
        cmp     al,32                   ;asc 32 = space
        jne     inp_err1                ;invalid character
        jmp     get_char                ;go back and get another note

;----- eliminate multiple occurances of notes in the set -----
;                and put in numerical order
;      pcset1 --> pcset2

sort_set:
        mov     byte ptr [di],255       ;end of data marker
        mov     di,offset pcset2        ;set DI to output of this sub-module

        mov     cl,0                    ;CX = pc to be checked
sort1:
        mov     si,offset pcset1        ;set SI to begining of input data
sort2:
        lodsb                           ;mov al,[si] ; inc si (get char)
        cmp     al,cl                   ;check for each pc (0...11)
        je      add_2                   ;add note to the set
        cmp     al,255                  ;255 = end of data marker
        jne     sort2                   ;if not, continue loop

        inc     cl                      ;check for next pc
        cmp     cl,11                   ;11 is the last pc
        jle     sort1                   ;go back to loop if pc<=11
        jmp     short sort_end          ;goto next sub-module

add_2:
        mov     [di],cl                 ;add pc to the set
        inc     di                      ;bump pointer
        inc     cl                      ;inc pc to be checked
        cmp     cl,11                   ;11 is the last pc
        jle     sort1                   ;go back to loop if pc<=11

sort_end:
        mov     bx,offset pcset2        ;BX = beginning of pcset2
        sub     di,bx                   ;find length of pcset1
        xchg    bx,di                   ;BX = length of pcset1


;----- "zero" the set by subtracting the smallest value from each note -----
;      pcset2 --> pcset1

zero:
        mov     si,offset pcset2        ;set up source pointer
        mov     di,offset pcset1        ;set up destination pointer
        mov     cx,bx                   ;initialize loop counter

        mov     ah,[si]         ;put first value in AL (value to subtract)

zero_loop:
        lodsb                   ;get a value
        sub     al,ah           ;subtract AH from it
        mov     [di],al         ;store the result
        inc     di              ;bump destination pointer
        loop    short zero_loop ;loop back until done

zero_end:
        mov     byte ptr [di],0ffh      ;FFh = end marker

        mov     al,0            ;AL = 0
        mov     pcset3+12,al    ;reset pcset3 flag

;----- extend set to prepare for set inversions -----
;      pcset1 --> pcset2

ext_begin:
        mov     si,offset pcset1        ;set up source pointer
        mov     di,offset pcset2        ;set up destination pointer
        mov     cx,bx                   ;initialize loop counter

ext_loop:
        lodsb                   ;get a value --> AL
        mov     [di],al         ;copy the previous set
        add     al,12           ;add 12 to the value
        mov     [di+bx],al      ;append the set+12
        inc     di              ;bump destination pointer
        loop    short ext_loop  ;loop until done

ext_end:
        mov     byte ptr [di+bx],0ffh   ;FFh = end marker

;----- find all inversions of the pcset -----
;      pcset2 --> pctemp

invert:
        mov     bp,0                    ;counter for inversion

inv_next:
        mov     cx,bx                   ;initialize loop counter

        mov     si,offset pcset2        ;set up source pointer
        add     si,bp                   ;add inversion counter

        mov     ax,13                   ;pctemp record length
        mul     bp                      ;find pctemp offset
        mov     di,offset pctemp        ;pctemp start address
        add     di,ax                   ;pctemp record address

        mov     ah,0ffh                 ;FFh = initialized (untested) inversion
        mov     [di+12],ah              ;store FFh in each pctemp record

        mov     ah,[si]                 ;get value to subtract

inv_loop:
        lodsb                           ;get value
        sub     al,ah                   ;find mod12 value
        mov     [di],al                 ;store mod12 value in pctemp record
        inc     di                      ;bump destination pointer
        loop    inv_loop                ;loop until inversion is done

        inc     bp                      ;bump inversion counter
        cmp     bp,bx                   ;BX = number of posible inversions
        jl      inv_next                ;go to next inversion

;------------------------------------------------------------------------------
; -----*** find the pcset inversion in normal order ***-----

        dec     bx                      ;BX = pointer for last note of inv.
        mov     dx,bx                   ;DX = pointer for last note of inv.

;----- find the lowest value (AL) in column BX (only marked w/ FFh) -----
           
low_find:
        mov     si,offset pctemp        ;SI = pointer to set inversions
        mov     cx,bp                   ;set loop counter
        mov     al,12                   ;initialize AL (lowest value found)

low_top:
        cmp     byte ptr [si+12],0ffh   ;check only inversions marked w/ FFh
        jne     low_top2                ;skip if not FFh

        cmp     [si+bx],al              ;find the lowest value in column BX
        jl      least_top               ;go to update AL (lowest value)
low_top2:
        add     si,13                   ;go to next inversion
        loop    low_top                 ;loop until all inversions are checked
        jmp     short mark_lowest       ;go to next processing step

least_top:
        mov     al,[si+bx]              ;update lowest top note value
        jmp     low_top2                ;go back to the loop

;----- mark the inversions with AL in column BX (and count them, AH) -----

mark_lowest:
        mov     si,offset pctemp        ;SI = pointer to set inversions

        mov     cx,bp                   ;set loop counter
        mov     ah,0                    ;AH = number of matches

mark_low1:
        cmp     byte ptr [si+12],0ffh   ;check only inversions marked w/ FFh
        jne     mark_low2               ;skip if not FFh

        cmp     al,[si+bx]              ;check for a match
        je      mark_match              ;record the match
        mov     byte ptr [si+12],0f0h   ;unmark the non-match
mark_low2:
        add     si,13                   ;go to next inversion
        loop    mark_low1               ;loop until all inversions are checked
        jmp     short mark_end          ;continue processing

mark_match:
        inc     ah                      ;counter the sets with lowest top note
        jmp     short mark_low2         ;go back to the loop

;-------------------------------------- separate outer loop -------------------

mark_end:
        cmp     ah,1                    ;only one most compact set?
        je      bn_order                ;already normal order, find best normal

        inc     bx                      ;inc note position to check
        cmp     bx,dx                   ;end of outer loop?
        je      tr_sym                  ;transpositionally symmetrical set

        cmp     bp,2                    ;only two notes in the set?
        je      tr_sym                  ;goto tr_sym (this is a bug fix)

        cmp     bx,bp                   ;is BP out of range (outside the set)?
        je      reset_bx                ;go back to column one
        jmp     low_find                ;loop back to check another column
reset_bx:
        mov     bx,1                    ;start checking from the beginning
        jmp     low_find                ;loop back to check another column

;-------------------------------------- end of finding normal order -----------

;----- transpositionally symmetrical:  record interval(s) -----

tr_sym:
        mov     di,ans_addr             ;position destination pointer
        inc     di                      ; to trans sym info space

        mov     si,offset pctemp        ;position source pointer
        mov     bx,0                    ;initialize counter/pointer
        mov     ah,0ffh                 ;initialize reference pitch register

tr_check:
        cmp     byte ptr [si+12],0ffh   ;is the flag set?
        je      tr_rec                  ;go record int(s) of transposition

tr_cont:
        add     si,13                   ;point to next inversion
        inc     bx                      ;increment counter/pointer
        cmp     bx,bp                   ;all inversions checked?
        jl      tr_check                ;loop back, until done
        jmp     short tr_end            ;append an end marker

tr_rec:
        cmp     ah,0ffh                 ;is this the first set?
        jne     tr_rec2                 ;if not skip set-up procedure

        mov     ah,pcset1+[bx]          ;get reference value
        jmp     short tr_cont           ;continue tr_rec

tr_rec2:
        mov     al,pcset1+[bx]          ;get value to be subtracted
        sub     al,ah                   ;AL = interval of transposition
        cmp     al,6                    ;largest interval of transposition
        jg      short tr_cont           ;redundant interval, continue loop
        mov     [di],al                 ;record AL in trans_sym space
        inc     di                      ;bump pointer to next position
        jmp     short tr_cont           ;continue in loop

tr_end:
        mov     byte ptr [di],0ffh      ;end marker for trans_sym

;----- move the normal order of the set to pcset2 for further processing -----
;      pctemp --> pcset2

bn_order:
        mov     si,offset pctemp        ;position source pointer
        mov     di,offset pcset2        ;position destination pointer
        mov     cx,bp                   ;inversion counter

bn_check:
        mov     al,[si+12]              ;get a flag byte
        cmp     al,0ffh                 ;check for 0ffh (winning inversion)
        je      bn_mov                  ;match found, continue processing

        add     si,13                   ;point to next inversion
        loop    bn_check                ;loop back until done
        jmp     input_err               ;error -- no 0ffh marker found

bn_mov:
        mov     cx,bp                   ;note counter
bn_mov2:
        lodsb                           ;get a pc
        mov     [di],al                 ;store the pc in pcset1
        inc     di                      ;bump pointer
        loop    bn_mov2                 ;loop back until done
        mov     byte ptr [di],0ffh      ;insert FFh end marker

;----- check to see if there is already a normal order in pcset3 -----

        mov     al,pcset3+12            ;AL = pcset3 flag
        cmp     al,0ffh                 ;FFh indicates normal order present
        jne     copy2to3                ;if not present, go on
        jmp     short best_no1          ;if so, go to compare normal orders

;----- copy pcset2 to pcset3 for later comparison and set pcset3 flag -----

copy2to3:
        mov     si,offset pcset2        ;position source pointer
        mov     di,offset pcset3        ;position destination pointer
        mov     cx,bp                   ;initialize loop counter

bn_mov3:
        lodsb                           ;get a pc
        mov     [di],al                 ;store the pc in pcset1
        inc     di                      ;bump pointer
        loop    bn_mov3                 ;loop back until done

        mov     al,0ffh                 ;FFh = pcset3 flag (no. order present)
        mov     pcset3+12,al            ;store pcset3 flag

;----- invert the normal order -----
;      pcset3 --> pcset1 (inversion of pcset1)

        std                             ;set direction flag (high to low)
        mov     si,offset pcset3        ;position source pointer
        add     si,bp                   ;add set length
        dec     si                      ;point to last pc of the set

        mov     di,offset pcset1        ;position destination pointer
        mov     cx,bp                   ;initialize loop counter

        mov     ah,[si]                 ;get reference pc (highest value)
        mov     dh,ah                   ;save the value

no_inv1:
        lodsb                           ;get a pc value
        sub     ah,al                   ;invert by subtraction
        mov     [di],ah                 ;store the result in pcset1
        inc     di                      ;bump destination pointer
        mov     ah,dh                   ;restore reference value in AH
        loop    no_inv1                 ;loop back until done
        cld                             ;clear direction flag

        mov     bx,bp                   ;BP = number of pc's in the set
        jmp     ext_begin               ;find normal order of inverion

;----- copy pcset3 to pcset1 for comparison to find best normal order -----

best_no1:
        mov     si,offset pcset3        ;position source pointer
        mov     di,offset pcset1        ;position destination pointer
        mov     cx,bp                   ;initialize loop counter

best_no2:
        lodsb                           ;get a pc
        mov     [di],al                 ;store the pc in pcset1
        inc     di                      ;bump pointer
        loop    best_no2                ;loop back until done

        mov     byte ptr [di],0ffh      ;insert FFh end marker

;----- compare normal orders to find best normal order -----

        mov     bx,1                    ;initialize position counter

best_compare:
        mov     al,[pcset1+bx]          ;get value from pcset1
        mov     ah,[pcset2+bx]          ;get value from pcset2

        cmp     al,ah                   ;compare the 2 sets
        jne     best_no3                ;best normal order exists

        inc     bx                      ;bump pointer for position checked
        cmp     bx,bp                   ;check for last position
        jl      best_compare            ;loop back until done

;----- pc sets are the same, the sets are inversionally symmetrical -----

        mov     di,ans_addr             ;DI = output address
        mov     byte ptr [di],0ffh      ;set flag for inversional symmetry

        mov     dx,offset pcset1        ;set source pointer
        jmp     short prime1            ;go to copy the answer in 'prime'

;----- set source pointer to point at the set in best normal order -----

best_no3:
        jl      pc1_no                  ;AL (pcset1) < AH (pcset2)

        mov     dx,offset pcset2        ;pcset2 is in best normal order
        jmp     short prime1            ;go to copy the answer in prime

pc1_no:
        mov     dx,offset pcset1        ;pcset1 is in best normal order

prime1:
;----- copy the best normal order into 'prime' (the answer blank) -----

        mov     di,ans_addr             ;position destination pointer
        add     di,13                   ; to prime form space

prime3:
        mov     si,dx                   ;position source pointer
        mov     cx,bp                   ;initialize loop counter
        mov     bx,si                   ;BX = offset of prime (for int_vect)

prime4:
        lodsb                           ;get a pc
        mov     [di],al                 ;store the pc in prime
        inc     di                      ;bump the destination pointer
        loop    prime4                  ;loop back until done

        mov     byte ptr [di],0ffh      ;FF = end marker

;----- compute the interval vector -----

        xor     al,al                   ;AL = 0 (to clear int_vect)
        mov     di,ans_addr             ;position pointer to int_vect
        add     di,26                   ; position in output buffer
        mov     cx,6                    ;initialize counter
int_v5:
        mov     [di],al                 ;initialize int_vect to 0
        inc     di                      ;bump pointer
        loop    int_v5                  ;loop until done
        xor     ch,ch                   ;so CX = CL

        cmp     bp,1                    ;check for 1-note pc set
        je      short intv_end          ;no interval vector, return from proc

        mov     di,ans_addr             ;position pointer to int_vect
        add     di,26                   ; position in output buffer
        mov     si,bx                   ;position source pointer to prime form

int_v4:
        mov     bx,1                    ;BX = 1

        mov     ah,[si]                 ;get lower value

int_v2:
        mov     al,[si+bx]              ;get higher value
        cmp     al,0ffh                 ;check for end marker (FF)
        je      int_v3                  ;exit inner loop

        sub     al,ah                   ;AL - AH --> AL
        cmp     al,6                    ;6 = largest interval class
        jle     int_v1                  ;IC is within bounds, continue

        mov     dl,12                   ;to invert interval
        sub     dl,al                   ;find IC by inverting
        mov     al,dl                   ;AL = interval class

int_v1:
        dec     al                      ;offset for 'int_vect'
        mov     cl,al                   ;CL = CX = AL
        add     di,cx                   ;compute the offset
        mov     dl,[di]                 ;get count for this IC
        inc     dl                      ;add on eto the count
        mov     [di],dl                 ;store the updated count
        sub     di,cx                   ;DI = points the int_vect

        inc     bx                      ;increment pointer to higher value
        jmp     short int_v2            ;loop back

int_v3:
        inc     si                      ;bump pointer to lower value
        mov     al,[si+1]               ;check last pitch class
        cmp     al,0ffh                 ;check for end marker (FF)
        jne     int_v4                  ;continue outer loop

intv_end:
        mov     di,ans_addr             ;DI = output buffer
        add     di,32                   ;add offset to end of interval vector
        mov     byte ptr [di],0ffh      ;append FF end marker
        jmp     short return            ;exit this procedure

;------------------------------------------------------------------------------

input_err:                              ;invalid data, abort program
        mov     di,ans_addr             ;set position for error indication
        mov     byte ptr [di],0f0h      ;F0 = input error indication

return:
        pop     di              ;restore registers
        pop     si
        pop     dx
        pop     cx
        pop     bx
        pop     ax

        ret                     ;return from procedure
pc_type endp
        end
