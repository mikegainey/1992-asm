comment |----------------------------------------------------------------------

File:  print1.asm  by Mike Gainey (2/23/92)

Modifications:  3/2/92  -- added imbedded functions 5,6,7
                3/3/92  -- appended 'txtio.asm' to this file
                3/13/92 -- added auto word-wrap, scrolling, and more
                3/16/92 -- control codes accessed via jump table, and more
                3/30/93 -- added "toggle intensity bit of window" function

Description:

  A flexible text output routine using direct video access using the
  txtio's v_chrout and v_getparams procedures.

  Entry:  DS:SI points to data to be displayed

    embedded control codes:

         00 = end of message
         01 = set video text mode (if not already in text mode)
         02 = set hardware/software cursor position (+1 byte: 00 = hide,
              01 = hardware at software ,02 = software at hardware)
         03 = clear screen (current attribute in AH used)
         04 = blank a window (+4 bytes:  top-left row,col
              (current attribute used)   bottom-right row,col)
         05 = carriage return and linefeed w/ scroll if needed
         06 = scroll the screen up one line
         07 = set display coordinates (+2 bytes:  row, column)
              0FFh = no change (for either coordinate)
         08 = relative cursor move (+2 bytes:  +/- row, +/- column)
         09 = change display attribute
         10 = center a line (+2 bytes:  length, row)
         11 = auto word wrap (+1 byte:  00 = off, FF = on)
         12 = change display direction
              (+1 byte:  00 = right, 01 = down, -1 = up)
         13 = toggle intensity bit of window (+4 bytes:  top-left row,col
                                                     bottom-right row,col)
         255 = on/off for control code recognition (so they can be displayed)
               (+1 byte: FF = off, 00 = on)


  Exit:  text is printed to the screen
------------------------------------------------------------------------------|
skip32s macro                           ;skips spaces after crlf's
        local   skp_lp                  ;declare local variable
skp_lp: lodsb                           ;skip any spaces
        cmp     al,32                   ;32 = space
        je      skp_lp                  ;loop to skip spaces
        dec     si                      ;back up one (to the first non-space)
        endm
;------------------------------------------------------------------------------

        .model  small

        .data   ;--------------------------------------------------------------

VideoStruct     struc
  rows          db      0
  columns       db      0
  IsCGA         db      0
  IsColor       db      0
  VideoSegment  dw      0
  VideoOffset   dw      0
VideoStruct     ends

VideoData       VideoStruct <0,0,0,0,0,0>       ;initialize to 0

tl_r            db      0               ;top-left row
tl_c            db      0               ;top-left column
br_r            db      0               ;bottom-right row
br_c            db      0               ;bottom-right column
ctr_flag        db      0               ;center routine flag
wrapflag        db      0               ;auto word-wrap flag
ctrlflag        db      0               ;control recognition on/off flag
cur_dir         db      0               ;cursor direction (0=rt, 1=dn, -1=up)

prevStruct      struc
  row           db      0
  col           db      0
  atrb          db      0
prevStruct      ends

prev            prevStruct <0,0,7>              ;initialize to defaults

;----- function offset jump table ---------------------------------------------
jmptable        dw      endofmess       ;00 = end of message
                dw      set_video_mode  ;01 = set video mode (to text)
                dw      set_cursor      ;02 = set hardware cursor
                dw      clrscrn         ;03 = clear screen
                dw      blank_window    ;04 = blank a window (on the screen)
                dw      crlfcode        ;05 = carriage return and linefeed
                dw      scrlcode        ;06 = scroll the screen up one line
                dw      locate          ;07 = locate cursor (x,y)
                dw      rel_cursor_move ;08 = relative cursor move
                dw      ch_attr         ;09 = change attribute
                dw      center_line     ;10 = centers text on a line
                dw      wrap_switch     ;11 = auto word wrap on/off switch
                dw      cursor_dir      ;12 = change display direction
                dw      int_win         ;13 = toggle intensity bit of a window

        .code   ;--------------------------------------------------------------
        public print
        include macs.inc                ;misc. macros

print   proc    ;--------------------------------------------------------------

        @pushreg                        ;save entry registers

        mov     bx,offset VideoData     ;BX = pointer to VideoData

CheckData:                              ;check VideoData
        cmp     [bx].VideoSegment,0     ;check for valid VideoData
        jne     set_previous            ;if not 0, continue
        call    v_getparams             ;video parameters --> VideoData
        
set_previous:                           ;previous screen location and attribute
        mov     dh,prev.row             ;get row from previous call
        mov     dl,prev.col             ;get column+1 from previous call
        mov     ah,prev.atrb            ;get attribute from previous call

;--------------------------- Main Loop ----------------------------------------
get_chr:                                ;beginning of main loop of procedure
        lodsb                           ;get a byte (character) in AL; inc si

        cmp     al,0ffh                 ;check for ctrlflag code
        jne     no_ff                   ;not FF, continue
        jmp     ctrl_switch             ;execute switch function
no_ff:
        cmp     ctrlflag,0ffh           ;FF = control codes are turned off
        je      go_display              ;skip codes and display

        cmp     al,13                   ;function code (0-13) or character?
        jbe     ctrlcode                ;code, jump to routine (from table)

        jmp     short go_display        ;go to display the character
ctrlcode:
        mov     di,ax                   ;move function code to DI
        and     di,0ffh                 ;mask most significant byte
        shl     di,1                    ;multiply by two to get correct offset
        jmp     jmptable[di]            ;jump to routine using jmptable
        


;----- print a character at the current location -----

comment *    DS:BX = points to VideoData
             AL = character to be displayed
             DH = row position
             DL = column position
             AH = attribute
        *    ------------------------------

go_display:
        inbound dh,0,24                 ;make sure DH (row) is within bounds
        inbound dl,0,79                 ;make sure DL (column) is within bounds

        call    v_chrout                ;***** display the character! *****

        cmp     cur_dir,0               ;check cursor direction
        je      cur_right               ;right to left
        jmp     cur_vert                ;execute vertical cursor routine
cur_right:
        inc     dl                      ;next column position

        cmp     dl,79                   ;79 is the last column
        jbe     goodx                   ;good x-coordinate

        call    cr_lf                   ;carriage return and line feed
        skip32s                         ;skip spaces (32's)
goodx:
        cmp     al,32                   ;to detect spaces (32)
        jne     endmain                 ;not a space, continue
        cmp     wrapflag,0ffh           ;check if auto word-wrap is on
        jne     endmain                 ;if not, continue
        jmp     wordwrap                ;goto wordwrap code
endmain:
        jmp     get_chr                 ;loop back for next character
;--------------------------- End of Main Loop ---------------------------------

;----- end of message -----
endofmess:                      ;end of message marker
        mov     prev.row,dh     ;save row position
        mov     prev.col,dl     ;save column position
        mov     prev.atrb,ah    ;save attribute

        @popreg                 ;restore registers

        ret                     ;return to main code

;----- set video mode -----
set_video_mode:
        push    ax                      ;save AX (attribute)
        push    bx                      ;save BX pointer to VideoData

        mov     ah,0fh                  ;BIOS function 0Fh: get video mode
        int     10h                     ;goto BIOS
        and     al,7fh                  ;clear most significant bit
        cmp     al,3                    ;text mode (color)
        je      already_text            ;already text mode, continue
        cmp     al,7                    ;text mode (monochrome)
        je      already_text            ;already text mode, continue

        mov     al,VideoData.IsColor    ;get color indication
        cmp     al,1                    ;color or mono
        je      color_mode              ;color mode

        mov     al,87h                  ;monochrome mode = 7 (msbit set)
        jmp     short set_vmode         ;skip color mode

color_mode:
        mov     al,83h                  ;color mode = 3 (msbit set)

set_vmode:
        mov     ah,0                    ;BIOS function 00h: set video mode
        int     10h                     ;goto BIOS

already_text:
        pop     bx                      ;restore BX
        pop     ax                      ;restore AX
        jmp     get_chr                 ;go back to the main loop of the proc

;----- position software/hardware cursor -----
set_cursor:
        push    ax              ;save AX (attribute)
        push    bx              ;save BX (points to VideoData)

        lodsb                   ;get the subfunction character in AL
        cmp     al,0            ;00 = hide the hardware cursor
        jne     setc1           ;not 00, check the next code

        push    dx              ;save DX (row, column)
        mov     dh,25           ;26th screen row (hide hardware cursor)
	mov	dl,0		;1st column
	mov	ah,2		;BIOS service request number
	mov	bh,0		;display page number 0
	int	10h		;BIOS video interrupt
        pop     dx              ;restore DX (row, column)
        jmp     short sc_end    ;leave this module
setc1:
        cmp     al,1            ;01 = move hardware cursor to 'print' cursor
        jne     setc2           ;not 01, check the next code

        mov     ah,2            ;int10h #2:  set cursor position
        xor     bx,bx           ;page 0
                                ;DH and DL are already set (screen coordinates)
        int     10h             ;goto BIOS (set cursor position)
        jmp     short sc_end    ;leave this module
setc2:
        cmp     al,2            ;02 = move print cursor to hardware cursor pos.
        jne     sc_end          ;no code found, leave this module
        mov     ah,3            ;int10h #2:  get cursor position
        xor     bh,bh           ;page 0
        int     10h             ;goto BIOS (get cursor position in DH, DL)
        jmp     short sc_end    ;leave this module
sc_end:
        pop     bx              ;restore BX (points to VideoData)
        pop     ax              ;restore AX (attribute)

        jmp     get_chr         ;go back to the main loop of the procedure

;----- clear screen -----
clrscrn:
        push    dx                              ;save DX register
        push    es                              ;save ES register
        mov     al,32                           ;blank character
        mov     dx,VideoData.VideoSegment       ;get video mem segment
        mov     es,dx                           ;ES = video segment
        mov     di,0                            ;screen pointer = top, left
        mov     cx,2000                         ;set iteration counter
        rep     stosw                           ;clear the screen
        pop     es                              ;restore ES register
        pop     dx                              ;restore DX register

        jmp     get_chr                         ;go to main loop of procedure

;----- blank window given top-left (row,col) and bottom-right (row,col) -----
blank_window:
        push    dx              ;save DX register (software cursor position)
        lodsb                   ;get byte and inc si
        inbound al,0,24         ;make sure AL is within bounds (0-24)
        mov     tl_r,al         ;save top-left row
        lodsb                   ;get byte and inc si
        inbound al,0,79         ;make sure AL is within bounds (0-79)
        mov     tl_c,al         ;save top-left column
        lodsb                   ;get byte and inc si
        inbound al,0,24         ;make sure AL is within bounds (0-24)
        mov     br_r,al         ;save bottom-right row
        lodsb                   ;get byte and inc si
        inbound al,0,79         ;make sure AL is within bounds (0-79)
        mov     br_c,al         ;save bottom-right column

	mov	al,32		;blank character
        mov     dh,tl_r         ;top-left row
bl_loop2:
        mov     dl,tl_c         ;top-left column

bl_loop1:
	call	v_chrout	;output to screen

	inc	dl		;next column
        cmp     dl,br_c         ;compare with bottom_right column #
        jbe     bl_loop1        ;continue column loop (inner)

	inc	dh		;next row
        cmp     dh,br_r         ;compare with bottom-right row #
        jbe     bl_loop2        ;continue row loop (outer)

        pop     dx              ;restore DX register (software cursor position)
        jmp     get_chr         ;go back to the main loop of the procedure

;----- carriage return and linefeed /w scroll if needed (procedure) -----
crlfcode:
        call    cr_lf                   ;carriage return and linefeed
        jmp     get_chr                 ;go back to the main loop

cr_lf   proc    ;--------------------------------------------------------------
        xor     dl,dl                   ;carriage return (column = 0)
        inc     dh                      ;next line
        cmp     dh,24                   ;24 = last line (row)
        jbe     noscroll                ;row value is valid

        call    scroll                  ;scroll the screen up one line
        mov     dh,24                   ;set to last row (24th)
noscroll:
        ret                             ;return from this procedure
cr_lf   endp    ;--------------------------------------------------------------

;---- scroll the screen up one line (procedure) -----

scrlcode:
        call    scroll                  ;execute this routine
        jmp     get_chr                 ;get next character

scroll  proc    ;--------------------------------------------------------------
        push    bx                              ;save offset to VideoData
        push    dx                              ;save current column and row
        push    si                              ;save pointer to proc input
        push    es                              ;save ES register
        push    ax                              ;save attribute (AH)
        push    ds                              ;save DS register

        mov     ax,VideoData.VideoSegment       ;get video memory address
        mov     es,ax                           ;setup ES register
        mov     ds,ax                           ;setup DS register

        cld                                     ;clear direction flag (ascend)
        mov     si,160                          ;position source pointer
        xor     di,di                           ;position destination pointer
        mov     cx,1920                         ;80 words/line x 24 lines
        rep     movsw                           ;scroll the screen
        pop     ds                              ;restore the DS register

        pop     ax                              ;restore attribute (AH)
        mov     al,32                           ;32 = blank character (space)
        mov     di,3840                         ;last line: 24 x 80 x 2 = 3840
        mov     cx,80                           ;blank 80 chars (last line)
        rep     stosw                           ;blank last line after scroll
        pop     es                              ;restore the ES register

        pop     si                              ;pointer to procedure input
        pop     dx                              ;current column and row
        pop     bx                              ;offset to VideoData

        ret                                     ;to the main loop of procedure
scroll  endp    ;--------------------------------------------------------------

;----- locate: set display coordinates -----
locate:                         ;set display coordinates
        lodsb                   ;AL = row position
        cmp     al,0ffh         ;FF = no change
        je      samerow         ;don't change the row position
        mov     dh,al           ;DH = row position
samerow:
        lodsb                   ;AL = column position
        cmp     al,0ffh         ;FF = no change
        je      samecol         ;don't change the column position
        mov     dl,al           ;DL = column position
samecol:
        jmp     get_chr         ;go back to the main loop of the procedure

;----- relative cursor move -----
rel_cursor_move:
        lodsb                           ;AL = row shift
        add     dh,al                   ;modify DH (row position)
        inbound dh,0,24                 ;make sure DH is within bounds (0-24)
        lodsb                           ;AL = column shift
        add     dl,al                   ;modify DL (column position)
        inbound dl,0,79                 ;make sure DL is within bounds (0-79)
        jmp     get_chr                 ;go back to main loop of procedure

;----- change attribute -----
ch_attr:                        ;change attribute
        mov     ah,[si]         ;AH = attribute
        inc     si              ;bump pointer to next character
        jmp     get_chr         ;go back to the main loop of the procedure

;----- center a line -----
center_line:
        mov     al,ctr_flag     ;check if this is just and end marker
        cmp     al,0ffh         ;FF = means the next 10 is just an end marker
        jne     center1         ;if not, execute the routine

        xor     al,al           ;AL = 0
        mov     ctr_flag,al     ;clear the flag
        jmp     get_chr         ;get next character

center1:
        lodsb                   ;AL = row position
        cmp     al,0ffh         ;FF = use current row
        je      center2         ;use current row, continue
        mov     dh,al           ;DH = row for centered line
center2:
        push    si              ;save source pointer
        mov     cx,si           ;CX = beginning of string

center_loop:
        lodsb                   ;get string length
        cmp     al,10            ;look for end of string marker (10)
        jne     center_loop     ;loop until end marker is found

        sub     si,cx           ;find length of the string
        dec     si              ;don't count the end marker
        mov     cx,si           ;CL = length of the string
        mov     al,cl           ;AL = length of the string

        pop     si              ;get original SI back

	mov	dl,80		;total number of columns
        sub     dl,al           ;find number of blank spaces
        cmp     dl,0            ;negative DL means string longer than line
        jg      valid_stg       ;string length is valid
        xor     dl,dl           ;set DL to 0
valid_stg:
	shr	dl,1		;divide DL by two

        mov     ctr_flag,0ffh   ;set flag so main loop will skip end marker
        jmp     get_chr         ;go back to the main loop of the procedure

;----- automatic word-wrap routine (switch) -----
wrap_switch:
        lodsb                           ;get on/off indicator
        cmp     al,0ffh                 ;FF = auto word-wrap on
        je      wrap_on                 ;FF code found
        mov     wrapflag,0              ;turn off auto word-wrap
        jmp     get_chr                 ;go back to main loop of procedure
wrap_on:
        mov     wrapflag,0ffh           ;turn on auto word-wrap
        jmp     get_chr                 ;go back to main loop of procedure

;----- auto word-wrap:  whenever wrapflag=FF and a space (32) is found -----
wordwrap:
        push    si                      ;save source (input) pointer position
        mov     cx,si                   ;CX = SI
space_search:
        lodsb                           ;get a character (looking for a space)
        cmp     al,32                   ;check for a space
        je      found_space             ;space found, compute word length
        ja      space_search            ;keep looking for a space
        pop     si                      ;control code found, abort word-wrap
        jmp     get_chr                 ;go back to main loop of procedure
found_space:
        sub     si,cx                   ;find distance between spaces
        dec     si                      ;SI = word length
        mov     cx,si                   ;CX = word length
        pop     si                      ;get source pointer back

        push    ax                      ;save AX register (attribute)
        mov     al,80                   ;80 columns per line
        sub     al,dl                   ;AL = columns left on the line
        xor     ah,ah                   ;so AX = columns left on the line

        cmp     cx,ax                   ;will the word fit on the line?
        ja      wrapword                ;no, put the word on the next line
        pop     ax                      ;get AX (attribute) back
        jmp     get_chr                 ;yes, continue as usual

wrapword:
        pop     ax                      ;get AX (attribute) back
        call    cr_lf                   ;carriage return and linefeed
        skip32s                         ;skip spaces (32's)
        jmp     get_chr                 ;go back to the main loop

;----- control code recognition switch (00 = on, FF = off) -----
ctrl_switch:
        lodsb                           ;get character
        mov     ctrlflag,al             ;update control code recognition flag
        jmp     get_chr                 ;goto main loop

;----- cursor direction switch and routine -----
cursor_dir:
        lodsb                           ;AL = direction indication
        mov     cur_dir,al              ;update 'cur_dir' byte
        jmp     get_chr                 ;goto main loop

cur_vert:
        add     dh,cur_dir              ;update cursor position
        jmp     get_chr                 ;goto main loop

;----- set attribute of a window -----
int_win:
        push    ax              ;save AX register (attribute)
        push    dx              ;save DX register (software cursor position)

        lodsb                   ;get byte and inc si
        inbound al,0,24         ;make sure AL is within bounds (0-24)
        mov     tl_r,al         ;save top-left row
        lodsb                   ;get byte and inc si
        inbound al,0,79         ;make sure AL is within bounds (0-79)
        mov     tl_c,al         ;save top-left column
        lodsb                   ;get byte and inc si
        inbound al,0,24         ;make sure AL is within bounds (0-24)
        mov     br_r,al         ;save bottom-right row
        lodsb                   ;get byte and inc si
        inbound al,0,79         ;make sure AL is within bounds (0-79)
        mov     br_c,al         ;save bottom-right column

        mov     dh,tl_r         ;top-left row
at_loop2:
        mov     dl,tl_c         ;top-left column

at_loop1:
        call    v_getchattr     ;get character and attribute at (DH,DL)
        test    ah,00000111b    ;check for any color bits (r g b)
        jz      at_next         ;if they're off, skip this character
        test    ah,00001000b    ;check intensity bit
        jz      set_i           ;intensity bit was off
        and     ah,11110111b    ;turn intensity bit off
        jmp     short wrt_attr  ;screen output
set_i:
        or      ah,00001000b    ;turn intensity bit on
wrt_attr:
	call	v_chrout	;output to screen

at_next:
	inc	dl		;next column
        cmp     dl,br_c         ;compare with bottom_right column #
        jbe     at_loop1        ;continue column loop (inner)

	inc	dh		;next row
        cmp     dh,br_r         ;compare with bottom-right row #
        jbe     at_loop2        ;continue row loop (outer)

        pop     dx              ;restore DX register (software cursor position)
        pop     ax              ;restore AX register (attribute)
        jmp     get_chr         ;go back to the main loop of the procedure

;------------------------------------------------------------------------------
print   endp

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

comment |

-------------------------------------------------------------------------------
  File:  TXTIO.ASM  (appended to 'print.asm)
         note: only v_getparams and v_chrout procedures are included
               v_strout and v_getchattr are not included

  Low-level text mode I/O routines

  Author:  Naba Barkakati, 3/4/89

  p. 573-78 of Microsoft Macro Assembler Bible (The Waite Group)

  Description:  Direct video routines compatible with all popular displays.
------------------------------------------------------------------------------|

BIOSdata        segment at 40h
    org 49h
  VideoMode     db      ?
  ScreenColumns db      ?
    org 4eh
  VMemOffset    dw      ?
    org 63h
  VideoPort     dw      ?
    org 84h
  ScreenRows    db      ?
    org 87h
  EGAinfo       db      ?
BIOSdata        ends

;------------------------------------------------------------------------------
;  Define Macros

ComputeVideoAddress     macro
;  Entry: DS:BX pointer to videodata
;         DH,DL row, column
;  Exit : DI will be offset of byte in video memory
;         ES will be segment address of video memory

        push    ax
        mov     al,[bx].columns
        mul     dh
        xor     dh,dh
        add     ax,dx
        shl     ax,1
        add     ax,[bx].VideoOffset
        mov     di,ax
        pop     ax
        mov     es,[bx].VideoSegment
        endm

SynchWait               macro
                        local   wait1, wait2
;  Entry: none
;  Exit : horizontal retrace begins

        mov     dx,3dah
wait1:  in      al,dx
        test    al,1
        jnz     wait1
wait2:  in      al,dx
        test    al,1
        jz      wait2
        endm

;------------------------------------------------------------------------------
;  Determine video information
;  Entry: DS:BX points to VideoStruct structure
;  Exit : Structure is filled with video info

v_getparams     proc
                mov     ax,BIOSdata     ;set up ES to address segment
                mov     es,ax           ;BIOSdata
                assume es:BIOSdata
                push    bx
                mov     ah,12h          ;call EGA BIOS function 12h
                mov     bl,10h          ;by using video interrupt 10h
                int     10h             ;to get info
                mov     cl,bl
                pop     bx
                cmp     cl,10h
                jne     isEGAactive
                cmp     VideoPort,3d4h
                jne     NotCGA1
                mov     [bx].IsCGA,1
                jmp     short skipNotCGA
NotCGA1:
                mov     [bx].IsCGA,0
skipNotCGA:
                mov     [bx].rows,25
                jmp     short checkColor
IsEGAactive:
                test    EGAinfo,8
                je      EGAactive
                cmp     VideoPort,3d4h
                jne     EGAactive
                mov     [bx].isCGA,1
                jmp     short skipEGAactive
EGAactive:
                mov     [bx].IsCGA,0
skipEGAactive:
                mov     al,ScreenRows
                mov     [bx].rows,al
                inc     [bx].rows
CheckColor:
                cmp     VideoPort,3d4h
                jne     Mono
                mov     [bx].IsColor,1
                mov     word ptr [bx].VideoSegment,0b800h
                jmp     short skipMono
Mono:
                mov     [bx].IsColor,0
                mov     word ptr [bx].VideoSegment,0b000h
skipMono:
                mov     al,ScreenColumns
                mov     [bx].columns,al
                mov     ax,VMemOffset
                mov     [bx].VideoOffset,ax
                ret
v_getparams     endp

;------------------------------------------------------------------------------
;  v_chrout:  Print a single character
;
;  Entry: DS:BX points to VideoStruct structure
;         AL = character to be displayed
;         DH = row position
;         DL = column position
;         AH = attribute

v_chrout        proc
                push    ax
                push    cx
                push    dx
                push    di
                push    es
                ComputeVideoAddress
                test    byte ptr [bx].IsCGA,1
                jz      coutnosynch
                mov     cx,ax
                cli
                SynchWait
                xchg    ax,cx
                stosw
                sti
                jmp     short coutdone
coutnosynch:
                test    byte ptr [bx].IsColor,1         ;check color/mono
                jnz     chrout                          ;skip if color

                or      ah,00000111b                    ;white foreground
                and     ah,10001111b                    ;black background

chrout:
                stosw
coutdone:
                pop     es
                pop     di
                pop     dx
                pop     cx
                pop     ax
                ret
v_chrout        endp

;------------------------------------------------------------------------------
;  v_getchattr:  Return character and attribute
;
;  Entry: DS:BX = address of VideoStruct structure
;         DH = row position
;         DL = column position
;  Exit : AL = character at that position
;         AH = attribute

v_getchattr     proc
                push    dx
                push    di
                push    es
                ComputeVideoAddress
                test    byte ptr [bx].IsCGA,1
                jz      gcnosynch
                SynchWait
gcnosynch:
                mov     ax,es:[di]
                pop     es
                pop     di
                pop     dx
                ret
v_getchattr     endp
                end
