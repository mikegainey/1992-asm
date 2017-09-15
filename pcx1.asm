;graphics experiment program with .pcx files

        .model  small
        .stack
        .data   ;--------------------------------------------------------------

title1  db      stm,cls,attr,2, ctr,1,'PCX file viewer',ctr
        db      ctr,2, 'test program -- Mike Gainey (3/22/92)',ctr
        db      pos,5,0, 'Input .pcx filename:'
        db      pos,6,0, '> ',pcur,1,0

all_err db      crlf,13 dup ('error '),crlf,pcur,1,0

pcxfile db      40,?,42 dup(0)          ;buffer for pxc filename (path)
handle  dw      0                       ;file handle

file_msw        dw      0               ;file size most sig word
file_lsw        dw      0               ;file size least sig word
file_para       dw      0               ;file size in paragraphs
file_seg        dw      0               ;base segment address of allocated blk

img_seg         dw      0               ;base segment address of unpacked image

toread_msw      dw      0               ;bytes to be read into memory
toread_lsw      dw      0               ;bytes to be read into memory
readptr_seg     dw      0               ;read pointer (segment)

pcxheader       struc                   ;define header structure
  manu          db      ?               ;manufacturer (always 0A)
  vers          db      ?               ;version number (0,2,3,or 5)
  encode        db      ?               ;encoding (always 01)
  bppix         db      ?               ;bits per pixel (color bits)
  xmin          dw      ?               ;lower bound of x-coordinates
  ymin          dw      ?               ;lower bound of y-coordinates
  xmax          dw      ?               ;upper bound of x_coordinates
  ymax          dw      ?               ;upper bound of y_coordinates
  hres          dw      ?               ;horizontal resolution (of scanner)
  vres          dw      ?               ;vertical resolution   (of scanner)
  palet         db      48 dup(?)       ;color palette
  reserved      db      ?               ;reserved (?)
  cplane        db      ?               ;color planes
  bpline        dw      ?               ;bytes per line
  ptype         dw      ?               ;palette type (grey or color)
  filler        db      58 dup(?)       ;filler space (?)
pcxheader       ends

v2a_in          dw      ?                       ;val2asc input word
v2a_out         db      pos,0ffh,20,8 dup(?)    ;val2asc output string
disp_stg        db      80 dup(?)               ;for 'print' procedure
cr              db      crlf,0                  ;carriage return & line feed

h_head  db      pcur,0,crlf,crlf,attr,3,'--- PCX header ---',attr,2,crlf,crlf,0
h_data  db      'manufacturer:',1, 'version:',1, 'encoding:',1
        db      'bits per pixel:',1, 'x-min:',2, 'y-min:',2
        db      'x-max:',2, 'y-max:',2, 'horiz. resolution:',2
        db      'vert. resolution:',2,3, 'color planes:',1
        db      'bytes per line:',2, 'palette type:',2,4

        .code   ;--------------------------------------------------------------

        include bios.inc                ;BIOS macros
        include macs.inc                ;other macros

        extrn   print:proc, val2asc:proc

entry:
        mov     ax,@data                ;get data segment
        mov     ds,ax                   ;setup DS register

;----- resize program memory allocation -----
        mov     ax,es                   ;get psp segment (bottom)
        mov     bx,ss                   ;get stack segment (top)
        add     bx,40h                  ;add 1k stack space to top
        sub     bx,ax                   ;subtract (top - bottom)
        inc     bx                      ;add one (just in case)
        mov     ah,74                   ;int21h #74 (resize memory block)
        int     21h                     ;DOS function (resize memory block)
        jnc     start                   ;if ok, continue
        jmp     error                   ;if memory allocation error, abort

start:
        display title1                  ;title and input prompt (local macro)

        gets    pcxfile                 ;get asciiz string (filename)

;----- open file -----
        mov     ah,61                   ;int21 #61 (open file)
        xor     al,al                   ;read access only
        mov     dx,offset pcxfile       ;pointer to filename buffer
        add     dx,2                    ;DX points to filename
        int     21h                     ;DOS function (open file)

        jnc     open_ok                 ;no error opening file
        jmp     error                   ;error in opening file
open_ok:
        mov     handle,ax               ;save file handle

;----- find file length -----
        mov     ah,66                   ;int21h #66 (set file pointer)
        mov     al,2                    ;offset from end of file
        mov     bx,handle               ;BX = file handle
        xor     cx,cx                   ;offset = 0 (most significant half)
        xor     dx,dx                   ;offset = 0 (least significant half)
        int     21h                     ;DOS function (open file)

        jnc     size_ok                 ;no error sizing the file
        jmp     error                   ;error in sizing the file
size_ok:
        mov     file_msw,dx             ;save file size (most sig word)
        mov     toread_msw,dx           ;copy of file_msw for file read
        mov     file_lsw,ax             ;save file size (least sig word)
        mov     toread_lsw,ax           ;copy of file_lsw for file read

;----- find file length in paragraphs -----
        cmp     dx,0fh                  ;DX has to be <=16
        jbe     msw_ok                  ;DX is <=16
        jmp     error                   ;error, file is too big
msw_ok:
        mov     cl,4                    ;4 bit shift
        shr     ax,cl                   ;divide lsw by 16 by shifting
        mov     cl,12                   ;12 bit shift
        shl     dx,cl                   ;move bits 0-3 to 12-15 position
        add     ax,dx                   ;AX now equals file size in paragraphs
        inc     ax                      ;add one paragraph (for lost bits)
        mov     file_para,ax            ;save size in paragraphs

;----- allocate memory block -----
getmem:
        mov     ah,72                   ;int21h #72 (allocate memory block)
        mov     bx,file_para            ;BX = size in paragraphs
        int     21h                     ;DOS function (allocate memory block)

        jnc     alloc_ok                ;function successful
        jmp     error                   ;error, in memory allocation
alloc_ok:
        mov     file_seg,ax             ;save base segment of allocated block
        mov     readptr_seg,ax          ;copy of file_seg for file read
        mov     es,ax                   ;set ES register to segment of block

;----- set file pointer to beginning of file -----
        mov     ah,66                   ;DOS function (set file pointer)
        mov     al,0                    ;absolute offset from start of file
        mov     bx,handle               ;BX = file handle
        xor     cx,cx                   ;offset = 0
        xor     dx,dx                   ;offset = 0
        int     21h                     ;DOS function (set file pointer)
        jnc     readfile                ;check for error
        jmp     error                   ;error, abort program

;----- read pcx file into memory at file_seg -----
readfile:
        cmp     toread_msw,0            ; > one segment to be read from disk?
        ja      readfff0                ; > one seg, read FFF0h bytes

        mov     ah,63                   ;DOS function (read file)
        mov     bx,handle               ;BX = handle
        mov     cx,toread_lsw           ;CX = number of bytes to read
        mov     dx,readptr_seg          ;segment of allocated buffer for file
        push    ds                      ;save DS register
        mov     ds,dx                   ;DS = segment of buffer for pcx file
        xor     dx,dx                   ;DX = 0 offset
        int     21h                     ;DOS function (read file or device)
        pop     ds                      ;restore DS register
        jnc     readok1                 ;check for file read error
        jmp     error                   ;error, abort program
readok1:
        cmp     ax,cx                   ;correct number of bytes read?
        je      readok2                 ;yes, continue
        jmp     error                   ;error, abort program
readok2:
        jmp     short header            ;continue program

readfff0:
        mov     ah,63                   ;DOS function (read file)
        mov     bx,handle               ;BX = handle
        mov     cx,0fff0h               ;read FFFh segments
        mov     dx,readptr_seg          ;segment of allocated buffer for file
        push    ds                      ;save DS register
        mov     ds,dx                   ;DS = segment of buffer for pcx file
        xor     dx,dx                   ;DX = 0 offset
        int     21h                     ;DOS function (read file or device)
        pop     ds                      ;restore DS register

        add     readptr_seg,0fffh       ;update file buffer pointer
        sub     toread_lsw,0fff0h       ;subtract bytes read (lsw)
        sbb     toread_msw,0            ;subtract bytes read (msw)
        jmp     short readfile          ;loop back until done

;----- read pcx header info -----
header:
        mov     es,file_seg             ;BX = base of file in memory (segment)
        mov     bl,10                   ;base 10
        xor     cx,cx                   ;CX is temp storage switch w/ SI

        display h_head                  ;"PCX header"

        mov     si,offset h_data        ;position source pointer
        mov     di,offset disp_stg      ;position destination pointer

h_getc:
        lodsb                           ;get character
        cmp     al,5                    ;character or code?
        jbe     h_len                   ;length code

        mov     [di],al                 ;copy header string to disp_stg
        inc     di                      ;bump destination pointer
        jmp     short h_getc            ;loop back for next character

h_len:
        mov     byte ptr [di],0         ;end marker for header string
        display disp_stg                ;display header string

	cmp	al,1			;byte value?
        jne     h_int                   ;check for integer value

        xchg    si,cx                   ;get index for header data
        mov     al,es:[si]              ;get value
        inc     si                      ;bump source pointer
        xor     ah,ah                   ;so AX = AL
        mov     v2a_in,ax               ;val2asc input
        push    si                      ;save SI register
        mov     si,offset v2a_in        ;pointer to val2asc input
        mov     di,offset v2a_out+3     ;val2asc output
        call    val2asc                 ;convert number to ascii
        pop     si                      ;restore SI register
        display v2a_out                 ;display number
        display cr                      ;carriage return & line feed
        xchg    si,cx                   ;get index for header strings
        mov     di,offset disp_stg      ;position destination pointer
	jmp	short h_getc		;loop back until done

h_int:
	cmp	al,2			;integer value?
        jne     h_skip49                ;check for skip49 flag

	xchg	si,cx			;get index for header data
	mov	ax,es:[si]		;get value
	add	si,2			;bump source pointer
	mov	v2a_in,ax		;val2asc input
        push    si                      ;save SI register
        mov     si,offset v2a_in        ;pointer to val2asc input
	mov	di,offset v2a_out+3	;val2asc output
	call	val2asc 		;convert number to ascii
        pop     si                      ;restore SI register
        display v2a_out                 ;display number
        display cr                      ;carriage return & line feed
	xchg	si,cx			;get index for header strings
        mov     di,offset disp_stg      ;position destination pointer
	jmp	short h_getc		;loop back until done

h_skip49:
	cmp	al,3			;skip palette code?
	jne	h_end			;end header flag

        add     cx,49                   ;advance pointer past palette info
	jmp	short h_getc		;loop back until done

h_end:
	cmp	al,4			;check for end marker
        je      img_mem                 ;allocate memory for image data
        jmp     error                   ;error, abort program

;----- allocate memory for unpacked image -----
img_mem:
        mov     ah,72                   ;int21h #72 (allocate memory block)
        mov     bx,0ffffh               ;BX = size in paragraphs (max)
        int     21h                     ;DOS function (allocate memory block)
        jc      mem_available           ;find memory available
        jmp     error                   ;this allocation is supposed to fail
mem_available:
        mov     ah,72                   ;int21h #72 (allocate memory block)
        int     21h                     ;allocate largest block available
        mov     img_seg,ax              ;save segment of image memory

;----- unpack the image:  (file_seg --> img_seg) -----
        mov     ax,file_msw             ;file size (msw)
        mov     toread_msw,ax           ;toread_msw = bytes to be unpacked
        mov     ax,file_lsw             ;file_size (lsw)
        mov     toread_lsw,ax           ;toread_msw = bytes to be unpacked
        mov     si,128                  ;set up source pointer (past header)
        xor     di,di                   ;set up destination pointer
        xor     bp,bp                   ;initialize source bytes counter

        mov     es,img_seg              ;ES = destination segment register
        mov     dx,file_seg             ;get file segment
        push    ds                      ;save DS register
        mov     ds,dx                   ;DS = source segment register
        mov     cx,ds:[bpline]          ;CX = bytes per line (countdown)
        mov     bx,cx                   ;BX = bytes per line (reference)

img_getc:
        lodsb                           ;get a byte
        inc     bp                      ;BP = source bytes counter
        mov     ah,al                   ;AH = AL
        and     ah,11000000b            ;check two high bits (data or index?)
        cmp     ah,11000000b            ;equal = index, not equal = byte
        je      img_run                 ;a run of bytes

        stosb                           ;store byte in image buffer
        dec     cx                      ;CX = bytes remaining for line
        cmp     cx,0                    ;number of remaining bytes on this line
        je      img_eol                 ;end of line?
        jmp     short img_getc          ;loop back for more

img_run:
        and     al,00111111b            ;mask off two high bits
        mov     ah,al                   ;copy AL into AH
        push    cx                      ;save CX register
        mov     cl,al                   ;initialize counter
        xor     ch,ch                   ;so CX = CL
        lodsb                           ;get the byte to be duplicated CX times
        inc     bp                      ;BP = source bytes counter
        rep     stosb                   ;do the run of bytes
        pop     cx                      ;restore CX register
        mov     al,ah                   ;AL = index (repetition count)
        xor     ah,ah                   ;so AX = AL
        sub     cx,ax                   ;CX = bytes remaining for line
        cmp     cx,0                    ;end of line?
        je      img_eol                 ;end of line
        jmp     short img_getc          ;loop back for more

img_eol:
        mov     dx,si                   ;temp save for SI register
        mov     ax,ds                   ;AX = DS source segment register
        mov     cl,4                    ;for 4-bit shift
        shr     si,cl                   ;shift SI right four bits
        add     ax,si                   ;update source segment value
        mov     ds,ax                   ;update source segment register
        and     dx,0fh                  ;use only last four bits of old SI
        mov     si,dx                   ;update SI register

        mov     ax,ds                   ;save updated DS value
        pop     ds                      ;restore data DS register
        push    ax                      ;push updated DS on the stack

        mov     dx,di                   ;temp save for DI register
        mov     ax,es                   ;AX = ES destination segment register
        mov     cl,4                    ;for 4-bit shift
        shr     di,cl                   ;shift DI right four bits
        add     ax,di                   ;update destination segment value
        mov     es,ax                   ;update destination segment register
        and     dx,0fh                  ;use only last four bits of old DI
        mov     di,dx                   ;update DI register

        sub     toread_lsw,bp           ;update toread_lsw
        sbb     toread_msw,0            ;update toread_msw
        cmp     toread_msw,0            ;check if more bytes to read
        jne     img_cont1               ;more bytes, continue
        cmp     toread_lsw,128          ;check if more bytes to read
        jbe     unpacked                ;file is unpacked, exit this loop

img_cont1:
        pop     ds                      ;DS = updated value
        xor     bp,bp                   ;reset source bytes counter (BP)
        mov     cx,bx                   ;initialize bytes remaining for line
        jmp     short img_getc          ;loop back until file is unpacked

unpacked:
        jmp     short exit

error:
        display all_err
exit:
        mov     ah,76                   ;int21h #76 (exit to DOS)
        mov     al,0                    ;no error code
        int     21h                     ;DOS function
        end


