;program to display the 16 colors available

        .model  small
        .stack
        .data   ;--------------------------------------------------------------

title1  db      stm,pcur,0,cls,attr,2,ctr,1,'Color display demo',ctr
        db      ctr,2,'by Mike Gainey (3/25/92)',ctr,crlf,crlf,0

colors  db      attr,7,'color 0 =  black',crlf
        db      attr,1,'color 1 =  blue',crlf
        db      attr,2,'color 2 =  green',crlf
        db      attr,3,'color 3 =  cyan',crlf
        db      attr,4,'color 4 =  red',crlf
        db      attr,5,'color 5 =  magenta',crlf
        db      attr,6,'color 6 =  brown',crlf
        db      attr,7,'color 7 =  white',crlf
        db      attr,8,'color 8 =  dark grey',crlf
        db      attr,9,'color 9 =  light blue',crlf
        db      attr,10,'color 10 = light green',crlf
        db      attr,11,'color 11 = light cyan',crlf
        db      attr,12,'color 12 = light red',crlf
        db      attr,13,'color 13 = light magenta',crlf
        db      attr,14,'color 14 = yellow',crlf
        db      attr,15,'color 15 = bright white',crlf,crlf
        db      pcur,1,0

        .code   ;--------------------------------------------------------------
        extrn   print:proc
        include macs.inc

entry:
        mov     ax,@data                ;set data segment
        mov     ds,ax                   ;setup DS register
start:
        display title1                  ;display title info
        display colors                  ;display colors with codes

        mov     ah,76                   ;DOS function
        mov     al,0                    ;error code
        int     21h                     ;goto DOS
        end
