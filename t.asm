;misc test file

        .model  small
        .stack
        .data
        .code

entry:
        mov     ax,@data
        mov     ds,ax

        mov     ah,1
        mov     cx,0607h
        int     10h

        mov     ah,0                    ;wait for keypress
        int     16h

        mov     ax,4c00h
        int     21h
        end
