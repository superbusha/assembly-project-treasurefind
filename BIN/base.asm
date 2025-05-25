IDEAL
p386
MODEL small
STACK 100h
DATASEG
	
filename1   db 'menu.bmp',0
filename2	db 'bg2.bmp',0
filename3	db 'boat3.bmp',0
filename4	db 'iceberg.bmp',0
filename5   db 'darksea.bmp',0  ;dark sea
filename6	db 'iceberg2.bmp',0
filename7	db 'wave2.bmp',0
filename8	db 'dead.bmp',0
filename9	db 'bloods1.bmp',0
filename10	db 'bloods2.bmp',0
filename11	db 'bloods3.bmp',0
filename12	db 'bloodsea.bmp',0
filename13	db 'barrier.bmp',0
filename14	db 'wreck.bmp',0
filename15	DB 'wreck2.bmp',0
filename16	DB 'treasure.bmp',0
filename17	db 'victory.bmp',0
filehandle  dw ?
Header      db 54 dup (0)
Palette     db 256*4 dup (0)
ScrLine     db 320 dup (0)
ErrorMsg    db 'Error', 13, 10 ,'$'
ppx dw  ?
ppy dw ?
imageHeight dw ?
imageLength dw ?
boatx dw 150
icebergy dw 33 
icebergx dw 30
icebergx2 dw 70
icebergy2 dw 130
icebergx3 dw 200
icebergy3 dw 130
collisionx dw ?
collisiony dw 152
randomSeed dw 12345        ; initial seed
dicebergy1 dw 10
dicebergx1 dw 30
dicebergy2 dw 10
dicebergx2 dw 200
wavetimer dw 0
wavey dw 0
wavehittime dw 0
timeinthedarksea dw 0
barriertime dw 0
barriery dw -100
wrecky1 dw 0
wreckx1 dw 0
wrecky2 dw 60
wreckx2 dw 60
bloodtime dw 0
treasurey dw 0
CODESEG
proc PrintImage
push bp
mov bp, sp
nameOffset equ [bp+8]
imHeight equ [bp+4]
imLength equ [bp+6]

call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap
call CloseFile

pop bp
ret 6
endp PrintImage

proc checkwhite
    mov ax, 0A000h      ; VGA video memory segment
    mov es, ax

	
    mov cx, [collisiony]  ; Y coordinate (boat Y + offset)
    mov ax, cx
    shl cx, 6           ; cx *= 64
    shl ax, 8           ; ax *= 256
    add cx, ax          ; cx = y * 320
    add cx, [collisionx]     ; Add boat X position

    mov di, cx
    mov al, [es:di]     ; Get pixel color at boat position

    cmp al, 255         ; Check if it's white
    jne NotWhite
    mov al, 1           ; Flag = 1 if white
    ret
NotWhite:
    mov al, 0           ; Flag = 0 otherwise
    ret
endp checkwhite

proc checkyellow
    mov ax, 0A000h      ; VGA video memory segment
    mov es, ax

	
    mov cx, [collisiony]  ; Y coordinate (boat Y + offset)
    mov ax, cx
    shl cx, 6           ; cx *= 64
    shl ax, 8           ; ax *= 256
    add cx, ax          ; cx = y * 320
    add cx, [collisionx]     ; Add boat X position

    mov di, cx
    mov al, [es:di]     ; Get pixel color at boat position

    cmp al, 11        ; Check if it's white
    jne Notyellow
    mov al, 1           ; Flag = 1 if white
    ret
Notyellow:
    mov al, 0           ; Flag = 0 otherwise
    ret
endp checkyellow

proc checkwave
    mov ax, 0A000h      ; VGA video memory segment
    mov es, ax

	
    mov cx, [collisiony]  ; Y coordinate (boat Y + offset)
    mov ax, cx
    shl cx, 6           ; cx *= 64
    shl ax, 8           ; ax *= 256
    add cx, ax          ; cx = y * 320
    add cx, [collisionx]     ; Add boat X position

    mov di, cx
    mov al, [es:di]     ; Get pixel color at boat position

    cmp al, 4        ; Check if it's blue
    jne Notblue
    mov al, 1           ; Flag = 1 if white
    ret
Notblue:
    mov al, 0           ; Flag = 0 otherwise
    ret
endp checkwave


proc OpenFile
mov ah, 3Dh
xor al, al
mov dx, [bp+8]
int 21h
jc openerror
mov [filehandle], ax
ret 

openerror:
mov dx, offset ErrorMsg
mov ah, 9h
int 21h
ret 
endp OpenFile

proc ReadHeader
mov ah,3fh
mov bx, [filehandle]
mov cx,54
mov dx,offset Header
int 21h
ret 
endp ReadHeader

proc ReadPalette
mov ah,3fh
mov cx,400h
mov dx,offset Palette
int 21h
ret 
endp ReadPalette

proc CopyPal
mov si,offset Palette
mov cx,256
mov dx,3C8h
mov al,0
out dx,al
inc dx
PalLoop:
mov al,[si+2]
shr al,2
out dx,al
mov al,[si+1]
shr al,2
out dx,al
mov al,[si]
shr al,2
out dx,al
add si,4
loop PalLoop
ret 
endp CopyPal

proc CopyBitmap
mov ax, 0A000h
mov es, ax
mov cx, imHeight
add cx, [ppy]
PrintBMPLoop:
push cx
mov di, cx
shl cx, 6
shl di, 8
add di, cx
add di, [ppx]
mov ah, 3Fh
mov cx, imLength
mov dx, offset ScrLine
int 21h
cld
mov cx, imLength
mov si, offset ScrLine
CopyPixelLoop:
lodsb
cmp al, 253
je SkipPixel
stosb
jmp ContinueLoop
SkipPixel:
inc di
ContinueLoop:
loop CopyPixelLoop
pop cx
loop PrintBMPLoop
ret 
endp CopyBitmap

proc CloseFile
mov ah,3Eh
mov bx, [filehandle]
int 21h
ret 
endp CloseFile

proc OpenGraphicMode
mov ax, 13h
int 10h
ret 
endp OpenGraphicMode

proc OpenTextMode
mov ah, 0
mov al, 2
int 10h
ret 
endp OpenTextMode

proc Delay
mov cx, 16000     ; Shorter delay (previously 0FFFFh)
DelayOuter:
    mov bx, 160     ; Shorter inner loop (previously 0FFh)
DelayInner:
    dec bx
    jnz DelayInner
    loop DelayOuter
    ret
endp Delay

proc dDelay
mov cx, 8000     ; Shorter delay (previously 0FFFFh)
dDelayOuter:
    mov bx, 20     ; Shorter inner loop (previously 0FFh)
dDelayInner:
    dec bx
    jnz DelayInner
    loop DelayOuter
    ret
endp dDelay


start:
mov ax, @data
mov ds, ax
call OpenGraphicMode


PUSH offset filename1
MOV [imageHeight], 200
MOV [imageLength], 320
PUSH [imageLength]
PUSH [imageHeight]
MOV [ppx], 0
MOV [ppy], 0
CALL PrintImage

; Initial key check loop, waiting for 'q' or 'Q' to continue (can be removed if not needed)
CheckKey:
mov ah, 0
int 16h
cmp al, 113
je ExitProgram
cmp al, 81
je ExitProgram
jmp CheckKey

ExitProgram:
call OpenGraphicMode
PUSH offset filename2
MOV [imageHeight], 200
MOV [imageLength], 320
PUSH [imageLength]
PUSH [imageHeight]
MOV [ppx], 0
MOV [ppy], 0
CALL PrintImage

PUSH offset filename3
MOV [imageHeight], 44
MOV [imageLength], 24
PUSH [imageLength]
PUSH [imageHeight]
MOV [ppx], 150
MOV [ppy], 140
CALL PrintImage

walkloop:
    mov ah, 1          ; Check if key pressed
    int 16h
    jz NoKeyPressed    ; No key, skip reading

    mov ah, 0          ; Read key
    int 16h

    cmp al, 97         ; 'a'
    je moveright
    cmp al, 100        ; 'd'
    je moveleft
    cmp al, 113        ; 'q'
    je exit
    cmp al, 81         ; 'Q'
    je exit

NoKeyPressed:
    jmp printmovement

moveright:
    sub [boatx], 5
    jmp printmovement

moveleft:
    add [boatx], 5

printmovement:
    PUSH offset filename2
    MOV [imageHeight], 200
    MOV [imageLength], 320
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], 0
    MOV [ppy], 0
    CALL PrintImage

    mov ax, [boatx]
    PUSH offset filename3
    MOV [imageHeight], 44
    MOV [imageLength], 24
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], ax
    MOV [ppy], 140
    CALL PrintImage

    add [icebergy],3
    mov ax, [icebergy]
	mov dx, [icebergx]
    PUSH offset filename4
    MOV [imageHeight], 33
    MOV [imageLength], 32
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], dx
    MOV [ppy], ax
    CALL PrintImage
	
	add [icebergy2],1
    mov ax, [icebergy2]
	mov dx, [icebergx2]
    PUSH offset filename4
    MOV [imageHeight], 33
    MOV [imageLength], 32
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], dx
    MOV [ppy], ax
    CALL PrintImage
	
	add [icebergy3],6
    mov ax, [icebergy3]
	mov dx, [icebergx3]
    PUSH offset filename4
    MOV [imageHeight], 33
    MOV [imageLength], 32
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], dx
    MOV [ppy], ax
    CALL PrintImage
	
	; Collision check
	
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	
	mov ax,[boatx]
	mov [collisionx],ax
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisionx],11
	sub [collisiony],10
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	mov ax,[boatx]
	
	add [collisionx],7
	add [collisiony],6
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisionx],5
	add [collisiony],4
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisiony],15
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	sub [collisionx],23
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	sub [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite

	;reset values
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	;end Collision check
	
	cmp [icebergy], 180
	jg change1iceberg
	cmp [icebergy2], 180
	jg change2iceberg
	cmp [icebergy3],180
	jg change3iceberg
    call Delay
	
	;
	cmp [boatx],10
	jl BEFOREDARKSEA
    jmp walkloop
	
	BEFOREDARKSEA:
	mov [boatx],189
	jmp DARKSEA
	;change the x of the iceberg 
	
	; Get timer ticks
	; --- Random number generation block ---
	change1iceberg:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [icebergx],dx
	mov [icebergy],-25
	jmp walkloop
	change2iceberg:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [icebergx2],dx
	mov [icebergy2],-25
	jmp walkloop
	change3iceberg:
		mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [icebergx3],dx
	mov [icebergy3],-25
	jmp walkloop
	;end of the fucking iceberg changes for x
; Now DX contains a random number between 1 and 300


	DARKSEA:
	
	inc [timeinthedarksea]
	cmp [wavehittime],1
	jg wavemovement
	
	mov ah, 1          ; Check if key pressed
    int 16h
    jz DNoKeyPressed    ; No key, skip reading

    mov ah, 0          ; Read key
    int 16h

    cmp al, 97         ; 'a'
    je Dmoveright
    cmp al, 100        ; 'd'
    je Dmoveleft
    cmp al, 113        ; 'q'
    je exit
    cmp al, 81         ; 'Q'
    je exit

DNoKeyPressed:
    jmp Dprintmovement

Dmoveright:
    sub [boatx], 5
    jmp Dprintmovement

Dmoveleft:
    add [boatx], 5
	jmp Dprintmovement
	;-----------------enp of regualr movement dont touch
	
	
	;wave movement
	wavemovement:
	inc [wavehittime]
	cmp [wavehittime],150
	jg resetwavetimer
	
	mov ah, 1          ; Check if key pressed
    int 16h
    jz wNoKeyPressed    ; No key, skip reading

    mov ah, 0          ; Read key
    int 16h

    cmp al, 97         ; 'a'
    je wmoveright
    cmp al, 100        ; 'd'
    je wmoveleft
    cmp al, 113        ; 'q'
    je exit
    cmp al, 81         ; 'Q'
    je exit
	
	wNoKeyPressed:
    jmp Dprintmovement

wmoveright:
    sub [boatx], 2
    jmp Dprintmovement

wmoveleft:
    add [boatx], 2
	jmp Dprintmovement
	
	
	
	Dprintmovement:
	cmp [timeinthedarksea],2
	jg bloodstage3
	cmp [timeinthedarksea],400
	jg bloodstage2
	cmp [timeinthedarksea],200
	jg bloodstage1
	
	
	PUSH offset filename5
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	jmp afterbg
	
	bloodstage1:
	PUSH offset filename9
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	jmp afterbg
	
	bloodstage2:
	PUSH offset filename10
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	jmp afterbg
	
	bloodstage3:
	PUSH offset filename11
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	
	cmp[boatx],10
	jl BLOODSEA
	jmp afterbg
	
	
	afterbg:
	mov ax, [boatx]
    PUSH offset filename3
    MOV [imageHeight], 44
    MOV [imageLength], 24
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], ax
    MOV [ppy], 140
    CALL PrintImage
	
	cmp [wavetimer],70
	jl nowave
	inc [wavey]
	mov ax, [wavey]
    PUSH offset filename7
    MOV [imageHeight], 20
    MOV [imageLength], 252
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], 20
    MOV [ppy], ax
    CALL PrintImage
	nowave:
	
	
	nowave:
	add [dicebergy1],3
	mov ax,[dicebergy1]
	mov dx,[dicebergx1]
	PUSH offset filename6
    MOV [imageHeight], 48
    MOV [imageLength], 56
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], dx
    MOV [ppy], ax
    CALL PrintImage
	
	add [dicebergy2],2
	mov ax,[dicebergy2]
	mov dx,[dicebergx2]
	PUSH offset filename6
    MOV [imageHeight], 48
    MOV [imageLength], 56
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], dx
    MOV [ppy], ax
    CALL PrintImage
	
	; Collision check
	
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	
	mov ax,[boatx]
	mov [collisionx],ax
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisionx],11
	sub [collisiony],10
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	mov ax,[boatx]
	
	add [collisionx],7
	add [collisiony],6
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisionx],5
	add [collisiony],4
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisiony],15
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	add [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	sub [collisionx],23
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	sub [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	
	;reset values
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	;end Collision check
	
	
	;wave check
	mov ax,[boatx]
	mov [collisionx],ax
	call checkwave
	cmp al, 1
	je wavehit
	;end wave check
	cmp [dicebergy1], 180
	jg dchange1iceberg
	cmp [dicebergy2],170
	jg dchange2iceberg
	
	call ddelay
	inc [wavetimer]
	cmp [wavey],190
	jg resetwave
	
	cmp [boatx],310
	jg backtonormalsea
	
	jmp DARKSEA
	
	; --- Random number generation block ---
	dchange1iceberg:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [dicebergx1],dx
	mov [dicebergy1],-20
	jmp DARKSEA
	dchange2iceberg:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [dicebergx2],dx
	mov [dicebergy2],-20
	jmp DARKSEA
	
	
	;wave seaction
	wavehit:
	mov [wavehittime],2
	jmp DARKSEA
	resetwave:
	mov [wavetimer],0
	mov [wavey],0
	jmp DARKSEA
	resetwavetimer:
	mov [wavehittime],0
	jmp DARKSEA
	
	backtonormalsea:
	mov [timeinthedarksea],0
	mov [boatx],11
	jmp walkloop
	
	

	
	BLOODSEA:
	
	PUSH offset filename12
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	mov ah, 1          ; Check if key pressed
    int 16h
    jz BNoKeyPressed    ; No key, skip reading
	
    mov ah, 0          ; Read key
    int 16h
	
    cmp al, 97         ; 'a'
    je Bmoveright
    cmp al, 100        ; 'd'
    je Bmoveleft
    cmp al, 113        ; 'q'
    je exit
    cmp al, 81         ; 'Q'
    je exit

BNoKeyPressed:
    jmp Bprintmovement

Bmoveright:
    sub [boatx], 5
    jmp Bprintmovement

Bmoveleft:
    add [boatx], 5
	jmp Bprintmovement
	
	Bprintmovement:
	mov ax, [boatx]
    PUSH offset filename3
    MOV [imageHeight], 44
    MOV [imageLength], 24
    PUSH [imageLength]
    PUSH [imageHeight]
    MOV [ppx], ax
    MOV [ppy], 140
    CALL PrintImage
	
	cmp [bloodtime],200
	jl notreasure
	
	inc [treasurey]
	mov ax,[treasurey]
	PUSH offset filename16
	MOV [imageHeight], 100
	MOV [imageLength], 100
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 160
	MOV [ppy], ax
	CALL PrintImage
	
	
	notreasure:
	inc [wrecky1]
	inc [wreckx1]
	mov ax,[wrecky1]
	mov dx,[wreckx1]
	PUSH offset filename14
	MOV [imageHeight], 40
	MOV [imageLength], 60
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], dx
	MOV [ppy], ax
	CALL PrintImage
	
	inc [wrecky2]
	mov ax,[wrecky2]
	mov dx,[wreckx2]
	PUSH offset filename15
	MOV [imageHeight], 80
	MOV [imageLength], 60
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], dx
	MOV [ppy], ax
	CALL PrintImage
	
	;inc [barriertime]
	;cmp [barriertime],50
	;jl nobarrier
	;
	;inc [barriery]
	;mov ax,[barriery]
	; PUSH offset filename13
    ;MOV [imageHeight], 200
    ;MOV [imageLength], 320
    ;PUSH [imageLength]
    ;PUSH [imageHeight]
    ;MOV [ppx], 0
    ;MOV [ppy], ax
    ;CALL PrintImage
	
	nobarrier:
	
	
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	
	mov ax,[boatx]
	mov [collisionx],ax
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	add [collisionx],11
	sub [collisiony],10
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	mov ax,[boatx]
	
	add [collisionx],7
	add [collisiony],6
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	add [collisionx],5
	add [collisiony],4
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	add [collisiony],15
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	add [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	sub [collisionx],23
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory
	
	sub [collisiony],17
	call checkwhite
	cmp al, 1
	je PixelIsWhite
	call checkyellow
	cmp al,1
	je victory

	;reset values
	mov ax,[boatx]
	mov [collisionx],ax
	mov [collisiony],152
	;end Collision check
	
	cmp [wrecky1],180
	jg changewreck1
	cmp [wrecky2],180
	jg changewreck2
	
	inc [bloodtime]
	CALL dDelay
	jmp BLOODSEA
	
	changewreck1:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [wreckx1],dx
	mov [wrecky1],-20
	jmp BLOODSEA
	changewreck2:
	mov ax, [randomSeed]       ; get previous seed
	mov cx, 25173              ; multiplier
	mul cx                     ; DX:AX = AX * 25173
	add ax, 13849              ; add increment
	mov [randomSeed], ax       ; store new seed
	
	xor dx, dx
	mov cx, 300
	div cx                     ; AX / 300 → remainder in DX
	inc dx
	mov [wreckx2],dx
	mov [wrecky2],-20
	jmp BLOODSEA
	
	

	
	PixelIsWhite:
	PUSH offset filename8
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	
	mov [boatx],160
	mov [icebergy],0
	mov [icebergy2],0
	mov [icebergy3],0
	mov [dicebergy1],0
	mov [dicebergy2],0
	mov [wavetimer],0
	mov [wavey],0
	mov [timeinthedarksea],0
	CheckKey5:
	mov ah, 0
	int 16h
	cmp al, 113
	je walkloop
	cmp al, 81
	je walkloop
	ExitProgramm:
	jmp CheckKey5
	
	victory:
	PUSH offset filename17
	MOV [imageHeight], 200
	MOV [imageLength], 320
	PUSH [imageLength]
	PUSH [imageHeight]
	MOV [ppx], 0
	MOV [ppy], 0
	CALL PrintImage
	
	
exit:
mov ax, 4c00h
int 21h
END start
