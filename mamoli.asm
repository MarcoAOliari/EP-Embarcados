segment code
..start:
		mov 		ax,data
		mov 		ds,ax
		mov 		ax,stack
		mov 		ss,ax
		mov 		sp,stacktop

; salvar modo corrente de video(vendo como est� o modo de video da maquina)
		mov  		ah,0Fh
		int  		10h
		mov  		[modo_anterior],al   

; alterar modo de video para gr�fico 640x480 16 cores
		mov     	al,12h
		mov     	ah,0
		int     	10h

inicializacao:
		call 	desenhaInterface

		mov		ax, 1h
		int		33h

loopPrincipal:
		mov 	ax, 5h
		mov		bx, 0h
		int		33h
		mov		[mouseOn], bx
		mov		[mouseXPos], cx
		mov		[mouseYPos], dx 

		; verifica ativacao do mouse
		cmp 	word[mouseOn], 1
		jne		loopPrincipal

		; botoes todos acima de 80 pixels
		cmp 	word[mouseYPos], 80 
		ja 		loopPrincipal

		; checagem de botoes
		cmp 	word[mouseXPos], 89
		jbe		Abrir

		cmp 	word[mouseXPos], 159
		jbe		Sair

		cmp 	word[mouseXPos], 300
		jbe		PassaBaixas

		cmp 	word[mouseXPos], 469
		jbe		PassaAltas
		jmp 	Gradiente

Abrir:
		call 	desenhaInterface
		mov		byte[cor], amarelo
		call	wAbrir

    mov   ax, 3d00h
    mov   dx, file
    int   21h
    mov   [handle], ax
    call  leBuffer
		jmp		loopPrincipal

Sair:
		call 	desenhaInterface
		mov		byte[cor], amarelo
		call	wSair

    mov  	ah,0   			; set video mode
    mov  	al,[modo_anterior]   	; modo anterior
    int  	10h
		mov   ax,4c00h
		int   21h

PassaBaixas:
		call 	desenhaInterface
		mov		byte[cor], amarelo
		call	wPassaBaixas

		; escolhe o id do filtro
		mov word[filtro], 1
		mov word[j], 302

		mov   ax, 3d00h
    mov   dx, file
    int   21h
    mov   [handle], ax
		call 	leLinhas

		jmp		loopPrincipal

PassaAltas:
		call 	desenhaInterface
		mov		byte[cor], amarelo
		call	wPassaAltas

		; escolhe o id do filtro
		mov word[filtro], 2
		mov word[j], 302

		mov   ax, 3d00h
    mov   dx, file
    int   21h
    mov   [handle], ax
		call 	leLinhas

		jmp		loopPrincipal

Gradiente:
		call 	desenhaInterface
		mov		byte[cor], amarelo
		call	wGradiente

		; escolhe o id do filtro
		mov word[filtro], 3
		mov word[j], 302

		mov   ax, 3d00h
    mov   dx, file
    int   21h
    mov   [handle], ax
		call 	leLinhas

		jmp		loopPrincipal

leBuffer:
    mov word[i], 0
    mov ah, 3fh
    mov bx, [handle]
    mov dx, buffer
    mov cx, 1200
    int 21h

novaLinha:
    mov bx, 0
    mov dl, byte[buffer]
    mov ax, 0
    mov cl, 10
    inc bx
    cmp dl, ' '
    je leProx
    sub dl, '0'
    mov al, dl

leProx:
    mov dl, byte[buffer + bx]
    inc bx
    cmp dl, ' '
    je  novoPixel
    sub dl, '0'
    mul cl
    add al, dl
    jmp leProx

novoPixel:
    mov cl, 16
    div cl
    mov cl, 10
    mov byte[cor], al
    mov ax, word[i]
    push ax
    mov ax, word[j]
    add ax, 98
    push ax
    call plot_xy
    mov ax, 0

    inc word[i]
    cmp word[i], 300
    je  comparaJ
    jmp leProx
  
comparaJ: ; terminou de ler uma linha
    dec word[j]
    cmp word[j], 0
    je terminaAbrir
completaBuffer:
    mov dx, [fileref]
    mov cx, [fileref + 2]
    add dx, bx  ; soma o percorrido por bx na ultima leitura
    adc cx, 0
    mov [fileref], dx
    mov [fileref + 2], cx
    mov al, 0
    mov bx, [handle]
    mov ah, 42h
    int 21h ; seek no arquivo
    mov bx, 0

    mov ax, 0
    mov word[i], 0

    jmp leBuffer

terminaAbrir:
    mov word[i], 0
    mov word[j], 300
    mov ah, 3eh
    mov bx, [handle]
    int 21h

    mov word[fileref], 0
    mov word[fileref + 2], 0
    ret

leLinhas:
		mov word[i], 0
    mov ah, 3fh
    mov bx, [handle]
    mov dx, buffer
    mov cx, 1200
    int 21h

		mov byte[linha1], 0
		mov byte[linha1 + 301], 0
		mov byte[linha2], 0
		mov byte[linha2 + 301], 0
		mov byte[linha3], 0
		mov byte[linha3 + 301], 0

;preenchimento de linhas para os filtros
primeiroChar:
		mov bx, 0
    mov dl, byte[buffer]
    mov ax, 0
    mov cl, 10
    inc bx
    cmp dl, ' '
    je leLinha
    sub dl, '0'
    mov al, dl

leLinha:
		mov dl, byte[buffer + bx]
    inc bx
    cmp dl, ' '
    je  inteiroLido
    sub dl, '0'
    mul cl
    add al, dl
    jmp leLinha

inteiroLido:
		inc word[i]
		call preencheLinha
		cmp word[i], 300
		je proxLinha
		mov ax, 0
		jmp leLinha

encheBuffer:
		mov dx, [fileref]
    mov cx, [fileref + 2]
    add dx, bx  ; soma o percorrido por bx na ultima leitura
    adc cx, 0
    mov [fileref], dx
    mov [fileref + 2], cx
    mov al, 0
    mov bx, [handle]
    mov ah, 42h
    int 21h ; seek no arquivo
    
		mov bx, 0
    mov ax, 0

    jmp leLinhas

proxLinha:
		dec word[j]
		mov word[linhaAtual], 3
		mov word[i], 0
		cmp word[j], 0
		je fimLeitura
		cmp word[j], 301
		jne realizaTroca
		jmp encheBuffer
	
realizaTroca:
		call aplicaFiltro
		; call imprimeLinha
		call trocaLinhas
		jmp encheBuffer

trocaLinhas: 
		push bx
		push cx
		push dx

		mov cx, 300
		mov bx, 1

		loopTroca:
			mov dl, byte[linha2 + bx]
			mov byte[linha1 + bx], dl
			mov dl, byte[linha3 + bx]
			mov byte[linha2 + bx], dl
			inc bx
			loop loopTroca

		cmp word[j], 1
		je zeraLinha3

		pop dx
		pop cx
		pop bx
		ret

zeraLinha3:
		mov cx, 300
		mov bx, 1

		loopz3:
			mov byte[linha3 + bx], 0
			inc bx
			loop loopz3

		pop dx
		pop cx
		pop bx
		ret

fimLeitura:
		mov word[i], 0
    mov word[j], 300
    mov ah, 3eh
    mov bx, [handle]
    int 21h

    mov word[fileref], 0
    mov word[fileref + 2], 0
		call zeraLinhas
		mov word[linhaAtual], 2
		mov ax, 0
		mov bx, 0
		ret

zeraLinhas:
		push bx
		push cx

		mov cx, 300
		mov bx, 1

		loopZera:
			mov byte[linha1 + bx], 0
			mov byte[linha2 + bx], 0
			mov byte[linha3 + bx], 0
			inc bx
			loop loopZera

		pop cx
		pop bx
		ret

imprimeLinha:
		push ax
		push bx
		push cx
		push dx

		mov dl, 16
		mov cx, 300
		mov bx, 1
		mov ax, 0

			loopImprime:
				mov al, byte[linha2 + bx]
				div dl
				mov byte[cor], al
				mov ax, bx
				add ax, 300
				push ax
				mov ax, word[j]
				add ax, 98
				push ax
				call plot_xy
				mov ax, 0
				inc bx
				loop loopImprime

		pop dx
		pop cx
		pop bx
		pop ax
		ret



preencheLinha:
		cmp word[linhaAtual], 2
		je preencheLinha2
		jmp preencheLinha3

preencheLinha2:
		push bx
		mov bx, word[i]
		mov byte[linha2 + bx], al
		pop bx
		ret

preencheLinha3:
		push bx
		mov bx, word[i]
		mov byte[linha3 + bx], al
		pop bx
		ret

aplicaFiltro:
		cmp word[filtro], 1
		je filtro_pb
		cmp word[filtro], 2
		je filtro_pa
		jmp filtro_gr

filtro_pb:
		call pb
		ret

filtro_pa:
		call pa
		ret

filtro_gr:
		call gr
		ret

pb:
		push ax
		push bx
		push cx
		push dx

		mov dx, 0
		mov cx, 300
		mov bx, 1
		mov ax, 0

		loop_pb:
			mov al, byte[linha1 + bx - 1]
			mov dl, byte[linha1 + bx]
			add ax, dx
			mov dl, byte[linha1 + bx + 1]
			add ax, dx
			mov dl, byte[linha2 + bx - 1]
			add ax, dx
			mov dl, byte[linha2 + bx]
			add ax, dx
			mov dl, byte[linha2 + bx + 1]
			add ax, dx
			mov dl, byte[linha3 + bx - 1]
			add ax, dx
			mov dl, byte[linha3 + bx]
			add ax, dx
			mov dl, byte[linha3 + bx + 1]
			add ax, dx

			mov dl, 144 ; divisao por 9 e 16
			div dl

			call pixelFiltro

			mov dx, 0
			mov ax, 0

			inc bx
			loop loop_pb
		
		pop dx
		pop cx
		pop bx
		pop ax
		ret

pa:
		push ax
		push bx
		push cx
		push dx
		push si
		push di

		mov dx, 0
		mov cx, 300
		mov bx, 1
		mov ax, 0
		mov si, 0

		loop_pa:
			call linha1Pa
			call linha2Pa
			call linha3Pa
	
			mov dl, 16
			idiv dl

			call pixelFiltro

			inc bx
			loop loop_pa

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		ret

linha1Pa:
		mov al, byte[linha1 + bx - 1]
		mov dx, -1
		imul dx

		mov di, ax
		mov ax, 0
		mov al, byte[linha1 + bx]
		imul dx
		add ax, di

		mov di, ax
		mov ax, 0
		mov al, byte[linha1 + bx + 1]
		imul dx
		add ax, di
		ret

linha2Pa:
		mov di, ax
		mov ax, 0
		mov al, byte[linha2 + bx - 1]
		imul dx
		add ax, di

		mov dx, 9

		mov di, ax
		mov ax, 0
		mov al, byte[linha2 + bx]
		imul dx
		add ax, di

		mov dx, -1

		mov di, ax
		mov ax, 0
		mov al, byte[linha2 + bx + 1]
		imul dx
		add ax, di
		ret

linha3Pa:
		mov di, ax
		mov ax, 0
		mov al, byte[linha3 + bx - 1]
		imul dx
		add ax, di

		mov di, ax
		mov ax, 0
		mov al, byte[linha3 + bx]
		imul dx
		add ax, di

		mov di, ax
		mov ax, 0
		mov al, byte[linha3 + bx + 1]
		imul dx
		add ax, di
		ret

gr:	
		push ax
		push bx
		push cx
		push dx
		push si
		push di

		mov dx, 0
		mov cx, 300
		mov bx, 1
		mov ax, 0
		mov si, 0

		loop_gr:
			call gr_gx
			; call gr_gy

			mov dl, 16
			idiv dl

			call pixelFiltro

			inc bx
			mov word[gx], 0
			mov word[gy], 0
			loop loop_gr

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		ret

gr_gx:
		call gr_gx_l1
		ret

gr_gx_l1:
		mov al, byte[linha1 + bx - 1]
		mov dx, -1
		imul dx

		mov di, ax
		mov ax, 0
		mov dx, -2
		mov al, byte[linha1 + bx]
		imul dx
		add ax, di

		mov di, ax
		mov ax, 0
		mov dx, -1
		mov al, byte[linha1 + bx + 1]
		imul dx
		add ax, di
		ret

gr_gx_l3:
		mov di, ax
		mov ax, 0
		mov al, byte[linha3 + bx - 1]
		add ax, di

		mov di, ax
		mov ax, 0
		mov dx, 2
		mov al, byte[linha3 + bx]
		imul dx
		add ax, di

		mov di, ax
		mov ax, 0
		mov dx, 2
		mov al, byte[linha3 + bx + 1]
		imul dx
		add ax, di
		ret
		
pixelFiltro:
		mov byte[cor], al
		mov ax, bx
		add ax, 300
		push ax
		mov ax, word[j]
		add ax, 98
		push ax
		call plot_xy
		mov ax, 0
		mov dx, 0

		ret

desenhaInterface:
;desenhar retas
    ; linha horizontal 1
		mov		byte[cor],branco_intenso
		mov		ax,0
		push		ax
		mov		ax,399
		push		ax
		mov		ax,639
		push		ax
		mov		ax,399
		push		ax
		call		line

    ; linha horizontal 2
		mov		ax,0
		push		ax
		mov		ax,98
		push		ax
		mov		ax,639
		push		ax
		mov		ax,98
		push		ax
		call		line

    ; linha vertical central
		mov		ax,300
		push		ax
		mov		ax,99
		push		ax
		mov		ax,300
		push		ax
		mov		ax,479
		push		ax
		call		line

    ; abrir | sair
		mov		ax,89
		push		ax
		mov		ax,399
		push		ax
		mov		ax,89
		push		ax
		mov		ax,479
		push		ax
		call		line

    ; sair | passa-baixas
		mov		ax,159
		push		ax
		mov		ax,399
		push		ax
		mov		ax,159
		push		ax
		mov		ax,479
		push		ax
		call		line

    ; passa-altas | gradiente
		mov		ax,469
		push		ax
		mov		ax,399
		push		ax
		mov		ax,469
		push		ax
		mov		ax,479
		push		ax
		call		line

    ; quadro com nome
		mov		ax,19
		push		ax
		mov		ax,9
		push		ax
		mov		ax,619
		push		ax
		mov		ax,9
		push		ax
		call		line

		mov		ax,619
		push		ax
		mov		ax,9
		push		ax
		mov		ax,619
		push		ax
		mov		ax,89
		push		ax
		call		line

		mov		ax,619
		push		ax
		mov		ax,89
		push		ax
		mov		ax,19
		push		ax
		mov		ax,89
		push		ax
		call		line

		mov		ax,19
		push		ax
		mov		ax,89
		push		ax
		mov		ax,19
		push		ax
		mov		ax,9
		push		ax
		call		line

		call 	wAbrir
		call 	wSair
		call 	wPassaBaixas
		call 	wPassaAltas
		call 	wGradiente
		call	wNomeDisciplina
    ret

;------------------------------------------------------------
;escrever mensagens
wAbrir:
        ; Abrir
    	mov     	cx,5			;n�mero de caracteres
    	mov     	bx,0
    	mov     	dh,2			
    	mov     	dl,3			

l4:
		call	cursor
    	mov     al,[bx+abrir]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l4
		ret

wSair:
        ; Sair
    	mov     	cx,4			
    	mov     	bx,0
    	mov     	dh,2			
    	mov     	dl,14			

l5:
		call	cursor
    	mov     al,[bx+sair]
		call	caracter
    	inc     bx			
		inc		dl			
    	loop    l5
		ret

wPassaBaixas:
        ; Passa-Baixas
    	mov     	cx,12			
    	mov     	bx,0
    	mov     	dh,2			
    	mov     	dl,23			

l6:
		call	cursor
    	mov     al,[bx+passa_baixa]
		call	caracter
    	inc     bx			
		inc		dl			
    	loop    l6
		ret

wPassaAltas:
        ; Passa-Altas
    	mov     	cx,11			
    	mov     	bx,0
    	mov     	dh,2			
    	mov     	dl,43			

l7:
		call	cursor
    	mov     al,[bx+passa_altas]
		call	caracter
    	inc     bx			
		inc		dl			
    	loop    l7
		ret

wGradiente:
        ; Gradiente
    	mov     	cx,9			
    	mov     	bx,0
    	mov     	dh,2			
    	mov     	dl,65			

l8:
		call	cursor
    	mov     al,[bx+gradiente]
		call	caracter
    	inc     bx			
		inc		dl			;avanca a coluna
    	loop    l8
		ret

wNomeDisciplina:
        ; Nome
    	mov     	cx,29			
    	mov     	bx,0
    	mov     	dh,26			
    	mov     	dl,24			

l9:
		call	cursor
    	mov     al,[bx+nome]
		call	caracter
    	inc     bx			
		inc		dl			;avanca a coluna
    	loop    l9

        ; Disciplina
    	mov     	cx,30			
    	mov     	bx,0
    	mov     	dh,27			
    	mov     	dl,23			

l10:
		call	cursor
    	mov     al,[bx+embarcados]
		call	caracter
    	inc     bx			
		inc		dl			;avanca a coluna
    	loop    l10
		ret

;------------------------------------------------------------
;***************************************************************************
;
;   fun��o cursor
;
; dh = linha (0-29) e  dl=coluna  (0-79)
cursor:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
		mov     	ah,2
		mov     	bh,0
		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________
;
;   fun��o caracter escrito na posi��o do cursor
;
; al= caracter a ser escrito
; cor definida na variavel cor
caracter:
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		push		bp
    		mov     	ah,9
    		mov     	bh,0
    		mov     	cx,1
   		mov     	bl,[cor]
    		int     	10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		ret
;_____________________________________________________________________________
;
;   fun��o plot_xy
;
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
plot_xy:
		push		bp
		mov		bp,sp
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
	    mov     	ah,0ch
	    mov     	al,[cor]
	    mov     	bh,0
	    mov     	dx,479
		sub		dx,[bp+4]
	    mov     	cx,[bp+6]
	    int     	10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		4
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay:				;loop
	mov		si,di
	cmp		si,0
	jg		inf       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar
inf:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar:	
	mov		si,dx
	add		si,ax
	push    si			;coloca a abcisa x+xc na pilha
	mov		si,cx
	add		si,bx
	push    si			;coloca a ordenada y+yc na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,dx
	push    si			;coloca a abcisa xc+x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do s�timo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc+x na pilha
	call plot_xy		;toma conta do segundo octante
	mov		si,ax
	add		si,cx
	push    si			;coloca a abcisa xc+y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do oitavo octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	add		si,cx
	push    si			;coloca a ordenada yc+y na pilha
	call plot_xy		;toma conta do terceiro octante
	mov		si,ax
	sub		si,dx
	push    si			;coloca a abcisa xc-x na pilha
	mov		si,bx
	sub		si,cx
	push    si			;coloca a ordenada yc-y na pilha
	call plot_xy		;toma conta do sexto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	sub		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quinto octante
	mov		si,ax
	sub		si,cx
	push    si			;coloca a abcisa xc-y na pilha
	mov		si,bx
	add		si,dx
	push    si			;coloca a ordenada yc-x na pilha
	call plot_xy		;toma conta do quarto octante
	
	cmp		cx,dx
	jb		fim_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;    fun��o full_circle
;	 push xc; push yc; push r; call full_circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
; cor definida na variavel cor					  
full_circle:
	push 	bp
	mov	 	bp,sp
	pushf                        ;coloca os flags na pilha
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]    ; resgata xc
	mov		bx,[bp+6]    ; resgata yc
	mov		cx,[bp+4]    ; resgata r
	
	mov		si,bx
	sub		si,cx
	push    ax			;coloca xc na pilha			
	push	si			;coloca yc-r na pilha
	mov		si,bx
	add		si,cx
	push	ax		;coloca xc na pilha
	push	si		;coloca yc+r na pilha
	call line
	
		
	mov		di,cx
	sub		di,1	 ;di=r-1
	mov		dx,0  	;dx ser� a vari�vel x. cx � a variavel y
	
;aqui em cima a l�gica foi invertida, 1-r => r-1
;e as compara��es passaram a ser jl => jg, assim garante 
;valores positivos para d

stay_full:				;loop
	mov		si,di
	cmp		si,0
	jg		inf_full       ;caso d for menor que 0, seleciona pixel superior (n�o  salta)
	mov		si,dx		;o jl � importante porque trata-se de conta com sinal
	sal		si,1		;multiplica por doi (shift arithmetic left)
	add		si,3
	add		di,si     ;nesse ponto d=d+2*dx+3
	inc		dx		;incrementa dx
	jmp		plotar_full
inf_full:	
	mov		si,dx
	sub		si,cx  		;faz x - y (dx-cx), e salva em di 
	sal		si,1
	add		si,5
	add		di,si		;nesse ponto d=d+2*(dx-cx)+5
	inc		dx		;incrementa x (dx)
	dec		cx		;decrementa y (cx)
	
plotar_full:	
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	add		si,cx
	push	si		;coloca a abcisa y+xc na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call 	line
	
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	add		si,dx
	push	si		;coloca a abcisa xc+x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha			
	mov		si,bx
	sub		si,cx
	push    si		;coloca a ordenada yc-y na pilha
	mov		si,ax
	sub		si,dx
	push	si		;coloca a abcisa xc-x na pilha	
	mov		si,bx
	add		si,cx
	push    si		;coloca a ordenada yc+y na pilha	
	call	line
	
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha			
	mov		si,bx
	sub		si,dx
	push    si		;coloca a ordenada yc-x na pilha
	mov		si,ax
	sub		si,cx
	push	si		;coloca a abcisa xc-y na pilha	
	mov		si,bx
	add		si,dx
	push    si		;coloca a ordenada yc+x na pilha	
	call	line
	
	cmp		cx,dx
	jb		fim_full_circle  ;se cx (y) est� abaixo de dx (x), termina     
	jmp		stay_full		;se cx (y) est� acima de dx (x), continua no loop
	
	
fim_full_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6
;-----------------------------------------------------------------------------
;
;   fun��o line
;
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
line:
		push		bp
		mov		bp,sp
		pushf                        ;coloca os flags na pilha
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
		mov		ax,[bp+10]   ; resgata os valores das coordenadas
		mov		bx,[bp+8]    ; resgata os valores das coordenadas
		mov		cx,[bp+6]    ; resgata os valores das coordenadas
		mov		dx,[bp+4]    ; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg		ax,cx
		xchg		bx,dx
		jmp		line1
line2:		; deltax=0
		cmp		bx,dx  ;subtrai dx de bx
		jb		line3
		xchg		bx,dx        ;troca os valores de bx e dx entre eles
line3:	; dx > bx
		push		ax
		push		bx
		call 		plot_xy
		cmp		bx,dx
		jne		line31
		jmp		fim_line
line31:		inc		bx
		jmp		line3
;deltax <>0
line1:
; comparar m�dulos de deltax e deltay sabendo que cx>ax
	; cx > ax
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		ja		line32
		neg		dx
line32:		
		mov		[deltay],dx
		pop		dx

		push		ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax
line4:
		push		ax
		push		dx
		push		si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul		si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1
ar1:		sub		ax,si
		sbb		dx,0
arc1:
		idiv		word [deltax]
		add		ax,bx
		pop		si
		push		si
		push		ax
		call		plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		fim_line
		inc		si
		jmp		line4

line5:		cmp		bx,dx
		jb 		line7
		xchg		ax,cx
		xchg		bx,dx
line7:
		push		cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push		dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx



		mov		si,bx
line6:
		push		dx
		push		si
		push		ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul		si
		mov		si,[deltay]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2
ar2:		sub		ax,si
		sbb		dx,0
arc2:
		idiv		word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push		di
		push		si
		call		plot_xy
		pop		dx
		cmp		si,dx
		je		fim_line
		inc		si
		jmp		line6

fim_line:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		8
;*******************************************************************
segment data

cor		db		branco_intenso

;	I R G B COR
;	0 0 0 0 preto
;	0 0 0 1 azul
;	0 0 1 0 verde
;	0 0 1 1 cyan
;	0 1 0 0 vermelho
;	0 1 0 1 magenta
;	0 1 1 0 marrom
;	0 1 1 1 branco
;	1 0 0 0 cinza
;	1 0 0 1 azul claro
;	1 0 1 0 verde claro
;	1 0 1 1 cyan claro
;	1 1 0 0 rosa
;	1 1 0 1 magenta claro
;	1 1 1 0 amarelo
;	1 1 1 1 branco intenso

preto		equ		0
azul		equ		1
verde		equ		2
cyan		equ		3
vermelho	equ		4
magenta		equ		5
marrom		equ		6
branco		equ		7
cinza		equ		8
azul_claro	equ		9
verde_claro	equ		10
cyan_claro	equ		11
rosa		equ		12
magenta_claro	equ		13
amarelo		equ		14
branco_intenso	equ		15

modo_anterior	db		0
linha   	dw  		0
coluna  	dw  		0
deltax		dw		0
deltay		dw		0	
mens    	db  		'Funcao Grafica'
abrir    	db  		'Abrir'
sair        db          'Sair'
passa_baixa db          'Passa-Baixas'
passa_altas db          'Passa-Altas'
gradiente   db          'Gradiente'
nome        db          'Marco Antonio Milaneze Oliari'
embarcados  db          'Sistemas Embarcados I - 2022/1'
mouseOn		dw			0
mouseXPos	dw			0
mouseYPos	dw			0
file      db            'imagem.txt',0
handle  dw      0
buffer    resb    1200
linha1:		times		302	db	0
linha2:		times		302	db	0
linha3		resb		302
linhaAtual dw			2
inicio		 dw			1
i         dw      0
j         dw      300
k 				dw			0			; para armazenar uma linha nos filtros
gx				dw			0
gy				dw			0
filtro		dw			0
fileref   dw      0,0
;*************************************************************************
segment stack stack
    		resb 		512
stacktop:


