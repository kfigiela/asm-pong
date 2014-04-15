segment code
..start:  
	mov ax,data
	mov ds,ax
	mov ax,stack
	mov ss,ax
	mov sp,stacktop
          
	; tryb graficzny
	mov ax, 13h
	int 10h      
 
	reset_game:     
	push es
    mov   ax,0x0040
    mov   es,ax
    mov   si,0x006c
    mov   ax,[es:si]
	pop es


	mov word [vx], -1
	
	test ax, 1
	jnz p1_starts
	neg word [vx]
	p1_starts:      
	            
	; and ax, 7	
	; sub ax, 3
	; mov word [vy], ax
	
	       
	
	mov byte [p1score], 0
	mov byte [p2score], 0
	
	mov word [py], 100
	mov word [px], 160
;	mov word [vx], 2
    mov word [vy], -1  
	mov word [y1], 75
	mov word [y2], 75
	mov word [h], 50
	mov word [ymax], 198 - 50
	mov word [status], 0	 
	call rysuj

	mov dh, 4 ; col
	mov bx, press_key_start_msg1
	call display_msg
	
 	mov dh, 8 ; col
	mov bx, press_key_start_msg2
	call display_msg        
	
	mov dh, 9 ; col
	mov bx, press_key_start_msg3
	call display_msg
	
	mov dh, 10 ; col
	mov bx, press_key_start_msg4
	call display_msg
	
	mov dh, 11 ; col
	mov bx, press_key_start_msg5
	call display_msg
	                     
	mov dh, 20 ; col
	mov bx, press_key_start_msg6
	call display_msg  
	
	mov ah, 02h   ; set cursor pos
	mov bh, 0
	mov dl, 0 ; col
	mov dh, 0
	int 10h	

	
	mov dx, empty_line	
	mov ah, 09h;
	int 21h
  
	
             
   
start_key:	
	xor ah, ah
	int 16h
	      
	cmp al, '1'
	je mode_singleplayer
	
	cmp al, '2'
	je mode_multiplayer       
	
	cmp al, '3'
	je mode_demo       
	
	cmp al, 'q'
	je koniec    
	
	jmp start_key
	
	

	mode_singleplayer:
	mov byte [ai], 1  
	
	mov ah, 02h   ; set cursor pos
	mov bh, 0        
	mov dh, 0
	mov dl, 10 ; col
	int 10h	
	
	mov dx, playerS
	mov ah, 09h;
	int 21h      
	
	jmp main_loop	
	
	mode_multiplayer:
	mov byte [ai], 0 
	                   
	mov ah, 02h   ; set cursor pos
	mov bh, 0        
	mov dh, 0
	mov dl, 10 ; col
	int 10h	
	
	mov dx, playerM
	mov ah, 09h;
	int 21h      
	
	jmp main_loop    
	
	mode_demo:
	mov byte [ai], 3
	
	mov ah, 02h   ; set cursor pos
	mov bh, 0        
	mov dh, 0
	mov dl, 10 ; col
	int 10h	
	
	mov dx, playerD
	mov ah, 09h;
	int 21h      

	
	jmp main_loop    
	
	

	
     
    main_loop:     
		; sleep cx:dx us
		mov cx,00h
		mov dx,04ffh
		mov ah,86h
		int 15h            
		 		 

		mov   ah,01H
		int   16H    
		jz noKey
		mov   ah,00H
		int   16H    
		           
		cmp al, 'q'
		je end_of_game
		noKey:
		
		call klatka
		call rysuj
		
		; czy gra zakonczona
		cmp word [status], 0
		je main_loop  
		
	;przesun pileczke - widac ze nie trafiona lepiej
    end_of_game:

	mov ax, [vy]
	add word [py], ax   
   	mov ax, [vx]
	add word [px], ax	
	
	call rysuj
		

    cmp word [status], 1

	; je p1_won_anim
	; p2_won_anim:
	; 	mov cx,00h
	; 	mov dx,01h
	; 	mov ah,86h
	; 	int 15h			; pauza o długości CX:DX mikrosekund           
	; 	inc word [y1]
	; 	call rysuj       
	; 	mov bx, [ymax]
	; 	cmp word [y1], bx
	; 	jl p2_won_anim
	; jmp end_anim      
	; 
	; p1_won_anim:
	; 	mov cx,00h
	; 	mov dx,01h
	; 	mov ah,86h
	; 	int 15h			; pauza o długości CX:DX mikrosekund           
	; 	inc word [y2]
	; 	call rysuj           
	; 	                   
	; 	mov bx, word [ymax]
	; 	cmp word [y2], bx
	; 	jl p1_won_anim
	; end_anim:     
	        
	mov ah, 02h   ; set cursor pos
	mov bh, 0
	mov dh, 12 ; row
	mov dl, 2 ; col
	int 10h
	
    cmp word [status], 1
	je p1_won
    mov dx, p2_won_msg    
	jmp display_winner
	p1_won:
	mov dx, p1_won_msg
	display_winner:
		
	mov ah, 09h;
	int 21h    
	;                   
	; mov ah, 02h   ; set cursor pos
	; mov bh, 0
	; mov dh, 16 ; row
	; mov dl, 2 ; col
	; int 10h
	; 
	; mov dx, press_key_end_msg
	; mov ah, 09h;
	; int 21h    
	; 
         

	clearBuffer:
		mov   ah,01H
		int   16H
		jz   kbdBuffClear       ;jmp if key is ready
		mov ah, 00H        ;key is ready, get it
		int 16H            ;now process the key   
		jmp   clearBuffer   ;loop back and check for a key

	kbdBuffClear:
	
	mov ah, 0
	int 16h    
	
	cmp al, 'q'
	jne reset_game      
	
	; mov ah, 1
	; xor ah,ah
	; int 16h
	
koniec:      

	
	mov ax, 3
	int 10h ; powrót do trybu tekstowego


	mov ax, 4c00h
	int 21h	


	
klatka:
	push ecx  

; AI 2
	test byte [ai], 1
	jz no_ai2
	
	cmp word [vx], 0
	jl no_ai2

	mov ax, [py]
	mov bx, [y2]
	sub ax, 25
	sub ax, bx 
	mov bl, 10
	mov dx, ax
	sar dx, 5
	sar ax, 3
	sub ax, dx
	add word [y2], ax               

	no_ai2:
	
; AI 1
	test byte [ai], 2
	jz no_ai1

	cmp word [vx], 0
	jg no_ai1
	mov ax, [py]
	mov bx, [y1]
	sub ax, 25
	sub ax, bx 
	mov bl, 10
	mov dx, ax
	sar dx, 5
	sar ax, 3
	sub ax, dx
	add word [y1], ax	
	
	; mov ax, [py]     
	; sub ax, 10
	; mov word [y1], ax   
	 
	no_ai1:


	 ; mov [y2], ax   
	 ; mov [y1], ax   
		

; obsługa klawiatury
   	mov ah, 2
   	int 16h     

	test byte [ai], 2
	jnz ai2_running

   	         
   	cmp al, 4
   	jz p1_up_not 
   		sub word [y1], 2
   	p1_up_not:
   	              
   	cmp al, 2
   	jz p1_down_not
   		add word [y1], 2
   	p1_down_not:
                      
	ai1_running: 

	test byte [ai], 1
	jnz ai2_running
   	
   	test al, 1
   	jz p2_up_not    
   		sub word [y2], 2
   	p2_up_not:
   	      
   	test al, 8
   	jz p2_down_not      
   		add word [y2], 2
   	p2_down_not:

	ai2_running:

	cmp word [y1], 12
	jge zakres_y1_0_ok
	mov word [y1], 12
	zakres_y1_0_ok:
	
	cmp word [y2], 12
	jge zakres_y2_0_ok
	mov word [y2], 12
	zakres_y2_0_ok:
                         
	mov bx, [ymax]
	cmp word [y1], bx
	jle zakres_y1_1_ok
	mov word [y1], bx
	zakres_y1_1_ok:
	
	cmp word [y2], bx
	jle zakres_y2_1_ok
	mov word [y2], bx
	zakres_y2_1_ok:
	

	
  
      

	;przesun pileczke
	mov ax, [vy]
	add word [py], ax   
   	mov ax, [vx]
	add word [px], ax	
                      

; odbicie od dolu
    cmp word [py], word 199-3
  	   jl not_btm_hit
		neg word [vy]
		mov ax, [py]
		sub ax, 199-3
		mov bx, 199-3
		sub bx, ax
		mov [py], bx
	not_btm_hit:

; odbicie od gory	                
	cmp word [py], word 13
	jg not_top_hit
	neg word [vy]
	mov ax, [py]
	sub ax, 13
	mov bx, 13
	add bx, ax
	mov [py], bx
	not_top_hit:	   
	             
; odbicie od lewej	
	cmp word [px], word 8
	jg not_left_hit 
		mov ax, [y1]
		cmp [py], ax
		jl left_fail
	
		add ax, [h]
		cmp [py], ax
		jg left_fail 
	
		left_success:
		neg word [vx]
		     
		mov ax, [y1]
		call zmiana_kata
		
		mov ax, [px]
		sub ax, 8
		mov bx, 8
		add bx, ax
		mov [px], bx
		
		call hit_beep
		inc byte [p1score]	   
		
		mov ah, 02h   ; set cursor pos
		mov bh, 0
		mov dh, 0	
		mov dl, 0 ; col
		int 10h   

		xor ah, ah
		mov al, [p1score]

		call punkty		   
		   
		jmp not_left_hit
		left_fail:
			mov word [status], 2
	not_left_hit:	

;odbicie od prawej
	cmp word [px], word 319 - 6 - 3
	jl not_right_hit  
		; 
		mov ax, [y2]
		cmp [py], ax
		jl right_fail
		
		add ax, [h]
		cmp [py], ax
		jg right_fail 
		
		right_success:
		neg word [vx]  
		
		mov ax, [y2]
		call zmiana_kata
		
		mov ax, [px]
		sub ax, 319 - 6 - 3
		mov bx, 319 - 6 - 3
		sub bx, ax
		mov [px], bx 
		           
		call hit_beep
		inc byte [p2score]  
		
	   	mov ah, 02h   ; set cursor pos
		mov bh, 0
		mov dh, 0	
		mov dl, 37 ; col
		int 10h   
                  
		xor ah, ah
		mov al, [p2score]
		call punkty		   
		   
	 
		
		jmp not_right_hit  
		
		right_fail:
			mov word [status], 1       
	not_right_hit:  
    
	pop ecx
	ret

rysuj:
    push ecx        
;double buffering	
	mov ax, 09000H
	mov es, ax

; clear_screen
	cld
    mov di,0              ; di = 0 - zacznij od pierwszego pixela (0,0)
    mov al,0	        	; do al kolor
    mov ah,0    		    ; do ah kolor
    mov cx,16000          ; zapisujemy po dwa bajty (64000 / 2) bo tak jest szybciej
    rep stosd  

  	mov cx, 320
	borders_h:       
		mov bx, cx
		mov byte [es:bx-1+320*10], 0x0f
		mov byte [es:bx+320*199-1], 0x0f
		loop borders_h
             
	mov cx, 190
	mov bx, 320*10
	borders_v:       
		mov byte [es:bx+319], 0x0f
		mov byte [es:bx], 0x0f
		add bx, 320
		loop borders_v    

	mov cx, [h]

	mov ax, [y1]
	mov bx, 320
	mul bx     
	mov si, ax	
    
	mov ax, [y2]
	mov bx, 320
	mul bx	   

	mov di, ax
	add di, 319	
	rakieta_1:
		mov [es:si+5], byte 0x0a
		mov [es:di-5], byte 0x0c
		mov [es:si+6], byte 0x0a
		mov [es:di-6], byte 0x0c
		add si, 320
		add di, 320
		loop rakieta_1    
           

	mov ax, [py]
	mov bx, 320
	mul bx     
	add ax, [px]
	mov si, ax 

	mov [es:si-1], dword 0xad0bad00
	mov [es:si+320-1], dword 0x0b0b0b00
	mov [es:si+320*2-1], dword 0xad0bad00       

	push ds   
	      
	mov ax, 09000H
	mov ds, ax

    mov ax, 0a000h         ; do ax adres segmentu graficznego
    mov es,ax             ; es = ax 

	mov si, 3200
	mov di, 3200

	cld 

    mov cx, 15200          ; zapisujemy po dwa bajty (64000 / 2) bo tak jest szybciej
    rep movsd


	pop ds    
	
	pop ecx
	ret
 
hit_beep:   
push ax
	push bx
	push dx
	push cx
	pushf   

	cli
	in al,61h ;Get the speaker status.
	push ax ;Save it.
	or al,3 ;Turn on the speaker by turning on bits 1-0.
	out 61h,al ;Send it.
	mov al,0B6h ;Some control byte.
	out 43h,al ;Init finished!

	mov dx,0012h ;Prep for divide.
	mov ax,34DEh ;DX:AX = 1234DEh = 1,182,193
	mov bx, 440
	div bx ;AX r DX = DX:AX / BX
	out 42h,al ;Send low byte first.
	xchg ah,al
	out 42h,al ;Then high byte.

	;... some code, probably delay
	mov cx,01h
	mov dx,00h
	mov ah,086h
	int 15h			; pauza o długości CX:DX mikrosekund                  


	pop ax ;Restore speaker status.
	out 61h,al ;Send it, to turn it off.

	popf 
	pop cx
	pop dx
	pop bx
	pop ax

	ret
	   
zmiana_kata:	
	mov bx, [py]
	sub bx, ax
	cmp bx, 7
	jle przyspiesz
	
	cmp bx, 43
	jge przyspiesz
	
	cmp bx, 20
	jl trafil_end
	cmp bx, 30
	jg trafil_end
	   
    jmp zwolnij

	przyspiesz:
		cmp word [vy], 0
		jl vyminus
		jmp vyplus

	zwolnij:
		cmp word [vy], 0
		jl vyplus
		jmp vyminus	

	vyplus:
	inc word [vy]
	jmp trafil_end

	vyminus:
	dec word [vy]
	jmp trafil_end
	
	trafil_end:   
	
	cmp word [vy], 4
	jl zm_low	
	mov word [vy], 4
	
	zm_low:
	cmp word [vy], -4
	jg 	zmiana_kata_end
	mov word [vy], -4
	
	
	zmiana_kata_end:
	ret 
	
display_msg: ; dh - linia, bx - wskaznik na komunikat
	push bx
	mov ah, 02h   ; set cursor pos
	mov bh, 0
	mov dl, 2 ; col
	int 10h	
	pop bx 	
	
	mov dx, bx
	mov ah, 09h;
	int 21h
	ret
	       
	punkty:   
		mov bl, 100
		div bl    
		push ax
		add al, '0'
		mov dl, al
		mov ah,02h		; numer funkcji: 0Eh
		int 21h			; wypisz znak
		pop ax       
		               
		
		mov bl, 10
		mov al, ah
		mov ah, 0
		div bl
		push ax    

		add al, '0'
		mov dl, al
		mov ah,02h		; numer funkcji: 0Eh
		int 21h			; wypisz znak
		pop ax
		           
		mov dl, ah
		add dl, '0'
		mov ah,02h		; numer funkcji: 0Eh
		int 21h			; wypisz znak



		ret			; return, powrót	
	
align 8
segment data 
	p1_won_msg db 		"      Player 1 is the winner!    $"
	p2_won_msg db 		"      Player 2 is the winner!    $"
	empty_line db 		"                                       $"
	playerS db "Player 1 vs Computer$"
	playerM db "Player 1 vs Player 2$"
	playerD db "Computer vs Computer$"
	press_key_start_msg1 db 	"               AGH PONG            $"
	press_key_start_msg2 db 	"          1: Singleplayer          $"
	press_key_start_msg3 db 	"          2: Multiplayer           $"
	press_key_start_msg4 db 	"          3: Demo                  $"
	press_key_start_msg5 db 	"          Q: Quit                  $"
	press_key_start_msg6 db 	"      (c) 2009  Kamil Figiela      $"
	press_key_end_msg db 		"      Hit any key to continue      $"
	demo_msg db			 		"               DEMO                $"
	status dw 0  
	ai db 0 	

	h dw 30

	y1 dw 50   		
	y2 dw 100
	ymax dw 198-30
	
	p1score db 0
	p2score db 0
	last_clock dw 0
  

	
	px dw 160
	
	py dw 100
	
	vx dw 1
	vy dw 2 
 
	
	

segment stack stack
	  resb 256
stacktop:        





