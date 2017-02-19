#make_bin#


; set loading address, .bin file will be loaded to this address:
#LOAD_SEGMENT=0FFFFh#
#LOAD_OFFSET=0000h#

; set entry point:
#CS=0000h#	; same as loading segment
#IP=0000h#	; same as loading offset

; set segment registers
#DS=0000h#	; same as loading segment
#ES=0000h#	; same as loading segment

; set stack
#SS=0000h#	; same as loading segment
#SP=0FFFEh#	; set to top of loading segment

; set general registers (optional)
#AX=0000h#
#BX=0000h#
#CX=0000h#
#DX=0000h#
#SI=0000h#
#DI=0000h#
#BP=0000h#





;ROM-8K
    ;00000h-01FFFh
;RAM-4k            ;check RAM and ROM address
    ;02000h-02FFFh
    
    
    jmp     st1 
    db     4 dup(0)
    
;IVT entry for NMI
         dw     isr_stop
         dw     0000        
         
         db     1012 dup(0)
 
;data declaration
	;cycle   dl
    ;stop    ch
    ;load    dh
    ;door    cl                
        
;main progran
st1:

	
    cli
; intialize ds, es,ss to start of RAM
          
		  mov       ax,0200h
          mov       ds,ax
          mov       es,ax
          mov       ss,ax
          mov       sp,00FFEH
          mov ax,0
          mov bx,0
          mov cx,0
          mov dx,0
	  
;initialize 8255
	mov al, 10010000b
	out 06h, al 
	
	mov al, 10010000b
	out 26h, al 
	
	    ;00h1-0-load
	           ;1-resume
	           ;2-start
	           ;3-door
	           ;4-detergent
	           ;5-stop
	           ;6-cycle
	           ;7-water level
	     ;02h1-pb0-light
	             ;pb1-medium
	             ;pb2-heavy   
	     ;04h1-0-gate2
	            ;1-
	            ;2-
	            ;3-agitator
	            ;4-rt
	            ;5-buzzer for process
	            ;6-buzzer for cycle
	            ;7-buzzer for error

;initialize 8253
	mov al, 00010110b ;10h-mode3
	out 16h, al
	mov al,5       ;count load =5                                                                        
	out 10h,al
	mov al,01110100b  ;12h-mode2  
	out 16h, al
	mov al,10h         ;count load =2710h                                                                    
	out 12h,al                                                                            
	mov al,27h                                                                             
	out 12h,al 
	mov al, 10110000b ;14h-mode0	     ;what happens whengate closed and count loaded?
	out 16h, al
	mov al, 0E0h  ;2min-2EE0h
    out 14h,al
	mov al, 02Eh
	out 14h,al
	
stp:
;intial value in ports in 8255
    mov al,00h
    out 02h,al
    out 04h,al

                                                                              
;making stop,load,cycle 0
    mov ch,00h
    mov dh,00h
    mov cl,00h 
    mov dl,01h ;dl-cycle








  
ld:
    in al, 00h
	and al, 01h    ;load button at 00h1-0
	jz ldst
	;debounce
	;call debounce
	in al, 00h
	and al, 01h    ;load button at 00h1-0
	jz ldst
lc:	in al, 00h
	and al, 01h    ;assumption - user presses only one button at a time
	jnz lc
	inc dh  ;dh-load
	cmp dh, 03h
	jle l1
	mov dh, 01h
l1:	cmp dh, 1
	jnz l2
	mov al,1      ;led in 02h0 for light load
	out 02h,al
l2:	cmp dh, 2
	jnz l3
	mov al,2  ;led in 02h1 for medium load
	out 02h,al			
l3: cmp dh, 3
	jnz l0
	mov al,4            ;led in 02h2 for heavy load
	out 02h,al
l0:

ldst:in al, 00h   ;check start
	and al, 04h
	jz ld
	;debounce
	;call debounce                                                                                          ;start detecting late
	in al, 00h
	and al, 04h    ;start button at 00h3
	jz ld
ldstc:	in al, 00h
	and al, 04h
	jnz ldstc
	mov bl,1  ;debugging
	jmp strt
    
strt:   
		;check load if 0 
		cmp dh, 0  ;load-dh
		jnz s10
		;buzzer
		call buzz_err
        jmp ld
s10:	cmp dh,3 ;load-3
		jle s2
		;buzzer
		call buzz_err
		jmp ld
		;cycle-1st rinse for all cycle
		
	s2:	;check door close          ;assumption that if error occured prior to 1st cycle
			;if door open
			    in al,00h                                                                        ;debounce not there
			    and al,08h          ;door closed=0,door at pa3                                                 ;changed
			    jz s5  
			    ;buzzer
			    call buzz_err
			    jmp ldst
	s5: ;water level check
			    in al,00h                    ;water level at pa7
			    and al,80h                     
			    jnz s6 
			    ;buzzer
			    call buzz_err
			    jmp ldst
	s6:		     ;dl-cycle
				;load count in 14h
				;for load =1
				cmp dh, 1 
				jnz s3
				mov al, 064h  ;2min-2EE0h
				out 14h,al
				mov al, 0h
				out 14h,al
				jmp stcyc
				;for load=2
	s3: 		cmp dh, 2
				jnz s4
				mov al, 064h   ;3min-4650h
				out 14h,al
				mov al, 0h
				out 14h,al
				jmp stcyc
	s4: 	
				mov al, 064h   ;3min-4650h
				out 14h,al
				mov al, 0h
				out 14h,al
				
stcyc:			;set gate of counter 3==at pc0
                mov al,00000001b
                out 26h,al
				
				cmp dh, 1   ;dh-load dl-cycle
				jnz sc1
				cmp dl, 4
				jl sc3
				jmp sc4
		sc1:    cmp dh, 2
				jnz sc2
				cmp dl, 4
				jl sc3
				jmp sc4
		sc2:  	cmp dl, 6
				jge sc4
				
        sc3:    ;agitator on                                     ;agitator on pc3
				mov al, 00000111b
				out 26h, al
				jmp cyc
				
		sc4:	;rt on 
				;agitator on pc3
				mov al, 00000110b
				out 26h, al
				
cyc:		cmp dh, 1   ;dh-load dl-cycle
            jnz z1
            cmp dl, 4
            je cycdr
			jmp z3
     z1:    cmp dh, 2
            jnz z2
            cmp dl, 4
            je cycdr
			jmp z3
     z2:  	cmp dl, 6
            je cycdr
     
	 z3:	in al, 00h
			and al, 08h
			jnz dr
			
	 cycdr: in al, 00h
			and al, 40h
			jz stcyc
			
			mov al, 0FFh
			out 14h, al
			out 14h, al
			
	cycres: ;buzzer
			call buzz_cyc
			mov al,00000000b        ;re-setting gate
			out 26h,al
			;agitator off                                     ;agitator on pc3
			mov al, 00000110b
			out 26h, al
			;rt off
			mov al, 00001000b
			out 26h, al
			inc dl
		    cmp dl,5
		    jl  cont
		    cmp dl,7
		    jge pro_end
		    cmp dh,3           ;dh-load
		    jz res
pro_end:    call buzz_pro
		    jmp stp                                                ;end of cycle jmp to stp
	cont:	jmp res
			
dr:			;set gate of counter 3==0
            mov al,00000000b
            out 26h,al
			;buzzer
			call buzz_err
			;agitator off
			mov al, 00000110b
			out 26h, al
			inc cl ;cl-door
			jmp res 
			
res:        
            in al, 00h        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			and al, 02h
			jz res
	resp:	in al, 00h
			and al, 02h
			jnz resp
			;debounce
			;call debounce
		;	in al, 00h
		;    and al, 02h   ;resume button at 00h1-1 
		;	jz res
		
			cmp cl,0  ;cl-door
			jnz int_door
			             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	e:  	
    cycle2:
;wash cycle for all load
			cmp dl,2
			jnz cycle3
			
			in al, 00h
			and al, 10h							;check detergent
			jnz c2wl
			;buzzer
			call buzz_err
			jmp res
	c2wl:   in al,00h                    ;water level at pa7
			and al,80h                     ;if door closed  pa7-0
			jnz c2d2
			;buzzer
			call buzz_err
			jmp res
	c2d2:   in al, 00h       ;check door opened or not
			and al, 08h
			jz c2c
			;buzzer
			call buzz_err
			jmp res
	c2c:    
	         ;for laod 1
			cmp dh,1
			jnz r2
            mov al,64h ;3  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
	r2:     ;for laod 2
			cmp dh,2
			jnz r3
            mov al,64h ;5  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
	r3:     ;for laod 3
            mov al,64h ;5  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca

	cycle3:
;rinse cycle for all loads
			cmp dl,3
			jnz cycle4 
			in al,00h
			and al,80h
			jnz c3c				;check water level
			;buzzer
			call buzz_err
			jmp res
	c2d:    in al, 00h       ;check door opened or not
			and al, 08h
			jz c3c
			;buzzer
			call buzz_err
			jmp res
			
	c3c:    ;load 1
			cmp dh,1
			jnz r4
			mov al,64h ;2  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
	r4:    ;load 2    
			cmp dh,2
			jnz r5
            mov al,64h ;3  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
	r5:     ;load 3
            mov al,64h ;3  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
    
	cycle4:;dry for load 1,2 and wash for load 3 
			cmp dl,4
			jnz cycle5
			cmp dh,3
			jz r7
			;check water level
			in al,00h
			and al,80h  ;water_vele at pa7						
			jz c4c            ;water level should be 0
			;buzzer
			call buzz_err
			jmp res
			;load 1
	c4c:     cmp dh,1
			jnz r6
            mov al,64h ;2  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cycr
	r6:     ;load 2    
            mov al,64h ;4  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cycr
	r7:     ;load 3      
            in al, 00h
	        and al, 10h							;check detergent        ;if detergent is there pa4=1
	        jnz c4wl5
            ;buzzer
            call buzz_err
            jmp res
	c4wl5:  
			in al,00h
			and al,80h						;check water level
		    jnz c4d
            ;buzzer
            call buzz_err
            jmp res
      c4d:  in al, 00h       ;check door opened or not
			and al, 08h
			jz c4c5
			;buzzer
			call buzz_err
			jmp res
            
	c4c5:   ;load count
            mov al,64h ;5  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
    

    
	cycle5:;rinse for load 3
			cmp dl,5
			jnz cycle6
            in al, 00h              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	        and al, 08h							;check door
	        jz c5wl                            ;0 for door close
            ;buzzer
            call  buzz_err                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            jmp res
	c5wl:   in al,00h
	        and al,80h
	        jnz c5c
            ;buzzer
            call buzz_err
            jmp res
	c5c:    mov al,64h ;3  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cyca
 
	cycle6:
		;dry cycle for load 3 
            in al,00h
            and al,80h
			jz c6c					;check water level   
            ;buzzer
            call buzz_err
            jmp res
	c6c:    mov al,64h ;4  min
            out 14h,al
            mov al,00h
            out 14h,al
            jmp cycr
    cyca:  
			;agitator on
			mov al, 00000111b
			out 26h, al
			mov al,00000001b        ;setting gate
			out 26h,al
			jmp cyc
    cycr:
			;revolving tube on        ;rt on pc4
			mov al, 00001001b
			out 26h, al
			mov al,00000001b        ;setting gate
			out 26h,al
			jmp cyc
                 
int_door:
           ;set gate
           mov al,01h
           out 26h,al
           ;agitator on
		   mov al, 00000111b
		   out 26h, al
		   mov cl,0
           jmp cyc
isr_stop:   
            ;buzzer
			call debounce
			in al,00h
			and al,20h
			jz fk
			call buzz_err
			inc ch ;ch-stop
			mov al,00
			out 02h,al
			out 04h,al
	fk:	    iret

 ;debounce function    
debounce proc near
	mov bx,10
 t1:mov ax,58
 t2:dec ax
    jz t2
    dec bx
    jnz t1
    ret
debounce endp 

buzz_pro  proc near            ;differnt time for diff buzz
	mov al,00001011b
	out 26h,al
	mov bx,100
 b1:mov ax,05555h
 b2:dec ax
    jz t2
    dec bx
    jnz b1
	mov al,00001010b
	out 26h,al
	ret
buzz_pro endp 


buzz_cyc proc near
	mov al,00001101b
	out 26h,al
	mov bx,5555h
 bb1:mov ax,0FFFFh
 bb2:dec ax
    jz bb2
    dec bx
    jnz bb1
	mov al,00001100b
	out 26h,al
	ret
buzz_cyc endp
    
buzz_err  proc near
	mov al,00001111b
	out 26h,al
	mov bx,05555h
 b3:mov ax,0FFFFh
 b4:dec ax
    jz b4
    dec bx
    jnz b3
	mov al,00001110b
	out 26h,al
	ret
buzz_err endp