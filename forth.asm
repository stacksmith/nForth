
; fasm demonstration of writing simple ELF executable

include "macros.asm"
MEMSIZE = $100000

format ELF executable 3
entry start

segment readable executable writeable
FINALHEAD = a
start:

 	;-----------------------------------------------------------------------
 	; First brk gets the top of allocated memory; second attempts to allot
 	; more memory.
 	;
 	mov	dword[MEM.START+4],start	; memory block start
 	mov	dword[HERE+4],TOP	; memory top
 	mov	eax,45                 		; brk
 	mov	ebx,0				; 0 is an illegal memory top
 	int	0x80			; eax = top of allocated memory
 	lea	ebx,[eax+MEMSIZE]	; ask for this much
 	mov	eax,45			;brk
 	int	0x80
	mov	[MEM.END+4],eax		;calculate end

	mov	esp,eax			;put stack there
	lea	ebp,[esp-1024]		;datastack
	lea	ebx,[esp-1024-4096]	;tib
	mov	[TIB+4],ebx
	mov	[PARSE.PTR+4],ebx	;current parse ptr
	mov	byte[ebx],0		;will force reload
	
	call	_ws
	call	_parse	      
	;; 
				;	call	_FNV1a
	;;  	mov 	ebx,16		;
	;;  	call	_number
	;; 	call	_search
;	mov	ebx,[LAST+4]	
;	mov	ebx,[ebx-4] ;hash	
	
	mov	esi,test1+4	;start
	NEXT
hexasc db "0123456789ABCDEF"

;; in parsing mode: esi=src

_is_ws: cmp	al,' '			;space
	je	.x
	cmp	al,9			;tab
	je	.x
	cmp	al,$A			;LF
	je	.x
	cmp	al,$D			;CR
.x:	ret	

_ws:	push	esi
	mov	esi,[PARSE.PTR+4]
	jmp	.loop
.reload:
	push	ebx
	xor	ebx,ebx			;stdin
	mov	ecx,[TIB+4]		;address
	mov	esi,ecx
	mov	[PARSE.PTR+4],ecx
	mov	edx,1024		;tib size...
	mov	eax,3			;read
	int	$80
	mov	byte[ecx+eax],0		;null-term
	mov 	ebx,eax
	pop	ebx
 
.loop:	lodsb      			;al = char
	test	al,al			;EOL? 
	jz	.reload
.ws:	call	_is_ws
	je	.loop	
	dec	esi			;point at the first non-ws cjar
	mov	[PARSE.PTR+4],esi
	pop	esi
	ret
	
	
; eax=scan,mulresult
; 
; ecx=temp,cnt
; edx
; esi=string
; edi=accum
; ebp=dsp
; [ebp+4]=base
; [ebp]=accum

;; On entry: ebx = base
_number:
	push	esi
	mov	esi,[PARSE.PTR+4]
	xchg	ebp,esp
	push	ebx
	xchg	ebp,esp
	mov	edi,0			;clear accum
	jmp	.in

;; process an ASCII digit in al, in a given base; result in ebx
;;
.digit:	push	edi			
	mov	edi,hexasc	
	mov	ecx,[ebp]		;base
	inc	ecx
	mov	ebx,ecx
	repne	scasb
	pop	edi
	jecxz	.err	
	sub	ebx,ecx			;result + 1!

;; multiply accumulator by base, and add digit (it's +1)
;;
	mov	eax,edi			;eax = accumulated value
	mov	edx,[ebp]		;load base into edx
	mul	edx			;multiply by base
	lea	edi,[eax+ebx-1]		;add digit+1, accum in edi
;; get an ASCII character and process, unless ws
.in:	lodsb
	call	_is_ws
	jne	.digit
	
	sub	esi,1			;restore terminating character
.x:
	mov	[PARSE.PTR+4],esi
	pop	esi
	mov	ebx,edi			;accumulated value
	ret
	
.err:	mov	ebx,-1
	ret
	

_find_hash: ;(hash--addr/0)
		
_FNV1a: ;from esi
	mov	ebx,FNV_OFFSET_BASIS
	jmp	.in
.loop:	inc	esi
	xor	eax,ebx				;eax = char xor hash
	mov	edx,FNV_PRIME
	mul	edx				;eax = (char xor hash) * FNV_PRIME
	mov	ebx,eax
.in:	movzx	eax,byte[esi]			;eax = char
	call	_is_ws
	jne	.loop
	ret

_search:
	mov	eax,[LAST+4]
	xchg	eax,ebx				;ebx = entry;  eax = hash
.loop:	cmp	[ebx-4],eax
	je	.done				;found
	mov	ebx,[ebx-8]			;link
	test	ebx,ebx
	jne	.loop
.done:	test	ebx,ebx				;0/zr = fail
	ret
	
_parse:
	push	esi
	mov	esi,[PARSE.PTR+4]
	push	esi                             ;keep around in case of search failure
	call	_FNV1a				;ebx=hash
	call	_search
	jnz	.found
	pop	esi
	mov	ebx,16
	call	_number		;
	jmp	.x
.found: pop	eax				;drop ptr to name
	mov	[PARSE.PTR+4],esi
.x:	pop	esi
	ret
	

	
strlen:
	mov	edi,ebx
	xor	ecx,ecx
	dec ecx
	xor	eax,eax
	repne scasb
	neg	ecx
	lea	ebx,[ecx-3]		;negation, starting at -1, 0-term
	ret

writeln:
	mov	edx,ebx                 ;size
	mov	ebx,1			;stdout
	mov	ecx,[TIB+4]		;buffer
	mov	eax,4			;write
	int	$80
	mov ebx,eax
	ret
	
readln:	
	xor	ebx,ebx			;stdin
	mov	ecx,[TIB+4]		;address
	mov	edx,1024		;tib size...
	mov	eax,3			;read
	int	$80
	mov	byte[ecx+eax],0		;null-term
	mov 	ebx,eax
	ret
		
docol:	push	esi		;stack instruction pointer
	lea	esi,[eax+4]
	NEXT

dovar:	xchg	esp,ebp
	push	ebx
	xchg	esp,ebp
	lea	ebx,[eax+4]
	NEXT

HEAD LAST,dovar
	dd	FINALHEAD
	
HEAD MEM.START,dovar
	dd	0	
HEAD HERE,dovar
	dd	0	
HEAD MEM.END,dovar
	dd	0	
HEAD ERR.FRAME,dovar
	dd	0	
HEAD TIB,dovar
	dd	0	
HEAD PARSE.PTR,dovar
	dd	0	

	
	
HEAD exit,$+4
	mov	eax,1
	xor	ebx,ebx
	int 	0x80


HEAD test1,docol
	dd	hexd
	dd	exit
	


	dd	PARSE.PTR
	dd	fetch
	dd	hexd
	dd	cr
	
	dd	PARSE.PTR
	dd	fetch
	dd	hexd
	dd	cr
	
	dd	exit
		

HEAD test2,docol
	dd	cr
	dd	cr
	dd	return

HEAD cr,$+4
	xchg	esp,ebp
	push	ebx
	xchg	esp,ebp
	mov	ebx,$0A
	jmp	cout+4
	
HEAD cout,$+4
	pusha
	push	ebx		;char is at [esp]
	mov	eax,4
	mov	ebx,1		;stdout
	mov	ecx,esp		;char address
	mov	edx,ebx		;length 11
	int	0x80
	pop	ebx
	popa
popret:
	xchg	ebp,esp
	pop	ebx
	xchg	ebp,esp
	NEXT

	
HEAD hexd,$+4
	mov	ecx,8
hexloop:
	rol	ebx,4
	pusha
	mov	eax,4
	and	ebx,$0000000F
	lea	ecx,[hexasc+ebx]	;address of character
	mov	ebx,1		;stdout
	mov	edx,ebx		;length 1
	int	0x80
	popa
	loop	hexloop
	jmp	popret
	
HEAD hexb,$+4
	shl	ebx,24
	mov	ecx,2
	jmp	hexloop

HEAD hexw,$+4
	shl	ebx,16
	mov	ecx,4
	jmp	hexloop



HEAD ERR.CATCH,$+4
	xchg	ebp,esp
	push	ebx
	xchg	ebp,esp
	push	esi			;save IP just after catch
	push	dword[ERR.FRAME]	;save old frame
	mov	[ERR.FRAME],esp	        ;establish new frame
	xor	ebx,ebx			;return 0
	NEXT
	
HEAD ERR.CLR,$+4
	mov	esp,[ERR.FRAME]		;restore stack to frame
	pop	dword[ERR.FRAME]	;restore previous frame
	pop	edx			;drop unused IP
	NEXT
; ERXIT, (id--) non-zero ID.
HEAD ERXIT,$+4
	mov	esp,[ERR.FRAME]		;restore stack to frame
	pop	dword[ERR.FRAME]	;restore previous frame
	pop	esi			;restore IP
	NEXT
; -------------------
; Compilation
; -------------------

; , - ( x -- ) compile x to the current definition.
;    Stores the number on the stack to the memory location currently
;    pointed to by dp.
HEADN comma,",",$+4
	mov	edi,[HERE+4]
	xchg	eax,ebx
	stosd
	mov	[HERE+4],edi
	xchg	ebp,esp
        pop 	ebx
        xchg 	ebp,esp
	NEXT
; lit - ( -- num) push the value in the cell straight after lit.

HEAD lit,$+4
	lodsd
	xchg	ebp,esp
	push	ebx
	xchg	eax,ebx
	NEXT

; rot - ( x y z -- y z x )
HEAD rot,$+4
	xchg	ebp,esp
	pop	edx
	pop	eax
	push	edx
	push	ebx
	xchg	eax,ebx
	xchg	ebp,esp
	NEXT	
; drop - ( x -- ) remove x from the stack.
HEAD drop,$+4
	xchg	ebp,esp
	pop	ebx
	xchg	ebp,esp
	NEXT
; dup - ( x -- x x ) add a copy of x to the stack
HEADN _dup,"dup",$+4
	xchg	ebp,esp
	push	ebx
	xchg	ebp,esp
	NEXT
; # swap - ( x y -- y x ) exchange x and y
HEAD swap,$+4
	xchg	ebp,esp
	pop	eax
	push	ebx
	xchg	eax,ebx
	xchg	ebp,esp
	NEXT
; -------------------
; Maths / Logic
; -------------------

; + - ( x y -- z) calculate z=x+y then return z
HEADN plus,"+",$+4
	xchg	ebp,esp
	pop	eax
	xchg	ebp,esp
	add	ebx,eax
	NEXT
; = - ( x y -- flag ) return true if x=y
HEADN equal,"=",$+4
	xchg	ebp,esp
	pop	eax		;   if = 	if !=
	sub	ebx,eax		; 0, ZR	        !0, NZ
	xchg	ebp,esp
	sub	ebx,1		;-1, C          ?, NC
	sbb	ebx,ebx		; -1 		0
	NEXT
	
; -------------------
; Peek and Poke
; -------------------

; @ - ( addr -- x ) read x from addr
HEADN fetch,"@",$+4
	mov	ebx,[ebx]
	NEXT
; ! - ( x addr -- ) store x at addr
HEADN _store,"!",$+4
	xchg	ebp,esp
	pop	dword[ebx]
	pop	ebx
	xchg	ebp,esp
	NEXT
; -------------------
; Flow Control
; -------------------
; 0branch - ( x -- ) jump if x is zero
HEAD zbranch,$+4
	lodsd			;eax = address to maybe jump to
	test	ebx,ebx		;zr?
	jnz	@f
	xchg	eax,esi		;0, continue from address in eax
@@:	xchg	ebp,esp
	pop	ebx
	xchg	ebp,esp
	NEXT

; branch - ( -- ) unconditional jump

HEADN _branch,"branch",$+4
	mov	esi,[esi]
	NEXT
; execute - ( xt -- ) call the word at xt
HEAD execute,$+4
	xchg	ebp,esp
	mov	edi,ebx
	pop	ebx
	xchg	ebp,esp
	jmp	edi

; -------------------
; Parsing
; -------------------

parse:	;esi-buf, ebx=accum

	
	
	

	lodsb
	sub	eax,'0'
	
HEADN return,";",$+4
	pop	esi
	NEXT

HEAD a,$+4
	NEXT

align 4
TOP:
