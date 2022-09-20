
	
; fasm demonstration of writing simple ELF executable

MEMSIZE = $100000
LASTHEAD = osexit
	
include "macros.asm"

format ELF executable 3
entry start

segment readable executable writeable

hCBRACE = $D80C1648
hthanx  = $07596E46	

inmsg:	db "nForth 0.0.1, Copyright (C) 2022 StackSmith",10
.1:

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
	lea	edx,[ebp-4096]		;tib
	mov	[TIB+4],edx		
				;	mov	ebx,[ebx-4] ;hash
	DSTACK
	push	1
	push	2
	push	3
	RSTACK
	mov	ebx,4
	
	mov	esi,main+4	;start
	NEXT			

_prompt:
	DSTACK
	push	ebx
	push	_promptstr
	push	5
	RSTACK
	mov	ebx,1 		;stout
	call	_oswrite
	DSTACK
	pop	ebx
	RSTACK
	ret
_promptstr:	db 10,"OK> "
;;; buf,cnt,handle
_osread:
	mov	eax,3		;READ
	jmp	_oswrite.1

_oswrite:	
	mov	eax,4		;WRITE
.1:	push	esi
	DSTACK
	pop	edx		;count
	pop	ecx		;buffer
	RSTACK
	int	$80
	mov	ebx,eax		;result

	pop	esi
	ret

;;; mode,flags,path
_osopen:	
	mov	eax,5		;sys_open
	jmp	_oswrite.1
	

_osexit:	
	mov	eax,1
	int 	0x80

	
	;; On entry: ebx = base
_number:
	;; 	push	esi
	;; 	mov	esi,[PARSE.PTR+4]
	xchg	ebp,esp
	push	ebx
	xchg	ebp,esp
	mov	edi,0		;clear accum
	jmp	.in
;; process an ASCII digit in al, in a given base; result in ebx
;;
.digit:	 push	edi		;
	mov	edi,HEXTAB+4
	mov	ecx,[ebp]	; base
	inc	ecx
	mov	ebx,ecx		; ebx = base+1
	repne	scasb
	 pop	edi
	jecxz	.err	
	sub	ebx,ecx		; ebx = result + 1!
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
	;; 	mov	[PARSE.PTR+4],esi
	;; 	pop	esi
	add	ebp,4		;done with base
	mov	ebx,edi		;accumulated value
	ret
	
.err:	DSTACK
 	pop	ebx		;get rid of base
	RSTACK
	mov	ebx,-10
	jmp	ERXIT+4
	
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
	call	_prompt

	mov	esi,[TIB+4]	;buffer
	mov	[PARSE.PTR+4],ecx ;reset parse pointer
	DSTACK
	push	ebx
	push	esi		;buffer
	push	1024		;cnt
	RSTACK
	mov	ebx,[HANDLE.IN+4]
	call	_osread
	test	ebx,ebx
	js	.readerr
	mov	byte[esi+ebx],0		;null-term
	DSTACK
	pop	ebx
	RSTACK
.loop:	lodsb      			;al = char
	test	al,al			;EOL? 
	jz	.reload
.ws:	call	_is_ws
	je	.loop	
	dec	esi			;point at the first non-ws cjar
	mov	[PARSE.PTR+4],esi
	pop	esi
	ret
.readerr:	
;;;----------------------------------------------------------------------------
;;; _search for a hash, from LATEST to first.  Return 0 or entry.
;;; 
_search: 			;(hash--0/zr or --entry/nz)
	mov	eax,[LATEST+4]
	xchg	eax,ebx				;ebx = entry;  eax = hash
	xor	edx,edx				;edx = skipback size
.loop:	sub	ebx,edx
	cmp	[ebx-4],eax
	je	.done				;found
	movzx	edx,word[ebx-6]			;link (16-bit amount to skip back);
	test	edx,edx				;link of 0 means end
	jne	.loop
	xor	ebx,ebx
.done:	test	ebx,ebx				;0/zr = fail
	ret

	
;;; ----------------------------------------------------------------------------
;;; _parse
;;; Parse a word in TIB at PARSE.PTR, as a word, and then as a number.  
;; word:   -- word,1
;; number: -- num,0
;; otherwise, num will force an error.
_parse:
	push	esi
	mov	esi,[PARSE.PTR+4]
	push	esi                             ;keep around in case of search failure
	call	_FNV1a				;hash it
	call	_search				;try to find it (NZ=found)
	jnz	.found
	pop	esi		;restore parse position
	mov	ebx,16		;try to find as hex
	call	_number		;number
	DSTACK
	push	ebx
	RSTACK
	xor	ebx,ebx
	jmp	.x
.found: pop	eax				;drop ptr to name
	DSTACK
	push	ebx		;--entry,entry  ;anything nz will do
	RSTACK
.x:	mov	[PARSE.PTR+4],esi  		;update parse position
	pop	esi
	ret
;;; ****************************************************************************
;;; ----- DO NOT PUT HEADS ABOVE HERE
HEAD osexit,$+4
	jmp	_osexit
HEAD oswrite,$+4
	call	_oswrite
	NEXT
HEAD osread,$+4
	call	_osread
	NEXT
HEAD osopen,$+4
	call	_osopen
	NEXT
	
HEADN parsereset,"\",$+4 		;"
	mov	edx,[TIB+4]
	mov	[PARSE.PTR+4],edx ;reset parse ptr
	mov	byte[edx],0
	NEXT


HEAD emit,docol
	dd 	XDSP		;push char, load pointer
	dd	one		;length
	dd	xdup		;stdout
	dd	oswrite,drop,drop
	dd	return

;;; buf,cnt
HEAD type,docol
	dd	one		;stdout
	dd	oswrite
	dd	drop,return

HEAD ws,$+4
	call	_ws
	NEXT
	
HEAD skipword,$+4
	push	esi
	mov	esi,[PARSE.PTR+4]
@@:	lodsb
	call	_is_ws
	jne	@b
	dec	esi
	mov	[PARSE.PTR+4],esi
	pop	esi
	NEXT
	
; eax=scan,mulresult
; 
; ecx=temp,cnt
; edx
; esi=string
; edi=accum
; ebp=dsp
; [ebp+4]=base
; [ebp]=accum


	
;;; ----------------------------------------------------------------------------
;;; Hash a string at esi, advancing esi until WS.  Hash overwrites EBX.
;;; 
_FNV1a: ;from esi
	DSTACK
	push	ebx
	RSTACK
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

HEADN XHASH,"HASH",$+4
	push	esi
	mov	esi,[PARSE.PTR+4]
	call	_FNV1a
	pop	esi
	NEXT
; 

HEAD parse,$+4
	call	_parse
	NEXT

HEAD strlen,$+4 		; (str--str,len)
	DSTACK
	push	ebx
	RSTACK
 	mov	edi,ebx		;starting from string start
 	mov	ecx,-1		;infinite count
  	xor	eax,eax		;seek 0
 	repne scasb
  	neg	ecx
  	lea	ebx,[ecx-3]		;negation, starting at -1, 0-term
  	NEXT
	
;; _strlen:
;; 	mov	edi,ebx
;; 	mov	ecx,-1
;;  	xor	eax,eax
;; 	repne scasb
;;  	neg	ecx
;;  	lea	ebx,[ecx-3]		;negation, starting at -1, 0-term
;;  	ret
;;; HEAD type,$+4	       ;(c-addr,cnt--)

;; HEAD type,$+4
;; 	call	_type
;; 	NEXT
;; ;; readln:	
;; 	xor	ebx,ebx			;so
;; 	mov	ecx,[TIB+4]		;address
;; 	mov	edx,1024		;tib size...
;; 	mov	eax,3			;read
;; 	int	$80
;; 	mov	byte[ecx+eax],0		;null-term
;; 	mov 	ebx,eax
;; 	ret
	
docol:	push	esi		;stack instruction pointer
	lea	esi,[eax+4]
	NEXT

dovar:	xchg	esp,ebp
	push	ebx
	xchg	esp,ebp
	lea	ebx,[eax+4]
	NEXT
HEAD HEXTAB,dovar
	db "0123456789ABCDEF"
	
HEAD LATEST,dovar
	dd	FINALHEAD
HEAD HANDLE.IN,dovar
	dd	0
HEAD HANDLE.OUT,dovar
	dd	1	
HEAD MEM.START,dovar
	dd	0
HEAD RUNPTR,dovar
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
HEAD FIXUP.IF,dovar
	dd	0

HEAD cr,docol
	dd	lit
	dd	$A
	dd	emit
	dd	return

HEAD space,docol
	dd	lit,$20,emit,return
HEAD spaces,docol
	dd	xdup,zbranch,.done
.again:	dd	space
	dd	minus1
	dd	xdup,nzbranch,.again
.done:	dd	drop,return	;
	
HEAD hello,docol,1
	dd	_strlit
	mstring <"nForth 0.0.1, Copyright (C) 2022 StackSmith",10>
.strend:
align 4
	dd	type
	dd	return
	
HEAD main,docol
	dd	hello
.in:	dd	parsereset
	dd	ERR.CATCH
	dd	xdup,zbranch,.noerr

.err:	dd 	drop			 ;for now ignore error number
	dd	RUNPTR,fetch,HERE,xstore ;abandon
	dd	TIB,fetch,strlen,type,cr ;print the error line
	dd	PARSE.PTR,fetch,TIB,fetch,minus
 	dd	spaces,lit,'^',emit,cr ;
	dd	branch, .in

.noerr:	dd	drop
.loop:	dd	INTERPRET
 	dd	branch, .loop




;dd	TIB, fetch, PARSE.PTR, _store ;reset parse ptr
	;dd	lit,0, PARSE.PTR, fetch,_store	;store a 0 to force a reload

;; 	dd lit, 1
;; 	dd dbra, 	.one
;; 	dd dbra, 	.two
;; 	dd dbra,	.three
;; 	dd lit,$DEED,hexd,cr
;; 	dd branch, .in

	
;; .zero:   dd lit,0,hexd,cr,exit
;; .one:   dd lit,1,hexd,cr,exit
;; .two:   dd lit,2,hexd,cr,exit

;; .three:   dd lit,3,hexd,cr,exit
	
;; 	dd	ERR.CLR
;; 	dd	exit

;;; on entry, esp has 2 return addresses.
ANON _sysp, $+4
	DSTACK
	push	ebx		;1,2,3,3
	push	dword[esp+8]	;1,2,3,1,3
	push	dword[esp+8]	;1,2,3,1,2,3
	RSTACK
	NEXT

HEADN XDSP,"DSP",$+4
	DSTACK
	push	ebx		;value to stack
	RSTACK
	mov	ebx,ebp		;load pointer to value
	NEXT

HEADN XRSP,"RSP",$+4
	DSTACK
	push	ebx		;value to stack
	RSTACK
	mov	ebx,esp
	NEXT
	
HEAD sys,docol
	dd 	_strlit
	mstring <"DSP",9," RSP",9,"  DIC",9,"   SRC",$A>
	
	dd	type
	dd	XDSP,hexd,space,XRSP,hexd,space
	dd	HERE,fetch,hexd,space
	dd	PARSE.PTR,fetch,hexd,cr
	dd	_sysp
	dd	hexd,space,hexd,space,hexd,cr
	dd	return

HEAD decibranz,$+4
	dec	dword[esp]	;decrement counter
	jz	.done
.continue:
	mov	esi,[esi]	;go there 
	NEXT
.done:
	add	esp,4		;drop 0 count
	add	esi,4		;skip loop target
	NEXT
	
HEADN hexd,".",$+4
	mov	ecx,8
hexloop:
	rol	ebx,4
	pusha
	mov	eax,4
	and	ebx,$0000000F		;nybble
	lea	ecx,[4+HEXTAB+ebx] 	;address of character
	mov	ebx,1			;stdout
	mov	edx,ebx			;length 1
	int	0x80
	popa
	loop	hexloop
	jmp	_popret
	
HEAD hexb,$+4
	shl	ebx,24
	mov	ecx,2
	jmp	hexloop

HEAD hexw,$+4
	shl	ebx,16
	mov	ecx,4
	jmp	hexloop


;;; ------------------------------------------------------------------------------
;;; ERROR HANDLING

;;; ERR.CATCH (--0/zr) or (--errno/nz)
HEAD ERR.CATCH,$+4
	DSTACK
	push	ebx
	RSTACK
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
;; .number:
;; 	mov	edi,[HERE+4]
;; 	mov	eax,lit
;; 	stosd
;; 	DSTACK
;; 	pop	eax	 	;number
;; 	pop	ebx
;; 	RSTACK
;; 	stosd
;; 	mov	[HERE+4],edi
;; 	NEXT
HEAD gettype,$+4  		;entry--type
	movzx	ebx,word[ebx-8]
	NEXT
	
HEAD COMPILE.ONE,docol
	dd	parse		;don't forget to WS prior to this
	dd	zbranch,.number
.word:
	dd	xdup, gettype
	dd	zbranch,.proc
;;; Immediate word; execute now
.self:	dd	execute,return
;;; If a procedure, simply compile its token
.proc:	dd      comma,return
;;; Number? Compile <lit><number>
.number:	
	dd 	lit,lit,comma  	;compile <LIT>
	dd	comma           ;compile <number>
	dd	return


HEAD COMPILE.UNTIL,docol	;delim
	dd	branch,.in
.ne:	dd	COMPILE.ONE	 ;todo: fix redundant hashing
.in:	dd	ws
	dd	xdup,XHASH,equal ;is current hash = delim?
	dd	zbranch,.ne
	dd	drop,skipword	;delimiter skipped
	dd	return

HEADN OBRACE,"[",docol,1       	;immediate!
	dd	lit,hCBRACE,COMPILE.UNTIL,return

HEADN colon,":",docol,1
	dd	HERE,fetch, LATEST,fetch, minus, lit, 8, plus
	dd	lit, 16, shiftlt,comma
	dd	ws,XHASH,comma
	dd 	skipword
	dd	HERE,fetch,LATEST,xstore
	dd	lit,docol,comma
 	dd	ws,COMPILE.ONE	;
	dd	lit,return,comma
	dd	HERE,fetch,RUNPTR,xstore
	dd	return
;;;   HERE @ LATEST @ - 8 + 16 << ,  \ backlink
;;;   ws HASH ,                      \ hash from source
;;;   skipword                       \ do not compile name
;;;   HERE @ LATEST !                \ we are the LATEST
;;;   & docol ,                      \ procedure
;;;   ws COMPILE.ONE                 \ compile body
;;;   & ; ,                          \ compile <return>
;;;   HERE @ RUNTPR !                \ do not run definition
;;;   ;
	
;;; In order to execute
HEAD CALLSTREAM,$+4
	push	esi
	mov	esi,ebx
	DSTACK
	pop	ebx
	RSTACK
	NEXT
	
	
HEAD INTERPRET,docol
	dd	HERE,fetch,RUNPTR,xstore	; start of compile
	dd	ws,COMPILE.ONE
	dd	lit,return,comma	; terminate with return
	dd	RUNPTR,fetch,CALLSTREAM
	dd	RUNPTR,fetch,HERE,xstore ; erase
	dd	return


	

				; , - ( x -- ) compile x to the current definition.
;    Stores the number on the stack to the memory location currently
;    pointed to by dp.
HEADN comma,",",$+4
	mov	edi,[HERE+4]
	xchg	eax,ebx
	stosd
	mov	[HERE+4],edi
	jmp	_popret

; lit - ( -- num) push the value in the cell straight after lit.

HEAD lit,$+4
	lodsd
	DSTACK
	push	ebx
	RSTACK
	xchg	eax,ebx
	NEXT
;;; string ( -- str cnt)

HEADN zero,"zero",$+4
	DSTACK
	push	ebx
	RSTACK
	xor	ebx,ebx
	NEXT
HEADN one,"one",$+4
	DSTACK
	push	ebx
	RSTACK
	mov	ebx,1
	NEXT
	dd	return
; rot - ( x y z -- y z x )
HEAD rot,$+4
	DSTACK
	pop	edx
	pop	eax
	push	edx
	jmp	swap.1
; # swap - ( x y -- y x ) exchange x and y
HEAD swap,$+4
	DSTACK
	pop	eax
.1:	push	ebx
	RSTACK
	xchg	eax,ebx
	NEXT
; drop - ( x -- ) remove x from the stack.
HEAD drop,$+4
_popret:
	DSTACK
	pop	ebx
	RSTACK
	NEXT
; dup - ( x -- x x ) add a copy of x to the stack
HEADN xdup,"dup",$+4
	DSTACK
	push	ebx
	RSTACK
	NEXT

; -------------------
; Maths / Logic
; -------------------

; + - ( x y -- z) calculate z=x+y then return z
HEADN plus,"+",$+4
	DSTACK
	pop	eax
	RSTACK
	add	ebx,eax
	NEXT

; + - ( x y -- z) calculate z=x-y then return z
HEADN minus,"-",$+4
	DSTACK
	pop	eax
	RSTACK
	sub	eax,ebx
	xchg	eax,ebx
	NEXT

HEADN shiftlt,"<<",$+4
	mov	ecx,ebx
	DSTACK
	pop	ebx
	RSTACK
	shl	ebx,cl
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

HEADN minus1,"-1",$+4
	dec	ebx
	NEXT
HEADN plus1,"+1",$+4
	inc	ebx
	NEXT
	
; -------------------
; Peek and Poke
; -------------------

; @ - ( addr -- x ) read x from addr
HEADN fetch,"@",$+4
	mov	ebx,[ebx]
	NEXT
HEADN fetchc,"@c",$+4
	movzx	ebx,byte[ebx]
	NEXT
HEADN fetchw,"@w",$+4
	movzx	ebx,word[ebx]
	NEXT
; ! - ( x addr -- ) store x at addr
HEADN xstore,"!",$+4
	DSTACK
	pop	dword[ebx]
.1:	pop	ebx
	RSTACK
	NEXT
HEADN xstorec,"!c",$+4
	DSTACK
	pop	eax
	mov	[ebx],al
	jmp	xstore.1
HEADN xstorew,"!w",$+4
	DSTACK
	pop	eax
	mov	[ebx],ax
	jmp	xstore.1
; -------------------
; Flow Control
; -------------------
; 0branch - ( x -- ) jump if x is zero
HEADN zbranch,"0branch",$+4
	lodsd			;eax = address to maybe jump to
	test	ebx,ebx		;zr?
	cmovz	esi,eax
@@:	jmp	_popret

HEADN nzbranch,"nzbranch",$+4
	lodsd			;eax = address to maybe jump to
	test	ebx,ebx		;zr?
	cmovnz	esi,eax
@@:	jmp	_popret

; branch - ( -- ) unconditional jump
HEADN branch,"branch",$+4
	mov	esi,[esi]
	NEXT
; djnz - decrement TOS.  if 0, remove 0 and branch.
HEAD dbra,$+4
	dec	ebx
	lodsd			;eax is branch target
	cmovz	esi,eax
	NEXT


; execute - ( xt -- ) call the word's xt
HEAD execute,$+4
	mov	eax,ebx
	DSTACK
	pop	ebx
	RSTACK
	jmp	dword[eax]

HEADN xpush,"push",$+4
	push	ebx
	jmp	_popret

HEADN xpop,"pop",$+4
	DSTACK
	push	ebx
	RSTACK
	pop	ebx
	NEXT
	
HEAD dumpbyte,docol
	dd	xdup,fetch,hexb,space,plus1,return	
HEAD dump16,docol		;addr
	dd	xdup,hexd,space,space
	dd	lit,16,xpush	;16 times
@@:	dd	dumpbyte
	dd	decibranz,@b
	dd	cr,return
HEAD dump,docol
	dd	dump16,dump16,dump16,dump16,return

HEADN return,";",$+4
	pop	esi
	NEXT
;;; ============================================================================
;;; Strings (short) are stored as:
;;; <_strlit><bytesize><string...>..aligned
;;; 
ANON _strlit,$+4
	DSTACK
	lodsb			;load string size (byte)
	push	ebx
	push	esi		;
	RSTACK
	movzx	ebx,al		;TOS = cnt
 	lea	esi,[esi+ebx+3]	;bump IP, aligning
 	and	esi,$FFFFFFFC ;
	NEXT
;;; primitive to copy string from source into dictionary, preceded by its byte
;;; size.  Line boundary may not be crossed!
ANON _getstr,$+4
	push	esi
	mov	esi,[PARSE.PTR+4]
	mov	edi,[HERE+4]
	mov	edx,edi		;edx points at count byte
	xor	ecx,ecx
	lodsb			;skip past space following "
.loop:  inc	ecx
	stosb
	lodsb
	test	al,al
	je	.err
	cmp	al,'"'		;"
	jne	.loop
	dec	ecx             ;-1 for the byte count itself.
	mov	[edx],cl	;byte count, update
	add	edi,3
	and	edi,$FFFFFFFC	;align
	mov	[HERE+4],edi
	mov	[PARSE.PTR+4],esi ;just past "
	pop	esi
	NEXT
.err:	DSTACK
	push	ebx
	RSTACK
	mov	ebx,-11
	jmp	ERXIT+4
;;; " string"  At compile time, compile the string that follows.
;;; at runtime, (--string,size) 
HEADN stringlit,'"',docol,1 		;"
	dd	lit,_strlit,comma ; compile <_strlit>
	dd	_getstr		  ; copy string 
	dd	return
;;; ." msg" At compile time, compile string followed by <type>
;;; At runtime, print the message.
HEADN prstringlit,'."',docol,1 	;"
	dd	stringlit
	dd	lit,type,comma

;;; --------------------------------------------------------------------
;;; CONTROL STRUCTURES
;;; compile: <push>|... <timesp><loopstart>
HEADN xtimes,"times",docol,1 	;(n--   n times (...)
	dd	lit,xpush,comma		;compile <push>.  During compile,
	dd	HERE,fetch,xpush 	;save target address on RSP
	dd	ws,COMPILE.ONE		;compile expression
	dd	lit,decibranz,comma	;complile <decibranz>
	dd	xpop,comma		;compile target
	dd	return




HEADN xif,"if",docol,1
  	dd	FIXUP.IF,fetch,xpush 	;reentrant: push old IF
	dd	lit,zbranch,comma 	;compile <zbranch>
 	dd	HERE,fetch,FIXUP.IF,xstore ;this is the fixup position
	dd	zero,comma
	dd	lit,hthanx,COMPILE.UNTIL   ;compile to thanx
 	dd	HERE,fetch,FIXUP.IF,fetch,xstore ;fixup to here, past thanx
  	dd	xpop,FIXUP.IF,xstore ;restore for reentrant if
	dd	return

HEADN xelse,"else",docol,1
	dd	FIXUP.IF,fetch,zbranch,.err
	dd	lit,branch,comma ;complie <branch> over else clause
	dd	HERE,fetch,zero,comma	 ;keep next fixup position
	dd	HERE,fetch,FIXUP.IF,fetch,xstore ;fixup if's target
	dd	FIXUP.IF,xstore		   ;and save else's fixup
	dd	return
.err:	dd	lit,$99,ERXIT
	
FINALHEAD = LASTHEAD
align 4
TOP:
