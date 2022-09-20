
MACRO PRINT val,bits=32 {
   repeat bits/4
        d = '0' + val shr (bits-%*4) and 0Fh
        if d > '9'
            d = d + 'A'-'9'-1
        end if
        display d
    end repeat
}

MACRO PRINT64 val {
      PRINT val,64
}

FNV_PRIME        equ 16777619;   $01000193
FNV_OFFSET_BASIS equ 2166136261; $811C9DC5
macro HASH xname {
  common local .idx,.hash,.ptr,.temp,.len
 
  virtual at 0
     db xname
     .len = $
     .idx = 0                ;index to pull characters
     .hash = FNV_OFFSET_BASIS
     repeat .len
        load .temp byte from $$ + .idx
       .hash = (.hash xor .temp)
       .hash = (.hash * FNV_PRIME) and $FFFFFFFF
       .idx = .idx+1
     end repeat
  end virtual
  dd .hash
  PRINT .hash
  display $A
}


MACRO	DSTACK{
	xchg	ebp,esp
}
MACRO	RSTACK {	
	xchg	ebp,esp
}
	
MACRO NEXT {
	lodsd			;load next token
	jmp	dword[eax]
}

; -8 2 flags
; -6 2	Link field
; -4 4	Hash field (name hash)
; -0 4	Code field
; ...
IMMEDIATE equ 1	
MACRO HEAD name,type, immediate=0 {
align 4	
	dw immediate
	dw ((name - LASTHEAD) and $FFFF)
	HASH `name
name:	dd type
LASTHEAD = name
;;; 	define LASTHEAD name
}
MACRO HEADN name,hashname,type,immediate=0 {	
align 4
	dw immediate
	dw ((name - LASTHEAD) and $FFFF)
	HASH hashname
name:	dd type
LASTHEAD = name
;;; 	define LASTHEAD name
}

MACRO ANON name,type {
align 4
name:	dd type
}
	
MACRO mstring args {
LOCAL .end
	db  .end-$-1
	db args
.end:
	align 4
}
