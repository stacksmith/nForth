.PHONY: all clean

UNAME := $(shell uname)

all: forth

clean:
	rm -f *.lst *~

forth:	macros.asm forth.asm
	fasm forth.asm -s forth.sym
	fasmlist -a forth.sym forth.lst
	rm forth.sym
