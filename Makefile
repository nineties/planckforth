# planckforth -
# Copyright (C) 2021 nineties

default: x86hex
	
x86hex: planck.xxd
	xxd -r -c 8 $< > planck
	chmod +x planck

c: others/planck.c
	gcc -Wall -O2 $< -o planck

python: others/planck.py
	cp others/planck.py planck
	chmod +x planck

.PHONY: clean
clean:
	rm -f planck
