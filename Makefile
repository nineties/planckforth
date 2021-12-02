# planckforth -
# Copyright (C) 2021 nineties

default: i386-linux-handwritten

planck: default
	
i386-linux-handwritten: planck.xxd
	xxd -r -c 8 $< > planck
	chmod +x planck

c: others/planck.c
	gcc -Wall -O2 $< -o planck -DCOMPILER="$(shell gcc --version | head -n1)"

python: others/planck.py
	cp others/planck.py planck
	chmod +x planck

.PHONY: clean test
clean:
	rm -f planck

test: planck bootstrap.fs runtests.fs
	./planck < bootstrap.fs runtests.fs
