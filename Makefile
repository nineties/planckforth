# planckforth -
# Copyright (C) 2021 nineties

default: i386-linux-handwrite
	
i386-linux-handwrite: planck.xxd
	xxd -r -c 8 $< > planck
	chmod +x planck

c: others/planck.c
	gcc -Wall -O2 $< -o planck -DCOMPILER="\"`gcc --version | head -n1`\""

python: others/planck.py
	cp others/planck.py planck
	chmod +x planck

.PHONY: clean
clean:
	rm -f planck
