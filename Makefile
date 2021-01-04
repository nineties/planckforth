# planckforth - 
# Copyright (C) 2021 nineties

planck: planck.xxd
	xxd -r -c 8 $< > $@
	chmod +x $@

c: others/planck.c
	gcc -Wall -O2 $< -o planck

python: others/planck.py
	cp others/planck.py planck
	chmod +x planck

.PHONY: clean
clean:
	rm -f planck
