# planckforth - 
# Copyright (C) 2021 nineties

planck: planck.xxd
	xxd -r -c 8 $< > $@
	chmod +x $@

.PHONY: clean
clean:
	rm -f planck
