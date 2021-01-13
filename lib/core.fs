\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

defined? roll [unless]
    : roll ( wn ... w1 n -- w1 wn ... w2 -- )
        dup 0<= if drop else swap >r 1- recurse r> swap then
    ;
[then]
