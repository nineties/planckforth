\ planckforth -
\ Copyright (C) 2021 nineties

\ Ignore test codes. lib/tester.fs will redefine this when
\ running tests.
: T{
    begin
        word throw
        s" }T" streq if exit then
    again
;

s" Invalid argument" exception constant INVALID-ARGUMENT

: check-argument ( f  -- )
    unless INVALID-ARGUMENT throw then
;

defined? roll [unless]
    : roll ( wn ... w1 n -- w1 wn ... w2 )
        dup 0<= if drop else swap >r 1- recurse r> swap then
    ;
[then]

private{

( === Cons Cell === )

struct
    cell% field first
    cell% field second
end-struct cons-cell%

: cons ( a b  -- cons )
    cons-cell% %allocate throw
    tuck second !
    tuck first !
; export

: car first @ ; export
: cdr second @ ; export

}private
