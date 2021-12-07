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

( === Builtin Exceptions === )
s" Index out of range" exception constant OUT-OF-RANGE export

defined? roll [unless]
    : roll ( w[n-1] ... w0 n -- w0 w[n-2] ... w0 w[n-1] )
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

( === Enum === )

\ 0
\ enum A
\ enum B
\ drop

\ 0 constant A
\ 1 constant B

: enum ( n "name" -- n )
    dup constant 1+
; export

}private
