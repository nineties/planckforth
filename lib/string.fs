\ planckforth -
\ Copyright (C) 2021 nineties

( === String === )

." HELLO " cr
private{

\ Heap-allocated string object
\ p: null terminated string
: make-string ( p -- str )
    dup strlen 1 + allocate throw tuck strcpy .s
; export

: release-string ( str -- ) free ; export

}private

T{ s" AAAAA" make-string constant A -> }T
T{ s" BBBBBBB" make-string constant B -> }T
T{ A s" AAAAA" streq -> true }T
T{ B s" BBBBBBB" streq -> true }T
T{ A release-string -> }T
T{ B release-string -> }T
