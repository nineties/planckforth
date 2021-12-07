\ planckforth -
\ Copyright (C) 2021 nineties

( === Heap Allocated String === )

private{

: make-string ( c-addr -- str )
    dup 0<> check-argument
    dup strlen 1+ allocate throw
    tuck strcpy
; export

: release-string ( str -- )
    free
; export

: concat-string ( str1 str2 -- newstr )
    dup 0<> check-argument
    over 0<> check-argument
    over strlen over strlen
    ( str1 str2 n1 n2 )
    over + 1+ allocate throw
    ( str1 str2 n1 ptr )
    3 pick over strcpy          \ copy str1 to ptr
    ( str1 str2 n1 ptr)
    tuck + 2 pick swap strcpy   \ copy str2 to ptr + n1
    swap drop swap drop
; export

}private

T{ s" AAAAA" make-string constant A -> }T
T{ s" BBBBBBB" make-string constant B -> }T
T{ s" " make-string constant C -> }T
T{ A B concat-string constant D -> }T
T{ A s" AAAAA" streq -> true }T
T{ B s" BBBBBBB" streq -> true }T
T{ C s" " streq -> true }T
T{ D s" AAAAABBBBBBB" streq -> true }T
T{ A strlen -> 5 }T
T{ B strlen -> 7 }T
T{ C strlen -> 0 }T
T{ D strlen -> 12 }T
T{ A release-string -> }T
T{ B release-string -> }T
T{ C release-string -> }T
T{ D release-string -> }T
