\ planckforth -
\ Copyright (C) 2021 nineties

( === Variable Length Array === )

private{

( === Allocation strategy === )
defined? array-alloc-strategy [unless]

\ Compute new capacity
: array-alloc-strategy ( u1 -- u2 )
    dup 0= if 4 else 2 * then
;

[then]

struct
    ptr% field array>buf
    int% field array>size
    int% field array>capa
end-struct array%

s" Index out of range" exception constant OUT-OF-RANGE export

\ Allocate array with capacity
: allocate-array ( n capa -- arr )
    array% %allocate throw
    ( n capa addr )
    over cells allocate throw over array>buf !
    tuck array>capa !
    tuck array>size !
;

: make-array ( n -- arr )
    dup 0>= check-argument
    \ compute capa
    dup 0= if 10 else dup then
    allocate-array
; export

: array-size ( arr -- n ) array>size @ ; export 

: check-index ( i arr -- )
    over 0< if OUT-OF-RANGE throw then
    array-size >= if OUT-OF-RANGE throw then
;

: array@ ( i arr -- w )
    2dup check-index
    array>buf @ swap cells + @
; export

: array! ( v i arr -- )
    2dup check-index
    array>buf @ swap cells + !
; export

: array-reallocate ( capa arr -- )
    over cells allocate throw
    \ copy elements to new buffer
    over array>buf @ over 3 pick array>size @ cells memcpy
    over array>buf @ free
    over array>buf !
    over over array>capa !
    2drop
;

: array-resize ( n arr -- )
    over 0>= check-argument
    2dup array>capa @ < if
        \ If n is smaller than the capacity
        \ just change array>size
        array>size !
        exit
    else
        2dup array-reallocate
        array>size !
    then
; export

: array-push ( w arr -- )
    dup array>size @ over array>capa @ >= if
        dup array>capa @ array-alloc-strategy
        ( w arr new-capa )
        over array-reallocate
    then
    swap ( arr w ) 
    over array>buf @ 2 pick array>size @ cells + !
    array>size 1 swap +!
; export

: array-pop ( arr -- w )
    dup array-size 0> unless OUT-OF-RANGE throw then
    1 over array>size -!
    dup array-size cells swap array>buf @ + @
; export


}private

T{ -1 ' make-array catch -> -1 INVALID-ARGUMENT }T

T{ 0 make-array constant A -> }T
T{ A array-size -> 0 }T
T{ 0 A ' array@ catch -> 0 A OUT-OF-RANGE }T
T{ 1 0 A ' array! catch -> 1 0 A OUT-OF-RANGE }T
T{ A ' array-pop catch -> A OUT-OF-RANGE }T

T{ :noname 100 0 do i A array-push loop ; execute -> }T

T{ A array-size -> 100 }T
T{ 0 A array@ -> 0 }T
T{ 5 A array@ -> 5 }T
T{ 10 A array@ -> 10 }T
T{ 50 A array@ -> 50 }T
T{ 99 A array@ -> 99 }T

T{ A array-pop -> 99 }T
T{ A array-size -> 99 }T
T{ A array-pop -> 98 }T
T{ A array-size -> 98 }T

T{ -1 A ' array-resize catch -> -1 A INVALID-ARGUMENT }T
T{ 5 A array-resize -> }T
T{ A array-size -> 5 }T
T{ 100 A array-resize -> }T
T{ A array-size -> 100 }T

T{ 1 -1 A ' array! catch -> 1 -1 A OUT-OF-RANGE }T
T{ 1 100 A ' array! catch -> 1 100 A OUT-OF-RANGE }T
T{ -1 A ' array@ catch -> -1 A OUT-OF-RANGE }T
T{ 100 A ' array@ catch -> 100 A OUT-OF-RANGE }T
T{ 1 0 A array! -> }T
T{ 0 A array@ -> 1 }T
T{ 2 99 A array! -> }T
T{ 99 A array@ -> 2 }T
