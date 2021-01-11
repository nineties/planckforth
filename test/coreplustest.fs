\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

decimal

testing do +loop with run-time increment, negative increment, infinite loop
\ Contributed by Reinhold Straub

variable iterations
variable increment
: gd7 ( limit start increment -- )
   increment !
   0 iterations !
   do
      1 iterations +!
      i
      iterations @  6 = if leave then
      increment @
   +loop iterations @
;

T{  4  4 -1 gd7 -> 4 1 }T
T{  1  4 -1 gd7 -> 4 3 2 1 4 }T
T{  4  1 -1 gd7 -> 1 0 -1 -2 -3 -4 6 }T
T{  4  1  0 gd7 -> 1 1 1 1 1 1 6 }T
T{  0  0  0 gd7 -> 0 0 0 0 0 0 6 }T
T{  1  4  0 gd7 -> 4 4 4 4 4 4 6 }T
T{  1  4  1 gd7 -> 4 5 6 7 8 9 6 }T
T{  4  1  1 gd7 -> 1 2 3 3 }T
T{  4  4  1 gd7 -> 4 5 6 7 8 9 6 }T
T{  2 -1 -1 gd7 -> -1 -2 -3 -4 -5 -6 6 }T
T{ -1  2 -1 gd7 -> 2 1 0 -1 4 }T
T{  2 -1  0 gd7 -> -1 -1 -1 -1 -1 -1 6 }T
T{ -1  2  0 gd7 -> 2 2 2 2 2 2 6 }T
T{ -1  2  1 gd7 -> 2 3 4 5 6 7 6 }T
T{  2 -1  1 gd7 -> -1 0 1 3 }T
T{ -20 30 -10 gd7 -> 30 20 10 0 -10 -20 6 }T
T{ -20 31 -10 gd7 -> 31 21 11 1 -9 -19 6 }T
T{ -20 29 -10 gd7 -> 29 19 9 -1 -11 5 }T

\ ------------------------------------------------------------------------------
testing do +loop with large and small increments

\ Contributed by Andrew Haley

max-uint 8 rshift 1+ constant ustep
ustep negate constant -ustep
max-int 7 rshift 1+ constant step
step negate constant -step

variable bump

T{ : gd8 bump ! do 1+ bump @ +loop ; -> }T
T{ 0 max-uint 0 ustep gd8 -> 256 }T
T{ 0 0 max-uint -ustep gd8 -> 256 }T

T{ 0 max-int min-int step gd8 -> 256 }T
T{ 0 min-int max-int -step gd8 -> 256 }T

\ Two's complement arithmetic, wraps around modulo wordsize
\ Only tested if the Forth system does wrap around, use of conditional
\ compilation deliberately avoided

max-int 1+ min-int = constant +wrap?
min-int 1- max-int = constant -wrap?
max-uint 1+ 0=       constant +uwrap?
0 1- max-uint =      constant -uwrap?

: gd9  ( n limit start step f result -- )
   >r if gd8 else 2drop 2drop r@ then -> r> }T
;

T{ 0 0 0  ustep +uwrap? 256 gd9
T{ 0 0 0 -ustep -uwrap?   1 gd9
T{ 0 min-int max-int  step +wrap? 1 gd9
T{ 0 max-int min-int -step -wrap? 1 gd9

\ ------------------------------------------------------------------------------
testing do +loop with maximum and minimum increments

: (-mi) max-int dup negate + 0= if max-int negate else -32767 then ;
(-mi) constant -max-int

T{ 0 1 0 max-int gd8  -> 1 }T
T{ 0 -max-int negate -max-int over gd8  -> 2 }T

T{ 0 max-int  0 max-int gd8  -> 1 }T
T{ 0 max-int  1 max-int gd8  -> 1 }T
T{ 0 max-int -1 max-int gd8  -> 2 }T
T{ 0 max-int dup 1- max-int gd8  -> 1 }T

T{ 0 min-int 1+   0 min-int gd8  -> 1 }T
T{ 0 min-int 1+  -1 min-int gd8  -> 1 }T
T{ 0 min-int 1+   1 min-int gd8  -> 2 }T
T{ 0 min-int 1+ dup min-int gd8  -> 1 }T

\ ------------------------------------------------------------------------------
\ testing +loop Setting i to an arbitrary value

\ The specification for +loop permits the loop index i to be set to any value
\ including a value outside the range given to the corresponding  do.

\ set-i is a helper to set i in a do ... +loop to a given value
\ n2 is the value of i in a do ... +loop
\ n3 is a test value
\ If n2=n3 then return n1-n2 else return 1
: set-i  ( n1 n2 n3 -- n1-n2 | 1 ) 
   over = if - else 2drop 1 then
;

: -set-i ( n1 n2 n3 -- n1-n2 | -1 )
   set-i dup 1 = if negate then
;

: pl1 20 1 do i 18 i 3 set-i +loop ;
T{ pl1 -> 1 2 3 18 19 }T
: pl2 20 1 do i 20 i 2 set-i +loop ;
T{ pl2 -> 1 2 }T
: pl3 20 5 do i 19 i 2 set-i dup 1 = if drop 0 i 6 set-i then +loop ;
T{ pl3 -> 5 6 0 1 2 19 }T
: pl4 20 1 do i max-int i 4 set-i +loop ;
T{ pl4 -> 1 2 3 4 }T
: pl5 -20 -1 do i -19 i -3 -set-i +loop ;
T{ pl5 -> -1 -2 -3 -19 -20 }T
: pl6 -20 -1 do i -21 i -4 -set-i +loop ;
T{ pl6 -> -1 -2 -3 -4 }T
: pl7 -20 -1 do i min-int i -5 -set-i +loop ;
T{ pl7 -> -1 -2 -3 -4 -5 }T
: pl8 -20 -5 do i -20 i -2 -set-i dup -1 = if drop 0 i -6 -set-i then +loop ;
T{ pl8 -> -5 -6 0 -1 -2 -20 }T

\ ------------------------------------------------------------------------------
testing multiple recurses in one colon definition

: ack ( M N -- U )    \ Ackermann function, from Rosetta Code
   over 0= if  nip 1+ exit  then       \ ack(0, n) = n+1
   swap 1- swap                        ( -- m-1 n )
   dup  0= if  1+  recurse exit  then  \ ack(m, 0) = ack(m-1, 1)
   1- over 1+ swap recurse recurse     \ ack(m, n) = ack(m-1, ack(m,n-1))
;

T{ 0 0 ack ->  1 }T
T{ 3 0 ack ->  5 }T
T{ 2 4 ack -> 11 }T

\ ------------------------------------------------------------------------------
testing multiple else's in an if statement
\ Discussed on comp.lang.forth and accepted as valid ANS Forth

: melse if 1 else 2 else 3 else 4 else 5 then ;
T{ 0 melse -> 2 4 }T
T{ -1 melse -> 1 3 5 }T

\ ------------------------------------------------------------------------------
testing manipulation of >in in interpreter mode

T{ 12345 depth over 9 < 32 * + 3 + >in ! -> 12345 2345 345 45 5 }T
T{ 14145 8115 ?dup 0= 33 * >in +! tuck mod 14 >in ! gcd calculation -> 15 }T

\ ------------------------------------------------------------------------------
testing immediate with constant  variable and create [ ... does> ]

T{ 123 constant iw1 immediate iw1 -> 123 }T
T{ : iw2 iw1 literal ; iw2 -> 123 }T
T{ variable iw3 immediate 234 iw3 ! iw3 @ -> 234 }T
T{ : iw4 iw3 [ @ ] literal ; iw4 -> 234 }T
T{ :noname [ 345 ] iw3 [ ! ] ; drop iw3 @ -> 345 }T
T{ create iw5 456 , immediate -> }T
T{ :noname iw5 [ @ iw3 ! ] ; drop iw3 @ -> 456 }T
T{ : iw6 create , immediate does> @ 1+ ; -> }T
T{ 111 iw6 iw7 iw7 -> 112 }T
T{ : iw8 iw7 literal 1+ ; iw8 -> 113 }T
T{ : iw9 create , does> @ 2 + immediate ; -> }T
\ : find-iw bl word find nip ;  ( -- 0 | 1 | -1 )
skip T{ 222 iw9 iw10 find-iw iw10 -> -1 }T   \ iw10 IS NOT IMMEDIATE
skip T{ iw10 find-iw iw10 -> 224 1 }T        \ iw10 BECOMES IMMEDIATE

\ ------------------------------------------------------------------------------
testing that immediate doesn't toggle a flag

variable it1 0 it1 !
: it2 1234 it1 ! ; immediate immediate
T{ : it3 it2 ; it1 @ -> 1234 }T

\ ------------------------------------------------------------------------------
testing parsing behaviour of s" ." and (
\ which should parse to just beyond the terminating character no space needed

T{ : gc5 s" A string"drop ; gc5 -> }T
T{ ( A comment)1234 -> 1234 }T
T{ : pb1 cr ." You should see 2345: "." 2345"( A comment) cr ; pb1 -> }T
 
\ ------------------------------------------------------------------------------
testing number prefixes # $ % and 'c' character input
\ Adapted from the Forth 200X Draft 14.5 document

variable old-base
decimal base @ old-base !
T{ #1289 -> 1289 }T
T{ #-1289 -> -1289 }T
T{ $12eF -> 4847 }T
T{ $-12eF -> -4847 }T
T{ %10010110 -> 150 }T
T{ %-10010110 -> -150 }T
T{ 'z' -> 122 }T
T{ 'Z' -> 90 }T
\ Check base is unchanged
T{ base @ old-base @ = -> <true> }T

\ rEPEAT IN hEX MODE
16 old-base ! 16 base !
T{ #1289 -> 509 }T
T{ #-1289 -> -509 }T
T{ $12eF -> 12ef }T
T{ $-12eF -> -12ef }T
T{ %10010110 -> 96 }T
T{ %-10010110 -> -96 }T
T{ 'z' -> 7A }T
T{ 'Z' -> 5A }T
\ Check BASE is unchanged
T{ base @ old-base @ = -> <true> }T   \ 2

decimal
\ Check number prefixes in compile mode
T{ : nmp  #8327 $-2cbe %011010111 ''' ; nmp -> 8327 -11454 215 39 }T

\ ------------------------------------------------------------------------------
testing definition names
\ should support {1..31} graphical characters
: !"#$%&'()*+,-./0123456789:;<=>? 1 ;
T{ !"#$%&'()*+,-./0123456789:;<=>? -> 1 }T
: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^ 2 ;
T{ @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^ -> 2 }T
: _`abcdefghijklmnopqrstuvwxyz{|} 3 ;
T{ _`abcdefghijklmnopqrstuvwxyz{|} -> 3 }T
: _`abcdefghijklmnopqrstuvwxyz{|~ 4 ;     \ Last character different
T{ _`abcdefghijklmnopqrstuvwxyz{|~ -> 4 }T
T{ _`abcdefghijklmnopqrstuvwxyz{|} -> 3 }T

\ ------------------------------------------------------------------------------
testing find with a zero length string and a non-existent word

\ create emptystring 0 c,
\ : emptystring-find-check ( C-ADDR 0 | XT 1 | XT -1 -- T|F )
\     dup if ." FIND returns a TRUE value for an empty string!" cr then
\     0= swap emptystring = = ;
skip T{ emptystring find emptystring-find-check -> <true> }T

\ create non-existent-word   \ Same as in exceptiontest.fth
\        15 c, char $ c, char $ c, char q c, char w c, char e c, char q c,
\    char w c, char e c, char q c, char w c, char e c, char r c, char t c,
\    char $ c, char $ c,
skip T{ non-existent-word find -> non-existent-word 0 }T

\ ------------------------------------------------------------------------------
testing if ... begin ... repeat (unstructured)

T{ : uns1 dup 0 > if 9 swap begin 1+ dup 3 > if exit then repeat ; -> }T
T{ -6 uns1 -> -6 }T
T{  1 uns1 -> 9 4 }T

\ ------------------------------------------------------------------------------
testing does> doesn't cause a problem with a created address

\ : make-2const does> 2@ ;
skip T{ create 2k 3 , 2k , make-2const 2k -> ' 2k >body 3 }T

\ ------------------------------------------------------------------------------
testing allot ( n -- ) where n <= 0

T{ here 5 allot -5 allot here = -> <true> }T
T{ here 0 allot here = -> <true> }T
 
\ ------------------------------------------------------------------------------

cr ." End of additional Core tests" cr
