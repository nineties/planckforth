\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

testing Core Extension words

decimal

testing true false

T{ true  -> 1 }T
T{ false -> 0 }T

\ -----------------------------------------------------------------------------
testing <> u>   (contributed by James Bowman)

T{ 0 0 <> -> false }T
T{ 1 1 <> -> false }T
T{ -1 -1 <> -> false }T
T{ 1 0 <> -> true }T
T{ -1 0 <> -> true }T
T{ 0 1 <> -> true }T
T{ 0 -1 <> -> true }T

T{ 0 1 u> -> false }T
T{ 1 2 u> -> false }T
T{ 0 mid-uint u> -> false }T
T{ 0 max-uint u> -> false }T
T{ mid-uint max-uint u> -> false }T
T{ 0 0 u> -> false }T
T{ 1 1 u> -> false }T
T{ 1 0 u> -> true }T
T{ 2 1 u> -> true }T
T{ mid-uint 0 u> -> true }T
T{ max-uint 0 u> -> true }T
T{ max-uint mid-uint u> -> true }T

\ -----------------------------------------------------------------------------
testing 0<> 0>   (contributed by James Bowman)

T{ 0 0<> -> false }T
T{ 1 0<> -> true }T
T{ 2 0<> -> true }T
T{ -1 0<> -> true }T
T{ max-uint 0<> -> true }T
T{ min-int 0<> -> true }T
T{ max-int 0<> -> true }T

T{ 0 0> -> false }T
T{ -1 0> -> false }T
T{ min-int 0> -> false }T
T{ 1 0> -> true }T
T{ max-int 0> -> true }T

\ -----------------------------------------------------------------------------
testing nip tuck roll pick   (contributed by James Bowman)

T{ 1 2 nip -> 2 }T
T{ 1 2 3 nip -> 1 3 }T

T{ 1 2 tuck -> 2 1 2 }T
T{ 1 2 3 tuck -> 1 3 2 3 }T

T{ : RO5 100 200 300 400 500 ; -> }T
T{ RO5 3 roll -> 100 300 400 500 200 }T
T{ RO5 2 roll -> RO5 rot }T
T{ RO5 1 roll -> RO5 swap }T
T{ RO5 0 roll -> RO5 }T

T{ RO5 2 pick -> 100 200 300 400 500 300 }T
T{ RO5 1 pick -> RO5 over }T
T{ RO5 0 pick -> RO5 dup }T

\ -----------------------------------------------------------------------------
testing 2>r 2r@ 2r>   (contributed by James Bowman)

skip T{ : RR0 2>r 100 r> r> ; -> }T
skip T{ 300 400 RR0 -> 100 400 300 }T
skip T{ 200 300 400 RR0 -> 200 100 400 300 }T

skip T{ : RR1 2>r 100 2r@ r> r> ; -> }T
skip T{ 300 400 RR1 -> 100 300 400 400 300 }T
skip T{ 200 300 400 RR1 -> 200 100 300 400 400 300 }T
skip 
skip T{ : RR2 2>r 100 2r> ; -> }T
skip T{ 300 400 RR2 -> 100 300 400 }T
skip T{ 200 300 400 RR2 -> 200 100 300 400 }T

\ -----------------------------------------------------------------------------
testing hex   (contributed by James Bowman)

T{ base @ hex base @ decimal base @ - swap base ! -> 6 }T

\ -----------------------------------------------------------------------------
testing within   (contributed by James Bowman)

T{ 0 0 0 within -> false }T
T{ 0 0 mid-uint within -> true }T
T{ 0 0 mid-uint+1 within -> true }T
T{ 0 0 max-uint within -> true }T
T{ 0 mid-uint 0 within -> false }T
T{ 0 mid-uint mid-uint within -> false }T
T{ 0 mid-uint mid-uint+1 within -> false }T
T{ 0 mid-uint max-uint within -> false }T
T{ 0 mid-uint+1 0 within -> false }T
T{ 0 mid-uint+1 mid-uint within -> true }T
T{ 0 mid-uint+1 mid-uint+1 within -> false }T
T{ 0 mid-uint+1 max-uint within -> false }T
T{ 0 max-uint 0 within -> false }T
T{ 0 max-uint mid-uint within -> true }T
T{ 0 max-uint mid-uint+1 within -> true }T
T{ 0 max-uint max-uint within -> false }T
T{ mid-uint 0 0 within -> false }T
T{ mid-uint 0 mid-uint within -> false }T
T{ mid-uint 0 mid-uint+1 within -> true }T
T{ mid-uint 0 max-uint within -> true }T
T{ mid-uint mid-uint 0 within -> true }T
T{ mid-uint mid-uint mid-uint within -> false }T
T{ mid-uint mid-uint mid-uint+1 within -> true }T
T{ mid-uint mid-uint max-uint within -> true }T
T{ mid-uint mid-uint+1 0 within -> false }T
T{ mid-uint mid-uint+1 mid-uint within -> false }T
T{ mid-uint mid-uint+1 mid-uint+1 within -> false }T
T{ mid-uint mid-uint+1 max-uint within -> false }T
T{ mid-uint max-uint 0 within -> false }T
T{ mid-uint max-uint mid-uint within -> false }T
T{ mid-uint max-uint mid-uint+1 within -> true }T
T{ mid-uint max-uint max-uint within -> false }T
T{ mid-uint+1 0 0 within -> false }T
T{ mid-uint+1 0 mid-uint within -> false }T
T{ mid-uint+1 0 mid-uint+1 within -> false }T
T{ mid-uint+1 0 max-uint within -> true }T
T{ mid-uint+1 mid-uint 0 within -> true }T
T{ mid-uint+1 mid-uint mid-uint within -> false }T
T{ mid-uint+1 mid-uint mid-uint+1 within -> false }T
T{ mid-uint+1 mid-uint max-uint within -> true }T
T{ mid-uint+1 mid-uint+1 0 within -> true }T
T{ mid-uint+1 mid-uint+1 mid-uint within -> true }T
T{ mid-uint+1 mid-uint+1 mid-uint+1 within -> false }T
T{ mid-uint+1 mid-uint+1 max-uint within -> true }T
T{ mid-uint+1 max-uint 0 within -> false }T
T{ mid-uint+1 max-uint mid-uint within -> false }T
T{ mid-uint+1 max-uint mid-uint+1 within -> false }T
T{ mid-uint+1 max-uint max-uint within -> false }T
T{ max-uint 0 0 within -> false }T
T{ max-uint 0 mid-uint within -> false }T
T{ max-uint 0 mid-uint+1 within -> false }T
T{ max-uint 0 max-uint within -> false }T
T{ max-uint mid-uint 0 within -> true }T
T{ max-uint mid-uint mid-uint within -> false }T
T{ max-uint mid-uint mid-uint+1 within -> false }T
T{ max-uint mid-uint max-uint within -> false }T
T{ max-uint mid-uint+1 0 within -> true }T
T{ max-uint mid-uint+1 mid-uint within -> true }T
T{ max-uint mid-uint+1 mid-uint+1 within -> false }T
T{ max-uint mid-uint+1 max-uint within -> false }T
T{ max-uint max-uint 0 within -> true }T
T{ max-uint max-uint mid-uint within -> true }T
T{ max-uint max-uint mid-uint+1 within -> true }T
T{ max-uint max-uint max-uint within -> false }T

T{ min-int min-int min-int within -> false }T
T{ min-int min-int 0 within -> true }T
T{ min-int min-int 1 within -> true }T
T{ min-int min-int max-int within -> true }T
T{ min-int 0 min-int within -> false }T
T{ min-int 0 0 within -> false }T
T{ min-int 0 1 within -> false }T
T{ min-int 0 max-int within -> false }T
T{ min-int 1 min-int within -> false }T
T{ min-int 1 0 within -> true }T
T{ min-int 1 1 within -> false }T
T{ min-int 1 max-int within -> false }T
T{ min-int max-int min-int within -> false }T
T{ min-int max-int 0 within -> true }T
T{ min-int max-int 1 within -> true }T
T{ min-int max-int max-int within -> false }T
T{ 0 min-int min-int within -> false }T
T{ 0 min-int 0 within -> false }T
T{ 0 min-int 1 within -> true }T
T{ 0 min-int max-int within -> true }T
T{ 0 0 min-int within -> true }T
T{ 0 0 0 within -> false }T
T{ 0 0 1 within -> true }T
T{ 0 0 max-int within -> true }T
T{ 0 1 min-int within -> false }T
T{ 0 1 0 within -> false }T
T{ 0 1 1 within -> false }T
T{ 0 1 max-int within -> false }T
T{ 0 max-int min-int within -> false }T
T{ 0 max-int 0 within -> false }T
T{ 0 max-int 1 within -> true }T
T{ 0 max-int max-int within -> false }T
T{ 1 min-int min-int within -> false }T
T{ 1 min-int 0 within -> false }T
T{ 1 min-int 1 within -> false }T
T{ 1 min-int max-int within -> true }T
T{ 1 0 min-int within -> true }T
T{ 1 0 0 within -> false }T
T{ 1 0 1 within -> false }T
T{ 1 0 max-int within -> true }T
T{ 1 1 min-int within -> true }T
T{ 1 1 0 within -> true }T
T{ 1 1 1 within -> false }T
T{ 1 1 max-int within -> true }T
T{ 1 max-int min-int within -> false }T
T{ 1 max-int 0 within -> false }T
T{ 1 max-int 1 within -> false }T
T{ 1 max-int max-int within -> false }T
T{ max-int min-int min-int within -> false }T
T{ max-int min-int 0 within -> false }T
T{ max-int min-int 1 within -> false }T
T{ max-int min-int max-int within -> false }T
T{ max-int 0 min-int within -> true }T
T{ max-int 0 0 within -> false }T
T{ max-int 0 1 within -> false }T
T{ max-int 0 max-int within -> false }T
T{ max-int 1 min-int within -> true }T
T{ max-int 1 0 within -> true }T
T{ max-int 1 1 within -> false }T
T{ max-int 1 max-int within -> false }T
T{ max-int max-int min-int within -> true }T
T{ max-int max-int 0 within -> true }T
T{ max-int max-int 1 within -> true }T
T{ max-int max-int max-int within -> false }T

\ -----------------------------------------------------------------------------
testing unused  (contributed by James Bowman & Peter Knaggs)

variable unused0
skip T{ unused drop -> }T                  
skip T{ align unused unused0 ! 0 , unused CELL+ unused0 @ = -> true }T
skip T{ unused unused0 ! 0 C, unused char+ unused0 @ = -> true }T  \ aligned -> unaligned
skip T{ unused unused0 ! 0 C, unused char+ unused0 @ = -> true }T  \ unaligned -> ?

\ -----------------------------------------------------------------------------
testing again   (contributed by James Bowman)

T{ : AG0 701 begin dup 7 mod 0= if exit then 1+ again ; -> }T
T{ AG0 -> 707 }T

\ -----------------------------------------------------------------------------
testing marker   (contributed by James Bowman)

T{ : MA? word throw find 0<> ; -> }T
T{ marker MA0 -> }T
T{ : MA1 111 ; -> }T
T{ marker MA2 -> }T
T{ : MA1 222 ; -> }T
T{ MA? MA0 MA? MA1 MA? MA2 -> true true true }T
T{ MA1 MA2 MA1 -> 222 111 }T
T{ MA? MA0 MA? MA1 MA? MA2 -> true true false }T
T{ MA0 -> }T
T{ MA? MA0 MA? MA1 MA? MA2 -> false false false }T

\ -----------------------------------------------------------------------------
testing ?do

: QD ?do i loop ;
T{ 789 789 QD -> }T
T{ -9876 -9876 QD -> }T
T{ 5 0 QD -> 0 1 2 3 4 }T

: QD1 ?do i 10 +loop ;
T{ 50 1 QD1 -> 1 11 21 31 41 }T
T{ 50 0 QD1 -> 0 10 20 30 40 }T

: QD2 ?do i 3 > if leave else i then loop ;
T{ 5 -1 QD2 -> -1 0 1 2 3 }T

: QD3 ?do i 1 +loop ;
T{ 4  4 QD3 -> }T
T{ 4  1 QD3 -> 1 2 3 }T
T{ 2 -1 QD3 -> -1 0 1 }T

: QD4 ?do i -1 +loop ;
T{  4 4 QD4 -> }T
T{  1 4 QD4 -> 4 3 2 1 }T
T{ -1 2 QD4 -> 2 1 0 -1 }T

: QD5 ?do i -10 +loop ;
T{   1 50 QD5 -> 50 40 30 20 10 }T
T{   0 50 QD5 -> 50 40 30 20 10 0 }T
T{ -25 10 QD5 -> 10 0 -10 -20 }T

variable ITERS
variable INcrMNT

: QD6 ( limit start increment -- )
   INcrMNT !
   0 ITERS !
   ?do
      1 ITERS +!
      i
      ITERS @  6 = if leave then
      INcrMNT @
   +loop ITERS @
;

T{  4  4 -1 QD6 -> 0 }T
T{  1  4 -1 QD6 -> 4 3 2 1 4 }T
T{  4  1 -1 QD6 -> 1 0 -1 -2 -3 -4 6 }T
T{  4  1  0 QD6 -> 1 1 1 1 1 1 6 }T
T{  0  0  0 QD6 -> 0 }T
T{  1  4  0 QD6 -> 4 4 4 4 4 4 6 }T
T{  1  4  1 QD6 -> 4 5 6 7 8 9 6 }T
T{  4  1  1 QD6 -> 1 2 3 3 }T
T{  4  4  1 QD6 -> 0 }T
T{  2 -1 -1 QD6 -> -1 -2 -3 -4 -5 -6 6 }T
T{ -1  2 -1 QD6 -> 2 1 0 -1 4 }T
T{  2 -1  0 QD6 -> -1 -1 -1 -1 -1 -1 6 }T
T{ -1  2  0 QD6 -> 2 2 2 2 2 2 6 }T
T{ -1  2  1 QD6 -> 2 3 4 5 6 7 6 }T
T{  2 -1  1 QD6 -> -1 0 1 3 }T

\ -----------------------------------------------------------------------------
testing buffer:

skip T{ 8 buffer: BUF:test -> }T
skip T{ BUF:test dup ALIGNED = -> true }T
skip T{ 111 BUF:test ! 222 BUF:test CELL+ ! -> }T
skip T{ BUF:test @ BUF:test CELL+ @ -> 111 222 }T

\ -----------------------------------------------------------------------------
testing value to

T{ 111 value VAL1 -999 value VAL2 -> }T
T{ VAL1 -> 111 }T
T{ VAL2 -> -999 }T
T{ 222 to VAL1 -> }T
T{ VAL1 -> 222 }T
T{ : VD1 VAL1 ; -> }T
T{ VD1 -> 222 }T
T{ : VD2 to VAL2 ; -> }T
T{ VAL2 -> -999 }T
T{ -333 VD2 -> }T
T{ VAL2 -> -333 }T
T{ VAL1 -> 222 }T
T{ 123 value VAL3 immediate VAL3 -> 123 }T
T{ : VD3 VAL3 literal ; VD3 -> 123 }T

\ -----------------------------------------------------------------------------
testing case of endof endcase

: CS1 case 1 of 111 endof
           2 of 222 endof
           3 of 333 endof
           >r 999 r>
      endcase
;

T{ 1 CS1 -> 111 }T
T{ 2 CS1 -> 222 }T
T{ 3 CS1 -> 333 }T
T{ 4 CS1 -> 999 }T

\ Nested case's

: CS2 >r case -1 of case r@ 1 of 100 endof
                            2 of 200 endof
                           >r -300 r>
                    endcase
                 endof
              -2 of case r@ 1 of -99  endof
                            >r -199 r>
                    endcase
                 endof
                 >r 299 r>
         endcase r> drop
;

T{ -1 1 CS2 ->  100 }T
T{ -1 2 CS2 ->  200 }T
T{ -1 3 CS2 -> -300 }T
T{ -2 1 CS2 -> -99  }T
T{ -2 2 CS2 -> -199 }T
T{  0 2 CS2 ->  299 }T

\ Boolean short circuiting using case

: CS3  ( N1 -- N2 )
   case 1- false of 11 endof
        1- false of 22 endof
        1- false of 33 endof
        44 swap
   endcase
;

T{ 1 CS3 -> 11 }T
T{ 2 CS3 -> 22 }T
T{ 3 CS3 -> 33 }T
T{ 9 CS3 -> 44 }T

\ Empty case statements with/without default

T{ : CS4 case endcase ; 1 CS4 -> }T
T{ : CS5 case 2 swap endcase ; 1 CS5 -> 2 }T
T{ : CS6 case 1 of endof 2 endcase ; 1 CS6 -> }T
T{ : CS7 case 3 of endof 2 endcase ; 1 CS7 -> 1 }T

\ -----------------------------------------------------------------------------
testing :noname recurse

variable NN1
variable NN2
:noname 1234 ; NN1 !
:noname 9876 ; NN2 !
T{ NN1 @ execute -> 1234 }T
T{ NN2 @ execute -> 9876 }T

T{ :noname ( n -- 0,1,..n ) dup if dup >r 1- recurse r> then ;
   constant RN1 -> }T
T{ 0 RN1 execute -> 0 }T
T{ 4 RN1 execute -> 0 1 2 3 4 }T

:noname  ( n -- n1 )    \ Multiple RECURSEs in one definition
   1- dup
   case 0 of exit endof
        1 of 11 swap recurse endof
        2 of 22 swap recurse endof
        3 of 33 swap recurse endof
        drop abs recurse exit
   endcase
; constant RN2

T{  1 RN2 execute -> 0 }T
T{  2 RN2 execute -> 11 0 }T
T{  4 RN2 execute -> 33 22 11 0 }T
T{ 25 RN2 execute -> 33 22 11 0 }T

\ -----------------------------------------------------------------------------
testing C"

skip T{ : CQ1 C" 123" ; -> }T
skip T{ CQ1 count evaluate -> 123 }T
skip T{ : CQ2 C" " ; -> }T
skip T{ CQ2 count evaluate -> }T
skip T{ : CQ3 C" 2345"count evaluate ; CQ3 -> 2345 }T

\ -----------------------------------------------------------------------------
testing compile,

:noname dup + ; constant dup+
T{ : Q dup+ compile, ; -> }T
T{ : AS1 [ Q ] ; -> }T
T{ 123 AS1 -> 246 }T

\ -----------------------------------------------------------------------------
\ Cannot automatically test SAVE-INPUT and RESTORE-INPUT from a console source

testing SAVE-INPUT and RESTORE-INPUT with a string source

variable SI_INC 0 SI_INC !

: SI1
   SI_INC @ >in +!
   15 SI_INC !
;

: s$ s" SAVE-INPUT SI1 RESTORE-INPUT 12345" ;

skip T{ s$ evaluate SI_INC @ -> 0 2345 15 }T

\ -----------------------------------------------------------------------------
testing .(

cr cr .( Output from .() 
T{ cr .( You should see -9876: ) -9876 . -> }T
T{ cr .( and again: ).( -9876)cr -> }T

cr cr .( On the next 2 lines you should see First then Second messages:)
T{ : DOTP  cr ." Second message via ." [char] " emit    \ Check .( is immediate
     [ cr ] .( First message via .( ) ; DOTP -> }T
cr cr
skip T{ : IMM? word throw find nip ; IMM? .( -> 1 }T

\ -----------------------------------------------------------------------------
testing .r and u.r - has to handle different cell sizes

\ Create some large integers just below/above MAX and Min INTs
\ max-int 73 79 */ constant LI1
\ min-int 71 73 */ constant LI2
\ 
\ LI1 0 <# #S #> nip constant LENLI1

\ : (.r&u.r)  ( u1 u2 -- )  \ u1 <= string length, u2 is required indentation
\    tuck + >r
\    LI1 over SPACES  . cr r@    LI1 swap  .r cr
\    LI2 over SPACES  . cr r@ 1+ LI2 swap  .r cr
\    LI1 over SPACES u. cr r@    LI1 swap u.r cr
\    LI2 swap SPACES u. cr r>    LI2 swap u.r cr
\ ;
\ 
\ : .r&u.r  ( -- )
\    cr ." You should see lines duplicated:" cr
\    ." indented by 0 spaces" cr 0      0 (.r&u.r) cr
\    ." indented by 0 spaces" cr LENLI1 0 (.r&u.r) cr \ Just fits required width
\    ." indented by 5 spaces" cr LENLI1 5 (.r&u.r) cr
\ ;
\ 
\ cr cr .( Output from .r and u.r)
skip T{ .r&u.r -> }T

\ -----------------------------------------------------------------------------
testing pad erase
\ Must handle different size characters i.e. 1 chars >= 1 

84 constant chars/pad      \ Minimum size of pad in chars
chars/pad chars constant AUS/pad
: checkpad  ( caddr u ch -- f )  \ f = true if u chars = ch
   swap 0
   ?do
      over i chars + c@ over <>
      if 2drop unloop false exit then
   loop  
   2drop true
;

skip T{ pad drop -> }T
skip T{ 0 invert pad C! -> }T
skip T{ pad c@ constant MAXCHAR -> }T
skip T{ pad chars/pad 2DUP MAXCHAR FILL MAXCHAR checkpad -> true }T
skip T{ pad chars/pad 2DUP chars erase 0 checkpad -> true }T
skip T{ pad chars/pad 2DUP MAXCHAR FILL pad 0 erase MAXCHAR checkpad -> true }T
skip T{ pad 43 chars + 9 chars erase -> }T
skip T{ pad 43 MAXCHAR checkpad -> true }T
skip T{ pad 43 chars + 9 0 checkpad -> true }T
skip T{ pad 52 chars + chars/pad 52 - MAXCHAR checkpad -> true }T

\ Check that use of word and pictured numeric output do not corrupt pad
\ Minimum size of buffers for these are 33 chars and (2*n)+2 chars respectively
\ where n is number of bits per cell

\ pad chars/pad erase
\ 2 base !
\ max-uint max-uint <# #S char 1 dup hold hold #> 2drop
\ decimal
\ bl word 12345678123456781234567812345678 drop
skip T{ pad chars/pad 0 checkpad -> true }T

\ -----------------------------------------------------------------------------
testing parse

skip T{ char | parse 1234| dup rot rot evaluate -> 4 1234 }T
skip T{ char ^ parse  23 45 ^ dup rot rot evaluate -> 7 23 45 }T
\ : PA1 [char] $ parse dup >r pad swap chars MOVE pad r> ;
skip T{ PA1 3456
skip    dup rot rot evaluate -> 4 3456 }T
skip T{ char A parse A swap drop -> 0 }T
skip T{ char Z parse
skip    swap drop -> 0 }T
skip T{ char " parse 4567 "dup rot rot evaluate -> 5 4567 }T
 
\ -----------------------------------------------------------------------------
testing parse-name  (Forth 2012)
\ Adapted from the parse-name RfD tests

skip T{ parse-name abcd  STR1  S= -> true }T        \ No leading spaces
skip T{ parse-name      abcde STR2 S= -> true }T    \ Leading spaces

\ Test empty parse area, new lines are necessary
skip T{ parse-name
skip  nip -> 0 }T
\ Empty parse area with spaces after parse-name
skip T{ parse-name         
skip  nip -> 0 }T

skip T{ : parse-name-test ( "name1" "name2" -- n )
skip     parse-name parse-name S= ; -> }T
skip T{ parse-name-test abcd abcd  -> true }T
skip T{ parse-name-test abcd   abcd  -> true }T  \ Leading spaces
skip T{ parse-name-test abcde abcdf -> false }T
skip T{ parse-name-test abcdf abcde -> false }T
skip T{ parse-name-test abcde abcde
skip    -> true }T         \ Parse to end of line
skip T{ parse-name-test abcde           abcde         
skip    -> true }T         \ Leading and trailing spaces

\ -----------------------------------------------------------------------------
testing defer defer@ defer! is action-of (Forth 2012)
\ Adapted from the Forth 200X RfD tests

skip T{ defer defer1 -> }T
skip T{ : MY-defer defer ; -> }T
skip T{ : is-defer1 is defer1 ; -> }T
skip T{ : action-defer1 action-of defer1 ; -> }T
skip T{ : DEF! defer! ; -> }T
skip T{ : DEF@ defer@ ; -> }T

skip T{ ' * ' defer1 defer! -> }T
skip T{ 2 3 defer1 -> 6 }T
skip T{ ' defer1 defer@ -> ' * }T
skip T{ ' defer1 DEF@ -> ' * }T
skip T{ action-of defer1 -> ' * }T
skip T{ action-defer1 -> ' * }T
skip T{ ' + is defer1 -> }T
skip T{ 1 2 defer1 -> 3 }T
skip T{ ' defer1 defer@ -> ' + }T
skip T{ ' defer1 DEF@ -> ' + }T
skip T{ action-of defer1 -> ' + }T
skip T{ action-defer1 -> ' + }T
skip T{ ' - is-defer1 -> }T
skip T{ 1 2 defer1 -> -1 }T
skip T{ ' defer1 defer@ -> ' - }T
skip T{ ' defer1 DEF@ -> ' - }T
skip T{ action-of defer1 -> ' - }T
skip T{ action-defer1 -> ' - }T

skip T{ MY-defer defer2 -> }T
skip T{ ' dup is defer2 -> }T
skip T{ 1 defer2 -> 1 1 }T

\ -----------------------------------------------------------------------------
testing holds  (Forth 2012)

: htest s" Testing holds" ;
: htest2 s" works" ;
: htest3 s" Testing holds works 123" ;
skip T{ 0 0 <#  htest holds #> htest S= -> true }T
skip T{ 123 0 <# #S bl hold htest2 holds bl hold htest holds #>
skip    htest3 S= -> true }T
skip T{ : HLD holds ; -> }T
skip T{ 0 0 <#  htest HLD #> htest S= -> true }T

\ -----------------------------------------------------------------------------
testing refill source-id
\ refill and source-id from the user input device can't be tested from a file,
\ can only be tested from a string via evaluate

skip T{ : RF1  s" refill" evaluate ; RF1 -> false }T
skip T{ : SID1  s" source-id" evaluate ; SID1 -> -1 }T

\ ------------------------------------------------------------------------------
testing s\"  (Forth 2012 compilation mode)
\ Extended the Forth 200X RfD tests
\ Note this tests the Core Ext definition of s\" which has unedfined
\ interpretation semantics. s\" in interpretation mode is tested in the tests on
\ the File-Access word set

skip T{ : SSQ1 s\" abc" s" abc" S= ; -> }T  \ No escapes
skip T{ SSQ1 -> true }T
skip T{ : SSQ2 s\" " ; SSQ2 swap drop -> 0 }T    \ Empty string

skip T{ : SSQ3 s\" \a\b\e\f\l\m\q\r\t\v\x0F0\x1Fa\xaBx\z\"\\" ; -> }T
skip T{ SSQ3 swap drop          ->  20 }T    \ String length
skip T{ SSQ3 drop            c@ ->   7 }T    \ \a   BEL  Bell
skip T{ SSQ3 drop  1 chars + c@ ->   8 }T    \ \b   BS   Backspace
skip T{ SSQ3 drop  2 chars + c@ ->  27 }T    \ \e   ESC  Escape
skip T{ SSQ3 drop  3 chars + c@ ->  12 }T    \ \f   FF   Form feed
skip T{ SSQ3 drop  4 chars + c@ ->  10 }T    \ \l   LF   Line feed
skip T{ SSQ3 drop  5 chars + c@ ->  13 }T    \ \m        cr of cr/LF pair
skip T{ SSQ3 drop  6 chars + c@ ->  10 }T    \           LF of cr/LF pair
skip T{ SSQ3 drop  7 chars + c@ ->  34 }T    \ \q   "    Double Quote
skip T{ SSQ3 drop  8 chars + c@ ->  13 }T    \ \r   cr   Carriage Return
skip T{ SSQ3 drop  9 chars + c@ ->   9 }T    \ \t   TAB  Horizontal Tab
skip T{ SSQ3 drop 10 chars + c@ ->  11 }T    \ \v   VT   Vertical Tab
skip T{ SSQ3 drop 11 chars + c@ ->  15 }T    \ \x0F      Given Char
skip T{ SSQ3 drop 12 chars + c@ ->  48 }T    \ 0    0    Digit follow on
skip T{ SSQ3 drop 13 chars + c@ ->  31 }T    \ \x1F      Given Char
skip T{ SSQ3 drop 14 chars + c@ ->  97 }T    \ a    a    Hex follow on
skip T{ SSQ3 drop 15 chars + c@ -> 171 }T    \ \xaB      Insensitive Given Char
skip T{ SSQ3 drop 16 chars + c@ -> 120 }T    \ x    x    Non hex follow on
skip T{ SSQ3 drop 17 chars + c@ ->   0 }T    \ \z   NUL  No Character
skip T{ SSQ3 drop 18 chars + c@ ->  34 }T    \ \"   "    Double Quote
skip T{ SSQ3 drop 19 chars + c@ ->  92 }T    \ \\   \    Back Slash

\ The above does not test \n as this is a system dependent value.
\ Check it displays a new line
cr .( The next test should display:)
cr .( One line...)
cr .( another line)
skip T{ : SSQ4 s\" \nOne line...\nanotherLine\n" TYPE ; SSQ4 -> }T

skip \ Test bare escapable characters appear as themselves
skip T{ : SSQ5 s\" abeflmnqrtvxz" s" abeflmnqrtvxz" S= ; SSQ5 -> true }T

skip T{ : SSQ6 s\" a\""2drop 1111 ; SSQ6 -> 1111 }T \ Parsing behaviour

skip T{ : SSQ7  s\" 111 : SSQ8 S\\\" 222\" evaluate ; SSQ8 333" evaluate ; -> }T
skip T{ SSQ7 -> 111 222 333 }T
skip T{ : SSQ9  s\" 11 : SSQ10 S\\\" \\x32\\x32\" evaluate ; SSQ10 33" evaluate ; -> }T
skip T{ SSQ9 -> 11 22 33 }T

\ -----------------------------------------------------------------------------
core-ext-errors set-error-count

cr .( End of Core Extension word tests) cr


