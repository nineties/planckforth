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
variable INCRMNT

: QD6 ( limit start increment -- )
   INCRMNT !
   0 ITERS !
   ?do
      1 ITERS +!
      i
      ITERS @  6 = if leave then
      INCRMNT @
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

skip T{ 8 buffer: BUF:TEST -> }T
skip T{ BUF:TEST dup ALIGNED = -> true }T
skip T{ 111 BUF:TEST ! 222 BUF:TEST CELL+ ! -> }T
skip T{ BUF:TEST @ BUF:TEST CELL+ @ -> 111 222 }T

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
testing CASE OF ENDOF ENDCASE

: CS1 CASE 1 OF 111 ENDOF
           2 OF 222 ENDOF
           3 OF 333 ENDOF
           >r 999 r>
      ENDCASE
;

T{ 1 CS1 -> 111 }T
T{ 2 CS1 -> 222 }T
T{ 3 CS1 -> 333 }T
T{ 4 CS1 -> 999 }T

\ Nested CASE's

: CS2 >r CASE -1 OF CASE r@ 1 OF 100 ENDOF
                            2 OF 200 ENDOF
                           >r -300 r>
                    ENDCASE
                 ENDOF
              -2 OF CASE r@ 1 OF -99  ENDOF
                            >r -199 r>
                    ENDCASE
                 ENDOF
                 >r 299 r>
         ENDCASE r> drop
;

T{ -1 1 CS2 ->  100 }T
T{ -1 2 CS2 ->  200 }T
T{ -1 3 CS2 -> -300 }T
T{ -2 1 CS2 -> -99  }T
T{ -2 2 CS2 -> -199 }T
T{  0 2 CS2 ->  299 }T

\ Boolean short circuiting using CASE

: CS3  ( N1 -- N2 )
   CASE 1- false OF 11 ENDOF
        1- false OF 22 ENDOF
        1- false OF 33 ENDOF
        44 swap
   ENDCASE
;

T{ 1 CS3 -> 11 }T
T{ 2 CS3 -> 22 }T
T{ 3 CS3 -> 33 }T
T{ 9 CS3 -> 44 }T

\ Empty CASE statements with/without default

T{ : CS4 CASE ENDCASE ; 1 CS4 -> }T
T{ : CS5 CASE 2 swap ENDCASE ; 1 CS5 -> 2 }T
T{ : CS6 CASE 1 OF ENDOF 2 ENDCASE ; 1 CS6 -> }T
T{ : CS7 CASE 3 OF ENDOF 2 ENDCASE ; 1 CS7 -> 1 }T

\ -----------------------------------------------------------------------------
testing :NONAME RECURSE

variable NN1
variable NN2
:NONAME 1234 ; NN1 !
:NONAME 9876 ; NN2 !
T{ NN1 @ EXECUTE -> 1234 }T
T{ NN2 @ EXECUTE -> 9876 }T

T{ :NONAME ( n -- 0,1,..n ) dup if dup >r 1- RECURSE r> then ;
   constant RN1 -> }T
T{ 0 RN1 EXECUTE -> 0 }T
T{ 4 RN1 EXECUTE -> 0 1 2 3 4 }T

:NONAME  ( n -- n1 )    \ Multiple RECURSEs in one definition
   1- dup
   CASE 0 OF exit ENDOF
        1 OF 11 swap RECURSE ENDOF
        2 OF 22 swap RECURSE ENDOF
        3 OF 33 swap RECURSE ENDOF
        drop ABS RECURSE exit
   ENDCASE
; constant RN2

T{  1 RN2 EXECUTE -> 0 }T
T{  2 RN2 EXECUTE -> 11 0 }T
T{  4 RN2 EXECUTE -> 33 22 11 0 }T
T{ 25 RN2 EXECUTE -> 33 22 11 0 }T

\ -----------------------------------------------------------------------------
testing C"

T{ : CQ1 C" 123" ; -> }T
T{ CQ1 COUNT EVALUATE -> 123 }T
T{ : CQ2 C" " ; -> }T
T{ CQ2 COUNT EVALUATE -> }T
T{ : CQ3 C" 2345"COUNT EVALUATE ; CQ3 -> 2345 }T

\ -----------------------------------------------------------------------------
testing COMPILE,

:NONAME dup + ; constant dup+
T{ : Q dup+ COMPILE, ; -> }T
T{ : AS1 [ Q ] ; -> }T
T{ 123 AS1 -> 246 }T

\ -----------------------------------------------------------------------------
\ Cannot automatically test SAVE-INPUT and RESTORE-INPUT from a console source

testing SAVE-INPUT and RESTORE-INPUT with a string source

variable SI_INC 0 SI_INC !

: SI1
   SI_INC @ >IN +!
   15 SI_INC !
;

: S$ S" SAVE-INPUT SI1 RESTORE-INPUT 12345" ;

T{ S$ EVALUATE SI_INC @ -> 0 2345 15 }T

\ -----------------------------------------------------------------------------
testing .(

CR CR .( Output from .() 
T{ CR .( You should see -9876: ) -9876 . -> }T
T{ CR .( and again: ).( -9876)CR -> }T

CR CR .( On the next 2 lines you should see First then Second messages:)
T{ : DOTP  CR ." Second message via ." [char] " EMIT    \ Check .( is immediate
     [ CR ] .( First message via .( ) ; DOTP -> }T
CR CR
T{ : IMM? bl word find nip ; IMM? .( -> 1 }T

\ -----------------------------------------------------------------------------
testing .R and U.R - has to handle different cell sizes

\ Create some large integers just below/above MAX and Min INTs
max-int 73 79 */ constant LI1
min-int 71 73 */ constant LI2

LI1 0 <# #S #> nip constant LENLI1

: (.R&U.R)  ( u1 u2 -- )  \ u1 <= string length, u2 is required indentation
   tuck + >r
   LI1 over SPACES  . CR r@    LI1 swap  .R CR
   LI2 over SPACES  . CR r@ 1+ LI2 swap  .R CR
   LI1 over SPACES U. CR r@    LI1 swap U.R CR
   LI2 swap SPACES U. CR r>    LI2 swap U.R CR
;

: .R&U.R  ( -- )
   CR ." You should see lines duplicated:" CR
   ." indented by 0 spaces" CR 0      0 (.R&U.R) CR
   ." indented by 0 spaces" CR LENLI1 0 (.R&U.R) CR \ Just fits required width
   ." indented by 5 spaces" CR LENLI1 5 (.R&U.R) CR
;

CR CR .( Output from .R and U.R)
T{ .R&U.R -> }T

\ -----------------------------------------------------------------------------
testing PAD ERASE
\ Must handle different size characters i.e. 1 CHARS >= 1 

84 constant CHARS/PAD      \ Minimum size of PAD in chars
CHARS/PAD CHARS constant AUS/PAD
: CHECKPAD  ( caddr u ch -- f )  \ f = true if u chars = ch
   swap 0
   ?do
      over i CHARS + C@ over <>
      if 2drop UNLOOP false exit then
   loop  
   2drop true
;

T{ PAD drop -> }T
T{ 0 invert PAD C! -> }T
T{ PAD C@ constant MAXCHAR -> }T
T{ PAD CHARS/PAD 2DUP MAXCHAR FILL MAXCHAR CHECKPAD -> true }T
T{ PAD CHARS/PAD 2DUP CHARS ERASE 0 CHECKPAD -> true }T
T{ PAD CHARS/PAD 2DUP MAXCHAR FILL PAD 0 ERASE MAXCHAR CHECKPAD -> true }T
T{ PAD 43 CHARS + 9 CHARS ERASE -> }T
T{ PAD 43 MAXCHAR CHECKPAD -> true }T
T{ PAD 43 CHARS + 9 0 CHECKPAD -> true }T
T{ PAD 52 CHARS + CHARS/PAD 52 - MAXCHAR CHECKPAD -> true }T

\ Check that use of word and pictured numeric output do not corrupt PAD
\ Minimum size of buffers for these are 33 chars and (2*n)+2 chars respectively
\ where n is number of bits per cell

PAD CHARS/PAD ERASE
2 base !
max-uint max-uint <# #S char 1 dup HOLD HOLD #> 2drop
decimal
bl word 12345678123456781234567812345678 drop
T{ PAD CHARS/PAD 0 CHECKPAD -> true }T

\ -----------------------------------------------------------------------------
testing PARSE

T{ char | PARSE 1234| dup rot rot EVALUATE -> 4 1234 }T
T{ char ^ PARSE  23 45 ^ dup rot rot EVALUATE -> 7 23 45 }T
: PA1 [char] $ PARSE dup >r PAD swap CHARS MOVE PAD r> ;
T{ PA1 3456
   dup rot rot EVALUATE -> 4 3456 }T
T{ char A PARSE A swap drop -> 0 }T
T{ char Z PARSE
   swap drop -> 0 }T
T{ char " PARSE 4567 "dup rot rot EVALUATE -> 5 4567 }T
 
\ -----------------------------------------------------------------------------
testing PARSE-NAME  (Forth 2012)
\ Adapted from the PARSE-NAME RfD tests

T{ PARSE-NAME abcd  STR1  S= -> true }T        \ No leading spaces
T{ PARSE-NAME      abcde STR2 S= -> true }T    \ Leading spaces

\ Test empty parse area, new lines are necessary
T{ PARSE-NAME
  nip -> 0 }T
\ Empty parse area with spaces after PARSE-NAME
T{ PARSE-NAME         
  nip -> 0 }T

T{ : PARSE-NAME-TEST ( "name1" "name2" -- n )
    PARSE-NAME PARSE-NAME S= ; -> }T
T{ PARSE-NAME-TEST abcd abcd  -> true }T
T{ PARSE-NAME-TEST abcd   abcd  -> true }T  \ Leading spaces
T{ PARSE-NAME-TEST abcde abcdf -> false }T
T{ PARSE-NAME-TEST abcdf abcde -> false }T
T{ PARSE-NAME-TEST abcde abcde
   -> true }T         \ Parse to end of line
T{ PARSE-NAME-TEST abcde           abcde         
   -> true }T         \ Leading and trailing spaces

\ -----------------------------------------------------------------------------
testing DEFER DEFEr@ DEFER! IS ACTION-OF (Forth 2012)
\ Adapted from the Forth 200X RfD tests

T{ DEFER DEFER1 -> }T
T{ : MY-DEFER DEFER ; -> }T
T{ : IS-DEFER1 IS DEFER1 ; -> }T
T{ : ACTION-DEFER1 ACTION-OF DEFER1 ; -> }T
T{ : DEF! DEFER! ; -> }T
T{ : DEF@ DEFEr@ ; -> }T

T{ ' * ' DEFER1 DEFER! -> }T
T{ 2 3 DEFER1 -> 6 }T
T{ ' DEFER1 DEFEr@ -> ' * }T
T{ ' DEFER1 DEF@ -> ' * }T
T{ ACTION-OF DEFER1 -> ' * }T
T{ ACTION-DEFER1 -> ' * }T
T{ ' + IS DEFER1 -> }T
T{ 1 2 DEFER1 -> 3 }T
T{ ' DEFER1 DEFEr@ -> ' + }T
T{ ' DEFER1 DEF@ -> ' + }T
T{ ACTION-OF DEFER1 -> ' + }T
T{ ACTION-DEFER1 -> ' + }T
T{ ' - IS-DEFER1 -> }T
T{ 1 2 DEFER1 -> -1 }T
T{ ' DEFER1 DEFEr@ -> ' - }T
T{ ' DEFER1 DEF@ -> ' - }T
T{ ACTION-OF DEFER1 -> ' - }T
T{ ACTION-DEFER1 -> ' - }T

T{ MY-DEFER DEFER2 -> }T
T{ ' dup IS DEFER2 -> }T
T{ 1 DEFER2 -> 1 1 }T

\ -----------------------------------------------------------------------------
testing HOLDS  (Forth 2012)

: HTEST S" Testing HOLDS" ;
: HTEST2 S" works" ;
: HTEST3 S" Testing HOLDS works 123" ;
T{ 0 0 <#  HTEST HOLDS #> HTEST S= -> true }T
T{ 123 0 <# #S bl HOLD HTEST2 HOLDS bl HOLD HTEST HOLDS #>
   HTEST3 S= -> true }T
T{ : HLD HOLDS ; -> }T
T{ 0 0 <#  HTEST HLD #> HTEST S= -> true }T

\ -----------------------------------------------------------------------------
testing REFILL SOURCE-ID
\ REFILL and SOURCE-ID from the user input device can't be tested from a file,
\ can only be tested from a string via EVALUATE

T{ : RF1  S" REFILL" EVALUATE ; RF1 -> false }T
T{ : SID1  S" SOURCE-ID" EVALUATE ; SID1 -> -1 }T

\ ------------------------------------------------------------------------------
testing S\"  (Forth 2012 compilation mode)
\ Extended the Forth 200X RfD tests
\ Note this tests the Core Ext definition of S\" which has unedfined
\ interpretation semantics. S\" in interpretation mode is tested in the tests on
\ the File-Access word set

T{ : SSQ1 S\" abc" S" abc" S= ; -> }T  \ No escapes
T{ SSQ1 -> true }T
T{ : SSQ2 S\" " ; SSQ2 swap drop -> 0 }T    \ Empty string

T{ : SSQ3 S\" \a\b\e\f\l\m\q\r\t\v\x0F0\x1Fa\xaBx\z\"\\" ; -> }T
T{ SSQ3 swap drop          ->  20 }T    \ String length
T{ SSQ3 drop            C@ ->   7 }T    \ \a   BEL  Bell
T{ SSQ3 drop  1 CHARS + C@ ->   8 }T    \ \b   BS   Backspace
T{ SSQ3 drop  2 CHARS + C@ ->  27 }T    \ \e   ESC  Escape
T{ SSQ3 drop  3 CHARS + C@ ->  12 }T    \ \f   FF   Form feed
T{ SSQ3 drop  4 CHARS + C@ ->  10 }T    \ \l   LF   Line feed
T{ SSQ3 drop  5 CHARS + C@ ->  13 }T    \ \m        CR of CR/LF pair
T{ SSQ3 drop  6 CHARS + C@ ->  10 }T    \           LF of CR/LF pair
T{ SSQ3 drop  7 CHARS + C@ ->  34 }T    \ \q   "    Double Quote
T{ SSQ3 drop  8 CHARS + C@ ->  13 }T    \ \r   CR   Carriage Return
T{ SSQ3 drop  9 CHARS + C@ ->   9 }T    \ \t   TAB  Horizontal Tab
T{ SSQ3 drop 10 CHARS + C@ ->  11 }T    \ \v   VT   Vertical Tab
T{ SSQ3 drop 11 CHARS + C@ ->  15 }T    \ \x0F      Given Char
T{ SSQ3 drop 12 CHARS + C@ ->  48 }T    \ 0    0    Digit follow on
T{ SSQ3 drop 13 CHARS + C@ ->  31 }T    \ \x1F      Given Char
T{ SSQ3 drop 14 CHARS + C@ ->  97 }T    \ a    a    Hex follow on
T{ SSQ3 drop 15 CHARS + C@ -> 171 }T    \ \xaB      Insensitive Given Char
T{ SSQ3 drop 16 CHARS + C@ -> 120 }T    \ x    x    Non hex follow on
T{ SSQ3 drop 17 CHARS + C@ ->   0 }T    \ \z   NUL  No Character
T{ SSQ3 drop 18 CHARS + C@ ->  34 }T    \ \"   "    Double Quote
T{ SSQ3 drop 19 CHARS + C@ ->  92 }T    \ \\   \    Back Slash

\ The above does not test \n as this is a system dependent value.
\ Check it displays a new line
CR .( The next test should display:)
CR .( One line...)
CR .( another line)
T{ : SSQ4 S\" \nOne line...\nanotherLine\n" TYPE ; SSQ4 -> }T

\ Test bare escapable characters appear as themselves
T{ : SSQ5 S\" abeflmnqrtvxz" S" abeflmnqrtvxz" S= ; SSQ5 -> true }T

T{ : SSQ6 S\" a\""2drop 1111 ; SSQ6 -> 1111 }T \ Parsing behaviour

T{ : SSQ7  S\" 111 : SSQ8 S\\\" 222\" EVALUATE ; SSQ8 333" EVALUATE ; -> }T
T{ SSQ7 -> 111 222 333 }T
T{ : SSQ9  S\" 11 : SSQ10 S\\\" \\x32\\x32\" EVALUATE ; SSQ10 33" EVALUATE ; -> }T
T{ SSQ9 -> 11 22 33 }T

\ -----------------------------------------------------------------------------
CORE-EXT-ERRORS SET-ERROR-COUNT

CR .( End of Core Extension word tests) CR


