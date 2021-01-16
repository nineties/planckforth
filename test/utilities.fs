\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

decimal

( First a definition to see if a word is already defined. Note that          )
( [defined] [if] [else] and [then] are in the optional Programming Tools     )
( word set.                                                                  )

variable (\?) 0 (\?) !     ( Flag: Word defined = 0 | word undefined = 1 )

( [?def]  followed by [?if] cannot be used again until after [then] )
: [?def]  ( "name" -- )
   word throw find 0= (\?) !
;

\ Test [?def]
T{ 0 (\?) ! [?def] ?deftest1 (\?) @ -> 1 }T
: ?deftest1 1 ;
T{ -1 (\?) ! [?def] ?deftest1 (\?) @ -> 0 }T

: [?undef] [?def] (\?) @ 0= (\?) ! ;

\ Equivalents of [if] [else] [then], these must not be nested
: [?if]  ( f -- )  (\?) ! ; immediate
: [?else]  ( -- )  (\?) @ 0= (\?) ! ; immediate
: [?then]  ( -- )  0 (\?) ! ; immediate

( A conditional comment and \ will be defined. Note that these definitions )
( are inadequate for use in Forth blocks. If needed in the blocks test     )
( program they will need to be modified here or redefined there )

( \? is a conditional comment )
: \?  ( "..." -- )  (\?) @ if exit then source strlen >in ! ; immediate

\ Test \?
T{ [?def] ?deftest1 \? : ?deftest1 2 ;    \ Should not be redefined
          ?deftest1 -> 1 }T
T{ [?def] ?deftest2 \? : ?deftest1 2 ;    \ Should be redefined
          ?deftest1 -> 2 }T

[?def] true  \?  1 constant true
[?def] false \?  0 constant false
[?def] nip   \?  : nip swap drop ;
[?def] tuck  \?  : tuck swap over ;

( source R:c )
[?def] parse
\? : parse  ( ch "ccc<ch>" -- caddr u )
\?    >r source >in @ + ( start )
\?    dup r> swap >r >r ( start, R: start ch )
\?    begin
\?       dup c@
\?    while
\?       dup c@ r@ <>
\?    while
\?       1+
\?    repeat
\?    dup source - 1+ >in ! 
\?    r> drop r> tuck - 1 /
\? ;

[?def] .(  \? : .(  [char] ) parse typen ; immediate

\ \ s=  to compare (case sensitive) two strings to avoid use of COMPARE from
\ \ the String word set. It is defined in core.fr and conditionally defined
\ \ here if core.fr has not been included by the user
\ 
\ [?def] s=
\ \? : s=  ( caddr1 u1 caddr2 u2 -- f )   \ f = true if strings are equal
\ \?    rot over = 0= if drop 2drop false exit then
\ \?    dup 0= if drop 2drop true exit then 
\ \?    0 do
\ \?         over c@ over c@ = 0= if 2drop false unloop exit then
\ \?         char+ swap char+
\ \?      loop 2drop true
\ \? ;
\ 
\ \ Buffer for strings in interpretive mode since s" only valid in compilation
\ \ mode when File-Access word set is defined
\ 
\ 64 constant sbuf-size
\ create sbuf1 sbuf-size chars allot
\ create sbuf2 sbuf-size chars allot
\ 
\ \ ($") saves string at (caddr)
\ : ($")  ( caddr "ccc" -- caddr' )
\    [char] " parse rot 2dup c!       ( -- ca2 u2 ca)
\    char+ swap 2dup 2>r chars move   ( -- )  ( R: -- ca' u2 )
\    2r>
\ ;
\ 
\ : $"   ( "ccc" -- caddr u )  sbuf1 ($") ;
\ : $2"  ( "ccc" -- caddr u )  sbuf2 ($") ;
\ : $clear  ( caddr -- ) sbuf-size bl fill ;
\ : clear-sbufs  ( -- )  sbuf1 $clear sbuf2 $clear ;
\ 
\ \ More definitions in core.fr used in other test programs, conditionally
\ \ defined here if core.fr has not been loaded
\ 
\ [?def] max-uint   \? 0 invert                 constant max-uint
\ [?def] max-int    \? 0 invert 1 rshift        constant max-int
\ [?def] min-int    \? 0 invert 1 rshift invert constant min-int
\ [?def] mid-uint   \? 0 invert 1 rshift        constant mid-uint
\ [?def] mid-uint+1 \? 0 invert 1 rshift invert constant mid-uint+1
\ 
\ [?def] 2constant \? : 2constant  create , , does> 2@ ;
\ 
\ base @ 2 base ! -1 0 <# #S #> swap drop constant bits/cell base !
\ 
\ 
\ \ ------------------------------------------------------------------------------
\ \ Tests
\ 
\ : str1  s" abcd" ;  : str2  s" abcde" ;
\ : str3  s" abCd" ;  : str4  s" wbcd"  ;
\ : s"" s" " ;
\ 
\ T{ str1 2dup s= -> true }T
\ T{ str2 2dup s= -> true }T
\ T{ s""  2dup s= -> true }T
\ T{ str1 str2 s= -> false }T
\ T{ str1 str3 s= -> false }T
\ T{ str1 str4 s= -> false }T
\ 
\ T{ clear-sbufs -> }T
\ T{ $" abcdefghijklm"  sbuf1 count s= -> true  }T
\ T{ $" nopqrstuvwxyz"  sbuf2 over  s= -> false }T
\ T{ $2" abcdefghijklm" sbuf1 count s= -> false }T
\ T{ $2" nopqrstuvwxyz" sbuf1 count s= -> true  }T
\ 
\ \ ------------------------------------------------------------------------------
\ 
\ CR $" Test utilities loaded" type CR
