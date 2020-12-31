h@l@h@!h@C+h!k1k0-h@$k
h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!
h@l@h@!h@C+h!k1k0-h@$k h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!

h@l@ h@!h@C+h! k1k0-h@$ k\h@k1k0-+$ h@C+h!
    i       h@!h@C+h!
    kkf     h@!h@C+h!
    kLf     h@!h@C+h!
    k:k0-   h@!h@C+h!
    k=f     h@!h@C+h!
    kJf     h@!h@C+h!
    k0k5-C* h@!h@C+h!
    kef     h@!h@C+h!
l!

\ **Now we can use single-line comments!**

\ planckforth -
\ Copyright (C) 2020 nineties

\ This project aims to create a full-fledged Forth interpreter
\ by bootstrapping from hand-written tiny ELF binary.

\ In the 1st stage, only single character words are registered
\ in the dictionary.
\ List of builtin words:
\ 'Q' ( -- )         Exit the process
\ 'C' ( -- n )       The size of Cells
\ 'h' ( -- addr )    The address of 'here' cell
\ 'l' ( -- addr )    The address of 'latest' cell
\ 'k' ( -- c )       Read character
\ 't' ( c -- )       Print character
\ 'j' ( -- )         Unconditional branch
\ 'J' ( a -- )       Jump if a == 0
\ 'f' ( c -- xt )    Get execution token of c
\ 'x' ( xt -- ... )  Run the execution token
\ '@' ( addr -- a )  Load value from addr
\ '!' ( a addr -- )  Store value to addr
\ '?' ( addr -- c )  Load byte from addr
\ '$' ( c addr -- )  Store byte to addr
\ 'd' ( -- addr )    Get data stack pointer
\ 'D' ( addr -- )    Set data stack pointer
\ 'r' ( -- addr )    Get return stack pointer
\ 'R' ( addr -- )    Set return stack pointer
\ 'i' ( -- addr )    Get the interpreter function
\ 'e' ( -- )         Exit current function
\ 'L' ( -- a )       Load immediate
\ 'S' ( -- addr len) Load string literal
\ '+' ( a b -- c )   c = (a + b)
\ '-' ( a b -- c )   c = (a - b)
\ '*' ( a b -- c )   c = (a * b)
\ '/' ( a b -- c )   c = (a / b)
\ '%' ( a b -- c )   c = (a % b)
\ '&' ( a b -- c )   c = (a & b)
\ '|' ( a b -- c )   c = (a | b)
\ '^' ( a b -- c )   c = (a ^ b)
\ '<' ( a b -- c )   c = (a < b)
\ '=' ( a b -- c )   c = (a == b

\ The 1st stage interpreter repeats execution of k, f and x.
\ There is no concept such as IMMEDIATE mode yet.

\ There following line is an example program of planckforth
\ which prints "Hello World!\n"
\ --
\ kHtketkltkltkotk tkWtkotkrtkltkdtk!tk:k0-tQ
\ --
\ This code repeats that 'k' reads a character and 't' prints it.
\ Note that ':' (58) minus '0' (48) is '\n' (10).

\ The structure of the dictionary is sames as many Forth implementation.
\ +------+----------+---------+------------+---------------+
\ | link | len+flag | name... | padding... | code field ...|
\ +------+----------+---------+------------+---------------+
\ - link pointer to the previous entry (CELL byte)
\ - length of the name (7 bits)
\ - immediate flag (1 bit)
\ - characters of the name (N bits)
\ - padding to align CELL boundary if necessary.
\ - codewords and datawords (CELL-bye aligned)

\ The code group at the beginning of this file
\ defines ' ' and '\n' as no-op operation and
\ '\' to read following characters until '\n'.
\ Since I couldn't write a comment at the beginning,
\ I repost the definition of '\' for explanation.
\ --
\ h@                            ( save addr of new entry )
\ l@ h@!h@C+h!                  ( set link pointer. *here++ = latest )
\ k1k0-h@$ k\h@k1k0-+$ h@C+h!   ( write the name '\' and its length )
\ i       h@!h@C+h!             ( docol )
\ kkf     h@!h@C+h!             ( key )
\ kLf     h@!h@C+h!             ( lit )
\ k:k0-   h@!h@C+h!             ( '\n' )
\ k=f     h@!h@C+h!             ( = )
\ kJf     h@!h@C+h!             ( branch )
\ k0k5-C* h@!h@C+h!             ( -5*CELL )
\ kef     h@!h@C+h!             ( exit )
\ l!                            ( set latest to this new entry. )
\ --

\ That's all for the brief explanation. Let's restart bootstrap!

\ The COMMA operator
\ ',' ( a -- )  Store a to 'here' and increment 'here' CELL bytes.
h@l@ h@!h@C+h! k1k0-h@$ k,h@k1k0-+$ h@C+h!
    i   h@!h@C+h!   \ docol
    \ store 'a' to here
    khf h@!h@C+h!
    k@f h@!h@C+h!
    k!f h@!h@C+h!
    \ here <- here + CELL
    khf h@!h@C+h!
    k@f h@!h@C+h!
    kCf h@!h@C+h!
    k+f h@!h@C+h!
    khf h@!h@C+h!
    k!f h@!h@C+h!
    \ exit
    kef h@!h@C+h!
l!

\ TICK-like operator
\ '\'' ( "c" -- xt )    Get execution token of following character
\ NB: This definition is different from the usual definition of tick
\ because it does not skip leading spaces and can read only a single
\ character. It will be redefined in later stage.
h@l@, k1k0-h@$ k'h@k1k0-+$ h@C+h!
    i, kkf, kff, kef,
l!

\ Utility for defining a word
\ 'c' ( "c" -- w )
\ Read character, create new word then push its address.
\ 'latest' will not be updated.
h@l@, k1k0-h@$ kch@k1k0-+$ h@C+h!
    i, 'h, '@, 'l, '@, ',,
    'L, k1k0-, 'h, '@, '$,
    'k, 'h, '@, 'L, k1k0-, '+, '$,
    'h, '@, 'C, '+, 'h, '!,
'e, l!

\ '_' ( a -- ) DROP
c_ i, 'd, 'C, '+, 'D, 'e, l!

\ '#' ( a -- a a )  DUP
c# i, 'd, '@, 'e, l!



\ Implementations of TOR and FROMR are a bit tricky.
\ Since return-address will be placed at the top of return stack,
\ the code in the body of these function have to manipulate
\ 2nd element of the stack.

\ '{' ( a -- R:a ) TOR
\ Move value from data stack to return stack.
c{ i,
    'r, 'r, '@,     \ ( a rsp ret )
    'r, 'C, '-, '#, \ ( a rsp ret rsp-1 rsp-1 )
    'R,             \ ( a rsp+1 ret rsp ) extend return stack
    '!,             \ ( a rsp+1 ) store return address to the top
    '!,             \ store a to the 2nd
'e, l!

\ '}' ( R:a -- a ) FROMR
\ Move value from return stack to data stack.
c} i,
    'r, 'C, '+, '@, \ ( a ) load 2nd value
    'r, '@,         \ ( a ret ) load return addr
    'r, 'C, '+, '#, \ ( a ret rsp+1 rsp+1 )
    'R,             \ ( a ret rsp ) reduce return stack
    '!,             \ ( a , R:ret ) store return addr to top of return stack
'e, l!

\ 'o' ( a b -- a b a ) OVER
co i, 'd, 'C, '+, '@, 'e, l!

\ '~' ( a b -- b a ) SWAP
c~ i,
    'o,             \ ( a b a )
    '{,             \ ( a b , R:a )
    'd, 'C, '+,     \ ( a b sp+1 , R:a )
    '!,             \ ( b , R:a )
    '},             \ ( b a )
'e, l!

\ 'B' ( c -- ) C-COMMA
\ Store byte 'c' to here and increment it
cB i, 'h, '@, '$, 'h, '@, 'L, k1k0-, '+, 'h, '!, 'e, l!

\ 'm' ( c-addr u -- ) CMOVE,
\ Copy u bytes from c-addr to here,
\ increment here u bytes.
cm i,
\ <loop>
    '#, 'J, k>k0-C*,        \ goto <exit> if u=0
        '{,                 \ preserve u
        '#, '?, 'B,         \ copy byte
        'L, k1k0-, '+,      \ increment c-addr
        '}, 'L, k1k0-, '-,  \ decrement u
        'j, k0k?-C*,        \ goto <loop>
\ <exit>
    '_, '_,
'e, l!

\ 'a' ( c-addr -- a-addr ) ALIGNED
\ Round up 'a' to a multiple of CELL
ca i,
    'L, Ck1k0--, '+,    \ ( a+CELL-1 )
    'L, k0k0-C-,        \ ( a+CELL-1 ~(CELL-1) )
    '&,
'e, l!

\ 'A' ( -- ) ALIGN
\ Round up 'here' to a multiple of CELL
cA i, 'h, '@, 'a, 'h, '!, 'e, l!

\ 'E' ( c-addr1 u1 c-addr2 u2 -- n ) STR=
\ Compare two strings.
\ Return 1 if they are same 0 otherwise.
cE i,
    '{, '~, '},                 \ ( c-addr1 c-addr2 u1 u2 )
    'o, '=, 'J, kVk0-C*,        \ jump to <not_equal> if u1!=u2
\ <loop>
        \ ( c-addr1 c-addr2 u )
        '#, 'J, kMk0-C*,        \ jump to <equal> if u==0
            '{,                 \ preserve u
            'o, '?,             \ ( c-addr1 c-addr2 c1 )
            'o, '?,             \ ( c-addr1 c-addr2 c1 c2 )
            '},                 \ ( c-addr1 c-addr2 c1 c2 u ) restore u
            '~, '{, '~, '},     \ ( c-addr1 c-addr2 u c1 c2 )
            '=, 'J, kFk0-C*,    \ jump to <not_equal> if c1!=c2
            '{, '{,             \ ( c-addr1 , R:u c-addr2 )
            'L, k1k0-, '+,      \ increment c-addr1
            '}, 'L, k1k0-, '+,  \ increment c-addrr2
            '}, 'L, k1k0-, '-,  \ decrement u
            'j, k0kN-C*,        \ jump to <loop>
\ <equal>
    '_, '_, '_, 'L, k1k0-, 'e,
\ <not_equal>
    '_, '_, '_, 'L, k0k0-, 'e,
l!

\ 's' ( c -- n)
\ Return 1 if c==' ' or c=='\n', 0 otherwise.
cs i, '#, 'L, k , '=, '~, 'L, k:k0-, '=, '|, 'e, l!

\ 'W' ( "name" -- c-addr u )
\ Skip leading spaces (' ' and '\n'),
\ Read name, then return its address and length.
\ The maximum length of the name is 127. The behavior is undefined
\ when the name exceeds 127 characters,
\ Note that it returns the address of statically allocated buffer,
\ so the content will be overwritten each time 'w' executed.

\ Allocate buffer of 127 bytes or more,
\ push the address for compilation of 'w'
h@ # kOk0++ h! A
cW~
i,
    \ skip leading spaces
    'k, '#, 's, 'J, k4k0-C*, '_, 'j, k0k7-C*,
    \ p=address of buffer
    'L, #, '~,
\ <loop>
    \ ( p c )
    'o, '$,                     \ store c to p
    'L, k1k0-, '+,              \ increment p
    'k, '#, 's, 'J, k0k9-C*,    \ goto <loop> if c is not space
    '_, 'L, ,                   \ ( p buf )
    '~, 'o, '-,                 \ ( buf p-buf )
'e, l!

\ 'F' ( c-addr u -- w )
\ Lookup multi-character word from dictionary.
\ Return 0 if the word is not found.
cF i,
    'l, '@,
\ <loop> ( addr u it )
    '#, 'J, kIk0-C*,        \ goto <exit> if it=NULL
        '{, 'o, 'o, 'r, '@, '~, '{, '~, '}, '},
        \ ( addr u it addr u it )
        '#, 'L, Ck1k0-+, '+,        \ address of name
        '~, 'C, '+, '?,             \ length+flag
        'L, kOk0+, '&,              \ take length (lower 7-bits)
        \ ( addr1 u1 it addr1 u1 addr2 u2 )
        'E, 'J, k3k0-C*,            \ goto <1> if different name
        'j, k4k0-C*,                \ goto <exit>
\ <1>
        '@, 'j, k0kO-C*,            \ load link, goto <loop>
\ <exit>
    '{, '_, '_, '}, \ Drop addr u return it
'e, l!

\ 'G' ( w -- xt )
\ Get CFA of the word
cG i,
    'C, '+, '#, '?, \ ( addr len+flag )
    'L, kOk0+, '&,  \ take length
    '+,             \ add length to the addr
    'L, k1k0-, '+,  \ add 1 to the addr (1byte for len+field)
    'a,             \ align
'e, l!

\ 'M' ( -- c-addr)
\ The state variable
\ 0: immediate mode
\ 1: compile mode
h@ k0k0-,   \ allocate 1 cell and fill 0
cM~ i, 'L, , 'e, l!

\ 'I'
\ The 2nd Stage Interpreter
cI i,
\ <loop>
    'W,                 \ read name from input
    'F,                 \ find word
    'M, '@,             \ read state
    'J, kAk0-C*,        \ goto <immediate> if state=0
\ <compile>
        '#, 'C, '+, '?, \ ( w len+flag )
        'L, k@k@+, '&,  \ test immediate bit
        'L, k0k0-, '=,
        'J, k5k0-C*,    \ goto <immediate> if immediate-bit=1
        'G, ',,         \ compile CFA
        'j, k0kE-C*,    \ goto <loop>
\ <immediate>
        'G, 'x,         \ execute CFA
        'j, k0kI-C*,    \ goto <loop>
l!

I \ Enter 2nd Stage

\ === 2nd Stage Interpreter ===

r C + R     \ Drop 1st stage interpreter from call stack

\ '\'' ( "name" -- xt )
\ Redefine existing '\'' which uses 'k' and 'f'
\ to use 'W' and 'F'.
c ' i , ' W , ' F , ' G , ' e , l !

\ [ immediate ( -- )
\ Switch to immediate mode
c [ i , ' L , k 0 k 0 - , ' M , ' ! , ' e , l !
\ Set immediate-bit of [
l @ C + # { ? k @ k @ + | } $

\ ] ( -- )
\ Switch to compile mode
c ] i , ' L , k 1 k 0 - , ' M , ' ! , ' e , l !

\ : ( "name" -- R:w ) COLON
\ Read name, create word, push it to return stack,
\ compile 'docol' and enter compile mode.
c : i ,
    \ save the address of the word to
    \ **2nd position** of return stack.
    \ Same reasons for TOR and FROMR
    ' h , ' @ , ' # ,
    ' } , ' ~ , ' { , ' { ,
    ' l , ' @ , ' , ,   \ fill link
    ' W ,               \ read name ( addr len )
    ' # , ' B ,         \ fill length ( addr len )
    ' m ,               \ fill name
    ' A ,               \ align here
    ' i , ' , ,         \ compile docol
    ' ] ,               \ enter compile mode
' e , l !

\ ; immediate ( R:w -- ) SEMICOLON
\ Compile 'exit', add w to dictionary and  enter immediate mode.
c ; i ,
    ' L , ' e , ' , ,   \ compile exit

    \ pick 2nd element of return stack
     ' } , ' } , ' ~ , ' { ,
     ' l , ' ! ,         \ update latest
     ' [ ,               \ enter immediate mode
' e , l !
\ Set immediate-bit of ';'
l @ C + # { ? k @ k @ + | } $

: immediate-bit [ ' L , k @ k @ + , ] ;
: length-mask   [ ' L , k O k 0 + , ] ;

\ ( "name" -- )
: set-immediate
    W F C + # { ? immediate-bit | } $
;

\ Set immediate-bit of single-line comment word \
\ so that we can write comments in compile-mode.
set-immediate \
Q
