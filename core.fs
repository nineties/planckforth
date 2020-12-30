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
\ - length of the name (5 bits)
\ - immediate flag (1 bit)
\ - smuege flag (1 bit)
\ - unused bit (1 bit)
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
    'h, '@, 'C, '+, 'h, '!, 'e,
l!

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
c{  i,
    'r, 'r, '@,     \ ( a rsp ret )
    'r, 'C, '-, '#, \ ( a rsp ret rsp-1 rsp-1 )
    'R,             \ ( a rsp+1 ret rsp ) extend return stack
    '!,             \ ( a rsp+1 ) store return address to the top
    '!,             \ store a to the 2nd
    'e,
l!

\ '}' ( R:a -- a ) FROMR
\ Move value from return stack to data stack.
c}  i,
    'r, 'C, '+, '@, \ ( a ) load 2nd value
    'r, '@,         \ ( a ret ) load return addr
    'r, 'C, '+, '#, \ ( a ret rsp+1 rsp+1 )
    'R,             \ ( a ret rsp ) reduce return stack
    '!,             \ ( a , R:ret ) store return addr to top of return stack
    'e,
l!

\ 'o' ( a b -- a b a ) OVER
co i, 'd, 'C, '+, '@, 'e, l!

\ '~' ( a b -- b a ) SWAP
c~ i,
    'o,             \ ( a b a )
    '{,             \ ( a b , R:a )
    'd, 'C, '+,     \ ( a b sp+1 , R:a )
    '!,             \ ( b , R:a )
    '},             \ ( b a )
    'e,
l!


kA kB ~ t t \ AB
k
t

kA kB o t t t
k
t
Q
