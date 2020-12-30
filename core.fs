h@l@h@!h@C+h!k1k0-h@$k
h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!
h@l@h@!h@C+h!k1k0-h@$k h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!

h@l@ h@!h@C+h!
k1k0-h@$ k\h@k1k0-+$ h@C+h!
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
h@l@h@! h@C+h!
k1k0-h@$ k,h@k1k0-+$ h@C+h!
    i   h@!h@C+h!   \ docol
    khf h@!h@C+h!
    k@f h@!h@C+h!
    k!f h@!h@C+h!   \ store 'a' to here
    khf h@!h@C+h!
    k@f h@!h@C+h!
    kCf h@!h@C+h!
    k+f h@!h@C+h!   \ compute here + CELL
    khf h@!h@C+h!
    k!f h@!h@C+h!   \ here <- here + CELL
    kef h@!h@C+h!   \ exit
l!

Q
