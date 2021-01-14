h@l@h@!h@C+h!k1k0-h@$k:k0-h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!
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
\ Copyright (C) 2021 nineties

\ This project aims to bootstrap a Forth interpreter
\ from hand-written tiny ELF binary.

\ In the 1st stage, only single character words are registered
\ in the dictionary.
\ List of builtin words:
\ 'Q' ( n -- )          Exit the process
\ 'C' ( -- n )          The size of Cells
\ 'h' ( -- a-addr )     The address of 'here' cell
\ 'l' ( -- a-addr )     The address of 'latest' cell
\ 'k' ( -- c )          Read character
\ 't' ( c -- )          Print character
\ 'j' ( -- )            Unconditional branch
\ 'J' ( n -- )          Jump if a == 0
\ 'f' ( c -- xt )       Get execution token of c
\ 'x' ( xt -- ... )     Run the execution token
\ '@' ( a-addr -- w )   Load value from addr
\ '!' ( w a-addr -- )   Store value to addr
\ '?' ( c-addr -- c )   Load byte from addr
\ '$' ( c c-addr -- )   Store byte to addr
\ 'd' ( -- a-addr )     Get data stack pointer
\ 'D' ( a-addr -- )     Set data stack pointer
\ 'r' ( -- a-addr )     Get return stack pointer
\ 'R' ( a-addr -- )     Set return stack pointer
\ 'i' ( -- a-addr )     Get the interpreter function
\ 'e' ( -- )            Exit current function
\ 'L' ( -- u )          Load immediate
\ 'S' ( -- c-addr )     Load string literal
\ '+' ( a b -- c )      c = (a + b)
\ '-' ( a b -- c )      c = (a - b)
\ '*' ( a b -- c )      c = (a * b)
\ '/' ( a b -- c )      c = (a / b)
\ '%' ( a b -- c )      c = (a % b)
\ '&' ( a b -- c )      c = (a & b)
\ '|' ( a b -- c )      c = (a | b)
\ '^' ( a b -- c )      c = (a ^ b)
\ '<' ( a b -- c )      c = (a < b)
\ 'u' ( a b -- c )      c = (a unsigned< b)
\ '=' ( a b -- c )      c = (a == b)
\ '{' ( a b -- c )      c = a << b (logical)
\ '}' ( a b -- c )      c = a >> b (logical)
\ ')' ( a b -- c )      c = a >> b (arithmetic)
\ 'v' ( -- a-addr u )   argv and argc
\ 'V' ( -- c-addr )     Runtime information string

\ The 1st stage interpreter repeats execution of k, f and x.
\ There following line is an example program of planckforth
\ which prints "Hello World!\n"
\ --
\ kHtketkltkltkotk tkWtkotkrtkltkdtk!tk:k0-tQ
\ --
\ This code repeats that 'k' reads a character and 't' prints it.
\ Note that ':' (58) minus '0' (48) is '\n' (10).

\ The structure of the dictionary.
\ +------+----------+---------+------------+---------------+
\ | link | len+flag | name... | padding... | code field ...|
\ +------+----------+---------+------------+---------------+
\ - link pointer to the previous entry (CELL byte)
\ - length of the name (6 bits)
\ - smudge bit (1 bit)
\ - immediate bit (1 bit)
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
    'L, k1k0-, 'h, '@, '$,                  \ fill 1
    'k, 'h, '@, 'L, k1k0-, '+, '$,          \ fill "c"
    'L, k0k0-, 'h, '@, 'L, k2k0-, '+, '$,   \ fill "\0"
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
\ Round up to a nearlest multiple of CELL
ca i,
    'L, Ck1k0--, '+,    \ ( a+CELL-1 )
    'L, k0k0-C-,        \ ( a+CELL-1 ~(CELL-1) )
    '&,
'e, l!

\ 'A' ( -- ) ALIGN
\ Round up 'here' to a nearlest multiple of CELL
cA i, 'h, '@, 'a, 'h, '!, 'e, l!

\ 'E' ( c-addr1 c-addr2 -- flag ) STR=
\ Compate null-terminated strings.
\ Return 1 if they are same 0 otherwise.
cE i,
\ <loop>
    'o, '?, 'o, '?,         \ ( c-addr1 c-addr2 c1 c2 )
    'o, '=, 'J, k=k0-C*,    \ goto <not_equal> if c1<>c2
    'J, kAk0-C*,            \ goto <equal> if c1==0
    'L, k1k0-, '+, '~,      \ increment c-addr2
    'L, k1k0-, '+, '~,      \ increment c-addr1
    'j, k0kC-C*,            \ goto <loop>
\ <not_equal>
    '_, '_, '_, 'L, k0k0-, 'e,
\ <equal>
    '_, '_, 'L, k1k0-, 'e,
l!

\ 'z' ( c-addr -- u ) STRLEN
\ Calculate length of string
cz i,
    'L, k0k0-,  \ 0
\ <loop>
    'o, '?, 'J, k;k0-C*,    \ goto <exit> if '\0'
    'L, k1k0-, '+,  '~,     \ increment u
    'L, k1k0-, '+,  '~,     \ increment c-addr
    'j, k0k=-C*,            \ goto <loop>
\ <exit>
    '~, '_, 'e,
l!

\ 's' ( c -- n)
\ Return 1 if c==' ' or c=='\n', 0 otherwise.
cs i, '#, 'L, k , '=, '~, 'L, k:k0-, '=, '|, 'e, l!

\ 'W' ( "name" -- c-addr )
\ Skip leading spaces (' ' and '\n'),
\ Read name, then return its address and length.
\ The maximum length of the name is 63. The behavior is undefined
\ when the name exceeds 63 characters,
\ Note that it returns the address of statically allocated buffer,
\ so the content will be overwritten each time 'w' executed.

\ Allocate buffer of 63+1 bytes or more,
\ push the address for compilation of 'w'
h@ # kpk0-+ h! A
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
    '_,
    'L, k0k0-, 'o, '$,           \ fill \0
    '_, 'L, ,                    \ return buf
'e, l!

\ 'F' ( c-addr -- w )
\ Lookup multi-character word from dictionary.
\ Return 0 if the word is not found.
\ Entries with smudge-bit=1 are ignored.
cF i,
    'l, '@,
\ <loop> ( addr it )
    '#, 'J, kEk0-C*,        \ goto <exit> if it=NULL
        '#, 'C, '+, '?,     \ ( addr it len+flag )
        'L, k@, '&,         \ test smudge-bit of it
        'J, k4k0-C*,
\ <1>
            \ smudge-bit=1
            '@,             \ load link
            'j, k0k>-C*,    \ goto <loop>
\ <2>
            \ smudge-bit=0
            'o, 'o,                     \ ( addr it addr it )
            'L, Ck1k0-+, '+,        \ address of name
            \ ( addr1 it addr1 addr2 )
            'E, 'J, k0k:-C*,            \ goto <1> if different name
\ <exit>
    '{, '_, '}, \ Drop addr, return it
'e, l!

\ 'G' ( w -- xt )
\ Get CFA of the word
cG i,
    'C, '+, '#, '?, \ ( addr len+flag )
    'L, kok0-, '&,  \ take length
    '+,             \ add length to the addr
    'L, k2k0-, '+,  \ add 2 to the addr (len+field and \0)
    'a,             \ align
'e, l!

\ 'M' ( -- a-addr)
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
        'G, ',,         \ compile
        'j, k0kE-C*,    \ goto <loop>
\ <immediate>
        'G, 'x,         \ execute
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

\ : ( "name" -- ) COLON
\ Read name, create word with smudge=1,
\ compile 'docol' and enter compile mode.
c : i ,
    ' A ,                   \ align here
    ' h , ' @ ,
    ' l , ' @ , ' , ,       \ fill link
    ' l , ' ! ,             \ update latest
    ' W ,                   \ read name ( addr )
    ' # , ' z , ' # ,       \ ( addr len len )
    ' L , k @ , ' | ,       \ set smudge-bit
    ' B ,                   \ fill length + smudge-bit
    ' m ,                   \ fill name
    ' L , k 0 k 0 - , ' B , \ fill \0
    ' A ,                   \ align here
    ' i , ' , ,             \ compile docol
    ' ] ,                   \ enter compile mode
' e , l !

\ ; ( -- ) SEMICOLON
\ Compile 'exit', unsmudge latest, and enter immediate mode.
c ; i ,
    ' A ,               \ align here
    ' L , ' e , ' , ,   \ compile exit
    ' l , ' @ ,
    ' C , ' + , ' # , ' ? ,
    ' L , k [ k d + ,   \ 0xbf
    ' & , ' ~ , ' $ ,   \ unsmudge
    ' [ ,               \ enter immediate mode
' e , l !
\ Set immediate-bit of ';'
l @ C + # { ? k @ k @ + | } $

: immediate-bit [ ' L , k @ k @ + , ] ; \ 0x80
: smudge-bit    [ ' L , k @ , ] ;       \ 0x40
: length-mask   [ ' L , k o k 0 - , ] ; \ 0x3f

\ ( "name" -- )
: set-immediate
    W F C + # { ? immediate-bit | } $
;

\ Set immediate-bit of single-line comment word \
\ so that we can write comments in compile-mode.
set-immediate \

\ Set immediate-bit of 'latest'
: immediate
    l @ C + # { ? immediate-bit | } $
;

: alias-builtin \ ( "name-new" "name-old" -- )
    \ Create new word "name-new".
    \ Copy code pointer of builtin word "name-old" to
    \ the new word "name-new".
    \ "name-old" must not be a FORTH word.
    A h @ l @ , l !         \ fill link, update latest
    W # z # B m             \ fill length and chars of "name-new"
    [ ' L , k 0 k 0 - , ] B \ fill \0
    A
    W F G @ ,               \ fill code-pointer of "name-old"
;

\ Add new names to builtin primities.
\ Instead of defining as a new FORTH word like shown below,
\ the aliases ared created by copying their code-pointer.
\ : new-name old-name ;
\ Primitive operators which manipulate program counter and return stack
\ can not be defined as a FORTH word.

alias-builtin quit      Q
alias-builtin cell      C
alias-builtin &here     h
alias-builtin &latest   l
alias-builtin emit      t
alias-builtin branch    j
alias-builtin 0branch   J
alias-builtin execute   x
alias-builtin c@        ?
alias-builtin c!        $
alias-builtin sp@       d
alias-builtin sp!       D
alias-builtin rp@       r
alias-builtin rp!       R
alias-builtin docol     i
alias-builtin exit      e
alias-builtin lit       L
alias-builtin litstring S
alias-builtin /mod      /
alias-builtin and       &
alias-builtin or        |
alias-builtin xor       ^
alias-builtin u<        u
alias-builtin lshift    (
alias-builtin rshift    )
alias-builtin arshift   %
alias-builtin runtime-info_ V

: bye [ ' L , k 0 k 0 - , ] quit ;

\ Rename existing FORTH words
: >cfa      G ;
: c,        B ;
: memcpy,   m ;
: strlen    z ;
: streq     E ;
: state     M ;
: aligned   a ;
: align     A ;

: here      &here @ ;
: latest    &latest @ ;
: >dfa >cfa cell + ;

\ === Stub Functions ===
\ Use 1-step indirect reference so that we can replace
\ the runtime later.

: allot-cell &here @ # cell + &here ! ;

alias-builtin key-old   k

allot-cell : &key  [ ' L , , ] ;
allot-cell : &key! [ ' L , , ] ;

: key   &key @ execute ;    \ ( -- c ) Push -1 at EOF
' key-old &key !

: key!  &key! @ execute ;   \ ( -- c ) Throw exception at EOF
' key-old &key! !

allot-cell : &word [ ' L , , ] ;
: word  &word @ execute ;   \ ( "name" -- c-addr e )
: stub-word W [ ' L , k 0 k 0 - , ] ;
' stub-word &word !

allot-cell : &word! [ ' L , , ] ;
: word! &word! @ execute ;  \ ( "name" -- c-addr ) Throw exception at error
' W &word! !

allot-cell : &find [ ' L , , ] ;  \ ( c-addr -- nt|0 )
allot-cell : &find! [ ' L , , ] ; \ ( c-addr -- nt ) Throw exception at error

: find  &find @ execute ;
: find! &find! @ execute ;
' F &find !
' F &find! !

: ' word! find! >cfa ;

\ === Compilers ===

\ compile: ( n -- )
\ runtime: ( -- n )
: literal
    lit lit ,   \ compile lit
    ,           \ compile n
; immediate

\ compile: ( "name" -- )
\ '[compile] word' compiles word *now* even if it is immediate
: [compile]
    ' ,
; immediate

\ ( xt -- )
\ postpone compilation of xt
: (compile)
    [compile] literal   \ compile 'literal'
    [ ' , ] literal ,   \ compile ,
;

\ compile: ( "name" -- )
\ 'compile word' compiles word *later* even if it is immediate
: compile
    ' (compile)
; immediate

\ ( -- xt )
: :noname
    align
    here
    [ docol ] literal , \ compile docol
    ]                   \ enter compile mode
;

\ ( "name" -- xt )
\ compile time tick
: [']
    '                   \ read name and get xt
    [compile] literal   \ call literal
; immediate

\ === Constants ===

\ Since we don't have integer literals yet,
\ define small integer words for convenience
\ and readability.
: 0     [ key 0 key 0 - ] literal ;
: 1     [ key 1 key 0 - ] literal ;
: 2     [ key 2 key 0 - ] literal ;
: 3     [ key 3 key 0 - ] literal ;
: 4     [ key 4 key 0 - ] literal ;
: 5     [ key 5 key 0 - ] literal ;
: 10    [ key : key 0 - ] literal ;
: 16    [ key @ key 0 - ] literal ;
: -1    [ key 0 key 1 - ] literal ;

: true 1 ;
: false 0 ;

\ === Address Arithmetic ===

: cell+ cell + ;
: cell- cell - ;
: cells cell * ;
: char+ 1 + ;
: char- 1 - ;
: chars ;

\ === Stack Manipulation ===

: drop  sp@ cell+ sp! ;     \ ( w -- )
: dup   sp@ @ ;             \ ( w -- w w )

: >r rp@ rp@ @ rp@ cell - dup rp! ! ! ;         \ ( w -- R:w )
: r> rp@ cell + @ rp@ @ rp@  cell + dup rp! ! ; \ ( R:w -- w)
: r@ rp@ cell + @ ; \ ( -- w, R: w -- w )

: swap  sp@ cell + dup @ >r ! r> ;  \ ( a b -- b a )
: rot   >r swap r> swap ;           \ ( a b c -- b c a )
: -rot  swap >r swap r> ;           \ ( a b c -- c a b )
: nip   swap drop ;                 \ ( a b -- b )
: over  >r dup r> swap ;            \ ( a b -- a b a )
: tuck  dup -rot ;                  \ ( a b -- b a b )
: pick  cells sp@ + cell + @ ;      \ ( wu ... x0 u -- xu ... x0 xu )

: 2drop drop drop ;                 \ ( a b -- )
: 3drop 2drop drop ;                \ ( a b c -- )
: 2dup  over over ;                 \ ( a b -- a b a b )
: 3dup  2 pick 2 pick 2 pick ;      \ ( a b c -- a b c a b c )
: 2swap >r -rot r> -rot ;           \ ( a b c d -- c d a b )
: 2nip  2swap 2drop ;               \ ( a b c d -- c d )
: 2over 3 pick 3 pick ;             \ ( a b c d -- a b c d a b )
: 2tuck 2swap 2over ;               \ ( a b c d -- c d a b c d )
: 2rot  >r >r 2swap r> r> 2swap ;   \ ( a b c d e f -- c d e f a b )
: -2rot 2swap >r >r 2swap r> r> ;   \ ( a b c d e f -- e f a b c d )

: rdrop r> rp@ ! ;  \ ( R:w -- )

\ ( R xu ... x0 u -- xu ... x0 xu )
: rpick
    cells rp@ + cell + @
;

\ ( -- a-addr )
\ The bottom address of stacks.
\ sp@ and rp@ points bottom if runtime so far is correct.
: sp0 [ sp@ ] literal ;
: rp0 [ rp@ ] literal ;

\ === Integer Arithmetic ===

: 1+ 1 + ;
: 1- 1 - ;

: /     /mod swap drop ;
: mod   /mod drop ;

\ ( n -- -n )
: negate 0 swap - ;

\ ( n1 -- n2 )
: not false = ;

\ ( n1 -- n2 )
\ bitwise invert
: invert -1 xor ;

: >     swap < ;
: <=    > not ;
: >=    < not ;
: u>    swap u< ;
: u<=   u> not ;
: u>=   u< not ;
: <>    = not ;

: 0=    0 = ;
: 0<>   0 <> ;
: 0<    0 < ;
: 0>    0 > ;
: 0<=   0 <= ;
: 0>=   0 >= ;

\ ( x a b -- f )
\ Returns a <= x & x < b if a <= b.
\ It is equivalent to x-a u< b-a. See chapter 4 of
\ Hacker's delight.
: within over - >r - r> u< ;

\ arithmetic shift
: 2* 1 lshift ;
: 2/ 1 arshift ;

\ === Conditional Branch ===
\ <condition> if <if-true> then
\ <condition> if <if-true> else <if-false> then
\ <condition> unless <if-false> then
\ <condition> unless <if-false> else <if-true> then

\ compile: ( -- orig )
\ runtime: ( n -- )
: if
    compile 0branch
    here 0 ,    \ save location of offset, fill dummy
; immediate

\ compile: ( orig -- )
\ runtime: ( -- )
: then
    here        \ ( orig dest )
    over -      \ ( orig offset )
    swap !      \ fill offset to orig
; immediate

\ compile: ( orig1 -- orig2 )
\ runtime: ( -- )
: else
    compile branch
    here 0 ,    \ save location of offset, fill dummy
    swap
    \ fill offset, here-orig1, to orig1
    here
    over -
    swap !
; immediate

\ compile: ( -- orig )
\ runtime: ( n -- )
: unless
    compile not
    [compile] if
; immediate

\ ( n -- n n | n )
\ duplicate if n<>0
: ?dup dup if dup then ;

\ === Loops ===
\ begin <body> <condition> until
\ begin <body> again
\ begin <condition> while <body> repeat

\ compile: ( -- dest )
\ runtime: ( -- )
: begin
    here        \ save location
; immediate

\ compile: ( dest -- )
\ runtime: ( n -- )
: until
    compile 0branch
    here - ,    \ fill offset
; immediate

\ compile: ( dest -- )
\ runtime: ( -- )
: again
    compile branch
    here - ,    \ fill offset
; immediate

\ compile: ( dest -- orig dest )
\ runtime: ( n -- )
\ dest=location of begin
\ orig=location of while
: while
    compile 0branch
    here swap
    0 ,        \ save location, fill dummy
; immediate

\ compile: ( orig dest -- )
\ runtime: ( -- )
\ dest=location of begin
\ orig=location of while
: repeat
    compile branch
    here - ,                \ fill offset from here to begin
    here over - swap !      \ backfill offset from while to here
; immediate

\ === Recursive Call ===

\ recursive call.
\ compiles xt of current definition
: recurse
    latest >cfa ,
; immediate

\ === Case ===

\ ---
\ <value> case
\   <value1> of <case1> endof
\   <value2> of <case2> endof
\   ...
\   <default case>
\ endcase
\ ---
\ This is equivalent to
\ ---
\ <value>
\ <value1> over = if drop <case1> else
\ <value2> over = if drop <case2> else
\ ...
\ <default case>
\ then ... then then
\ ---


\ compile: ( -- 0 )
\ runtime: ( n -- )
: case
    0       \ push 0 to indicate there is no more case
; immediate

\ compile: ( -- orig )
: of
    compile over
    compile =
    [compile] if
    compile drop
; immediate

\ <value> a b rangeof <body> endof
\ Execute <body> when
\ a <= <value> and <value> <= b
: rangeof
    compile 2
    compile pick
    compile >=
    compile swap
    compile 2
    compile pick
    compile <=
    compile and
    [compile] if
    compile drop
; immediate

\ compile: ( orig1 -- orig2 )
: endof
    [compile] else
; immediate

: endcase
    compile drop
    begin ?dup while
        [compile] then
    repeat
; immediate

\ === Integer Arithmetic (that require control flow words) ===
\ ( a b -- c )
: max 2dup > if drop else nip then ;
: min 2dup < if drop else nip then ;

: abs dup 0< if negate then ;

\ === Multiline Comment ===

: '('   [ key ( ] literal ;
: ')'   [ key ) ] literal ;

: (
    1   \ depth counter
    begin ?dup while
        key! case
        '(' of 1+ endof \ increment depth
        ')' of 1- endof \ decrement depth
        endcase
    repeat
; immediate

(
    Now we can use multiline comment with ( nests. )
)

( === Memory Operation === )

: +! ( n a-addr -- ) tuck @ + swap ! ;
: -! ( n a-addr -- ) tuck @ swap - swap ! ;

\ allocate n bytes
: allot ( n -- ) &here +!  ;

( === create and does> === )

\ no-operation
: nop ;

\ ( "name" -- )
\ Read name and create new dictionary entry.
\ When the word is executed, it pushs value of here
\ at the end of the entry.
: create
    align
    latest ,                    \ fill link
    here cell- &latest !        \ update latest
    word! dup strlen
    dup c, memcpy, 0 c, align    \ fill length, name and \0
    docol ,                     \ compile docol
    ['] lit ,
    here 3 cells + ,            \ compile the address
    ['] nop ,                   \ does>, if any, will fill this cell
    ['] exit ,                  \ compile exit
;

: >body ( xt -- a-addr )
    5 cells +
;

: (does>)
    latest >cfa
    3 cells + ! \ replace nop
;

: does>
    align
    0 [compile] literal \ literal for xt
    here cell-          \ save addr of xt

    \ replace nop with xt at runtime
    compile (does>)

    [compile] ; \ finish compilation of initialization part
    :noname     \ start compilation of does> part
    swap !      \ backfill xt to the operand of literal
; immediate

( === Variable and Constant === )

\ ( "name" -- )
: variable create 0 , ;

\ ( n "name" -- )
: constant create , does> @ ;

( === Throw and Catch === )

\ 'xt catch' saves data stack pointer and a marker
\ to indicate where to return on return stack
\ then execute 'xt'.
\ When 'n throw' is executed, the catch statement returns
\ 'n'. If no throw is executed, returns 0.

\ At the beginning of execution of 'xt', return stack
\ contains following information.
\ +-------------------------+
\ | original return address |
\ | saved stack pointer     |
\ | exception marker        | <- top of return stack
\ +-------------------------+
\ If no 'throw' is called, after execution of 'xt'
\ program goes to the exception-marker because it is
\ on the top of return stack.
\ The exception-marker drops 'saved stack pointer',
\ push 0 to indicate no error and return to the
\ 'original return address'.
\ When 'n throw' is called, it scans return stack
\ to find the exception-marker, restore return stack pointer
\ and data stack pointer, push error code, and returns to
\ the 'original return address'

create exception-marker
    ' rdrop ,   \ drop saved stack pointer
    0 literal   \ push 0 to indicate no-error
    ' exit ,

: catch ( xt -- n )
    sp@ cell+ >r            \ save stack pointer
    exception-marker >r     \ push exception marker
    execute
;

: success 0 ;

: throw ( w -- )
    ?dup unless exit then   \ do nothing if no error
    rp@
    begin
        dup rp0 cell- <     \ rp < rp0
    while
        dup @               \ load return stack entry
        exception-marker = if
            rp!     \ restore return stack pointer
            rdrop   \ drop exception marker

            \ Reserve enough working space of data stack since
            \ following code manipulates data stack pointer
            \ and write value to data stack directly via
            \ address.
            dup dup dup dup

            r>      \ original stack pointer
            \ ( n sp )
            cell-   \ allocate space for error code
            tuck !  \ store error code of top of stack
            sp!     \ restore data stack pointer
            exit
        then
        cell+
    repeat
    drop
;

( === Printing Numbers === )

\ Skip reading spaces, read characters and returns first character
: char      ( <spces>ccc -- c ) word! c@ ;

\ compile-time version of char
: [char]    ( compile: <spaces>ccc -- ; runtime: --- c )
    char
    [compile] literal
; immediate


: '\n' [ key : key 0 - ] literal ; \ neline (10)
: bl   [ key P key 0 - ] literal ; \ space (32)
: '"'  [char] "" ;

: cr    '\n' emit ;
: space bl emit ;


variable base   \ number base
: decimal   10 base ! ;
: hex       16 base ! ;

decimal \ set default to decimal

: '0' [char] 0 ;
: '9' [char] 9 ;
: 'a' [char] a ;
: 'x' [char] x ;
: 'z' [char] z ;
: 'A' [char] A ;
: 'Z' [char] Z ;
: '-' [char] - ;
: '&' [char] & ;
: '#' [char] # ;
: '%' [char] % ;
: '$' [char] $ ;
: '\'' [char] ' ;

\ Display unsigned integer u2 with number base u1.
: print-uint ( u1 u2 -- )
    over /mod   ( base mod quot )
    ?dup if
        >r over r> \ ( base mod base quot )
        recurse
    then
    dup 10 < if '0' + else 10 - 'a' + then emit
    drop
;

\ Display signed integer n with number base u.
: print-int ( u n -- )
    dup 0< if '-' emit negate then
    print-uint
;

\ Display unsigned integer followed by a space.
: u. ( u -- ) base @ swap print-uint space ;

\ Display n followed by a space.
: . ( n -- ) base @ swap print-int space ;

\ Display n as a signed decimal number followed by a space.
: dec. ( n -- ) 10 swap print-int space ;

\ Display u as an unsigned hex number prefixed with $
\ and followed by a space.
: hex. ( u -- ) '$' emit 16 swap print-uint space ;

\ Number of characters of u in 'base'
: uwidth ( u -- u )
    base @ /
    ?dup if recurse 1+ else 1 then
;

: spaces ( n -- )
    begin dup 0> while space 1- repeat drop
;

\ Display unsigned integer u right aligned in n characters.
: u.r ( u n -- )
    over uwidth
    - spaces base @ swap print-uint
;

\ Display signed integer n1 right aligned in n2 characters.
: .r ( n1 n2 -- )
    over 0>= if
        u.r
    else
        swap negate
        dup uwidth 1+
        rot swap - spaces
        '-' emit
        base @ swap print-uint
    then
;

( === Parsing Numbers === )

\ Parse string c-addr as an unsigned integer with base u
\ and return n. f represents the conversion is success or not.
: parse-uint ( u c-addr -- n f )
    0   \ accumulator
    begin over c@ while
        \ ( base addr acc )
        >r                  \ save acc
        dup c@ swap 1+ >r   \ load char, increment addr and save
        dup case
        '0' '9' rangeof '0' - endof
        'a' 'z' rangeof 'a' - 10 + endof
        'A' 'Z' rangeof 'A' - 10 + endof
            \ failed to convert
            2drop r> r> nip false
            exit
        endcase
        2dup
        \ ( base n base n )
        swap 0 swap
        \ ( base n n 0 base )
        within unless
            \ failed to convert
            2drop r> r> nip false
            exit
        then
        \ ( base addr n acc )
        r> swap r>
        3 pick * +
    repeat
    \ success
    nip nip true
;

: parse-int ( u c-addr -- n f )
    dup c@ '-' = if
        1+ parse-uint swap negate swap
    else
        parse-uint
    then
;

\ Parse string as number.
\ This function interprets prefixes that specifies number base.
: >number ( c-addr -- n f )
    dup c@ unless
        drop
        0 false
        exit
    then
    dup c@ case
    '-' of
        1+
        recurse if
            negate true
        else
            false
        then
    endof
    '&' of 1+ 10 swap parse-int endof
    '#' of 1+ 10 swap parse-int endof
    '$' of 1+ 16 swap parse-int endof
    '%' of 1+ 2 swap parse-int endof
    '0' of
        \ hexadecimal
        \ ( addr )
        1+
        dup c@ unless
            drop 0 true exit
        then
        dup c@ 'x' = if
            1+ 16 swap parse-uint exit
        then
        drop 0 false exit
    endof
    '\'' of
        \ character code
        \ ( addr )
        1+
        dup c@ unless
            drop 0 false exit
        then
        dup c@ swap 1+
        c@ case
        0 of true exit endof
        '\'' of true exit endof
            drop 0 false
        endcase
    endof
        \ default case
        \ ( addr base )
        drop base @ swap parse-uint
        dup \ need this because endcase drops top of stack
    endcase
;

( === String === )

\ c-addr2 = c-addr1+n
\ u2 = u1-n
: succ-buffer ( c-addr1 u1 n -- c-addr2 u2 )
    dup -rot - >r + r>
;

\ ( c-from c-to u -- )
\ Copy u bytes from c-from to c-to.
\ The memory regions must not be overlapped.
: memcpy
    begin dup 0> while
        1- >r   \ decrement u, save
        over c@
        over c! \ copy character
        1+ >r   \ increment c-to, save
        1+      \ increment c-from
        r> r>
    repeat 3drop
;

\ we already have memcpy,

\ ( c-from c-to -- )
\ copy nul terminated string from c-from to c-to
: strcpy
    begin over c@ dup while
        \ ( c-from c-to c )
        over c!
        1+ swap 1+ swap
    repeat
    over c!
    2drop
;

\ ( c-addr -- )
\ copy string to here including \0
: strcpy,
    begin dup c@ dup while
        c, 1+
    repeat 2drop
    0 c,
;

\ ( c-from c-to u -- )
: strncpy
    begin dup 0> while
        >r
        \ ( c-from c-to )
        over c@ over c!
        over c@ unless r> 3drop exit then
        1+ swap 1+ swap r> 1-
    repeat
    drop 1- 0 swap c! drop
;

\ ( c-addr1 c-addr2 u -- f )
: strneq
    begin dup 0> while
        1- >r
        dup 1+ >r c@
        swap dup 1+ >r c@
        <> if rdrop rdrop rdrop false exit then
        r> r> r>
    repeat
    3drop true
;

\ Print string
: type ( c-addr -- )
    begin dup c@ dup while  \ while c<>\0
        emit 1+
    repeat
    2drop
;

\ Print string up to u characters
: typen ( c-addr u -- )
    begin dup 0> while
        1- swap dup c@ dup unless
            2drop exit
        then
        emit 1+ swap
    repeat
;


\ Allocate a buffer for string literal
bl bl * constant s-buffer-size  \ 1024
create s-buffer s-buffer-size allot

\ Will define the error message corresponds to this error later
\ because we can't write string literal yet.
char 0 char B - constant STRING-OVERFLOW-ERROR \ -18

\ Parse string delimited by "
\ compile mode: the string is stored as operand of 'string' operator.
\ immediate mode: the string is stored to temporary buffer.
: s"
    state @ if
        compile litstring
        here 0 ,    \ save location of length and fill dummy
        0           \ length of the string + 1 (\0)
        begin key! dup '"' <> while
            c,      \ store character
            1+      \ increment length
        repeat drop
        0 c,        \ store \0
        1+ aligned
        swap !      \ back-fill length
        align
    else
        s-buffer dup    \ save start address
        begin key! dup '"' <> while
            ( buf pos c pos-buf )
            over 3 pick - s-buffer-size 1- >= if
                STRING-OVERFLOW-ERROR throw
            then
            over c! \ store char
            1+      \ increment address
        repeat drop
        0 swap c!   \ store \0
    then
; immediate

\ Print string delimited by "
: ."
    [compile] s"
    state @ if
        compile type
    else
        type
    then
; immediate

( === Error Code and Messages === )

\ Single linked list of error code and messages.
\ Thre structure of each entry:
\ | link | code | message ... |
variable error-list
0 error-list !

: error>next    ( a-addr -- a-addr) @ ;
: error>message ( a-addr -- c-addr ) 2 cells + ;
: error>code    ( a-addr -- n ) cell+ @ ;

: add-error ( n c-addr -- )
    error-list here
    ( n c-addr )
    over @ ,    \ fill link
    swap !      \ update error-list
    swap ,      \ fill error-code
    strcpy,     \ fill message
;

: def-error ( n c-addr "name" -- )
    create over ,
    add-error
    does> @
;

decimal
STRING-OVERFLOW-ERROR s" Too long string literal" add-error

variable next-user-error
s" -256" >number drop next-user-error !

\ Create new user defined error and returns error code.
: exception ( c-addr -- n )
    next-user-error @ swap add-error
    next-user-error @
    1 next-user-error -!
;

( === 3rd Stage Interpreter === )

s" -13" >number drop s" Undefined word" def-error UNDEFINED-WORD-ERROR
:noname
    find ?dup unless UNDEFINED-WORD-ERROR throw then
; &find! !

create word-buffer s" 64" >number drop cell+ allot

: interpret
    word!                   \ read name from input
    \ ( addr )
    dup word-buffer strcpy  \ save input
    dup find                \ lookup dictionary
    ?dup if
        \ Found the word
        nip
        state @ if
            \ compile mode
            dup cell+ c@ immediate-bit and if
                \ execute immediate word
                >cfa execute
            else
                \ compile the word
                >cfa ,
            then
        else
            \ immediate mode
            >cfa execute
        then
    else
        >number unless
            UNDEFINED-WORD-ERROR throw
        then
        \ Not found
        state @ if
            \ compile mode
            [compile] literal
        then
    then
;

:noname
    rp0 rp! \ drop 2nd stage
    begin
        ['] interpret catch
        ?dup if
            \ lookup error code
            error-list @
            begin ?dup while
                \ ( error-code error-entry )
                dup error>code
                2 pick = if
                    error>message type
                    ." : "
                    word-buffer type cr
                    bye
                then
                error>next
            repeat
            ." Unknown error code: "
            word-buffer type
            ."  (" 0 .r ." )" cr
            bye
        then
    again
; execute

( === Error-codes === )

decimal
-1 s" Aborted" def-error ABORTED-ERROR
-37 s" File I/O exception" def-error FILE-IO-ERROR
-39 s" Unexpected end of file" def-error UNEXPECTED-EOF-ERROR
-59 s" ALLOCATE" def-error ALLOCATE-ERROR
-62 s" CLOSE-FILE" def-error CLOSE-FILE-ERROR
-68 s" FLUSH-FILE" def-error FLUSH-FILE-ERROR
-69 s" OPEN-FILE" def-error OPEN-FILE-ERROR
-70 s" READ-FILE" def-error READ-FILE-ERROR
-71 s" READ-LINE" def-error READ-LINE-ERROR
-75 s" WRITE-FILE" def-error WRITE-FILE-ERROR

: abort ABORTED-ERROR throw ;

s" Not implemented" exception constant NOT-IMPLEMENTED
: not-implemented NOT-IMPLEMENTED throw ;

( 31 bytes )
s" Not reachable here. may be a bug" exception constant NOT-REACHABLE
: not-reachable NOT-REACHABLE throw ;

( === Do-loop === )

\ limit start do ... loop

1 constant do-mark
2 constant leave-mark

create do-stack 16 cells allot
variable do-sp
do-stack 16 cells + do-sp !

: >do ( w -- do: w )
    cell do-sp -!
    do-sp @ !
;

: do> ( do: w -- w )
    do-sp @ @
    cell do-sp +!
;

: do@ ( do: w -- w, do: w)
    do-sp @ @
;

\ compile: ( -- dest mark )
: do
    compile >r  \ save start
    compile >r  \ save limit
    here >do do-mark >do
; immediate

: leave ( -- orig mark )
    compile branch
    here >do
    0 ,        \ fill dummy offset
    leave-mark >do
; immediate

: backpatch-leave ( dest , do: orig1 mark1 ... -- do: origN markN ... )
    begin do@ leave-mark = while
        do> drop do>
        2dup -
        swap !
    repeat
    drop
;

: loop
    compile r>
    compile r>
    compile 1+
    compile 2dup
    compile >r
    compile >r
    compile =
    compile 0branch
    here cell + backpatch-leave     \ leave jumps to here
    do> drop            \ do-mark
    do> here - ,
    compile rdrop
    compile rdrop
; immediate

\ This code is take from Gforth
: crossed-boundary? ( d n i )
    swap -      ( d i-n )
    2dup +      ( d i-n i+d-n )
    over xor    ( d i-n (i-n)^(i+d-n) )
    >r xor r>   ( d^(i-n) (i^n)^(i+d-n) )
    and 0<
;

: +loop
    compile r>
    compile r>
    compile 3dup
    compile rot
    compile +
    compile >r
    compile >r
    compile crossed-boundary?
    compile 0branch
    here cell + backpatch-leave     \ leave jumps to here
    do> drop            \ do-mark
    do> here - ,
    compile rdrop
    compile rdrop
; immediate

: unloop ( R:a b -- )
    compile rdrop
    compile rdrop
; immediate

: i 2 rpick ;
: j 4 rpick ;
: k 6 rpick ;

( === Dump of data stack === )

\ ( -- n )
\ Number of elemtns in the stack
: depth     sp0 sp@ - cell- cell / ;
: rdepth    rp0 rp@ - cell / ;

: .s ( -- )
    sp0 sp@ - cell- cell /  ( depth of the stack )
    '<' emit 0 u.r '>' emit space
    sp@ sp0 ( beg end )
    begin 2dup < while
        cell- dup @ .
    repeat 2drop
    cr
;

( === Data Structure === )

\ align n1 to u-byte boundary
: aligned-by ( n1 u -- n2 )
    1- dup invert   \ ( n1 u-1 ~(u-1) )
    -rot + and
;

\ align here to u-byte boundary
: align-by ( u -- )
    here swap aligned-by &here !
;

: struct ( -- offset )
    0
;

\ struct ... end-struct new-word
\ defines new-word as a operator
\ that returns alignment and size of the struct.
\ new-word: ( -- align size )
: end-struct ( offset "name" -- )
    create , does> @ cell swap
;

: cell% ( -- align size ) cell cell ;
: char% ( -- align size ) 1 1 ;
: byte% cell% ;
: ptr% cell% ;
: int% cell% ;

\ allocate user memory
: %allot ( align size -- addr )
    here -rot swap align-by allot
;

: field ( offset1 align size "name" -- offset2 )
    \ align offset with 'align'
    -rot aligned-by \ ( size offset )
    create
        dup ,   \ fill offset
        +       \ return new offset
    does> @ +
;

( === File I/O === )

-1 constant EOF

\ file access methods (fam)
0x00 constant R/O  \ read-only
0x01 constant W/O  \ write-only
0x02 constant R/W  \ read-write

1024 constant BUFSIZE
128 constant FILENAME-MAX

\ File
struct
    cell% field file>fd     \ file desctipro
    cell% field file>read   ( c-addr u fd -- n )
    cell% field file>write  ( c-addr u fd -- n )

    char% field file>fam
    char% FILENAME-MAX * field file>name

    \ read buffer
    cell% field file>rbuf
    cell% field file>rbeg   \ read head
    cell% field file>rend

    \ write buffer
    cell% field file>wbuf
    cell% field file>wbeg   \ write head
    cell% field file>wend
end-struct file%

: writable? ( file -- f ) file>fam c@ R/O <> ;
: readable? ( file -- f ) file>fam c@ W/O <> ;

\ Write buffer
\ +-------------+-----+
\ |aaaaaaaaaaaaa|     |
\ +-------------+-----+
\ ^             ^     ^
\ wbuf          wbeg  wend

: write-buffer-content ( file -- c-addr u )
    dup file>wbeg @ swap file>wbuf tuck -
;

: empty-write-buffer ( file -- )
    dup file>wbuf @ over file>wbeg !
    dup file>wbuf @ over file>wend !
    drop
;

: succ-write-buffer ( file n -- )
    swap file>wbeg +!
;

: write-buffer-count ( file -- n )
    dup file>wbeg @ swap file>wbuf @ -
;

\ Read buffer
\ +-------------+-----+
\ |     |aaaaaaa|     |
\ +-------------+-----+
\ ^     ^       ^
\ rbuf  rbeg    rend

: read-buffer-content ( file -- c-addr u)
    dup file>rend @ swap file>rbeg @ tuck -
;

: empty-read-buffer ( file -- )
    dup file>rbuf @ over file>rbeg !
    dup file>rbuf @ over file>rend !
    drop
;

: succ-read-buffer ( file n -- )
    swap file>rbeg +!
;

: read-buffer-count ( file -- n )
    dup file>rend @ swap file>rbeg @ -
;

\ Flush output buffer of file, return error-code.
: flush-file ( file -- e )
    dup writable? unless FLUSH-FILE-ERROR exit then
    dup write-buffer-content ( file buf u )
    begin
        ( file buf u )
        dup 0= if 2drop empty-write-buffer success exit then
        2dup 4 pick file>fd @ 5 pick file>write @ execute
        ( file buf u n )
        dup 0< if 2drop FLUSH-FILE-ERROR exit then
        ( file buf u n )
        2dup < if not-reachable then
        succ-write-buffer
    again
;

\ Write bytes from c-addr u to file, return error-code.
: write-file ( c-addr u file -- e )
    dup writable? unless WRITE-FILE-ERROR exit then
    over 0<= if 3drop WRITE-FILE-ERROR exit then

    dup write-buffer-content BUFSIZE swap - ( space )
    2 pick ( space u )
    <= if
        ( c-addr u file )
        \ enough space, copy data
        2 pick over file>wbeg @ 3 pick memcpy
        \ increment wbeg
        swap succ-write-buffer drop success exit
    then
    ( c-addr u file )
    dup flush-file throw

    over BUFSIZE <= if
        \ fill data to wbuf
        2 pick over file>wbeg @ 3 pick memcpy
        swap succ-write-buffer drop success exit
    then

    \ write large data directly to the file
    begin
        ( c-addr u file )
        2 pick 2 pick 2 pick file>fd @ 3 pick file>write @ execute
        ( c-addr u file n )
        dup 0< if 2drop 2drop WRITE-FILE-ERROR exit then
        swap >r succ-buffer r>
        over 0>
    until
    empty-write-buffer 2drop success
;

\ Read u1-bytes at most from file, write it to c-addr.
\ Return number of bytes read and error-code.
: read-file ( c-addr u1 file -- u2 e )
    dup readable? unless READ-FILE-ERROR exit then
    over 0<= if 3drop 0 success exit then

    dup read-buffer-count 2 pick ( count u1 )
    >= if
        \ enough data in read buffer
        dup file>rbeg @ 3 pick 3 pick memcpy
        \ increment rbeg
        over succ-read-buffer
        nip success exit
    then

    \ copy rbeg..rend to the buffer
    dup read-buffer-content 4 pick swap memcpy
    ( buf u file )
    dup read-buffer-count dup >r
    ( buf u file n , R:written )
    swap >r succ-buffer r>
    dup empty-read-buffer

    ( buf u file , R:count )
    over BUFSIZE <= if
        \ read data to rbuf as much as BUFSIZE
        dup file>rbuf @ BUFSIZE 2 pick file>fd @ 3 pick file>read @ execute
        dup 0< if 2drop 2drop r> READ-FILE-ERROR exit then
        ( buf u file n , R:count )
        dup 2 pick file>rend +!
        2 pick min
        over file>rbeg @ 4 pick 2 pick memcpy
        dup 2 pick file>rbeg +!
        ( buf u file n , R:count )
        >r 3drop r> r> + success
    else
        \ read large data directly from the file
        dup file>fd @ swap file>read @ execute
        ( n , R:count )
        dup 0< if drop r> READ-FILE-ERROR exit then
        r> + success
    then
;

\ Read a character. Return EOF at end of input.
: key-file ( file -- c )
    0 sp@ 1 3 pick read-file throw
    ( file c u )
    1 = if
        nip
    else
        2drop EOF
    then
;

\ Read characters from 'file' to the buffer c-addr u1
\ until reaches '\n' or end of file, null character is
\ stored at last.
\ u2 is the number of characters written to the buffer.
\ flag=true if it reads '\n'. e is error code.
: read-line ( c-addr u1 file -- u2 e )
    over 1- 0 do
        2 pick i + 1 2 pick read-file
        dup 0< if false leave then
        drop
        ( c-addr u1 file u2 )
        0= if i success false leave then \ EOF
        2 pick i + c@ '\n' = if
            i 1+ success true leave
        then
    loop
    ( c-addr u1 file u2 e flag )
    >r >r
    3 pick over + 0 swap c! \ fill '\0'
    >r 3drop r> r> r> swap
;

\ Temporary runtime stdin and stdout using 'key' and 'type'

create stdin_ file% %allot drop
R/O stdin_ file>fam c!
' not-implemented stdin_ file>write !
here BUFSIZE allot stdin_ file>rbuf !
stdin_ dup file>rbuf @ swap file>rbeg !
stdin_ dup file>rbuf @ swap file>rend !
s" <stdin>" stdin_ file>name FILENAME-MAX strncpy

\ Read just 1 byte from stdin to c-buffer
:noname ( c-addr u obj -- n )
    drop
    1 < if
        drop 0
    else
        key-old swap c!
        1
    then
; stdin_ file>read !

( === Input Stream === )

\ input stream stack
struct
    cell% field input>next
    cell% field input>file
    cell% field input>lineno
end-struct inputstream%

variable inputstreams
0 inputstreams !

: push-inputstream ( file -- )
    inputstream% %allot   \ addr
    swap over input>file !
    0 over input>lineno !
    inputstreams @ over input>next !
    inputstreams !
;

: pop-inputstream ( -- file )
    inputstreams @ dup
    input>next @ inputstreams !
    input>file @
;

stdin_ push-inputstream

\ Replacing parser functions using input stream.

variable source-buffer BUFSIZE allot
BUFSIZE constant source-buffer-size
variable source-buffer-pos 0 source-buffer-pos !
variable source-buffer-end 0 source-buffer-end !

: increment-lineno ( -- ) 1 inputstreams @ input>lineno +! ;

: source ( -- c-addr) source-buffer ;
: >in ( -- c-addr ) source-buffer-pos ;

\ Throw UNEXPECTED-EOF-ERROR at EOF
:noname ( -- c )
    key dup EOF = if drop UNEXPECTED-EOF-ERROR throw then
; &key! !

\ New version of single line comment
: \ begin key! '\n' = until ; immediate

\ New version of 'key'.
: new-key ( -- c )
    source-buffer-pos @ source-buffer-end @ = if
        \ the buffer is empty
        0 source-buffer-pos !
        0 source-buffer-end !
        increment-lineno

        source-buffer BUFSIZE inputstreams @ input>file @ read-line throw
        if
            \ reached end of line
            dup 0= if
                drop '\n' exit \ empty line
            then
            source-buffer-end +!
        else
            \ reached EOF
            dup 0= if
                drop EOF exit
            then
            source-buffer-end +!
        then
    then
    source-buffer source-buffer-pos @ + c@
    1 source-buffer-pos +!
;

\ Read a word from input stream, return address of the string
\ and error-code.
:noname ( -- c-addr e )
    \ skip leading spaces
    0
    begin
        drop
        key
        dup bl <> over '\n' <> and
    until
    dup EOF = if
        drop word-buffer UNEXPECTED-EOF-ERROR
        exit
    then
    word-buffer tuck c!
    1+
    begin
        \ ( p )
        key
        dup bl = over '\n' = or over EOF = or if
            drop
            0 swap c!   \ store \0
            word-buffer success
            exit
        then
        over c!
        1+
    again
; &word !

:noname
    word throw
; &word! !

: : ( "name -- )
    align
    here latest , &latest !
    word throw dup strlen
    smudge-bit or c,
    strcpy,
    align
    docol ,
    ]
;

( === 4th Stage Interpreter === )

-56 s" Bye" def-error QUIT

: interpret-inner
    begin
        word \ read name from input

        \ EOF at this point is not an error
        UNEXPECTED-EOF-ERROR = if QUIT throw then

        dup word-buffer strcpy  \ save input
        dup find                \ lookup dictionary
        ?dup if
            \ Found the word
            nip
            state @ if
                \ compile mode
                dup cell+ c@ immediate-bit and if
                    \ execute immediate word
                    >cfa execute
                else
                    \ compile the word
                    >cfa ,
                then
            else
                \ immediate mode
                >cfa execute
            then
        else
            >number unless
                UNDEFINED-WORD-ERROR throw
            then
            \ Not found
            state @ if
                \ compile mode
                [compile] literal
            then
        then
    again
;

: interpret-outer
    begin
        ['] interpret-inner catch
        ?dup if
            \ lookup error code
            dup QUIT = if throw then

            decimal
            '[' emit inputstreams @ input>file @ file>name type ':' emit
            inputstreams @ input>lineno @ 0 u.r ." ] "

            error-list @
            begin ?dup while
                \ ( error-code error-entry )
                dup error>code
                2 pick = if
                    error>message type
                    ." : "
                    word-buffer type cr
                    1 quit
                then
                error>next
            repeat
            ." Unknown error code: "
            word-buffer type
            ."  (" 0 .r ." )" cr
            1 quit
        then
    again
;

:noname
    rp0 rp! \ drop 3rd stage
    ['] new-key &key !

    ['] interpret-outer catch bye
; execute

( === [if]..[else]..[then] === )

: [if] ( f -- )
    unless
        \ skip inputs until corresponding [else] or [then]
        0 \ depth
        begin
            word throw
            dup s" [if]" streq if
                drop 1+
            else dup s" [else]" streq if
                drop
                dup 0= if drop exit then
            else s" [then]" streq if
                dup 0= if drop exit then
                1-
            then then then
        again
    then
; immediate

: [unless] ( f -- )
    not
    [compile] [if]
; immediate

: [else]
    \ If the condition is false, [else] is skipped by [if].
    \ So when the execution reaches [else] it means that
    \ the condition was true.

    \ skip inputs until corresponding [then]
    0 \ depth
    begin
        word throw
        dup s" [if]" streq if
            drop 1+
        else s" [then]" streq if
            dup 0= if drop exit then
            1-
        then then
    again
; immediate

: [then] ; immediate \ do nothing

( === Dictionary === )

\ print the name of the word
: id. ( nt -- )
    cell+ dup c@ length-mask and
    begin dup 0> while
        swap 1+ dup c@ emit swap 1-
    repeat
    2drop
;

\ print all visible words
: words
    latest
    begin ?dup while
        dup cell+ c@ smudge-bit and unless
            dup id. space
        then
        @
    repeat
    cr
;

: name>link ( nt -- nt ) @ ;
: name>string ( nt -- c-addr ) cell+ 1+ ;

( === Command-line Arguments === )

variable argc
variable argv
v argc ! argv !

: arg ( u -- c-addr )
    dup argc @ < if
        cells argv @ + @
    else
        drop 0
    then
;

\ Remove 1 arg, update argv and argc
: shift-args ( -- )
    argc @ 1 = if exit then
    argc @ 1 do
        i 1+ arg            \ argv[i+1]
        i cells argv @ +    \ &argv[i]
        !                   \ copy argv[i+1] to argv[i]
    loop
    1 argc -!
;

\ Take 1 arg and shift arguments
: next-arg ( -- c-addr )
    argc @ 1 = if 0 exit then
    1 arg
    shift-args
;

( === Version and Copyright === )

\ The version of planckforth (not runtime)
: version s" 0.0.1" ;

: strchr ( c-addr2 c -- c-addr2 )
    begin over c@ while
        over c@ over = if drop exit then
        swap 1+ swap
    repeat
    2drop 0
;

\ The version string is colon separated
\ <runtime name>:<copyright>

create runtime-info runtime-info_ strcpy,

runtime-info constant runtime
runtime-info ':' strchr 0 over c! 1+ constant copyright-text

: copyright
    copyright-text type cr
;

\ The version of PlanckForth (not runtime)
: version s" 0.0.1" ;

( === Environment Dependent Code === )

runtime  s" i386-linux-handwritten" streq [if]

%000 constant eax immediate
%001 constant ecx immediate
%010 constant edx immediate
%011 constant ebx immediate
%100 constant esp immediate
%101 constant ebp immediate
%110 constant esi immediate
%111 constant edi immediate

: mod-reg-r/m ( mod reg r/m -- u )
    0
    swap 0x7 and or
    swap 0x7 and 8 * or
    swap 0x3 and 64 * or
;

: scale-index-byte ( scale index byte -- u )
    0
    swap 0x7 and or
    swap 0x7 and 8 * or
    swap 0x3 and 64 * or
;

\ compile 'pop reg' and 'push reg'
: pop ( reg -- ) 0x58 + c, ; immediate
: push ( reg -- ) 0x50 + c, ; immediate

\ lodsl; jmp *(%eax);
: next ( -- ) 0xad c, 0xff c, 0x20 c, ; immediate
: int80 ( -- ) 0xcd c, 0x80 c, ; immediate

\ movl disp(reg1), reg2
: movmr ( disp reg1 reg2 -- )
    0x8b c, \ opcode
    swap dup %100 = if \ if reg1=esp
        \ ( disp reg2 reg1 )
        %01 -rot mod-reg-r/m c,
        %00 %100 %100 scale-index-byte c,
    else
        \ ( disp reg2 reg1 )
        %01 -rot mod-reg-r/m c,
    then
    c, \ displacement
; immediate

\ overwrite code field by DFA
: ;asm
    [compile] ; \ finish compilation
    latest dup >dfa swap >cfa !
; immediate

: syscall0 ( n -- e )
    eax pop
    int80
    eax push
    next
;asm

: syscall1 ( arg1 n -- e )
    eax pop
    ebx pop
    int80
    eax push
    next
;asm

: syscall2 ( arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    int80
    eax push
    next
;asm

: syscall3 ( arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    int80
    eax push
    next
;asm

: syscall4 ( arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push            \ save program counter ( arg4 esi )
    [ 4 ] esp esi movmr     \ movl 4(%esp), %esi
    int80
    esi pop             \ restore esi
    ebx pop
    eax push
    next
;asm

: syscall5 ( arg5 arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push            \ save esi ( arg5 arg4 esi )
    [ 4 ] esp esi movmr
    [ 8 ] esp edi movmr
    int80
    esi pop
    ebx pop
    ebx pop
    eax push
    next
;asm

: syscall6 ( arg6 arg5 arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push
    ebp push    \ ( arg6 arg5 arg4 esi ebp )
    [ 8 ] esp esi movmr
    [ 12 ] esp edi movmr
    [ 16 ] esp ebp movmr
    int80
    ebp pop
    esi pop
    ebx pop
    ebx pop
    ebx pop
    eax push
    next
;asm

( === Heap Memory === )

192 constant SYS-MMAP2

0x0 constant PROT-NONE
0x1 constant PROT-READ
0x2 constant PROT-WRITE
0x4 constant PROT-EXEC
0x8 constant PROT-SEM

0x01 constant MAP-SHARED
0x02 constant MAP-PRIVATE
0x0f constant MAP-TYPE
0x10 constant MAP-FIXED
0x20 constant MAP-ANONYMOUS

: mmap2 ( addr1 u -- addr2 e )
    >r >r                                   \ ( R: u addr1 )
    0                                       \ offset
    -1                                      \ fd
    MAP-ANONYMOUS MAP-PRIVATE or            \ flags
    PROT-READ PROT-WRITE or PROT-EXEC or    \ prot
    r> r> swap                              \ u addr1
    SYS-MMAP2
    syscall6
    dup -1 = if
        ALLOCATE-ERROR
    else
        success
    then
;

\ Secure a large heap memory block and cut memories from the block.
\ The allocated memories are never released until the program exit.
0x8000000 constant BLOCK-SIZE ( 128MB )
variable block-addr
variable next-addr
variable remaining-size

0 BLOCK-SIZE mmap2 throw block-addr !
block-addr @ next-addr !
BLOCK-SIZE remaining-size  !

: (allocate) ( u -- addr )
    dup remaining-size @ <= if
        ( u addr )
        next-addr @
        swap dup next-addr +! remaining-size -!
    else
        drop -1
    then
;

( === File I/O === )

3 constant SYS-READ
4 constant SYS-WRITE
5 constant SYS-OPEN
6 constant SYS-CLOSE

: (open) ( c-addr fam -- fd )
    swap SYS-OPEN syscall2
;

: (close) ( obj -- n )
    SYS-CLOSE syscall1
;

: (read) ( c-addr u fd -- n )
    >r swap r> SYS-READ syscall3
;

: (write) ( c-addr u1 fd -- n )
    >r swap r>              \ ( u1 u1 c-addr fd )
    SYS-WRITE syscall3      \ ( u1 u2 )
;

[then] \ End of environment dependent code

: defined? ( "name" -- f )
    word throw find 0 <>
;

: need-defined ( "name" -- )
    word throw dup find unless
        ." Implementation of " type ."  is missing." cr
        ." Please implement it or use --gen <target> option." cr
        UNDEFINED-WORD-ERROR throw
    then drop
;

( === Heap Memory === )

need-defined (allocate)

: allocate ( size -- addr e )
    (allocate) dup 0<> if success else ALLOCATE-ERROR then
;

\ allocate heap memory
: %allocate ( align size -- addr e )
    over + allocate ?dup unless
        swap 1- invert and success
    then
;

( === open/close === )

need-defined (open)
need-defined (close)
need-defined (write)
need-defined (read)

: open-file ( c-addr fam -- file e )
    2dup (open) dup -1 = if
        ( c-addr fam fd )
        3drop 0 OPEN-FILE-ERROR exit
    then
    file% %allocate throw
    tuck file>fd !
    tuck file>fam !
    tuck file>name FILENAME-MAX strncpy
    ['] (read) over file>read !
    ['] (write) over file>write !
    dup file>fam @ W/O <> if
        BUFSIZE allocate throw over file>rbuf !
        dup file>rbuf @ over file>rbeg !
        dup file>rbuf @ over file>rend !
    then
    dup file>fam @ R/O <> if
        BUFSIZE allocate throw over file>wbuf !
        dup file>wbuf @ over file>wbeg !
        dup file>wbuf @ BUFSIZE + over file>wend !
    then
    success
;

: close-file ( file -- e )
    file>fd @ (close) 0= if success else CLOSE-FILE-ERROR then
;

( === File Include === )

: included ( c-addr -- )
    R/O open-file throw
    push-inputstream
    ['] interpret-outer catch drop
    pop-inputstream close-file throw
;

: include ( "name" -- )
    word throw included
;

( === Primitive Instructions === )

: insn:docol docol ;
: insn:exit ['] e ;
: insn:lit ['] lit ;
: insn:litstring ['] litstring ;
: insn:branch ['] branch ;
: insn:0branch ['] 0branch ;

( === Remove Unnecessary Words === )

\ compile: ( "name" -- )
\ runtime: ( nt1 -- nt2 )
: update-dictionary ( "name1" "name" ... -- )
    compile 0
    begin
        word throw
        dup s" end-update-dictionary" streq if
            drop
            compile &latest
            compile !
            exit
        then
        find ?dup if
            [compile] literal
            compile tuck
            compile !
        else
            UNDEFINED-WORD-ERROR throw
        then
    again
; immediate

\ rebuilt dictionary
:noname
    update-dictionary
        insn:docol insn:exit insn:lit insn:litstring insn:branch insn:0branch

        words id. name>string name>link
        include included source >in
        next-arg shift-args arg argv argc version runtime copyright

        [if] [unless] [else] [then] defined?
        open-file close-file write-file flush-file
        read-file key-file read-line
        R/W W/O R/O EOF

        abort ABORTED-ERROR
        QUIT not-reachable NOT-REACHABLE
        not-implemented NOT-IMPLEMENTED
        WRITE-FILE-ERROR READ-FILE-ERROR OPEN-FILE-ERROR
        FLUSH-FILE-ERROR CLOSE-FILE-ERROR
        ALLOCATE-ERROR UNEXPECTED-EOF-ERROR FILE-IO-ERROR
        STRING-OVERFLOW-ERROR UNDEFINED-WORD-ERROR
        exception

        %allocate %allot char% cell% byte% ptr% int% field struct end-struct
        sp0 sp@ sp! dup ?dup drop swap over tuck pick nip rot -rot
        2rot -2rot 2tuck 2over 2nip 2swap 2dup 2drop 3dup 3drop depth
        rp0 rp@ rp! r> >r r@ rdrop rpick rdepth

        allocate allot memcpy strlen streq strneq strcpy strcpy,
        cell cell+ cell- cells char+ char- chars align aligned +! -!

        if else then unless begin until again while repeat
        recurse case of rangeof endof endcase
        do loop +loop unloop leave i j k

        char [char] key emit spaces
        .s . .r u. u.r dec. hex. type typen
        ." s" bl '\n' cr space base decimal hex
        catch throw success
        : ; [ ] immediate create >body :noname does> variable constant
        ' ['] compile [compile] literal state
        + - * /mod / mod negate not and or xor invert within max min abs
        < > <= >= = <> 0< 0> 0<= 0>= 0= 0<> 1+ 1-
        u< u> u<= u>= lshift rshift 2* 2/

        true false

        ( \
        c@ c! c, @ ! ,
        word find >cfa >dfa
        bye execute exit here latest
    end-update-dictionary
; execute


( === End of bootstrap === )

:noname
    argc @ 2 < if exit then
    1 arg s" --version" 10 strneq if
        ." PlanckForth " version type cr bye
    else 1 arg s" --runtime" 10 strneq if
        runtime type cr bye
    then then
; execute

include lib/core.fs

:noname
    rdrop
    argc @ 1 > if
        next-arg dup argv @ !
        included
    else
        ." Welcome to PlanckForth " version type
        ."  [" runtime type ." ]" cr
        copyright
        ." Type 'bye' to exit." cr
        s" /dev/tty" included
    then
; execute
