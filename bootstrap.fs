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
\ 'Q' ( -- )            Exit the process
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
\ '=' ( a b -- c )      c = (a == b)

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

alias-builtin bye       Q
alias-builtin cell      C
alias-builtin &here     h
alias-builtin &latest   l
alias-builtin key       k
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
alias-builtin div       /
alias-builtin mod       %
alias-builtin and       &
alias-builtin or        |
alias-builtin xor       ^

\ Rename existing FORTH words
: word      W ;
: find      F ;
: >cfa      G ;
: c,        B ;
: cmove,    m ;
: strlen    z ;
: str=      E ;
: state     M ;
: aligned   a ;
: align     A ;

: here      &here @ ;
: latest    &latest @ ;
: >dfa >cfa cell + ;

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
: 10    [ key : key 0 - ] literal ;
: 16    [ key @ key 0 - ] literal ;
: -1    [ key 0 key 1 - ] literal ;

: true 1 ;
: false 0 ;

\ === Address Arithmetic ===

: cell+ cell + ;
: cell- cell - ;
: cells cell * ;

\ === Stack Manipulation ===

: drop  sp@ cell+ sp! ;     \ ( w -- )
: dup   sp@ @ ;             \ ( w -- w w )

: >r rp@ rp@ @ rp@ cell - dup rp! ! ! ;         \ ( w -- R:w )
: r> rp@ cell + @ rp@ @ rp@  cell + dup rp! ! ; \ ( R:w -- w)

: swap  sp@ cell + dup @ >r ! r> ;  \ ( a b -- b a )
: rot   >r swap r> swap ;           \ ( a b c -- b c a )
: -rot  swap >r swap r> ;           \ ( a b c -- c a b )
: nip   swap drop ;                 \ ( a b -- a )
: over  >r dup r> swap ;            \ ( a b -- a b a )
: tuck  dup -rot ;                  \ ( a b -- b a b )
: pick  cells sp@ + cell + @ ;      \ ( wu ... x0 u -- xu ... x0 xu )

: 2drop drop drop ;                 \ ( a b -- )
: 2dup  over over ;                 \ ( a b -- a b a b )
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
\ sp@ and rp@ points bottom if implementation so far is correct.
: sp0 [ sp@ ] literal ;
: rp0 [ rp@ ] literal ;

\ === Integer Arithmetic ===

: 1+ 1 + ;
: 1- 1 - ;

\ ( a b -- (a mod b) (a / b) )
: /mod 2dup mod -rot / ;

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
: <>    = not ;

: 0=    0 = ;
: 0<>   0 <> ;
: 0<    0 < ;
: 0>    0 > ;
: 0<=   0 <= ;
: 0>=   0 >= ;

\ ( a b c -- (a<=c & c<b) )
: within tuck > -rot <= and ;

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

\ compile: ( dest -- dest orig )
\ runtime: ( n -- )
\ dest=location of begin
\ orig=location of while
: while
    compile 0branch
    here 0 ,        \ save location, fill dummy
; immediate

\ compile: ( dest orig -- )
\ runtime: ( -- )
\ dest=location of begin
\ orig=location of while
: repeat
    swap
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

\ === Multiline Comment ===

: '('   [ key ( ] literal ;
: ')'   [ key ) ] literal ;

: (
    1   \ depth counter
    begin ?dup while
        key case
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
: allot ( n -- c-addr )
    here swap
    &here +!
;

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
    word dup strlen
    dup c, cmove, 0 c, align    \ fill length, name and \0
    docol ,                     \ compile docol
    ['] lit ,
    here 3 cells + ,            \ compile the address
    ['] nop ,                   \ does>, if any, will fill this cell
    ['] exit ,                  \ compile exit
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
: char      ( <spces>ccc -- c ) word c@ ;

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
        \ mod base quot base
        >r over r>
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
            2drop r> r> swap drop false
            exit
        endcase
        2dup
        \ ( base n base n )
        0 -rot
        \ ( base n 0 base n )
        within unless
            \ failed to convert
            2drop r> r> swap drop false
            exit
        then
        \ ( base addr n acc )
        r> swap r>
        3 pick * +
    repeat
    \ success
    swap drop
    swap drop
    true
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
    '&' of 1+ 10 swap parse-uint endof
    '#' of 1+ 10 swap parse-uint endof
    '%' of 1+ 2 swap parse-uint endof
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

\ ( c-from c-to u -- )
\ Copy u bytes from c-from to c-to.
\ The memory regions must not be overlapped.
: cmove
    begin dup 0> while
        1- >r   \ decrement u, save
        over c@
        over c! \ copy character
        1+ >r   \ increment c-to, save
        1+      \ increment c-from
        r> r>
    repeat
    drop drop drop
;

\ we already have cmove,

\ ( c-from c-to -- )
\ copy nul terminated string from c-from to c-to
: strcpy
    begin over c@ dup while
        \ ( c-from c-to c )
        over c!
        1+ swap 1+ swap
    repeat
    over c!
    drop drop
;

\ ( c-addr -- )
\ copy string to here including \0
: strcpy,
    begin dup c@ dup while
        c, 1+
    repeat 2drop
    0 c,
;

\ Print string
: type ( c-addr -- )
    begin dup c@ dup while  \ while c<>\0
        emit 1+
    repeat
    2drop
;

\ Allocate a buffer for string literal
bl constant s-buffer-size  \ 1024
create s-buffer s-buffer-size allot drop

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
        begin key dup '"' <> while
            c,      \ store character
            1+      \ increment length
        repeat drop
        0 c,        \ store \0
        1+
        swap !      \ back-fill length
        align
    else
        s-buffer dup    \ save start address
        begin key dup '"' <> while
            2dup swap - s-buffer-size >= if
                throw STRING-OVERFLOW-ERROR
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
s" -13" >number drop s" Undefined word" def-error UNDEFINED-WORD-ERROR

variable next-user-error
s" -256" >number drop next-user-error !

\ Create new user defined error and returns error code.
: exception ( c-addr -- n )
    next-user-error @ swap add-error
    next-user-error @
    1 next-user-error -!
;

( === 3rd Stage Interpreter === )

create word-buffer s" 64" >number drop cell+ allot drop

: interpret
    word                    \ read name from input
    \ ( addr )
    dup word-buffer strcpy  \ save input
    dup find                \ lookup dictionary
    ?dup if
        \ Found the word
        swap drop
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

: main
    rdrop   \ drop 2nd stage
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
            ." Unknown error code: " . cr
            bye
        then
    again
;

main

( === Error-codes === )

decimal
-1 s" Aborted" def-error ABORTED-ERROR
-37 s" File I/O exception" def-error FILE-IO-ERROR
-39 s" Unexpected end of file" def-error UNEXPECTED-EOF-ERROR
-68 s" FLUSH-FILE" def-error FLUSH-FILE-ERROR
-70 s" READ-FILE" def-error READ-FILE-ERROR
-71 s" READ-LINE" def-error READ-LINE-ERROR
-75 s" WRITE-FILE" def-error WRITE-FILE-ERROR

: abort ABORTED-ERROR throw ;

( === Dump of data stack === )
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

\ allocate user memory
: %allot ( align size -- addr )
    swap align-by allot
;

: field ( offset1 align size "name" -- offset2 )
    \ align offset with 'align'
    -rot aligned-by \ ( size offset )
    create
        dup ,   \ fill offset
        +       \ return new offset
    does> @ +
;

( === File I/O Abstraction === )

-1 constant EOF

\ file access methods (fam)
0x00 constant R/O  \ read-only
0x01 constant R/W  \ read-write
0x02 constant W/O  \ write-only

\ File
struct
    cell% field file>read-file  ( c-addr u1 obj -- u2 e )
    cell% field file>read-line  ( c-addr u1 obj -- u2 flag e )
    cell% field file>key-file   ( obj -- c e )
    cell% field file>write-file ( c-addr u obj -- e )
    cell% field file>flush-file ( obj -- e )
    char% field file>fam
    cell% field file>name

    \ implementation dependent file object
    cell% field file>obj
end-struct file%

: writable? ( file -- f ) file>fam c@ R/O <> ;
: readable? ( file -- f ) file>fam c@ W/O <> ;

\ Write bytes from buffer c-addr u1 to file, return error-code.
: write-file ( c-addr u1 file -- e )
    dup writable? if
        dup file>obj swap file>write-file @ execute
    else
        WRITE-FILE-ERROR
    then
;

\ Read u1-bytes at most from file, write it to c-addr.
\ Return number of bytes read and error-code.
: read-file ( c-addr u1 file -- u2 e )
    dup readable? if
        dup file>obj swap file>read-file @ execute
    else
        0 READ-FILE-ERROR
    then
;

\ Flush output buffer of file, return error-code.
: flush-file ( file -- e )
    dup writable? if
        dup file>obj swap file>flush-file @ execute
    else
        FLUSH-FILE-ERROR
    then
;

\ Read a character. Return EOF at end of input.
: key-file ( file -- c )
    dup file>obj swap file>key-file @ execute throw
;

\ Read characters from 'file' to the buffer c-addr u1
\ until reaches '\n' or end of file.
\ The last '\n' is not stored to the buffer.
\ u2 is the number of characters written to the buffer.
\ flag=true if it reaches '\n'.
\ e is error code.
: read-line ( c-addr u1 file -- u2 flag e )
    dup readable? if
        dup file>obj swap file>read-line @ execute
    else
        READ-LINE-ERROR
    then
;

\ Temporary implementation stdin and stdout using 'key' and 'type'

s" Not implemented" exception constant NOT-IMPLEMENTED

: not-implemented NOT-IMPLEMENTED throw ;

create stdin_ file% %allot drop
R/O stdin_ file>fam c!
' not-implemented stdin_ file>write-file !
' not-implemented stdin_ file>flush-file !

\ Read u byte from stdin to c-addr.
:noname ( c-addr u obj -- u e )
    drop
    dup >r
    begin dup 0> while
        \ c-addr u c
        key 2 pick c!
        1- swap 1+ swap
    repeat
    2drop
    r> success  \ 0: no-error
; stdin_ file>read-file !

:noname ( c-addr u1 obj -- u2 flag e )
    drop 0
    begin
        ( c-addr u1 u2 )
        over 0<= if
            -rot dup dup false success
            exit
        then
        key
        dup '\n' = if
            ( c-addr u1 u2 c )
            drop -rot drop drop true success
            exit
        then
        3 pick c!
        1+ >r 1- swap 1+ swap r>
    again
; stdin_ file>read-line !

:noname ( obj -- c e )
    drop key success
; stdin_ file>key-file !

create stdout_ file% %allot drop
W/O stdout_ file>fam c!
' not-implemented stdout_ file>read-file !
' not-implemented stdout_ file>read-line !
' not-implemented stdout_ file>key-file !

\ Write u byte from c-addr to stdout.
:noname ( c-addr u obj -- e )
    drop type success
; stdout_ file>write-file !

\ do nothing
:noname drop success ; stdout_ file>flush-file !

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

: pop-inputstream ( -- )
    inputstreams @ inputstreams !
;

stdin_ push-inputstream

\ Rewrite existing functions that reads inputs using inputstream.

: key ( -- c )
    inputstreams @ input>file @ key-file
;

\ Read a word from input stream, return address of the string
\ and error-code.
: word ( -- c-addr e )
    inputstreams @ input>file @
    \ skip leading spaces
    0
    begin
        drop
        dup key-file    \ ( file c )
        dup bl <> over '\n' <> and
    until
    dup EOF = if
        drop word-buffer UNEXPECTED-EOF-ERROR
    then
    word-buffer tuck c!
    1+
    begin
        \ ( file p )
        over key-file
        dup bl = over '\n' = or if
            drop
            0 swap c!   \ store \0
            drop word-buffer success
            exit
        then
        over c!
        1+
    again
;

: ' ( "name" -- xt )
    word throw
    find ?dup if
        >cfa
    else
        UNDEFINED-WORD-ERROR throw
    then
;
: [compile] ' , ; immediate
: (compile)
    [compile] literal
    [ ' , ] literal ,
;
: compile ' (compile) ; immediate
: ['] ' [compile] literal ; immediate

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

: create ( "name" -- )
     align
     here latest , &latest !
     word throw dup strlen c, strcpy,
     align
     docol ,
     compile lit
     here 3 cells + ,
     compile nop
     compile exit
;


: char ( "ccc" -- c ) word throw c@ ;

( === 4th Stage Interpreter === )

-56 s" Bye" def-error QUIT

: interpret
    word                    \ read name from input

    \ EOF at this point is not an error
    UNEXPECTED-EOF-ERROR = if QUIT throw then

    dup word-buffer strcpy  \ save input
    dup find                \ lookup dictionary
    ?dup if
        \ Found the word
        swap drop
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

: interpret-loop
    begin
        ['] interpret catch
        ?dup if
            \ lookup error code
            dup QUIT = if throw then
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
            ." Unknown error code: " . cr
            bye
        then
    again
;

: switch-to-4th-stage
    rdrop   \ drop 3rd stage

    ['] interpret-loop catch bye
;

switch-to-4th-stage

( === [if]..[else]..[then] === )

: [if] ( f -- )
    unless
        \ skip inputs until corresponding [else] or [then]
        0 \ depth
        begin
            word throw
            dup s" [if]" str= if
                drop 1+
            else dup s" [else]" str= if
                drop
                dup 0= if drop exit then
            else s" [then]" str= if
                dup 0= if drop exit then
                1-
            then then then
        again
    then
; immediate

: [else]
    \ If the condition is false, [else] is skipped by [if].
    \ So when the execution reaches [else] it means that
    \ the condition was true.

    \ skip inputs until corresponding [then]
    0 \ depth
    begin
        word throw
        dup s" [if]" str= if
            drop 1+
        else s" [then]" str= if
            dup 0= if drop exit then
            1-
        then then
    again
; immediate

: [then] ; immediate \ do nothing

( === Do-loop === )

\ limit start do ... loop

1 constant do-mark
2 constant leave-mark

create do-stack 16 cells allot drop
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
    compile 2dup
    compile >r  \ save start
    compile >r  \ save limit
    \ leave if start >= limit
    compile >
    compile 0branch
    0 ,
    here >do do-mark >do
    here cell- >do leave-mark >do
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

: i 2 rpick ;
: j 4 rpick ;
: k 6 rpick ;

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

( === Environment-Dependent Code === )

\ Parse '--gen' option.
\ $ ./planck < bootstrap --gen i386-linux ...

: strn= ( c-addr1 c-addr2 u -- f )
    begin dup 0> while
        1- >r
        over c@ over c@
        <> if r> drop drop drop false exit then
        1+ swap 1+ swap r>
    repeat drop drop drop
    true
;

variable codegen-target

\ Parse command-line arguments.
: read-commandline-args ( -- )
    s" no-codegen" codegen-target !
    begin argc @ 1 > while
        1 arg dup c@ '-' <> if drop exit then
        dup s" --gen" 5 strn= if
            dup 5 + c@ '=' = if
                6 + codegen-target !
                shift-args
            else
                drop shift-args
                next-arg codegen-target !
            then
        else
            ." Unknown option: " type cr
            abort
        then
    repeat
;

read-commandline-args

codegen-target @ s" i386-linux" str= [if]

%000 constant eax immediate
%001 constant ecx immediate
%010 constant edx immediate
%011 constant ebx immediate

\ compile 'pop reg' and 'push reg'
: pop ( reg -- ) 0x58 + c, ; immediate
: push ( reg -- ) 0x50 + c, ; immediate

\ lodsl; jmp *(%eax);
: next ( -- ) 0xad c, 0xff c, 0x20 c, ; immediate
: int80 ( -- ) 0xcd c, 0x80 c, ; immediate

\ overwrite code field by DFA
: ;asm
    [compile] ; \ finish compilation
    latest dup >dfa swap >cfa !
; immediate

: syscall0 ( n -- r )
    eax pop
    int80
    eax push
    next
;asm

: syscall1 ( arg1 n -- r )
    eax pop
    ebx pop
    int80
    eax push
    next
;asm

: syscall2 ( arg2 arg1 n -- r )
    eax pop
    ebx pop
    ecx pop
    int80
    eax push
    next
;asm

: syscall3 ( arg3 arg2 arg1 n -- r )
    eax pop
    ebx pop
    ecx pop
    edx pop
    int80
    eax push
    next
;asm

( === File I/O === )

5 constant SYS_OPEN
6 constant SYS_CLOSE

: fam-to-mode ( fam -- u )
    case
    R/O of 0x00 endof
    W/O of 0x01 endof
    R/W of 0x02 endof
        FILE-IO-ERROR throw
    endcase
;

: (open-file) ( c-addr fam -- obj f )
    fam-to-mode swap SYS_OPEN syscall2 dup 0>=
;

: (close-file) ( obj -- f)
    SYS_CLOSE syscall1 0>=
;

[else] \ i386-linux

codegen-target @ s" no-codegen" str= not [if]
    ." Unknown codegen target: " codegen-target @ type cr
    abort
[then] [then]

( === open/close === )


-62 s" CLOSE-FILE" def-error CLOSE-FILE-ERROR
-69 s" OPEN-FILE" def-error OPEN-FILE-ERROR

: open-file ( c-addr fam -- file e )
    2dup (open-file) if
        file% %allot
        tuck file>obj !
        tuck file>fam !
        tuck file>name !
        success
    else
        OPEN-FILE-ERROR throw
    then
;

: close-file ( file -- e )
    file>obj (close-file) unless
        CLOSE-FILE-ERROR throw
    then
;

s" bootstrap.fs" R/O open-file

." Ready" cr
