\ planckforth -
\ Copyright (C) 2021 nineties

variable verbose
\ true verbose !
false verbose !

: empty-stack sp0 sp! ;

variable #ok 0 #ok !
variable #error 0 #error !
variable #skip 0 #skip !

: ESC [ 0x1b ] literal ;
: red ESC emit ." [31m" ;
: green ESC emit ." [32m" ;
: yellow ESC emit ." [33m" ;
: reset ESC emit ." [m" ;
: error ( c-addr -- )
    red type source type reset
    empty-stack
    1 #error +!
;

variable actual-depth
create actual-results 20 cells allot


: T{ ;
: ->    ( save depth and contents )
    depth dup actual-depth !
    ?dup if
        0 do actual-results i cells + ! loop
    then
;

: }T    ( compare expected data and actual-results )
    depth actual-depth @ <> if
        s" wrong number of results: " error exit
    then
    true >r
    depth ?dup if
        0 do
            actual-results i cells + @ <> if
                s" incorrect result: " error
                r> drop false >r
                leave
            then
        loop
    then
    r> if
        1 #ok +!
    then
;

: testing
    source verbose @ if
        dup type
    else
        '.' emit
    then
    strlen >in !  \ sking this line
;

: skip
    source verbose @ if
        dup type
    then
    strlen >in !  \ skip this line
    1 #skip +!
;

: report-and-exit
    decimal

    cr ." --------------------------------"
    cr ." Run " #ok @ #error @ + #skip @ + . ." tests" cr
    green ." ok:" #ok @ .
    red ." failed:" #error @ .
    yellow ." skipped:" #skip @ .
    reset
    cr ." --------------------------------"
    cr
    #error @ 0= if bye else abort then
;
