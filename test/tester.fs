\ planckforth -
\ Copyright (C) 2021 nineties

variable verbose
true verbose !

: empty-stack sp0 sp! ;

variable #errors 0 #errors !

: ESC [ 0x1b ] literal ;
: error ( c-addr -- )
    ESC emit ." [31m"
    type source type
    ESC emit ." [m"
    empty-stack
    1 #errors +!
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
    depth ?dup if
        0 do
            actual-results i cells + @ <> if
                s" incorrect result: " error leave
            then
        loop
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
