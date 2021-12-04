\ planckforth -
\ Copyright (C) 2021 nineties

( === Hash Table === )

include lib/bitscan.fs
include lib/array.fs

private{

s" Key not found" exception constant KEY-NOT-FOUND export


create prime_numbers
    5 , 11 , 17 , 37 , 67 , 131 , 257 , 521 , 1031 ,
    2053 , 4099 , 8209 , 16411 , 32771 , 65537 , 131101 ,
    0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
    0 , 0 , 0 ,

struct
    cell% field table>bucket
    cell% field table>hash       ( hash function )
    cell% field table>equal      ( equal function for keys )
    cell% field table>entries    ( list of entries )
    int% field table>size       ( number of entries )
end-struct table%

struct
    cell% field entry>key
    cell% field entry>value
    cell% field entry>sibling    ( pointer to the next entry in bucket )
    cell% field entry>next       ( pointer to the next entry in entries )
    int% field entry>hash       ( the hash value )
end-struct entry%

\ Number of elments in the table
: table-size ( tbl -- n ) table>size @ ; export

\ Make hashtable considering given size hint
: make-table-with-hint ( hash equal n -- tbl )
    bitscan-reverse cells prime_numbers + @ ( n to bucket size )
    make-array  ( allocate bucket )

    ( hash equal bucket )
    table% %allocate throw
    tuck table>bucket !
    tuck table>equal !
    tuck table>hash !
    0 over table>entries !
    0 over table>size !
;

10 constant DEFAULT_TABLE_SIZE_HINT

\ Make hashtable. It takes two functions.
\ hash ( w -- n ) : compute hash value of w
\ equal ( w1 w2 -- n ) : compute equality of w1 and w2
: make-table ( hash equal -- tbl )
    DEFAULT_TABLE_SIZE_HINT make-table-with-hint
; export

: release-table ( tbl -- )
    dup table>entries @
    begin ?dup while
        dup entry>next @
        swap
        free
    repeat
    dup table>bucket @ release-array
    free
; export

: find-entry ( key tbl hash -- entry )
    over table>bucket @ array-size mod ( key tbl idx )
    over table>bucket @ array@ ( key tbl entry )
    swap table>equal @ -rot ( equal key entry )
    begin ?dup while
        dup entry>key @
        2 pick 4 pick execute if
            ( equal key entry )
            nip nip exit
        then
        entry>next @
    repeat
    2drop 0
;

\ Lookup table entry. KEY-NOT-FOUND exception is raised
\ when there is no corresponding entry.
: table@ ( key tbl -- val )
    2dup
    2dup table>hash @ execute
    find-entry ?dup if
        entry>value @ nip nip
    else
        KEY-NOT-FOUND throw
    then
; export

\ Returns true when the key is in the table
: ?table-in ( key tbl -- n )
    2dup table>hash @ execute
    find-entry 0 <>
; export

\ Store key-value pair to the table
: table! ( val key tbl -- )
    2dup
    2dup table>hash @ execute
    dup >r find-entry r> swap
    ( val key tbl hash entry )
    ?dup if
        ( `tbl` already has an entry for `key` )
        nip nip nip
        entry>value !
    else
        swap >r
        ( val key hash , R:tbl )
        entry% %allocate throw
        tuck entry>hash !
        tuck entry>key !
        tuck entry>value !
        0 over entry>sibling !
        0 over entry>next !
        r>
        ( entry tbl )
        \ Find corresponding bucket entry
        over entry>hash @
        over table>bucket @
        tuck array-size mod
        ( entry tbl bucket index )

        \ Add new entry to the bucket
        2dup swap array@
        4 pick entry>sibling !
        3 pick swap rot array!

        \ Add the entry to the list of entries
        ( entry tbl )
        tuck table>entries @
        over entry>next !
        over table>entries !

        \ Increment table>size
        table>size 1 swap +!
    then
; export

\ Returns cons-list of keys
: table-keys ( tbl -- list )
    0 swap table>entries @
    begin ?dup while
        tuck entry>key @ swap cons swap
        entry>next @
    repeat
; export

\ Returns cons-list of values
: table-values ( tbl -- list )
    0 swap table>entries @
    begin ?dup while
        tuck entry>value @ swap cons swap
        entry>next @
    repeat
; export

( === tables for major builtin types === )
: hash-next ( n1 n2 -- n3 )
    + 6122117 * 1627577 +
;

: hash-int ( n -- n )
    0 hash-next
; export

: make-int-table ( -- tbl )
    ['] hash-int ['] = make-table
; export

}private

T{ make-int-table constant A -> }T
T{ A table-size -> 0 }T
T{ 0 A ' table@ catch -> 0 A KEY-NOT-FOUND }T
T{ 0 A ?table-in -> false }T
T{ 1 0 A table! -> }T
T{ 0 A ?table-in -> true }T
T{ 0 A table@ -> 1 }T
T{ A table-size -> 1 }T
T{ 2 0 A table! -> }T
T{ A table-size -> 1 }T
T{ 3 1 A table! -> }T
T{ A table-size -> 2 }T
T{ 1 A table@ -> 3 }T
T{ :noname 100 0 do i 1 + i A table! loop ; execute -> }T
:noname
    100 0 do
        T{ i A table@ -> i 1 + }T
    loop
; execute
T{ A table-size -> 100 }T

T{ A table-keys car -> 0 }T
T{ A table-keys cdr car -> 1 }T
T{ A table-keys cdr cdr car -> 2 }T
T{ A table-values car -> 1 }T
T{ A table-values cdr car -> 2 }T
T{ A table-values cdr cdr car -> 3 }T
