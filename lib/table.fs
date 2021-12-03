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

: table-size ( tbl -- n ) table>size @ ; export

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

: table@-helper ( key tbl -- entry )
    2dup table>hash @ execute ( key tbl hashed-key )
    over table>bucket @ array-size mod ( key tbl idx )
    over table>bucket @ array@ ( key tbl entry )
    swap table>equal @ -rot ( equal key entry )
    begin ?dup while
        dup entry>key @
        2 pick 4 pick execute if
            ( equal key entry )
            nip nip exit
        then
    repeat
    2drop 0
;

: table@ ( key tbl -- v )
    2dup table@-helper ?dup if
        entry>value @ nip nip
    else
        KEY-NOT-FOUND throw
    then
; export

: ?table-in ( key tbl -- n )
    table@-helper 0 <>
; export

( tables for major builtin types )
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
