\ planckforth -
\ Copyright (C) 2021 nineties

( === Hash Table === )

include lib/bitscan.fs
include lib/array.fs

private{

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
; export

10 constant DEFAULT_TABLE_SIZE_HINT

: make-table ( hash equal -- tbl )
    DEFAULT_TABLE_SIZE_HINT make-table-with-hint
; export

}private
