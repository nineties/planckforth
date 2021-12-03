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
    ptr% field table>bucket
    ptr% field table>hash       ( hash function )
    ptr% field table>equal      ( equal function for keys )
    ptr% field table>entries    ( list of entries )
    int% field table>size       ( number of entries )
end-struct table%

struct
    ptr% field entry>key
    ptr% field entry>value 
    ptr% field entry>sibling    ( pointer to the next entry in bucket )
    ptr% field entry>next       ( pointer to the next entry in entries )
    int% field entry>hash       ( the hash value )
end-struct entry%

: make-table-with-size ( hash equal n -- tbl )
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

}private
