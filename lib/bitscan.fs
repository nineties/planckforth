\ planckforth -
\ Copyright (C) 2021 nineties

( === Bit-Scan operations === )

cell 16 > [if]
    ." Bit-Scan for integers longer than 128bits is not supported"
    not-supported
[then]

private{

create bsf-modulo-131-table
    -1 , 0 , 1 , 72 , 2 , 46 , 73 , 96 ,
    3 , 14 , 47 , 56 , 74 , 18 , 97 , 118 ,
    4 , 43 , 15 , 35 , 48 , 38 , 57 , 23 ,
    75 , 92 , 19 , 86 , 98 , 51 , 119 , 29 ,
    5 , -1 , 44 , 12 , 16 , 41 , 36 , 90 ,
    49 , 126 , 39 , 124 , 58 , 60 , 24 , 105 ,
    76 , 62 , 93 , 115 , 20 , 26 , 87 , 102 ,
    99 , 107 , 52 , 82 , 120 , 78 , 30 , 110 ,
    6 , 64 , -1 , 71 , 45 , 95 , 13 , 55 ,
    17 , 117 , 42 , 34 , 37 , 22 , 91 , 85 ,
    50 , 28 , 127 , 11 , 40 , 89 , 125 , 123 ,
    59 , 104 , 61 , 114 , 25 , 101 , 106 , 81 ,
    77 , 109 , 63 , 70 , 94 , 54 , 116 , 33 ,
    21 , 84 , 27 , 10 , 88 , 122 , 103 , 113 ,
    100 , 80 , 108 , 69 , 53 , 32 , 83 , 9 ,
    121 , 112 , 79 , 68 , 31 , 8 , 111 , 67 ,
    7 , 66 , 65 ,

\ Find the index of the least significant 1 bit
\ If u == 0, returns -1
: bitscan-forward ( u -- u )
    dup negate and \ LS1B isolation
    131 mod \ perfect hashing
    cells bsf-modulo-131-table + @
; export


create msb-table
    -1 , 0 , 1 , 1 , 2 , 2 , 2 , 2 , 3 , 3 , 3 , 3 , 3 , 3 , 3 , 3 ,
    4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 ,
    5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 ,
    5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 ,
    6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 ,
    6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 ,
    6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 ,
    6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,
    7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 , 7 ,

\ Find the index of the most significant 1 bit
\ If u == 0, returns -1
: bitscan-reverse ( u -- u )
    dup 0xff u< if cells msb-table + @ exit then
; export

}private

T{ 0 bitscan-forward -> -1 }T
T{ 1 bitscan-forward -> 0 }T
T{ 2 bitscan-forward -> 1 }T
T{ 3 bitscan-forward -> 0 }T
T{ 4 bitscan-forward -> 2 }T
T{ 5 bitscan-forward -> 0 }T
T{ -1 bitscan-forward -> 0 }T
T{ max-int bitscan-forward -> 0 }T
T{ min-int bitscan-forward -> cell 8 * 1- }T

T{ 0 bitscan-reverse -> -1 }T
T{ 1 bitscan-reverse -> 0 }T
T{ 2 bitscan-reverse -> 1 }T
T{ 3 bitscan-reverse -> 1 }T
T{ 4 bitscan-reverse -> 2 }T
T{ -1 bitscan-reverse -> cell 8 * 1- }T
T{ max-int bitscan-reverse -> cell 8 * 2 - }T
T{ min-int bitscan-reverse -> cell 8 * 1- }T
