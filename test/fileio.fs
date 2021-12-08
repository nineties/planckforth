\ planckforth -
\ Copyright (C) 2021 nineties

testing file I/O

T{
    s" test/fileio-test0.txt" R/O open-file throw
    constant FILE0 ->
}T
T{ 32 allocate throw constant BUF -> }T
T{ BUF 32 FILE0 read-file throw -> 27 }T
T{ s" ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" BUF 27 strneq -> true }T
T{ FILE0 close-file throw -> }T
T{ BUF free -> }T
