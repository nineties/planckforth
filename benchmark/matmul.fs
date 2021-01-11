60 constant M
90 constant K
60 constant N

M K * cells allocate throw constant mat1
K N * cells allocate throw constant mat2
M N * cells allocate throw constant mat3

:noname
    M 0 do
        K 0 do
            i j + 1+ mat1 K j * i + cells + !
        loop
    loop

    K 0 do
        N 0 do
            i j + 1+ mat2 N j * i + cells + !
        loop
    loop

    M 0 do
        N 0 do
            0
            K 0 do
                mat1 K k * i + cells + @
                mat2 N i * j + cells + @
                * +
            loop
            mat3 N i * j + cells + !
        loop
    loop
; execute
