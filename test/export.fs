\ planckforth -
\ Copyright (C) 2021 nineties

testing private and export

: f1 ;
private{

    : f2 ;

    T{ defined? f1 -> true }T
    T{ defined? f2 -> true }T

    private{

        : f3 ;

        T{ defined? f2 -> true }T
        T{ defined? f3 -> true }T

    }private

    T{ defined? f1 -> true }T
    T{ defined? f2 -> true }T
    T{ defined? f3 -> false }T

}private

T{ defined? f1 -> true }T
T{ defined? f2 -> false }T
T{ defined? f3 -> false }T
