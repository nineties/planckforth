# PlanckForth: Bootstrapping Forth from Handwritten Binary

```
$ make
```

# Builtin Words

| code | name     | semantics                                 |
|:----:|:--------:|:-----------------------------------------:|
| Q    | quit     | ( -- ) Exit the process                   |
| C    | cell     | ( -- n ) The size of Cells                |
| h    | here     | ( -- addr ) The address of 'here' cell    |
| l    | latest   | ( -- addr ) The address of 'latest' cell  |
| k    | key      | ( -- n ) Read character                   |
| t    | type     | ( n -- ) Print character                  |
| j    | jump     | ( -- ) Unconditional branch.              |
| J    | 0jump    | ( a -- ) Jump if a == 0.                  |
| f    | find     | ( c -- xt ) Get execution token of c      |
| x    | execute  | ( xt -- ... ) Run the execution token     |
