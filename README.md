# PlanckForth: Bootstrapping Forth from Handwritten Binary

```
$ make
```


```
$ cat helloworld.fs | ./plank
```

# Builtin Words

| code | name     | stack effect  | semantics                      |
|:----:|:---------|:--------------|:-------------------------------|
| Q    | quit     | ( -- )        | Exit the process               |
| C    | cell     | ( -- n )      | The size of Cells              |
| h    | here     | ( -- addr )   | The address of 'here' cell     |
| l    | latest   | ( -- addr )   | The address of 'latest' cell   |
| k    | key      | ( -- n )      | Read character                 |
| t    | type     | ( n -- )      | Print character                |
| j    | jump     | ( -- )        | Unconditional branch.          |
| J    | 0jump    | ( a -- )      | Jump if a == 0.                |
| f    | find     | ( c -- xt )   | Get execution token of c       |
| x    | execute  | ( xt -- ... ) | Run the execution token        |
| @    | fetch    | ( addr -- a ) | Load value from addr           |
| !    | store    | ( a addr -- ) | Store value to addr            |
| ?    | cfetch   | ( addr -- c ) | Load byte from addr            |
| $    | cstore   | ( c addr -- ) | Store byte to addr             |
| d    | dfetch   | ( -- addr )   | Get data stack pointer         |
| D    | dstore   | ( addr -- )   | Set data stack pointer         |
