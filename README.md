# PlanckForth: Bootstrapping Forth from Handwritten Executable

```
$ make
```


```
$ cat helloworld.fs | ./plank
```

# Builtin Words

| code | name     | stack effect   | semantics                    |
|:----:|:---------|:---------------|:-----------------------------|
| Q    | quit     | ( -- )         | Exit the process             |
| C    | cell     | ( -- n )       | The size of Cells            |
| h    | here     | ( -- addr )    | The address of 'here' cell   |
| l    | latest   | ( -- addr )    | The address of 'latest' cell |
| k    | key      | ( -- n )       | Read character               |
| t    | type     | ( n -- )       | Print character              |
| j    | jump     | ( -- )         | Unconditional branch.        |
| J    | 0jump    | ( a -- )       | Jump if a == 0.              |
| f    | find     | ( c -- xt )    | Get execution token of c     |
| x    | execute  | ( xt -- ... )  | Run the execution token      |
| @    | fetch    | ( addr -- a )  | Load value from addr         |
| !    | store    | ( a addr -- )  | Store value to addr          |
| ?    | cfetch   | ( addr -- c )  | Load byte from addr          |
| $    | cstore   | ( c addr -- )  | Store byte to addr           |
| d    | dfetch   | ( -- addr )    | Get data stack pointer       |
| D    | dstore   | ( addr -- )    | Set data stack pointer       |
| r    | rfetch   | ( -- addr )    | Get return stack pointer     |
| R    | rstore   | ( addr -- )    | Set return stack pointer     |
| i    | docol    | ( -- addr )    | Get the interpreter function |
| e    | exit     | ( -- )         | Exit current function        |
| L    | lit      | ( -- a )       | Load immediate               |
| S    | string   | ( -- addr len) | Load string literal          |
| +    | add      | ( a b -- c )   | c = (a + b)                  |
| -    | sub      | ( a b -- c )   | c = (a - b)                  |
| *    | mul      | ( a b -- c )   | c = (a * b)                  |
| /    | div      | ( a b -- c )   | c = (a / b)                  |
| %    | mod      | ( a b -- c )   | c = (a % b)                  |
| &    | and      | ( a b -- c )   | c = (a & b)                  |
| \|   | or       | ( a b -- c )   | c = (a \| b)                 |
| ^    | xor      | ( a b -- c )   | c = (a ^ b)                  |
| <    | less     | ( a b -- c )   | c = (a < b)                  |
| =    | equal    | ( a b -- c )   | c = (a == b)                 |
