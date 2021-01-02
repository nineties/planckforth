# PlanckForth: Bootstrapping Forth from Handwritten Executable

```
$ git clone https://github.com/nineties/planckforth.git
$ cd planckforth
$ make
$ cat helloworld.fs | ./planck
```

# Builtin Words

| code | name     | stack effect    | semantics                    |
|:----:|:---------|:----------------|:-----------------------------|
| Q    | quit     | ( -- )          | Exit the process             |
| C    | cell     | ( -- n )        | The size of Cells            |
| h    | &here    | ( -- c-addr )   | The address of 'here' cell   |
| l    | &latest  | ( -- c-addr )   | The address of 'latest' cell |
| k    | key      | ( -- c )        | Read character               |
| t    | type     | ( c -- )        | Print character              |
| j    | jump     | ( -- )          | Unconditional branch         |
| J    | 0jump    | ( n -- )        | Jump if a == 0               |
| f    | find     | ( c -- xt )     | Get execution token of c     |
| x    | execute  | ( xt -- ... )   | Run the execution token      |
| @    | fetch    | ( a-addr -- w ) | Load value from addr         |
| !    | store    | ( w a-addr -- ) | Store value to addr          |
| ?    | cfetch   | ( c-addr -- c ) | Load byte from addr          |
| $    | cstore   | ( c c-addr -- ) | Store byte to addr           |
| d    | dfetch   | ( -- a-addr )   | Get data stack pointer       |
| D    | dstore   | ( a-addr -- )   | Set data stack pointer       |
| r    | rfetch   | ( -- a-addr )   | Get return stack pointer     |
| R    | rstore   | ( a-addr -- )   | Set return stack pointer     |
| i    | docol    | ( -- a-addr )   | Get the interpreter function |
| e    | exit     | ( -- )          | Exit current function        |
| L    | lit      | ( -- a )        | Load immediate               |
| S    | string   | ( -- c-addr u ) | Load string literal          |
| +    | add      | ( a b -- c )    | c = (a + b)                  |
| -    | sub      | ( a b -- c )    | c = (a - b)                  |
| *    | mul      | ( a b -- c )    | c = (a * b)                  |
| /    | div      | ( a b -- c )    | c = (a / b)                  |
| %    | mod      | ( a b -- c )    | c = (a % b)                  |
| &    | and      | ( a b -- c )    | c = (a & b)                  |
| \|   | or       | ( a b -- c )    | c = (a \| b)                 |
| ^    | xor      | ( a b -- c )    | c = (a ^ b)                  |
| <    | less     | ( a b -- c )    | c = (a < b)                  |
| =    | equal    | ( a b -- c )    | c = (a == b)                 |
