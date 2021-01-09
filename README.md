# PlanckForth: Bootstrapping Forth from Handwritten Binary

This project aims to bootstrap a Forth interpreter from hand-written tiny ELF binary.

## How to build

You can create a binary for x86 linux environment by following steps.
Only `xxd` is needed to build PlanckForth.

```
$ git clone https://github.com/nineties/planckforth.git
$ cd planckforth
$ make
xxd -r -c 8 planck.xxd > planck
chmod +x planck
```

There are implementations in C and Python 3 as a reference in `others`.
Try `make c` or `make python`

## Hello World

The hello world program at the beginning looks like this.

```
$ ./planck
kHtketkltkltkotk tkWtkotkrtkltkdtk!tk:k0-tQ
```
After bootstrapping by `bootstrap.fs`, it looks like this.

```
$ ./planck < bootstrap.fs --i386-linux
Ready.
." Hello World!" cr
```

The option `--i386-linux` is to specify OS and hardware for dynamic
code generation by `bootstrap.fs`.
It is not necessay in case of implementations by other languages.


`bootstrap.fs` also takes a file as an input like this.

```
$ cat example/fib.fs
: fib dup 2 < unless 1- dup recurse swap 1- recurse + then ;
38 fib . cr
$ ./planck < bootstrap.fs --i386-linux example/fib.fs
39088169
```

# Builtin Words

| code | name      | stack effect    | semantics                    |
|:----:|:----------|:----------------|:-----------------------------|
| Q    | quit      | ( -- )          | Exit the process             |
| C    | cell      | ( -- n )        | The size of Cells            |
| h    | &here     | ( -- a-addr )   | The address of 'here' cell   |
| l    | &latest   | ( -- a-addr )   | The address of 'latest' cell |
| k    | key       | ( -- c )        | Read character               |
| t    | type      | ( c -- )        | Print character              |
| j    | jump      | ( -- )          | Unconditional branch         |
| J    | 0jump     | ( n -- )        | Jump if a == 0               |
| f    | find      | ( c -- xt )     | Get execution token of c     |
| x    | execute   | ( xt -- ... )   | Run the execution token      |
| @    | fetch     | ( a-addr -- w ) | Load value from addr         |
| !    | store     | ( w a-addr -- ) | Store value to addr          |
| ?    | cfetch    | ( c-addr -- c ) | Load byte from addr          |
| $    | cstore    | ( c c-addr -- ) | Store byte to addr           |
| d    | dfetch    | ( -- a-addr )   | Get data stack pointer       |
| D    | dstore    | ( a-addr -- )   | Set data stack pointer       |
| r    | rfetch    | ( -- a-addr )   | Get return stack pointer     |
| R    | rstore    | ( a-addr -- )   | Set return stack pointer     |
| i    | docol     | ( -- a-addr )   | Get the interpreter function |
| e    | exit      | ( -- )          | Exit current function        |
| L    | lit       | ( -- n )        | Load immediate               |
| S    | litstring | ( -- c-addr )   | Load string literal          |
| +    | add       | ( a b -- c )    | c = (a + b)                  |
| -    | sub       | ( a b -- c )    | c = (a - b)                  |
| *    | mul       | ( a b -- c )    | c = (a * b)                  |
| /    | div       | ( a b -- c )    | c = (a / b)                  |
| %    | mod       | ( a b -- c )    | c = (a % b)                  |
| &    | and       | ( a b -- c )    | c = (a & b)                  |
| \|   | or        | ( a b -- c )    | c = (a \| b)                 |
| ^    | xor       | ( a b -- c )    | c = (a ^ b)                  |
| <    | less      | ( a b -- c )    | c = (a < b)                  |
| =    | equal     | ( a b -- c )    | c = (a == b)                 |
