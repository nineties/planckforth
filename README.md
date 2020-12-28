# PlanckForth: Bootstrapping Forth from Handwritten Binary

```
$ make
```

# Builtin Words

| char | semantics                                 |
|:----:|:-----------------------------------------:|
| Q    | ( -- ) Exit the process                   |
| C    | ( -- n ) The size of Cells                |
| h    | ( -- addr ) The address of 'here' cell    |
| l    | ( -- addr ) The address of 'latest' cell  |
