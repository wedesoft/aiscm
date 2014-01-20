TODO
====

* (error ...)
* variable does not need value
* jit: pointers and integers on stack, loops, return values

```Ruby
Sequence[1, 2, 3, 4].to_s
# "Lambda(Variable1(INDEX(INT(4))),Lookup(*(UBYTE)(Malloc(4)),Variable1(INDEX(INT(4))),INT(1)))"
```
