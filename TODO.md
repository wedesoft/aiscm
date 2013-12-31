TODO
====

* error handling
* tests
* documentation
* literate programming
* replace malloc C code with bytevectors

```Scheme
(pointer->bytevector (make-pointer (+ 1 (pointer-address (bytevector->pointer #vu8(1 2 3 4))))) 3)
```
