# TODO

MOV AX, 42 -> machine code
MOV x, 42 -> data structure
MOV x, (dword-ptr y z *4 1) -> data structure
JMP, JE, ... 'a -> data structure

MOV x, 42: x is output
ADD x, 42: x is input and output
ADD y, x: x is input

pool: just give me 4 registers to work with:

E.g.
(x (param pool <reg<32>>)
(y (param pool <reg<16>>)
(a (get pool <reg<32>>); push outer variables on the stack, return register?
(b (get pool <reg<16>>)
...
Inheriting pool

(define-syntax-rule (environment pool vars body)
  (begin (push pool) (let [(retval (let vars body))] (pop pool) retval))

* register pool
* used registers
* variable stack (push pop)

EAX equal? (make ...)
