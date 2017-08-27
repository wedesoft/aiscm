(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))
(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm element) (aiscm int) (aiscm sequence) (aiscm pointer) (aiscm expression) (aiscm operation) (aiscm util))


(define ctx (make <context>))

(define-method (set3 (self <element>) value)
  (slot-set! self 'value value))

(define-method (set3 (self <sequence<>>) value)
  (let* [(classes      (list (class-of self) (class-of (wrap value))))
         (args         (map skeleton classes))
         (parameters   (map parameter args))
         (commands     (virtual-variables '() (content-vars args) (attach (apply duplicate parameters) (RET))))
         (instructions (asm ctx <null> (map typecode (content-vars args)) commands))]
    (apply instructions (append-map unbuild classes (list self value)))
    value))

(define classes (list (sequence <ubyte>) (sequence <ubyte>)))
(define args (map skeleton classes))
(define parameters (map parameter args))
(define commands (virtual-variables '() (content-vars args) (attach (apply duplicate parameters) (RET))))
(define instructions (asm ctx <null> (map typecode (content-vars args)) commands))

(define s (seq 2 3 5))
(define t (seq 3 5 7))
(define header (list s t))

(apply instructions (append-map unbuild classes header))
s

(test-begin "playground")
(test-eqv "set value of integer"
  123 (let [(i (make <int> #:value 0))] (set3 i 123) (get i)))
(test-eqv "return value after setting integer"
  123 (set3 (make <int> #:value 0) 123))
(let [(s (seq 1 2 3))]
  (set3 s 0)
  (test-equal "fill array with zeros"
    '(0 0 0) (to-list s))
  (test-eqv "return value after setting array"
    0 (set3 s 0))
  (set3 s (seq 2 3 5))
  (test-equal "copy content of other array"
    '(2 3 5) (to-list s)))
(test-end "playground")
