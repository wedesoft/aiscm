(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))
(use-modules (oop goops) (aiscm asm) (aiscm jit) (aiscm element) (aiscm int) (aiscm sequence) (aiscm pointer) (aiscm expression) (aiscm operation) (aiscm util))

(define-method (element (offset <integer>) self) (project (dump offset self)))
(define-method (element (offset <pair>) self) (crop (- (cdr offset) (car offset)) (dump (car offset) self)))

(define ctx (make <context>))

; TODO: cache
; TODO: ranges for element, set, and get

(define (compiled-copy self value)
  (let* [(classes      (list (class-of self) (class-of (wrap value))))
           (args         (map skeleton classes))
           (parameters   (map parameter args))
           (commands     (virtual-variables '() (content-vars args) (attach (apply duplicate parameters) (RET))))
           (instructions (asm ctx <null> (map typecode (content-vars args)) commands))]
      (apply instructions (append-map unbuild classes (list self value)))
      value))

(define-method (get (self <sequence<>>) . args)
  (if (null? args) self (get (fetch (fold-right element self args)))))

(define-method (set3 (self <element>) value)
  (slot-set! self 'value value))

(define-method (set3 (self <pointer<>>) value)
  (compiled-copy self value))

(define-method (set3 (self <sequence<>>) . args)
  (compiled-copy (fold-right element self (all-but-last args)) (last args)))

(test-begin "playground")
(test-equal "'get' without additional arguments should return the sequence itself"
  '(1 2 3) (to-list (get (seq 1 2 3))))
(test-equal "Content of converted 2D array"
  '((1 2 3) (4 5 6)) (to-list (arr (1 2 3) (4 5 6))))
(test-equal "Getting row of 2D array"
  '(4 5 6) (to-list (get (arr (1 2 3) (4 5 6)) 1)))
(test-equal "Getting element of 2D array with one call to 'get'"
  2 (get (arr (1 2 3) (4 5 6)) 1 0))
(test-equal "Getting element of 2D array with two calls to 'get'"
  2 (get (get (arr (1 2 3) (4 5 6)) 0) 1))
(test-equal "Get range of values from 1D array"
  '(3 5) (to-list (get (seq 2 3 5 7) '(1 . 3))))

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
  (test-equal "return value when setting array"
    '(2 3 5) (to-list (set3 s (seq 2 3 5))))
  (test-equal "copy content of other array"
    '(2 3 5) (to-list s)))
(let [(m (arr (1 2 3) (4 5 6)))]
  (test-equal "return value when setting 2D array"
    '(7 8) (to-list (set3 m (seq 7 8))))
  (test-equal "set values of 2D array using 1D array"
    '((7 7 7) (8 8 8)) (to-list m)))
(let [(s (seq 1 2 3))]
  (set3 s 1 5)
  (test-equal "set one value of a 1D array"
    '(1 5 3) (to-list s)))
(let [(m (arr (1 2 3) (4 5 6)))]
  (set3 m 0 1 7)
  (test-equal "set one value of a 2D array"
    '((1 2 3) (7 5 6)) (to-list m)))
(test-end "playground")
