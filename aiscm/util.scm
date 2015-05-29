(define-module (aiscm util)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (toplevel-define! malloc destroy attach index all-but-last
            drop-up-to take-up-to flatten cycle uncycle integral alist-invert
            assq-set assv-set assoc-set product sort-by argmin argmax gather
            pair->list nodes adjacent color-graph union difference fixed-point
            first-index last-index compact)
  #:export-syntax (def-once expand))
(define (toplevel-define! name val)
  (module-define! (current-module) name val))
(define-syntax-rule (def-once name value)
  (let [(sym (string->symbol name))]
    (if (not (defined? sym (current-module)))
      (toplevel-define! sym value))
    (primitive-eval sym)))
(define (malloc size) (bytevector->pointer (make-bytevector size)))
(define-generic destroy)
(define (attach lst x) (reverse (cons x (reverse lst))))
(define (index a b)
  (let [(tail (member a (reverse b)))]
    (if tail (length (cdr tail)) #f)))
(define all-but-last (compose reverse cdr reverse))
(define (drop-up-to lst n)
  (if (null? lst) lst (if (zero? n) lst (drop-up-to (cdr lst) (1- n)))))
(define (take-up-to lst n)
  (if (zero? n) '() (if (null? lst) lst (cons (car lst) (take-up-to (cdr lst) (1- n))))))
(define-syntax-rule (expand n expr) (map (lambda (tmp) expr) (iota n)))
(define (flatten x)
  (cond ((null? x) x)
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else      (list x))))
(define (cycle lst) (attach (cdr lst) (car lst)))
(define (uncycle lst) (cons (last lst) (all-but-last lst)))
(define (integral lst)
  (letrec [(accumulate (lambda (lst x)
                         (if (null? lst)
                           lst
                           (let [(xs (+ (car lst) x))]
                             (cons xs (accumulate (cdr lst) xs))))))]
    (accumulate lst 0)))
(define (alist-invert alist)
  (map (lambda (x) (cons (cdr x) (car x))) alist))
(define (alist-set = alist key val)
  (if (null? alist)
    (list (cons key val))
    (if (= (caar alist) key)
      (cons (cons key val) (cdr alist))
      (cons (car alist) (assq-set (cdr alist) key val)))))
(define (assq-set alist key val) (alist-set eq? alist key val))
(define (assv-set alist key val) (alist-set eqv? alist key val))
(define (assoc-set alist key val) (alist-set equal? alist key val))
(define (product lst1 lst2) (concatenate (map (lambda (x) (map (cut cons x <>) lst2)) lst1)))
(define (sort-by lst fun) (sort-list lst (lambda args (apply < (map fun args)))))
(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))
(define (argmin fun lst) (argop min fun lst))
(define (argmax fun lst) (argop max fun lst))
(define (gather sizes lst)
  (if (null? sizes) '()
    (let [(n (car sizes))]
      (cons (take lst n) (gather (cdr sizes) (drop lst n))))))
(define (fixed-point initial iteration compare?)
  (let [(successor (iteration initial))]
    (if (compare? initial successor)
      initial
      (fixed-point successor iteration compare?))))
(define (union . args) (apply lset-union (cons eq? args)))
(define (difference . args) (apply lset-difference (cons eq? args)))
(define (pair->list pair) (list (car pair) (cdr pair)))
(define (nodes graph) (delete-duplicates (append (map car graph) (map cdr graph))))
(define ((adjacent graph) node)
  (nodes (filter (compose (cut memv node <>) pair->list) graph)))
(define* (color-graph conflicts nodes colors #:key (predefined '()))
  (if (null? nodes) predefined
    (let* [(target    (argmin (compose length conflicts) nodes))
           (coloring  (color-graph conflicts (delete target nodes) colors #:predefined predefined)); TODO: remove target from conflicts
           (blocked   (map (cut assq-ref coloring <>) (conflicts target)))
           (available (find (negate (cut memv <> blocked)) colors))]
      (cons (cons target available) coloring))))
(define (first-index pred lst)
  (if (null? lst) #f
    (if (pred (car lst)) 0
      (let [(idx (first-index pred (cdr lst)))]
        (if idx (1+ idx) #f)))))
(define (last-index pred lst)
  (if (null? lst) #f
    (let [(idx (last-index pred (cdr lst)))]
      (if idx (1+ idx)
        (if (pred (car lst)) 0 #f)))))
(define (compact . args) (filter identity args))
