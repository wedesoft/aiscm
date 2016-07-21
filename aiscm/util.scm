(define-module (aiscm util)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 curried-definitions)
  #:use-module (system foreign)
  #:export (toplevel-define! super gc-malloc-pointerless destroy xor attach index-of all-but-last
            drop-up-to take-up-to flatten cycle uncycle cycle-times integral alist-invert
            assq-set assq-remove product sort-by sort-by-pred argmin argmax gather
            pair->list nodes live-intervals overlap color-intervals union difference fixed-point
            first-index last-index compact index-groups update-intervals
            bytevector-sub bytevector-concat objdump map-if map-select aiscm-error symbol-list typed-header
            clock elapsed)
  #:export-syntax (define-class* template-class synchronise))
(load-extension "libguile-aiscm-util" "init_util")
(define (toplevel-define! name val)
  (module-define! (current-module) name val) val)
(define (super class)
  (let [(supers (class-direct-supers class))]
    (if (null? supers)
      (scm-error 'misc-error 'super "Class ~a has no super class" (list class) #f)
      (car supers))))
(define-syntax-rule (define-class* name super metaname metasuper slots ...)
  (begin
    (define-class metaname (metasuper))
    (define-class name (super) slots ...  #:metaclass metaname)))
(define-method (member-string x) (format #f "~a" x))
(define-method (member-string (x <class>))
  (let [(name (format #f "~a" (class-name x)))]
    (xsubstring name 1 (1- (string-length name)))))
(define-syntax-rule (template-class (base args ...) super finaliser ...)
  (let* [(members  (map member-string (list args ...)))
         (name     (string->symbol (format #f "<~a<~a>>" (quote base) (string-join members ","))))
         (metaname (string->symbol (format #f "<meta~a>" name)))]
    (if (not (defined? name (current-module)))
      (let* [(metaclass (make <class> #:dsupers (list (class-of super)) #:name metaname))
             (class     (make metaclass #:dsupers (list super) #:name name))]
        (toplevel-define! metaname metaclass)
        (toplevel-define! name class)
        (for-each (lambda (fun) (fun class metaclass)) (list finaliser ...))
        class)
      (primitive-eval name))))
(define-generic destroy)
(define (xor a b) (not (eq? a b)))
(define (attach lst x) (reverse (cons x (reverse lst))))
(define (index-of a b)
  (let [(tail (member a (reverse b)))]
    (if tail (length (cdr tail)) #f)))
(define all-but-last (compose reverse cdr reverse))
(define (drop-up-to lst n)
  (if (null? lst) lst (if (zero? n) lst (drop-up-to (cdr lst) (1- n)))))
(define (take-up-to lst n)
  (if (zero? n) '() (if (null? lst) lst (cons (car lst) (take-up-to (cdr lst) (1- n))))))
(define (flatten x)
  (cond ((null? x) x)
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else      (list x))))
(define (cycle lst) (attach (cdr lst) (car lst)))
(define (uncycle lst) (cons (last lst) (all-but-last lst)))
(define (cycle-times lst n)
  (cond ((> n 0) (cycle-times (cycle   lst) (1- n)))
        ((< n 0) (cycle-times (uncycle lst) (1+ n)))
        (else    lst)))
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
(define (assq-remove alist key) (filter (compose not (cut eq? key <>) car) alist))
(define (product lst1 lst2) (append-map (lambda (x) (map (cut cons x <>) lst2)) lst1))
(define (sort-by lst fun) (sort-list lst (lambda args (apply < (map fun args)))))
(define (sort-by-pred lst pred) (sort-by lst (lambda (arg) (if (pred arg) 1 0))))
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
(define (live-intervals live variables)
  (map
    (lambda (v) (cons v (cons (first-index (cut memv v <>) live) (last-index (cut memv v <>) live))))
    variables))
(define ((overlap-interval intervals) interval)
  (map car (filter (lambda (x) (and (>= (cddr x) (car interval))
                                    (<= (cadr x) (cdr interval)))) intervals)))
(define ((overlap intervals) var)
  (let [(interval (assq-ref intervals var))]
    ((overlap-interval intervals) interval)))
(define* (color-intervals intervals nodes colors #:key (predefined '()) (blocked '()))
  (if (null? nodes) predefined
    (let* [(target    (argmin (compose length (overlap intervals)) nodes))
           (color-map (color-intervals (assq-remove intervals target)
                                       (delete target nodes)
                                       colors
                                       #:predefined predefined
                                       #:blocked blocked))
           (adjacent  ((overlap intervals) target))
           (busy      (append (map (cut assq-ref color-map <>) adjacent)
                              ((overlap-interval blocked) (assq-ref intervals target))))
           (available (find (negate (cut memv <> busy)) colors))]
      (cons (cons target available) color-map))))
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
(define (index-groups lst)
  (let* [(length-list       (map length lst))
         (integrated-length (integral length-list))
         (start-points      (cons 0 (all-but-last integrated-length)))
         (end-points        (map 1- integrated-length))]
    (map cons start-points end-points)))
(define (update-intervals intervals groups)
  (map (lambda (pair)
         (cons (car pair)
               (cons (car (list-ref groups (cadr pair)))
                     (cdr (list-ref groups (cddr pair)))))) intervals))
(define (bytevector-sub bv offset len)
  (let [(retval (make-bytevector len))]
    (bytevector-copy! bv offset retval 0 len)
    retval))
(define (bytevector-concat lst)
  (let* [(lengths (map bytevector-length lst))
         (steps   (integral lengths))
         (offsets (cons 0 (all-but-last steps)))
         (retval  (make-bytevector (last steps)))]
    (for-each (lambda (arg offset len) (bytevector-copy! arg 0 retval offset len))
              lst offsets lengths)
    retval))
(define (objdump code) (let [(filename (tmpnam))]
  (call-with-output-file filename (cut put-bytevector <> (u8-list->bytevector (flatten code))))
  (system (format #f "objdump -D -b binary -Mintel -mi386:x86-64 ~a" filename))))
(define (map-if pred fun1 fun2 . lsts) (apply map (lambda args (apply (if (apply pred args) fun1 fun2) args)) lsts))
(define (map-select select fun1 fun2 . lsts) (apply map (lambda (val . args) (apply (if val fun1 fun2) args)) select lsts))
(define (symbol-list n) (map (lambda _ (gensym)) (iota n)))
(define (typed-header lst tag) (map (cut list <> tag) lst))
(define (delete-ref lst k) (if (zero? k) (cdr lst) (cons (car lst) (delete-ref (cdr lst) (1- k)))))
(define (aiscm-error context msg . args) (scm-error 'misc-error context msg args #f)); also see source code of srfi-37
(define (clock) (current-time))
(define (elapsed since)
  (let [(difference (time-difference (current-time) since))]
    (+ (time-second difference) (* 1e-9 (time-nanosecond difference)))))
(define-syntax-rule (synchronise expr time-remaining method)
  (let [(result expr)]
    (method (max 0 time-remaining))
    result))
