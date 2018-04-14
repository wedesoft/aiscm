;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
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
  #:export (toplevel-define! super gc-malloc gc-malloc-pointerless destroy xor attach index-of all-but-last
            drop-up-to take-up-to flatten cycle uncycle cycle-times integral alist-invert
            assq-set assq-remove product sort-by sort-by-pred partial-sort argmin argmax gather
            pair->list nodes live-intervals overlap-interval overlap color-intervals union difference fixed-point
            first-index last-index compact
            bytevector-sub bytevector-concat objdump map-if map-select aiscm-error symbol-list typed-header typed-header2
            clock elapsed object-slots scm->address address->scm list-with)
  #:export-syntax (define-class* template-class synchronise define-typed-method define-nary-typed-method))


(load-extension "libguile-aiscm-util" "init_util")

(define (toplevel-define! name val)
  "Create a definition in the top level context"
  (module-define! (current-module) name val) val)

(define (super class)
  "Get (first) direct super class of a class"
  (let [(supers (class-direct-supers class))]
    (if (null? supers)
      (scm-error 'misc-error 'super "Class ~a has no super class" (list class) #f)
      (car supers))))

(define-syntax-rule (define-class* name super metaname metasuper slots ...)
  "Define a class with corresponding meta class and set up inheritance for both classes"
  (begin
    (define-class metaname (metasuper))
    (define-class name (super) slots ...  #:metaclass metaname)))

(define-method (member-string x)
  "Convert value to string"
  (format #f "~a" x))
(define-method (member-string (x <class>))
  "Get class name and remove leading '<' character and trailing '>' character"
  (let [(name (format #f "~a" (class-name x)))]
    (xsubstring name 1 (1- (string-length name)))))

(define-syntax-rule (template-class (base args ...) super finaliser ...)
  "Define a parametric class deriving from BASE using multiple ARGS as parameters"
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
    (and tail (length (cdr tail)))))

(define (all-but-last lst)
  "Return all but last element of LST."
  (reverse (cdr (reverse lst))))

(define (drop-up-to lst n)
  "Return the list LST after dropping up to N elements."
  (if (null? lst) lst (if (zero? n) lst (drop-up-to (cdr lst) (1- n)))))

(define (take-up-to lst n)
  "Return up to N elements of the list LST."
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
(define (assq-remove alist . keys) (filter (compose not (cut memv <> keys) car) alist))
(define (product lst1 lst2) (append-map (lambda (x) (map (cut cons x <>) lst2)) lst1))

(define (sort-by lst fun)
  "Sort LST by return values of FUN"
  (sort-list lst (lambda args (apply < (map fun args)))))

(define (sort-by-pred lst pred)
  "Sort LST by boolean return value of PRED"
  (sort-by lst (lambda (arg) (if (pred arg) 1 0))))

(define (partial-sort lst less)
  "Sort the list LST. LESS is a partial order used for comparing elements."
  (or (and (null? lst) '())
      (if (every (compose not (cut less <> (car lst))) (cdr lst))
          (cons (car lst) (partial-sort (cdr lst) less))
          (partial-sort (cycle lst) less))))

(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))
(define (argmin fun lst) (argop min fun lst))
(define (argmax fun lst) (argop max fun lst))
(define (gather sizes lst)
  (if (null? sizes) '()
    (let [(n (car sizes))]
      (cons (list-head lst n) (gather (cdr sizes) (list-tail lst n))))))
(define (fixed-point initial iteration compare?)
  (let [(successor (iteration initial))]
    (if (compare? initial successor)
      initial
      (fixed-point successor iteration compare?))))
(define (union . args) (apply lset-union (cons equal? args)))
(define (difference . args) (apply lset-difference (cons equal? args)))
(define (pair->list pair) (list (car pair) (cdr pair)))
(define (live-intervals live variables)
  (map
    (lambda (v) (cons v (cons (first-index (cut memv v <>) live) (last-index (cut memv v <>) live))))
    variables))

(define ((overlap-interval intervals) interval)
  "Get list of variables with overlapping intervals"
  (map car (filter (lambda (x) (and (>= (cddr x) (car interval))
                                    (<= (cadr x) (cdr interval)))) intervals)))

(define (first-index pred lst)
  (and (not (null? lst))
       (if (pred (car lst))
           0
           (let [(idx (first-index pred (cdr lst)))]
             (and idx (1+ idx))))))
(define (last-index pred lst)
  (and (not (null? lst))
       (let [(idx (last-index pred (cdr lst)))]
         (if idx (1+ idx)
           (and (pred (car lst)) 0)))))
(define (compact . args) (filter identity args))

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

(define (objdump code)
  "dump machine CODE to temporary file and deassemble using the 'objdump' tool"
  (let [(filename (string-append (tmpnam) ".bin"))]
    (call-with-output-file filename (cut put-bytevector <> (u8-list->bytevector (flatten code))))
    (system (format #f "objdump -D -b binary -Mintel -mi386:x86-64 ~a" filename))
    code))

(define (map-if pred fun1 fun2 . lsts) (apply map (lambda args (apply (if (apply pred args) fun1 fun2) args)) lsts))
(define (map-select select fun1 fun2 . lsts) (apply map (lambda (val . args) (apply (if val fun1 fun2) args)) select lsts))
(define (symbol-list n) (map (lambda _ (gensym)) (iota n)))
(define (typed-header lst tag) (map (cut list <> tag) lst))
(define (typed-header2 lst tags) (map list lst tags))
(define (delete-ref lst k) (if (zero? k) (cdr lst) (cons (car lst) (delete-ref (cdr lst) (1- k)))))
(define (aiscm-error context msg . args) (scm-error 'misc-error context msg args #f)); also see source code of srfi-37

(define (clock)
  "Get current time with high precision"
  (current-time))

(define* (elapsed reference #:optional (reset #f))
  "Return time elapsed and optionally reset the clock"
  (let [(difference (time-difference (current-time) reference))]
    (if reset (add-duration! reference difference))
    (+ (time-second difference) (* 1e-9 (time-nanosecond difference)))))

(define-syntax-rule (synchronise expr time-remaining method)
  "Abstract time synchronisation method"
  (let [(result expr)]
    (method (max 0 time-remaining))
    result))

(define (object-slots obj) (map (compose (cut slot-ref obj <>) slot-definition-name) (class-slots (class-of obj))))

(define (scm->address scm) (pointer-address (scm->pointer scm)))
(define (address->scm address) (pointer->scm (make-pointer address)))

(define (list-with lst idx val)
  (if (null? lst) lst (cons (if (zero? idx) val (car lst)) (list-with (cdr lst) (1- idx) val))))

(define-syntax define-typed-method
  (lambda (x)
    (syntax-case x ()
      ((k name types fun)
       (let* [(args   (symbol-list (length (syntax->datum #'types))))
              (header (map list args (syntax->datum #'types)))]
         #`(define-method (name #,@(datum->syntax #'k header)) (fun #,@(datum->syntax #'k args))))))))

(define-macro (define-nary-typed-method name arity type fun)
  "Define an n-ary method with arguments of a specified type"
  `(define-typed-method ,name ,(make-list arity type) ,fun))
