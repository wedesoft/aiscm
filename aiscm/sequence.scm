;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm sequence)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:export (<meta<sequence<>>> <sequence<>>
            sequence seq multiarray to-list to-array default-strides
            dump crop project roll unroll downsample dimension stride)
  #:export-syntax (arr)
  #:re-export (<pointer<element>> <meta<pointer<element>>>
               <pointer<int<>>> <meta<pointer<int<>>>>))
(define-class* <sequence<>> <element> <meta<sequence<>>> <meta<element>>
              (shape #:init-keyword #:shape #:getter shape)
              (strides #:init-keyword #:strides #:getter strides))
(define (default-strides shape)
  (map (compose (cut apply * <>) (cut take shape <>)) (iota (length shape))))
(define (sequence type . args)
  (if (is-a? type <meta<element>>)
    (if (null? args)
      (template-class (sequence type) <sequence<>>
        (lambda (class metaclass)
          (define-method (initialize (self class) initargs)
            (let-keywords initargs #f (shape size value strides)
              (let* [(value   (or value (make <mem>
                                         #:pointerless (pointerless? type)
                                         #:size (* (size-of (typecode type))
                                                   (or size (apply * shape))))))
                     (shape   (or shape (list size)))
                     (strides (or strides (default-strides shape)))]
                (next-method self (list #:value value #:shape shape #:strides strides)))))
          (define-method (project (self metaclass)) (pointer type))
          (define-method (dimensions (self metaclass)) (1+ (dimensions type)))
          (define-method (typecode (self metaclass)) (typecode type))))
      (to-array type args))
    (to-array (cons type args))))

(define seq sequence)

(define-syntax-rule (arr arg1 args ...)
  (if (is-a? (quote arg1) <symbol>) (to-array arg1 '(args ...)) (to-array '(arg1 args ...))))

(define-method (pointerless? (self <meta<sequence<>>>)) (pointerless? (typecode self)))

(define-method (pointer (target-class <meta<sequence<>>>)) target-class)
(define (multiarray type dimensions)
  (if (zero? dimensions) (pointer type) (multiarray (sequence type) (1- dimensions))))
(define-method (stride (self <sequence<>>)) (last (strides self)))
(define-method (dimension (self <sequence<>>)) (last (shape self)))
(define-method (size (self <sequence<>>)) (apply * (shape self)))
(define-method (size-of (self <sequence<>>)) (* (size self) (size-of (typecode self))))
(define-method (project (self <sequence<>>))
  (make (project (class-of self))
        #:value   (value self)
        #:shape   (all-but-last (shape self))
        #:strides (all-but-last (strides self))))
(define-method (crop (n <integer>) (self <sequence<>>))
  (make (class-of self)
        #:value   (value self)
        #:shape   (attach (all-but-last (shape self)) n)
        #:strides (strides self)))
(define-method (crop (n <null>) (self <sequence<>>)) self)
(define-method (crop (n <pair>) (self <sequence<>>))
  (crop (last n) (roll (crop (all-but-last n) (unroll self)))))
(define-method (rebase value (self <sequence<>>))
  (make (class-of self) #:value value #:shape (shape self) #:strides (strides self)))
(define-method (dump (offset <integer>) (self <sequence<>>))
  (let [(value (+ (value self) (* offset (last (strides self)) (size-of (typecode self)))))]
    (rebase value (crop (- (last (shape self)) offset) self))))
(define-method (dump (n <null>) (self <sequence<>>)) self)
(define-method (dump (n <pair>) (self <sequence<>>))
  (dump (last n) (roll (dump (all-but-last n) (unroll self)))))
(define (element offset self) (project (dump offset self)))
(define-method (fetch (self <sequence<>>)) self)
(define-method (get (self <sequence<>>) . args)
  (if (null? args) self (get (fetch (fold-right element self args)))))
(define-method (set (self <sequence<>>) . args)
  (store (fold-right element self (all-but-last args)) (last args)))
(define-method (store (self <sequence<>>) value)
  (for-each (compose (cut store <> value) (cut element <> self))
            (iota (last (shape self))))
  value)
(define-method (store (self <sequence<>>) (value <null>)) value)
(define-method (store (self <sequence<>>) (value <pair>))
  (store (project self) (car value))
  (store (dump 1 self) (cdr value))
  value)
(define-method (to-list self) self)
(define-method (to-list (self <sequence<>>))
  (map (compose to-list (cut get self <>)) (iota (last (shape self)))))
(define-method (shape (self <null>)) #f)
(define-method (shape (self <pair>)) (attach (shape (car self)) (length self)))
(define-method (to-array (lst <list>)) (to-array (apply native-type (flatten lst)) lst))
(define-method (to-array (typecode <meta<element>>) (lst <list>))
  (let* [(shape  (shape lst))
         (retval (make (multiarray typecode (length shape)) #:shape shape))]
    (store retval lst)
    retval))
(define (print-columns self first infix count width port)
  (if (zero? count)
    (begin
      (if first (display "(" port))
      (display ")" port))
    (let* [(text      (call-with-output-string (cut display (get (fetch (project self))) <>)))
           (remaining (- width (string-length text) 1))]
      (display (if first "(" infix) port)
      (if (positive? remaining)
        (begin
          (display text port)
          (print-columns (dump 1 self) #f infix (1- count) remaining port))
        (begin
          (display "...)" port)))))
  1)
(define (print-rows self first infix count height port)
  (if (zero? count)
    (begin
      (if first (display "(" port))
      (if (positive? height) (display ")" port))
      height)
    (if (> height 0)
      (begin
        (display (if first "(" infix) port)
        (let [(h (if (> (dimensions self) 2)
                   (print-rows (project self) #t (string-append infix " ") (last (shape (project self))) height port)
                   (begin
                     (print-columns (project self) #t " " (car (shape self)) (- 80 2) port)
                     (if (= height 1) (display "\n ..." port))
                     (1- height))))]
          (print-rows (dump 1 self) #f infix (1- count) h port)))
      height)))
(define (print-elements self port)
  (if (> (dimensions self) 1)
    (print-rows self #t "\n " (last (shape self)) (- 11 1) port)
    (print-columns self #t " " (car (shape self)) (- 80 2) port)))
(define-method (write (self <sequence<>>) port)
  (if (is-a? (value self) <mem>)
    (begin (format port "#~a:~%" (class-name (class-of self)))
           (print-elements self port))
    (format port "#<~a ~a ~a ~a>" (class-name (class-of self))
                 (value self) (shape self) (strides self))))
(define-method (coerce (a <meta<sequence<>>>) (b <meta<element>>))
  (multiarray (coerce (typecode a) b) (dimensions a)))
(define-method (coerce (a <meta<element>>) (b <meta<sequence<>>>))
  (multiarray (coerce a (typecode b)) (dimensions b)))
(define-method (coerce (a <meta<sequence<>>>) (b <meta<sequence<>>>))
  (multiarray (coerce (typecode a) (typecode b)) (max (dimensions a) (dimensions b))))
(define (roll self) (make (class-of self)
        #:value   (value self)
        #:shape   (cycle (shape self))
        #:strides (cycle (strides self))))
(define (unroll self) (make (class-of self)
        #:value   (value self)
        #:shape   (uncycle (shape self))
        #:strides (uncycle (strides self))))
(define-method (downsample (n <integer>) (self <sequence<>>))
   (let [(shape   (shape self))
         (strides (strides self))]
     (make (class-of self)
           #:value   (value self)
           #:shape   (attach (all-but-last shape) (quotient (+ (1- n) (last shape)) n))
           #:strides (attach (all-but-last strides) (* n (last strides))))))
(define-method (downsample (n <null>) (self <sequence<>>)) self)
(define-method (downsample (n <pair>) (self <sequence<>>))
  (downsample (last n) (roll (downsample (all-but-last n) (unroll self)))))
(define-method (build (self <meta<sequence<>>>) lst)
  (let [(shape   (reverse (map (cut list-ref lst <>) (iota (dimensions self) 0 2))))
        (strides (reverse (map (cut list-ref lst <>) (iota (dimensions self) 1 2))))]
  (make self #:strides strides
             #:shape shape
             #:value (make <mem> #:base (make-pointer (last lst)) #:size (apply * (size-of (typecode self)) shape)))))
(define-method (unbuild (type <meta<sequence<>>>) self)
  (cons (dimension self)
    (cons (stride self)
      (unbuild (project type) (project self)))))
(define-method (content (type <meta<sequence<>>>) (self <sequence<>>))
  (cons (make <long> #:value (dimension self))
    (cons (make <long> #:value (stride self))
      (content (project type) (project self)))))
(define-method (signed? (self <meta<sequence<>>>)) (signed? (typecode self)))
