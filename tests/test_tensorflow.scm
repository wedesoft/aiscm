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
(use-modules (srfi srfi-64)
             (oop goops)
             (aiscm tensorflow)
             (aiscm core))

(test-begin "aiscm tensorflow")

(test-group "tensor type"
  (test-assert "conversion to tensor creates something"
    (to-tensor (arr 2 3 5)))
  (for-each (lambda (type)
    (test-equal (format #f "round trip of tensor type ~a" (class-name type))
      type (typecode (from-tensor (to-tensor (make (multiarray type 1) #:shape '(3)))))))
      (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long> <float> <double>))
  (test-equal "round trip of tensor shape"
    '(2 3 5) (shape (from-tensor (to-tensor (make (multiarray <int> 3) #:shape '(2 3 5))))))
  (test-equal "round trip of tensor data"
    '(2 3 5) (to-list (from-tensor (to-tensor (arr <int> 2 3 5)))))
  (test-equal "round trip of scalar value"
    42.0 (from-tensor (to-tensor 42.0)))
  (test-equal "ensure row-major representation"
    '((1 3) (2 4)) (to-list (from-tensor (to-tensor (roll (arr (1 2) (3 4))))))))

(test-group "build graph"
  (test-assert "create placeholder"
    (tf-placeholder #:dtype <float>))
  (test-error "error creating placeholder without type argument"
    'misc-error (tf-placeholder))
  (test-assert "create identity"
    (tf-identity (tf-placeholder #:dtype <float>)))
  (test-assert "create identity with type argument"
    (tf-identity (tf-placeholder #:dtype <float>) #:T <float>))
  (test-error "error if type mismatch is encountered"
    'misc-error (tf-identity (placeholder #:dtype <float>) #:T <double>)))

(test-group "run session"
  (test-assert "create session"
    (make-session))
  (test-eqv "run trivial session"
    42.0
    (let* [(s (make-session))
           (p (tf-placeholder #:dtype <double>))]
      (run s (list (cons p 42.0)) (tf-identity p))))
  (test-equal "run trivial session with list of outputs"
    (list 42.0 42.0)
    (let* [(s (make-session))
           (p (tf-placeholder #:dtype <double>))]
      (run s (list (cons p 42.0)) (list (tf-identity p) (tf-identity p))))))

(test-group "variables and constants"
  (test-assert "create variable"
    (tf-variable #:dtype <float> #:shape '(3 2)))
  (test-error "error using uninitialised variable"
    'misc-error
    (let* [(s (make-session))
           (v (tf-variable #:dtype <float> #:shape '(3 2)))]
      (run s '() v)))
  (test-eqv "Constant tensor"
    42.0
    (let* [(s (make-session))
           (c (tf-const #:value 42.0 #:dtype <double>))]
      (run s '() c)))
  (test-eqv "Variable assignment"
    42.0
    (let* [(s (make-session))
           (v (tf-variable #:dtype <double> #:shape '()))
           (c (tf-const #:value 42.0 #:dtype <double>))]
      (run s '() (tf-assign v c))
      (run s '() v))))

(test-group "further operations"
  (test-eqv "operation with list of inputs"
    5.0
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '()))
          (y (tf-variable #:dtype <double> #:shape '()))]
      (run s (list (cons x 2.0) (cons y 3.0)) (tf-add-n (list x y)))))
  (test-error "test integer mismatch"
    'misc-error
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '()))]
      (run s (list (cons x 2.0)) (tf-add-n (list x) #:N 2))))
  (test-eqv "test matching integer parameter"
    2.0
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '()))]
      (run s (list (cons x 2.0)) (tf-add-n (list x) #:N 1))))
  (test-assert "instantiate max-pooling"
    (let [(x (tf-variable #:dtype <double> #:shape '(1 4 4 1)))]
      (tf-max-pool x #:padding 'SAME #:strides '(1 2 2 1) #:ksize '(1 2 2 1))))
  (test-assert "unequal operator returning boolean result"
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '()))
          (y (tf-variable #:dtype <double> #:shape '()))]
      (run s (list (cons x 2.0) (cons y 3.0)) (tf-not-equal x y))))
  (test-assert "approximate equal operator with floating-point attribute"
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '()))
          (y (tf-variable #:dtype <double> #:shape '()))]
      (run s (list (cons x 2.0) (cons y 2.1)) (tf-approximate-equal x y #:tolerance 0.2))))
  (test-equal "test bucketizing with floating-point list attribute"
    '(0 1 2)
    (let [(s (make-session))
          (x (tf-variable #:dtype <double> #:shape '(-1)))]
      (to-list (run s (list (cons x (arr 2.0 3.0 5.0))) (tf-bucketize x #:boundaries '(2.5 4.5))))))
  (test-equal "Cummulative product with boolean true attribute"
    '(30 15 5)
    (let [(s (make-session))
          (a (tf-const #:dtype <int> #:value (arr <int> 2 3 5)))
          (n (tf-const #:dtype <int> #:value 0))]
      (to-list (run s '() (tf-cumprod a n #:reverse #t)))))
  (test-eqv "Upcast integer when converting scalar to tensor"
    42
    (let [(s  (make-session))
          (c (tf-const #:dtype <int> #:value 42))]
      (run s '() c))))

(test-group "Gradients"
  (test-eqv "derivative of x squared at x=3"
    6.0
    (let* [(s (make-session))
           (x (tf-placeholder #:dtype <double>))
           (g (tf-add-gradient (tf-square x) x))]
      (run s (list (cons x 3.0)) g)))
  (test-eqv "derivative of variable"
    -6.0
    (let* [(s (make-session))
           (w (tf-variable #:dtype <double> #:shape '()))
           (c (tf-const #:dtype <double> #:value 0.0))
           (x (tf-placeholder #:dtype <double>))
           (g (tf-add-gradient (tf-square (tf-sub x w)) w))]
      (run s '() (tf-assign w c))
      (run s (list (cons x 3.0)) g))))

(test-group "implicit constant"
  (test-assert "detect Tensorflow output"
    (tf-output? (tf-const #:dtype <double> #:value 0.0)))
  (test-assert "detect type different"
    (not (tf-output? 42)))
  (test-equal "convert array to tensor constant"
    '((1 2) (3 4))
    (let [(s (make-session))
          (a (tf-const #:dtype <int> #:value (arr <int> 1 2 3 4)))]
      (to-list (run s '() (tf-reshape a (arr <int> 2 2))))))
  (test-eqv "convert scalar to tensor constant"
    5 (let [(s  (make-session))] (run s '() (tf-add 2 3)) ))
  (test-eqv "convert list of scalars to list of constants"
    5 (let [(s  (make-session))] (run s '() (tf-add-n (list 2 3))))))

(test-group "multiple outputs"
  (test-assert "top-k returns two values"
    (let [(s (make-session))]
      (list? (run s '() (tf-top-kv2 (arr <int> 2 5 3) 2)))))
  (test-equal "top-k returns two different values"
    '((5 3) (1 2))
    (let [(s  (make-session))]
      (map to-list (run s '() (tf-top-kv2 (arr 2 5 3) 2 #:sorted #t))))))

(test-group "saving and loading of graphs"
  (test-equal "get first output of operation by name"
    42
    (let [(s (make-session))
          (c (tf-const #:dtype <int> #:value 42 #:name "test-const"))]
      (run s '() (tf-graph-operation-by-name "test-const" 0))))
  (test-error "error if operation not found"
    'misc-error
    (tf-graph-operation-by-name "no-such-op" 0))
  (test-assert "method to reset graph exists"
    (defined? 'tf-reset-graph))
  (test-error "test reset of graph"
    'misc-error
    (let [(c (tf-const #:dtype <int> #:value 42 #:name "test-op"))]
      (tf-reset-graph)
      (tf-graph-operation-by-name "test-op" 0)))
  (test-assert "save and load graph"
    (let [(file-name (tmpnam))
          (c (tf-const #:dtype <int> #:value 42 #:name "saved-op"))]
      (tf-graph-export file-name)
      (tf-reset-graph)
      (tf-graph-import file-name)
      (tf-graph-operation-by-name "saved-op" 0)))
  (test-equal "get operation names"
    (list "test-op")
    (begin
      (tf-reset-graph)
      (tf-const #:dtype <int> #:value 42 #:name "test-op")
      (tf-operation-names))))

(test-end "aiscm tensorflow")
