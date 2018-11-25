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
           (c (tf-const #:value (to-tensor 42.0) #:dtype <double>))]
      (run s '() c)))
  (test-eqv "Variable assignment"
    42.0
    (let* [(s (make-session))
           (v (tf-variable #:dtype <double> #:shape '()))
           (c (tf-const #:value (to-tensor 42.0) #:dtype <double>))]
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
      (tf-max-pool x #:padding 'SAME #:strides '(1 2 2 1) #:ksize '(1 2 2 1)))))

(test-end "aiscm tensorflow")
