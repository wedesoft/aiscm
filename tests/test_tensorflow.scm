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
    42.0 (from-tensor (to-tensor 42.0))))

(test-group "build graph"
  (test-assert "create graph"
    (make-graph))
  (test-assert "create placeholder"
    (placeholder (make-graph) #:dtype <float>))
  (test-error "error creating placeholder without type argument"
    'misc-error (placeholder (make-graph)))
  (test-assert "create identity"
    (let [(g (make-graph))] (identity_ g (placeholder g #:dtype <float>))))
  (test-assert "create identity with type argument"
    (let [(g (make-graph))] (identity_ g (placeholder g #:dtype <float>) #:T <float>)))
  (test-error "error if type mismatch is encountered"
    'misc-error (let [(g (make-graph))] (identity_ g (placeholder g #:dtype <float>) #:T <double>))))

(test-group "run session"
  (test-assert "create session"
    (make-session (make-graph)))
  (test-expect-fail 1)
  (test-equal "run session"
    42
    (let* [(g (make-graph))
           (s (make-session g))
           (p (placeholder g #:dtype <double>))]
      (from-tensor (run s (list (cons p (to-tensor 42.0))) (identity_ g p))))))

(test-end "aiscm tensorflow")
