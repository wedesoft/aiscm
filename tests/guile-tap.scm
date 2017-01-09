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
(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:export (run-tests diagnostics)
  #:export-syntax (ok skip todo throws?))

(define tests '())

(define count 0)

(define (run-test test) (test))

(define-syntax-rule (schedule prog) (set! tests (cons (lambda () prog) tests)))

(define (run-tests)
  (format #t "1..~a~&~!" count)
  (for-each run-test (reverse tests))
  (set! tests '()))

(define (diagnostics msg)
  (schedule (format #t "# ~a~&" msg)))

(define (print-result result index description)
  (format #t "~a ~a - ~a~&~!" (if result "ok" "not ok") index description))

(define-syntax-rule (ok expr description)
  (let [(n (1+ count))]
    (set! count (1+ count))
    (schedule (catch #t
                (lambda () (print-result expr n description))
                (lambda (key function fmt vals . args)
                  (let* [(msg  (apply format #f fmt vals))
                         (info (format #f "~a (ERROR: In procedure ~a: ~a)" description function msg))]
                    (print-result #f n info)))))))

(define-syntax-rule (skip expr description)
  (ok #t (string-append description " # SKIP")))

(define-syntax-rule (todo expr description)
  (ok expr (string-append description " # TODO")))

(define-syntax-rule (throws? expr)
  (catch #t
    (lambda () expr (throw 'test-failed))
    (lambda (key . args) (case key ((test-failed) #f) (else #t)))))
