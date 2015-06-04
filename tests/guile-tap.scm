(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:export (planned-tests ok diagnostic)
  #:export-syntax (skip todo throws?))
(define (planned-tests num) (format #t "1..~a~&~!" num))

(define test-counter 0)
(define (incr-counter) (set! test-counter (+ test-counter 1)))

(define (ok result description)
  (incr-counter)
  (format #t "~a ~a - ~a~&~!" (if result "ok" "not ok") test-counter description))

(define-syntax-rule (skip expr description)
  (ok #t (string-append description " # SKIP")))

(define (todo expr description)
  (ok expr (string-append description " # TODO")))

(define-syntax-rule (throws? expr)
  (catch #t
    (lambda () expr (throw 'test-failed))
    (lambda (key . args) (case key ((test-failed) #f) (else #t)))))
