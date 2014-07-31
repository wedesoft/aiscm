(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:export (planned-tests ok diagnostic)
  #:export-syntax (skip todo throws?))
(define (planned-tests num) (format #t "1..~a~&~!" num))

(define test-counter 0)
(define (incr-counter) (set! test-counter (+ test-counter 1)))

(define (ok result . description)
  (incr-counter)
  (format #t "~a ~a" (if result "ok" "not ok") test-counter)
  (if (not (null? description)) (format #t " - ~a" (car description)))
  (format #t "~&~!"))

(define-syntax-rule (skip expr . explanation)
  (if (not (null? (quote explanation)))
    (ok #t (string-append (car (quote explanation)) " # SKIP"))
    (ok #t "# SKIP")))

(define-syntax-rule (todo expr . explanation)
  (if (not (null? (quote explanation)))
    (ok expr (string-append (car (quote explanation)) " # TODO"))
    (ok expr "# TODO")))

(define (diagnostic message) (format #t "# ~a~&~!" message))

(define-syntax-rule (throws? expr)
  (catch #t
    (lambda () expr (throw 'test-failed))
    (lambda (key . args) (case key ((test-failed) #f) (else #t)))))
