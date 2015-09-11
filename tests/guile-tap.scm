(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:export (planned-tests diagnostics)
  #:export-syntax (ok skip todo throws?))
(define (planned-tests num) (format #t "1..~a~&~!" num))

(define (diagnostics msg) (format #t "# ~a~&" msg))

(define test-counter 0)
(define (incr-counter) (set! test-counter (+ test-counter 1)))

(define (print-result result index description)
  (format #t "~a ~a - ~a~&~!" (if result "ok" "not ok") index description))

(define-syntax-rule (ok expr description)
  (begin
    (incr-counter)
    (catch #t
      (lambda () (print-result expr test-counter description))
      (lambda (key function fmt vals . args)
        (let* [(msg  (apply (cut format #f fmt <...>) vals))
               (info (format #f "~a (ERROR: In procedure ~a: ~a)" description function msg))]
          (print-result #f test-counter info))))))

(define-syntax-rule (skip expr description)
  (ok #t (string-append description " # SKIP")))

(define (todo expr description)
  (ok expr (string-append description " # TODO")))

(define-syntax-rule (throws? expr)
  (catch #t
    (lambda () expr (throw 'test-failed))
    (lambda (key . args) (case key ((test-failed) #f) (else #t)))))
