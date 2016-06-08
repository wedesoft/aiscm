(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:export (run-tests diagnostics)
  #:export-syntax (ok skip todo throws?))

(define tests '())

(define count 0)

(define (run-test test) (test))

(define-syntax-rule (schedule prog) (set! tests (cons (lambda () prog) tests)))

(define (run-tests)
  (format #t "1..~a~&~!" count)
  (for-each run-test (reverse tests)))

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
                  (let* [(msg  (apply (cut format #f fmt <...>) vals))
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
