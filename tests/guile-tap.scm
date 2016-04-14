(define-module (guile-tap)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:export (run-tests diagnostics)
  #:export-syntax (ok skip todo throws?))

(define tests '())

(define (run-test test counter) (test counter))

(define (run-tests)
  (let [(n (length tests))]
    (format #t "1..~a~&~!" n)
    (for-each run-test (reverse tests) (map 1+ (iota n)))))

(define (diagnostics msg) (format #t "# ~a~&" msg))

(define (print-result result index description)
  (format #t "~a ~a - ~a~&~!" (if result "ok" "not ok") index description))

(define-syntax-rule (ok expr description)
  (set! tests
        (cons (lambda (counter)
                (catch #t
                  (lambda () (print-result expr counter description))
                  (lambda (key function fmt vals . args)
                    (let* [(msg  (apply (cut format #f fmt <...>) vals))
                           (info (format #f "~a (ERROR: In procedure ~a: ~a)" description function msg))]
                      (print-result #f counter info)))))
              tests)))

(define-syntax-rule (skip expr description)
  (ok #t (string-append description " # SKIP")))

(define-syntax-rule (todo expr description)
  (ok expr (string-append description " # TODO")))

(define-syntax-rule (throws? expr)
  (catch #t
    (lambda () expr (throw 'test-failed))
    (lambda (key . args) (case key ((test-failed) #f) (else #t)))))
