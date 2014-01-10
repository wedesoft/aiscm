(define-module (guile-tap)
 :export (planned-tests ok bail-out! diagnostic skip todo)

; ~& in format strings
 :use-module (ice-9 format))

; Internal variables

(define total-tests 0)
(define test-counter 0)
(define printed-total #f)
(define bailed-out #f)

; Internal functions

(define (print-total)
 (if (not printed-total)
  (begin
   (format #t "~&1..~a" total-tests)
   (set! printed-total #t))))

(define (incr-counter)
  (set! test-counter (+ test-counter 1)))

(define (print-description description)
 (if (not (null? description))
   (format #t " - ~a" (car description))))

(define (do-directive tests directive explanation)
  (for-each
	(lambda (e)
	  (primitive-eval e)
	  (format #t " # ~a ~a~%"
			  directive
			  (if (not (null? explanation))
					   (car explanation) "")))
	  tests))

; Exported functions

(define (planned-tests num)
 (set! total-tests num))

(define plan planned-tests)

(define ok
 (lambda (result . description)
   (print-total)

   (if (not bailed-out)
	 (begin
	   (incr-counter)

	   (format #t "~&~a ~a"
			   (if result
				 "ok" "not ok")
			   test-counter)

	   (print-description description)))))


(define bail-out!
 (lambda (. why)
   (set! bailed-out #t)

   (format #t "~&Bail out! ~a~%"
		   (if (not (null? why))
			 (car why)
			 ""))))

(define (diagnostic message)
 (format #t "~&# ~a" message))

(define diag diagnostic)

(define skip
 (lambda (skip? tests . why)
  (do-directive tests "skip" why)))

(define todo
 (lambda (tests . why)
  (do-directive tests "todo" why)))

