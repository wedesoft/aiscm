(use-modules (ice-9 threads))
(define n 0)
(define frames '())
(define m (make-mutex))
(define c (make-condition-variable))
(define (pop)
  (lock-mutex m)
  (if (null? frames)
    (wait-condition-variable c m))
  (let [(result (car frames))]
    (set! frames (cdr frames))
    (unlock-mutex m)
    result))
(define q #f)
(define t
  (make-thread
    (lambda _
      (while (not q)
        (usleep 75000)
        (lock-mutex m)
        (set! frames (append frames (list n)))
        (signal-condition-variable c)
        (unlock-mutex m)
        (set! n (1+ n))))))
(for-each
  (lambda _
    (usleep 100000)
    (format #t "~a~&" (pop)))
  (iota 10))
(for-each
  (lambda _
    (format #t "~a~&" (pop)))
  (iota 10))
(define q #t)
(join-thread t)
