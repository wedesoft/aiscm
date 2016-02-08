(use-modules (ice-9 rdelim) (ice-9 regex))
(define (compile port)
 (let [(line (read-line port))]
   (if (not (eof-object? line))
     (let [(match (string-match "@(.*)@" line))]
       (if match
         (call-with-input-file (match:substring match 1) compile)
         (write-line line))
       (compile port)))))
(compile (current-input-port))
