(use-modules (oop goops)
             (ice-9 format)
             (rnrs bytevectors)
             (aiscm tensorflow)
             (aiscm core)
             (aiscm pulse))

;(define robot (socket PF_INET SOCK_DGRAM 0))
;(connect robot AF_INET (car (hostent:addr-list (gethostbyname "raspberrypi.local"))) 2200)
;(define commands (list "0,0,0" "-100,-100,0" "100,-100,0" "-100,100,0"))
(define words (list "stop" "go" "left" "right"))
(define rate 11025)
(define chunk 512)
(define n-hidden 128)
(tf-graph-import "speech-model.meta")

(define x (tf-graph-operation-by-name "x"))
(define c (tf-graph-operation-by-name "c"))
(define cs (tf-graph-operation-by-name "cs"))
(define pred (tf-graph-operation-by-name "prediction"))

(define session (make-session))
(run session '()
  (list (tf-graph-operation-by-name "init-wcc")
        (tf-graph-operation-by-name "init-wcx")
        (tf-graph-operation-by-name "init-bc" )
        (tf-graph-operation-by-name "init-wuc")
        (tf-graph-operation-by-name "init-wux")
        (tf-graph-operation-by-name "init-bu" )
        (tf-graph-operation-by-name "init-w"  )
        (tf-graph-operation-by-name "init-b"  )))

(define c0 (fill <double> (list 1 n-hidden) 0.0))
(define record (make <pulse-record> #:typecode <sint> #:channels 1 #:rate rate))

(while #t
  (let [(samples (reshape (read-audio record chunk) (list 1 chunk)))]
    (set! c0 (run session (list (cons x samples) (cons c c0)) cs))
    (let [(out (run session (list (cons c c0)) pred))]
      (if (not (zero? out))
         (begin
           ;(display (list-ref commands (1- out)) robot)
           (format #t "~a~&"(list-ref words (1- out))))))))
