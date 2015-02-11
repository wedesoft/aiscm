(define-module (aiscm pointer)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm int)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (<pointer<>>
            <meta<pointer<>>>
            pointer
            fetch
            store))
(define-class <meta<pointer<>>> (<meta<element>>))
(define-class <pointer<>> (<element>) #:metaclass <meta<pointer<>>>)
(define-method (pointer targetclass)
  (let* [(name (format #f "<pointer~a>" (class-name targetclass)))
         (metaname (format #f "<meta~a>" name))
         (metaclass (def-once metaname
                              (make <class>
                                    #:dsupers (list <meta<pointer<>>>)
                                    #:name metaname)))
         (retval (def-once name (make metaclass
                                      #:dsupers (list <pointer<>>)
                                      #:name name)))]
    (define-method (initialize (self retval) initargs)
      (let-keywords initargs #t (value)
        (let [(value (or value (make <mem> #:size (size-of targetclass))))]
          (next-method self (list #:value value)))))
    (define-method (typecode (self metaclass)) targetclass)
    retval))
(define-method (fetch (self <pointer<>>))
  (let [(t (typecode self))]
    (unpack t (read-bytes (get-value self) (size-of t)))))
(define-method (store (self <pointer<>>) value)
  (begin
    (write-bytes (get-value self) (pack (make (typecode self) #:value value)))
    value))
(define-method (+ (self <pointer<>>) (offset <integer>))
  (make (class-of self)
        #:value (+ (get-value self)
                   (* offset ((compose size-of typecode) self)))))
(define-method (pack (self <pointer<>>))
  (pack (make <native-int>
              #:value ((compose pointer-address get-memory get-value) self))))
(define-method (types (type <meta<pointer<>>>)) (list <long>))
