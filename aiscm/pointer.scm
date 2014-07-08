(define-module (aiscm pointer)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm int)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (<pointer<>>
            <meta<pointer<>>>
            pointer
            target
            fetch
            store))
(define-class <meta<pointer<>>> (<meta<element>>))
(define-class <pointer<>> (<element>) #:metaclass <meta<pointer<>>>)
(define-generic target)
(define (pointer targetclass)
  (let* [(name (format #f "<pointer~a>" (class-name targetclass)))
         (metaname (format #f "<meta~a>" name))
         (metaclass (def-once metaname
                              (make <class>
                                    #:dsupers (list <meta<pointer<>>>)
                                    #:slots '()
                                    #:name metaname)))
         (retval (def-once name (make metaclass
                                      #:dsupers (list <pointer<>>)
                                      #:slots '()
                                      #:name name)))]
    (define-method (target (self metaclass)) targetclass)
    retval))
(define-method (fetch (self <pointer<>>))
  (let [(t (target (class-of self)))]
    (unpack t (read-bytes (get-value self) (size-of t)))))
(define-method (store (self <pointer<>>) (element <element>))
  (let [(converted (make (target (class-of self)) #:value (get-value element)))]
    (write-bytes (get-value self) (pack converted))
    converted))
(define-method (+ (self <pointer<>>) (offset <integer>))
  (make (class-of self)
        #:value (+ (get-value self)
                   (* offset ((compose size-of target class-of) self)))))
(define-method (pack (self <pointer<>>))
  (pack (make <native-int>
              #:value ((compose pointer-address get-memory get-value) self))))
(define-method (write (self <pointer<>>) port)
  (format port "#<~a #x~x>"
          (class-name (class-of self))
          ((compose pointer-address get-memory get-value) self)))
(define-method (display (self <pointer<>>) port)
  (format port "#<~a #x~x>"
          (class-name (class-of self))
          ((compose pointer-address get-memory get-value) self)))
