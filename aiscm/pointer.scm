(define-module (aiscm pointer)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm malloc)
  #:use-module (rnrs bytevectors)
  #:export (<pointer<>>
            <meta<pointer<>>>
            make-pointer-class
            target
            make-pointer
            fetch))
(define-class <meta<pointer<>>> (<class>))
(define-class <pointer<>> (<element>) #:metaclass <meta<pointer<>>>)
(define-generic target)
(define (make-pointer-class targetclass)
  (let* ((name (format #f "<pointer<~a>>" (class-name targetclass)))
         (metaname (format #f "<meta<~a>>" name))
         (metaclass (make-class (list <meta<pointer<>>>) '() #:name metaname))
         (retval (make-class (list <pointer<>>)
                             '()
                             #:name name
                             #:metaclass metaclass)))
    (define-method (target (self metaclass)) targetclass)
    retval))
(define (make-pointer targetclass value)
  (make (make-pointer-class targetclass) #:value value))
;(define-method (fetch (self <pointer<>>))
;  (storage-size (target (class-of self))))
(define-method (fetch (self <pointer<>>))
  (let ((t (target (class-of self))))
    (unpack t (read-bytes (get-value self) (storage-size t)))))
; TODO: store
; TODO: lookup
