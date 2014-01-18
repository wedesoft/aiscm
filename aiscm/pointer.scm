(define-module (aiscm pointer)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm malloc)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (<pointer<>>
            <meta<pointer<>>>
            make-pointer-class
            target
            make-pointer
            fetch
            store
            lookup))
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
(define-method (fetch (self <pointer<>>))
  (let ((t (target (class-of self))))
    (unpack t (read-bytes (get-value self) (storage-size t)))))
(define-method (store (self <pointer<>>) (element <element>))
  (let ((converted (make (target (class-of self)) #:value (get-value element))))
    (write-bytes (get-value self) (pack converted))
    converted))
(define-method (+ (self <pointer<>>) (offset <integer>))
  (make (class-of self) #:value (+ (get-value self) (* offset (storage-size (target (class-of self)))))))
(define-method (lookup (self <pointer<>>) (value <integer>) (stride <integer>))
  (+ self (* value stride)))
(define-method (write (self <pointer<>>) port)
  (format port "#<~a #x~x>"
          (class-name (class-of self))
          (pointer-address (get-memory (get-value self)))))
(define-method (display (self <pointer<>>) port)
  (format port "#<~a #x~x>"
          (class-name (class-of self))
          (pointer-address (get-memory (get-value self)))))
