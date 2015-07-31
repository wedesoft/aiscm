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
(define-class* <pointer<>> <element> <meta<pointer<>>> <meta<element>>)
(define-method (pointer target)
  (template-class (pointer target) <pointer<>>
    (lambda (class metaclass)
      (define-method (initialize (self class) initargs)
        (let-keywords initargs #t (value)
          (let [(value (or value (make <mem> #:size (size-of target))))]
            (next-method self (list #:value value)))))
      (define-method (typecode (self metaclass)) target))))
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
(define-method (content (self <pointer<>>))
  (list (pointer-address (get-memory (get-value self)))))
