(define-module (aiscm magick)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm rgb)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm jit)
  #:use-module (aiscm op)
  #:export (read-image write-image))
(load-extension "libguile-magick" "init_magick")
(define (read-image file-name)
  (let* [(picture  (magick-read-image file-name))
         (typecode (if (eq? (car picture) 'I) <ubyte> <ubytergb>))]
    (make (multiarray typecode 2)
          #:shape (cadr picture)
          #:value (make <mem> #:base (caddr picture) #:size (cadddr picture)))))
(define (write-image img file-name)
  (let [(format    (cond ((eq? (typecode img) <ubyte>)    'I)
                         ((eq? (typecode img) <ubytergb>) 'RGB)
                         (else #f)))
        (compacted (ensure-default-strides img))]
    (if (not format)
      (scm-error 'unsupported-typecode
                 'write-image
                 "Saving of typecode ~a not supported"
                 (list (typecode img))
                 #f))
    (if (not (eqv? (dimension img) 2))
      (scm-error 'wrong-dimension
                 'write-image
                 "Image must have 2 dimensions but had ~a"
                 (list (dimension img))
                 #f))
    (magick-write-image format (shape compacted) (get-memory (slot-ref compacted 'value)) file-name)
    img))
