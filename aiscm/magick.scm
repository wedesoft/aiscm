(define-module (aiscm magick)
  #:use-module (oop goops)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm rgb)
  #:use-module (aiscm sequence)
  #:use-module (aiscm image)
  #:export (read-image))
(load-extension "libguile-magick" "init_magick")
(define (read-image file-name)
  (let [(picture (magick-read-image file-name))]
    (make <image>
          #:format 'BGRA
          #:shape (car picture)
          #:mem (make <mem> #:base (cadr picture) #:size (caddr picture)))))
