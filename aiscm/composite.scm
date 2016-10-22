(define-module (aiscm composite)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm pointer)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:export (<composite> <meta<composite>>
            components deconstruct))

(define-class* <composite> <element> <meta<composite>> <meta<element>>)
(define-method (pointerless? (self <meta<composite>>)) (pointerless? (base self)))
(define-generic components)
(define-method (component (type <meta<composite>>) self offset)
  (let* [(type (base (typecode self)))]
    (set-pointer-offset (pointer-cast type self) (* offset (size-of type)))))
(define (deconstruct type self) (map (cut <> self) (components type)))
