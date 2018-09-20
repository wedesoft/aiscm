(use-modules (oop goops) (aiscm util) (system foreign) (rnrs bytevectors) (aiscm core) (aiscm image) (aiscm magick) (aiscm xorg) (aiscm v4l2) (srfi srfi-1) (srfi srfi-26))

(define-method (native-type2 value) (native-type value))
(define-method (native-type2 (value <integer>)) <int>)

(define-syntax-rule (define-tensor (name args ...) expression)
  (define-method (name args ...)
    (let [(fun (jit (map native-type2 (list args ...)) (lambda (args ...) expression)))]
      (add-method! name
                   (make <method> #:specializers (map class-of (list args ...))
                                  #:procedure fun))
      (name args ...))))

(define-tensor (test x) (+ x x))

;(define-tensor (name arr size) (tensor [size] i (get arr i)))
