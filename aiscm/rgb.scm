(define-module (aiscm rgb)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm util)
  #:export (rgb type
            <rgb> reg green blue
            <rgb<>> <meta<rgb<>>>
            <ubytergb> <rgb<integer<8,unsigned>>>  <rgb<integer<8,unsigned>>>
            <bytergb>  <rgb<integer<8,signed>>>    <rgb<integer<8,signed>>>
            <usintrgb> <rgb<integer<16,unsigned>>> <rgb<integer<16,unsigned>>>
            <sintrgb>  <rgb<integer<16,signed>>>   <rgb<integer<16,signed>>>
            <uintrgb>  <rgb<integer<32,unsigned>>> <rgb<integer<32,unsigned>>>
            <intrgb>   <rgb<integer<32,signed>>>   <rgb<integer<32,signed>>>
            <ulonggb>  <rgb<integer<64,unsigned>>> <rgb<integer<64,unsigned>>>
            <longrgb>  <rgb<integer<64,signed>>>   <rgb<integer<64,signed>>>))
(define-class <rgb> ()
  (red   #:init-keyword #:red   #:getter red)
  (green #:init-keyword #:green #:getter green)
  (blue  #:init-keyword #:blue  #:getter blue))
(define-method (rgb (r <real>) (g <real>) (b <real>)) (make <rgb> #:red r #:green g #:blue b))
(define-method (write (self <rgb>) port)
  (format port "(rgb ~a ~a ~a)" (red self) (green self) (blue self)))
(define-class* <rgb<>> <element> <meta<rgb<>>> <meta<element>>)
(define-method (rgb (t <meta<element>>))
  (template-class (rgb t) <rgb<>>
    (lambda (class metaclass)
      (define-method (type (self metaclass))t); TODO: rename this
      (define-method (size-of (self metaclass)) (* 3 (size-of t))))))
(define-method (initialize (self <rgb<>>) initargs)
  (let-keywords initargs #f (red green blue)
    (next-method self (list #:value (list red green blue)))) )
(define-method (pack (self <rgb<>>))
  (let* [(channels (map (cut make (type (class-of self)) #:value <>) (get-value self)))
         (size     (size-of (type (class-of self))))]
    (bytevector-concat (map pack channels))))
(define-method (unpack (self <meta<rgb<>>>) (packed <bytevector>))
  (let* [(size    (size-of (type self)))
         (vectors (map (cut bytevector-sub packed <> size) (map (cut * size <>) (iota 3))))]
    (make self #:red   (get (unpack (type self) (car vectors)))
               #:green (get (unpack (type self) (cadr vectors)))
               #:blue  (get (unpack (type self) (caddr vectors))))))
(define <ubytergb> (rgb <ubyte>))
(define <bytergb> (rgb <byte>))
(define <usintrgb> (rgb <usint>))
(define <sintrgb> (rgb <sint>))
(define <uintrgb> (rgb <uint>))
(define <intrgb> (rgb <int>))
(define <ulongrgb> (rgb <ulong>))
(define <longrgb> (rgb <long>))
