;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(define-module (aiscm rgb)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm composite)
  #:use-module (aiscm bool)
  #:use-module (aiscm pointer)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:use-module (aiscm jit)
  #:use-module (aiscm operation)
  #:use-module (aiscm expression)
  #:use-module (aiscm sequence)
  #:export (<rgb>
            <rgb<>> <meta<rgb<>>>
            <pointer<rgb<>>> <meta<pointer<rgb>>>>
            <ubytergb> <rgb<int<8,unsigned>>>  <meta<rgb<int<8,unsigned>>>>
            <bytergb>  <rgb<int<8,signed>>>    <meta<rgb<int<8,signed>>>>
            <usintrgb> <rgb<int<16,unsigned>>> <meta<rgb<int<16,unsigned>>>>
            <sintrgb>  <rgb<int<16,signed>>>   <meta<rgb<int<16,signed>>>>
            <uintrgb>  <rgb<int<32,unsigned>>> <meta<rgb<int<32,unsigned>>>>
            <intrgb>   <rgb<int<32,signed>>>   <meta<rgb<int<32,signed>>>>
            <ulonggb>  <rgb<int<64,unsigned>>> <meta<rgb<int<64,unsigned>>>>
            <longrgb>  <rgb<int<64,signed>>>   <meta<rgb<int<64,signed>>>>
            rgb red green blue coerce-rgb)
  #:re-export (== != - ~ + * & | ^ << >> / % max min += *= max= min= where))


(define ctx (make <context>))

(define-class <rgb> ()
  (red   #:init-keyword #:red   #:getter red)
  (green #:init-keyword #:green #:getter green)
  (blue  #:init-keyword #:blue  #:getter blue))
(define-method (rgb r g b) (make <rgb> #:red r #:green g #:blue b))
(define-method (write (self <rgb>) port)
  (format port "(rgb ~a ~a ~a)" (red self) (green self) (blue self)))
(define-method (red   self) self)
(define-method (green self) self)
(define-method (blue  self) self)
(define-class* <rgb<>> <composite> <meta<rgb<>>> <meta<composite>>)
(define-method (rgb (t <meta<element>>))
  (template-class (rgb t) <rgb<>>
    (lambda (class metaclass)
      (define-method (base (self metaclass))t)
      (define-method (size-of (self metaclass)) (* 3 (size-of t))))))
(define-method (components (self <meta<rgb<>>>)) (list red green blue))
(define-method (rgb (t <meta<sequence<>>>)) (multiarray (rgb (typecode t)) (dimensions t)))
(define-method (coerce-rgb (r <meta<element>>) (g <meta<element>>) (b <meta<element>>))
  (rgb (reduce coerce #f (list r g b))))

(define-method (rgb (r <meta<element>>) (g <meta<element>>) (b <meta<element>>))
  (lambda (out r g b) (append-map duplicate (content <rgb<>> out) (list r g b))))

(define-method (red   (self <rgb<>>)) (make (base (class-of self)) #:value (red   (get self))))
(define-method (green (self <rgb<>>)) (make (base (class-of self)) #:value (green (get self))))
(define-method (blue  (self <rgb<>>)) (make (base (class-of self)) #:value (blue  (get self))))

(define-method (write (self <rgb<>>) port)
  (format port "#<~a ~a>" (class-name (class-of self)) (get self)))
(define-method (base (self <meta<sequence<>>>)) (multiarray (base (typecode self)) (dimensions self)))
(define-method (pack (self <rgb<>>)) (bytevector-concat (map pack (content (class-of self) self))))
(define-method (unpack (self <meta<rgb<>>>) (packed <bytevector>))
  (let* [(size    (size-of (base self)))
         (vectors (map (cut bytevector-sub packed <> size) (map (cut * size <>) (iota 3))))]
    (make self #:value (apply rgb (map (lambda (vec) (get (unpack (base self) vec))) vectors)))))
(define <ubytergb> (rgb <ubyte>))
(define <bytergb>  (rgb <byte> ))
(define <usintrgb> (rgb <usint>))
(define <sintrgb>  (rgb <sint> ))
(define <uintrgb>  (rgb <uint> ))
(define <intrgb>   (rgb <int>  ))
(define <ulongrgb> (rgb <ulong>))
(define <longrgb>  (rgb <long> ))
(define-method (coerce (a <meta<rgb<>>>) (b <meta<element>>)) (rgb (coerce (base a) b)))
(define-method (coerce (a <meta<element>>) (b <meta<rgb<>>>)) (rgb (coerce a (base b))))
(define-method (coerce (a <meta<rgb<>>>) (b <meta<rgb<>>>)) (rgb (coerce (base a) (base b))))
(define-method (coerce (a <meta<rgb<>>>) (b <meta<sequence<>>>)) (multiarray (coerce a (typecode b)) (dimensions b)))
(define-method (native-type (c <rgb>) . args)
  (rgb (apply native-type (concatenate (map-if (cut is-a? <> <rgb>) (cut deconstruct <rgb<>> <>) list (cons c args))))))
(define-method (build (self <meta<rgb<>>>) value) (apply rgb value))
(define-method (unbuild (type <meta<rgb<>>>) self)
  (append-map (cut unbuild (base type) <>) (deconstruct type self)))
(define-method (content (type <meta<rgb<>>>) (self <rgb>))
  (append-map (cut content (base type) <>) (deconstruct type self)))
(define-method (content (type <meta<rgb<>>>) (self <rgb<>>))
  (append-map (cut content (base type) <>) (deconstruct type self)))
(define-method (typecode (self <rgb>)) (rgb (reduce coerce #f (map typecode (content <rgb<>> self)))))

(define-syntax-rule (unary-rgb-op op)
  (define-method (op (a <rgb>)) (apply rgb (map op (content <rgb<>> a)))))

(unary-rgb-op -)
(unary-rgb-op ~)
(unary-rgb-op <<)
(unary-rgb-op >>)

(define-method (+=   (a <rgb>) (b <rgb>)) (append-map +=   (content <rgb<>> a) (content <rgb<>> b)))
(define-method (*=   (a <rgb>) (b <rgb>)) (append-map *=   (content <rgb<>> a) (content <rgb<>> b)))
(define-method (max= (a <rgb>) (b <rgb>)) (append-map max= (content <rgb<>> a) (content <rgb<>> b)))
(define-method (min= (a <rgb>) (b <rgb>)) (append-map min= (content <rgb<>> a) (content <rgb<>> b)))

(define-syntax-rule (binary-rgb-op op)
  (begin
    (define-method (op (a <rgb>)  b       ) (apply rgb (map (cut op <> b) (content <rgb<>> a)                    )))
    (define-method (op  a        (b <rgb>)) (apply rgb (map (cut op a <>)                     (content <rgb<>> b))))
    (define-method (op (a <rgb>) (b <rgb>)) (apply rgb (map op            (content <rgb<>> a) (content <rgb<>> b))))
    (define-method (op (a <rgb>)     (b <element>)) (op (wrap a) b))
    (define-method (op (a <element>) (b <rgb>)    ) (op a (wrap b)))))

(binary-rgb-op +  )
(binary-rgb-op -  )
(binary-rgb-op *  )
(binary-rgb-op &  )
(binary-rgb-op |  )
(binary-rgb-op ^  )
(binary-rgb-op << )
(binary-rgb-op >> )
(binary-rgb-op /  )
(binary-rgb-op %  )
(binary-rgb-op max)
(binary-rgb-op min)

(define-syntax-rule (binary-rgb-cmp op f)
  (begin
    (define-method (op (a <rgb>)  b       ) (apply f (map (cut op <> b) (content <rgb<>> a)                    )))
    (define-method (op  a        (b <rgb>)) (apply f (map (cut op a <>)                     (content <rgb<>> b))))
    (define-method (op (a <rgb>) (b <rgb>)) (apply f (map op            (content <rgb<>> a) (content <rgb<>> b))))))

(binary-rgb-cmp equal? equal?)
(binary-rgb-cmp == &&)
(binary-rgb-cmp != ||)

(define-method (where (m <param>) (a <rgb>) (b <rgb>))
  (apply rgb (map (cut where m <...>) (content <rgb<>> a) (content <rgb<>> b))))
(define-method (where (m <param>) (a <rgb>)  b       )
  (apply rgb (map (cut where m <> b) (content <rgb<>> a))))
(define-method (where (m <param>)  a        (b <rgb>))
  (apply rgb (map (cut where m a <>) (content <rgb<>> b))))

(define-method (var (self <meta<rgb<>>>)) (let [(type (base self))] (rgb (var type) (var type) (var type))))
(pointer <rgb<>>)
(define-method (red   (self <pointer<>>)) self)
(define-method (green (self <pointer<>>)) self)
(define-method (blue  (self <pointer<>>)) self)
(define-method (red   (self <pointer<rgb<>>>)) (component (typecode self) self 0))
(define-method (green (self <pointer<rgb<>>>)) (component (typecode self) self 1))
(define-method (blue  (self <pointer<rgb<>>>)) (component (typecode self) self 2))

(define-operator-mapping red   (<meta<element>>) (unary-extract red  ))
(define-operator-mapping green (<meta<element>>) (unary-extract green))
(define-operator-mapping blue  (<meta<element>>) (unary-extract blue ))

(define-jit-method base red   1)
(define-jit-method base green 1)
(define-jit-method base blue  1)

(define-jit-method coerce-rgb rgb 3)

(define-method (decompose-value (target <meta<rgb<>>>) x)
  (make <rgb> #:red   (parameter (red   (delegate x)))
              #:green (parameter (green (delegate x)))
              #:blue  (parameter (blue  (delegate x)))))

(define-method (to-type (target <meta<rgb<>>>) (self <rgb>))
  (apply rgb (map (cut to-type (base target) <>) (content <rgb<>> self))))
