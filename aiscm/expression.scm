;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm expression)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:use-module (aiscm composite)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm variable)
  #:use-module (aiscm util)
  #:export (<param> <lookup> <indexer> <function> <injecter>
            skeleton delegate parameter lookup indexer index name coercion term type
            make-function subst dimension-hint injecter)
  #:re-export (typecode value get project shape strides rebase content)
  #:export-syntax (inject dim))


(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<sequence<>>>))
  (let [(slice (skeleton (project self)))]
    (make self
          #:value   (value slice)
          #:shape   (cons (var <long>) (shape   slice))
          #:strides (cons (var <long>) (strides slice)))))

(define-class <param> ()
  (delegate #:init-keyword #:delegate #:getter delegate))

(define-method (parameter (self <element>)) (make <param> #:delegate self))
(define-method (parameter (self <sequence<>>))
  (let [(idx (var <long>))]
    (indexer idx
             (lookup idx
                     (parameter (project self))
                     (parameter (make <long> #:value (stride self))))
             (parameter (make <long> #:value (dimension self))))))
(define-method (parameter (self <meta<element>>)) (parameter (skeleton self)))

(define-class <indexer> (<param>)
  (dimension #:init-keyword #:dimension #:getter dimension)
  (index     #:init-keyword #:index     #:getter index))
(define (indexer index delegate dimension)
  (make <indexer> #:dimension dimension #:index index #:delegate delegate))

(define-syntax dim
  (lambda (x)
    (syntax-case x ()
      ((dim expr) #'expr)
      ((dim indices ... index expr) #'(let [(index (var <long>))] (indexer index (dim indices ... expr) (dimension-hint index)))))))

(define-class <lookup> (<param>)
  (index    #:init-keyword #:index    #:getter index   )
  (stride   #:init-keyword #:stride   #:getter stride  ))
(define-method (lookup index delegate stride)
  (make <lookup> #:index index #:delegate delegate #:stride stride))
(define-method (lookup idx (obj <indexer>) stride)
  (indexer (index obj) (lookup idx (delegate obj) stride) (dimension obj)))

(define-class <function> (<param>)
  (coercion  #:init-keyword #:coercion  #:getter coercion)
  (name      #:init-keyword #:name      #:getter name)
  (term      #:init-keyword #:term      #:getter term))
(define (make-function name coercion fun args)
  (make <function> #:delegate args
                   #:coercion coercion
                   #:name     name
                   #:term     (lambda (out) (fun out args))))

(define-class <injecter> (<param>)
  (name  #:init-keyword #:name  #:getter name)
  (index #:init-keyword #:index #:getter index))
(define (injecter name index delegate)
  (make <injecter> #:name name #:index index #:delegate delegate))

(define-syntax-rule (inject name index delegate)
  (let [(index (var <long>))]
    (injecter name index delegate)))

(define-method (type (self <param>)) (typecode (delegate self)))
(define-method (type (self <indexer>)) (sequence (type (delegate self))))
(define-method (type (self <lookup>)) (type (delegate self)))
(define-method (type (self <function>)) (apply (coercion self) (map type (delegate self))))
(define-method (type (self <injecter>)) (type (delegate self)))
(define-method (typecode (self <param>)) (typecode (type self)))

(define-method (value (self <param>)) (value (delegate self)))
(define-method (value (self <indexer>)) (value (delegate self)))
(define-method (value (self <lookup>)) (value (delegate self)))

(define dimension-hint (make-object-property))

(define (element idx self)
  (set! (dimension-hint idx) (dimension self))
  (subst (delegate self) (index self) idx))

(define-method (get (self <param>) . args)
  "Use multiple indices to access elements"
  (if (null? args) self (fold-right element self args)))

(define-method (subst self candidate replacement) self)
(define-method (subst (self <indexer>) candidate replacement)
  (indexer (index self) (subst (delegate self) candidate replacement) (dimension self)))
(define-method (subst (self <lookup>) candidate replacement)
  (lookup (if (eq? (index self) candidate) replacement (index self))
          (subst (delegate self) candidate replacement)
          (stride self)))

(define-method (project (self <indexer>))
  (project (delegate self) (index self)))
(define-method (project (self <indexer>) (idx <var>))
  (indexer (index self) (project (delegate self) idx) (dimension self)))
(define-method (project (self <lookup>) (idx <var>))
  (if (eq? (index self) idx)
      (delegate self)
      (lookup (index self) (project (delegate self) idx) (stride self))))

(define-method (shape (self <indexer>)) (attach (shape (delegate self)) (dimension self)))
(define-method (shape (self <function>)) (argmax length (map shape (delegate self))))
(define-method (shape (self <injecter>)) (shape (delegate self)))

(define-method (strides (self <indexer>)) (attach (strides (delegate self)) (stride (lookup self (index self)))))
(define-method (lookup (self <indexer>)) (lookup self (index self)))
(define-method (lookup (self <indexer>) (idx <var>)) (lookup (delegate self) idx))
(define-method (lookup (self <lookup>) (idx <var>)) (if (eq? (index self) idx) self (lookup (delegate self) idx)))
(define-method (stride (self <indexer>)) (stride (lookup self)))

(define-method (rebase value (self <param>)) (parameter (rebase value (delegate self))))
(define-method (rebase value (self <indexer>))
  (indexer (index self) (rebase value (delegate self)) (dimension self)))
(define-method (rebase value (self <lookup>))
  (lookup (index self) (rebase value (delegate self)) (stride self)))

(define-method (content (type <meta<element>>) (self <param>)) (map parameter (content type (delegate self))))
(define-method (content (type <meta<scalar>>) (self <function>)) (list self))
(define-method (content (type <meta<composite>>) (self <function>)) (delegate self))
(define-method (content (type <meta<sequence<>>>) (self <param>))
  (cons (dimension self) (cons (stride self) (content (project type) (project self)))))
