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
(define-module (aiscm tensors)
  #:use-module (oop goops)
  #:use-module (aiscm core)
  #:use-module (aiscm util)
  #:export (<tensor> <index> <functional> <lookup> <elementary<>>
            expression->tensor)
  #:export-syntax (define-tensor tensor term index stride elementary tensor-iterate)
  #:re-export (get memory shape typecode project rebase))

(define-class <tensor> ())

(define-class <index> (<tensor>)
  (size #:init-keyword #:size #:getter size))

(define-method (typecode (self <index>))
  <int>)

(define-class <elementary<>> (<tensor>)
  (memory #:init-keyword #:memory #:getter memory))

(define (elementary type)
  (template-class (elementary type) <elementary<>>
    (lambda (class metaclass)
      (define-method (typecode (self class)) type))))

(define-class <functional> (<tensor>)
  (index #:init-keyword #:index #:getter index)
  (term  #:init-keyword #:term  #:getter term ))

(define-method (shape (self <functional>))
  (attach (shape (term self)) (size (index self))))

(define-method (typecode (self <functional>))
  (typecode (term self)))

(define-method (memory (self <functional>))
  (memory (term self)))

(define-class <lookup> (<tensor>)
  (index  #:init-keyword #:index  #:getter index )
  (stride #:init-keyword #:stride #:getter stride)
  (term  #:init-keyword #:term  #:getter term ))

(define-method (typecode (self <lookup>))
  (typecode (term self)))

(define-method (memory (self <lookup>))
  (memory (term self)))

(define-method (stride (self <lookup>) (idx <index>))
  (if (eq? (index self) idx)
    (stride self)
    (stride (term self) idx)))

(define-method (subst (self <functional>) (before <index>) (after <index>))
  (make <functional> #:term (subst (term self) before after) #:index (index self)))

(define-method (subst (self <lookup>) (before <index>) (after <index>))
  (if (eq? (index self) before)
    (begin
      (slot-set! after 'size (size before))
      (make <lookup> #:index after #:stride (stride self) #:term (term self)))
    (make <lookup> #:index (index self) #:stride (stride self) #:term (subst (term self) before after))))

(define-method (get (self <functional>) (idx <index>))
  (subst (term self) (index self) idx))

(define-method (fetch (self <elementary<>>))
  (fetch (typecode self) (memory self)))

(define-method (fetch (self <int>))
  self)

(define-method (expression->tensor self)
  "Pass-through value by default"
  self)

(define-method (lookup i s t)
  "Instantiate lookup object"
  (make <lookup> #:index i #:stride s #:term t))

(define-method (lookup i s (t <functional>))
  "Swap order so that functional objects are not inside lookup objects"
  (make <functional> #:index (index t) #:term (lookup i s (term t))))

(define-method (expression->tensor (self <llvmarray<>>))
  "Convert compiled array to function of index"
  (if (zero? (dimensions self))
    (make (elementary (typecode self)) #:memory (memory self))
    (let [(i (make <index>))]
      (slot-set! i 'size (llvm-last (shape self)))
      (make <functional>
            #:term (lookup i (llvm-last (strides self)) (expression->tensor (project self)))
            #:index i))))

(define-method (project (self <functional>))
  (project (term self) (index self)))

(define-method (project (self <functional>) (idx <index>))
  (make <functional> #:term (project (term self) idx) #:index (index self)))

(define-method (project (self <lookup>) (idx <index>))
  (if (eq? idx (index self))
    (term self)
    (make <lookup> #:index (index self) #:stride (stride self) #:term (project (term self) idx))))

(define-method (project (self <int>) (idx <index>))
  self)

(define-method (rebase (self <functional>) p)
  (make <functional> #:term (rebase (term self) p) #:index (index self)))

(define-method (rebase (self <lookup>) p)
  (make <lookup> #:index (index self) #:stride (stride self) #:term (rebase (term self) p)))

(define-method (rebase (self <elementary<>>) p)
  (make (class-of self) #:memory p))

(define-method (rebase (self <index>) i)
  i)

(define-method (stride (self <functional>))
  (stride (term self) (index self)))

(define-method (stride (self <functional>) (idx <index>))
  (stride (term self) idx))

(define-method (tensor-iterate (self <functional>))
  (tensor-iterate (term self) (index self)))

(define-method (tensor-iterate (self <functional>) (idx <index>))
  (tensor-iterate (term self) idx))

(define-method (tensor-iterate (self <lookup>) (idx <index>))
  (if (eq? (index self) idx)
    (list (build-phi (pointer (typecode self)))
          (memory self)
          (stride self))
    (tensor-iterate (term self) idx)))

(define-method (tensor-iterate (self <index>) (idx <index>))
  (list (build-phi <int>)
        (typed-constant <int> 0)
        (typed-constant <int> 1)))

(define-syntax tensor
  (lambda (x)
    (syntax-case x ()
      ((k (i n) expression) #'(let [(i (make <index>))] (slot-set! i 'size n) (make <functional> #:term expression #:index i)))
      ((k i expression)     #'(let [(i (make <index>))] (make <functional> #:term expression #:index i))))))

(define (elementwise-tensor result expression)
  (if (zero? (dimensions result))
    (store (memory result) (fetch expression))
    (let [(start  (make-basic-block "start"))
          (for    (make-basic-block "for"))
          (body   (make-basic-block "body"))
          (finish (make-basic-block "finish"))
          (end    (make-basic-block "end"))]
      (llvm-begin
        (build-branch start)
        (position-builder-at-end start)
        (let [(q (tensor-iterate expression))]
          (jit-let [(pend (+ (memory result) (* (llvm-last (shape result)) (llvm-last (strides result)))))]
            (build-branch for)
            (position-builder-at-end for)
            (jit-let [(p (build-phi (pointer (typecode result))))]
              (add-incoming p start (memory result))
              (add-incoming (car q) start (cadr q))
              (build-cond-branch (ne p pend) body end)
              (position-builder-at-end body)
              (elementwise-tensor (project (rebase result p)) (project (rebase expression (car q))))
              (build-branch finish)
              (position-builder-at-end finish)
              (add-incoming p finish (+ p (llvm-last (strides result))))
              (add-incoming (car q) finish (+ (car q) (caddr q)))
              (build-branch for)
              (position-builder-at-end end))))))))

(define (evaluate-tensor expression)
  (if (null? (shape expression))
    expression
    (jit-let [(result (allocate-array (typecode expression) (apply llvmlist (shape expression))))]
      (elementwise-tensor result expression)
      result)))

(define (adapted-native-type value) (if (is-a? value <integer>) <int> (native-type value)))

(define-syntax-rule (define-tensor (name args ...) expression)
  (define-method (name args ...)
    (let [(fun (jit (map adapted-native-type (list args ...))
                 (lambda arguments (apply (lambda (args ...) (evaluate-tensor expression)) (map expression->tensor arguments)))))]
      (add-method! name
                   (make <method> #:specializers (map class-of (list args ...))
                                  #:procedure fun))
      (name args ...))))
