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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (aiscm core)
  #:use-module (aiscm util)
  #:export (<tensor> <index> <functional> <lookup> <elementary<>> <tensormap> <reduction>
            expression->tensor operation arguments)
  #:export-syntax (define-tensor tensor term index stride elementary tensor-iterate sum-over product-over min-over max-over)
  #:re-export (get memory shape typecode project rebase + - * ~ sqrt sin cos tan asin acos atan exp pow minor major))

(define-class <tensor> ())

(define-class <index> (<tensor>)
  (size #:init-keyword #:size #:getter size))

(define-method (typecode (self <index>))
  "Indices denote integer values"
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
  "Shape of function object adds one dimension to the contained term"
  (cons (size (index self)) (shape (term self))))

(define-method (typecode (self <functional>))
  "Element type of function object is type of contained term"
  (typecode (term self)))

(define-method (memory (self <functional>))
  "Memory of function object is memory of contained term"
  (memory (term self)))

(define-class <lookup> (<tensor>)
  (index  #:init-keyword #:index  #:getter index )
  (stride #:init-keyword #:stride #:getter stride)
  (term   #:init-keyword #:term   #:getter term  ))

(define-method (typecode (self <lookup>))
  "Element type of lookup object is type of contained term"
  (typecode (term self)))

(define-method (memory (self <lookup>))
  "Memory of lookup object is memory of contained term"
  (memory (term self)))

(define-class <tensormap> (<tensor>)
  (operation #:init-keyword #:operation #:getter operation)
  (arguments #:init-keyword #:arguments #:getter arguments)
  (coercion  #:init-keyword #:coercion  #:getter coercion ))

(define-class <reduction> (<tensor>)
  (operation #:init-keyword #:operation #:getter operation)
  (index     #:init-keyword #:index     #:getter index    )
  (term      #:init-keyword #:term      #:getter term     ))

(define-method (typecode (self <reduction>))
  (typecode (term self)))

(define-method (stride (self <functional>))
  "Stride of function object is stride for index bound by this object"
  (stride (term self) (index self)))

(define-method (stride (self <functional>) (idx <index>))
  "Search for lookup object recursively when encountering another function object"
  (stride (term self) idx))

(define-method (stride (self <lookup>) (idx <index>))
  "Return stride of lookup object if index matches"
  (if (eq? (index self) idx)
    (stride self)
    (stride (term self) idx)))

(define-method (subst (self <functional>) (before <index>) (after <index>))
  "Recursively substitute index in expression"
  (make <functional> #:term (subst (term self) before after) #:index (index self)))

(define-method (subst (self <lookup>) (before <index>) (after <index>))
  "Substitute matching index in lookup object"
  (if (eq? (index self) before)
    (begin
      (slot-set! after 'size (size before))
      (make <lookup> #:index after #:stride (stride self) #:term (term self)))
    (make <lookup> #:index (index self) #:stride (stride self) #:term (subst (term self) before after))))

(define-method (get (self <tensor>)) self)

(define-method (get (self <tensor>) (first <index>) . rest)
  "Indexing a function object removes function and substitutes index"
  (let* [(indices (cons first rest))
         (idx     (last indices))]
    (apply get (subst (term self) (index self) idx) (all-but-last indices))))

(define-method (fetch (self <elementary<>>))
  "Fetch value from memory"
  (fetch (typecode self) (memory self)))

(define-method (fetch (self <tensormap>))
  (apply (operation self) (map fetch (arguments self))))

(define-method (fetch (self <void>))
  "Fetch on scalar has no effect"
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
      (slot-set! i 'size (llvm-car (shape self)))
      (make <functional>
            #:term (lookup i (llvm-car (strides self)) (expression->tensor (project self)))
            #:index i))))

(define-method (project (self <functional>))
  "Remove functional object and corresponding lookup object with matching index"
  (project (term self) (index self)))

(define-method (project (self <functional>) (idx <index>))
  "Recursively search for lookup object to remove"
  (make <functional> #:term (project (term self) idx) #:index (index self)))

(define-method (project (self <lookup>) (idx <index>))
  "Remove lookup object if index matches"
  (if (eq? idx (index self))
    (term self)
    (make <lookup> #:index (index self) #:stride (stride self) #:term (project (term self) idx))))

(define-method (rebase (self <lookup>) p)
  "Rebase lookup object with given pointer or index"
  (make <lookup> #:index (index self) #:stride (stride self) #:term (rebase (term self) p)))

(define-method (rebase (self <elementary<>>) p)
  "Change pointer of element-accessing object"
  (make (class-of self) #:memory p))

(define-syntax-rule (define-unary-tensor-op coercion op)
  "Define unary operation to use in tensor expressions"
  (begin
    (define-method (op (a <tensor>))
      (make <tensormap> #:operation op #:arguments (list a) #:coercion coercion))
    (define-method (op (a <functional>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op (get a i)))))))

(define-unary-tensor-op identity -   )
(define-unary-tensor-op identity ~   )
(define-unary-tensor-op to-float sqrt)
(define-unary-tensor-op to-float sin )
(define-unary-tensor-op to-float cos )
(define-unary-tensor-op to-float tan )
(define-unary-tensor-op to-float asin)
(define-unary-tensor-op to-float acos)
(define-unary-tensor-op to-float atan)
(define-unary-tensor-op to-float exp )

(define-syntax-rule (define-binary-tensor-op coercion op)
  "Define binary operation to use in tensor expressions"
  (begin
    (define-method (op (a <tensor>) (b <tensor>))
      (make <tensormap> #:operation op #:arguments (list a b) #:coercion coercion))
    (define-method (op (a <tensor>) (b <void>))
      (make <tensormap> #:operation op #:arguments (list a b) #:coercion coercion))
    (define-method (op (a <void>) (b <tensor>))
      (make <tensormap> #:operation op #:arguments (list a b) #:coercion coercion))
    (define-method (op a (b <tensor>))
      (op (typed-constant (native-type a) a) b))
    (define-method (op (a <tensor>) b)
      (op a (typed-constant (native-type b) b)))
    (define-method (op (a <functional>) (b <functional>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op (get a i) (get b i)))))
    (define-method (op (a <functional>) (b <void>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op (get a i) b))))
    (define-method (op (a <functional>) (b <lookup>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op (get a i) b))))
    (define-method (op (a <lookup>) (b <functional>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op a (get b i)))))
    (define-method (op (a <void>) (b <functional>))
      (let [(i  (make <index>))]
        (make <functional> #:index i #:term (op a (get b i)))))
    (define-method (op a (b <functional>))
      (op (typed-constant (native-type a) a) b))
    (define-method (op (a <functional>) b)
      (op a (typed-constant (native-type b) b)))))

(define-binary-tensor-op coerce   +    )
(define-binary-tensor-op coerce   -    )
(define-binary-tensor-op coerce   *    )
(define-binary-tensor-op coerce   /    )
(define-binary-tensor-op coerce   minor)
(define-binary-tensor-op coerce   major)
(define-binary-tensor-op coerce   pow  )
(define-binary-tensor-op to-float atan )

(define-method (typecode (self <tensormap>))
  "Get typecode of elementwise operation"
  (apply (coercion self) (map typecode (arguments self))))

(define-method (tensor-iterate (self <functional>))
  "Get iterator information by using index of function object"
  (tensor-iterate (term self) (index self)))

(define-method (tensor-iterate (self <functional>) (idx <index>))
  "Recursively get iterator information"
  (let [(iter (tensor-iterate (term self) idx))]
    (list (car iter) (cadr iter) (caddr iter) (make <functional> #:term (cadddr iter) #:index (index self)))))

(define-method (tensor-iterate (self <reduction>) (idx <index>))
  (let [(iter (tensor-iterate (term self) idx))]
    (list (car iter) (cadr iter) (caddr iter)
          (make <reduction> #:operation (operation self) #:term (cadddr iter) #:index (index self)))))

(define-method (tensor-iterate (self <lookup>) (idx <index>))
  "Return iterator information and rebased lookup object if index matches"
  (if (eq? (index self) idx)
    (let [(p (build-phi (pointer (typecode self))))]
      (list (list p) (list (memory self)) (list (stride self)) (rebase (term self) p)))
    (let [(iter (tensor-iterate (term self) idx))]
      (list (car iter) (cadr iter) (caddr iter) (make <lookup> #:term (cadddr iter) #:stride (stride self) #:index (index self))))))

(define-method (tensor-iterate (self <index>) (idx <index>))
  "Return iterator information and rebased index if index matches"
  (if (eq? self idx)
    (let [(i (build-phi <int>))]
      (list (list i) (list (typed-constant <int> 0)) (list (typed-constant <int> 1)) i))
    (list '() '() '() self)))

(define-method (tensor-iterate (self <void>) (idx <index>))
  "Return empty iterator information when encountering instantiated index"
  (list '() '() '() self))

(define-method (tensor-iterate (self <elementary<>>) (idx <index>))
  "Return empty iterator information when encountering lower-dimensional tensor"
  (list '() '() '() self))

(define-method (tensor-iterate (self <tensormap>) (idx <index>))
  (let [(args-iterate (map (cut tensor-iterate <> idx) (arguments self)))]
    (list (append-map car   args-iterate)
          (append-map cadr  args-iterate)
          (append-map caddr args-iterate)
          (make <tensormap> #:operation (operation self) #:arguments (map cadddr args-iterate) #:coercion (coercion self)))))

(define-syntax tensor
  (lambda (x)
    (syntax-case x ()
      ((k (i n) expression) #'(let [(i (make <index>))] (slot-set! i 'size n) (make <functional> #:term expression #:index i)))
      ((k i expression)     #'(let [(i (make <index>))] (make <functional> #:term expression #:index i))))))

(define (elementwise-tensor result expression)
  "Element-wise computation of tensor expression"
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
          (jit-let [(pend (+ (memory result) (* (llvm-car (shape result)) (llvm-car (strides result)))))]
            (build-branch for)
            (position-builder-at-end for)
            (jit-let [(p (build-phi (pointer (typecode result))))]
              (add-incoming p start (memory result))
              (apply llvm-begin (map (cut add-incoming <> start <>) (car q) (cadr q)))
              (build-cond-branch (ne p pend) body end)
              (position-builder-at-end body)
              (elementwise-tensor (project (rebase result p)) (cadddr q))
              (build-branch finish)
              (position-builder-at-end finish)
              (add-incoming p finish (+ p (llvm-car (strides result))))
              (apply llvm-begin (map (lambda (ptr stride) (add-incoming ptr finish (+ ptr stride))) (car q) (caddr q)))
              (build-branch for)
              (position-builder-at-end end))))))))

(define (evaluate-tensor expression)
  "Evaluate expression when scalar and element-wise evaluate otherwise"
  (if (null? (shape expression))
    (fetch expression)
    (jit-let [(result (allocate-array (typecode expression) (apply llvmlist (shape expression))))]
      (elementwise-tensor result expression)
      result)))

(define-method (reduction expression idx op)
  (make <reduction> #:term expression #:index idx #:operation op))

(define-method (reduction (expression <functional>) idx op)
  (make <functional> #:index (index expression) #:term (reduction (term expression) idx op)))

(define-syntax-rule (define-reduction name op)
  (define-syntax name
    (lambda (x)
      (syntax-case x ()
        ((k (i n) expression) #'(let [(i (make <index>))] (slot-set! i 'size n) (reduction expression i op)))
        ((k i expression)     #'(let [(i (make <index>))] (reduction expression i op)))))))

(define-reduction sum-over     +    )
(define-reduction product-over *    )
(define-reduction min-over     minor)
(define-reduction max-over     major)

(define-method (fetch (self <reduction>))
  (let [(q0     (tensor-iterate (term self) (index self)))
        (q      (tensor-iterate (term self) (index self)))
        (i      (build-phi <int>))
        (entry  (make-basic-block "entry" ))
        (init   (make-basic-block "init"  ))
        (start  (make-basic-block "start" ))
        (for    (make-basic-block "for"   ))
        (body   (make-basic-block "body"  ))
        (finish (make-basic-block "finish"))
        (end    (make-basic-block "end"   ))]
    (llvm-begin
      (build-branch entry)
      (position-builder-at-end entry)
      (build-branch init)
      (position-builder-at-end init)
      (apply llvm-begin (map (cut add-incoming <> entry <>) (car q0) (cadr q0)))
      (let [(q1 (map + (cadr q) (caddr q)))]
        (jit-let [(result0 (fetch (cadddr q0)))]
          (apply llvm-begin q1)
          (build-branch start)
          (position-builder-at-end start)
          (build-branch for)
          (position-builder-at-end for)
          (jit-let [(result (build-phi (typecode self)))
                    (i      (build-phi <int>))]
            (add-incoming result start result0)
            (add-incoming i start (typed-constant <int> 1))
            (apply llvm-begin (map (lambda (ptr ptr1) (add-incoming ptr start ptr1)) (car q) q1))
            (build-cond-branch (ne i (size (index self))) body end)
            (position-builder-at-end body)
            (jit-let [(result1 (fetch (cadddr q)))]
              (build-branch finish)
              (position-builder-at-end finish)
              (add-incoming result finish ((operation self) result result1))
              (add-incoming i finish (+ i (typed-constant <int> 1)))
              (apply llvm-begin (map (lambda (ptr str) (add-incoming ptr finish (+ ptr str))) (car q) (caddr q)))
              (build-branch for)
              (position-builder-at-end end)
              result)))))))

(define (adapted-native-type value) (if (is-a? value <integer>) <int> (native-type value)))

(define-syntax-rule (define-tensor (name args ...) expression)
  "Define jit-compilation for a tensor expression with given arguments"
  (define-method (name args ...)
    (let [(fun (jit (map adapted-native-type (list args ...))
                 (lambda arguments (apply (lambda (args ...) (evaluate-tensor expression)) (map expression->tensor arguments)))))]
      (add-method! name
                   (make <method> #:specializers (map class-of (list args ...))
                                  #:procedure fun))
      (name args ...))))
