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
(define-module (aiscm tensorflow)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (aiscm core)
  #:use-module (aiscm util)
  #:export (to-tensor from-tensor placeholder identity_ make-session run variable const_ assign))

(load-extension "libguile-aiscm-tensorflow" "init_tensorflow")

(define graph (make-graph))

(define typemap
  (list (cons <ubyte>  TF_UINT8 )
        (cons <byte>   TF_INT8  )
        (cons <usint>  TF_UINT16)
        (cons <sint>   TF_INT16 )
        (cons <uint>   TF_UINT32)
        (cons <int>    TF_INT32 )
        (cons <ulong>  TF_UINT64)
        (cons <long>   TF_INT64 )
        (cons <float>  TF_FLOAT )
        (cons <double> TF_DOUBLE)))

(define inverse-typemap (alist-invert typemap))

(define-method (to-tensor (arr <multiarray<>>))
  (make-tensor (assq-ref typemap (typecode arr)) (shape arr) (size-of arr) (memory arr)))

(define-method (to-tensor value)
  (let [(arr (make (multiarray (native-type value) 0) #:shape '()))]
    (set arr value)
    (to-tensor arr)))

(define (from-tensor tensor)
  (let [(info (tf-from-tensor tensor))]
    (get (make (multiarray (assq-ref inverse-typemap (car info)) (length (cadr info))) #:shape (cadr info) #:memory (caddr info)))))

(define (make-session)
  (make-tf-session graph))

(define (placeholder . args)
  (let-keywords args #f (dtype)
    (let [(description (make-description graph "Placeholder" (gensym "x")))]
      (tf-set-attr-type description "dtype" (assq-ref typemap dtype))
      (tf-finish-operation description))))

(define (identity_ input . args)
  (let-keywords args #f (T)
    (let [(description (make-description graph "Identity" (gensym "x")))]
      (tf-add-input description input)
      (if T (tf-set-attr-type description "T" (assq-ref typemap T)))
      (tf-finish-operation description))))

(define (variable . args)
  (let-keywords args #f (dtype shape)
    (let [(description (make-description graph "Variable" (gensym "x")))]
      (tf-set-attr-type description "dtype" (assq-ref typemap dtype))
      (tf-set-attr-shape description "shape" shape)
      (tf-finish-operation description))))

(define (const_ . args)
  (let-keywords args #f (value dtype)
    (tf-const graph (gensym "x") value (assq-ref typemap dtype))))

(define (assign ref value)
  (let [(description (make-description graph "Assign" (gensym "x")))]
    (tf-add-input description ref)
    (tf-add-input description value)
    (tf-finish-operation description)))
