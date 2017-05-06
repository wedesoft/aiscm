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
(define-module (aiscm program)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm element)
  #:use-module (aiscm asm)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:export (labels substitute-variables))


(define (labels prog)
  "Get positions of labels in program"
  (filter (compose symbol? car) (map cons prog (iota (length prog)))))

(define-method (substitute-variables self alist) self)
(define-method (substitute-variables (self <var>) alist)
  "replace variable with associated value if there is one"
  (let [(target (assq-ref alist self))]
    (if (or (is-a? target <register>) (is-a? target <address>))
      (to-type (typecode self) target)
      (or target self))))
(define-method (substitute-variables (self <ptr>) alist)
  (let [(target (substitute-variables (car (get-args self)) alist))]
    (if (is-a? target <pair>)
      (ptr (typecode self) (car target) (+ (cadr (get-args self)) (cdr target)))
      (apply ptr (typecode self) target (cdr (get-args self))))))
(define-method (substitute-variables (self <cmd>) alist)
  (apply (get-op self) (map (cut substitute-variables <> alist) (get-args self))))
(define-method (substitute-variables (self <list>) alist)
  (map (cut substitute-variables <> alist) self))
