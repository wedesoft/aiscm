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
(define-module (aiscm op)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (aiscm element)
  #:use-module (aiscm asm)
  #:use-module (aiscm expression)
  #:use-module (aiscm jit)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm util)
  #:export (fill))


(define ctx (make <context>))

(define (fill type shape value)
  (let* [(result-type  (multiarray type (length shape)))
         (args         (list (skeleton result-type) (skeleton type)))
         (parameters   (map parameter args))
         (prog         (virtual-variables '() (content-vars args) (attach (apply code parameters) (RET))))
         (instructions (asm ctx <null> (map typecode (content-vars args)) prog))
         (result       (make result-type #:shape shape))]
    (apply instructions (append-map unbuild (list result-type type) (list result value)))
    result))
