;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
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
