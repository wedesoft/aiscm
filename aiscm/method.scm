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
(define-module (aiscm method)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:export (<native-method> <native-value>
            make-native-method function-pointer return-type argument-types native-value)
  #:re-export (get))

(define-class <native-method> ()
  (function-pointer #:init-keyword #:function-pointer #:getter function-pointer)
  (return-type      #:init-keyword #:return-type      #:getter return-type)
  (argument-types   #:init-keyword #:argument-types   #:getter argument-types))

(define (make-native-method return-type argument-types function-pointer)
  (make <native-method> #:function-pointer function-pointer
                        #:return-type      return-type
                        #:argument-types   argument-types))

(define-class <native-value> ()
  (value       #:init-keyword #:value       #:getter get)
  (return-type #:init-keyword #:return-type #:getter return-type))

(define (native-value return-type value)
  (make <native-value> #:value value #:return-type return-type))
