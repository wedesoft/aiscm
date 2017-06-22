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
(define-module (aiscm loop)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm int)
  #:use-module (aiscm asm)
  #:use-module (aiscm element)
  #:use-module (aiscm sequence)
  #:use-module (aiscm variable)
  #:use-module (aiscm expression)
  #:export (<loop-detail> <multi-loop>
            iterator step loop-setup loop-increment
            loop-details body multi-loop)
  #:re-export (typecode stride base))


(define-class <loop-detail> ()
  (typecode #:init-keyword #:typecode #:getter typecode)
  (iterator #:init-keyword #:iterator #:getter iterator)
  (step     #:init-keyword #:step     #:getter step    )
  (stride   #:init-keyword #:stride   #:getter stride  )
  (base     #:init-keyword #:base     #:getter base    ))

(define (loop-setup self)
  (list (IMUL (step self) (value (stride self)) (size-of (typecode self)))
        (MOV (iterator self) (base self))))

(define (loop-increment self)
  (list (ADD (iterator self) (step self))))

(define-class <multi-loop> ()
  (loop-details #:init-keyword #:loop-details #:getter loop-details)
  (body         #:init-keyword #:body         #:getter body        ))

(define-method (multi-loop (self <lookup>) (idx <var>))
  (if (eq? idx (index self))
    (let* [(iterator    (var <long>))
           (step        (var <long>))
           (loop-detail (make <loop-detail> #:typecode (typecode self)
                                            #:iterator iterator
                                            #:step     step
                                            #:stride   (stride self)
                                            #:base     (value self)))]
      (make <multi-loop> #:loop-details (list loop-detail)
                         #:body         (rebase iterator (delegate self))))
    (let [(t (multi-loop (delegate self) idx))]
      (make <multi-loop> #:loop-details (loop-details t)
                         #:body         (lookup (index self) (body t) (stride self))))))

(define-method (multi-loop (self <indexer>) (idx <var>))
  (let [(t (multi-loop (delegate self) idx))]
    (make <multi-loop> #:loop-details (loop-details t)
                       #:body         (indexer (index self) (body t) (dimension self)))))

(define-method (multi-loop (self <indexer>))
  (multi-loop (delegate self) (index self)))

(define-method (multi-loop (self <function>) . idx)
  (let* [(arguments (map (cut apply multi-loop <> idx) (delegate self)))
         (details   (append-map loop-details arguments))
         (bodies    (map body arguments))]
    (make <multi-loop> #:loop-details details #:body (apply (name self) bodies))))

(define-method (multi-loop (self <param>) . idx)
  (make <multi-loop> #:loop-details '() #:body self))
