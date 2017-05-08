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
(define-module (aiscm compile)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm element)
  #:use-module (aiscm asm)
  #:use-module (aiscm util)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:export (replace-variables))


(define (replace-variables allocation cmd temporary)
  "Replace variables with registers and add spill code if necessary"
  (let* [(location         (cut assq-ref allocation <>))
         (primary-argument (first-argument cmd))
         (primary-location (location primary-argument))]
    ; cases requiring more than one temporary variable are not handled at the moment
    (if (is-a? primary-location <address>)
      (let [(register (to-type (typecode primary-argument) temporary))]
        (compact (and (memv primary-argument (input cmd)) (MOV register primary-location))
                 (substitute-variables cmd (assq-set allocation primary-argument temporary))
                 (and (memv primary-argument (output cmd)) (MOV primary-location register))))
      (let [(spilled-pointer (filter (compose (cut is-a? <> <address>) location) (get-ptr-args cmd)))]
        ; assumption: (get-ptr-args cmd) only returns zero or one pointer argument requiring a temporary variable
        (attach (map (compose (cut MOV temporary <>) location) spilled-pointer)
                (substitute-variables cmd (fold (lambda (var alist) (assq-set alist var temporary)) allocation spilled-pointer)))))))
