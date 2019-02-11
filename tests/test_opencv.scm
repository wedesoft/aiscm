;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
(use-modules (srfi srfi-64)
             (aiscm core)
             (aiscm opencv))

(test-begin "aiscm opencv")

(test-group "connected components"
  (test-equal "connected components of unsigned byte array"
    '((1 0 0) (0 0 2)) (to-list (car (connected-components (arr (1 0 0) (0 0 1)) 8))))
  (test-eqv "count number of connected components"
    3 (cdr (connected-components (arr (1 0 0) (0 0 1)) 8)))
  (test-eq "return integer array by default"
    <int> (typecode (car (connected-components (arr (1 0 0) (0 0 1)) 8))))
  (test-eq "return unsigned short int if requested"
    <usint> (typecode (car (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <usint>))))
  (test-equal "connected components with unsigned short int result"
    '((1 0 0) (0 0 2)) (to-list (car (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <usint>))))
  (test-error "throw error if label type is unsupported"
    'misc-error (connected-components (arr (1 0 0) (0 0 1)) 8 #:label-type <byte>)))

(test-group "Aruco markers"
  (test-equal "shape of Charuco board"
    '(500 700) (charuco-board 5 7 100 50 DICT_4X4_50))
  (test-error "throw error if board has wrong parameters"
    'misc-error (charuco-board 5 7 100 150 DICT_4X4_50)))

(test-end "aiscm opencv")
