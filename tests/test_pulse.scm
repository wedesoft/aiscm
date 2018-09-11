;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
             (oop goops)
             (aiscm core)
             (aiscm pulse))


(test-begin "aiscm pulse")

(test-eqv "convert unsigned byte to Pulse audio type"
  PA_SAMPLE_U8 (type->pulse-type <ubyte>))
(test-eqv "convert short integer to Pulse audio type"
  PA_SAMPLE_S16LE (type->pulse-type <sint>))
(test-eqv "convert integer to Pulse audio type"
  PA_SAMPLE_S32LE (type->pulse-type <int>))
(test-eqv "convert floating-point to Pulse audio type"
  PA_SAMPLE_FLOAT32LE (type->pulse-type <float>))
(test-error "throw error if type not supported by Pulse audio"
  'misc-error (type->pulse-type <usint>))
(test-eq "convert Pulse audio short integer to integer type"
  <sint> (pulse-type->type PA_SAMPLE_S16LE))

(test-end "aiscm pulse")
