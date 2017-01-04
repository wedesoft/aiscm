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
(use-modules (oop goops)
             (aiscm int)
             (aiscm float)
             (aiscm pulse)
             (guile-tap))
(ok (eqv? PA_SAMPLE_U8 (type->pulse-type <ubyte>))
    "convert unsigned byte to Pulse audio type")
(ok (eqv? PA_SAMPLE_S16LE (type->pulse-type <sint>))
    "convert short integer to Pulse audio type")
(ok (eqv? PA_SAMPLE_S32LE (type->pulse-type <int>))
    "convert integer to Pulse audio type")
(ok (eqv? PA_SAMPLE_FLOAT32LE (type->pulse-type <float>))
    "convert floating-point to Pulse audio type")
(ok (throws? (type->pulse-type <usint>))
    "throw error if type not supported by Pulse audio")
(ok (eq? <sint> (pulse-type->type PA_SAMPLE_S16LE))
    "convert Pulse audio short integer to integer type")
(run-tests)
