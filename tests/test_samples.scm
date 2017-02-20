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
(use-modules (srfi srfi-64)
             (oop goops)
             (aiscm element)
             (aiscm int)
             (aiscm float)
             (aiscm mem)
             (aiscm sequence)
             (aiscm samples))


(test-begin "aiscm samples")

(define l '((2 3) (5 7) (11 13) (17 19)))
(define m (to-array <sint> l))
(define mem (value m))
(define samples (make <samples> #:typecode <sint> #:shape '(2 4) #:rate 22050 #:planar #f #:mem mem))
(define array (arr <int> (2 3) (5 7) (11 13) (17 19)))

(test-eq "query typecode of samples"
  <sint> (typecode samples))
(test-equal "query shape of samples"
  '(2 4) (shape samples))
(test-eqv "query number of channels"
  2 (channels samples))
(test-eqv "query sampling rate"
  22050 (rate samples))
(test-assert "query whether samples are planar"
  (not (planar? samples)))
(test-eq "check data is memorized"
  mem (slot-ref samples 'mem))
(test-equal "'to-array' should convert the audio samples to a 2D array"
  l (to-list (to-array samples)))
(test-eq "check sample type when converting array to samples"
  <int> (typecode (to-samples array 22050)))
(test-assert "check samples are not marked as planar"
  (not (planar? (to-samples array 22050))))
(test-equal "check samples converted from array have the right shape"
  '(2 4) (shape (to-samples array 22050)))
(test-eq "check sample memory is initialised when converting from array"
  (slot-ref array 'value) (slot-ref (to-samples array 22050) 'mem))
(test-eqv "use specified sampling rate when converting from array to samples"
  22050 (rate (to-samples array 22050)))
(test-equal "packed audio has one offset which is zero"
  '(0) (slot-ref samples 'offsets))

(test-begin "type conversions")
  (test-eq "test packed unsigned byte format tag"
    AV_SAMPLE_FMT_U8   (type+planar->avtype <ubyte> #f))
  (test-eq "test packed signed short integer format tag"
    AV_SAMPLE_FMT_S16  (type+planar->avtype <sint> #f))
  (test-eq "test packed signed integer format tag"
    AV_SAMPLE_FMT_S32  (type+planar->avtype <int> #f))
  (test-eq "test packed floating point format tag"
    AV_SAMPLE_FMT_FLT  (type+planar->avtype <float> #f))
  (test-eq "test packed double precision floating point format tag"
    AV_SAMPLE_FMT_DBL  (type+planar->avtype <double> #f))
  (test-eq "test planar unsigned byte format tag"
    AV_SAMPLE_FMT_U8P  (type+planar->avtype <ubyte> #t))
  (test-eq "test planar signed short integer format tag"
    AV_SAMPLE_FMT_S16P (type+planar->avtype <sint> #t))
  (test-eq "test planar signed integer format tag"
    AV_SAMPLE_FMT_S32P (type+planar->avtype <int> #t))
  (test-eq "test planar floating point format tag"
    AV_SAMPLE_FMT_FLTP (type+planar->avtype <float> #t))
  (test-eq "test planar double precision floating point format tag"
    AV_SAMPLE_FMT_DBLP (type+planar->avtype <double> #t))
  (test-error "throw error if type not supported by FFmpeg audio"
    'misc-error (type+planar->avtype <byte> #f))
  (test-eq "convert packed  unsigned byte type tag to type"
    <ubyte> (avtype->type AV_SAMPLE_FMT_U8))
  (test-eq "convert signed short integer type tag to type"
    <sint> (avtype->type AV_SAMPLE_FMT_S16))
  (test-eq "convert planar unsigned byte type tag to type"
    <ubyte> (avtype->type AV_SAMPLE_FMT_U8P))
(test-end "type conversions")

(test-eq "convert samples to integer"
  <int> (typecode (convert-samples samples <int> 22050 #f)))
(test-eqv "size of converted sample data"
  32 (slot-ref (slot-ref (convert-samples samples <int> 22050 #f) 'mem) 'size))
(test-equal "content of converted array"
  (map (cut map (cut ash <> 16) <>) l) (to-list (to-array (convert-samples samples <int> 22050 #f))))
(test-eq "trivial conversion from short integer to short integer"
  <sint> (typecode (convert-samples samples <sint> 22050 #f)))
(test-equal "content of trivial conversion"
  l (to-list (to-array (convert-samples samples <sint> 22050 #f))))
(test-equal "increasing sampling rate increases the sample size"
  '(2 8) (shape (convert-samples samples <sint> 44100 #f)))
(test-equal "increasing sampling rate increases the sample size"
  '(2 8) (shape (convert-samples samples <sint> 44100 #f)))

(test-end "aiscm samples")
