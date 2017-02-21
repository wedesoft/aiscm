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

(load-extension "libguile-aiscm-tests" "init_tests")

(define stereo-values '((2 3) (5 7) (11 13) (17 19)))
(define stereo-array (to-array <sint> stereo-values))
(define stereo-mem (value stereo-array))
(define stereo-samples (make <samples> #:typecode <sint> #:shape '(2 4) #:rate 22050 #:planar #f #:mem stereo-mem))

(define mono-values '(2 3 5 7))
(define mono-array (to-array <sint> mono-values))

(define planar-values '((2 5 11 17) (3 7 13 19)))
(define planar-array (roll (to-array <sint> planar-values)))
(define planar-mem (value planar-array))
(define planar-samples (make <samples> #:typecode <sint> #:shape '(2 4) #:rate 22050 #:planar #t #:mem planar-mem))

(test-begin "convert offsets to pointers")
  (test-assert "Test first pointer"
    (first-pointer-from-offset))
  (test-assert "Test second pointer"
    (second-pointer-from-offset))
(test-end "convert offsets to pointers")

(test-eq "query typecode of samples"
  <sint> (typecode stereo-samples))
(test-equal "query shape of samples"
  '(2 4) (shape stereo-samples))
(test-eqv "query number of channels"
  2 (channels stereo-samples))
(test-eqv "query sampling rate"
  22050 (rate stereo-samples))
(test-assert "query whether samples are planar"
  (not (planar? stereo-samples)))
(test-eq "check data is memorized"
  stereo-mem (slot-ref stereo-samples 'mem))
(test-equal "'to-array' should convert the audio samples to a 2D array"
  stereo-values (to-list (to-array stereo-samples)))
(test-eq "check sample type when converting array to samples"
  <sint> (typecode (to-samples stereo-array 22050)))
(test-assert "check samples are not marked as planar"
  (not (planar? (to-samples stereo-array 22050))))
(test-equal "check samples converted from array have the right shape"
  '(2 4) (shape (to-samples stereo-array 22050)))
(test-eq "check sample memory is initialised when converting from array"
  (slot-ref stereo-array 'value) (slot-ref (to-samples stereo-array 22050) 'mem))
(test-eqv "use specified sampling rate when converting from array to samples"
  22050 (rate (to-samples stereo-array 22050)))
(test-equal "packed audio has one offset which is zero"
  '(0) (slot-ref stereo-samples 'offsets))

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
  <int> (typecode (convert-samples stereo-samples <int> #f)))
(test-eqv "size of converted sample data"
  32 (slot-ref (slot-ref (convert-samples stereo-samples <int> #f) 'mem) 'size))
(test-equal "content of converted array"
  (map (cut map (cut ash <> 16) <>) stereo-values) (to-list (to-array (convert-samples stereo-samples <int> #f))))
(test-eq "trivial conversion from short integer to short integer"
  <sint> (typecode (convert-samples stereo-samples <sint> #f)))
(test-equal "content of trivial conversion"
  stereo-values (to-list (to-array (convert-samples stereo-samples <sint> #f))))
(test-equal "check shape of mono samples is two-dimensional"
  '(1 4) (shape (to-samples mono-array 22050)))
(test-equal "check samples get compacted"
  '((2) (5) (11) (17)) (to-list (to-array (to-samples (get (roll stereo-array) 0) 22050))))
(test-assert "planar audio sample is marked as such"
  (planar? planar-samples))
(test-equal "planar audio has offsets including element size"
  '(0 8) (slot-ref planar-samples 'offsets))
(test-expect-fail 2)
(test-equal "convert planar to packed audio samples"
  stereo-values (to-list (to-array (convert-samples planar-samples <sint> #f))))
(test-equal "pack audio samples before converting to array"
  stereo-values (to-list (to-array planar-samples)))

(test-end "aiscm samples")
