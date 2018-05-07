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
(use-modules (srfi srfi-64))


(test-begin "aiscm ringbuffer")

(load-extension "libguile-aiscm-tests" "init_tests")

(test-assert "Fetching from empty ring buffer should return no data"
  (ringbuffer-fetch-empty))
(test-assert "Ring buffer initial size is as specified"
  (ringbuffer-initial-size))
(test-assert "Adding data to ring buffer sets fill"
  (ringbuffer-add-data))
(test-assert "Ring buffer should allow fetching stored data"
  (ringbuffer-store-and-fetch))
(test-assert "Adding more data to ring buffer appends to it"
  (ringbuffer-store-appends-data))
(test-assert "Do not fetch more than the specified number of bytes"
  (ringbuffer-fetch-limit))
(test-assert "Fetching from ring buffer advances it"
  (ringbuffer-fetching-advances))
(test-assert "Storing to ring buffer should be aware of offset"
  (ringbuffer-storing-respects-offset))
(test-assert "Ring buffer should wrap around"
  (ringbuffer-wrap-around))
(test-assert "Ringbuffer should grow n size if required"
  (ringbuffer-grow))
(test-assert "Ringbuffer writing should wrap around"
  (ringbuffer-wrap-write))
(test-assert "Flushing ring buffer should empty it"
  (ringbuffer-flushing))

(test-end "aiscm ringbuffer")
