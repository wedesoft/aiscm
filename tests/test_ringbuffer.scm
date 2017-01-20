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
(use-modules (guile-tap))

(load-extension "libguile-aiscm-tests" "init_tests")

(ok (ringbuffer-fetch-empty)
    "Fetching from empty ring buffer should return no data")
(ok (ringbuffer-initial-size)
    "Ring buffer initial size is as specified")
(ok (ringbuffer-add-data)
    "Adding data to ring buffer sets fill")
(ok (ringbuffer-store-and-fetch)
    "Ring buffer should allow fetching stored data")
(ok (ringbuffer-store-appends-data)
    "Adding more data to ring buffer appends to it")
(ok (ringbuffer-fetch-limit)
    "Do not fetch more than the specified number of bytes")
(ok (ringbuffer-fetching-advances)
    "Fetching from ring buffer advances it")
(ok (ringbuffer-storing-respects-offset)
    "Storing to ring buffer should be aware of offset")
(ok (ringbuffer-wrap-around)
    "Ring buffer should wrap around")
(ok (ringbuffer-grow)
    "Ringbuffer should grow n size if required")
(ok (ringbuffer-wrap-write)
    "Ringbuffer writing should wrap around")
(ok (ringbuffer-flushing)
    "Flushing ring buffer should empty it")
(run-tests)

