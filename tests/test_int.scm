(use-modules (aiscm int)
             (oop goops)
             (guile-tap))
(planned-tests 21)
(ok (equal? 64 (bits (make-int-class 64 signed)))
  "Query number of bits from integer class")
(ok (signed? (make-int-class 64 signed))
  "Query signed-ness from signed integer class")
(ok (not (signed? (make-int-class 64 unsigned)))
  "Query signed-ness from unsigned integer class")
(todo '((ok (equal? (make-ubyte #x21) (make-usint #x21))
          "equal integer objects"))
  "Does not work with different classes")
(ok (not (equal? (make-ubyte #x21) (make-usint #x4321)))
  "unequal integer objects")
(ok (equal? 128 (bits (make-int-class 128 signed)))
  "integer class maintains number of bits")
(ok (signed? (make-int-class 128 signed))
  "integer class maintains signedness for signed integer")
(ok (not (signed? (make-int-class 128 unsigned)))
  "integer class maintains signedness for unsigned integer")
(ok (equal?
      #vu8(#x01 #x02)
      (pack (make (make-int-class 16 unsigned) #:value #x0201)))
  "pack custom integer value")
(ok (equal? #vu8(#xff) (pack (make-ubyte #xff)))
  "pack unsigned byte value")
(ok (equal? #vu8(#x01 #x02) (pack (make-usint #x0201)))
  "pack unsigned short integer value")
(ok (equal? #vu8(#x01 #x02 #x03 #x04) (pack (make-uint #x04030201)))
  "pack unsigned integer value")
(ok (equal? (unpack <ubyte> #vu8(#xff)) (make-ubyte #xff))
  "unpack unsigned byte value")
(ok (equal? (unpack <usint> #vu8(#x01 #x02)) (make-usint #x0201))
  "unpack unsigned short integer value")
(ok (equal? (unpack <uint> #vu8(#x01 #x02 #x03 #x04)) (make-uint #x04030201))
  "unpack unsigned integer value")
(ok (equal? 127 (slot-ref (unpack <byte> (pack (make-byte 127))) 'value))
  "pack and unpack signed byte")
(ok (equal? -128 (slot-ref (unpack <byte> (pack (make-byte -128))) 'value))
  "pack and unpack signed byte with negative number")
(ok (equal? 32767 (slot-ref (unpack <sint> (pack (make-sint 32767))) 'value))
  "pack and unpack signed short integer")
(ok (equal? -32768 (slot-ref (unpack <sint> (pack (make-sint -32768))) 'value))
  "pack and unpack signed short integer with negative number")
(ok (equal? 2147483647 (slot-ref (unpack <int> (pack (make-int 2147483647))) 'value))
  "pack and unpack signed integer")
(ok (equal? -2147483648 (slot-ref (unpack <int> (pack (make-int -2147483648))) 'value))
  "pack and unpack signed integer with negative number")
(format #t "~&")
