# Native data types

## Native integers

The following example shows how to instantiate a 32-bit signed integer object with value 42.

```Scheme
(use-modules (oop goops) (aiscm int))
(make <int> #:value 42)
; #<<int<32,signed>> 42>

```
