#!r6rs
;; Import order important

(import
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)	
   (st-sequence-coll)
   (st-array-coll)
   (st-array)
   (st-boolean)
   (st-character)
   (st-string)
   (st-symbol)
   (st-list)
   (st-magnitude)
   (st-number)
   (st-complex)
   (st-float)
   (st-fraction)
   (st-integer)
   (st-blockClosure)
   (st-dictionary)
   (st-exception)
   (st-error)
   (st-error-subs)
   (st-arith-err-subs)
   (st-stream)
   (st-tokenizer)
   (st-parser)
   (st-xlate)
)

(import (simple-regression-testing))
(verbose-test-output? #f)
(break-on-test-error? #f)


;;;			--- E O F ---			;;;
