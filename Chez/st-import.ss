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

(define (test-from core-nameSym)
  (load (string-append
         "st-"
         (symbol->string core-nameSym)
         "-tests.ss")))

(define test-names
  '(
    base
    metaclass
    object
    boolean
    number
    character
    string
    symbol
    array
    list
    set
    stream
    blockClosure
    error
    condition
    dictionary
    tokenizer
    parser
    xlate
)  )

;; (for-each test-from test-names)
;; (run-all-tests)


;;;			--- E O F ---			;;;
