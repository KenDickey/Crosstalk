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

;; (describe obj)
;; (display-obj obj)
;; (display-selectors obj)
;; (display-ivars class)
;; (inst-method-names class)
;; (display-subclasses class)
;; (hashtable-keys mdict)

(define source-names
  (list
   "st-base.sls"
   "st-class-structure.sls"
   "st-metaclass.sls"
   "st-behavior.sls"
   "st-collection.sls"	
   "st-sequence-coll.sls"
   "st-array-coll.sls"
   "st-array.sls"
   "st-boolean.sls"
   "st-character.sls"
   "st-string.sls"
   "st-symbol.sls"
   "st-list.sls"
   "st-magnitude.sls"
   "st-number.sls"
   "st-complex.sls"
   "st-float.sls"
   "st-fraction.sls"
   "st-integer.sls"
   "st-blockClosure.sls"
   "st-dictionary.sls"
   "st-exception.sls"
   "st-error.sls"
   "st-error-subs.sls"
   "st-arith-err-subs.sls"
   "st-stream.sls"
   "st-tokenizer.sls"
   "st-parser.sls"
   "st-xlate.sls"
) )

;; (for-each load source-names)


;;;			--- E O F ---			;;;
