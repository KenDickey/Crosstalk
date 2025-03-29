#!r6rs
;; Import order important

(compile-imported-libraries #t)

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

;; R6RS Library Bodies are not executed unless invoked,
;; so invoke directly for dynamic state setup.

(init-st-xlate) ;; init chained library initializers
(init-st-arith-err-subs)


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
   "st-base"
   "st-class-structure"
   "st-metaclass"
   "st-behavior"
   "st-collection"	
   "st-sequence-coll"
   "st-array-coll"
   "st-array"
   "st-boolean"
   "st-character"
   "st-string"
   "st-symbol"
   "st-list"
   "st-magnitude"
   "st-number"
   "st-complex"
   "st-float"
   "st-fraction"
   "st-integer"
   "st-blockClosure"
   "st-dictionary"
   "st-exception"
   "st-error"
   "st-error-subs"
   "st-arith-err-subs"
   "st-stream"
   "st-tokenizer"
   "st-parser"
   "st-xlate"
) )


(define (compile-st-sources)
  (for-each
   (lambda (name) (compile-library (string-append name ".sls")))
   source-names))

(define (load-st-libraries)
  (for-each
   (lambda (name) (load-library (string-append name ".so")))
   source-names))


;;;			--- E O F ---			;;;
