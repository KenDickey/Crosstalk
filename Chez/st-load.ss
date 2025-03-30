#!r6rs
;; Import order important

(import (rnrs))

(compile-imported-libraries #t)

;; R6RS Library Bodies are not executed unless invoked,
;;   so invoke directly
;;   in proper order
;;   for dynamic state setup.

;; EVAL THESE IN ORDER..

  (import (st-base))
  (init-st-base)
  (import (st-class-structure))
  (init-st-class-structure)
  (import (st-metaclass))
  (init-st-metaclass)
  (import (st-behavior))
  (init-st-behavior)
  (import (st-collection))
  (init-st-collection)
  (import (st-sequence-coll))
  (init-st-sequence-coll)
  (import (st-array-coll))
  (init-st-array-coll)
  (import (st-array))
  (init-st-array)
  (import (st-boolean))
  (init-st-boolean)
  (import (st-character))
  (init-st-character)
  (import (st-string))
  (init-st-string)
  (import (st-symbol))
  (init-st-symbol)
  (import (st-list))
  (init-st-list)
  (import (st-magnitude))
  (init-st-magnitude)
  (import (st-number))
  (init-st-number)
  (import (st-complex))
  (init-st-complex)
  (import (st-float))
  (init-st-float)
  (import (st-fraction))
  (init-st-fraction)
  (import (st-integer))
  (init-st-integer)
  (import (st-blockClosure))
  (init-st-blockClosure)
  (import (st-dictionary))
  (init-st-dictionary)
  (import (st-exception))
  (init-st-exception)
  (import (st-error))
  (init-st-error)
  (import (st-error-subs))
  (init-st-error-subs)
  (import (st-arith-err-subs))
  (init-st-arith-err-subs)
  (import (st-stream))
  (init-st-stream)
  (import (st-tokenizer))
  (init-st-tokenizer)
  (import (st-parser))
  (init-st-parser)
  (import (st-xlate))
  (init-st-xlate)


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
