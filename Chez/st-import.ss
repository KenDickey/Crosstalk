#!r6rs
;; Import order important

(compile-imported-libraries #t)

(import
   (for (st-base) expand run)
   (for (st-class-structure) expand run)
   (for (st-metaclass) expand run)
   (for (st-behavior) expand run)
   (for (st-collection) expand run)	
   (for (st-sequence-coll) expand run)
   (for (st-array-coll) expand run)
   (for (st-array) expand run)
   (for (st-boolean) expand run)
   (for (st-character) expand run)
   (for (st-string) expand run)
   (for (st-symbol) expand run)
   (for (st-list) expand run)
   (for (st-magnitude) expand run)
   (for (st-number) expand run)
   (for (st-complex) expand run)
   (for (st-float) expand run)
   (for (st-fraction) expand run)
   (for (st-integer) expand run)
   (for (st-blockClosure) expand run)
   (for (st-dictionary) expand run)
   (for (st-exception) expand run)
   (for (st-error) expand run)
   (for (st-error-subs) expand run)
   (for (st-arith-err-subs) expand run)
   (for (st-stream) expand run)
   (for (st-tokenizer) expand run)
   (for (st-parser) expand run)
   (for (st-xlate) expand run)
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
