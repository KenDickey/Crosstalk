
(compile-imported-libraries #t)
(print-length 30)
(print-depth 6)

(define st-files
  (list ;; order matters
   "st-core-classes.ss"
   "st-core-methods.ss"
   "st-boolean.ss"
   "st-collection.ss"
   "st-array.ss"
   "st-list.ss"
   "st-blockClosure.ss"
   "st-character.ss"
   "st-string.ss"
   "st-symbol.ss"
   
) )

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
;; (display-allSupers class)
;; (selectors class)
;; (hashtable-keys mdict)
;; (name obj) ; (printString obj)
;; (map name (allInstVarNames class))
;; (map name (allSuperclasses class))
;; (map name (allSubclasses class))

(define source-names
  (list
   "st-core-mechanics"
   "st-core-classes"
   "st-core-methods"
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
