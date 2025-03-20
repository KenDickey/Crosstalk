;;; IMPLEMENTS: Unit tests for st-symbol.sls
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017; March 2025

(import (simple-regression-testing)
        (st-symbol))

(define (setup-st-symbol)   #f)
(define (cleanup-st-symbol) #f)

(add-test-suite 'st-symbol
                setup-st-symbol
                cleanup-st-symbol)

(add-equal-test 'st-symbol
     6
     ($ (string->symbol "123456") 'size)
     "size")

(add-equal-test 'st-symbol
  "#'aSymbol'"
  (perform: 'aSymbol 'printString)
  "#aSymbol printString")


;; (ensure-exception-raised 'st-symbol
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

