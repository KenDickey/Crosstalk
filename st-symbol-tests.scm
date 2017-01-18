;;; IMPLEMENTS: Unit tests for st-symbol.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-symbol)

(define (setup-st-symbol)   #f)
(define (cleanup-st-symbol) #f)

(add-test-suite 'st-symbol
                setup-st-symbol
                cleanup-st-symbol)

(add-equal-test 'st-symbol
     6
     ($ '|123456| 'size)
     "size")

;; (ensure-exception-raised 'st-symbol
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

