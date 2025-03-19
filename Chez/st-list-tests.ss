;;; IMPLEMENTS: Unit tests for st-list.ss
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017; March 2025

(import (st-list))

(define (setup-st-list)   #f)
(define (cleanup-st-list) #f)

(add-test-suite 'st-list
                setup-st-list
                cleanup-st-list)

(add-equal-test 'st-list
   3
   (st-eval "(List with: 1 with: 2 with: 3) third")
   "(1 2 3) third")

(add-equal-test 'st-list
   '(1 4 9)
   (st-eval "(List with: 1 with: 2 with: 3) collect: [:e | e * e]")
   "(1 2 3) collect: [:e | e * e]")

(add-equal-test 'st-list
   '(2 3)
   (st-eval "(List with: 1 with: 2 with: 3) select: [:e | e > 1]")
   "(1 2 3) select: [:e | e > 1]")

(add-equal-test 'st-list
   "List( 1 2 )"
   (st-eval "(List with: 1 with: 2) printString")
   "(1 2) printString")

;; (ensure-exception-raised 'st-list
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

