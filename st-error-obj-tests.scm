;;; IMPLEMENTS: Unit tests for st-error-obj.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-error-obj)

(define (setup-st-error-obj)   #f)
(define (cleanup-st-error-obj) #f)

(add-test-suite 'st-error-obj
                setup-st-error-obj
                cleanup-st-error-obj)

(add-equal-test 'st-error-obj
  1
  (st-eval "[1] ensure: [0]")
  "[1] ensure: [0]")


;; (ensure-exception-raised 'st-error-obj
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

