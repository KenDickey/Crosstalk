;;; IMPLEMENTS: Unit tests for st-*.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 29 June 2016

;; (require 'st-*)

(define (setup-st-*)   #f)
(define (cleanup-st-*) #f)

(add-test-suite 'st-*
                setup-st-*
                cleanup-st-*)

;; (add-equal-test 'st-*
;;   (vector 3)
;;   (perform:with: Array
;;                  'with: 3)
;;   "Array with: 3")

;; (ensure-exception-raised 'st-*
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

