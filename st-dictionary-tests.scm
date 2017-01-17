;;; IMPLEMENTS: Unit tests for st-dictionary.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-dictionary)

(define (setup-st-dictionary)   #f)
(define (cleanup-st-dictionary) #f)

(add-test-suite 'st-dictionary
                setup-st-dictionary
                cleanup-st-dictionary)

;; (add-equal-test 'st-dictionary
;;   (vector 3)
;;   (perform:with: Array
;;                  'with: 3)
;;   "Array with: 3")

;; (ensure-exception-raised 'st-dictionary
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

