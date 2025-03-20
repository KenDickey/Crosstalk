;;; IMPLEMENTS: Unit tests for st-stream.sls
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017; March 2025

(import (simple-regression-testing)
        (st-stream))

(define (setup-st-stream)   #f)
(define (cleanup-st-stream) #f)

(add-test-suite 'st-stream
                setup-st-stream
                cleanup-st-stream)

(add-equal-test 'st-stream
   'CharStream
   (perform:
    (perform: (open-output-string) 'class)
    'name)
   "Output-string class")

;; (ensure-exception-raised 'st-stream
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

