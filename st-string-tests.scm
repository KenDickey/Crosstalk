;;; FILE: "st-string-tests.scm"
;;; IMPLEMENTS: Unit tests for st-string.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-string)


(define (setup-st-string)   #false)
(define (cleanup-st-string) #false)

(add-test-suite 'st-string
                setup-st-string
                cleanup-st-string)

(add-equal-test 'st-string
  "this and that"
  (perform:with: "this " '|,| "and that")
  "'this ' , ' and that'")

(add-equal-test 'st-string
  "this and that"
  (perform:with: String
                 'streamContents:
                 (lambda (port)
                   (display "this " port)
                   (display "and " port)
                   (display "that" port)))
  "String streamContents: oneArgBlock")

(add-equal-test 'st-string
  "howDoYouDo?"
  (perform: " how do you do? " 'asCamalCase)
  "' how do you do? ' asCamalCase")

;; (ensure-exception-raised 'st-string
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;; NYI

;;;			--- E O F ---			;;;
