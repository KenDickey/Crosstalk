;;; IMPLEMENTS: Unit tests for st-date-time.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-date-time)

(define (setup-st-date-time)   #f)
(define (cleanup-st-date-time) #f)

(add-test-suite 'st-date-time
                setup-st-date-time
                cleanup-st-date-time)

(add-equal-test 'st-date-time
  "3:04:05:06.000000789"
  ($ (make-time 'time-duration 789 273906) 'printString)
  "Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 789")

(add-equal-test 'st-date-time
  "3:04:05:06.000000789"
  (st-eval
"(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 789) printString"
  )
  "(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 789) printString")

(add-equal-test 'st-date-time
  "3:04:05:06"
  (st-eval
"(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 0) printString"
  )
  "(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 0) printString")


(add-equal-test 'st-date-time
  "3:04:05:06"
  (st-eval
"(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 0) printString"
  )
  "(Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 0) printString")

(add-equal-test 'st-date-time
  "3:04:05:06.700000890"
  (st-eval
"(Duration days: 3 hours: 4 minutes: 5 seconds: 6.7 nanoSeconds: 890) printString")
"(Duration days: 3 hours: 4 minutes: 5 seconds: 6.7 nanoSeconds: 890) printString")

;; (ensure-exception-raised 'st-date-time
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

