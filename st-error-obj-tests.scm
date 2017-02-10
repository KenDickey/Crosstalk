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

;; See also tests in "st-blockClosure-tests.scm"

(add-equal-test 'st-error-obj
  "Error: error 2"
  (st-eval "| result |
result :=
	[ [ Error signal: 'error 1']
	  on: Exception
	  do: [ :ex1 | Error signal: 'error 2']
	]
	on: Exception
	do: [ :ex2 | ex2 description].
 result.")
  "Nested exceptions A")

(add-equal-test 'st-error-obj
  "Error: error 2"
  (st-eval
"| result |
  result :=
	[ [ Error signal: 'error 1']
	  on: Exception
	  do: [ :ex1 | Error signal: 'error 2']
	]
	on: Exception
	do: [ :ex2 | ex2 description ].
   result.")
  "Nested exceptions B")

(add-equal-test 'st-error-obj
  "Error: error 2"
  (st-eval
"| result |
  result :=
	[ [ Error signal: 'error 1']
	  on: Exception
	  do: [ Error signal: 'error 2'] \"Zero ARGS\"
	]
	on: Exception
	do: [ :ex2 | ex2 description ].
   result.")
  "Nested exceptions C")


;; (ensure-exception-raised 'st-error-obj
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

