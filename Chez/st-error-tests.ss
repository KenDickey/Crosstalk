;;; IMPLEMENTS: Unit tests for st-error-tests.ss
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017; March 2025

(import (st-error)
        (st-error-subs)
        (st-exception))

(define (setup-st-error-obj)   #f)
(define (cleanup-st-error-obj) #f)

(add-test-suite 'st-error-obj
                setup-st-error-obj
                cleanup-st-error-obj)

(add-equal-test 'st-error-obj
  1
  (st-eval "[1] ensure: [0]")
  "[1] ensure: [0]")

(add-equal-test 'st-error-obj
  0
  (st-eval "| a | [a := 1] ensure: [a := 0]. a")
  "[..] ensure: [a := 0]")

;; See also tests in "st-blockClosure-tests.scm"

(add-equal-test 'st-error-obj
  "Warning: error 2"
  (st-eval "| result |
result :=
	[ [ Warning signal: 'error 1']
	  on: Warning
	  do: [ :ex1 | Warning signal: 'error 2']
	]
	on: Warning
	do: [ :ex2 | ex2 description].
 result.")
  "Nested exceptions A")

(add-equal-test 'st-error-obj
  "Warning: error 2"
  (st-eval
"| result |
  result :=
	[ [ Warning signal: 'error 1']
	  on: Warning
	  do: [ :ex1 | Warning signal: 'error 2']
	]
	on: Warning
	do: [ :ex2 | ex2 description ].
   result.")
  "Nested exceptions B")

(add-equal-test 'st-error-obj
  "Warning: error 2"
  (st-eval
"| result |
  result :=
	[ [ Warning signal: 'error 1']
	  on: Warning
	  do: [ Warning signal: 'error 2'] \"Zero ARGS\"
	]
	on: Warning
	do: [ :ex2 | ex2 description ].
   result.")
  "Nested exceptions C")

(add-equal-test 'st-error-obj
  (vector 3 4)
  (st-eval "| result a |
   result :=
	( [ a := 0. Warning signal: 'error 1'. a := 2]
	  on: Warning
	  do: [ :ex | a := 3. ex return: 4. a := 5 ] ).
   { a. result. }")
  "ex return: 4"
)

(add-equal-test 'st-error-obj
  (vector 2 2)
  (st-eval "| result a |
   result :=
	( [ a := 0. Warning signal: 'error 1'. a := 2]
	  on: Warning
	  do: [ :ex | a := 3. ex resume: 4. a := 5 ] ).
   { a. result. }")
  "ex resume: 4"
)

(add-equal-test 'st-error-obj
  (vector 3 5)
  (st-eval "| result a |
   result :=
	( [ a := 0. Warning signal: 'error 1' ]
	  on: Warning
	  do: [ :ex | a := 3. ex resume: 5. a := 6 ] ).
   { a. result. }")
  "ex resume: 5"
)

(add-equal-test 'st-error-obj
  (vector 3 5)
  (st-eval "| result a |
   result :=
	[ [ a := 0. Warning signal: 'error 1' ]
	  on: Warning
	  do: [ :ex | a := 3. ex pass. a := 6 ] ]
        on: Warning do: [ :ex | ex resume: 5 ].
   { a. result. }")
  "ex pass"
)

(add-equal-test 'st-error-obj
  (vector 6 7)
  (st-eval "| result a |
   result :=
	[ [ a := 0. Warning signal: 'error 1' ]
	  on: Warning
	  do: [ :ex | a := 3. ex outer. a := 6. a + 1 ] ]
        on: Warning do: [ :ex | ex resume: 5 ].
   { a. result. }")
  "ex outer"
)


(add-equal-test 'st-error-obj
  'ok
   (st-eval
"[nil zork]
  on: MessageNotUnderstood
  do: [:ex|
	(ex message selector == #zork)
	   ifTrue: [ex resume: #ok].
	ex pass
  ]")
   "handle MessageNotUnderstood")

;; (ensure-exception-raised 'st-error-obj
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

