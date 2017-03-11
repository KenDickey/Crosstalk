;;; FILE: "st-blockClosure-tests.scm"
;;; IMPLEMENTS: Unit tests for st-blockClosure.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-blockClosure)

(define (setup-st-blockClosure)   #f)
(define (cleanup-st-blockClosure) #f)

(add-test-suite 'st-blockClosure
                setup-st-blockClosure
                cleanup-st-blockClosure)

(add-eq-test 'st-blockClosure
  'name
  (perform: (primLookup: (behavior Object) 'name)
            'selector)
  "Object's name method's name")

(add-equal-test 'st-blockClosure
  1
  (perform: (primLookup: (behavior Object) 'name)
            'argumentCount)
  "Object's name method's argument count/arity")

(add-equal-test 'st-blockClosure
  3
  (perform: (lambda () 3)
            'value)
  "[3] value")

(add-equal-test 'st-blockClosure
  3
  (perform:with:with:
     (lambda (a b) (+ a b))
     'value:value:
     1
     2)
  "[:a :b | a + b ] value: 1 value: 2")

(add-eq-test 'st-blockClosure
  -1
  (perform:
     (let ( (x 3) )
       (lambda ()
         (set! x (- x 1))
         (if (zero? x) -1 st-nil)))
     'whileNil)
  "whileNil")

(add-eq-test 'st-blockClosure
  3
  (let ( (x st-nil) )
    (perform:with:
     (lambda () (set! x 1) 2)
     'ensure:
     (lambda () (set! x 3)))
    x)
  "[x := 1. 2] ensure: [x := 3]. x")

(add-eq-test 'st-blockClosure
  4
  (let ( (x #false) )
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler
        (lambda (exn) (k x))
        (lambda ()
          (perform:with:
           (lambda () (error 'bogus))
           'ensure:
           (lambda () (set! x 4)))))))
    x)
  "[<error>] ensure: [4]")
  

(add-equal-test 'st-blockClosure
  "UnhandledError: WoW!"
  (st-eval
   "[37 squared. UnhandledError new signal: 'WoW!']
	on: UnhandledError
	do: [:ex | ex description ]")
  "[...] on: UnhandledError do: [..].")

(add-equal-test 'st-blockClosure
  "UnhandledError: WoW!"
  (st-eval
   "[37 squared. UnhandledError new signal: 'WoW!']
	on: UnhandledError
	do: [:ex | ex description ]")
  "[...] on: UnhandledError do: [..].")

(add-equal-test 'st-blockClosure
  3
  (st-eval
" | foo bar baz |
   foo := 1.
   bar := [ 
      [foo := 2. ^ 37]
      ifCurtailed: [ foo := 3].
      foo := 4
   ].
   baz := bar value.
   foo.")
  "[...] ifCurtailed: [..].")

(add-equal-test 'st-blockClosure
    "Error: error 2"
    (st-eval "| result |
  result := [ [ Error signal: 'error 1']
	      on: Error
	      do: [ :ex1 | Error signal: 'error 2']
	    ]
	    on: Exception
	    do: [ :ex2 | ex2 description].
result.")
    "Re-thrown exception")

;; (ensure-exception-raised 'st-blockClosure
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;
