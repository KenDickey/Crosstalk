;;; FILE: "st-boolean-tests.ss"
;;; IMPLEMENTS: Unit tests for st-boolean.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(import (simple-regression-testing)
        (st-base)
        (st-boolean))

(define (setup-st-boolean) #f)
(define (cleanup-st-boolean) #f)

(add-test-suite 'st-boolean setup-st-boolean cleanup-st-boolean)

(add-eq-test 'st-boolean
  st-false
  (perform: st-nil 'notNil)
  "nil notNil --> false")

(add-eq-test 'st-boolean
  st-true
  (perform: st-nil 'isNil)
  "nil isNil --> true")

(add-eq-test 'st-boolean
  3
  (perform:with: st-nil 'ifNil: (lambda () 3))
  "nil ifNil: [3]")

(add-eq-test 'st-boolean
  3
  (perform:with:with:
     st-nil
     'ifNil:ifNotNil:
     (lambda () 3)
     (lambda () 4))
  "nil ifNil: [3] ifNotNil: [4]")

(add-eq-test 'st-boolean
  4
  (perform:with:with:
     st-nil
     'ifNotNil:ifNil:
     (lambda () 3)
     (lambda () 4))
  "nil ifNotNil: [3] ifNil: [4]")

(add-eq-test 'st-boolean
  3
  (perform:with:with:
     st-true
     'ifTrue:ifFalse:
     (lambda () 3)
     (lambda () 4))
  "true ifTrue: [3] ifFalse: [4]")

(add-eq-test 'st-boolean
  4
  (perform:with:with:
     st-true
     'ifFalse:ifTrue:
     (lambda () 3)
     (lambda () 4))
  "true ifFalse: [3] ifTrue: [4]")

(add-eq-test 'st-boolean
  4
  (perform:with:with:
     st-false
     'ifTrue:ifFalse:
     (lambda () 3)
     (lambda () 4))
  "false ifTrue: [3] ifFalse: [4]")

(add-eq-test 'st-boolean
  3
  (perform:with:with:
     st-false
     'ifFalse:ifTrue:
     (lambda () 3)
     (lambda () 4))
  "false ifFalse: [3] ifTrue: [4]")


;; (ensure-exception-raised 'st-boolean
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;
