;;; FILE: "st-blockClosure-tests.sch"
;;; IMPLEMENTS: Unit tests for st-blockClosure.sch
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
  (perform:with:
     (lambda () 3)
     'ensure:
     (lambda () 4))
  "[4] ensure: [3]")

;; (add-eq-test 'st-blockClosure
;;   4
;;   (perform:with:
;;      (lambda () (error 'bogus))
;;      'ensure:
;;      (lambda () 4))
;;   "[<error>] ensure: [3]")


;; (ensure-exception-raised 'st-blockClosure
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;
