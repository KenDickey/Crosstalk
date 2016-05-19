;;; FILE: "st-kernal-tests.sch"
;;; IMPLEMENTS: Unit tests for st-kernel
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-kernel)

;; global..  FIXME: need test environment
(define test-behavior           #f)
(define indexed+named-slots-obj #f)
(define aByteVector             #f)

(define (setup-st-kernel)
  (set! test-behavior (make-mDict-placeholder 'Test))
  (add-getters&setters test-behavior '(foo bar baz))
  (add-array-accessors test-behavior 5)
  (set! indexed+named-slots-obj
        (make-st-object test-behavior 7 4)) ;; 4 indexed
  ;; setters return self -- set up state here
  (perform:with:with: indexed+named-slots-obj 'at:put: 1 11)
  (perform:with:with: indexed+named-slots-obj 'at:put: 2 22)
  (perform:with:with: indexed+named-slots-obj 'at:put: 4 44)
  (perform:with: indexed+named-slots-obj 'bar: "BarBar")
  (perform:with: indexed+named-slots-obj 'foo: 'mobyFoo)
  (perform:with: indexed+named-slots-obj 'baz: #\Z)

  (set! aByteVector (make-st-bytevector 4 5))
  (perform:with:with: aByteVector 'at:put: 1 11)
  (perform:with:with: aByteVector 'at:put: 3 33)  
)


(define (cleanup-st-kernel)
  (set! test-behavior #f)
  (set! indexed+named-slots-obj #f)
  (set! aByteVector #f)
)


(add-test-suite 'st-kernel setup-st-kernel cleanup-st-kernel)

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate "Index out of range")
   (perform:with:with: indexed+named-slots-obj 'at:put: 0 #f)
   "at:put: with index < 1")

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate  "Index out of range")
   (perform:with:with: indexed+named-slots-obj 'at:put: 5 #f)
   "at:put: with index (5) > max (4)")

(add-equal-test 'st-kernel 
  22
  (perform:with: indexed+named-slots-obj 'at: 2)
  "obj at: 2")

(add-equal-test 'st-kernel 
  11
  (perform:with: indexed+named-slots-obj 'at: 1)
  "obj at: 1")

(add-equal-test 'st-kernel 
  st-nil
  (perform:with: indexed+named-slots-obj 'at: 3)
  "obj at: 3 --> default is nil")

(add-equal-test 'st-kernel 
  44
  (perform:with: indexed+named-slots-obj 'at: 4)
  "obj at: 4")

(add-equal-test 'st-kernel 
  'mobyFoo
  (perform: indexed+named-slots-obj 'foo)
  "obj foo -> 'mobyFoo")

(add-equal-test 'st-kernel 
  '"BarBar"
  (perform: indexed+named-slots-obj 'bar)
  "obj bar-> 'BarBar'")

(add-equal-test 'st-kernel 
  '#\Z
  (perform: indexed+named-slots-obj 'baz)
  "obj baz -> $Z")

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate  "Failed message send: #glerph to ")
   (perform: indexed+named-slots-obj 'glerph)
   "obj glerph -> doesNotUnderstand")

(add-equal-test 'st-kernel 
  11
  (perform:with: aByteVector 'at: 1)
  "bVec at: 1 --> 11")

(add-equal-test 'st-kernel 
  5 ;; initial-value is unchanges
  (perform:with: aByteVector 'at: 2)
  "bVec at: 2 --> 5")

(add-equal-test 'st-kernel 
  33
  (perform:with: aByteVector 'at: 3)
  "bVec at: 3 --> 33")

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate "Index out of range")
   (perform:with:with: aByteVector 'at:put: 0 0)
   "at:put: with index (0) < min (1)")

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate "Index out of range")
   (perform:with: aByteVector 'at: 0)
   "at: with index (0) < min (1)")

(ensure-exception-raised 'st-kernel
   (make-error-string-predicate "Index out of range")
   (perform: aByteVector 'at: 5)
   "at: with index (5) > max (4)")



;;;			--- E O F ---			;;;
