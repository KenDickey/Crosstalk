;;; FILE: "st-kernal-tests.sch"
;;; IMPLEMENTS: Unit tests for st-kernel
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-kernel)

;; global..  FIXME: need test environment
(define test-behavior #f)
(define indexed+named-slots-obj #f)
(define aByteVector #f)

(define (setup-st-kernel)
  (set! test-behavior (make-mDict-placeholder 'Test))
  (add-getters&setters test-behavior '(foo bar baz))
  (add-array-accessors test-behavior 5)
  (set! indexed+named-slots-obj
        (make-st-object test-behavior 7 4)) ;; 4 indexed
  (set! aByteVector (make-st-bytevector 4 5))
)


(define (cleanup-st-kernel)
  (set! test-behavior #f)
  (set! indexed+named-slots-obj #f)
  (set! aByteVector #f)
)


(add-test-suite 'st-kernel setup-st-kernel cleanup-st-kernel)

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with:with: indexed+named-slots-obj 'at:put: 1 11)
  "obj at: 1 put: 11")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with:with: indexed+named-slots-obj 'at:put: 2 22)
  "obj at: 2 put: 22")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with:with: indexed+named-slots-obj 'at:put: 4 44)
  "obj at: 4 put: 44")

(ensure-exception-raised 'st-kernel
   (lambda (expect-index-out-of-range) #f)
   (perform:with:with: indexed+named-slots-obj 'at:put: 0 #f)
   "at:put: with index < 1")

(ensure-exception-raised 'st-kernel
   (lambda (expect-index-out-of-range) #f)
   (perform:with:with: indexed+named-slots-obj 'at:put: 5 #f)
   "at:put: with index (5) > max (4)")

(add-eq-test 'st-kernel 
  22
  (perform:with: indexed+named-slots-obj 'at: 2)
  "obj at: 2")

(add-eq-test 'st-kernel 
  11
  (perform:with: indexed+named-slots-obj 'at: 1)
  "obj at: 1")

(add-eq-test 'st-kernel 
  st-nil
  (perform:with: indexed+named-slots-obj 'at: 3)
  "obj at: 3 --> default is nil")

(add-eq-test 'st-kernel 
  44
  (perform:with: indexed+named-slots-obj 'at: 4)
  "obj at: 4")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with: indexed+named-slots-obj 'bar: "BarBar")
  "obj bar: 'BarBar'")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with: indexed+named-slots-obj 'bar: "BarBar")
  "obj bar: 'BarBar'")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with: indexed+named-slots-obj 'foo: 'mobyFoo)
  "obj foo: #theFoo")

(add-eq-test 'st-kernel 
  indexed+named-slots-obj
  (perform:with: indexed+named-slots-obj 'baz: #\Z)
  "obj baz: $Z")

(add-eq-test 'st-kernel 
  '#\Z
  (perform: indexed+named-slots-obj 'foo)
  "obj foo -> 'mobyFoo")

(add-eq-test 'st-kernel 
  '"BarBar"
  (perform: indexed+named-slots-obj 'baz)
  "obj bar-> 'BarBar'")

(add-eq-test 'st-kernel 
  '#\Z
  (perform: indexed+named-slots-obj 'baz)
  "obj baz -> $Z")

(ensure-exception-raised 'st-kernel
   (lambda (expect-doesNotUNderstand) #f)
   (perform: indexed+named-slots-obj 'glerph)
   "obj glerph -> doesNotUnderstand")


(add-eq-test 'st-kernel 
  aByteVector
  (perform:with:with: aByteVector 'at:put: 1 11)
  "bVec at: 1 put: 11")

(add-eq-test 'st-kernel 
  11
  (perform:with: aByteVector 'at: 1)
  "bVec at: 1 --> 11")

(add-eq-test 'st-kernel 
  aByteVector
  (perform:with:with: aByteVector 'at:put: 3 33)
  "bVec at: 3 put: 33")

(add-eq-test 'st-kernel 
  #f
  (perform:with: aByteVector 'at: 2)
  "bVec at: 2 --> nil")

(add-eq-test 'st-kernel 
  33
  (perform:with: aByteVector 'at: 3)
  "bVec at: 3 --> 33")

(ensure-exception-raised 'st-kernel
   (lambda (expect-index-out-of-range) #f)
   (perform:with:with: aByteVector 'at:put: 5 #f)
   "at:put: with index (0) < min (1)")

(ensure-exception-raised 'st-kernel
   (lambda (expect-index-out-of-range) #f)
   (perform:with: aByteVector 'at: 5)
   "at:put: with index (5) > max (4)")



;;;			--- E O F ---			;;;
