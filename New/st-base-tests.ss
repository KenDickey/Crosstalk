#!r6rs
;;; FILE: "st-base-tests.ss"
;;; IMPLEMENTS: Unit tests for st-base.sls
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016; March 2025

;; (verbose-test-output? #t)
;; (break-on-test-error? #f) ; keep going
;; (run-all-tests)

(define test-behavior           #f)
(define indexed+named-slots-obj #f)
(define aByteVector             #f)
(define anArray                 #f)

(define (setup-st-base)
  (let* ( (slot-names        '(foo bar baz))
          (num-named-slots   (length slot-names))
          (num-indexed-slots 4)
          (array-start-index (+ num-header-slots num-named-slots))
        )
  (set! test-behavior (make-mDict-placeholder 'Test))
  (primAddSelector:withMethod:  ;; Testing context
 	test-behavior
        'isKindOf:
        (lambda ignored #f))
  (primAddSelector:withMethod:  ;; Testing context
 	test-behavior
        'superclass
        (lambda ignored '()))
  (add-getters&setters test-behavior num-header-slots '(foo bar baz))
  (add-array-accessors test-behavior array-start-index) ;; (5)
  (set! indexed+named-slots-obj
        (make-st-object test-behavior
                        (+ num-named-slots num-indexed-slots)))
  ;; setters return self -- set up state here
  (perform:with:with: indexed+named-slots-obj 'at:put: 1 11)
  (perform:with:with: indexed+named-slots-obj 'at:put: 2 22)
  (perform:with:with: indexed+named-slots-obj 'at:put: 4 44)
  (perform:with: indexed+named-slots-obj 'bar: "BarBar")
  (perform:with: indexed+named-slots-obj 'foo: 'mobyFoo)
  (perform:with: indexed+named-slots-obj 'baz: #\Z)

  (set! aByteVector (make-st-bytearray 4 5))
  (perform:with:with: aByteVector 'at:put: 1 11)
  (perform:with:with: aByteVector 'at:put: 3 33)
  (set! anArray (vector 1 2 3 4 5))
  (perform:with:with: anArray 'at:put: 5 "five")
) )


(define (cleanup-st-base)
  (set! test-behavior #f)
  (set! indexed+named-slots-obj #f)
  (set! aByteVector #f)
  (set! anArray #f)
)


(add-test-suite 'st-base setup-st-base cleanup-st-base)

(add-equal-test 'st-base 
  "true"
  (perform: #t 'printString)
  "true")

(add-equal-test 'st-base 
  "nil"
  (perform: '() 'printString)
  "nil")

(add-equal-test 'st-base 
  "$C"
  (perform: #\C 'printString)
  "$c")

(add-equal-test 'st-base 
  "#'Foo'"
  (perform: 'Foo 'printString)
  "#'Foo'")

(add-equal-test 'st-base 
  1
  (method-arity (insure-annotated (lambda (self b) (+ self b)) 'bogusMethod:))
  "1 arg method (+self)")

(add-equal-test 'st-base 
  2
  (method-arity (primLookup: st-array-behavior 'at:put:))
  "2 arg method (+self)")

(add-equal-test 'st-base 
  'at:put:
  (perform: (primLookup: st-array-behavior 'at:put:) 'selector)
  "at:put: method selector")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with:with: indexed+named-slots-obj 'at:put: 0 #f)
   "at:put: with index < 1")

(ensure-exception-raised 'st-base
   (make-error-string-predicate  "Index out of range")
   (perform:with:with: indexed+named-slots-obj 'at:put: 5 #f)
   "at:put: with index (5) > max (4)")

(add-equal-test 'st-base 
  22
  (perform:with: indexed+named-slots-obj 'at: 2)
  "obj at: 2")

(add-equal-test 'st-base 
  11
  (perform:with: indexed+named-slots-obj 'at: 1)
  "obj at: 1")

(add-equal-test 'st-base 
  '()
  (perform:with: indexed+named-slots-obj 'at: 3)
  "obj at: 3 --> default is nil")

(add-equal-test 'st-base 
  44
  (perform:with: indexed+named-slots-obj 'at: 4)
  "obj at: 4")

(add-equal-test 'st-base 
  'mobyFoo
  (perform: indexed+named-slots-obj 'foo)
  "obj foo -> 'mobyFoo")

(add-equal-test 'st-base 
  '"BarBar"
  (perform: indexed+named-slots-obj 'bar)
  "obj bar-> 'BarBar'")

(add-equal-test 'st-base 
  '#\Z
  (perform: indexed+named-slots-obj 'baz)
  "obj baz -> $Z")

;; (add-equal-test 'st-base
;;    "send-failed recursion: Object >> doesNotUnderstand:"
;;    (perform: indexed+named-slots-obj 'glerph)
;;    "obj glerph -> doesNotUnderstand")

(add-equal-test 'st-base 
  11
  (perform:with: aByteVector 'at: 1)
  "bVec at: 1 --> 11")

(add-equal-test 'st-base 
  5 ;; initial-value is unchanged
  (perform:with: aByteVector 'at: 2)
  "bVec at: 2 --> 5")

(add-equal-test 'st-base 
  33
  (perform:with: aByteVector 'at: 3)
  "bVec at: 3 --> 33")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with:with: aByteVector 'at:put: 0 0)
   "bVec at:put: with index (0) < min (1)")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with: aByteVector 'at: 0)
   "bVec at: with index (0) < min (1)")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with: aByteVector 'at: 5)
   "bVec at: with index (5) > max (4)")


(add-equal-test 'st-base 
  1
  (perform:with: anArray 'at: 1)
  "array at: 1 --> 1")

(add-equal-test 'st-base 
  "five" ;; initial-value is unchanged
  (perform:with: anArray 'at: 5)
  "array at: 5 --> 'five'")

(add-equal-test 'st-base 
  4
  (perform:with: anArray 'at: 4)
  "array at: 4 --> 4")

(add-equal-test 'st-base 
  5
  (perform: anArray 'size)
  "array size -> 5")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with:with: anArray 'at:put: 0 0)
   "array at:put: with index (0) < min (1)")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with: anArray 'at: 0)
   "array at: with index (0) < min (1)")

(ensure-exception-raised 'st-base
   (make-error-string-predicate "Index out of range")
   (perform:with: anArray 'at: 6)
   "array at: with index (6) > max (5)")


;;;			--- E O F ---			;;;
