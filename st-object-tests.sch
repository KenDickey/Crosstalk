;;; FILE: "st-object-tests.sch"
;;; IMPLEMENTS: Unit tests for st-object.sch
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-object)

(define %%test-object #f)

(define (setup-st-objet)
  (addSelector:withMethod:
 	st-object-behavior
        'with:with:with:with:with
        (lambda (self a1 a2 a3 a4 a5) (list a1 a2 a3 a4 a5)))

  (set! %%test-object (vector %%st-object-tag%% st-object-behavior))
)

(define (cleanup-st-object)
  (hashtable-delete! st-object-behavior 'with:with:with:with:with)
  (set! %%test-object #f)
)


(add-eq-test 'st-object
  'Object
  (perform: %%test-object 'class)
  "anObject Class -> 'Object")

(ensure-exception-raised 'st-object
   'expect-doesNotUNderstand
   (perform: %%test-object 'glerph)
   "obj glerph -> doesNotUnderstand")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '== 37)
  "anObject == 37")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '== (vector '(object) st-nil 3))
  "anObject == anOtherObject")

(add-eq-test 'st-object
  #r
  (perform:with: %%test-object '== %%test-object)
  "anObject == anObject")

(ensure-exception-raised 'st-object
   'expect-doesNotUNderstand
   (perform:with: %%test-object 'bogus: 666)
   "obj bogus: 666 -> doesNotUnderstand")

(add-equal-test 'st-object
  '(%%st-object-tag%% st-nil #t #f '() 1 #\c)
  (perform:withArguments: %%test-object 'with:with:with:with:with (vector %%st-object-tag%% st-nil #t #f '() 1 #\c))
  "anObject with: #(object) with: nil with: true with: false with: nil with: 1 with $c"
  

;;;			--- E O F ---			;;;
