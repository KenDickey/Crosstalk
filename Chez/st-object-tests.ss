;;; FILE: "st-object-tests.ss"
;;; IMPLEMENTS: Unit tests for st-object.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016; May 2025

(import
 (simple-regression-testing)
 (st-base)
 (st-class-structure)
 (st-metaclass)
 (st-behavior)
)

(define %%test-object #f)

(define (setup-st-object)
  (primAddSelector:withMethod:
 	st-object-behavior
        'with:with:with:with:with
        (lambda (self a1 a2 a3 a4 a5) (list a1 a2 a3 a4 a5)))

  (set! %%test-object (st-eval "Object new"))
;;        (make-st-object st-object-behavior 0))

  (smalltalkAt:Put: 'TestObject %%test-object)
)

(define (cleanup-st-object)
  (hashtable-delete! st-object-behavior 'with:with:with:with:with)
  (set! %%test-object #f)
)

(add-test-suite 'st-object setup-st-object cleanup-st-object)

(add-eq-test 'st-object
  'Object
  ($ ($ %%test-object 'class) 'name)
  "anObject class name -> #Object")

(add-equal-test 'st-object
   "an Object doesNotUnderstand: #glerph"
   (st-eval "TestObject glerph")
   "obj glerph -> doesNotUnderstand")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '== 37)
  "anObject == 37")

(add-eq-test 'st-object
  #f
  (perform:with: Object 'respondsTo: 'ugly)
  "anObject respondsTo: #ugly")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '~= 37)
  "anObject ~= 37")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '~~ %%test-object)
  "anObj ~~ anObj")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '== %%test-object)
  "anObj == anObj")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '= %%test-object)
  "anObj = anObj")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '~= %%test-object)
  "anObj ~= anObj")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '~~ #f)
  "anObj ~~ false")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '== #f)
  "anObj == false")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '= #f)
  "anObj = false")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '~= #f)
  "anObj ~= false")


;; (add-eq-test 'st-object
;;   #f
;;   (perform:with: "foo" '~~ "foo")
;;   "'foo' ~~ 'foo'")

;; (add-eq-test 'st-object
;;   #t
;;   (perform:with: 'foo '== 'foo)
;;   "#foo == #foo")

;; (add-eq-test 'st-object
;;   #t
;;   (perform:with: "foo" '= "foo")
;;   "'foo' = 'foo'")

;; (add-eq-test 'st-object
;;   #f
;;   (perform:with: "foo" '~= "foo")
;;   "'foo' ~= 'foo'")

(add-eq-test 'st-object
  #f
  (perform:with: %%test-object '== (vector '(object) '() 3))
  "anObject == anOtherObject")

(add-eq-test 'st-object
  #t
  (perform:with: %%test-object '== %%test-object)
  "anObject == anObject")

(add-equal-test 'st-object
  10 ;; (display-ivars Object)
  (perform: Object 'basicSize)
  "Object basicSize")

;; (add-equal-test 'st-object
;;   #t
;;   (perform: st-nil 'isNil)
;;   "nil isNil")

(add-equal-test 'st-object
  #f
  (perform: %%test-object 'isNil)
  "anObject isNil")

;; (add-equal-test 'st-object
;;   #f
;;   (perform: #f 'isNil)
;;   "false isNil")

;; (add-equal-test 'st-object
;;   #f
;;   (perform: st-nil 'notNil)
;;   "nil notNil")

;; (add-equal-test 'st-object
;;   #t
;;   (perform: st-false 'notNil)
;;   "false notNil")

(add-equal-test 'st-object
   "an Object doesNotUnderstand: #bogus:"
   (st-eval "TestObject bogus: 666")
   "obj bogus: 666 -> doesNotUnderstand")

(add-equal-test 'st-object
  '(#t #f () 1 #\c)
  (perform:withArguments: %%test-object 'with:with:with:with:with (vector #t #f '() 1 #\c))
  "anObject with: #(object) with: nil with: true with: false with: nil with: 1 with $c")
  

;;;			--- E O F ---			;;;
