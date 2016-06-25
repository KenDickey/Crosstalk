;;; FILE: "st-object-tests.scm"
;;; IMPLEMENTS: Unit tests for st-object.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-object)

(define %%test-object #f)

(define (setup-st-object)
  (primAddSelector:withMethod:
 	st-object-behavior
        'with:with:with:with:with
        (lambda (self a1 a2 a3 a4 a5) (list a1 a2 a3 a4 a5)))

  (set! %%test-object
        (make-st-object st-object-behavior 0))
)

(define (cleanup-st-object)
  (hashtable-delete! st-object-behavior 'with:with:with:with:with)
  (set! %%test-object #f)
)

(add-test-suite 'st-object setup-st-object cleanup-st-object)

(add-eq-test 'st-object
  'Object
  (perform: %%test-object 'class)
  "anObject Class -> 'Object")

(ensure-exception-raised 'st-object
   (make-error-string-predicate   "Failed message send: #glerph to ")
   (perform: %%test-object 'glerph)
   "obj glerph -> doesNotUnderstand")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '== 37)
  "anObject == 37")

(add-eq-test 'st-object
  #false
  (perform:with: Object 'respondsTo: 'ugly)
  "anObject respondsTo: #ugly")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '~= 37)
  "anObject ~= 37")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '~~ %%test-object)
  "anObj ~~ anObj")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '== %%test-object)
  "anObj == anObj")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '= %%test-object)
  "anObj = anObj")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '~= %%test-object)
  "anObj ~= anObj")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '~~ #false)
  "anObj ~~ false")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '== #false)
  "anObj == false")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '= #false)
  "anObj = false")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '~= #false)
  "anObj ~= false")


;; (add-eq-test 'st-object
;;   #false
;;   (perform:with: "foo" '~~ "foo")
;;   "'foo' ~~ 'foo'")

;; (add-eq-test 'st-object
;;   #true
;;   (perform:with: 'foo '== 'foo)
;;   "#foo == #foo")

;; (add-eq-test 'st-object
;;   #true
;;   (perform:with: "foo" '= "foo")
;;   "'foo' = 'foo'")

;; (add-eq-test 'st-object
;;   #false
;;   (perform:with: "foo" '~= "foo")
;;   "'foo' ~= 'foo'")

(add-eq-test 'st-object
  #false
  (perform:with: %%test-object '== (vector '(object) '() 3))
  "anObject == anOtherObject")

(add-eq-test 'st-object
  #true
  (perform:with: %%test-object '== %%test-object)
  "anObject == anObject")

(add-equal-test 'st-object
  0
  (perform: Object 'basicSize)
  "Object basicSize")

;; (add-equal-test 'st-object
;;   #true
;;   (perform: st-nil 'isNil)
;;   "nil isNil")

(add-equal-test 'st-object
  #false
  (perform: %%test-object 'isNil)
  "anObject isNil")

;; (add-equal-test 'st-object
;;   #false
;;   (perform: #false 'isNil)
;;   "false isNil")

;; (add-equal-test 'st-object
;;   #false
;;   (perform: st-nil 'notNil)
;;   "nil notNil")

;; (add-equal-test 'st-object
;;   #true
;;   (perform: st-false 'notNil)
;;   "false notNil")

(ensure-exception-raised 'st-object
   (make-error-string-predicate  "Failed message send: #bogus: to ")
   (perform:with: %%test-object 'bogus: 666)
   "obj bogus: 666 -> doesNotUnderstand")

(add-equal-test 'st-object
  '(#t #f () 1 #\c)
  (perform:withArguments: %%test-object 'with:with:with:with:with (vector %%st-object-tag%% '() #t #f '() 1 #\c))
  "anObject with: #(object) with: nil with: true with: false with: nil with: 1 with $c")
  

;;;			--- E O F ---			;;;
