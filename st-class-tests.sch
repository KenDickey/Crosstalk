;;; FILE: "st-class-tests.sch"
;;; IMPLEMENTS: Unit tests for st-class.sch
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-class)

(define TestClass #f)

(define (setup-st-class)
  #true)

(define (cleanup-st-class)
  (set! TestClass #f)
)

(add-test-suite 'st-class setup-st-class cleanup-st-class)

;; (add-equal-test 'st-class
;;   "Class"
;;   (perform: Class 'printString)
;;   "Class printString")

(add-equal-test 'st-class
  'Class
  (perform: Class 'name)
  "Class name")

(add-equal-test 'st-class
  '|Class class|
  (perform: (perform: Class 'class) 'name)
  "Class class name")

(add-equal-test 'st-class
  'Object
  (perform: (perform: Behavior 'superclass) 'name)
  "Behavior superclass name")

(add-equal-test 'st-class
  'ClassDescription
  (perform: (perform: MetaClass 'superclass) 'name)
  "MetaClass superclass name")

(add-equal-test 'st-class
  '|ClassDescription class|
  (perform:
   (perform:
    (perform: MetaClass 'superclass)
    'class)
   'name)
  "MetaClass superclass class name")

(add-equal-test 'st-class
  '|ClassDescription class|
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'superclass)
   'name)
  "MetaClass class superclass name")

(add-equal-test 'st-class
  '|MetaClass class|
  (perform: (perform: MetaClass 'class) 'name)
  "MetaClass class name")

(add-equal-test 'st-class
  'MetaClass
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'class)
   'name)
  "MetaClass class class name")


;;;			--- E O F ---			;;;
