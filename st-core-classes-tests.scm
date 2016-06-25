;;; FILE: "st-core-classes-tests.scm"
;;; IMPLEMENTS: Unit tests for st-core-classes.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'st-core-classes)

(define TestClass #f)

(define (setup-st-core-classes)
  #true)

(define (cleanup-st-core-classes)
  (set! TestClass #f)
)

(add-test-suite 'st-core-classes setup-st-core-classes cleanup-st-core-classes)

;; (add-equal-test 'st-core-classes
;;   "Class"
;;   (perform: Class 'printString)
;;   "Class printString")

(add-equal-test 'st-core-classes
  'Class
  (perform: Class 'name)
  "Class name")

(add-equal-test 'st-core-classes
  '|Class class|
  (perform: (perform: Class 'class) 'name)
  "Class class name")

(add-equal-test 'st-core-classes
  'Object
  (perform: (perform: Behavior 'superclass) 'name)
  "Behavior superclass name")

(add-equal-test 'st-core-classes
  'ClassDescription
  (perform: (perform: MetaClass 'superclass) 'name)
  "MetaClass superclass name")

(add-equal-test 'st-core-classes
  '|ClassDescription class|
  (perform:
   (perform:
    (perform: MetaClass 'superclass)
    'class)
   'name)
  "MetaClass superclass class name")

(add-equal-test 'st-core-classes
  '|ClassDescription class|
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'superclass)
   'name)
  "MetaClass class superclass name")

(add-equal-test 'st-core-classes
  '|MetaClass class|
  (perform: (perform: MetaClass 'class) 'name)
  "MetaClass class name")

(add-equal-test 'st-core-classes
  'MetaClass
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'class)
   'name)
  "MetaClass class class name")


;;;			--- E O F ---			;;;
