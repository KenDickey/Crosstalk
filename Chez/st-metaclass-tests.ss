;;; FILE: "st-metaclass-tests.ss"
;;; IMPLEMENTS: Unit tests for st-metaclass.sls
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016; March 2025

(import
   (simple-regression-testing)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
)

(define TestClass #f)

(define (setup-st-metaclass)
  #t)

(define (cleanup-st-metaclass)
  (set! TestClass #f)
)

(add-test-suite 'st-metaclass setup-st-metaclass cleanup-st-metaclass)

;; (add-equal-test 'st-metaclass
;;   "Class"
;;   (perform: Class 'printString)
;;   "Class printString")

(add-equal-test 'st-metaclass
  'Class
  (perform: Class 'name)
  "Class name")

(add-equal-test 'st-metaclass
  (string->symbol "Class class")
  (perform: (perform: Class 'class) 'name)
  "Class class name")

(add-equal-test 'st-metaclass
  'Object
  (perform: (perform: Behavior 'superclass) 'name)
  "Behavior superclass name")

(add-equal-test 'st-metaclass
  'ClassDescription
  (perform: (perform: MetaClass 'superclass) 'name)
  "MetaClass superclass name")

(add-equal-test 'st-metaclass
  (string->symbol "ClassDescription class")
  (perform:
   (perform:
    (perform: MetaClass 'superclass)
    'class)
   'name)
  "MetaClass superclass class name")

(add-equal-test 'st-metaclass
  (string->symbol "ClassDescription class")
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'superclass)
   'name)
  "MetaClass class superclass name")

(add-equal-test 'st-metaclass
  (string->symbol "MetaClass class")
  (perform: (perform: MetaClass 'class) 'name)
  "MetaClass class name")

(add-equal-test 'st-metaclass
  'MetaClass
  (perform:
   (perform:
    (perform: MetaClass 'class)
    'class)
   'name)
  "MetaClass class class name")


;;;			--- E O F ---			;;;
