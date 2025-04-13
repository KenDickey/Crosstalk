#!r6rs
;;; File: "st-core.ss"
;;; IMPLEMENTS: Basic Smalltalk Class Structure
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;;; Smalltalk objects are typically created and managed by
;;;   a Class.

;;; Because Smalltalk is a self-knowledgeable system, it requires
;;;   some grounding for consistent reflective behavior.

;;; Basically, each class has a superclass (except Object) and
;;;   each Class's class is a metaClass which is an
;;;   instance of MetaClass.

;;; An instance's shape (instance variables)
;;;   and behavior (instance methods)
;;;   is defined in its class
;;;      (anObj behavior) == (anObj class methodDict)

;;; A class's shape (class variables) and behavior (class methods)
;;;   is defined in the instance class's metaClass 

;;; Classes are instances of metaClasses
;;; metaClasses are instances of class MetaClass

;;; We tie these together here by making instances and then setting
;;;  up the proper references via instance variables and behaviors.

;; superclass           class               class class
;;  hierarchy
;; ----------------------------------------------------
;; nil                  UndefinedObject  UndefinedObject class
;;  Object              Object Class        MetaClass
;;    Behavior          Behavior Class           "
;;      ClassDescrption ClassDescription Class   "
;;         Class        Class Class              "
;;         MetaClass    MetaClass Class          "

;;; Behavior superclass -> Object
;;; Object   superclass -> nil  (ground case)
;;; Behavior class -> 'Behavior Class' (a metaClass)
;;; Behavior class class -> MetaClass
;;; MetaClass class -> 'MetaClass Class' (a metaClass)
;;; MetaClass class class -> MetaClass (wraps around)
;;; Behavior class superclass -> 'Object Class' (Object's metaClass)
;;; Behavior class superclass class -> Class
;;; Object class superclass -> Class

(import (st-base)) ; basic internal mechanics

;;; Enable reflective introspection
(smalltalkAt:put: 'Smalltalk Smalltalk)

;;; Bootstrap:
;;;  [1] Make "raw" Class objects with accessors
;;;  [2] Setup basic book-keeping: superclass, metaClass, ivar-names, ..
;;;  [3] Fill-in core methods with "copy-down" semantics.

;;; Each class knows its instance variable names (if any).

(define combined-classDescription-var-names
  '(superclass
    methodDict ;; shared between instances
    format ;; layout/structure
    instanceVariables
    organization))

(define metaClass-added-var-names
  '(subclasses name
    myMethodNames ;; used to avoid overwrite
    ;; NB: pools are depricated !!
    thisClass))

(define class-added-var-names
  '(subclasses name
    myMethodNames ;; used to avoid overwrite
    ;; NB: pools are depricated !!
    category comment)
)

(define combined-class-ivar-names
  (append combined-classDescription-var-names
          class-added-var-names))

(define combined-metaClass-ivar-names
  (append combined-classDescription-var-names
          metaClass-added-var-names))


;;; Setup core class/superclass relations

(define MetaClass
  (make-st-object st-metaclass-behavior
                  (length combined-metaClass-ivar-names)))
(define Class
  (make-st-object st-class-behavior
                  (length combined-class-ivar-names)))

(define ClassDescription
  (make-st-object @@st-metaclass-behavior
                  (length combined-classDescription-var-names)))

(define Behavior
  (make-st-object st-behavior-behavior
                  (length combined-class-ivar-names))))

(define Object
    (make-st-object st-object-behavior
                    (length combined-class-ivar-names)))



;;;			--- E O F ---			;;;


