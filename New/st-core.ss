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

;; Note: for this toy Smalltalk, we are not implementing ProtoObject
;; or Pools.

(import (st-base)) ; basic internal mechanics

;;; Enable reflective introspection
(smalltalkAt:put: 'Smalltalk Smalltalk)

;;; Bootstrap:
;;;  [1] Make "raw" Class objects with accessors
;;;  [2] Setup basic book-keeping: superclass, metaClass, ivar-names, ..
;;;  [3] Fill-in core methods with "copy-down" semantics.

;;; Each class knows its instance variable names (if any).

;;; For inheritance lookup, methods are transitively "copied down"
;;; to subclass behaviors unless overridden.

;;; This implies each Class|MetaClass remembers override selectors
;;; known via ivar #myMethodNames.

;;; The result is that each object's behavior contains either
;;; a locally defined method, an inherited method with the same
;;; selector, or requires a synthesized DNU (Does Not Understand)
;;; and lookup only requires a single hashtable reference.
;;; 

(define behavior-ivar-names
  '(superclass
    methodDict	;; shared by all instances
    format))	;; layout/structure

(define classDescription-ivar-names
  (append behavior-ivar-names
          '(instanceVariables
            organization)))

(define metaClass-ivar-names
  (append classDescription-ivar-names
          '(subclasses
            name
            myMethodNames ;; used to avoid overwrite
            ;; NB: pools are depricated !!
            thisClass)))

(define class-ivar-names
  (append classDescription-ivar-names
          '(subclasses
            name
            myMethodNames ;; used to avoid overwrite
            ;; NB: pools are depricated !!
            category
            comment)))

;;; Setup core class/superclass relations

(define Object ;; an object'a Class
  (make-st-object st-object-behavior 0))

(define Behavior
  (make-st-object st-behavior-behavior
                  (length behavior-ivar-names)))
(add-getters&setters st-behavior-behavior
                     num-header-slots
                     behavior-ivar-names)

(define ClassDescription
  (make-st-object st-classDescription-behavior
                  (length classDescription-ivar-names)))
(add-getters&setters st-classDescription-behavior
                     num-header-slots
                     classDescription-ivar-names)
(perform:with: ClassDescription
               'instanceVariables: classDescription-ivar-names)

(define Class
  (make-st-object st-class-behavior
                  (length class-ivar-names)))
(add-getters&setters st-class-behavior
                     num-header-slots
                     class-ivar-names)
(perform:with: Class
               'instanceVariables: class-ivar-names)

(define MetaClass
  (make-st-object st-metaClass-behavior
                  (length metaClass-ivar-names)))
(add-getters&setters st-metaClass-behavior
                     num-header-slots
                     metaClass-ivar-names)
(perform:with: MetaClass
               'instanceVariables: metaClass-ivar-names)

;;; MetaClasses for the above
;;; Class Class, MetaClass Class, ..
(define (make-meta name for-class superclass ivar-names mDict)
  (let ( (aMetaClass (make-st-object mDict (length metaClass-ivar-names))) )
    (add-getters&setters mDict
                     num-header-slots
                     metaClass-ivar-names)
    ($: aMetaClass 'name: name)
    ($: aMetaClass 'class: MetaClass)
    ($: aMetaClass 'thisClass: for-class)
    (unless (st-nil? superclass)
      ($: aMetaClass 'superclass: superclass)
      (addSubClass: superclass aMetaClass))
    ($: aMetaClass 'instanceVariables: ivar-names)
    ($: aMetaClass 'methodDict: mDict)
    aMetaClass)
  )

    
(define ObjectClass
  (make-meta (string->symbol "Object class")
             Object
             Class
             st-nil
             (make-method-dictionary))
  )

(define BehaviorClass
  (make-meta (string->symbol "Behavior class")
             Behavior
             ObjectClass
             behavior-ivar-names
             (make-method-dictionary))
  )

(define ClassDescrptionClass
  (make-meta (string->symbol "ClassDescrption class")
             ClassDescrption
             BehaviorClass
             classDescription-ivar-names
             (make-method-dictionary))
)
  

(define ClassClass
  (make-meta (string->symbol "Class class")
             Class
             ClassDescription
             class-ivar-names
             (make-method-dictionary))
  )

(define MetaClassClass
  (make-meta (string->symbol "MetaClass class")
             MetaClass
             ClassDescription
             metaClass-ivar-names
             (make-method-dictionary))
  )

;; make accessable to Smalltalk
(smalltalkAt:Put: 'Object Object)
(smalltalkAt:Put: 'Behavior Behavior)
(smalltalkAt:Put: 'ClassDescription ClassDescription)
(smalltalkAt:Put: 'Class Class)
(smalltalkAt:Put: 'MetaClass MetaClass)


;;;			--- E O F ---			;;;


