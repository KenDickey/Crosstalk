#!r6rs
;;; File: "st-core-classes.ss"
;;; IMPLEMENTS: Smalltalk Kernel Class Structure
;;;  Object, Behavior, ClassDescription, MetaClass, Class
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;;; Smalltalk objects are typically created and managed by
;;;   a Class.

;;; An instance's shape (instance variables)
;;;   and behavior (instance methods)
;;;   is defined in its class

;;; All instances of a Class SHARE the SAME method dictionary.
;;;   (anObj behavior) == (anObj class instanceBehavior)

;;; A class's shape (class variables) and behavior (class methods)
;;;   is defined in the instance class's metaClass

;;; Basically, each class has a superclass (except Object) and
;;;   each Class's class is a metaClass which is an
;;;   instance of MetaClass.

;;; Because Smalltalk is a self-knowledgeable system, it requires
;;;   some grounding for consistent reflective behavior.
;;; This is probably the most complex idea to understand about
;;;   reflective OO, the requirement that the system must
;;;   contain a model of itself.  The model is then used
;;;   to reason about and control the system and can be changed
;;;   by/from/within the system itself.
;;; The contribution here is an operational model of the Smaltalk
;;;   solution in Scheme code.  [Note "st-create-subclass.ss"].

;;; Inheritance is via the chain of superclasses (single inheritance)
;;; The superclass of the Object class is nil.

;;; The class of class Object is 'Object class' whose class is
;;; the singleton class MetaClass.

;;; So each Class is an instances of a singleton instance
;;;   of a metaClasses.

;;; All metaClasses are instances of class MetaClass

;;; Each instance of a class has, in addition to its own behavior,
;;; a class variable, #instanceBehavior, which is the single method 
;;; dictionary shared by every one of that Class's instances.

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
;;; Behavior class superclass -> 'Object class' (Object's metaClass)
;;; Behavior class superclass class -> Class
;;; Object class superclass -> Class

;; Note: for this toy Smalltalk, we are not implementing ProtoObject
;; nor Pools.

(import (st-core-mechanics))

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

;;; Setup core class/superclass relations
;;; Start with Classes, add metaClasses, stitch together.

(define (make-protoClass name super added-instance-ivars my-ivars)
  (let* ( (myBehavior (make-method-dictionary)) ; no metaclass yet
	  (protoClass
	   (make-st-object myBehavior (length my-ivars)))
	  (instanceBehavior
	   (if (st-nil? super)
	       (make-method-dictionary)
	       (clone-method-dictionary
		($ super 'instanceBehavior))))
	  (setter-getter-list
	   (add-getters&setters myBehavior
				num-header-slots
				my-ivars)) ;; for class or metaClass
	  (num-super-var-slots
	   (if (st-nil? super)
	       0
	       (length (allInstVarNames super))))
	  )
    (unless (st-nil? added-instance-ivars)
      (add-getters&setters instanceBehavior
			   (+ num-header-slots num-super-var-slots)
			   added-instance-ivars)
      ($: protoClass 'instanceVariableNames: added-instance-ivars))
    ($: protoClass 'instanceBehavior: instanceBehavior)
    ($: protoClass 'myMethodNames: setter-getter-list)

    ($: protoClass 'superclass: super)
    (unless (st-nil? super)
      (if (respondsTo: super 'subclasses)
	  (addSubclass: super protoClass)))
    ($: protoClass 'name: name)
    ;; Nota Bene -- no metaclass yet, so don't set #class
    ;; for the protoClass.  See make-meta, below
    (primSetClass: instanceBehavior protoClass) ;My instances know me
    
    protoClass)
  )


(define behavior-ivar-names
  '(class
    superclass
    instanceBehavior	;; shared by all instances
    format))	;; layout/structure

(define classDescription-ivar-names
  '(name
    instanceVariableNames
    organization
    myMethodNames ;; used to avoid overwrite
    ))

(define class-ivar-names
  '(subclasses
    ;; NB: pools are depricated !!
    category
    comment))

(define all-class-ivar-names
  (append  behavior-ivar-names
	   classDescription-ivar-names
	   class-ivar-names))

(define metaClass-ivar-names
  '(thisClass))  ;; NB: pools are depricated !!

(define all-metaClass-ivar-names
  (append  behavior-ivar-names
	   classDescription-ivar-names
	   metaClass-ivar-names))


(define Object ;; an Object instance's Class
  (make-protoClass 'Object
		   st-nil   ; nil superclass
		   st-nil   ; no local ivars
		   all-class-ivar-names)) ;; my slot-names

(define Behavior ;; a Behavior instance's Class
  (make-protoClass 'Behavior
		   Object
		   behavior-ivar-names
		   all-class-ivar-names))

(define ClassDescription
  (make-protoClass 'ClassDescription
		   Behavior
		   classDescription-ivar-names
		   all-class-ivar-names))

(define Class
  (make-protoClass 'Class
		   ClassDescription
		   class-ivar-names
		   all-class-ivar-names))

(define MetaClass
  (make-protoClass 'MetaClass
		   ClassDescription
		   metaClass-ivar-names
		   all-metaClass-ivar-names))

;;; MetaClasses for the above
;;; Object class, Class class, MetaClass class, ..
(define (make-meta name for-class superclass)
  (let* ( (classBehavior ($ MetaClass 'instanceBehavior))
	  (super-ivar-names (allInstVarNames superclass))
	  (newMeta-ivar-names
	   (append super-ivar-names metaClass-ivar-names))
	  (newMetaClass
	   (make-st-object classBehavior
			   (length newMeta-ivar-names)))
          )
    ;;; extend metaclass instance accessors for #thisClass slot
    ;; (add-getters&setters classBehavior
    ;; 			 (+ num-header-slots (length super-ivar-names))
    ;; 			 metaClass-ivar-names)
    ($: newMetaClass 'name: name)
    ($: newMetaClass 'thisClass: for-class)
    ($: newMetaClass 'superclass: superclass)
;;;               superclass
;;;  for-class  <- ^^meta^^ -> MetaClass
    ($: for-class    'class: newMetaClass)
    ($: newMetaClass 'class: MetaClass)
    (copyDownInstBehaviorFromSuper newMetaClass) ; instanceBehavior

    newMetaClass)
  )

(define (copyDownInstBehaviorFromSuper aMetaClassInst)
  ;; instanceBehavior needs copydown from superclass.
  ;; Here, the singleton instance #thisClass has a
  ;;   partially initialized behavior.
  ;; The metaClass #instanceBehavior needs to be set to this
  ;;   and superclass #instanceBehavior inherited methods added.
  (let* ( (my-superclass  ($ aMetaClassInst 'superclass))
	  (my-instance-behavior
	   (behavior ($ aMetaClassInst 'thisClass)))
	  (my-method-name-overrides
	   (vector->list
	    (hashtable-keys my-instance-behavior)))
       )
    (let-values ( ((sel-vec meth-vec)
		   (hashtable-entries
		    ($ my-superclass 'instanceBehavior)))
		)
      (vector-for-each
       (lambda (s m) ;; copydown if not overidden
	 (unless (memq s my-method-name-overrides)
	   (hashtable-set! my-instance-behavior s m)))
       sel-vec
       meth-vec)
      ($: aMetaClassInst 'instanceBehavior: my-instance-behavior)
;; (eq? (behavior <aClass>) ($ (class <aClass>) 'instanceBehavior) ==> #t
      aMetaClassInst ;; Convention is to return the #self object
) ) )
    
(define ObjectClass ;; `Object class`, a metaClass
  (make-meta (string->symbol "Object class")
             Object   ;; for-class
             Class))  ;; superclass
		      ;; class -> MetaClass (known)

(define BehaviorClass
  (make-meta (string->symbol "Behavior class")
             Behavior
             ObjectClass))

(define ClassDescriptionClass
  (make-meta (string->symbol "ClassDescription class")
             ClassDescription
             BehaviorClass))

(define ClassClass
  (make-meta (string->symbol "Class class")
             Class
             ClassDescriptionClass))

(define MetaClassClass
  (make-meta (string->symbol "MetaClass class")
             MetaClass
             ClassDescriptionClass))

;; (addSelector:withMethod:
;;         MetaClassClass
;; 	'subclasses
;; 	 (lambda (self) '()))

;; and of course UndefinedObject

(define UndefinedObject
  (make-protoClass 'UndefinedObject
		   Object
		   st-nil
		   all-class-ivar-names))

(define UndefinedObjectClass
  (make-meta (string->symbol "UndefinedObject class")
             UndefinedObject
             ObjectClass))

(setClass: st-nil UndefinedObject)
(rebase-mdict! UndefinedObject st-nil-behavior)
;; for st->scm generated code
(define nil st-nil)

;; make accessable to Smalltalk
(smalltalkAt:put: 'Object Object)
(smalltalkAt:put: 'Behavior Behavior)
(smalltalkAt:put: 'ClassDescription ClassDescription)
(smalltalkAt:put: 'Class Class)
(smalltalkAt:put: 'MetaClass MetaClass)
(smalltalkAt:put: 'UndefinedObject UndefinedObject)

;; For DEBUG printing
(primAddSelector:withMethod:
 st-symbol-behavior
 'asString
 (lambda (self) (symbol->string self)))

(addSelector:withMethod:
        Object
	'allInstVarNames
	allInstVarNames)

(addSelector:withMethod:
        ObjectClass
	'allInstVarNames
	 allInstVarNames)

(addSelector:withMethod:
        MetaClass
	'allInstVarNames
	 allInstVarNames)

(addSelector:withMethod:
        MetaClassClass
	'allInstVarNames
	 allInstVarNames)

;; earlly bound for debugging
(primAddSelector:withMethod: st-nil-behavior
			     'instanceVariableNames
			     (lambda (self) st-nil))

(primAddSelector:withMethod: 
    st-symbol-behavior
    'printOn:
    (lambda (self port)
      (let ( (charList (string->list (symbol->string self))) )
	(if (memq #\  charList)
    	    (format port "#'~a'" self)
	    (format port "#~a" self)))
    ) )

(primAddSelector:withMethod: 
    st-string-behavior
    'printOn:
    (lambda (self port)
      (format port "'~a'" self))
    )

(primAddSelector:withMethod: 
    st-list-behavior
    'printOn:
    (lambda (self port)
      (format port "List~a" self))
    )

(primAddSelector:withMethod: 
    st-symbol-behavior
    'printString
     printString)

(primAddSelector:withMethod: 
    st-string-behavior
    'printString
     printString)

(primAddSelector:withMethod: 
    st-list-behavior
    'printString
     printString)

;;;			--- E O F ---			;;;
