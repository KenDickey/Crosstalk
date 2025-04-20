#!r6rs
;;; File: "st-core-classes.ss"
;;; IMPLEMENTS: Smalltalk Kernel Class Structure
;;;  Object, Behavior, ClassDescription, MetaClass, Class
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
  '(class
    superclass
    methodDict	;; shared by all instances
    format))	;; layout/structure

(define classDescription-ivar-names
  '(instanceVariables
    organization))

(define metaClass-ivar-names
  '(subclasses
    name
    myMethodNames ;; used to avoid overwrite
    ;; NB: pools are depricated !!
    thisClass))

(define all-metaClass-ivar-names
  (append  behavior-ivar-names
	   classDescription-ivar-names
	   metaClass-ivar-names))

(define class-ivar-names
  '(subclasses
    name
    myMethodNames ;; used to avoid overwrite
    ;; NB: pools are depricated !!
    category
    comment))

(define all-class-ivar-names
  (append  behavior-ivar-names
	   classDescription-ivar-names
	   class-ivar-names))

;;; Setup core class/superclass relations

(define (make-protoClass name mDict super local-ivars)
  (let ( (aClass (make-st-object mDict (length all-class-ivar-names))) )
    (add-getters&setters mDict
			 num-header-slots
			 all-class-ivar-names)
    ($: aClass 'instanceVariables: local-ivars)
    ($: aClass 'superclass: super)
    (addSubclass: super aClass)
    ($: aClass 'methodDict: mDict)
    ;;; copydown
    ;; (unless (st-nil? super)
    ;;   (behavior-add-from-other mDict ($ super 'methodDict)))
    ($: aClass 'name: name)
    aClass)
  )

(define Object ;; an object'a Class
  (make-protoClass 'Object
		   st-object-behavior
		   st-nil   ; nil superclass
		   st-nil)) ; no local ivars

(define Behavior
  (make-protoClass 'Behavior
		   st-behavior-behavior
		   Object
		   behavior-ivar-names))

(define ClassDescription
  (make-protoClass 'ClassDescription
		   st-classDescription-behavior
		   Behavior
		   classDescription-ivar-names))

(define Class
  (make-protoClass 'Class
		   st-class-behavior
		   ClassDescription
		   class-ivar-names))

(define MetaClass
  (make-protoClass 'MetaClass
		   st-metaClass-behavior
		   ClassDescription
		   metaClass-ivar-names))


;;; MetaClasses for the above
;;; Class Class, MetaClass Class, ..
(define (make-meta name for-class superclass classVarNames)
  (let* ( (mDict (make-method-dictionary))
	  (aMetaClass
	   (make-st-object mDict
			  (length all-metaClass-ivar-names)))
        )
    (add-getters&setters mDict
                     num-header-slots
                     all-metaClass-ivar-names)
    ($: aMetaClass 'name: name)
    ($: aMetaClass 'thisClass: for-class)
    (unless (st-nil? superclass)
      ($: aMetaClass 'superclass: superclass)
      (addSubclass: superclass aMetaClass))
    ($: aMetaClass 'instanceVariables: st-nil)
    ($: aMetaClass 'methodDict: mDict)
    ($: for-class 'class: aMetaClass)
    aMetaClass)
  )

    
(define ObjectClass
  (make-meta (string->symbol "Object class")
             Object
             Class
	     st-nil))

(define BehaviorClass
  (make-meta (string->symbol "Behavior class")
             Behavior
             ObjectClass
	     st-nil))

(define ClassDescriptionClass
  (make-meta (string->symbol "ClassDescription class")
             ClassDescription
             BehaviorClass
	     st-nil))

(define ClassClass
  (make-meta (string->symbol "Class class")
             Class
             ClassDescriptionClass
	     st-nil))

(define MetaClassClass
  (make-meta (string->symbol "MetaClass class")
             MetaClass
             ClassDescriptionClass
	     st-nil))

;; and of course UndefinedObject

(define UndefinedObject
  (make-protoClass 'UndefinedObject
		   st-nil-behavior
		   Object
		   st-nil))

(define UndefinedObjectClass
  (make-meta (string->symbol "UndefinedObject class")
             UndefinedObject
             ObjectClass
	     st-nil))


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


;;; The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make the metaClass
;;;   Then ask the metaClass to make its instance

;;; (newSubclassName:iVars:cVars: aClass nameSym instVars classVars)


;; Internal helper. Create an INSTANCE of a Class or MetaClass
(trace-define (instantiateName:superclass:ivars:
         selfClass
         nameSymbol
         superClass
         addedInstanceVars)
  (let* ( (inherited-vars (perform: superClass 'allInstVarNames))
          (allIvars
             (append inherited-vars addedInstanceVars))
          (num-inherited-vars (length inherited-vars))
          (numAddedVars (length addedInstanceVars))
          (newInst
             (basicNew: selfClass numAddedVars))
          (newMethodDict
             (clone-method-dictionary (perform: superClass 'methodDict)))
        )
    (unless (zero? numAddedVars)
      (let ( (start-index (+ num-header-slots num-inherited-vars)) )
;;@@DEBUG{
 ;; (display (perform: selfClass 'name))
 ;; (display ":  start-index for added vars: ")
 ;; (display (number->string start-index))
 ;; (newline)
;;}DEBUG@@
         (add-getters&setters newMethodDict start-index addedInstanceVars))
    )
    (perform:with: newInst 'methodDict: newMethodDict)
    (primSetClass: newMethodDict newInst)
    (setClass:     newInst    selfClass)
    (perform:with: newInst    'superclass: superClass)
    (addSubclass:  superClass newInst)
    (perform:with: newInst    'name:       nameSymbol)
    (perform:with:
       newInst ;; ANSI requires a fresh (unshared) list
       'instanceVariables: (list-copy addedInstanceVars))
;;@@DEBUG{
;;    (display-ivars newInst)
;;}DEBUG@@
    (perform: newInst 'initialize)  ;; NB: should always return newInst !!
) )

(define (name->metaName nameSym)
  (string->symbol
   (string-append
    (symbol->string nameSym)
    " class")))

;;; Helper Checks

(define (checkClassName nameSym)
    (unless (and (symbol? nameSym)
               (let ( (name (symbol->string nameSym)) )
                 (and 
                  (> (string-length name) 1)
                  (char-upper-case? (string-ref name 0)))))
    (error
     'newSubclassName:iVars:cVars:
     "subclass name must be a symbol which starts uppercase"
     nameSym)))

(define (checkInstVarNames instanceVars)
  (unless (or (list? instanceVars) (vector? instanceVars))
    (error 'newSubclassName:iVars:cVars:
           "InstanceVariableNames must be a list or array of symbols"
           instanceVars)))

(define (checkClassVarNames classVars)
  (unless (or (list? classVars) (vector? classVars))
    (error 'newSubclassName:iVars:cVars:
           "ClassVariableNames must be a list or array of symbols"
           classVars)))

;;; Now we can ask a Class to create a new Subclass

(trace-define (newSubclassName:iVars:cVars:
         selfClass nameSym instanceVars classVars)

  (checkClassName nameSym)
  (checkInstVarNames instanceVars)
  (checkClassVarNames classVars)

  (let ( (instanceVarsList
          (if (vector? instanceVars)
              (vector->list instanceVars)
              instanceVars))
         (classVarsList
          (if (vector? classVars)
              (vector->list classVars)
              classVars))
       )
    (unless (every? symbol? instanceVarsList)
      (error 'newSubclassName:iVars:cVars:
             "InstanceVariableNames must be a list of symbols"
             instanceVarsList))

    (unless (every? symbol? classVarsList)
      (error 'newSubclassName:iVars:cVars:
             "ClassVariableNames must be a list of symbols"
             classVarsList))

    (let* (
	    (super (superclass selfClass))

            (newSubclass
             (make-protoClass
	      nameSym
	      (if (st-nil? super)
		  (make-method-dictionary)
		  (clone-behavior
		   ($ super 'methodDict)))
	      selfClass
	      instanceVarsList))

	    (newMetaClass
	     (make-meta
	      (name->metaName nameSym)
	      newSubclass
	      (class selfClass)
              classVarsList))
	    )

      (smalltalkAt:put: nameSym newSubclass)
      newSubclass	;; @@??@@ move initialize to here?
) ) )


;;;			--- E O F ---			;;;
