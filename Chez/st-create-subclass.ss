#!r6rs
;;; File: "st-create-subclass.ss"
;;; IMPLEMENTS: Allow a Class to create a subclass.
;;;  
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: May 2025

;;; (newSubclassName:iVars:cVars: aClass nameSym instVars classVars)

;;; The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make a new metaClass.
;;;   Then ask the new metaClass to make its singleton
;;;     instance, #thicClass.


;;; What to expect from this complex bit of business: 

;; Smalltalk syntax:
;; `Object newSubClassName: #SampleClass
;;         iVars: #(ivar1 ivar2 ivar3)
;;         cVars: #(cvar1 cvar2)`
;; --> Scheme
;; (newSubclassName:iVars:cVars:
;; 	 Object
;; 	 'SampleClass
;; 	 '(ivar1 ivar2 ivar3)
;; 	 '(cvar1 cvar2))

;; ==>

;; The 'MetaClass': [Created in/by "st-core-classes.ss"]
;; (vector <metaClassBehavior> ;; a method dictionary
;; 	class ;; 'MetaClass class'
;; 	superclass ;; ClassDescription
;; 	instanceBehavior ;; <metaClassInstance behavior>
;; 	format
;; 	name ;; 'MetaClass
;; 	instanceVariableNamess ;; '(thisClass)
;; 	organization
;; 	myMethodNames
;; 	)

;; The 'SampleClass class':
;; (vector <metaClassInstance behavior>
;; 	class ;; 'MetaClass'
;; 	superclass ;; 'Object Class'
;; 	instanceBehavior  ;; <'SampleClass' behavior>
;; 	format
;; 	name ;; 'SampleClass class'
;; 	instanceVariableNames ;; '(cvar1 cvar2)
;; 	organization
;; 	myMethodNames
;; 	subclasses 
;; 	category
;; 	comment
;; 	thisClass ;; 'SampleClass'
;; 	)

;; The 'SampleClass':
;; (vector  <'SampleClass' behavior>
;; 	class ;; 'SampleClass class'
;; 	superclass ;; 'Object'
;; 	instanceBehavior  ;; <'SampleClass' instance behavior>
;; 	format
;; 	name ; 'SampleClass'
;; 	instanceVariableNamess ;; '(ivar1 ivar2 ivar3)
;; 	organization
;; 	myMethodNames
;; 	subclasses ;; none yet
;; 	category
;; 	comment
;; 	cvar1  ;; ivars added by/from 'SampleClass class'
;; 	cvar2
;; 	)

;; An intance of SampleClass:
;; (vector  <'SampleClass' instance behavior>
;; 	 ivar1
;; 	 ivar2 ;; ivars added by 'SampleClass'
;; 	 ivar3
;; 	 )

;;; OK, ask a Class to create a new Subclass

(define (newSubclassName:iVars:cVars:
         aClass nameSym instanceVars classVars)

  (checkClassName nameSym)

  (let* ( (instanceVarsList
 	     (checkVarNames "instance Variabless" instanceVars))

          (classVarsList
 	     (checkVarNames "class Variabless" classVars))

	  (newMetaClass
             (instantiateName:superclass:instanceIVars:
              MetaClass ;; make a metaClass
              (name->metaName nameSym)
	      (class aClass) ;; who's super is my metaClass
              classVarsList))

	  (newSubclass
             (instantiateName:superclass:instanceIVars:
              newMetaClass ;; metaClass make the subclass
              nameSym
	      aClass ;; I am the superclass
              instanceVarsList))
	 )
    
    (perform:with: newMetaClass 'thisClass: newSubclass)
    (addSubclass: aClass newSubclass)
    (smalltalkAt:put: nameSym newSubclass)

    newSubclass	;; @@??@@ move initialize to here?
) )

;;; Helpers

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

(define (checkVarNames whatStr names)
  (unless (or (list? names) (vector? names))
    (error 'newSubclassName:iVars:cVars:
           (string-append whatStr
			  " must be a list or array of symbols")
           names))
  (let ( (name-list
	  (if (vector? names)
              (vector->list names)
              names))
	)
    (unless (every? symbol? name-list)
      (error 'newSubclassName:iVars:cVars:
             (string-append whatStr
			    " must be a list of symbols")
             name-list))
    name-list
    ) )

;; Create new instance of a Class or MetaClass.
(define (instantiateName:superclass:instanceIVars:
	 aClass
	 nameSymbol
	 super
	 addedInstanceVars)
  (let ( (newClassInst (basicNew aClass))
	 (instanceBehavior (clone-behavior ($ super 'instanceBehavior)))
	 (num-inherited-vars (length (allInstVarNames super)))
       )
    ;; Note: accessors already in ($ aClass 'instanceBehavior)
    ($: newClassInst 'name: nameSymbol)
    ($: newClassInst 'class: aClass) 
    ($: newClassInst 'superclass: super)
    ($: newClassInst 'instanceBehavior: instanceBehavior)
    (primSetClass: instanceBehavior newClassInst) ;; override
    (unless (st-nil? addedInstanceVars)
      ; newClassInst's instance ivars
      ($: newClassInst 'instanceVariableNames:
	  ;; ANSI requires a fresh list
	  (list-copy addedInstanceVars))
      ($: newClassInst 'myMethodNames:
	  (add-getters&setters instanceBehavior
			       (+ num-header-slots
				  num-inherited-vars)
			       addedInstanceVars)))
    newClassInst))  ; NB: no #initialize performed here

;; Classes have ivars to know their super and sub classes.
;; MetaClasses know only their superclasses.
;; To allow "copydown" to work, we need to fix this.
;;  (map printString ($ (class Boolean) 'subclasses))
;;     ==> ("a False class" "a True class")
(addSelector:withMethod:
 MetaClass
 'subclasses
 (lambda (aMetaClass)
   (map class ($ ($ aMetaClass 'thisClass) 'subclasses)))
 )

;; make this available as a Class method
(addSelector:withMethod:
 	ObjectClass
	'newSubclassName:iVars:cVars:
	 newSubclassName:iVars:cVars:)

;;;			--- E O F ---			;;;
