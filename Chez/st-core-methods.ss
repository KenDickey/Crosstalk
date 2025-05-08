#!r6rs
;;; File: "st-core-methods.ss"
;;; IMPLEMENTS: Smalltalk Kernel Class methods
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;; (load "st-core-classes.ss")

;;; OK, time to put some meat on the bones.
;;; Add methods to behaviors/methodDictionaries.

;;; ============================================
;;; Behavior adds intelligence to structure
;;;    (behavior obj) answers a method-dictionary
;;; ============================================

(define (printString obj) ;; polymorphic
;; String streamContents: [:s | self printOn: s]
  (let ( (outport (open-output-string)) )
    (if (respondsTo: obj 'printOn:)
	(perform:with: obj 'printOn: outport)
	(format #f "~a" obj))
    (get-output-string outport)))


;;; Object

($: Object 'myMethodNames: (selectors Object))

(addSelector:withMethod:
 	Object
        'perform:    ;; ANSI
        perform:)

(addSelector:withMethod:
 	Object
        'perform:with:	 ;; ANSI
        perform:with:)

(addSelector:withMethod:
 	Object
        'perform:with:with:  ;; ANSI
        perform:with:with:)

(addSelector:withMethod:
 	Object
        'perform:with:with:with:  ;; ANSI
        perform:with:with:with:)

(addSelector:withMethod:
 	Object
        'perform:withArguments:   ;; ANSI
        perform:withArguments:)

(addSelector:withMethod:
 	Object
        'perform:withArgsList:   ;; Scheme
        perform:withArgsList:)

(addSelector:withMethod:
 	Object
        'perform:withArguments:inSuperclass:
        perform:withArguments:inSuperclass:)

(addSelector:withMethod:
 	Object
        'doesNotUnderstand:    ;; ANSI
        doesNotUnderstand:)

(addSelector:withMethod:
 	Object
        'respondsTo:    ;; ANSI
        respondsTo:)

(addSelector:withMethod:
 	(class Object)
        'respondsTo:    ;; ANSI
        respondsTo:)


(addSelector:withMethod:
 	Object
        'isKindOf:    ;; ANSI
        isKindOf:)

(addSelector:withMethod:
 	(class Object)
        'isKindOf:    ;; ANSI
        isKindOf:)

(addSelector:withMethod:arity:
        Object
        '==    ;; ANSI
        (lambda (self other) (eq? self other))
	2)

(addSelector:withMethod:arity:
        Object
        '~~    ;; ANSI
        (lambda (self other) (not (eq? self other)))
	2)

(addSelector:withMethod:arity:
        Object
        '=   ;; ANSI
        (lambda (self other) (eqv? self other))
	2)

(addSelector:withMethod:arity:
        Object
        '~=   ;; ANSI
        (lambda (self other) (not (eqv? self other)))
	2)

(addSelector:withMethod:
 	Object
        'hash   ;; ANSI
        equal-hash)

(addSelector:withMethod:
 	Object
        'identityHash   ;; ANSI
        equal-hash)

(addSelector:withMethod:
 	Object
        'printString   ;; ANSI
        printString)

(addSelector:withMethod:
 	Object
        'printOn:  ;; ANSI
        (lambda (self outport)
          (let ( (vowels (string->list "aeiouAEIOU"))
                 (nameStr ($ (name self) 'asString))
               )
            (display
             (string-append
              (if (memq (string-ref nameStr 0) vowels)
                  "an " "a ")
              nameStr)
             outport))))

(addSelector:withMethod:
 	ObjectClass
        'printOn:  ;; ANSI
        (lambda (self outport)
          (let ( (vowels (string->list "aeiouAEIOU"))
                 (nameStr ($ (name self) 'asString))
               )
            (display
             (string-append
              (if (memq (string-ref nameStr 0) vowels)
                  "an " "a ")
              nameStr)
             outport))))


(addSelector:withMethod:
 	Object
        'is:
        (lambda (self aSymbol) #f)) ; base case

(addSelector:withMethod: ;; ANSI
 	Object
        'isMemberOf:
        (lambda (self someClass)
          (eq? (perform: self 'class) someClass))
)

(addSelector:withMethod: ;; ANSI
 	Object
        'notNil
        (lambda (self)
          (not (st-nil? self)))
)

(addSelector:withMethod: ;; ANSI
 	Object
        'isNil
        (lambda (self)
          (st-nil? self))
)

(addSelector:withMethod:  ;; base case
 	Object
        'basicSize ;; number of indexable slots in basic object
        (lambda (self)
          (cond
            ((st-object? self)  (st-object-length self))
            ((vector? self)     (vector-length self))
            ((string? self)     (string-length self))
            ((symbol? self)     (symbol-length self))
            ((bytevector? self) (bytevector-length self))
            (else 0)
        ) )
)

(addSelector:withMethod:  ;; base case
 	(class Object)
        'basicSize ;; number of indexable slots in basic object
        (lambda (self)
          (cond
            ((st-object? self)  (st-object-length self))
            ((vector? self)     (vector-length self))
            ((string? self)     (string-length self))
            ((symbol? self)     (symbol-length self))
            ((bytevector? self) (bytevector-length self))
            (else 0)
        ) )
)

(addSelector:withMethod:
 	Object
        'shallowCopy  
        ;; A shallow copy shares slot-values
        (lambda (self) (st-obj-copy self))
)

(addSelector:withMethod:
 	Object
        'copy  ;; NB: Subclasses should override #copy
               ;; NOT #basicCopy
        (lambda (self) (perform: self 'basicCopy))
)

(addSelector:withMethod:
 	Object
        'yourself    ;; ANSI
        (lambda (self) self)
)


(addSelector:withMethod:
 	Object
        'initialize
        (lambda (self) self))

(addSelector:withMethod:
 	Object
        'basicCopy
        (lambda (self)
          (cond
            ;; NB: vector-copy works for all St objects
            ((st-object? self)  (st-obj-copy self))
            ((vector? self)     (vector-copy self)) 
            ((string? self)     (string-copy self))
            ((symbol? self)     (symbol-copy self))
            ((procedure?  self) (procedure-copy  self))
            ((bytevector? self) (bytevector-copy self))
            ((hashtable?  self) (hashtable-copy  self))
            ((list? self)       (list-copy self))
            ;;@@ environment, port, ..?
            (else self) ;; immediates not copyable!
        ) )
)

(addSelector:withMethod:
 	Object
        'error:  ;; ANSI
        (lambda (self aString)
          (error aString self))) ;;; @@FIXME: Debug!

(addSelector:withMethod:
 	Object
        'value
        (lambda (self) self)) ;; St ideom

(addSelector:withMethod:arity:
     ObjectClass
     '>>
;; "Answer the compiled method associated with the argument, selector (a 
;; Symbol), a message selector in the receiver's method dictionary. If the 
;; selector is not in the dictionary, create an error notification."
     (lambda (self selectorSymbol)
       (primLookup: (perform: self 'instanceBehavior) selectorSymbol))
     2)


;; Am I self-referential, or what??
;;   Talk about "meta-circular"!!
(addSelector:withMethod: ObjectClass
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: ObjectClass
			 'allSuperclasses
			 allSuperclasses)

(addSelector:withMethod: ObjectClass
                         'allInstVarNames
                          allInstVarNames)

(addSelector:withMethod: Object
			 'superPerform:
			 superPerform:)

(addSelector:withMethod:
     Object
     'superPerform:with:
     superPerform:with:)

(addSelector:withMethod:
     Object
     'superPerform:with:with: superPerform:with:with:)

(addSelector:withMethod:
     Object
     'superPerform:with:with:with: superPerform:with:with:with:)

(addSelector:withMethod:
     Object
     'superPerform:withArguments: superPerform:withArguments:)

(addSelector:withMethod:
     Object
     'species
;; "Answer the preferred class for reconstructing the receiver.  For example, 
;; collections create new collections whenever enumeration messages such as 
;; collect: or select: are invoked.  The new kind of collection is determined by 
;; the species of the original collection.  Species and class are not always the 
;; same.  For example, the species of Interval is Array."
     (lambda (self) (class self)))


(addSelector:withMethod:
     ObjectClass
     'basicNew:   ;; NB: not initialized
      basicNew:) 

(addSelector:withMethod:
     ObjectClass
     'basicNew  ;; NB: not initialized
      basicNew)

(addSelector:withMethod:
     ObjectClass
     'new:    ;; initialized
     (lambda (self size)
       (perform: ($: self 'basicNew: size)
                 'initialize)))

(addSelector:withMethod:
     ObjectClass
     'new    ;; initialized
     (lambda (self)
       ($ (basicNew self) 'initialize))
     )

(addSelector:withMethod:
     ObjectClass
     'addSubclass: addSubclass:)

(addSelector:withMethod:
     ObjectClass
     'subclassesDo:
     (lambda (self aBlock)
       (for-each aBlock (perform: self 'subclasses))))


;;; Class Info

(perform:with:
     Object
     'category: 'Kernel-Objects)

(perform:with:
     Object
     'comment:
"Object is the root class for all of the other classes in the class hierarchy.

Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here.

Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (Array and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes.

Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as #isNil."
)

;;; Behavior

(addSelector:withMethod:
     Behavior
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Behavior)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Behavior
     'allSuperclasses
     allSuperclasses)

(perform:with:
     Behavior
     'category: 'Kernel-Classes)

(perform:with:
     Behavior
     'comment:
"My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass)."
)

;;; ClassDescription


(addSelector:withMethod:
     ClassDescription
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ClassDescription)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     ClassDescription
     'allInstVarNames
     allInstVarNames)

(perform:with:
     ClassDescription
     'category: 'Kernel-Classes)

(perform:with:
     ClassDescription
     'comment:
"I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	The maintenance of a ChangeSet, and logging changes on a file
	Most of the mechanism for fileOut.
	
I am an abtsract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.
"
)

;;; Class

(addSelector:withMethod:
     Class
     'allSubclasses
     allSubclasses)

(addSelector:withMethod:
     Class
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Class)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     Class
     'category: 'Kernel-Classes)

(perform:with:
     Class
     'comment:
"I add a number of facilities to those in ClassDescription:
	A set of all my subclasses (defined in ClassDescription, but only used here and below)
	A name by which I can be found in a SystemDictionary
	A classPool for class variables shared between this class and its metaclass
	A list of sharedPools which probably should be supplanted by some better mechanism.

My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.
"
)

;;; MetaClass

(addSelector:withMethod:
     MetaClass
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'MetaClass)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Class
     'allSubclasses
     allSubclasses)

;; (perform:with:
;;      MetaClass
;;      'category: 'Kernel-Classes)

;; (perform:with:
;;      MetaClass
;;      'comment:
;; "My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
;; In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
;; 	Integer superclass == Number, and
;; 	Integer class superclass == Number class.
;; However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
;; 	Object superclass == nil, and
;; 	Object class superclass == Class."
;; )

;;; Classes create new (sub)classes

(addSelector:withMethod:
     Object
     'newSubclassName:iVars:cVars:
     newSubclassName:iVars:cVars:)

;; Via copydown:

;; (addSelector:withMethod:
;;      Class
;;      'newSubclassName:iVars:cVars:
;;      newSubclassName:iVars:cVars:)

;; (addSelector:withMethod:
;;      MetaClass
;;      'newSubclassName:iVars:cVars:
;;      newSubclassName:iVars:cVars:)


;;; UndefinedObject -> st-nil

(addSelector:withMethod: 
 	UndefinedObject
        'printOn:
        (lambda (self port)
          (display "nil" port)))

(addSelector:withMethod: 
 	UndefinedObject
        'printString
        printString)

(addSelector:withMethod: 
 	UndefinedObject
        'notNil
        (lambda (self) st-false))

(addSelector:withMethod: 
 	UndefinedObject
        'asSymbol
        (lambda (self) 'nil)) 


(addSelector:withMethod: 
 	UndefinedObject
        'isNil
        (lambda (self) st-true))

(addSelector:withMethod:
        UndefinedObject
        'notNil
        (lambda (self) st-false))

(addSelector:withMethod:
        Object
        'notNil
        (lambda (self) st-true))

(addSelector:withMethod:
        UndefinedObject
        'isNil
        (lambda (self) st-true))

(addSelector:withMethod:
        Object
        'isNil
        (lambda (self) st-false))

(addSelector:withMethod:
        UndefinedObject
        'isEmptyOrNil ;; collection protocol
        (lambda (self) st-true))

(addSelector:withMethod:
        UndefinedObject
        'ifNil:
        (lambda (self thunk) (thunk)))

(addSelector:withMethod:
        Object
        'ifNil:  ;; NB: return self  !!
        (lambda (self thunk) self))

(addSelector:withMethod:
        UndefinedObject
        'ifNil:ifNotNil:
        (lambda (self nilBlock ifNotNilBlock)
          (nilBlock)))

(addSelector:withMethod:
        Object
        'ifNil:ifNotNil:
        (lambda (self nilBlock ifNotNilBlock)
          (ifNotNilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNil:
        (lambda (self nilBlock) st-nil))

(addSelector:withMethod:
        Object
        'ifNotNil:
        (lambda (self nilBlock)
          (nilBlock)))

(addSelector:withMethod:
        Object
        'ifNotNil:ifNil
        (lambda (self ifNotNilBlock nilBlock)
          (ifNotNilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNil:ifNil:
        (lambda (self ifNotNilBlock nilBlock)
          (nilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNilDo:
        (lambda (self block) st-nil))

(addSelector:withMethod:
        Object
        'ifNotNilDo:
        (lambda (self block) (block self)))

(addSelector:withMethod:
        UndefinedObject
        'basicCopy
        (lambda (self) self))

(addSelector:withMethod:
        UndefinedObject
        'asSymbol
        (lambda (self) 'nil))

(addSelector:withMethod:
        UndefinedObject
        'asString
        (lambda (self) "nil"))

(addSelector:withMethod: 
 	UndefinedObject
        'basicNew:
        (lambda (self size)
          (error 'basicNew: "You may not create any more undefined objects--use nil" self)))

(addSelector:withMethod: 
 	UndefinedObject
        'new
        (lambda (self)
          (error 'new "You may not create any more undefined objects--use nil" self)))

(addSelector:withMethod: 
 	UndefinedObject
        'new:
        (lambda (self size)
          (error 'new: "You may not create any more undefined objects--use nil" self)))

(addSelector:withMethod: 
 	UndefinedObject
        'value
        (lambda (self) st-nil))

(addSelector:withMethod:
        UndefinedObject
        'shallowCopy ;; Can't clone nil
        (lambda (self) self))

(addSelector:withMethod: 
 	UndefinedObject
        'initializedInstance
        (lambda (self) st-nil)) ; '()

(perform:with:
     UndefinedObject
     'category: 'Kernel-Objects)

(perform:with:
     UndefinedObject
     'comment:
"I describe the behavior of my sole instance, nil. nil represents a prior
 value for variables that have not been initialized, or for results which
 are meaningless."
)

;;;			--- E O F ---			;;;
