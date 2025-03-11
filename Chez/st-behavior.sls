#!r6rs
;;; FILE: "st-behavior.sls"
;;; IMPLEMENTS: Behavior, Collection, Set
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-behavior)

  (export
   Object
   Behavior
   ClassDescription   
   Collection
   Set
   IdentitySet
   )
  
  (import
   (rnrs base)
   (rnrs control (6))
   (rnrs hashtables (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   )

;;; OK.  Now use protoClasses to bootstrap core classes

(define Object
  (newSubclassName:iVars:cVars:
   Class
   'Object '() '()))

(define Behavior
  (newSubclassName:iVars:cVars:
   Object
   'Behavior '(superclass methodDict format) '()))

(define ClassDescription
  (newSubclassName:iVars:cVars:
   Behavior
   'ClassDescription '(instanceVariables organization) '()))


(define Collection
  (newSubclassName:iVars:cVars:
   Object
   'Collection '() '())
)

(define Set
  (newSubclassName:iVars:cVars:
   Collection
   'Set '(array tally) '())
)

(define IdentitySet
  (newSubclassName:iVars:cVars:
   Set
   'IdentitySet '() '())
)


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

;;; OK. Scaffolding in place.
;;; Can now use #newSubclassName:iVars:cVars:

;;;The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make the metaClass
;;;   Then ask the metaClass to make its instance

(perform:with: Object 'superclass: st-nil) ;; ground case
(perform:with: (class Object) 'superclass: Class)
(perform:with: Object 'methodDict: st-object-behavior)

;; Redo relations

(perform:with: MetaClassClass 'superclass: (class ClassDescription))
(perform:with: ClassClass     'superclass: (class ClassDescription))
(perform:with: (class ClassDescription)
               'subclasses: (list ClassClass MetaClassClass))

;;; (MetaClass class class) == MetaClass
(setClass: MetaClass MetaClassClass)

(perform:with: Class
               'instanceVariables: class-added-var-names)
(perform:with: MetaClass
               'instanceVariables: metaClass-added-var-names)
(perform:with: ClassClass
               'instanceVariables: '())
(perform:with: MetaClassClass
               'instanceVariables: '())

;; (for-each ;; get regular
;;  (lambda (class)
;;    (primAddSelector:withMethod:
;;     (behavior class)
;;     'allInstVarNames allInstVarNames)
;;    )
;;  (list Object Behavior ClassDescription Class MetaClass
;;        (class Object) (class Behavior) (class ClassDescription)
;;        (class Class) (class MetaClass)))
;;
;; See below.  Now done as:
;;   (addSelector:withMethod: Object
;;                            'allInstVarNames
;;                            allInstVarNames)

(perform:with: Class     'superclass: ClassDescription)
(perform:with: MetaClass 'superclass: ClassDescription)
(perform:with: ClassDescription
               'subclasses: (list Class MetaClass))

;; make accessable to Smalltalk
(smalltalkAt:put: 'Class Class)
(smalltalkAt:put: 'MetaClass MetaClass)

(perform:with: Object 'myMethodNames: (selectors Object))

;; Am I self-referential, or what??
;;   Talk about "meta-circular"!!
(addSelector:withMethod: (class Object)
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: (class Object)
                         'allInstVarNames
                         allInstVarNames)

(addSelector:withMethod:
     (class Object)   ;; NB: not initialized
     'basicNew: basicNew:) 

(addSelector:withMethod:
     (class Object)
     'basicNew  ;; NB: not initialized
     (lambda (self) (basicNew: self 0)))

(addSelector:withMethod:
     (class Object)
     'new:    ;; initialized
     (lambda (self size)
       (perform: (perform:with: self 'basicNew: size)
                 'initialize)))

(addSelector:withMethod:
     (class Object)
     'new    ;; initialized
     (lambda (self)
       (perform: (basicNew: self 0) 'initialize)))

(addSelector:withMethod:
     (class Object)
     'addSubclass: addSubclass:)

(addSelector:withMethod:
     (class Object)
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

(perform:with:
     Behavior
     'category: 'Kernel-Classes)

(perform:with:
     Behavior
     'comment:
"My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass)."
)

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
	
I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.
"
)

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

(perform:with:
     MetaClass
     'category: 'Kernel-Classes)

(perform:with:
     MetaClass
     'comment:
"My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class."
)

(addSelector:withMethod:
     Object
     'superPerform: superPerform:)

(addSelector:withMethod:
     Object
     'superPerform:with: superPerform:with:)

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
     (class Behavior)
     '>>
;; "Answer the compiled method associated with the argument, selector (a 
;; Symbol), a message selector in the receiver's method dictionary. If the 
;; selector is not in the dictionary, create an error notification."
     (lambda (self selectorSymbol)
       (primLookup: (perform: self 'methodDict) selectorSymbol)))

(addSelector:withMethod:
     Object
     '>>
;; "Answer the compiled method associated with the argument, selector (a 
;; Symbol), a message selector in the receiver's method dictionary. If the 
;; selector is not in the dictionary, create an error notification."
     (lambda (self selectorSymbol)
       (primLookup: (perform: self 'methodDict) selectorSymbol)))

(addSelector:withMethod:
 	(class Object)
        'newSubclassName:iVars:cVars:
        newSubclassName:iVars:cVars:)

(addSelector:withMethod:
 	MetaClass
        'newSubclassName:iVars:cVars:
        newSubclassName:iVars:cVars:
        )

(addSelector:withMethod: Behavior
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: (class Behavior)
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: (class Object)
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: (class (class Object)) ;; MetaClass
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod:
     Behavior
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Behavior)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     ClassDescription
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ClassDescription)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Class
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Class)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     MetaClass
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'MetaClass)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Behavior
     'allSelectors
     (lambda (self)
       ($ (hashtable-keys ($ self 'methodDict))
          'asIdentitySet))) ;; vector->identSet

(addSelector:withMethod:
     Behavior
     'selectors
     ;; Answer identSet of non-inherited method selectors
     (lambda (self)
       (let ( (superDict
               ($ (superclass self) 'methodDict))
              (selfDict ($ self 'methodDict))
              (iSet ($ IdentitySet 'new))
            )
         ($: selfDict
             'keysAndValuesDo:
             (lambda (k v)
               (cond
                ((hashtable-contains? superDict k)
                 (when (not  ;; not same v as super
                        (eq? v
                             (hashtable-ref superDict
                                            k
                                            st-nil)))
                   ($ iSet 'add: k)))
                ;; else must be local; add selector
                (else ($: iSet 'add: k)))))
         iSet
       ) )
)

(addSelector:withMethod:
     (class Behavior)
     'allSelectors
     (lambda (self)
       ($ (hashtable-keys ($ self 'methodDict))
          'asIdentitySet))) ;; vector->identSet

(addSelector:withMethod:
     (class Behavior)
     'selectors
     ;; Answer identSet of non-inherited method selectors
     (lambda (self)
       (let ( (superDict
               ($ (superclass self) 'methodDict))
              (selfDict ($ self 'methodDict))
              (iSet ($ IdentitySet 'new))
            )
         ($: selfDict
             'keysAndValuesDo:
             (lambda (k v)
               (cond
                ((hashtable-contains? superDict k)
                 (when (not  ;; not same v as super
                        (eq? v
                             (hashtable-ref superDict
                                            k
                                            st-nil)))
                   ($: iSet 'add: k)))
                ;; else must be local; add selector
                (else ($: iSet 'add: k)))))
         iSet
      ) )
   )



)
