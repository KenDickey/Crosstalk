;;; FILE: "st-core-classes.sch"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: 7 June 2016

;; (require 'st-kernel)  ; message send

;; We want to instantiate classes and bootstrap reflection
;; via Classes, which are the management structure for object behaviors.

;; Basically, each class has a superclass (except Object) and
;;   each Class's class is a metaClass which is an instance of MetaClass.
;; An instance's shape (instance valiables) and behavior (instance methods)
;;   is defined in its class
;;      (anObj behavior) == (anObj class methodDict)
;; A class's shape (class variables) and behavior (class methods)
;;   is defined in the instance class's metaClass 

;; Classes are instances of metaCLasses
;; metaClasses are instances of class MetaClass

;; We tie these together here by making instances and then setting
;;  up the proper references via instance variabkes and behaviors.

;; Boolean superclass -> Object
;; Object  superclass -> nil  (ground case)
;; Boolean class -> 'Boolean class' (its unnamed metaClass)
;; Boolean class class -> MetaClass
;; MetaClass class -> 'MetaClass class' (its metaClass)
;; MetaClass class class -> MetaClass (wraps around)
;; Boolean class superclass -> 'Object class' (Object's metaClass)
;; Boolean class superclass class -> Class
;; Object class superclass -> Class


;;; The Smalltalk Global Environment
(define smalltalk-dictionary (make-eq-hashtable))

(define combined-classDescription-var-names
  '(superclass methodDict format
    instanceVariables organization))

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



;; superclass           class               class class
;; ----------------------------------------------------
;; nil                  UndefinedObject  UndefinedObject class
;;  Object              Object class        MetaClass
;;    Behavior          Behavior class           "
;;      ClassDescrption ClassDescription class   "
;;         Class        Class class              "
;;         MetaClass    MetaClass class          "

;; shortcuts
(define (class      obj) (perform: obj 'class))
(define (superclass obj) (perform: obj 'superclass))

(define (allInstVarNames self)
  (let ( (ivarNames (perform: self 'instanceVariables))
         (super     (perform: self 'superclass))
       )
    (if (null? super)
        (list-copy ivarNames)
        (append (perform: super 'allInstVarNames) ivarNames))
) )

(define (allSuperclasses self)
  (let ( (mySuper (perform: self 'superclass)) )
    (if (null? mySuper)
        st-nil
        (append (allSuperclasses mySuper) (list mySuper)))
) )

(define (display-allSupers obj)
  (display-obj (allSuperclasses obj)))

(define (display-subs obj) ;; direct subclasses
  (display-obj (perform: obj 'subclasses)))

;; Below basicNew: Make a new instance of some class
(define (primNew: classSelf num-object-slots)
  (make-st-object
   (perform: classSelf 'methodDict)
   num-object-slots)
)

;; basicNew: Make a new instance of some class
(define (basicNew: classSelf num-added-vars)
;; NB: Added vars could be named and/or indexed
  (let* ( (num-inherited-vars
           (length
            (perform: classSelf 'allInstVarNames)))
          (newInst
           (primNew: classSelf
                     (+ num-inherited-vars num-added-vars)))
       )
    (setClass: newInst classSelf)
    newInst
) )

(define st-class-behavior (clone-behavior st-object-behavior))

(primAddSelector:withMethod:
     st-class-behavior
     'allInstVarNames allInstVarNames)

(primAddSelector:withMethod:
     st-class-behavior
     'basicNew: basicNew:)

;;; More mechanics: (addSubclass: classSelf subclass)
(define (addSubclass: classSelf subclass)
  (let ( (my-subclasses (perform: classSelf 'subclasses)) )
    (perform:with: classSelf
                   'subclasses:
                   (cons subclass my-subclasses))
) )

(primAddSelector:withMethod:
     st-class-behavior
     'addSubclass: addSubclass:)

(define st-metaClass-behavior (clone-behavior st-class-behavior))

(add-getters&setters st-class-behavior
                     num-header-slots
                     combined-class-ivar-names)

(add-getters&setters st-metaClass-behavior
                     num-header-slots
                     combined-metaClass-ivar-names)

;;; Scaffolding setup
;; Just enough behavior to allow instantiation bootstrap
;; to call: newSubclassName:iVars:cVars:

(define broken #false)

(define (make-protoClass
         name behav slot-names
         mDict child-ivar-names
         class super )
  (let* ( (behavior   (clone-behavior behav))
          (methodDict (clone-method-dictionary mDict))
          (class-instance
             (make-st-object behavior (length slot-names)))
        )
    (setClass: class-instance class) ;; NB: may be nil
    (perform:with: class-instance 'name: name)
    (perform:with: class-instance 'superclass: super) ;; may be nil
    (perform:with: class-instance 'instanceVariables: child-ivar-names)
    (perform:with: class-instance 'methodDict: methodDict)
   ;; return the new Class instance
    class-instance
) )

;;; Miminal Scaffolding
;; Temp for bootstrap -- re-relate later

(define ClassClass
  (make-protoClass
     '|Class class| ; name
     st-metaClass-behavior ; I am a MetaClass
     combined-metaClass-ivar-names    ;; slot-names
     st-class-behavior ; instances are Classes
     combined-class-ivar-names  ;; child-ivar-names
     '() ; class
     '() ; super
) )


(define Class
  (make-protoClass
     'Class ; name
     st-class-behavior
     combined-class-ivar-names  ;; slot-names
     st-class-behavior
     combined-class-ivar-names ;; child-ivar-names
     ClassClass ;; class
     '() ; super
) )

(define MetaClassClass
  (make-protoClass
     '|MetaClass class|
     st-metaClass-behavior ;; I am a MetaClass
     combined-metaClass-ivar-names   ;; slot-names
     st-class-behavior ;; that makes classes
     combined-class-ivar-names  ;; child-ivar-names
     '() ;; class is MetaClass
     ClassClass ;; super
 ) )

(define MetaClass
  (make-protoClass
     'MetaClass
     st-class-behavior   ;; MetaClass is a class
     combined-class-ivar-names   ;; slot-names
     st-metaClass-behavior ;; WHo's instances are meta-classes
     combined-metaClass-ivar-names  ;; child-ivar-names
     Class ;; class
     Class ;; super is really ClassDescription
 ) )

(perform:with: ClassClass     'thisClass:  Class)
(setClass:     ClassClass     MetaClass)
(perform:with: MetaClassClass 'thisClass:  MetaClass)
(perform:with: MetaClassClass 'superclass: ClassClass) ;; ClassDescription class

(setClass:     MetaClassClass  MetaClass) ;; Nota Bene!
(perform:with: MetaClass      'superclass: Class) ;; ClassDescription

;; Fake it until we fixup relations, below
(primAddSelector:withMethod:
     (behavior MetaClass)
     'allInstVarNames (lambda (self) combined-metaClass-ivar-names))

(primAddSelector:withMethod:
     (behavior MetaClassClass)
     'allInstVarNames (lambda (self) combined-class-ivar-names))

;;; OK. Scaffolding in place.
;;; Can now use #newSubclassName:iVars:cVars:

;;;The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make the metaClass
;;;   Then ask the metaClass to make its class

;; Helper. Create an instance of a Class or MetaClass
(define (instantiateName:superclass:ivars:
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
    (perform:with: newInst 'methodDict: newMethodDict)
    (primSetClass: newMethodDict newInst)
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

;;; Ask a class to create a new subclass
(define (newSubclassName:iVars:cVars:
         selfClass nameSym instanceVarsList classVarsList)
   ;; (when (hashtable-ref smalltalk-dictionary nameSym #f)
   ;;  (error "Class already exists" nameSym))
  (unless (and (symbol? nameSym)
               (let ( (name (symbol->string nameSym)) )
                 (and 
                  (> (string-length name) 1)
                  (char-upper-case? (string-ref name 0)))))
    (error "subclass name must be a symbol which starts uppercase" name))
  ;; (unless (or (string? category) (symbol? category))
  ;;       (error: "subclass name must be a string or symbol" category))
  (unless (and (list? instanceVarsList)
               (every? symbol? instanceVarsList))
    (error: "InstanceVariableNames must be a list of symbols" instanceVarsList))
  (unless (and (list? classVarsList)
               (every? symbol? classVarsList))
    (error "ClassVariableNames must be a list of symbols" classVarsList))
  (let* ( (newMetaClass
             (instantiateName:superclass:ivars:
                MetaClass
                (name->metaName nameSym)
                (class selfClass) ;;(perform: selfClass 'class)
                classVarsList))
          (newSubclass
             (instantiateName:superclass:ivars:
                newMetaClass
                nameSym
                selfClass
                instanceVarsList))
        )
    (perform:with: newMetaClass 'thisClass: newSubclass)
    (primSet:toValue: smalltalk-dictionary nameSym newSubclass)
    newSubclass		;; @@??@@ move initialize to here?
) )

;;; OK.  Now use protoClasses to bootstrap core classes


(define Object
  (newSubclassName:iVars:cVars:
   Class
   'Object '() '()))

(perform:with: Object 'superclass: st-nil) ;; ground case
(perform:with: (class Object) 'superclass: Class)
(perform:with: Object 'methodDict: st-object-behavior)

(define Behavior
  (newSubclassName:iVars:cVars:
   Object
   'Behavior '(superclass methodDict format) '()))

(define ClassDescription
  (newSubclassName:iVars:cVars:
   Behavior
   'ClassDescription '(instanceVariables organization) '()))

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

;;; Track which methods are added to a particular class
;;;  so they are not copied over from above.
;;  See #subclassAddSelector:withMethod: below
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (perform:with: self 'myMethodNames: (cons selector old-names))
    self
) )

(perform:with: Object 'myMethodNames: (selectors Object))

;;; Subclasses inherit mDict methods from their superclass
;;;  so adding a selector_method to a class affects
;;;  its instances, NOT the class instance itself.
(define (addSelector:withMethod: classSelf selector method)
  (add-method-name-to-myMethods classSelf selector) ;; def'ed here
  (subclassAddSelector:withMethod: classSelf selector method))

;;; NB: method added to methodDict of class
;;; => behavior of instances, not class itself !!
(define (subclassAddSelector:withMethod:
         classSelf selector method)
  (let* ( (mDict      (perform: classSelf 'methodDict))
          (subclasses (perform: classSelf 'subclasses))
        )
    (primAddSelector:withMethod: mDict selector method)
    (for-each
     (lambda (subClass)
       ;; if not overriden, copy down
       ;; Non-standard: avoids dynamic super-chain lookup
       (unless (memq selector (perform: subClass 'myMethodNames))
         (subclassAddSelector:withMethod: subClass selector method)))
     subclasses))
  classSelf
)

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
     'category: '|Kernel-Objects|)

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
     'category: '|Kernel-Classes|)

(perform:with:
     Behavior
     'comment:
"My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass)."
)

(perform:with:
     ClassDescription
     'category: '|Kernel-Classes|)

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
     'category: '|Kernel-Classes|)

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
     'category: '|Kernel-Classes|)

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



;;; Send to super
(define (superPerform: self selectorSym)
  ((lookupSelector: (superclass (class self)) selectorSym) self))

(define (superPerform:with: self selectorSym arg)
  ((lookupSelector: (superclass (class self)) selectorSym) self arg))

(define (superPerform:with:with: self selectorSym arg1 arg2)
  ((lookupSelector: (superclass (class self)) selectorSym) self arg1 arg2))

(define (superPerform:with:with:with: self selectorSym arg1 arg2 arg3)
  ((lookupSelector: (superclass (class self)) selectorSym) self arg1 arg2 arg3))

(define (superPerform:withArguments: self selectorSym argsArray)
  ;; @@FIXME: Check argsArry is a Smalltalk Array object..
  (apply (lookupSelector: (superclass (class self)) selectorSym)
         (cons self (cddr (vector->list argsArray)))))

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

;; (provide 'st-core-classes)

;;;			--- E O F ---			;;;
