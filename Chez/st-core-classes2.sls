#!r6rs
;;; FILE: "st-core-classes2.sls"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-core-classes2)

  (export
   ClassClass
   Class
   MetaClassClass
   MetaClass
   Object
   Behavior
   ClassDescription
   Collection
   Set
   IdentitySet
   SequenceableCollection
   ArrayedCollection
   Array
   ByteArray
   )
  
  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs bytevectors (6))
   (rnrs io simple (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs sorting (6))
   (rnrs hashtables (6))
   (only (chezscheme)
         format
         vector-copy
       )
   (st-kernel)
   (st-core-classes)
   )

;;; Scaffolding setup
;; Just enough behavior to allow instantiation bootstrap
;; to call: newSubclassName:iVars:cVars:

(define broken #f)

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
     (string->symbol "Class class") ; name
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
     (string->symbol "MetaClass class")
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

;; Helper
(define (every? proc list)
  (let loop ( (remainder list) )
    (cond
     ((null? remainder) #t)
     ((proc (car remainder)) (loop (cdr remainder)))
     (else #f))
) )

;;;The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make the metaClass
;;;   Then ask the metaClass to make its instance

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
         selfClass nameSym instanceVars classVars)
   ;; (when (hashtable-ref Smalltalk nameSym #f)
   ;;  (error "Class already exists" nameSym))
  (unless (and (symbol? nameSym)
               (let ( (name (symbol->string nameSym)) )
                 (and 
                  (> (string-length name) 1)
                  (char-upper-case? (string-ref name 0)))))
    (error
     'newSubclassName:iVars:cVars:
     "subclass name must be a symbol which starts uppercase"
     nameSym))
  ;; (unless (or (string? category) (symbol? category))
  ;;       (error "subclass name must be a string or symbol" category))
  (unless (or (list? instanceVars) (vector? instanceVars))
    (error 'newSubclassName:iVars:cVars:
           "InstanceVariableNames must be a list or array of symbols"
           instanceVars))
  (unless (or (list? classVars) (vector? classVars))
    (error 'newSubclassName:iVars:cVars:
           "ClassVariableNames must be a list or array of symbols"
           classVars))
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
      (for-each ;; give instances access to class vars
        (lambda (getter-name)
          (let* ( (setter-name
                   (string->symbol
                    (string-append
                     (symbol->string getter-name) ":")))
                )
            (addSelector:withMethod:
               newSubclass
               getter-name
               (lambda (self)
                 (perform: (class self) getter-name)))
            (addSelector:withMethod:
               newSubclass
               setter-name
               (lambda (self newVal)
                 (perform:with: (class self) setter-name newVal)))
        ) )
        classVarsList)
      (perform:with: newMetaClass 'thisClass: newSubclass)
      (smalltalkAt:Put: nameSym newSubclass)
      newSubclass		;; @@??@@ move initialize to here?
) ) )

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


;;; Track which methods are added to a particular class
;;;  so they are not copied over from above.
;;  See #subclassAddSelector:withMethod: below
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (perform:with: self 'myMethodNames: (cons selector old-names))
    self
) )


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
  (let* ( (mDict  ($ classSelf 'methodDict))
          (subs   ($ classSelf 'subclasses))
        )
    (primAddSelector:withMethod: mDict selector method)
    (for-each
       (lambda (subClass)
       ;; if not overriden, copy down
       ;; Non-standard: avoids dynamic super-chain lookup
         (unless (memq selector (perform: subClass 'myMethodNames))
           (subclassAddSelector:withMethod: subClass selector method)))
       subs))
  classSelf
)

;;; Set, Collection, Array, ..

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

(define SequenceableCollection
  (newSubclassName:iVars:cVars:
   Collection
   'SequenceableCollection '() '())
)

(define ArrayedCollection
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'ArrayedCollection '() '())
)

(define Array 
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'Array '() '())
)

;;(define (vector-for-each proc vec)
;;  (for-each proc (vector->list vec)))

(define ByteArray
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'ByteArray '() '())
)

(define (bytevector-for-each proc bvec)
  (let ( (size (bytevector-length bvec)) )
    (let loop ( (index 0) )
      (when (< index size)
        (proc (bytevector-ref bvec index))
        (loop (+ 1 index)))
) ) )

(define bytevector-ref  bytevector-u8-ref)
(define bytevector-set! bytevector-u8-set!)


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

;;; Make proper linkages

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
(smalltalkAt:Put: 'Class Class)
(smalltalkAt:Put: 'MetaClass MetaClass)

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
                                            nil)))
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
                                            nil)))
                   ($: iSet 'add: k)))
                ;; else must be local; add selector
                (else ($: iSet 'add: k)))))
         iSet
      ) )
)

;;; Set; Identity Set

(perform:with:
     Set
     'category:
     'Collections-Unordered)

(perform:with:
     Set
     'comment:
"I am an unordered collection of non-nil objects
 which does not contain duplicates."
)

(perform:with:
     IdentitySet
     'category:
     'Collections-Unordered)

(perform:with:
     IdentitySet
     'comment:
"I am the same as a Set,
 but my comparisons are with #== not #="
)

(addSelector:withMethod:
     Set
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Set)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     (class Set)
     'new:
     (lambda (self size)
       (let ( (newInst (perform: self 'new)) )
         (perform:with: newInst 'init: size)
         newInst)))

(addSelector:withMethod:
     Set
     'initialize
     (let ( (defaultSize 4) )
       (lambda (self)
         (perform:with: self 'init: defaultSize)
)    ) )

(addSelector:withMethod:
     Set
     'init:
     (lambda (self size)
       ;; make large enough to hold size elts
       ;; without growing -- see #fullCheck
       (let ( (initialSize
               (if (<= size 0)
                   1
                   (floor (/ (* (+ size 1) 4) 3))))
            )
         (superPerform: self 'initialize)
         (perform:with: self 'tally: 0)
         (perform:with: self
                        'array:
                        (perform:with: Array 'new: initialSize))
         self))
)

(addSelector:withMethod:
     (class Set)
     'with:
     (lambda (self elt1)
       (let ( (aSet (perform:with: Set 'new: 1)) )
         (perform:with: aSet 'add: elt1)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:
     (lambda (self elt1 elt2)
       (let ( (aSet (perform:with: Set 'new: 2)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:with:
     (lambda (self elt1 elt2 elt3)
       (let ( (aSet (perform:with: Set 'new: 3)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         (perform:with: aSet 'add: elt3)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:with:with:
     (lambda (self elt1 elt2 elt3 elt4)
       (let ( (aSet (perform:with: Set 'new: 4)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         (perform:with: aSet 'add: elt3)
         (perform:with: aSet 'add: elt4)
         aSet)))

(addSelector:withMethod:
     Set
     'fullCheck  ;; private
     (lambda (self)
       ;; Keep array at least 1/4 free for
       ;; better hash behavior
       (let ( (array-size
               (perform: (perform: self 'array)
                         'size))
              (tally (perform: self 'tally))
            )
         (unless (> (- array-size tally)
                    (max 1 (floor (/ array-size 4))))
           (perform: self 'grow)))))

(addSelector:withMethod:
     Set
     'grow ;; private
     (lambda (self)
       (let* ( (old-array  (perform: self 'array))
               (array-size (perform: old-array 'size))
               (new-size (+ array-size (max array-size 2)))
               (new-array
                  (perform:with: Array 'new: new-size))
             )
         (perform:with: self 'array: new-array)
         (perform:with: self 'tally: 0)
         (perform:with: old-array
                        'do:
                        (lambda (elt)
                          (unless (st-nil? elt)
                            (perform:with: self
                                           'noCheckAdd:
                                           elt))))
         self)))

(addSelector:withMethod:
     Set
     'noCheckAdd: ;; private -- obj not a duplicate
     (lambda (self elt)
       (let ( (index (perform:with: self 'findElementOrNil: elt)) )
         (perform:with:with:
              (perform: self 'array)
              'at:put: index elt)
         (perform:with: self 'tally:
                        (+ 1 (perform: self 'tally)))
         self)))

(addSelector:withMethod:
     Set
     'scanFor:
     (lambda (self obj)
       ; Scan key array for 1st slot containing nil
       ; or an element matching obj.  Answer index or zero.
       ; Subclasses may override me for different match predicates.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (mod (equal-hash obj) ;; hash fn
                               array-size))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
;;             (newline) (display index)
             (cond
              ((st-nil? elt)    (+ 1 index)) ;; Scheme->ST index
              ((equal? obj elt) (+ 1 index)) ;; Scheme->ST index ;; equal?
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
  ;;                 (newline) (display index)
                   (let ( (elt (vector-ref array index)) )
                     (cond
                      ((st-nil? elt)    (+ 1 index))
                      ((equal? obj elt) (+ 1 index))
                      ((= index mid-end)
                       0) ;; failed
                      (else (left-loop (+ 1 index))))))
               ))
              (else (right-loop (+ 1 index)))))))))


(addSelector:withMethod:
     Set
     'findElementOrNil: ;; private
     (lambda (self obj)
       ;; Answer first nil (empty) slot or
       ;; slot which contains obj
       ;; or 0.
       (let ( (index (perform:with: self 'scanFor: obj)) )
         (if (> index 0)
             index
             (error 'addSelector:withMethod:
                    "Internal error: No free space in set!"
                    self)))))

(addSelector:withMethod:
     Set
     'swap:with:
     (lambda (self oneIndex anotherIndex)
       (perform:with:with:
          (perform: self 'array)
          'swap:with: oneIndex anotherIndex)))

(addSelector:withMethod:
     Set
     'size ;; number of elements
     (lambda (self) (perform: self 'tally)))

(addSelector:withMethod:
     Set
     'do:
     (lambda (self aBlock)
       (if (zero? (perform: self 'tally))
           self
           (perform:with: (perform: self 'array)
                          'do:
                          (lambda (elt)
                            (unless (st-nil? elt)
                              (aBlock elt)))
       )  )
) )

(addSelector:withMethod:
     Set
     '=
     (lambda (self otherSet)
       (call/cc
        (lambda (return)
          (unless (perform:with: otherSet
                                 'isKindOf:
                                 Set)
            (return st-false))
          (unless (equal? (perform: self     'tally)
                          (perform: otherSet 'tally))
            (return st-false))
          (perform:with: self
                         'do:
                         (lambda (elt)
                           (unless (perform:with:
                                    otherSet
                                    'includes:
                                    elt)
                             (return st-false))))
          (return st-true)))))

(addSelector:withMethod:
     Set
     'includes:
     (lambda (self obj)
       (let ( (index
                 (perform:with: self 'findElementOrNil: obj))
            )
         (not (st-nil? (perform:with: self 'keyAt: index))))
)   )

(addSelector:withMethod:
     Set
     'occurrencesOf:
     (lambda (self elt)
       (if ($: self 'includes: elt)
           1
           0)))

(addSelector:withMethod:
     Set
     'keyAt:
     (lambda (self index)
       (perform:with:
          (perform: self 'array) 'at: index)))

(addSelector:withMethod:
     Set
     'addAll:
     (lambda (self aCollection)
       ($: aCollection
           'do:
           (lambda (elt) ($: self 'add: elt)))
       self))


(addSelector:withMethod:
     Set
     'add:
     (lambda (self newObj)
       (when (st-nil? newObj)
         (error
          'addSelector:withMethod:
          "Set's can't meaningly contain nil as an element"))
       (let ( (index (perform:with: self
                                    'findElementOrNil:
                                    newObj))
            )
         (when (st-nil? (perform:with: self 'keyAt: index))
           (perform:with:with:
               self
               'atNewIndex:put: index newObj))
         ;; else obj already present..
         newObj)))

(addSelector:withMethod:
     Set
     'atNewIndex:put:
     (lambda (self index obj)
       (perform:with:with:
          (perform: self 'array) 'at:put: index obj)
       (perform:with: self
                      'tally:
                      (+ 1 (perform: self 'tally)))
       (perform: self 'fullCheck)
       self))

(addSelector:withMethod:
     Set
     'remove:ifAbsent:
     (lambda (self oldObj absentBlock)
       (let ( (index (perform:with: self
                                    'findElementOrNil:
                                    oldObj))
              (array (perform: self 'array))
            )
       (if (st-nil? (perform:with: self 'keyAt: index))
           (absentBlock)
           (begin
             (perform:with:with:
              (perform: self 'array) 'at:put: index st-nil)
             (perform:with: self
                           'tally:
                           (- (perform: self 'tally) 1))
             (perform:with: self 'fixCollisionsFrom: index)
             oldObj)))))
                   
(addSelector:withMethod:
     Set
     'fixCollisionsFrom:
     (lambda (self oldIndex)
       ;; Removed elt at (ST) index.
       ;; Now relocate entries displaced by hash
       ;; collision at this index
       (let* ( (length (perform: (perform: self 'array) 'size))
               (fixupIndex
                 (if (= oldIndex length) 1 (+ 1 oldIndex)))
             )
         (let loop ( (oldIndex fixupIndex)
                     (elt (perform:with: self 'keyAt: fixupIndex)) )
           (unless (st-nil? elt)
             (let ( (newIndex (perform:with: self 'findElementOrNil: elt)) )
               (unless (= newIndex oldIndex)
                 (perform:with:with: self 'swap:with: oldIndex newIndex))
               (loop newIndex (perform:with: self 'keyAt: fixupIndex))))))     
)    )

(addSelector:withMethod:
     Set
     'collect:
     (lambda (self aBlock)
       (let ( (new-set (perform:with: (class self) ;NB: subclass may invoke
                                      'new:
                                      (perform: self 'size)))
              (array (perform: self 'array))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: new-set 'add: (aBlock elt))))
          array)
         new-set)))

(addSelector:withMethod:
     Set
     'copy
     (lambda (self)
       (let ( (the-copy (perform: (class self) 'new)) )
         (perform:with: the-copy 'tally: (perform: self 'tally))
         (perform:with: the-copy 'array:
                        (vector-copy (perform: self 'array)))
         the-copy)))

(addSelector:withMethod:
     Set
     'asArray
     (lambda (self)
       (let ( (elts '()) )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (set! elts (cons elt elts))))
          (perform: self 'array))
       (list->vector elts))))

(addSelector:withMethod:
     Array
     'asSet
     (lambda (self)
       (let ( (newSet
               (perform:with: Set
                              'new: (vector-length self)))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: newSet 'add: elt)))
          self)
         newSet)))

(addSelector:withMethod:
     Array
     'asIdentitySet
     (lambda (self)
       (let ( (newSet
               (perform:with: IdentitySet
                              'new: (vector-length self)))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: newSet 'add: elt)))
          self)
         newSet)))

(addSelector:withMethod:
     Set
     'scanFor:
     (lambda (self obj)
       ; Scan key array for 1st slot containing nil
       ; or an element matching obj.  Answer index or zero.
       ; Subclasses may override me for different match predicates.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (mod (equal-hash obj) ;; hash fn
                           array-size))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
;;             (newline) (display index)
             (cond
              ((st-nil? elt) (+ 1 index)) ;; Scheme->ST index
              ((eq? obj elt) (+ 1 index)) ;; Scheme->ST index ;; eq?
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
  ;;                 (newline) (display index)
                   (let ( (elt (vector-ref array index)) )
                     (cond
                      ((st-nil? elt) (+ 1 index))
                      ((eq? obj elt) (+ 1 index))
                      ((= index mid-end)
                       0) ;; failed
                      (else (left-loop (+ 1 index))))))
               ))
              (else (right-loop (+ 1 index)))))))))

;;; Collections


(perform:with:
     Collection
     'comment:
"I am the abstract superclass of all classes that represent a group of elements."
)

(perform:with:
     Collection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     Collection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Collection)
           (superPerform:with: self 'is: symbol))))


(perform:with:
     SequenceableCollection
     'comment:
"I am an abstract superclass for collections that have a well-defined order
 associated with their elements. Thus each element is externally-named by
 integers referred to as indices."
)

(perform:with:
     SequenceableCollection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     SequenceableCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'SequenceableCollection)
           (superPerform:with: self 'is: symbol))))


(perform:with:
     ArrayedCollection
     'comment:
"I am an abstract collection of elements with a fixed range
 of integers (from 1 to n>=0) as external keys."
)

(perform:with:
     ArrayedCollection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     ArrayedCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ArrayedCollection)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Collection
     'printElementsOn:
     (lambda (self port)
       (display "( " port)
       (perform:with: self
                      'do:
                      (lambda (elt)
                        (perform:with:
                           elt 'printOn: port)
                        (display " " port)))
       (display ")" port)))


(addSelector:withMethod:
     SequenceableCollection
     'withIndexDo:
     (lambda (self elementAndIndexBlock)
;; "Just like with:do: except that the iteration index
;;   supplies the second argument to the block."
       (let ( (limit ($ self 'size)) )
         (let loop ( (index 1) )
           (when (<= index limit)
             (elementAndIndexBlock
                 ($: self 'at: index)
                 index)
             (loop (+ index 1)))))))

(addSelector:withMethod:
     SequenceableCollection
     'beginsWith:
     (lambda (self aSequenceableCollection)
       (if (or ($ aSequenceableCollection 'isEmpty)
               (< ($ self 'size)
                  ($ aSequenceableCollection 'size)))
           st-false
           (let ( (max ($ aSequenceableCollection 'size)) )
             (let loop ( (index 1) )
               (cond
                ((> index max)
                 st-true) ;; all match
                (($: ($: self 'at: index)
                     '=
                     ($: aSequenceableCollection 'at: index))
                 (loop (+ index 1)))
                (else st-false)))
        ) ) )
)

($:: (smalltalkAt: 'SequenceableCollection)
     'addSelector:withMethod:
     'endsWith:
     (lambda (self aSequenceableCollection)
       (call/cc
         (lambda (return)
           (let ((start nil))
             ($: ($: ($ aSequenceableCollection 'isEmpty)
                     'or:
                     (lambda ()
                       ($: ($ self 'size)
                           '<
                           ($ aSequenceableCollection 'size))))
                 'ifTrue:
                 (lambda () (return false)))
             (let ((%%val%%
                     ($: ($ self 'size)
                         '-
                         ($ aSequenceableCollection 'size))))
               (set! start %%val%%)
               %%val%%)
             ($: aSequenceableCollection
                 'withIndexDo:
                 (lambda (each index)
                   ($: ($: ($: self 'at: ($: start '+ index)) '~= each)
                       'ifTrue:
                       (lambda () (return false)))))
             (return true))))))


)

;; Arrays


(perform:with:
     Array
     'comment:
"I present an ArrayedCollection whose elements are objects."
)

(perform:with:
     Array
     'category: 'Collections-Arrayed)

(addSelector:withMethod:
     Array
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Array)
           (superPerform:with: self 'is: symbol))))


;; Scheme Vectors
;;(set! st-array-behavior (perform: Array 'methodDict))

;; #at: #at:put: #basicSize #at:modify
(add-array-accessors st-array-behavior 0)

(addSelector:withMethod:
     Array
     'size
     (lambda (self)
       (vector-length self)))

(addSelector:withMethod:
     Array
     'basicSize
     (lambda (self)
       (vector-length self)))

(addSelector:withMethod:
     (class Array)
     'basicNew:
     (lambda (self size)
       (make-vector size st-nil)))

(addSelector:withMethod:
     (class Array)
     'new:
     (lambda (self size)
       (perform: (perform:with: self 'basicNew: size)
                 'initialize)))

(addSelector:withMethod:
     (class Array)
     'new
     (lambda (self)
       (make-vector 0)))

(addSelector:withMethod:
     (class Array)
     'with:
     (lambda (self anObject)
       (vector anObject)))

(addSelector:withMethod:
     (class Array)
     'with:with:
     (lambda (self obj1 obj2)
       (vector obj1 obj2)))

(addSelector:withMethod:
     (class Array)
     'with:with:with:
     (lambda (self obj1 obj2 obj3)
       (vector obj1 obj2 obj3)))

(addSelector:withMethod:
     (class Array)
     'with:with:with:with:
     (lambda (self obj1 obj2 obj3 obj4)
       (vector obj1 obj2 obj3 obj4)))


(addSelector:withMethod:
     (class Array)
     'withAll:
     (lambda (self aCollection)
       (let ( (elts st-nil) )
         (perform:with aCollection
                       'do:
                       (lambda (elt)
                         (set! elts (cons elt elts))))
         (list->vector (reverse elts)))))

(addSelector:withMethod:
     Array
     'do:
     (lambda (self aBlock)
       (vector-for-each aBlock self)
       self))

(addSelector:withMethod:
     Array
     'printOn:
     (lambda (self port)
       (display "#( " port)
       (vector-for-each
        (lambda (each)
          ($: each 'printOn: port)
          (display " " port))
        self)
       (display ")" port))
)


(addSelector:withMethod:
     Array
     'select:
     (lambda (self predicate?)
       (let ( (results '()) )
         (vector-for-each
          (lambda (each)
            (when (predicate? each)
              (set! results (cons each results))))
          self)
         (list->vector (reverse results))))
)

(addSelector:withMethod:
     Array
     'detect:  ;; here for testing
     (lambda (self predicate?)
       (let ( (myLen (vector-length self))
              (result #f)
            )
         (let loop ( (index 0) )
           (when (< index myLen)
            (if (predicate? (vector-ref self index))
                (set! result #t)
                (loop (+ 1 index)))))
         result))
)

(addSelector:withMethod:
     Array
     'asArray
     (lambda (self) ;; called by subclasses
       (if (eq? (class self) Array)
           self
           (superPerform:with: self 'asArray))))

(addSelector:withMethod:
     Array
     'asString
     (lambda (self)
       (list->string
        (map integer->char (vector->list self)))))

(addSelector:withMethod:
     Array
     'swap:with:
     (lambda (self oneIndex anotherIndex)
       (let* ( (index1 (- oneIndex 1)) ;; Scheme 0-based
               (index2 (- anotherIndex 1)) ;; ST 1 based
               (elt1 (vector-ref self index1))
               (elt2 (vector-ref self index2))
             )
         (vector-set! self index2 elt1)
         (vector-set! self index1 elt2)
         self)))

;;; Bytevector

(addSelector:withMethod:
     ByteArray
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ByteArray)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     ByteArray
     'comment:
"I present an ArrayedCollection whose elements are integers between 0 and 255."
)

(perform:with:
     ByteArray
     'category: 'Collections-Arrayed)

;; Scheme bytevectors
;;(set! st-bytevector-behavior (perform: ByteArray 'methodDict))

(addSelector:withMethod:
     (class ByteArray)
     'basicNew:
     (lambda (self size)
       (make-bytevector size 0)))

(addSelector:withMethod:
     (class ByteArray)
     'new:
     (lambda (self size)
       (perform:with: self 'basicNew: size)))

(addSelector:withMethod:
     (class ByteArray)
     'new 
     (lambda (self)
       (perform:with: self 'basicNew: 0)))

(addSelector:withMethod:
     (class ByteArray)
     'withAll:
     (lambda (self aCollection)
       (let* ( (size (perform: self 'size))
               (newByteArray (make-bytevector size 0))
               ;; cache constant method
               (at: (primLookup: (behavior aCollection) 'at:))
             )
         (let loop ( (index 0) )
           (when (< index size) ;; Scheme 0 based
             (bytevector-set!
                  newByteArray
                  index
                  (at: aCollection (+ 1 index))) ;; ST 1 based
             (loop (+ index 1)))
             newByteArray))))

(addSelector:withMethod:
     ByteArray
     'at:
     (lambda (self index)
       ;; NB: ST 1-based, Scheme 0-based
       (if (<= 1 index (bytevector-length self))
           (bytevector-ref self (- index 1))
           (error 'at:
                  "Index out of range"
                  self
                  index))))
     
(addSelector:withMethod:
     ByteArray
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-set! self (- index 1) newVal)
           (error 'at:put: "Index out of range" self index))))

(addSelector:withMethod:
     ByteArray
     'size 
     (lambda (self)
    ;; (perform: self 'basicSize)
       (bytevector-length self)))

(addSelector:withMethod:
     ByteArray
     'basicSize
     (lambda (self)
       (bytevector-length self)))

(addSelector:withMethod:
     ByteArray
     'printOn:
     (lambda (self port)
       (display "#[ " port)
       (bytevector-for-each
        (lambda (each)
          ($: each 'printOn: port)
          (display " " port))
        self)
       (display "]" port))
)

(addSelector:withMethod:
     ByteArray
     'do:
     (lambda (self aBlock)
       (bytevector-for-each aBlock self)
       self))


(addSelector:withMethod:
     ByteArray
     'asByteArray
     (lambda (self) self))

(addSelector:withMethod:
     ByteArray
     'asString
     (lambda (self)
       (let* ( (strLen (bytevector-length self))
               (result (make-string strLen #\space))
             )
         (let loop ( (index 0) )
           (when (< index strLen)
             (string-set! result
                          index
                          (integer->char (bytevector-ref self index)))
             (loop (+ index 1))))
         result))

)
