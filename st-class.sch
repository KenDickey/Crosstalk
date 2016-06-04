;;; FILE: "st-class.sch"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: 16 May 2016

;; (require 'st-kernel)  ; message send
;; (require 'st-object)  ; Object behavior

;; Now that we have Object behavior, we can instantiate classes
;;  and bootstrap reflection via Classes, which are the management
;;  structure for object behaviors.

;; Basically, each class has a superclass (except Object) and
;;   each Class's class is a metaClass which is an instance of MetaClass.
;; An instance's shape (instance valiables) and behavior (instance methods)
;;   is defined in its class
;; A class's shape (class variables) and behavior (class methods)
;;   is defined in the instance class's metaClass 

;; We have behaviors, instances, classes, and metaClasses
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


;; The Smalltalk Global Environment
(define smalltalk-dictionary (make-eq-hashtable))

(define basic-class-instance-variable-names
  '(superclass methodDict format
    instanceVariables organization
    subclasses name
    ;; NB: pools are depricated !!
    category comment
    myMethodNames)
)

(define num-basic-class-ivars (length basic-class-instance-variable-names))

;; Object
;;   Behavior
;;      Class Descrption
;;         Class
;;         MetaClass

(define st-Object-behavior
  (clone-method-dictionary st-object-behavior))
(add-getters&setters st-Object-behavior
                     num-header-slots
                     basic-class-instance-variable-names)
(define Object ;; class Object
  (make-st-object st-Object-behavior
                  num-basic-class-ivars))

(perform:with: Object
               'myMethodNames:
               (primSelectors st-Object-behavior))

(define st-Behavior-behavior  ;; start with object methods
  (clone-method-dictionary st-Object-behavior))
(define Behavior
  (make-st-object st-Behavior-behavior num-basic-class-ivars))

(define st-ClassDescription-behavior
  (clone-method-dictionary st-Behavior-behavior))
(define ClassDescription
  (make-st-object st-ClassDescription-behavior num-basic-class-ivars))

(define st-Class-behavior
  (clone-method-dictionary st-ClassDescription-behavior))
(define Class
  (make-st-object st-Class-behavior num-basic-class-ivars))

(define st-MetaClass-behavior
  (clone-method-dictionary st-ClassDescription-behavior))
(add-getters&setters st-MetaClass-behavior
                     (+ num-header-slots num-basic-class-ivars)
                     '(thisClass)) ;; added
(define MetaClass
  (make-st-object st-MetaClass-behavior
                  (+ 2 num-header-slots num-basic-class-ivars)))
(perform:with: MetaClass
               'myMethodNames:
               '(thisClass thisClass:))


;; Register all Classes by name in the Smalltalk Dictionary

(hashtable-set! smalltalk-dictionary 'Object           Object)
(hashtable-set! smalltalk-dictionary 'Behavior         Behavior)
(hashtable-set! smalltalk-dictionary 'ClassDescription ClassDescription)
(hashtable-set! smalltalk-dictionary 'Class            Class)
(hashtable-set! smalltalk-dictionary 'MetaClass        MetaClass)

;; Class names
(perform:with: Object           'name: 'Object)
(perform:with: Behavior         'name: 'Behavior)
(perform:with: ClassDescription 'name: 'ClassDescription)
(perform:with: Class            'name: 'Class)
(perform:with: MetaClass        'name: 'MetaClass)

;; Superclasses & Subclasses
(perform:with: Object           'superclass: st-nil)
(perform:with: Object           'subclasses: (list Behavior))
(perform:with: Behavior         'superclass: Object)
(perform:with: Behavior         'subclasses: (list ClassDescription))
(perform:with: ClassDescription 'superclass: Behavior)
(perform:with: ClassDescription 'subclasses: (list Class MetaClass))
(perform:with: Class            'superclass: ClassDescription)
;;(perform:with: Class            'subclasses (list ...))
(perform:with: MetaClass        'superclass: ClassDescription)

;; behaviors are method dictionaries
(perform:with: Object           'methodDict: st-Object-behavior)
(perform:with: Behavior         'methodDict: st-Behavior-behavior)
(perform:with: ClassDescription 'methodDict: st-ClassDescription-behavior)
(perform:with: Class            'methodDict: st-Class-behavior)
(perform:with: MetaClass        'methodDict: st-MetaClass-behavior)

;; Each class inherits iVars and adds any of its own
(perform:with: Behavior         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription 'instanceVariables: '(instanceVariables organization))
(perform:with: Class            'instanceVariables: '(subclasses name category comment myMethodNames))
(perform:with: MetaClass        'instanceVariables: '(thisClass))

;; More structure mechanics

(define (allInstVarNames self)
  (let ( (ivarNames (perform: self 'instanceVariables))
         (super     (perform: self 'superclass))
       )
    (if (null? super)
        (list-copy ivarNames)
        (append (perform: super 'allInstVarNames) ivarNames))
) )

(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (perform:with: self 'myMethodNames: (cons selector old-names))
    self
) )


(define (addSelector:withMethod: aClass selector method)
  (let* ( (mDict      (perform: aClass 'methodDict))
          (subclasses (perform: aClass 'subclasses))
        )
    (primAddSelector:withMethod: mDict selector method)
    (add-method-name-to-myMethods aClass selector)
    (for-each
     (lambda (subClass)  ;; if not overriden, copy down
       (unless (memq selector (perform: subClass 'myMethodNames))
         (addSelector:withMethod: subClass selector method)))
     subclasses))
  aClass
)

;; Am I self-referential, or what??
(addSelector:withMethod: Object
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: Object
                         'allInstVarNames
                         allInstVarNames)

(addSelector:withMethod: MetaClass
                         'allInstVarNames
                         allInstVarNames)


;; Make a new instance of some class
(define (basicNew: classSelf num-indexed-vars)
  (let ( (num-named-vars
            (length
             (perform: classSelf 'allInstVarNames)))
       )
    (make-st-object
         (perform: classSelf 'methodDict)
         (+ num-named-vars num-indexed-vars))
) )

(addSelector:withMethod: Behavior
       'basicNew:
       basicNew:)
(addSelector:withMethod: Behavior
       'basicNew
       (lambda (self) (basicNew: self 0)))
(addSelector:withMethod: Behavior
       'new:
       (lambda (self size)
         (perform: (basicNew: self size) 'initialize)))
(addSelector:withMethod: Behavior
        'new
        (lambda (self)
          (perform: (basicNew: self 0) 'initialize)))

(addSelector:withMethod:
     Object
     'addSubclass:
     (lambda (self subclass)
       (perform:with: self 'subclasses:
                      (cons subclass
                            (perform: self 'subclasses))))
)

;; Add a metaClass to an "orphan" class
(define (addMetaClass:classVars: orphanClass classVarNames)
  (let* ( (metaName
              (string->symbol
               (string-append
                (symbol->string
                 (perform: orphanClass 'name))
                " class"))
          )
          (allClassVarNames
              (append (perform: MetaClass 'allInstVarNames)
                      classVarNames)
          )
          (theMeta
              (basicNew: MetaClass (length allClassVarNames)))
          (orphansSuper (superclass orphanClass))
          (metaSuper
              (if (null? orphansSuper)
                  '()
                  (class orphansSuper)))
        )
;;    (perform:with: theMeta 'instanceVariables: '())
    (perform:with: theMeta 'name: metaName)
    (primSetClass: theMeta  MetaClass)
    (perform:with: theMeta 'thisClass: orphanClass)
    (primSetClass: orphanClass theMeta)
    (perform:with: theMeta
                   'superclass:
                   (perform:
                    (perform: orphanClass 'superclass)
                    'class))
    (unless (null? metaSuper)
      (perform:with: metaSuper 'addSubclass: theMeta))
    (perform: theMeta 'initialize)
    theMeta
) )


;; ;; Ask a metaClass to create its class
;; (define (classFromMeta:name:ivars:superclass:category:
;;          metaClass name ivar-names superclass category)
;;   (let* ( (theClass (perform:with: metaClass 'basicNew: (length ivar-names))) )
;;     (perform:with: theClass 'name:       name)
;;     (perform:with: theClass 'instanceVariables: ivar-names)
;;     (primSetClass: theClass metaClass)
;;     (perform:with: theClass 'superclass: superclass)
;;     (perform:with: theClass 'category:   category)
;;     (perform:with: theClass 'thisClass:  theClass) -- done by #basicNew:
;;     (hashtable-set! smalltalk-dictionary name theClass)
;;     (perform: theClass 'initialize)
;;     theClass
;; ) )

;; Create an instance of a Class or MetaClass
(define (instantiateName:superclass:ivars:
         selfClass
         nameSymbol
         superClass
         ivarList)
  (let* ( (allIvars
             (append (perform: superClass 'allInstVarNames)
                     ivarList))
          (newInst
             (basicNew: selfClass (length allIvars)))
          (newBehavior
             (clone-method-dictionary (perform: selfClass 'methodDict)))
          (numAddedVars (length ivarList))
        )
    (unless (zero? numAddedVars)
      (let ( (start-index (- (vector-length newInst) numAddedVars)) )
        (add-getters&setters newBehavior start-index ivarList)))
    (primSetClass:  newInst selfClass)
    (perform:with:
       newInst
       'name:       nameSymbol)
    (perform:with:
       newInst
       'superclass: superClass)
    (perform:with:
       newInst 
       'instanceVariables: (list-copy ivarList))
    (perform:with:
       newInst
       'methodDict:
       newBehavior)
    (perform:with:
       superClass
       'addSubclass: newInst)
;;@@DEBUG{
    ;; (describe newInst)
    ;; (display-ivars newInst)
;;}DEBUG@@
    (perform: newInst 'initialize)  ;; NB: should always return newInst !!
) )

(define (name->metaName nameSym)
  (string->symbol
   (string-append
    (symbol->string nameSym)
    " class")))

(define (newSubclassName:iVars:cVars:
         selfClass nameSym instanceVarsList classVarsList)
   (when (hashtable-ref smalltalk-dictionary nameSym #f)
    (error: "Class already exists" nameSym))
  (unless (and (symbol? nameSym)
               (let ( (name (symbol->string nameSym)) )
                 (and 
                  (> (string-length name) 1)
                  (char-upper-case? (string-ref name 0)))))
    (error: "subclass name must be a string which starts uppercase" name))
  ;; (unless (or (string? category) (symbol? category))
  ;;       (error: "subclass name must be a string or symbol" category))
  (unless (and (list? instanceVarsList)
               (every? symbol? instanceVarsList))
    (error: "InstannceVariableNames must be a list of symbols" instanceVarsList))
  (unless (and (list? classVarsList)
               (every? symbol? classVarsList))
    (error: "ClassVariableNames must be a list of symbols" classVarsList))
  (let* ( (newMetaClass
             (instantiateName:superclass:ivars:
                MetaClass
                (name->metaName nameSym)
                (class selfClass) ;;(perform: selfClass 'class)
                classVarsList))
          (newSubClass
             (instantiateName:superclass:ivars:
                newMetaClass
                nameSym
                selfClass
                instanceVarsList))
        )
    (hashtable-set! smalltalk-dictionary nameSym newSubClass)
    newSubClass		;; @@??@@ move initialize to here?
) )

;; <Class> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
;; Ask a class to create a new subclass
;; (define (subclass:instanceVariableNames:classVariableNames:category:
;;          self name ivars-list classvars-list category)
;;   (when (hashtable-ref smalltalk-dictionary name #f)
;;     (error: "Class already exists" name))
;;   (unless (and (string? name)
;;                (> (string-length name) 1)
;;                (char-upper-case? (string-ref name 0)))
;;     (error: "subclass name must be a string which starts uppercase" name))
;;   (unless (or (string? category) (symbol? category))
;;         (error: "subclass name must be a string or symbol" category))
;;   (unless (and (list? ivars-list)
;;                (every? symbol? ivars-list))
;;     (error: "InstannceVariableNames must be a list of symbols" ivars-list))
;;   (unless (and (list? classvars-list)
;;                (every? symbol? classvars-list))
;;     (error: "ClassVariableNames must be a list of symbols" classvars-list))
  
;;   (let* ( (metaName  (string-append name " class"))
;;           (allClassvarNames
;;              (append (perform: MetaClass 'allInstVarNames) classvars-list))
;;           (metaClass
;;              (perform:with: MetaClass 'basicNew: (length allClassvarNames)))
;;           (allIvarNames (append (perform: self 'allInstVarNames) ivars-list))
;;         )
;;     ;; set up metaClass
;;     (perform:with: metaClass 'instanceVariables: classvars-list)
;;     (perform:with: metaClass 'name:      metaName)
;;     (primSetClass: metaClass MetaClass)
;;     (perform:with: metaClass 'thisClass: subClass) ;; done in #basicNew:
;;     (perform:with: metaClass 'superclass: (perform: self 'class))
;;     (perform: metaClass 'initialize)

;;     ;; self
;;     (let ( (subClass
;;               (classFromMeta:name:ivars:superclass:category:
;;                    metaClass name allIvarNames self category))
;;          )
;;       (hashtable-set! smalltalk-dictionary metaName metaClass)
;;       (hashtable-set! smalltalk-dictionary name     subClass)
;; ;;      (perform: subClass 'initialize)
;; ;;	  -- done in #classFromMeta:name:ivars:superclass:category:
;;       (perform:with: self 'addSubclass: subClass)
;;       ;; return value:
;;       subClass)
;; ) )

;; (addSelector:withMethod:
;;     Object
;;     'subclass:instanceVariableNames:classVariableNames:category:
;;      subclass:instanceVariableNames:classVariableNames:category:)

;; OK, now for the meta-class hierarchy..

(addMetaClass:classVars: Object    basic-class-instance-variable-names)
(addMetaClass:classVars: Behavior  basic-class-instance-variable-names)
(addMetaClass:classVars: ClassDescription basic-class-instance-variable-names)
(addMetaClass:classVars: Class     basic-class-instance-variable-names)
(addMetaClass:classVars: MetaClass (append basic-class-instance-variable-names '(thisClass)))

(perform:with: Class 'subclasses: (list (perform: Object 'class)))
(perform:with: (perform: Object 'class) 'superclass: Class)

;; MetaClass class class == MetaClass
;; Ouch!

(define (class      obj) (perform: obj 'class))
(define (superclass obj) (perform: obj 'superclass))

(let* ( (metaclassClass (class MetaClass))
        (cloneDict
         (clone-method-dictionary
          (st-obj-behavior metaclassClass)))
      )
  ;; (behavior MetaClass) == (behavior (class MetaClass)
  ;; Change this.
  (st-obj-behavior-set! metaclassClass cloneDict)
  ;; Now we can set the 'class method
  (primAddSelector:withMethod: cloneDict
                               'class
                               (lambda (self) MetaClass))
)

;;; Now we have the desired circular relationship:

;; > (describe (class MetaClass))
;; "MetaClass class" is an instance of class #'MetaClass'

;; > (describe MetaClass)
;; "MetaClass" is an instance of class #'MetaClass class'

;;; Note Also:
;; > (describe (superclass MetaClass))
;; "ClassDescription" is an instance of class #'ClassDescription class'

;; > (describe (superclass (class MetaClass)))
;; "ClassDescription class" is an instance of class #'MetaClass class'

;; (provide 'st-class)

;;;			--- E O F ---			;;;
