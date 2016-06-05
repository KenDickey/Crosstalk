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

(define st-Object-behavior ;; Object Class instance behavior
  (clone-method-dictionary st-object-behavior))

(add-getters&setters st-Object-behavior ;; NB: the Object Class, not instance
                     num-header-slots
                     basic-class-instance-variable-names)

(define Object ;; class Object
  (make-st-object st-Object-behavior
                  num-basic-class-ivars))

(perform:with: Object
               'myMethodNames:
               (primSelectors st-Object-behavior))


;; OK. Create core class instances (structure)

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
(define MetaClass
  (make-st-object st-MetaClass-behavior
                  (+ 1 num-header-slots num-basic-class-ivars)))
(add-getters&setters st-MetaClass-behavior
                     (+ num-header-slots num-basic-class-ivars)
                     '(thisClass)) ;; added
(perform:with: MetaClass
               'myMethodNames:
               '(thisClass thisClass:))


;; Register all Classes by name in the Smalltalk Dictionary

(hashtable-set! smalltalk-dictionary 'Object           Object)
(hashtable-set! smalltalk-dictionary 'Behavior         Behavior)
(hashtable-set! smalltalk-dictionary 'ClassDescription ClassDescription)
(hashtable-set! smalltalk-dictionary 'Class            Class)
(hashtable-set! smalltalk-dictionary 'MetaClass        MetaClass)

;; Class names are symbols
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

;; behaviors are eq? among all instances and the methodDict of the class
;; (behavior instance) == (methodDict (class instance))
(perform:with: Object
               'methodDict: st-object-behavior) ;; NOT st-Object-behavior !!
(perform:with: Behavior
               'methodDict: (clone-method-dictionary st-object-behavior))
(perform:with: ClassDescription
               'methodDict: (clone-method-dictionary st-object-behavior))
(perform:with: Class
               'methodDict: (clone-method-dictionary st-object-behavior))
(perform:with: MetaClass
               'methodDict: (clone-method-dictionary st-object-behavior))

;; Each class inherits iVars and adds any of its own
(perform:with: Behavior         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription 'instanceVariables: '(instanceVariables organization))
(perform:with: Class            'instanceVariables: '(subclasses name category comment myMethodNames))
(perform:with: MetaClass        'instanceVariables: '(thisClass))

;; Below basicNew: Make a new instance of some class
(define (primNew: classSelf num-object-slots)
  (make-st-object
   (perform: classSelf 'methodDict)
   (+ num-header-slots num-object-slots)
) )

;; Add a metaClass to an "orphan" class
(define (primAddMetaClass: orphanClass)
  (let* ( (metaName
              (string->symbol
               (string-append
                (symbol->string
                 (perform: orphanClass 'name))
                " class"))
          )
          (allClassVarNames
              (append basic-class-instance-variable-names
                      '(thisClass))
          )
          (theMeta
              (primNew: MetaClass (length allClassVarNames)))
          (start-index ;; for added slot(s)
           ;; All slots are added here, so just skip header
              num-header-slots)
          (orphansSuper (perform: orphanClass 'superclass))
          (metaSuper
              (if (null? orphansSuper) ;; ==> Object
                  Class  ;; Nota Bene!!
                  (perform: orphansSuper 'class)))
        )
    (add-getters&setters (behavior theMeta) start-index allClassVarNames)
    (perform:with: theMeta 'name: metaName)
    (primSetClass: theMeta  MetaClass)
    (perform:with: theMeta 'thisClass: orphanClass)
    (primSetClass: orphanClass theMeta)
    (perform:with: theMeta
                   'superclass:
                   (perform:
                    (perform: orphanClass 'superclass)
                    'class))
    (perform:with: theMeta
                   'methodDict:
                   (behavior orphanClass))
    (unless (null? metaSuper) ;; addSubclass: not yet def'ed
      (perform:with: metaSuper 'subclasses:
                      (cons theMeta
                            (perform: metaSuper 'subclasses))))
    (perform: theMeta 'initialize)
    theMeta
) )


;;; OK, now for the meta-class hierarchy..

(primAddMetaClass: Object)
(primAddMetaClass: Behavior)
(primAddMetaClass: ClassDescription)
(primAddMetaClass: Class)
(primAddMetaClass: MetaClass)

(perform:with: Class 'subclasses: (list (perform: Object 'class)))
(perform:with: (perform: Object 'class) 'superclass: Class)

;; shortcuts
(define (class      obj) (perform: obj 'class))
(define (superclass obj) (perform: obj 'superclass))



;;; MetaClass class class == MetaClass
;;; Ouch!

(let* ( (metaclassClass (class MetaClass))
        (cloneDict
         (clone-method-dictionary
          (st-obj-behavior metaclassClass)))
      )
  ;; (behavior MetaClass) == (behavior (class MetaClass)
  ;; Change this.
  (st-obj-behavior-set! metaclassClass cloneDict)
  ;; Set the 'class method for 'MetaClass class'
  ;;  nbut now without mutating others..
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


;;; More structure mechanics

;;;(allInstVarNames st-obj)
;; Need metaclass's set up before able to imvoke this
(define (allInstVarNames self)
  (let ( (ivarNames (perform: self 'instanceVariables))
         (super     (perform: self 'superclass))
       )
    (if (null? super)
        (list-copy ivarNames)
        (append (perform: super 'allInstVarNames) ivarNames))
) )

;; Track which methods are added to a particular class
;;  so they are not copied over.
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (perform:with: self 'myMethodNames: (cons selector old-names))
    self
) )

;; subclasses inherit mDict methods from their superclass
(define (addSelector:withMethod: classSelf selector method)
  (let* ( (mDict      (perform: classSelf 'methodDict))
          (subclasses (perform: classSelf 'subclasses))
        )
    (primAddSelector:withMethod: mDict selector method)
    (add-method-name-to-myMethods classSelf selector) ;; def'ed here
    (for-each
     (lambda (subClass)
       ;; if not overriden, copy down
       ;; Non-standard: avoids dynamic super-chain lookup
       (unless (memq selector (perform: subClass 'myMethodNames))
         (addSelector:withMethod: subClass selector method)))
     subclasses))
  classSelf
)

;; Am I self-referential, or what??
(addSelector:withMethod: Object
                         'addSelector:withMethod:
                         addSelector:withMethod:)

(addSelector:withMethod: Object
                         'allInstVarNames
                         allInstVarNames)

(primAddSelector:withMethod: (behavior MetaClass)
                         'allInstVarNames
                         allInstVarNames)

(primAddSelector:withMethod: (behavior (class MetaClass))
                         'allInstVarNames
                         allInstVarNames)


;; basicNew: Make a new instance of some class
(define (basicNew: classSelf num-added-vars)
;; NB: Added vars could be named and/or indexed
  (let ( (num-inherited-vars
            (length
             (perform: classSelf 'allInstVarNames)))
       )
    (primNew: classSelf
              (+ num-inherited-vars num-added-vars))
) )

(addSelector:withMethod: Object
       'basicNew:
       basicNew:)
(addSelector:withMethod: Object
       'basicNew
       (lambda (self) (basicNew: self 0)))
(addSelector:withMethod: Object
       'new:
       (lambda (self size)
         (perform: (basicNew: self size) 'initialize)))
(addSelector:withMethod: Object
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



;; Create an instance of a Class or MetaClass
(define (instantiateName:superclass:ivars:
         selfClass
         nameSymbol
         superClass
         ivarList)
  (let* ( (inherited-vars (perform: superClass 'allInstVarNames))
          (allIvars
             (append inherited-vars ivarList))
          (numAddedVars (length ivarList))
          (newInst
             (basicNew: selfClass numAddedVars))
          (newMethodDict
             (clone-method-dictionary (perform: superClass 'methodDict)))
        )
    ;; Use copies of behavior and mDict to avoid mutating originals
    (perform:with: newInst 'methodDict: newMethodDict)
    (st-obj-behavior-set! newInst
                          (clone-method-dictionary
                             (st-obj-behavior newInst)))
    (unless (zero? numAddedVars)
      (let ( (start-index (+ num-header-slots (length inherited-vars))) )
;;@@DEBUG{
;; (display "start-index for added vars: ")
;; (display (number->string start-index))
;; (newline)
;;}DEBUG@@
    (add-getters&setters newMethodDict start-index ivarList)))
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
       'methodDict: newMethodDict)
    (perform:with:
       superClass
       'addSubclass: newInst)
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

;;; Create a new subclass of a class
(define (newSubclassName:iVars:cVars:
         selfClass nameSym instanceVarsList classVarsList)
   (when (hashtable-ref smalltalk-dictionary nameSym #f)
    (error: "Class already exists" nameSym))
  (unless (and (symbol? nameSym)
               (let ( (name (symbol->string nameSym)) )
                 (and 
                  (> (string-length name) 1)
                  (char-upper-case? (string-ref name 0)))))
    (error: "subclass name must be a symbol which starts uppercase" name))
  ;; (unless (or (string? category) (symbol? category))
  ;;       (error: "subclass name must be a string or symbol" category))
  (unless (and (list? instanceVarsList)
               (every? symbol? instanceVarsList))
    (error: "InstanceVariableNames must be a list of symbols" instanceVarsList))
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


;; (addSelector:withMethod:
;;     Object
;;     'subclass:instanceVariableNames:classVariableNames:category:
;;      subclass:instanceVariableNames:classVariableNames:category:)




;; (provide 'st-class)

;;;			--- E O F ---			;;;
