;;; FILE: "st-class.sch"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: 16 May 2016

;; (require 'st-kernel)  ; message send
;; (require 'st-object)  ; Object behavior

;; Now that we have Object behavior, we can instantiate classes
;;  and bootstrap reflection via Classes, which are the management
;;  structure for object behaviors.

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

(define st-Object-behavior
  (clone-method-dictionary st-object-behavior))
(add-getters&setters st-Object-behavior
                     num-header-slots
                     basic-class-instance-variable-names)
(define Object
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
(perform:with: Object
               'myMethodNames:
               '(thisClass thisClass:))

(hashtable-set! smalltalk-dictionary 'Object           Object)
(hashtable-set! smalltalk-dictionary 'Behavior         Behavior)
(hashtable-set! smalltalk-dictionary 'ClassDescription ClassDescription)
(hashtable-set! smalltalk-dictionary 'Class            Class)
(hashtable-set! smalltalk-dictionary 'MetaClass        MetaClass)

(perform:with: Object           'name: "Object")
(perform:with: Behavior         'name: "Behavior")
(perform:with: ClassDescription 'name: "ClassDescription")
(perform:with: Class            'name: "Class")
(perform:with: MetaClass        'name: "MetaClass")

(perform:with: Object           'superclass: st-nil)
(perform:with: Object           'subclasses: (list Behavior))
(perform:with: Behavior         'superclass: Object)
(perform:with: Behavior         'subclasses: (list ClassDescription))
(perform:with: ClassDescription 'superclass: Behavior)
(perform:with: ClassDescription 'subclasses: (list Class))
(perform:with: Class            'superclass: ClassDescription)
(perform:with: Class            'subclasses: (list Object Behavior ClassDescription))
(perform:with: MetaClass        'superclass: ClassDescription)

(perform:with: Object           'methodDict: st-Object-behavior)
(perform:with: Behavior         'methodDict: st-Behavior-behavior)
(perform:with: ClassDescription 'methodDict: st-ClassDescription-behavior)
(perform:with: Class            'methodDict: st-Class-behavior)
(perform:with: MetaClass        'methodDict: st-MetaClass-behavior)

(perform:with: Behavior         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription 'instanceVariables: '(instanceVariables organization))
(perform:with: Class            'instanceVariables: '(subclasses name category comment myMethodNames))
(perform:with: MetaClass        'instanceVariables: '(thisClass))

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

;; NB: self is a Class
(define (addSelector:withMethod: self selector method)
  (let* ( (mDict      (perform: self 'methodDict))
          (subclasses (perform: self 'subclasses))
        )
    (primAddSelector:withMethod: mDict selector method)
    (add-method-name-to-myMethods self selector)
    (for-each
     (lambda (subClass)  ;; if not overriden, copy down
       (unless (memq selector (perform: subClass 'myMethodNames))
         (addSelector:withMethod: subClass selector method)))
     subclasses))
  self
)

;; Am I self-referential, or what??
(addSelector:withMethod: Object 'addSelector:withMethod: addSelector:withMethod:)

(addSelector:withMethod: Object    'allInstVarNames allInstVarNames)
(addSelector:withMethod: MetaClass 'allInstVarNames allInstVarNames)


;; Make a new instance of me..
(define (basicNew: self num-indexed-vars) ;; self is a class
  (let ( (num-named-vars (length (perform: self 'allInstVarNames))) )
    (make-st-object (perform: self 'methodDict)
                    (+ num-named-vars num-indexed-vars))
) )

(addSelector:withMethod: Behavior 'basicNew:
                         basicNew:)
(addSelector:withMethod: Behavior 'basicNew
                         (lambda (self) (basicNew: self 0)))
(addSelector:withMethod: Behavior 'new:
                         (lambda (self size) (perform: (basicNew: self size) 'initialize)))
(addSelector:withMethod: Behavior 'new
                         (lambda (self) (perform: (basicNew: self 0) 'initialize)))

(addSelector:withMethod: MetaClass 'new
                         (lambda (self)
                           (if (eq? self (perform: (perform: self 'thisClass) 'class))
                               (error: "A Metaclass should only have one instance!" self)
                               (let ( (newMeta (perform: self 'basicNew)) )
                                 (perform:with: self 'thisClass: newMeta)
                                 (newMeta initialize)
                                 newMeta)
                         ) )
)

(addSelector:withMethod: MetaClass 'new:
                         (lambda (self size)
                           (if (eq? self (perform: (perform: self 'thisClass) 'class))
                               (error: "A Metaclass should only have one instance!" self)
                               (let ( (newMeta (perform:with: self 'basicNew: size)) )
                                 (perform:with: self 'thisClass: newMeta)
                                 (newMeta initialize)
                                 newMeta)
                         ) )
)

(addSelector:withMethod: Class 'addSubclass:
                         (lambda (self subclass)
                           (perform:with: self 'subclasses:
                                          (cons subclass
                                                (perform: self 'subclasses))))
)

;; Add a metaClass to an "orphan" class
(define (addMetaClass:classVars: orphanClass classVarNames)
  (let* ( (metaName
              (string-append (perform: orphanClass 'name) " class"))
          (allClassVarNames
              (append (perform: MetaClass 'allInstVarNames) classVarNames))
          (theMeta
              (basicNew: MetaClass (length allClassVarNames)))
        )
    (perform:with: theMeta 'instanceVariables: classVarNames)
    (perform:with: theMeta 'name: metaName)
    (primSetClass: theMeta  MetaClass)
;;  (perform:with: theMeta 'thisClass: orphanClass)
    (primSetClass: orphanClass theMeta)
    (hashtable-set! smalltalk-dictionary metaName theMeta)
    (perform: theMeta 'initialize)
    theMeta
) )


;; Ask a metaClass to create its class
(define (classFromMeta:name:ivars:superclass:category:
         metaClass name ivar-names superclass category)
  (let* ( (theClass (perform:with: metaClass 'basicNew: (length ivar-names))) )
    (perform:with: theClass 'name:       name)
    (perform:with: theClass 'instanceVariables: ivar-names)
    (primSetClass: theClass metaClass)
    (perform:with: theClass 'superclass: superclass)
    (perform:with: theClass 'category:   category)
;;  (perform:with: theClass 'thisClass:  theClass) -- done by #basicNew:
    (hashtable-set! smalltalk-dictionary name theClass)
    (perform: theClass 'initialize)
    theClass
) )


;; <Class> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
;; Ask a class to create a new subclass
(define (subclass:instanceVariableNames:classVariableNames:category: self name ivars-list classvars-list category)
  (when (hashtable-ref smalltalk-dictionary name #f)
    (error: "Class already exists" name))
  (unless (and (string? name)
               (> (string-length name) 1)
               (char-upper-case? (string-ref name 0)))
    (error: "subclass name must be a string which starts uppercase" name))
  (unless (or (string? category) (symbol? category))
        (error: "subclass name must be a string or symbol" category))
  (unless (and (list? ivars-list)
               (every? symbol? ivars-list))
    (error: "InstannceVariableNames must be a list of symbols" ivars-list))
  (unless (and (list? classvars-list)
               (every? symbol? classvars-list))
    (error: "ClassVariableNames must be a list of symbols" classvars-list))
  
  (let* ( (metaName  (string-append name " class"))
          (allClassvarNames
             (append (perform: MetaClass 'allInstVarNames) classvars-list))
          (metaClass
             (perform:with: MetaClass 'basicNew: (length allClassvarNames)))
          (allIvarNames (append (perform: self 'allInstVarNames) ivars-list))
        )
    ;; set up metaClass
    (perform:with: metaClass 'instanceVariables: classvars-list)
    (perform:with: metaClass 'name:      metaName)
    (primSetClass: metaClass MetaClass)
;;  (perform:with: metaClass 'thisClass: subClass) -- done in #basicNew:
    (perform:with: metaClass 'superclass: (perform: self 'class))
    (perform: metaClass 'initialize)

    ;; self
    (let ( (subClass
              (classFromMeta:name:ivars:superclass:category:
                   metaClass name allIvarNames self category))
         )
      (hashtable-set! smalltalk-dictionary metaName metaClass)
      (hashtable-set! smalltalk-dictionary name     subClass)
;;      (perform: subClass 'initialize)
;;	  -- done in #classFromMeta:name:ivars:superclass:category:
      (perform:with: self 'addSubclass: subClass)
      ;; return value:
      subClass)
) )

(addSelector:withMethod:
    Class
    'subclass:instanceVariableNames:classVariableNames:category:
     subclass:instanceVariableNames:classVariableNames:category:)

;; OK, now for the meta-class hierarchy..

(addMetaClass:classVars: Object    '())
(addMetaClass:classVars: Behavior  basic-class-instance-variable-names)
(addMetaClass:classVars: ClassDescription basic-class-instance-variable-names)
(addMetaClass:classVars: Class     basic-class-instance-variable-names)
(addMetaClass:classVars: MetaClass (append basic-class-instance-variable-names '(thisClass)))

;; MetaClass class class == MetaClass

(primSetClass: (perform: (perform: MetaClass 'class) 'class)
               MetaClass)



;;;			--- E O F ---			;;;
