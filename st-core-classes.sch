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
;; nil                  UndefinedObject
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

;; Track which methods are added to a particular class
;;  so they are not copied over.
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (perform:with: self 'myMethodNames: (cons selector old-names))
    self
) )

;; Below basicNew: Make a new instance of some class
(define (primNew: classSelf num-object-slots)
  (make-st-object
   (perform: classSelf 'methodDict)
   (+ num-header-slots num-object-slots)
) )

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

(define st-class-behavior (clone-behavior st-object-behavior))

(primAddSelector:withMethod:
     st-class-behavior
     'allInstVarNames allInstVarNames)

(primAddSelector:withMethod:
     st-class-behavior
     'basicNew: basicNew:)

(primAddSelector:withMethod:
     st-class-behavior
     'addSubclass: (lambda (self subclass)
                     (perform:with: self 'subclasses:
                                    (cons subclass
                                          (perform: self 'subclasses)))))

(define st-metaClass-behavior (clone-behavior st-class-behavior))

(add-getters&setters st-class-behavior
                     num-header-slots
                     combined-class-ivar-names)

(add-getters&setters st-metaClass-behavior
                     num-header-slots
                     combined-metaClass-ivar-names)

;;; Scaffolding setup
;; Just enough behavior to allow instantiation bootstrap
(define (make-protoClass
         name behav slot-names
         mDict child-ivar-names
         class super )
  (let* ( (behavior   (clone-behavior behav))
          (methodDict (clone-method-dictionary mDict))
          (class-instance (make-st-object behavior (length slot-names)))
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
(perform:with: MetaClassClass 'superclass: ClassClass)
(setClass:     MetaClassClass  MetaClass) ;; Nota Bene!
(perform:with: MetaClass      'superclass: Class) ;; ClassDescription
;; Ficup until we fix things
(primAddSelector:withMethod:
     (behavior MetaClass)
     'allInstVarNames (lambda (self) combined-metaClass-ivar-names))
(primAddSelector:withMethod:
     (behavior MetaClassClass)
     'allInstVarNames (lambda (self) combined-class-ivar-names))


;;; Get regular

;; Create an instance of a Class or MetaClass
(define (instantiateName:superclass:ivars:
         selfClass
         nameSymbol
         superClass
         addedInstanceVars)
  (let* ( (inherited-vars (perform: superClass 'allInstVarNames))
          (allIvars
             (append inherited-vars addedInstanceVars))
          (numAddedVars (length addedInstanceVars))
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
      (let ( (start-index (+ num-header-slots (length inherited-vars) -1)) )
;;@@DEBUG{
(display "start-index for added vars: ")
(display (number->string start-index))
(newline)
;;}DEBUG@@
      (add-getters&setters newMethodDict start-index addedInstanceVars)))
    (setClass: newInst selfClass)
    (perform:with:
       newInst
       'name:       nameSymbol)
    (perform:with:
       newInst
       'superclass: superClass)
    (perform:with:
       newInst ;; ANSI requires a fresh (unshared) list
       'instanceVariables: (list-copy addedInstanceVars))
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
          (newSubClass
             (instantiateName:superclass:ivars:
                newMetaClass
                nameSym
                selfClass
                instanceVarsList))
        )
    (perform:with: newMetaClass 'thisClass: newSubClass)
    (hashtable-set! smalltalk-dictionary nameSym newSubClass)
    newSubClass		;; @@??@@ move initialize to here?
) )

;;; OK.  Now use protoClasses to bootstrap core classes


(define Object
  (newSubclassName:iVars:cVars:
   Class
   'Object '() '()))

(perform:with: Object 'superclass: st-nil) ;; ground case
(perform:with: (class Object) 'superclass: Class)

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
(setClass: MetaClass MetaClassClass)

(perform:with: Class
               'instanceVariables: class-added-var-names)
(perform:with: MetaClass
               'instanceVariables: metaClass-added-var-names)
(perform:with: ClassClass
               'instanceVariables: '())
(perform:with: MetaClassClass
               'instanceVariables: '())


(for-each ;; get regular
 (lambda (class)
   (primAddSelector:withMethod:
    (behavior class)
    'allInstVarNames allInstVarNames)
   )
 (list Object Behavior ClassDescription Class MetaClass
       (class Object) (class Behavior) (class ClassDescription)
       (class Class) (class MetaClass)))

(perform:with: Class     'superclass: ClassDescription)
(perform:with: MetaClass 'superclass: ClassDescription)

;; ;; add methods from st-object-behavior into Object
;; (let ( (obj-behavior (behavior Object)) )
;;   (primSelectorsAndMethodsDo:
;;    st-object-behavior
;;    (lambda (selector method)
;;      (unless (primIncludesSelector: obj-behavior selector)
;;        (primAddSelector:withMethod: obj-behavior selector method))))
;;   (set! st-object-behavior obj-behavior))

;; (provide 'st-core-classes)

;;;			--- E O F ---			;;;
