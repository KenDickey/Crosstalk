#!r6rs
;;; FILE: "st-metaclass.sls"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-metaclass)

  (export
   init-st-metaclass

   ClassClass
   Class
   MetaClassClass
   MetaClass

   name->metaName
   instantiateName:superclass:ivars:
   newSubclassName:iVars:cVars:
   addSelector:withMethod:
   addSelector:withMethod:arity:
   primAppendLocalSelectors:
   )
  
  (import
   (rnrs base)
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs lists (6))
   (rnrs io simple (6))
   (only (chezscheme)
         trace-define
         print-level
         make-parameter)
   (st-base)
   (st-class-structure)
   )

;;; Scaffolding setup
;; Just enough behavior to allow instantiation bootstrap
;; to call: newSubclassName:iVars:cVars:

(trace-define (make-protoClass
         name
         myBehavior  ; method-dict of this class
         mySlotNames
         instMethodDict  ; shared method-dict for all instances
         child-ivar-names
         class
         super )
  (let ( (class-instance
             (make-st-object myBehavior (length mySlotNames)))
       )
    (setClass: class-instance class) ;; NB: may be nil
    (perform:with: class-instance 'name: name)
    (perform:with: class-instance 'superclass: super) ;; may be nil
    (perform:with: class-instance 'instanceVariables: child-ivar-names)
    (perform:with: class-instance 'methodDict: instMethodDict)
    (unless (st-nil? class)
      (addSubclass: class class-instance))
   ;; return the new Class instance
    (smalltalkAt:put: name class-instance)
    class-instance
) )

(define (primAppendLocalSelectors: aClass selector-list)
  (let ( (old-selectors (perform: aClass 'myMethodNames)) )
    (if (any? (lambda (sel) (memq sel old-selectors)) selector-list)
        (error 'primAppendLocalSelectors:
               "attempt to add duplicate selector"
               selector-list old-selectors))
    (perform:with: aClass 'myMethodNames:
                   (append selector-list old-selectors))
  ) )


;;; Miminal Scaffolding
;; Temp for bootstrap -- re-relate later

(define ClassClass
  (make-protoClass
     (string->symbol "Class class") ; name
     (clone-behavior st-metaClass-behavior) ; I am a MetaClass
     combined-metaClass-ivar-names  ;; my slot-names
     (clone-behavior st-class-behavior) ; instances are Classes
     combined-class-ivar-names      ;; instance ivar-names
     '() ; class
     '() ; super
) )


(define Class
  (make-protoClass
     'Class ; name
     (clone-behavior st-class-behavior)
     combined-class-ivar-names  ;; my ivar-names
     (clone-behavior st-class-behavior)
     combined-class-ivar-names ;; instance ivar-names
     ClassClass ;; class
     '() ; super
) )

(define MetaClassClass
  (make-protoClass
     (string->symbol "MetaClass class")
     (clone-behavior st-metaClass-behavior) ;; I am a MetaClass
     combined-metaClass-ivar-names   ;; slot-names
     (clone-behavior st-class-behavior) ;; that makes classes
     combined-class-ivar-names  ;; child-ivar-names
     '() ;; class is MetaClass
     ClassClass ;; super
 ) )

(define MetaClass
  (make-protoClass
     'MetaClass
     (clone-behavior st-class-behavior)   ;; MetaClass is a class
     combined-class-ivar-names   ;; slot-names
     (clone-behavior st-metaClass-behavior) ;; Who's instances are meta-classes
     combined-metaClass-ivar-names  ;; child-ivar-names
     Class ;; class
     Class ;; super is really ClassDescription
 ) )


;;;The regular way to make a new (sub)class instance:
;;;   Ask MetaClass to make the metaClass
;;;   Then ask the metaClass to make its instance

;; Helper. Create an INSTANCE of a Class or MetaClass
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
;;    (addSubclass:  superClass newInst)
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
(trace-define (newSubclassName:iVars:cVars:
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
      (addSubclass:  newMetaClass newSubclass)
      (smalltalkAt:put: nameSym newSubclass)
      newSubclass		;; @@??@@ move initialize to here?
) ) )


;;; Track which methods are added to a particular class
;;;  so they are not copied over from above.
;;  See #subclassAddSelector:withMethod: below
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (unless (memq selectors old-names)
      (perform:with: self 'myMethodNames: (cons selector old-names)))
    self
) )


;;; Subclasses inherit mDict methods from their superclass
;;;  so adding a selector_method to a class affects
;;;  its instances, NOT the class instance itself.
(define (addSelector:withMethod: classSelf selector method)
  (add-method-name-to-myMethods classSelf selector) ;; def'ed this class
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

(define (addSelector:withMethod:arity: classSelf selector method arity)
  (let ( (annotated-proc (annotate-procedure-with-arity method selector arity)) )
    (addSelector:withMethod: classSelf selector annotated-proc)))



;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-metaclass)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-class-structure)

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

'st-metaclass
) )

)

;;;			--- E O F ---			;;;
