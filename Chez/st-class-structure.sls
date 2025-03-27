#!r6rs
;;; FILE: "st-class-structure.sls"
;;; IMPLEMENTS: Class structure
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-class-structure)

  (export
;;   "Send to Super"
   %     superPerform:
   %:    superPerform:with:
   %::   superPerform:with:with:
   %:::  superPerform:with:with:with:
   %:::: superPerform:with:with:with:with:
   %&    superPerform:withArguments: ;; args array
   %*    superPerform:withArgsList:  ;; args list

   st-class-behavior
   st-metaClass-behavior

   allInstVarNames
   allSuperclasses
   allSubclasses

;; internal
   combined-class-ivar-names
   combined-metaClass-ivar-names
   combined-classDescription-var-names
   metaClass-added-var-names
   class-added-var-names

   display-allSupers
   display-subclasses

   primNew:
   basicNew:

   addSubclass:
   rebase-mdict!
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
   (st-base)
   )

;;; We want to instantiate classes and bootstrap reflection
;;; via Classes, which are the management structure for object
;;; behaviors.

;;; We build and populate the structure "by hand" here.

;;; Basically, each class has a superclass (except Object) and
;;;   each Class's class is a metaClass which is an
;;;   instance of MetaClass.

;;; An instance's shape (instance variables)
;;;   and behavior (instance methods)
;;;   is defined in its class
;;;      (anObj behavior) == (anObj class methodDict)

;;; A class's shape (class variables) and behavior (class methods)
;;;   is defined in the instance class's metaClass 

;;; Classes are instances of metaClasses
;;; metaClasses are instances of class MetaClass

;;; We tie these together here by making instances and then setting
;;;  up the proper references via instance variables and behaviors.

;;; Boolean superclass -> Object
;;; Object  superclass -> nil  (ground case)
;;; Boolean class -> 'Boolean class' (its unnamed metaClass)
;;; Boolean class class -> MetaClass
;;; MetaClass class -> 'MetaClass class' (its metaClass)
;;; MetaClass class class -> MetaClass (wraps around)
;;; Boolean class superclass -> 'Object class' (Object's metaClass)
;;; Boolean class superclass class -> Class
;;; Object class superclass -> Class


(define combined-classDescription-var-names
  '(superclass
    methodDict ;; shared between instances
    format ;; layout/structure
    instanceVariables
    organization))

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


(define (allSubclasses a-class)
  ;; NB: Class/Object/.. wraps around
  ;; ( Class is an Object; Object is a Class )
  ;; Returns a list of St Class objects, without a-class
  (let process-loop ( (all-subs '()) (to-process (list a-class)) )
    (if (null? to-process)
        all-subs ; done
        (let obj-loop ( (directSubs ($ (car to-process) 'subclasses))
                        (allSubs all-subs)
                        (toProcess (cdr to-process)) )
          (cond
           ((null? directSubs)
            (process-loop allSubs toProcess)
            )
           ((eq? (car directSubs) a-class) ; elide self
            (obj-loop (cdr directSubs) allSubs toProcess)
            )
           ((memq (car directSubs) allSubs) ; already seen
            (obj-loop (cdr directSubs) allSubs toProcess)
            )
           (else
            (obj-loop (cdr directSubs)
                      (cons (car directSubs) allSubs)
                      (cons (car directSubs) toProcess))
            )
          )
) ) ) )
    

(define (display-allSupers obj)
  (display-obj (allSuperclasses obj)))

(define (display-spaces n)
  (let loop ( (count 0) )
    (when (< count n)
      (display #\space)
      (loop (+ count 1)))))

(define (display-subs class shown indent delta)
  (when (not (memq class shown))
    (newline)
    (display-spaces indent)
    (display (printString class))
    (for-each
     (lambda (sub)
       (display-subs sub (cons class shown) (+ delta indent) delta))
     (list-sort (lambda (c1 c2) (symbol<? ($ c1 'name) ($ c2 'name)))
                ($ class 'subclasses)))
) )

(define (display-subclasses class)
  (display-subs class '() 0 3)
  (newline))
  
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

(define st-class-behavior     (clone-behavior st-object-behavior))

(define st-metaClass-behavior (clone-behavior st-object-behavior))

;;; More mechanics: (addSubclass: classSelf subclass)
(define (addSubclass: classSelf subclass)
  (let ( (my-subclasses (perform: classSelf 'subclasses)) )
    (perform:with: classSelf
                   'subclasses:
                   (cons subclass
                         my-subclasses))
) )

(define (rebase-mdict! aClass public-behavior)
  (behavior-add-from-other public-behavior
                           ($ aClass 'methodDict))
  ($: aClass 'methodDict: public-behavior))

;;; Send to super
(define (superPerform: self selectorSym)
  ((lookupSelector: (superclass (class self)) selectorSym) self))

(define (superPerform:with: self selectorSym arg)
  ((lookupSelector: (superclass (class self)) selectorSym) self arg))

(define (superPerform:with:with: self selectorSym arg1 arg2)
  ((lookupSelector: (superclass (class self)) selectorSym)
   self arg1 arg2))

(define (superPerform:with:with:with:
         self selectorSym arg1 arg2 arg3)
  ((lookupSelector: (superclass (class self)) selectorSym)
   self arg1 arg2 arg3))

(define (superPerform:with:with:with:with:
         self selectorSym arg1 arg2 arg3 arg4)
  ((lookupSelector: (superclass (class self)) selectorSym)
   self arg1 arg2 arg3 arg4))

(define (superPerform:withArguments: self selectorSym argsArray)
  (superPerform:withArgsList:
   self
   selectorSym
   (vector->list argsArray)))

(define (superPerform:withArgsList: self selectorSym argsList)
  (apply (lookupSelector: (superclass (class self)) selectorSym)
         (cons self argsList)))

;;; Shorter Syntax
(define %     superPerform:)
(define %:    superPerform:with:)
(define %::   superPerform:with:with:)
(define %:::  superPerform:with:with:with:)
(define %:::: superPerform:with:with:with:with:)
(define %&    superPerform:withArguments:) ;; args array
(define %*    superPerform:withArgsList:)  ;; args list


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

(add-getters&setters st-class-behavior
                     num-header-slots
                     combined-class-ivar-names)

(add-getters&setters st-metaClass-behavior
                     num-header-slots
                     combined-metaClass-ivar-names)


(primAddSelector:withMethod:
     st-class-behavior
     'allInstVarNames allInstVarNames)

(primAddSelector:withMethod:
     st-class-behavior
     'basicNew: basicNew:)

(primAddSelector:withMethod:
     st-class-behavior
     'addSubclass: addSubclass:)

(primAddSelector:withMethod:
     st-metaClass-behavior
     'allInstVarNames allInstVarNames)

(primAddSelector:withMethod:
     st-metaClass-behavior
     'basicNew: basicNew:)

(primAddSelector:withMethod:
     st-metaClass-behavior
     'addSubclass: addSubclass:)


)

;;;			--- E O F ---			;;;
