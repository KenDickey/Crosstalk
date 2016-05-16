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

(define st-Behavior-behavior (make-mDict-placeholder 'Behavior))
(add-getters&setters st-Behavior-behavior
                     '(superclass methodDict format))
(define Behavior-Class
  (make-st-object  st-Behavior-behavior
                   3
                   0))

(define st-ClassDescription-behavior (make-mDict-placeholder 'ClassDescription))
(add-getters&setters st-ClassDescription-behavior
                     '(superclass methodDict format
                       instanceVariables organization))
(define ClassDescription-Class
  (make-st-object st-ClassDescription-behavior
                  5
                  0))

(define st-Class-behavior (make-mDict-placeholder 'Behavior))
(add-getters&setters st-Class-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name)) ;; NB: Pools Depricated !!
(define Class-Class
  (make-st-object st-Class-behavior
                  7
                  0))

(define Object-Class (make-st-object st-object-behavior 0 0))

(define st-MetaClass-behavior (make-mDict-placeholder 'MetaClass))
(add-getters&setters st-MetaClass-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       thisClass))
(define MetaClass-Class
  (make-st-object st-MetaClass-behavior
                  6
                  0))

(hashtable-set! smalltalk-dictionary 'Object           Object-Class)
(hashtable-set! smalltalk-dictionary 'Behavior         Behavior-Class)
(hashtable-set! smalltalk-dictionary 'ClassDescription ClassDescription-Class)
(hashtable-set! smalltalk-dictionary 'Class            Class-Class)
(hashtable-set! smalltalk-dictionary 'MetaClass        MetaClass-Class)
(perform:with: st-Object-behavior           'superClass: st-nil)
(perform:with: st-Behavior-behavior         'superClass: Object-Class)
(perform:with: st-ClassDescription-behavior 'superClass: Behavior-Class)
(perform:with: st-Class-behavior            'superClass: ClassDescription-Class)
(perform:with: st-MetaClass-behavior        'superClass: Class-Class)
(perform:with: Object-Class           'methodDict: st-Object-behavior)
(perform:with: Behavior-Class         'methodDict: st-Behavior-behavior)
(perform:with: ClassDescription-Class 'methodDict: st-ClassDescription-behavior)
(perform:with: Class-Class            'methodDict: st-Class-behavior)
(perform:with: MetaClass-Class        'methodDict: st-MetaClass-behavior)
(perform:with: Behavior-Class         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription-Class 'instanceVariables: '(instanceVariables organization))
(perform:with: Class-Class            'instanceVariables: '(subclasses name))
(perform:with: MetaClass-Class        'instanceVariables: '(thisClass))
(perform:with: Object-Class           'name: 'Object)
(perform:with: Behavior-Class         'name: 'Behavior)
(perform:with: ClassDescription-Class 'name: 'ClassDescription)
(perform:with: Class-Class            'name: 'Class)
(perform:with: MetaClass-Class        'name: 'MetaClass)


;; (define (make-st-class name ivars behavior class superClass)
;;   @@@
;; )



;;;			--- E O F ---			;;;
