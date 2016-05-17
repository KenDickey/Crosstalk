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

(define st-Object-behavior st-object-behavior)
(add-getters&setters st-Object-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name)) ;; NB: Pools Depricated !!
(define Object-Class
  (make-st-object st-Object-behavior
                  7
                  0))

(define st-Behavior-behavior  ;; start with object methods
  (clone-method-dictionary st-Object-behavior))
(add-getters&setters st-Behavior-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name)) ;; NB: Pools Depricated !!
(define Behavior-Class
  (make-st-object  st-Behavior-behavior
                   7
                   0))

(define st-ClassDescription-behavior
  (clone-method-dictionary st-Behavior-behavior))
(add-getters&setters st-ClassDescription-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name))
(define ClassDescription-Class
  (make-st-object st-ClassDescription-behavior
                  7
                  0))

(define st-Class-behavior
  (clone-method-dictionary st-ClassDescription-behavior))
(add-getters&setters st-Class-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name)) ;; NB: Pools Depricated !!
(define Class-Class
  (make-st-object st-Class-behavior
                  7
                  0))

(define st-MetaClass-behavior
  (clone-method-dictionary st-Behavior-behavior))
(add-getters&setters st-MetaClass-behavior
                     '(superclass methodDict format
                       instanceVariables organization
                       subclasses name
                       thisClass))
(define MetaClass-Class
  (make-st-object st-MetaClass-behavior
                  8
                  0))

(hashtable-set! smalltalk-dictionary 'Object           Object-Class)
(hashtable-set! smalltalk-dictionary 'Behavior         Behavior-Class)
(hashtable-set! smalltalk-dictionary 'ClassDescription ClassDescription-Class)
(hashtable-set! smalltalk-dictionary 'Class            Class-Class)
(hashtable-set! smalltalk-dictionary 'MetaClass        MetaClass-Class)
(perform:with: Object-Class           'name: 'Object)
(perform:with: Behavior-Class         'name: 'Behavior)
(perform:with: ClassDescription-Class 'name: 'ClassDescription)
(perform:with: Class-Class            'name: 'Class)
(perform:with: MetaClass-Class        'name: 'MetaClass)

(perform:with: Object-Class           'superclass: st-nil)
(perform:with: Behavior-Class         'superclass: Object-Class)
(perform:with: ClassDescription-Class 'superclass: Behavior-Class)
(perform:with: Class-Class            'superclass: ClassDescription-Class)
(perform:with: MetaClass-Class        'superclass: Class-Class)

(perform:with: Object-Class           'methodDict: st-Object-behavior)
(perform:with: Behavior-Class         'methodDict: st-Behavior-behavior)
(perform:with: ClassDescription-Class 'methodDict: st-ClassDescription-behavior)
(perform:with: Class-Class            'methodDict: st-Class-behavior)
(perform:with: MetaClass-Class        'methodDict: st-MetaClass-behavior)

(perform:with: Behavior-Class         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription-Class 'instanceVariables: '(instanceVariables organization))
(perform:with: Class-Class            'instanceVariables: '(subclasses name))
(perform:with: MetaClass-Class        'instanceVariables: '(thisClass))

;; (define (make-st-class name ivars behavior class superClass)
;;   @@@
;; )



;;;			--- E O F ---			;;;
