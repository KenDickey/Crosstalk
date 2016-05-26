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
    comment
    myMethodNames)
)

(define num-basic-class-ivars (length basic-class-instance-variable-names))

(define st-Object-behavior
  (clone-method-dictionary st-object-behavior))
(add-getters&setters st-Object-behavior
                     num-header-slots
                     basic-class-instance-variable-names)
(define Object-Class
  (make-st-object st-Object-behavior
                  num-basic-class-ivars))

(perform:with: Object-Class
               'myMethodNames:
               (primSelectors st-Object-behavior))

(define st-Behavior-behavior  ;; start with object methods
  (clone-method-dictionary st-Object-behavior))
(define Behavior-Class
  (make-st-object st-Behavior-behavior num-basic-class-ivars))

(define st-ClassDescription-behavior
  (clone-method-dictionary st-Behavior-behavior))
(define ClassDescription-Class
  (make-st-object st-ClassDescription-behavior num-basic-class-ivars))

(define st-Class-behavior
  (clone-method-dictionary st-ClassDescription-behavior))
(define Class-Class
  (make-st-object st-Class-behavior num-basic-class-ivars))

(define st-MetaClass-behavior
  (clone-method-dictionary st-ClassDescription-behavior))
(add-getters&setters st-MetaClass-behavior
                     (+ num-header-slots num-basic-class-ivars)
                     '(thisClass)) ;; added
(define MetaClass-Class
  (make-st-object st-MetaClass-behavior
                  (+ 1 num-header-slots num-basic-class-ivars)))
(perform:with: Object-Class
               'myMethodNames:
               '(thisClass thisClass:))

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

;; (perform:with: Object-Class           'class: Class-Class)
;; (perform:with: Behavior-Class         'class: Class-Class)
;; (perform:with: ClassDescription-Class 'class: Class-Class)
;; (perform:with: Class-Class            'class: Class-Class)
;; (perform:with: MetaClass-Class        'class: Class-Class)

(perform:with: Object-Class           'superclass: st-nil)
(perform:with: Object-Class           'subclasses: (list Behavior-Class))
(perform:with: Behavior-Class         'superclass: Object-Class)
(perform:with: Behavior-Class         'subclasses: (list ClassDescription-Class))
(perform:with: ClassDescription-Class 'superclass: Behavior-Class)
(perform:with: ClassDescription-Class 'subclasses: (list Class-Class))
(perform:with: Class-Class            'superclass: ClassDescription-Class)
(perform:with: Class-Class            'subclasses: (list Object-Class Behavior-Class ClassDescription-Class))
(perform:with: MetaClass-Class        'superclass: ClassDescription-Class)

(perform:with: Object-Class           'methodDict: st-Object-behavior)
(perform:with: Behavior-Class         'methodDict: st-Behavior-behavior)
(perform:with: ClassDescription-Class 'methodDict: st-ClassDescription-behavior)
(perform:with: Class-Class            'methodDict: st-Class-behavior)
(perform:with: MetaClass-Class        'methodDict: st-MetaClass-behavior)

(perform:with: Behavior-Class         'instanceVariables: '(superclass methodDict format))
(perform:with: ClassDescription-Class 'instanceVariables: '(instanceVariables organization))
(perform:with: Class-Class            'instanceVariables: '(subclasses name comment myMethodNames))
(perform:with: MetaClass-Class        'instanceVariables: '(thisClass))

(define (allInstVarNames self)
  (let ( (ivarNames (perform: self 'instanceVariables))
         (super     (perform: self 'superclass))
       )
    (if (null? super)
        (list-copy ivarNames)
        (append (perform: super 'allInstVarNames) (list-copy ivarNames)))
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
(addSelector:withMethod: Object-Class 'addSelector:withMethod: addSelector:withMethod:)

(addSelector:withMethod: Object-Class 'allInstVarNames allInstVarNames)

(define (print-obj st-obj)
  (let ( (ivarNames (perform: (perform: st-obj 'class) 'allInstVarNames)) )
    (for-each
     (lambda (ivarName)
        (newline)
        (display ivarName)
        (display " -> ")
        (write (perform: st-obj name)))
     ivarNames)
    (newline)
) )

;; (define (make-st-class name ivars behavior class superClass)
;;   @@@
;; )



;;;			--- E O F ---			;;;
