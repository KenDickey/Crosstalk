;;; FILE: "st-boolean.scm"
;;; IMPLEMENTS: Boolean True False UndefinedObject (a.k.a. nil)
;;; LANGUAGE: Scheme R7RS
;;; AUTHOR: Ken Dickey
;;; DATE: 10 June 2016

;; (require 'st-core-classes)

;;; Boolean logic is typically inlined, but as a bootstrap..

(define Boolean
  (newSubclassName:iVars:cVars:
   Object
   'Boolean '() '())
)

(define True
  (newSubclassName:iVars:cVars:
   Boolean
   'True '() '())
)

(define False
  (newSubclassName:iVars:cVars:
   Boolean
   'False '() '())
)

(define UndefinedObject
  (newSubclassName:iVars:cVars:
   Object
   'UndefinedObject '() '()))

(perform:with: UndefinedObject
               'comment:
               "I describe the behavior of my sole instance, nil. nil represents a prior value for variables that have not been initialized, or for results which are meaningless.")

(addSelector:withMethod:
        Boolean
        'shallowCopy ;; Can't clone a boolean
        (lambda (self) self))

(addSelector:withMethod:
        UndefinedObject
        'shallowCopy ;; Can't clone nil
        (lambda (self) self))

;;(define st-boolean-behavior (behavior Boolean))
;; st-true-behavior and st-false-behavior are def'ed in "st_kernel.sch"
;; reset their behaviors
(set! st-true-behavior  (perform: True            'methodDict))
(set! st-false-behavior (perform: False           'methodDict))
(set! st-nil-behavior   (perform: UndefinedObject 'methodDict))

(perform:with:
     Boolean
     'category: '|Kernel-Objects|)

(perform:with:
     Boolean
     'comment:
"Boolean is an abstract class defining the protocol for logic testing
 operations and conditional control structures for the logical values
 represented by the instances of its subclasses True and False.

Boolean redefines #new so no instances of Boolean can be created.
 It also redefines several messages in the 'copying' protocol to ensure
 that only one instance of each of its subclasses True (the global true,
 logical assertion) and False (the global false, logical negation) ever
 exist in the system."
)

(perform:with:
     True
     'category: '|Kernel-Objects|)

(perform:with:
     True
     'comment:
"True defines the behavior of its single instance, true -- logical assertion.

Notice how the truth-value checks become direct message sends, without the
 need for explicit testing.

Be aware however that most of these methods are not sent as real messages in
 normal use. Most are inline coded by the compiler/translator - avoiding the
 overhead of the full message sends. So simply redefining these methods here
 will have no effect."
)

(perform:with:
     UndefinedObject
     'category: '|Kernel-Objects|)

(perform:with:
     UndefinedObject
     'comment:
"I describe the behavior of my sole instance, nil. nil represents a prior
 value for variables that have not been initialized, or for results which
 are meaningless."
)

(perform:with:
     False
     'category: '|Kernel-Objects|)

(perform:with:
     False
     'comment:
"False defines the behavior of its single instance, false -- logical negation.

Notice how the truth-value checks become direct message sends, without the
 need for explicit testing.

Be aware however that most of these methods are not sent as real messages
 in normal use. Most are inline coded by the compiler/translator - avoiding
 the overhead of the full message sends. So simply redefining these methods
 here will have no effect."
)


(addSelector:withMethod: 
 	(class Object)
        'initializedInstance
        (lambda (self) (perform: self 'new)))

(addSelector:withMethod: 
 	(class Boolean)
        'initializedInstance
        (lambda (self) st-nil)) ;; Nota Bene

(addSelector:withMethod: 
 	(class True)
        'initializedInstance
        (lambda (self) st-true)) ; #true

(addSelector:withMethod: 
 	(class False)
        'initializedInstance
        (lambda (self) st-false)) ; #false

(addSelector:withMethod: 
 	(class UndefinedObject)
        'initializedInstance
        (lambda (self) st-nil)) ; '()

(addSelector:withMethod: 
 	True
        'printOn:
        (lambda (self port)
          (display "true" port)))

(addSelector:withMethod: 
 	False
        'printOn:
        (lambda (self port)
          (display "false" port)))

(addSelector:withMethod: 
 	UndefinedObject
        'printOn:
        (lambda (self port)
          (display "nil" port)))

;; ANSI

;;; UndefinedObject -> nil

(addSelector:withMethod:
        UndefinedObject
        'notNil
        (lambda (self) st-false))

(addSelector:withMethod:
        Object
        'notNil
        (lambda (self) st-true))

(addSelector:withMethod:
        UndefinedObject
        'isNil
        (lambda (self) st-true))

(addSelector:withMethod:
        Object
        'isNil
        (lambda (self) st-false))

(addSelector:withMethod:
        UndefinedObject
        'isEmptyOrNil ;; collection protocol
        (lambda (self) st-true))

(addSelector:withMethod:
        Object
        'isNil
        (lambda (self) st-false))

(addSelector:withMethod:
        UndefinedObject
        'ifNil:
        (lambda (self thunk) (thunk)))

(addSelector:withMethod:
        Object
        'ifNil:
        (lambda (self thunk) st-nil))

(addSelector:withMethod:
        UndefinedObject
        'ifNil:ifNotNil:
        (lambda (self nilBlock ifNotNilBlock)
          (nilBlock)))

(addSelector:withMethod:
        Object
        'ifNil:ifNotNil:
        (lambda (self nilBlock ifNotNilBlock)
          (ifNotNilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNil:
        (lambda (self nilBlock) st-nil))

(addSelector:withMethod:
        Object
        'ifNotNil:
        (lambda (self nilBlock)
          (nilBlock)))

(addSelector:withMethod:
        Object
        'ifNotNil:ifNil
        (lambda (self ifNotNilBlock nilBlock)
          (ifNotNilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNil:ifNil:
        (lambda (self ifNotNilBlock nilBlock)
          (nilBlock)))

(addSelector:withMethod:
        UndefinedObject
        'ifNotNilDo:
        (lambda (self block) st-nil))

(addSelector:withMethod:
        Object
        'ifNotNilDo:
        (lambda (self block) (block self)))

(addSelector:withMethod:
        UndefinedObject
        'basicCopy
        (lambda (self) self))

;;; Boolean True False

(addSelector:withMethod: 
 	True
        '&  ;; #&  logical and  (full)
        (lambda (self aBoolean) aBoolean))

(addSelector:withMethod: 
 	False
        '& ;; #&
        (lambda (self aBoolean) st-false)) ;; self

;; Note:
;;  (symbol->string '|\||) --> "|"
;;  (string->symbol "|")   --> |\||

(addSelector:withMethod: 
 	True
        '|\|| ;; #| logical or (full)
        (lambda (self aBoolean) st-true)) ;; self

(addSelector:withMethod: 
 	False
        '|\|| ;; #|
        (lambda (self aBoolean) aBoolean)) 

(addSelector:withMethod: 
 	True
        'and:  ;; logical and (short circuit)
        (lambda (self thunk) (thunk)))

(addSelector:withMethod: 
 	False
        'and:
        (lambda (self thunk) st-false)) ;; self

(addSelector:withMethod: 
 	True
        'or: ;; logical or (short circuit)
        (lambda (self thunk) st-true)) ;; self

(addSelector:withMethod: 
 	False
        'or:
        (lambda (self thunk) (thunk))) 

(addSelector:withMethod: 
 	True
        'xor: ;; logical xor (full)
        (lambda (self aBoolean) (not aBoolean)))

(addSelector:withMethod: 
 	False
        'xor:
        (lambda (self aBoolean) aBoolean)) 

(addSelector:withMethod: 
 	True
        'not  ;; logical negation
        (lambda (self) st-false))

(addSelector:withMethod: 
 	False
        'not
        (lambda (self) st-true))

(addSelector:withMethod: 
 	True
        'ifFalse:   ;;; NB: nil, NOT #false !
        (lambda (self alternativeBlock) st-nil)) 

(addSelector:withMethod: 
 	False
        'ifFalse:
        (lambda (self alternativeBlock) (alternativeBlock)))

(addSelector:withMethod: 
 	True
        'ifTrue:
        (lambda (self alternativeBlock) (alternativeBlock))) 

(addSelector:withMethod: 
 	False
        'ifTrue:   ;;; NB: nil, NOT #false !
        (lambda (self alternativeBlock) st-nil)) 

(addSelector:withMethod: 
 	True
        'ifTrue:ifFalse:
        (lambda (self  consequentBlock alternativeBlock) (consequentBlock))) 

(addSelector:withMethod: 
 	False
        'ifTrue:ifFalse:
        (lambda (self consequentBlock alternativeBlock) (alternativeBlock))) 

(addSelector:withMethod: 
 	True
        'ifFalse:ifTrue:
        (lambda (self consequentBlock alternativeBlock) (alternativeBlock))) 

(addSelector:withMethod: 
 	False
        'ifFalse:ifTrue:
        (lambda (self consequentBlock alternativeBlock) (consequentBlock))) 

(addSelector:withMethod:
        Boolean
        'eqv:  ;; boolean equivalence
        (lambda (self aBool) (eq? self aBool))) ;; ^(self == aBool)

(addSelector:withMethod:
        Boolean
        'storeOn:
        (lambda (self aStream)
          (perform:with: self 'printOn: aStream)))

(define (st-false? obj) (boolean=? obj st-false))
(define (st-true?  obj) (boolean=? obj st-true))

(addSelector:withMethod:
        Boolean
        'and:and:
        (lambda (self block1 block2)
          (cond
           ((st-false? self) self) ;; #false
           ((st-false? (block1)) st-false)
           ((st-false? (block2)) st-false)
           (else st-true))))
          
(addSelector:withMethod:
        Boolean
        'and:and:and:
        (lambda (self block1 block2 block3)
          (cond
           ((st-false? self) self) ;; #false
           ((st-false? (block1)) st-false)
           ((st-false? (block2)) st-false)
           ((st-false? (block3)) st-false)
           (else st-true))))

(addSelector:withMethod:
        Boolean
        'and:and:and:and:
        (lambda (self block1 block2 block3 block4)
          (cond
           ((st-false? self) self) ;; #false
           ((st-false? (block1)) st-false)
           ((st-false? (block2)) st-false)
           ((st-false? (block3)) st-false)
           ((st-false? (block4)) st-false)
           (else st-true))))


(addSelector:withMethod:
        Boolean
        'or:or:
        (lambda (self block1 block2)
          (cond
           ((st-true? self) self) ;; #true
           ((st-true? (block1)) st-true)
           ((st-true? (block2)) st-true)
           (else st-false))))

(addSelector:withMethod:
        Boolean
        'or:or:or:
        (lambda (self block1 block2 block3)
          (cond
           ((st-true? self) self) ;; #true
           ((st-true? (block1)) st-true)
           ((st-true? (block2)) st-true)
           ((st-true? (block3)) st-true)
           (else st-false))))

(addSelector:withMethod:
        Boolean
        'or:or:or:or:
        (lambda (self block1 block2 block3 block4)
          (cond
           ((st-true? self) self) ;; #true
           ((st-true? (block1)) st-true)
           ((st-true? (block2)) st-true)
           ((st-true? (block3)) st-true)
           ((st-true? (block4)) st-true)
           (else st-false))))



;;;Override instance creation methods

(addSelector:withMethod: 
 	(class Boolean)
        'basicNew:
        (lambda (self size)
          (error "You may not create any more Booleans - this is two-valued logic" self)))

(addSelector:withMethod: 
 	(class Boolean)
        'new
        (lambda (self)
          (error "You may not create any more Booleans - this is two-valued logic" self)))

(addSelector:withMethod: 
 	(class Boolean)
        'new:
        (lambda (self size)
          (error "You may not create any more Booleans - this is two-valued logic" self)))

(addSelector:withMethod: 
 	(class  UndefinedObject)
        'basicNew:
        (lambda (self size)
          (error "You may not create any more undefined objects--use nil" self)))

(addSelector:withMethod: 
 	(class UndefinedObject)
        'new
        (lambda (self)
          (error "You may not create any more undefined objects--use nil" self)))

(addSelector:withMethod: 
 	(class UndefinedObject)
        'new:
        (lambda (self size)
          (error "You may not create any more undefined objects--use nil" self)))





;; (provide 'st-boolean)

;;;			--- E O F ---			;;;
