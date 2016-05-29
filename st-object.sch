;;; FILE: "st-object.sch"
;;; IMPLEMENTS: Basic Smalltalk objects
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016

;; (require 'st-kernel)

;;; Classes ProtoObject Object
;;@@ FIXME: ProtoObject NYI

;; @@FIXME: make continuable -- use Message object
(define (doesNotUnderstand: self selector)

;; NOTE: (make-message-send self selector rest-args)
;; @@FIXME: Conditions

  (error (string-append
               "#"
               (symbol->string selector)
                " not understood by ")
         self)
 )
  

;; All methods in Smalltalk have an exact number of arguments
;; The 'receiver' of the message is #self,
;;   so all messages have at least one argument
;; The most common usages have the fewest arguments

(define (perform: self selectorSym)
  ((lookupSelector: self selectorSym) self))

(define (perform:with: self selectorSym arg)
  ((lookupSelector: self selectorSym) self arg))

(define (perform:with:with: self selectorSym arg1 arg2)
  ((lookupSelector: self selectorSym) self arg1 arg2))

(define (perform:with:with:with: self selectorSym arg1 arg2 arg3)
  ((lookupSelector: self selectorSym) self arg1 arg2 arg3))

(define (perform:withArguments: self selectorSym argsArray)
  ;; @@FIXME: Check argsArry is a Smalltalk Array object..
  (apply (lookupSelector: self selectorSym)
         (cons self (cddr (vector->list argsArray)))))

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:    ;; ANSI
        perform:)

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:with:	 ;; ANSI
        perform:with:)

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:with:with:  ;; ANSI
        perform:with:with:)

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:with:with:with:  ;; ANSI
        perform:with:with:with:)

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:withArguments:   ;; ANSI
        perform:withArguments:)

(primAddSelector:withMethod:
 	st-object-behavior
        'doesNotUnderstand:    ;; ANSI
        doesNotUnderstand:)


(primAddSelector:withMethod:
 	st-object-behavior
        'respondsTo:    ;; ANSI
        (lambda (self selector)
          (primIncludesSelector: (behavior self) selector)))

(primAddSelector:withMethod:
 	st-object-behavior
        '==    ;; ANSI
        (lambda (self other) (eq? self other)))

(primAddSelector:withMethod:
 	st-object-behavior
        '~~    ;; ANSI
        (lambda (self other) (not (eq? self other))))

(primAddSelector:withMethod:
 	st-object-behavior
        '=   ;; ANSI
        (lambda (self other) (eqv? self other)))

(primAddSelector:withMethod:
 	st-object-behavior
        '~=   ;; ANSI
        (lambda (self other) (not (eqv? self other))))

;; @@ copy   -- ANSI
;; @@ doesNotUnderstand:  -- ANSI
;; @@ error:  -- ANSI
;; @@ hash -- ANSI
;; @@ identityHash -- ANSI
;; @@ printOn: -- ANSI

(primAddSelector:withMethod:  ;;  @@FIXME: bogus
 	st-object-behavior
        'printString   ;; ANSI
        ;; String streamContents: [:s | self printOn: s]
        (lambda (self)
          (string-append "an "
                         (perform: (perform: self 'class) 'name)))
)

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'isKindOf:
        (lambda (self someClass)
          (let ( (my-class (perform: self 'class)) )
            (if (eq? my-class someClass)
                #true
                (let loop ( (super-class (perform: someClass 'superClass)) )
                  (cond
                   ((null? super-class) #false)
                   ((eq? my-class super-class) #true)
                   (else (loop (perform: super-class 'superClass))))
        ) ) ) )
)

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'isMemberOf:
        (lambda (self someClass)
          (eq? (perform: self 'class) someClass))
)

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'notNil
        (lambda (self)
          (not (st-nil? self)))
)

(primAddSelector:withMethod:  ;; base case
 	st-object-behavior
        'basicSize
        (lambda (self) 0)
)

(primAddSelector:withMethod:
 	st-object-behavior
        'yourself    ;; ANSI
        (lambda (self) self)
)


(primAddSelector:withMethod:
 	st-object-behavior
        'initialize
        (lambda (self) self))



;;;			--- E O F ---			;;;
