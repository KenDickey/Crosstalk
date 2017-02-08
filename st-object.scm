;;; FILE: "st-object.scm"
;;; IMPLEMENTS: Basic Smalltalk object behavior
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016

;; (require 'st-kernel)

;;; Note that class Object is defined in "core-classes.scm"
;;; which requires this file.

;; @@FIXME: pre-Smalltalk namespace

(define true  #true)
(define false #false)
(define nil   '())

;; @@FIXME: make continuable -- use Message object
(define (doesNotUnderstand: self selector) ;; ANSI

;; NOTE: (make-message-send self selector rest-args)
;; @@FIXME: Conditions

  (error (string-append
               "#"
               (symbol->string selector)
                " not understood by ")
         self)
 )


(define make-subclassResponsibility
  (lambda (selector)
    (let ( (err-msg
             (string-append
              "My subclass should have overridden "
              (symbol->string selector)))
         )
      (lambda (self)
        (error err-msg
               self
               selector)))
) )


(define (subclassResponsibility self)
  (error "My subclass should have overwridden this method" self))

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

(define (perform:with:with:with:with:
         self selectorSym arg1 arg2 arg3 arg4)
  ((lookupSelector: self selectorSym) self arg1 arg2 arg3 arg4))

(define (perform:withArguments: self selectorSym argsArray)
  (apply (lookupSelector: self selectorSym)
         (cons self (vector->list argsArray))))

(define (perform:withArgsList: self selectorSym argsList)
  (apply (lookupSelector: self selectorSym)
         (cons self argsList)))

(define (perform:withArguments:inSuperclass:
           self selectorSym argsArray superClass)
  (apply (lookupSelector: superClass selectorSym)
         (cons self (vector->list argsArray))))


(define (superPerform: self selectorSym)
  ((superLookupSelector: self selectorSym) self))

(define (superPerform:with: self selectorSym arg)
  ((superLookupSelector: self selectorSym) self arg))

(define (superPerform:with:with: self selectorSym arg1 arg2)
  ((superLookupSelector: self selectorSym) self arg1 arg2))

(define (superPerform:with:with:with: self selectorSym arg1 arg2 arg3)
  ((superLookupSelector: self selectorSym) self arg1 arg2 arg3))

(define (superPerform:with:with:with:with:
         self selectorSym arg1 arg2 arg3 arg4)
  ((superLookupSelector: self selectorSym) self arg1 arg2 arg3 arg4))

(define (superPerform:withArguments: self selectorSym argsArray)
  (apply (superLookupSelector: self selectorSym)
         (cons self (vector->list argsArray))))

(define (superPerform:withArgsList: self selectorSym argsList)
  (apply (superLookupSelector: self selectorSym)
         (cons self argsList)))

;;; Shorter Syntax
(define $     perform:)
(define $:    perform:with:)
(define $::   perform:with:with:)
(define $:::  perform:with:with:with:)
(define $:::: perform:with:with:with:with:)
(define $&    perform:withArguments:) ;; args array
(define $*    perform:withArgsList:)  ;; args list

(define @     superPerform:)
(define @:    superPerform:with:)
(define @::   superPerform:with:with:)
(define @:::  superPerform:with:with:with:)
(define @:::: superPerform:with:with:with:with:)
(define @&    superPerform:withArguments:) ;; args array
(define @*    superPerform:withArgsList:)  ;; args list

;;;

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
        'perform:withArgsList:   ;; Scheme
        perform:withArgsList:)

(primAddSelector:withMethod:
 	st-object-behavior
        'perform:withArguments:inSuperclass:
        perform:withArguments:inSuperclass:)

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

(primAddSelector:withMethod:
 	st-object-behavior
        'hash   ;; ANSI
        equal-hash)

(primAddSelector:withMethod:
 	st-object-behavior
        'identityHash   ;; ANSI
        object-hash)

(primAddSelector:withMethod:
 	st-object-behavior
        'printString   ;; ANSI
        printString)

(primAddSelector:withMethod:
 	st-object-behavior
        'printOn:  ;; ANSI
        (lambda (self outport)
          (if (perform:with: self 'respondsTo: 'name)
              (display (perform: self 'name) outport)
              (format outport
                      "<instance of ~a>"
                      (perform: (perform: self 'class)'name))
       ) )

)

(define (isKindOf: self someClass)
  (let loop ( (super-class (perform: self 'class)) )
    (cond
     ((null? super-class) #false)
     ((eq? super-class someClass) #true)
     (else (loop (perform: super-class 'superclass))))
) )

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'isKindOf:
        isKindOf:
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

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'isNil
        (lambda (self)
          (st-nil? self))
)

(primAddSelector:withMethod:  ;; base case
 	st-object-behavior
        'basicSize ;; number of indexable slots in basic object
        (lambda (self)
          (cond
            ((vector? self)
             (if (and (< 1 (vector-length self))
                      (eq? %%st-object-tag%% (st-obj-tag self)))
                 (- (vector-length self) num-header-slots)
                 (vector-length self))
             )
            ((string? self)     (string-length self))
            ((symbol? self)     (symbol-length self))
            ((bytevector? self) (bytevector-length self))
            (else 0)
        ) )
)

(primAddSelector:withMethod:
 	st-object-behavior
        'shallowCopy  
        ;; A shallow copy shares slot-values
        (lambda (self) (vector-copy self))
)

(primAddSelector:withMethod:
 	st-object-behavior  ;; #copy -- ANSI
        'copy  ;; NB: Subclasses should override #copy
               ;; NOT #basicCopy
        (lambda (self) (perform: self 'basicCopy))
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

(primAddSelector:withMethod:
 	st-object-behavior
        'basicCopy
        (lambda (self)
          (cond
            ;; NB: vector-copy works for all St objects
            ((vector? self)     (vector-copy self)) 
            ((string? self)     (string-copy self))
            ((symbol? self)     (symbol-copy self))
            ((procedure?  self) (procedure-copy  self))
            ((bytevector? self) (bytevector-copy self))
            ((hashtable?  self) (hashtable-copy  self))
            ((list? self)       (list-copy self)) ;; @@ Not St; Error?
            ;;@@ environment, port, ..?
            (else self) ;; immediates not copyable!
        ) )
)

(primAddSelector:withMethod:
 	st-object-behavior
        'error:  ;; ANSI
        (lambda (self aString)
          (error aString self))) ;;; @@FIXME: Debug!

;; From PharoCandle.  Don't know why these are in Object?!?
(primAddSelector:withMethod:
 	st-object-behavior
        'putAscii:
        (lambda (self asciiValue)
          (display (integer->char asciiValue))))

(primAddSelector:withMethod:
 	st-object-behavior
        'putString:
        (lambda (self aString)
          (display aString))) ;;@@FIXME: St->Scheme




;;;@@FIXME; to do:
; #instVarAt: #instVarAt:put:
; #handleExceptionName:context:
; #become: #pointsTo: 
; #tryPrimitive:withArgs:


;;;			--- E O F ---			;;;
