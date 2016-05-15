;;; FILE: "st-object.sch"
;;; IMPLEMENTS: Basic Smalltalk objects
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016

;; (require 'st-kernel)

;;; Classes ProtoObject Object


(define st-object
  (vector %%st-object-tag%% st-object-behavior))

;; @@FIXME: make continuable -- use Message object
(define (doesNotUnderstand: self selector)
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
  ;; @@FIXME: Checl argsArry is a Smalltalk Array object..
  (apply (lookupSelector: self selectorSym)
         (cons self (cddr (vector->list argsArray)))))

(addSelector:withMethod:
 	st-object-behavior
        'perform:
        perform:)

(addSelector:withMethod:
 	st-object-behavior
        'perform:with:
        perform:with:)

(addSelector:withMethod:
 	st-object-behavior
        'perform:with:with:
        perform:with:with:)

(addSelector:withMethod:
 	st-object-behavior
        'perform:with:with:with:
        perform:with:with:with:)

(addSelector:withMethod:
 	st-object-behavior
        'perform:withArguments:
        perform:withArguments:)

(addSelector:withMethod:
 	st-object-behavior
        'doesNotUnderstand:
        doesNotUnderstand:)

(addSelector:withMethod:
 	st-object-behavior
        'class
        (lambda (self) 'object))

(addSelector:withMethod:
 	st-object-behavior
        '==
        (lambda (self other) (eq? self other)))

;; TEST
(addSelector:withMethod:
 	st-object-behavior
        'with:with:with:with:with
        (lambda (self a1 a2 a3 a4 a5) (list a1 a2 a3 a4 a5)))

; (perform: st-object 'class)
; (perform: st-object 'ugly)
; (perform:with: st-object '== #f)
; (perform:with: st-object '== (vector '(object) st-nil 3))
; (perform:with: st-object '== st-object)
; (perform:with: st-object 'bogus: 37)
; (perform:withArguments: st-object 'with:with:with:with:with (vector %%st-object-tag%% st-nil #t #f '() 1 #\c))
; (perform:with:with: st-object 'perform:with: '== st-object)


;;;			--- E O F ---			;;;
