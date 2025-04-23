#!r6rs
;;; File: "st-core-mechanics.sls"
;;; IMPLEMENTS: Core Smalltalk object mechanics
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;;; We will start with object behaviors
;;; A behavior is just a (method) dictionary / hashtable
;;; Each method takes a 'self' first argument and has
;;; a fixed arity.  Methods know their name and arity.

;;; Number >> between:and: [ self min max ]
;;;  ==> in method dictionary for class Number:
;;; 'between:and: -> (lambda (self min max) (<= min self max))

;;; Use Scheme immediates, vectors, bytevectors, closures ..
;;; See (define (behavior thing) ...) below.

;;; Absent way to name new data types, Smalltalk objects
;;; are just Scheme Vectors were the 1st slot contains
;;; a method dictionary including a binding of
;;; 'class -> (lambda (self) <class>)

(library (st-core-mechanics)

  (export
   
;;; Namespace
   
   Smalltalk 
   smalltalkAt:
   smalltalkAt:put:

;;; Basic access & lookup

   lookupSelector:	; (lookupSelector: obj 'selectorSymbol)
   send-failed		; (send-failed receiver selector rest-args)
   basicNew:		; (basicNew: classSelf num-added-vars)
   setClass:
   addSubclass:
   printString
   class
   superclass
   isKindOf:
   respondsTo:
   doesNotUnderstand:	; (doesNotUnderstand: self selector)
   make-subclassResponsibility ; (make-subclassResponsibility selector)
   subclassResponsibility      ; (subclassResponsibility self) -> error
   asException		       ; (asException aSchemeCondition)
   unspecified unspecified?
   printString
   allInstVarNames
   allSuperclasses
   allSubclasses
;; 'prim' prefix -> operates on behavior/hashtable
   primLookup:		; (primLookup: methodDict selector)
   primSet:toValue:     ; (primSet:toValue: methodDict selector method)
   primNew:		; (primNew: classSelf num-object-slots)
   primSetClass:
   primSelectorsDo:
   primSelectorsAndMethodsDo:
   primMethodsDo:
   primAddSelector:withMethod:
   primAddSelector:withMethod:arity:
   primSelectors
   

;;; Perform and short-form aliases

   $     perform:
   $:    perform:with:
   $::   perform:with:with:
   $:::  perform:with:with:with:
   $:::: perform:with:with:with:with:
   $&    perform:withArguments: ;; args array
   $*    perform:withArgsList:  ;; args list
   perform:withArguments:inSuperclass:

;;;   "Send to Super"

   %     superPerform:
   %:    superPerform:with:
   %::   superPerform:with:with:
   %:::  superPerform:with:with:with:
   %:::: superPerform:with:with:with:with:
   %&    superPerform:withArguments: ;; args array
   %*    superPerform:withArgsList:  ;; args list
   
;;; Smalltalk Object Representation

   st-object?
   make-st-object
   st-obj-copy
   make-st-bytearray
   num-header-slots
   behavior ;; (behavior object) -> answers a method dictionary
;;; mechanics
   add-getters&setters ;; internal
   add-array-accessors ;; internal
   st-obj-behavior	; (st-obj-behavior obj)
   st-obj-behavior-set!	; (st-obj-behavior-set! obj new-behavior)
   st-object-length	; (st-object-length obj)
   ;;			;  -> # vector slots less header
   addSelector:withMethod:
   addSelector:withMethod:arity:   

;;; Basic Objects

   st-nil st-true st-false
   st-nil?

;;; Method/Behavior Dictionaries

   method-dictionary?
   make-method-dictionary
   method-dictionary-size
   method-name
   method-arity
   clone-behavior	;; alias for  clone-method-dictionary
   clone-method-dictionary 
;;; Internal Mechanics
   rebase-mdict! 	   ; (rebase-mdict! aClass st-*-behavior)
   insure-annotated ;; closure->method
   annotate-procedure-with-arity 
   selector-arity
;;; Do
   primSelectorsDo:	; (primSelectorsDo: methodDict closure)
   primSelectorsAndMethodsDo:
             ; (primSelectorsAndMethodsDo: methodDict closure)
   primMethodsDo:	 ; (primMethodsDo: methodDict closure)



;;; Method Dictionaries for supported Scheme objects
   
   st-nil-behavior
   st-true-behavior
   st-false-behavior
   st-magnitude-behavior
   st-number-behavior
   st-integer-behavior
   st-complex-behavior
   st-float-behavior
   st-fraction-behavior
   st-character-behavior
   st-string-behavior
   st-symbol-behavior
   st-array-behavior
   st-list-behavior
   st-bytearray-behavior
   st-blockClosure-behavior
   st-object-behavior
   st-byte-stream-behavior
   st-char-stream-behavior
;;   st-date+time
;;   st-time-behavior
;;   st-duration-behavior
   st-condition-behavior
   st-dictionary-behavior
   st-identity-dictionary-behavior
;;; for DNU
   st-messageSend-behavior
;;; Other bootstrap
   st-behavior-behavior
   st-classDescription-behavior
   st-class-behavior
   st-metaClass-behavior

;;; Debug helpers (interactive)

   smalltalk-keys	; (selectors Smalltalk)
   selectors		; (selectors obj) -> method names
   name			; (name 3) -> 'Integer
   className		; (className thing) ~> (name (class thing))
   display-selectors	; (display-selectors obj) -> selectors of (behavior obj)
   inst-method-names    ; (inst-method-names aClass) -> ($ aClass 'myMethodNames)
   display-ivars	; (display-ivars st-obj)
   display-obj		; obj printString
   describe		; (describe object)
   display-allSupers	; (display-allSupers obj)
   display-subclasses	; (display-subclasses class)
   safer-printString

;;; Various internal Helpers   
   list-copy vector-copy
   every? any?
   symbol<?
   bytevector-ref
   bytevector-set!
   append-no-duplicates
   format
   )

  (import
   (rnrs base)
   (rnrs bytevectors (6))
   (rename (rnrs bytevectors (6))
    (bytevector-u8-ref  bytevector-ref)
    (bytevector-u8-set! bytevector-set!)
    )
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs records inspection (6))
   (rnrs records procedural (6))
   (rnrs conditions (6))   
   (rnrs sorting (6))
   (rnrs unicode (6))
   (rnrs hashtables (6))
      ;;; Method Dictionarys are Scheme hashtables
   (rename ;; Syntactic sugar tastes sweeter 8^O
    (rnrs hashtables (6))
    (make-eq-hashtable	 make-method-dictionary)
    (hashtable?		 method-dictionary?)
    (hashtable-size	 method-dictionary-size)
    (hashtable-set!	 primSet:toValue:)
    (hashtable-contains? primIncludesSelector:)
    )
   (only (chezscheme)
         format
         make-parameter
         parameterize
         print-level
         procedure-arity-mask
         make-arity-wrapper-procedure
         wrapper-procedure-data
         wrapper-procedure?
         open-output-string
         get-output-string
         vector-copy
         void
       )
   )


;;; A few Basic Objects

(define unspecified (void))
(define (unspecified? value) (eq? value unspecified))

(define st-nil      '())
(define st-true     #t)
(define st-false    #f)

(define (st-nil? val)
  (or (null? val) (unspecified? val)))

;;; ============================================
;;; The Smalltalk Global Environment/Namespace
;;; ============================================

(define Smalltalk (make-eq-hashtable))

(define (smalltalkAt: aSymbol)
  (hashtable-ref Smalltalk aSymbol st-nil))

(define (smalltalkAt:put: aSymbol aValue)
  (primSet:toValue: Smalltalk aSymbol aValue))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (smalltalk-keys) ;; sorted
  (vector-sort
   symbol<?
   (hashtable-keys Smalltalk)))


;;; ============================================
;;; Message lookup
;;; ============================================
  
(define (lookupSelector: self selectorSym) ;; Polymorphic
  (primLookup: (behavior self) selectorSym))

;; methodDict primLookup: aSymbol
(define (primLookup: methodDict symbol)
  (hashtable-ref methodDict
                 symbol
                 (lambda (self . rest-args)
                   (send-failed self symbol rest-args)))
                   ;; (make-messageSend self symbol rest-args)))
)

(define (safer-printString obj)
  (cond 
   ((respondsTo: obj 'printOn:)
    (perform: obj 'printString)
    )
   ((respondsTo: obj 'class)
    (let ( (class (perform: obj 'class)) )
      (cond
       ((or (symbol? class) (string? class))
        (format #f "<instance of #~a>" class)
        )
       ((respondsTo: class 'name)
        (format #f "<instance of #~a>" (perform: class 'name)))
       (else (format #f "~a" obj)))
      ))
   (else "#<object>")
   )
)


(define (send-failed receiver selector rest-args)
  (let ( (messageSend (make-messageSend receiver selector rest-args)) )
    (if (respondsTo: messageSend 'sendFailed)
    	($ messageSend 'sendFailed)
        (error 'send-failed
               (format #f
                       "**Failed message send: #~a to: ~a"
                       selector
                       (safer-printString receiver))
               rest-args)
) ) )

(define (make-messageSend receiver selector args-list)
  ;; args list was captured by a .rest
  (vector st-messageSend-behavior
          receiver
          selector
          (ensure-st-array args-list)))

(define list->st-array list->vector)

(define (ensure-st-array args)
  ;; args are a list captured by a .rest
  ;; Flatten & vectorise into st-array
  ;; Last element may be a Scheme vector,
  ;;  if so, add as a list at end..
  (let* ( (rev-args (reverse args))
          (reversed-args
           (cond
            ((null? rev-args) '())
            ((vector? (car rev-args))
             (append ;; spread
              (reverse (vector->list (car rev-args)))
              (cdr rev-args))
             )
            (else rev-args)))
         )
    (list->st-array (reverse reversed-args))
) )

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



;;; Shorter Syntax
(define $     perform:)
(define $:    perform:with:)
(define $::   perform:with:with:)
(define $:::  perform:with:with:with:)
(define $:::: perform:with:with:with:with:)
(define $&    perform:withArguments:) ;; args array
(define $*    perform:withArgsList:)  ;; args list


;;; "send to super"
;
; Message sent to a superclass method may invoke a message to
; its superclass method, so one has to track the meaning of
; "super" to allow multi-level super dispatch.
;
; Given multithreading, multiple comtexts may be active, so
; one cannot use, e.g. a hidden ivar, as the same object may
; me active in multiple threads.
;
; Two strategies are
;  [1] Use an object wrapper, which would be per-thread to
; keep track of super.
;  [2] Use a dynamic thread-local variable to keep track of super.
;
; The strategy here is [2], using Scheme's parameter objects with
; an association-list of (.. (obj super) ..) as per-thread stacks.
; Per-thread stacks are required as multiple send-to-super's may be
; used within a given thread.
;
; [2] has less mechanical involvement with dispatch where it is
; not used.  So send-to-super pays no cost where it is not used.

(define super-chain-alist (make-parameter '()))

(define (next-super-for obj)
  ;; Answer next superclass for obj
  (superclass
   (cond
    ((assq obj (super-chain-alist)) => cdr)
    (else (class obj))))
)

(define (superPerform: self selectorSym)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
      ((primLookup: ($ the-super 'methodDict) selectorSym) self))))

(define (superPerform:with: self selectorSym arg)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
  ((primLookup: ($ the-super 'methodDict) selectorSym) self arg))))

(define (superPerform:with:with: self selectorSym arg1 arg2)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
  ((primLookup: ($ the-super 'methodDict) selectorSym) self arg1 arg2))))

(define (superPerform:with:with:with: self selectorSym arg1 arg2 arg3)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
  ((primLookup: ($ the-super 'methodDict) selectorSym) self arg1 arg2 arg3))))

(define (superPerform:with:with:with:with:
         self selectorSym arg1 arg2 arg3 arg4)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
  ((primLookup: ($ the-super 'methodDict) selectorSym) self arg1 arg2 arg3 arg4))))

(define (superPerform:withArguments: self selectorSym argsArray)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
      (apply (primLookup: ($ the-super 'methodDict) selectorSym)
             (cons self (vector->list argsArray))))))

(define (superPerform:withArgsList: self selectorSym argsList)
  (let ( (the-super   (next-super-for self))
         (super-alist (super-chain-alist))
       )
    (parameterize ( (super-chain-alist
                     (cons (cons self the-super)
                           super-alist))
                  )
      (apply (primLookup: ($ the-super 'methodDict) selectorSym)
             (cons self argsList)))))

;;; Shorter Syntax
(define %     superPerform:)
(define %:    superPerform:with:)
(define %::   superPerform:with:with:)
(define %:::  superPerform:with:with:with:)
(define %:::: superPerform:with:with:with:with:)
(define %&    superPerform:withArguments:) ;; args array
(define %*    superPerform:withArgsList:)  ;; args list


;; shortcuts
(define (class      obj) (perform: obj 'class))
(define (superclass obj) (perform: obj 'superclass))

(define (isKindOf: self someClass)
  (let loop ( (ancestor-class (perform: self 'class)) )
    (cond
     ((null? ancestor-class) #f)
     ((eq? ancestor-class someClass) #t)
     ((not (st-object? ancestor-class)) #f)
     (else (loop (perform: ancestor-class 'superclass))))
) )

(define (respondsTo: self selector)
  (primIncludesSelector: (behavior self) selector))

(define (describe obj)
  (cond
   ((null? obj) (display "nil")
    )
   ((list? obj)
    (display "a list of length ")
    (display (length obj))
    )
   ((st-object? obj)
    (display (safer-printString obj))
    )
   ((vector? obj)
    (display "an array of length ")
    (display (vector-length obj))
    )
   ((bytevector? obj)
    (display "a bytevector of length ")
    (display (bytevector-length obj))
    )
   ((number? obj)
    (display "a number with value: ")
    (display obj)
    )
   ((eq? obj #t)  (display "true")
    )
   ((eq? obj #f) (display "false")
    )
   ((string? obj)
    (display "a string of length ")
    (display (string-length obj))
    )
   ((symbol? obj)
    (display "a symbol of length ")
    (display (string-length (symbol->string obj)))
    )
   ((char? obj)
    (display "$") (display obj)
    (display " is a character")
    )
   ((port? obj)
    (if (binary-port? obj)
        (display "binary ")
        (display "text "))
    (if (input-port? obj)
        (display "input Stream")
        (display "output Stream"))
    )  
   ;; @@FIXME: ...
   (else (format #t "~a" obj)) ;; procedures..
   )
  (newline)
 )


(define (name thing)
  (if (respondsTo: thing 'name)
      ($ thing 'name)
      (describe thing)))

(define (className thing)
  (cond
   ((respondsTo: thing 'class)
    (let ( (thing-class ($ thing 'class)) )
      (if (respondsTo: thing-class 'name)
        ($ thing-class 'name)
        (format #f "~a" thing-class)))
    )
   (else "#<classless Object>")))

;;;
;;; Smalltalk BlockClosures know their name and arity..
;;;

(define (method-arity blockClosure)
  ;; "thing foo: 3"  ==> ((lambda (self x) ...) thing 3)
  ;; so reduce procedure arity by 1 to get method arity
  (- (exact (log (procedure-arity-mask blockClosure) 2))
     1 ))

(define (arity->mask anInteger) (expt 2 anInteger))

(define (selector-arity selector-symbol)
  ;; selector symbols have a colon for each argument
  ;; 'foo	-> 0
  ;; 'foo:	-> 1
  ;; 'foo:bar:	-> 2
  (let* ( (char-vec (symbol->string selector-symbol))
          (num-chars (string-length char-vec))
        )
    (let loop ((count 0) (index 0))
      (cond
       ((>= index num-chars) count)
       ((char=? #\: (string-ref char-vec index))
        (loop (+ count 1)(+ index 1)))
       (else (loop count (+ index 1)))))))

(define (annotate-procedure proc name-symbol)
  ;; Take a Scheme closure & wrap to annotate
  ;; with method selector and arity.
  ;; Add 1 for hidden SELF first argument.
  (make-arity-wrapper-procedure
   proc
   (arity->mask (+ 1 (selector-arity name-symbol)))
   name-symbol))

(define (annotate-procedure-with-arity proc name-symbol arity)
  ;; Some selectors (e.g. < , <=) do NOT have colons
  ;; which can be counted to know their arity.
  ;; RAW Arity: self arg counted, so min arity is 1
  (make-arity-wrapper-procedure
   proc
   (arity->mask arity)
   name-symbol))
  

(define (method-name method)
  (wrapper-procedure-data method))

(define (insure-annotated procOrMethod selectorSymbol)
  (if (wrapper-procedure? procOrMethod)
      procOrMethod
      (annotate-procedure procOrMethod selectorSymbol)))

;;; methodDict primAddSelector: selector withMethod: compiledMethod
(define (primAddSelector:withMethod: methodDict selector methodClosure)
  (if (not (procedure? methodClosure))
      (error 'primAddSelector:withMethod:
             "Methods must be closures"
             methodClosure))

  (primSet:toValue: methodDict
                    selector
                    (insure-annotated methodClosure selector)
                    ))

;; used for > == ~= etc with no colons
(define (primAddSelector:withMethod:arity:
         methodDict selector methodClosure arity)
  (primAddSelector:withMethod:
   methodDict
   selector
   (annotate-procedure-with-arity methodClosure selector arity)))


;; methodDict selectors
(define (primSelectors methodDict) (vector->list (hashtable-keys methodDict)))

;(define primIncludesSelector: hashtable-contains?)

(define (primSelectorsDo: methodDict closure)
  (vector-for-each closure (hashtable-keys methodDict)))

(define (primSelectorsAndMethodsDo: methodDict closure)
  (let-values ( ((selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure selectors methods)))

(define (primMethodsDo: methodDict closure)
  (let-values ( ((ignored-selectors methods)
                 (hashtable-entries methodDict)) )
    (vector-for-each closure methods))) 

(define (clone-method-dictionary mDict)
   (hashtable-copy mDict #t)) ;; ensure mutable

(define clone-behavior clone-method-dictionary) ;; shorter to type


;;; ============================================
;;; Smalltalk Object Representation
;;; ============================================

(define st-obj-behavior-index 0) ;; 1st slot in a st-object

(define num-header-slots 1) ;; behavior

(define (st-obj-behavior obj)
  (vector-ref obj st-obj-behavior-index))

(define (st-obj-behavior-set! obj new-behavior)
  (vector-set! obj st-obj-behavior-index new-behavior))

(define (st-object? thing)  ;;@@FIXME: Should be New Datatype
  (and (vector? thing)
       (< 0 (vector-length thing))
       (hashtable? (vector-ref thing 0))))

(define (st-object-length obj)
  ;; @@NB: Unchecked
  (- (vector-length obj) 1))


;;; Generic ST Object representation:
;;; (vector:  behavior | optional-named-slots.. | optional-indexed-slots.. )


(define (make-st-object behavior num-object-slots)
  (let ( (st-obj (make-vector
                   (+ num-header-slots num-object-slots)
                   st-nil)) ;; init slots to UndefinedObject
       )
    (vector-set! st-obj st-obj-behavior-index behavior)
    st-obj)
)


;; Scheme immediates, vector, bytevector

(define (make-st-bytearray numBytes initialValue)
  (let ( (initVal (if (and (integer? initialValue)
                           (<= 0 initialValue 255))
                      initialValue
                      0)) ;; error?
       )
    (make-bytevector numBytes initVal)
) )
  
;; Done at class creation
;; NB: start-index includes num-header-slots, which is the minimum index
(define (add-getters&setters behavior start-index slot-names-list)
  (let loop ( (index start-index) (slot-names slot-names-list) )
    (if (null? slot-names)
        'done
        (let* ( (getter-name (car slot-names))
                (setter-name
                 (string->symbol
                  (string-append
                   (symbol->string getter-name) ":")))
                )
;;@@DEBUG{
;; (display "[")
;; (display (number->string index))
;; (display "] -> ")
;; (display getter-name)
;; (newline)
;;}DEBUG@@
          (primAddSelector:withMethod:
           behavior
           getter-name
           (lambda (self)
             (vector-ref self index)))
          
          (primAddSelector:withMethod:
           behavior
           setter-name
           (lambda (self newVal)
             (vector-set! self index newVal)
             self))
          
          (loop (+ index 1) (cdr slot-names))))
  )
)

(define (add-array-accessors behavior start-index)
;; #at: #at:put: #basicSize #at:modify
  (let ( (pre-start (- start-index 1)) )

    (primAddSelector:withMethod:
     behavior
     'at:
     (lambda (self user-index)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-length self))
             (vector-ref self vec-index)
             (error 'at:
                    "Index out of range"
                    user-index))))
   )

    (primAddSelector:withMethod:
     behavior
     'at:put:
     (lambda (self user-index newVal)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-length self))
             (begin
               (vector-set! self vec-index newVal)
               self)
             (error 'at:put:
                    "Index out of range"
                    user-index))))
     )

    (primAddSelector:withMethod:
     behavior
     'at:modify:
     (lambda (self user-index aBlock)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-length self))
             (let ( (original-elt (vector-ref self vec-index)) )
               (vector-set! self
                            vec-index
                            (aBlock original-elt))
               self)
             (error 'at:modify:
                    "Index out of range"
                    user-index)))) 
     )

    (primAddSelector:withMethod:
     behavior
     'basicSize
     (lambda (self)
       (- (vector-length self) start-index))
     )
) )

(define (primSetClass: behavior class)
  (primSet:toValue: behavior 'class (lambda (self) class)))

(define (setClass: obj class)
  (primSetClass: (behavior obj) class))


;;; Track which methods are added to a particular class
;;;  so they are not copied over from above.
;;  See #subclassAddSelector:withMethod: below
(define (add-method-name-to-myMethods self selector)
  (let ( (old-names (perform: self 'myMethodNames)) )
    (unless (memq selector old-names)
      (perform:with: self 'myMethodNames: (cons selector old-names)))
    self
) )

;;; Subclasses inherit mDict methods from their superclass
;;;  so adding a selector_method to a class affects
;;;  its instances, NOT the class instance itself.
(define (addSelector:withMethod: classSelf selector method)
  (primAddSelector:withMethod: ($ classSelf 'methodDict)
			       selector
			       method)
  (add-method-name-to-myMethods classSelf selector) ;; def'ed here
  (subclassAddSelector:withMethod: classSelf selector method))

;;; NB: method added to methodDict of class
;;; => behavior of instances, not class itself !!
(define (subclassAddSelector:withMethod:
         classSelf selector method)
  (for-each
   (lambda (subClass)
     ;; if not overriden, copy down
     ;; Non-standard: avoids dynamic super-chain lookup
     (unless (memq selector (perform: subClass 'myMethodNames))
       (primAddSelector:withMethod: ($ subClass 'methodDict)
				    selector
				    method)
       (subclassAddSelector:withMethod: subClass selector method)))
   ($ classSelf 'subclasses))
  classSelf
)

(define (addSelector:withMethod:arity: classSelf selector method arity)
  (add-method-name-to-myMethods classSelf selector) ;; def'ed here
  (subclassAddSelector:withMethod:
   classSelf
   selector
   (annotate-procedure-with-arity method selector arity)))


;;; ============================================
;;; Behavior adds intelligence to structure
;;;    (behavior obj) answers a method-dictionary
;;;	for supported Scheme objects as well
;;;	as 'behaviors' for Smalltalk objects.
;;; ============================================


(define (behavior thing)    ;; @@FIXME: optimize dispatch
  ;; Answer method-dictionary for thing
  (case thing  
    ;; immediates -- tagtype -> err
    ((#t)	st-true-behavior)
    ((#f)	st-false-behavior)
;; eof-object -- err
    (else
     (cond  
      ((st-object? thing) (vector-ref thing st-obj-behavior-index))
      ((number?  thing)
       (cond
        ((integer? thing)  st-integer-behavior)
;;      ((rational? thing) st-fraction-behavior) ;; test for 1/3 vs 0.333
        ((real?     thing) st-float-behavior)     
        ((complex?  thing) st-complex-behavior)  
        ;; FIXME:: Scaled Decimal
        (else (error 'behavior
                     "Unknown Scheme number representation"
                     thing))
       ))
      ((vector? thing)	   st-array-behavior)
      ((string? thing)     st-string-behavior) 
      ((symbol? thing)     st-symbol-behavior) 
      ((procedure? thing)  st-blockClosure-behavior) 
      ((bytevector? thing) st-bytearray-behavior)   
      ((char?    thing)    st-character-behavior) 
      ((port? thing)
       (cond
        ((textual-port? thing) st-char-stream-behavior)
        ((binary-port?  thing) st-byte-stream-behavior)
        (else
         (error 'behavior "Wierd port: " thing)))
      )
      ((hashtable? thing)
       (if (eq? eq? (hashtable-equivalence-function thing))
           st-identity-dictionary-behavior
           st-dictionary-behavior)
       )
      ((pair? thing)  st-list-behavior)
      ;; ((time? thing)
      ;;  (cond
      ;;   ((eq? 'time-duration (time-type thing))
      ;;    st-duration-behavior
      ;;    )
      ;;   ((eq? 'time-utc (time-type thing))
      ;;    st-time-behavior
      ;;    )
        ;; 'time-tai
        ;; 'time-monotonic
        ;; 'time-thread
        ;; 'time-process
;;        (else (error "Unhandled type" (time-type thing) thing)))
;;       )
;;      ((date? thing)          st-date+time-behavior)
      ((condition? thing)     st-condition-behavior)
      ;; hashtable; other records & record types 5
      ;; @@FIXME ...
      ((or (null? thing) (eq? thing (void)))   st-nil-behavior)
      (else (error 'behavior
                   "#behavior can't deal with other Scheme types yet"
                   thing))
    ) ) )
)

;; Behaviors (method dictionaries) map selectors (symbols) to methods (closures)

;;;  Smalltalk                      Scheme
;;; anArray at: 2 put: 'foo'       (at:put: anArray 2 "foo")

;; for debug..
(define (make-mDict-placeholder classNameSym)
  (let ( (mDict (make-method-dictionary)) )
    (primAddSelector:withMethod: mDict
                             'class
                             (lambda (self) classNameSym))
    (primAddSelector:withMethod: mDict
                             'name
                             (lambda (self) classNameSym))
    mDict
) )

(define st-nil-behavior          (make-mDict-placeholder 'UndefinedObject))
(define st-true-behavior         (make-mDict-placeholder 'True))
(define st-false-behavior        (make-mDict-placeholder 'False))
(define st-magnitude-behavior    (make-mDict-placeholder 'Magnitude))
(define st-number-behavior       (make-mDict-placeholder 'Number))
(define st-integer-behavior      (make-mDict-placeholder 'Integer))
(define st-complex-behavior      (make-mDict-placeholder 'Complex))
(define st-fraction-behavior     (make-mDict-placeholder 'Fraction))
(define st-float-behavior	 (make-mDict-placeholder 'Float))
;; @@FIXME: Scaled Decimal
(define st-character-behavior    (make-mDict-placeholder 'Character))
(define st-string-behavior       (make-mDict-placeholder 'String))
(define st-symbol-behavior       (make-mDict-placeholder 'Symbol))
(define st-array-behavior        (make-mDict-placeholder 'Array))
(define st-list-behavior         (make-mDict-placeholder 'List))
(define st-bytearray-behavior    (make-mDict-placeholder 'ByteArray))
(define st-blockClosure-behavior (make-mDict-placeholder 'BlockClosure))
(define st-object-behavior       (make-mDict-placeholder 'Object))
(define st-behavior-behavior     (make-mDict-placeholder 'Behavior))
(define st-classDescription-behavior
  (make-mDict-placeholder 'ClassDescription))
(define st-class-behavior        (make-mDict-placeholder 'Class))
(define st-metaClass-behavior    (make-mDict-placeholder 'MetaClass))
(define st-messageSend-behavior  (make-mDict-placeholder 'MessageSend))
(define st-byte-stream-behavior  (make-mDict-placeholder 'ByteStream))
(define st-char-stream-behavior  (make-mDict-placeholder 'CharStream))
(define st-date+time-behavior    (make-mDict-placeholder 'DateAndTime))
(define st-time-behavior         (make-mDict-placeholder 'Time))
(define st-duration-behavior     (make-mDict-placeholder 'Duration))
(define st-condition-behavior    (make-mDict-placeholder 'Condition))
;;(define st-record-behavior     (make-mDict-placeholder 'Record))
(define st-dictionary-behavior   (make-mDict-placeholder 'Dictionary))
(define st-identity-dictionary-behavior
				(make-mDict-placeholder 'IdentityDictionary))

;;; Some basics

(define (printString obj) ;; polymorphic
;; String streamContents: [:s | self printOn: s]
  (let ( (outport (open-output-string)) )
    (perform:with: obj 'printOn: outport)
    (get-output-string outport)))

;;;======================================================
;;; Interactive/REPL debug helpers.
;;;   What do we have here?

;; What selectors does  obj  respond to?
(define (selectors obj)
  (list-sort
   symbol<?
   (primSelectors (behavior obj))))

(define (display-selectors obj)
  (display (selectors obj))
  (newline))

(define (inst-method-names class)
  (list-sort
   symbol<?
   (perform: class 'myMethodNames)))
    
(define (display-obj obj)
  (display (safer-printString obj))
  (newline))
  
;; Most useful..
(define (display-ivars st-obj)
  (if (not (st-object? st-obj))
      (begin (describe st-obj) (display " has no instance vars"))
      (let* ( (obj-class (perform: st-obj 'class))
              (ivarNames
               (if (null? obj-class)
                   '()
                   ($ obj-class 'allInstVarNames)))
           )
        (cond
         ((null? obj-class)
          (display "entity has no class!!")
          )
         (else
          (describe st-obj)
          (for-each
           (lambda (ivarName)
             (let ( (printVal
                     (safer-printString ($ st-obj ivarName)))
                  )
             (format #t "  ~s -> ~a ~%" ivarName printVal)
             ) )
           ivarNames)
          (newline)
        ) ) )
      )
  (newline)
)


(define (stringify thunk)
  (parameterize ( (current-output-port
                   (open-output-string)) )
    (thunk)
    (get-output-string (current-output-port))))



(define (doesNotUnderstand: self selector) ;; ANSI
;; NB: method redefined in "st-error-obj.scm"
  (error 'doesNotUnderstand:
         (format #f "#~a not understood by ~a"
                 selector
                 self)
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
        (error 'subclassResponsibility
               err-msg
               self
               selector)))
) )

(define (subclassResponsibility self)
  (error 'subclassResponsibility
         "My subclass should have overwridden this method"
         self))

(define (st-obj-copy st-obj)
  (unless (st-object? st-obj)
    (error 'st-object-copy
           "Not a Smalltalk object!" st-obj))
  (vector-copy st-obj)) ;; shallow copy

;;;
;;; Scheme Conditions
;;;
;; Nota Bene:
;;   Scheme compound-conditions differ from St ExceptionSets
;; They are more like a Bag.  No one-to-one mapping exists.
;; We try to do the simple thing here..

; Exception>>doesNotUnderstand: (See file "st-error-obj.scm")
; Allows conditionDict identifiers as selectors to Exceptions

(define (asException aCondition)
  (unless (condition? aCondition)
    (error 'asException
           "Non-condtion passed to #asException"
           aCondition))
  (let ( (cDict (condition->dictionary aCondition)) )
    (cond
     ((or (error? aCondition) ( message-condition? aCondition))
      (let ((receiver ($ (smalltalkAt: 'Error) 'new)))
        ($: receiver 'conditionDict: cDict)
        ($: receiver 'messageText: (hashtable-ref cDict 'message st-nil)))
      )
     ((warning? aCondition)
      (let ((receiver ($ (smalltalkAt: 'Warning) 'new)))
        ($: receiver 'conditionDict: cDict)
        ($: receiver 'messageText: (hashtable-ref cDict 'message st-nil)))
      )
     (else
      (newline)
      (display (dict->alist cDict))
      (error 'asException "asException unhandled @@NYI@@" aCondition))))
 )

(define (condition-name simple-condition)
  (record-type-name
   (record-rtd simple-condition)))

(define (condition-names condition)
  (map condition-name
       (simple-conditions condition)))

(define (simple-condition? condition)
  (and (condition? condition)
       (= 1 (length (simple-conditions condition)))))

(define (condition-name->predicate-symbol simple-condition)
  (let loop ( (source-chars
               (string->list
                (symbol->string
                 (condition-name simple-condition))))
              (dest-chars '(#\s #\i))
              (char-case-fn char-upcase)
            )
    (cond
     ((null? source-chars)
      (string->symbol
       (list->string
        (reverse dest-chars)))
      )
     ((let ( (next-char (car source-chars)) )
        (not (or (char-alphabetic? next-char)
                 (char-numeric? next-char))))
      (loop (cdr source-chars)
            dest-chars
            char-upcase)
      )
     (else
      (loop (cdr source-chars)
            (cons (char-case-fn (car source-chars))
                  dest-chars)
            char-downcase)))))

(define (condition->dictionary condition)
  (let ( (dict (make-eq-hashtable))
         (conditions
          ;; In case of same field names,
          ;; first use of field name predominates,
          ;; so add back to front..
          (reverse (simple-conditions condition)))
       )
      (for-each
       (lambda (cdn)
         (let ( (rtd (record-rtd cdn)) )
           (hashtable-set! dict
                           (condition-name->predicate-symbol cdn)
                           #t)
           (let loop ( (field-names
                        (vector->list
                         (record-type-field-names rtd)))
                       (index 0)
                     )
             (if (>= index (length field-names))
                 dict
                 (begin
                   (hashtable-set! dict
                                   (car field-names)
                                   ((record-accessor rtd index) cdn))
                   (loop (cdr field-names) (+ index 1))))
         ) ) )
         conditions)
      dict)
)

(define (dict->alist dict)
  (let-values ( ((keys-vec vals-vec)
                 (hashtable-entries dict)) )
    (vector->list (vector-map cons keys-vec vals-vec))))


(define (list-copy some-list)
  (fold-right cons '() some-list))

(define (every? proc? list)
  (if (null? list)
      #t
      (and (proc? (car list))
           (every? proc? (cdr list)))))

(define (any? proc? list)
  (if (null? list)
      #f
      (or (proc? (car list))
          (any? proc? (cdr list)))))



;;;======================================================
;; superclass           class               class class
;; ----------------------------------------------------
;; nil                  UndefinedObject  UndefinedObject class
;;  Object              Object class        MetaClass
;;    Behavior          Behavior class           "
;;      ClassDescrption ClassDescription class   "
;;         Class        Class class              "
;;         MetaClass    MetaClass class          "

(define (allInstVarNames aClass)
  (let ( (ivarNames (perform: aClass 'instanceVariables))
         (super     (perform: aClass 'superclass))
       )
    (if (st-nil? super)
        (list-copy ivarNames) ;; ANSI Smalltalk requires a fresh list
        (append (perform: super 'allInstVarNames) ivarNames))
) )

(define (allSuperclasses aClass)
  (let ( (mySuper (perform: aClass 'superclass)) )
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
  (map name (allSuperclasses obj)))

(define (display-spaces n)
  (let loop ( (count 0) )
    (when (< count n)
      (display #\space)
      (loop (+ count 1)))))

(define (display-subs class shown indent delta)
  (when (not (memq class shown))
    (newline)
    (display-spaces indent)
    (display (name class)) ;; printString
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


(define (addSubclass: classSelf subclass)
  (unless (st-nil? classSelf)
    (let ( (my-subclasses (perform: classSelf 'subclasses)) )
      (perform:with: classSelf
                     'subclasses:
                     (cons subclass
                           my-subclasses))
) ) )

; internal helper
(define (append-no-duplicates from to)
  ;; retain original order
  (let loop ( (src (reverse from)) (dest to) )
    (cond
     ((null? src) dest) ; done
     ((memq (car src) dest)
      (loop (cdr src) dest)) ; skip
     (else  ; add
      (loop (cdr src) (cons (car src) dest))))
    ) )

;;; Properly meld early bound st-*-bahavior method dictionary
;;; into newly created Class to support Scheme datatypes.
;;;   (behavior obj) returns a st-*-behavior

(define (rebase-mdict! aClass st-*-behavior)
  (let ( (local-selectors
	  (append-no-duplicates ($ aClass 'myMethodNames)
				(primSelectors st-*-behavior)))
	 )
    ($: aClass 'myMethodNames: local-selectors)
    (let-values ( ((sel-vec meth-vec)
		   (hashtable-entries ($ aClass 'methodDict)) )
		)
      (vector-for-each
       (lambda (s m) ;; copydown if not overidden
	 (unless (memq s local-selectors)
	   (hashtable-set! st-*-behavior s m)))
       sel-vec
       meth-vec))

    (primSetClass: st-*-behavior aClass)
    ($: aClass 'methodDict: st-*-behavior)
    
    aClass)
  )
    
;; earlly bound for debugging
(primAddSelector:withMethod: st-nil-behavior
			     'instanceVariables
			     (lambda (self) st-nil))

) ;; library

;;;			--- E O F ---			;;;
