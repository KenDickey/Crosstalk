#!r6rs
;;; File: "st-base.sls"
;;; IMPLEMENTS: Basic Smalltalk object mechanics
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE:  February 2025

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

(library (st-base)

  (export
   
;;; Namespace
   
   Smalltalk 
   smalltalkAt:
   smalltalkAt:put:

;;; Basic access & lookup

   lookupSelector:	; (lookupSelector: obj 'selectorSymbol)
   primLookup:		; (primLookup: methodDict selector)
   primSet:toValue:     ; (primSet:toValue: methodDict selector method)
   send-failed		; (send-failed receiver selector rest-args)
   primSetClass:
   setClass:
   class
   superclass
   className:
   isKindOf:
   respondsTo:

;;; Perform and short-form aliases

   $     perform:
   $:    perform:with:
   $::   perform:with:with:
   $:::  perform:with:with:with:
   $:::: perform:with:with:with:with:
   $&    perform:withArguments: ;; args array
   $*    perform:withArgsList:  ;; args list

;;; Basic Objects

   st-nil st-true st-false
   st-nil?

;;; Method/Behavior Dictionaries

   method-dictionary?
   make-method-dictionary
   method-dictionary-size
   method-name
   method-arity
   clone-behavior
   clone-method-dictionary
   behavior-add-from-other
   primSelectorsDo:
   primSelectorsAndMethodsDo:
   primMethodsDo:
   primAddSelector:withMethod:

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
   st-messageSend-behavior

;;; Smalltalk Object Representation

   st-object?
   make-st-object
   num-header-slots
   st-object-length ;; internal
   behavior ;; answers a method dictionary
   add-getters&setters ;; internal
   add-array-accessors ;; internal
   

;;; Debug helpers (interactive)

   smalltalk-keys	; (selectors Smalltalk
   selectors		; (selectors obj) -> method names
   display-selectors
   inst-method-names
   display-ivars
   display-obj		; obj printString
   describe
   safer-printString

;;; Various internal Helpers   
   list-copy vector-copy
   every? any?
   symbol<?
   bytevector-ref
   bytevector-set!
   doesNotUnderstand:		; (doesNotUnderstand: self selector)
   printString
   format
   make-subclassResponsibility	; (make-subclassResponsibility selector)
   subclassResponsibility
   st-obj-copy
   asException			; (asException aCondition)
   unspecified unspecified?

   ;; For "st-base-tests.ss"
   make-mDict-placeholder
   make-st-bytearray
   insure-annotated ;; closure->method
   annotate-procedure-with-arity 
   selector-arity
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

;;  See "st-error.sls"
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

;; shortcuts
(define (class      obj) (perform: obj 'class))
(define (superclass obj) (perform: obj 'superclass))

(define (isKindOf: self someClass)
  (let loop ( (super-class (perform: self 'class)) )
    (cond
     ((null? super-class) #f)
     ((eq? super-class someClass) #t)
     ((not (st-object? super-class)) #f)
     (else (loop (perform: super-class 'superclass))))
) )

(define (respondsTo: self selector)
  (primIncludesSelector: (behavior self) selector))

(define (className: thing)
  (cond
   ((respondsTo: thing 'class)
    (let ( (thing-class (class thing)) )
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
  ;; RAW Arity: self arg counted
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

(define (behavior-add-from-other behavior mdict)
  (let-values ( ((sel-vec meth-vec) (hashtable-entries mdict)) )
    (vector-for-each (lambda (s m) (hashtable-set! behavior s m))
                     sel-vec
                     meth-vec)))
    


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
      ((char?    thing)    st-character-behavior) 
      ((integer? thing)    st-integer-behavior)
      ((st-object? thing) (vector-ref thing st-obj-behavior-index))
      ((number?  thing)
       (cond
        ((rational? thing) st-fraction-behavior) 
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
(define st-messageSend-behavior (make-mDict-placeholder 'MessageSend))


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

(define (display-obj obj) (display (safer-printString obj)))
  
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
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

(add-array-accessors st-array-behavior 0)

;;; Enable reflective introspection
(smalltalkAt:put: 'Smalltalk Smalltalk)


;;;
;;; Some Fundamental Methods are early-bound
;;;	to aid debugging in REPL
;;;

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printOn:
        (lambda (self port)
          (display "nil" port)
          "nil"))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'notNil
        (lambda (self) st-false))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'asSymbol
        (lambda (self) 'nil))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'isNil
        (lambda (self) st-true))

(primAddSelector:withMethod: 
 	st-true-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-true-behavior
        'printOn:
        (lambda (self port)
          (display "true" port)
          "true"))

(primAddSelector:withMethod: 
 	st-false-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-false-behavior
        'printOn:
        (lambda (self port)
          (display "false" port)
          "false")
        )

(primAddSelector:withMethod: 
 	st-string-behavior
        'asString     ;; like Scheme: write (for value)
        (lambda (self) self))

(primAddSelector:withMethod: 
 	st-string-behavior
        'printString ;; like Scheme: display (for parsers)
        printString)

(primAddSelector:withMethod: 
 	st-string-behavior
        'printOn:
        (lambda (self port)
          (format port "'~a'" self)
          self)
)

(primAddSelector:withMethod: 
 	st-character-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-character-behavior
        'printOn:
        (lambda (self port)
          (format port "$~c" self)
          (format #f "$~c" self)))

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'asString
        (lambda (self) (symbol->string self)))

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'printOn:
        (lambda (self port) ;;@@FIXME: elide #\'..' when all lower case..
          (format port "#'~a'" self)
          (format #f "#'~a'" self))
)

(primAddSelector:withMethod: 
 	st-integer-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-integer-behavior
        'printOn:
        (lambda (self port)
          (let ( (str (number->string self)) )
            (display str port)
            str
          ) )
        )

(primAddSelector:withMethod: 
 	st-float-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-float-behavior
        'printOn:
        (lambda (self port)
          (let ( (str (number->string self)) )
            (display str port)
            str
          ) )
        )

(primAddSelector:withMethod: 
 	st-complex-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-complex-behavior
        'printOn:
        (lambda (self port)
          (let ( (str (number->string self)) )
            (display str port)
            str
          ) )
        )

(primAddSelector:withMethod: 
 	st-fraction-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-fraction-behavior
        'printOn:
        (lambda (self port)
          (let ( (str (number->string self)) )
            (display str port)
            str
          ) )
        )

;;; Method Info

(primAddSelector:withMethod: 
 	st-blockClosure-behavior
        'selector
        (lambda (self) (method-name self)))

(primAddSelector:withMethod: 
 	st-blockClosure-behavior
        'argumentCount
        (lambda (self) (method-arity self)))

(primAddSelector:withMethod:  ;; alias
 	st-blockClosure-behavior
        'numArgs
        (lambda (self) (method-arity self)))


;; ByteArray [Scheme bytevector]

(primAddSelector:withMethod:
     st-bytearray-behavior
     'at:
     (lambda (self index)
       ;; NB: ST 1-based, Scheme 0-based
       (if (<= 1 index (bytevector-length self))
           (bytevector-ref self (- index 1))
           (error 'at: "Index out of range" self index))))
     
(primAddSelector:withMethod:
     st-bytearray-behavior
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-set! self (- index 1) newVal)
           (error 'at:put: "Index out of range" self index))))

(primAddSelector:withMethod:
     st-bytearray-behavior
     'size 
     (lambda (self)
       (bytevector-length self)))

(primAddSelector:withMethod:
     st-bytearray-behavior
     'basicSize
     (lambda (self)
       (bytevector-length self)))


;; MessageSend

(add-getters&setters st-messageSend-behavior
		;; first slot as index skips header
                     num-header-slots 
                     '(receiver selector arguments))

(primAddSelector:withMethod: 
 	st-messageSend-behavior
        'value    ;; retry original message send
        (lambda (self)  
          (let ( (receiver  (perform: self 'receiver))
                 (selector  (perform: self 'selector))
                 (arguments (perform: self 'arguments)) ;;@@@?@@@ vec or list?
               )
          (if (zero? (vector-length arguments))
              (perform: receiver selector)
              (perform:withArguments: receiver selector arguments))))
)

(primAddSelector:withMethod: 
 	st-messageSend-behavior
        'valueWithArguments:  ;; retry w different args
        (lambda (self newArgsArray)  
          (let ( (receiver  (perform: self 'receiver))
                 (selector  (perform: self 'selector))
               )
          (if (zero? (vector-length newArgsArray))
              (perform: receiver selector)
              (perform:withArguments:
                  receiver
                  selector
                  newArgsArray))))
)


;;;

;;  Smalltalk Arrays are Scheme Vectors..

(add-array-accessors st-array-behavior 0)

(primAddSelector:withMethod:
     st-array-behavior
     'size 
     (lambda (self)
       (vector-length self)))

(primAddSelector:withMethod:
     st-condition-behavior
     'asException  ;; def'ed in "st-condition.scm"
     (lambda (self)
       (asException self)))

(primAddSelector:withMethod:
     st-condition-behavior
     'asDictionary  ;; def'ed in "st-condition.scm"
     (lambda (self)
       (condition->dictionary self)))

;; Object

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
        respondsTo:)

(primAddSelector:withMethod: ;; ANSI
 	st-object-behavior
        'isKindOf:
        isKindOf:
)

(primAddSelector:withMethod:arity:
 	st-object-behavior
        '==    ;; ANSI
        (lambda (self other) (eq? self other))
        2)

(primAddSelector:withMethod:arity:
 	st-object-behavior
        '~~    ;; ANSI
        (lambda (self other) (not (eq? self other)))
        2)

(primAddSelector:withMethod:arity:
 	st-object-behavior
        '=   ;; ANSI
        (lambda (self other) (eqv? self other))
        2)

(primAddSelector:withMethod:arity:
 	st-object-behavior
        '~=   ;; ANSI
        (lambda (self other) (not (eqv? self other)))
        2)

(primAddSelector:withMethod:
 	st-object-behavior
        'hash   ;; ANSI
        equal-hash) 

(primAddSelector:withMethod:
 	st-object-behavior
        'identityHash   ;; ANSI
        equal-hash) ;; @@CHECKME: eq-kash ??

(primAddSelector:withMethod:
 	st-object-behavior
        'printString   ;; ANSI
        printString)

(primAddSelector:withMethod:
 	st-object-behavior
        'printOn:  ;; ANSI
        (lambda (self outport)
          (let ( (str
                  (string-append
                   "<instance of "
                   ($ (className: self) 'asString)
                   ">"))
               )
          (display str outport)
          str))
        )
        ;; (lambda (self outport)
        ;;   (let ( (vowels (string->list "aeiouAEIOU"))
        ;;          (className ($ (className: self) 'asString))
        ;;        )
        ;;     (display
        ;;      (string-append
        ;;       "<"
        ;;       (if (memq (string-ref className 0) vowels)
        ;;           "an " "a ")
        ;;       "instance of "
        ;;       className
        ;;       ">")
        ;;      outport))))


(primAddSelector:withMethod:
 	st-object-behavior
        'is:
        (lambda (self aSymbol) #f)) ; base case

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
            ((st-object? self)  (st-object-length self))
            ((vector? self)     (vector-length self))
            ((string? self)     (string-length self))
            ((symbol? self)     (string-length (symbol->string self)))
            ((bytevector? self) (bytevector-length self))
            (else 0)
        ) )
)

(primAddSelector:withMethod:
 	st-object-behavior
        'shallowCopy  
        ;; A shallow copy shares slot-values
        (lambda (self) (st-obj-copy self))
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
            ((st-object? self)  (st-obj-copy self))
            ((vector? self)     (vector-copy self)) 
            ((string? self)     (string-copy self))
            ((symbol? self)     self)
;;;            ((procedure?  self) (procedure-copy  self))
            ((bytevector? self) (bytevector-copy self))
            ((hashtable?  self) (hashtable-copy  self))
            ((list? self)       (list-copy self))
            ;;@@ environment, port, ..?
            (else self) ;; immediates not copyable!
        ) )
)

(primAddSelector:withMethod:
 	st-object-behavior
        'error:  ;; ANSI
        (lambda (self aString)
          (error #f aString self))) ;;; @@FIXME: Debug!

(primAddSelector:withMethod:
 	st-object-behavior
        'value
        (lambda (self) self)) ;; St ideom


;; @@@ From PharoCandle.  Don't know why these are in Object?!? @@@
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


)

;;;			--- E O F ---			;;;
