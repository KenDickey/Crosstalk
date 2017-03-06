;;; FILE: "st-kernel.scm"
;;; IMPLEMENTS: Basic Smalltalk object mechanics
;;; LANGUAGE: Scheme (R7RS small)
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016


;;; We will start with object behaviors -- not the objects themselves.
;;; A behavior is just a (method) dictionary / hashtable

;; NB: larceny -r7rs ...

;; All (import ...) done in "sis.sch"


;;; Method Dictionarys are Scheme hashtables

;; Syntactic sugar tastes sweeter ;^)

(define make-method-dictionary make-eq-hashtable)

(define method-dictionary? hashtable?)

(define method-dictionary-size hashtable-size)

;; methodDict primLookup: aSymbol
(define (primLookup: methodDict symbol)
  (hashtable-ref methodDict
                 symbol
                 (lambda (self . rest-args)
                   (send-failed self symbol rest-args)))
                   ;; (make-messageSend self symbol rest-args)))
)

(define primSet:toValue: hashtable-set!)

(define (saferIsKindOf: self someClass)
  (let loop ( (super-class (perform: self 'class)) )
    (cond
     ((null? super-class) #false)
     ((eq? super-class someClass) #true)
     ((not (st-object? super-class)) #false)
     (else (loop (perform: super-class 'superclass))))
) )

(define (class? thing)
  (cond
   ((not (st-object? thing)) #false)
   (else (saferIsKindOf: thing Class))))

;;  Nota Bene: send-failed is redefined in "st-error-obj.scm"
(define (send-failed receiver selector rest-args)
  (let ( (messageSend (make-messageSend receiver selector rest-args)) )
    (error (format #f "**Failed message send: #~a to: ~a"
                   selector
                   (if (class? receiver)
                       ($ receiver 'name)
                       receiver))
           rest-args)
) )

;;; methodDict primAddSelector: selector withMethod: compiledMethod
(define (primAddSelector:withMethod: methodDict symbol methodClosure)
  (if (not (procedure? methodClosure))
      (error "Methods must be closures" methodClosure))
  (procedure-name-set! methodClosure symbol) 
  (hashtable-set! methodDict symbol methodClosure))

;; methodDict selectors
(define (primSelectors methodDict) (vector->list (hashtable-keys methodDict)))

(define primIncludesSelector: hashtable-contains?)

(define (primSelectorsDo: methodDict closure)
  (vector-for-each closure (hashtable-keys methodDict)))

(define (primSelectorsAndMethodsDo: methodDict closure)
  (let-values ( ((selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure selectors methods)))

(define (primMethodsDo: methodDict closure)
  (let-values ( ((ignored-selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure methods))) 

(define (clone-method-dictionary mDict)
  (let ( (clone (make-eq-hashtable (hashtable-size mDict))) )
    (primSelectorsAndMethodsDo:
     	mDict
        (lambda (selector method)
          (primAddSelector:withMethod: clone selector method)))
    clone)
)

(define clone-behavior clone-method-dictionary) ;; shorter to type

;;; Basic Objects

(define st-nil  '())
(define st-true  #true)
(define st-false #false)

(define st-nil? null?)

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

;;; Behavior adds brains to structure

;; Nota Bene: st-*-behavior's are assigned when corresponding classes are created.
;;
;; Behaviors (method dictionaries) map selectors (symbols) to methods (closures)

;;;  Smalltalk                      Scheme
;;; anArray at: 2 put: 'foo'       (at:put: anArray 2 "foo")

(define st-nil-behavior          (make-mDict-placeholder 'UndefinedObject))
(define st-true-behavior         (make-mDict-placeholder 'True))
(define st-false-behavior        (make-mDict-placeholder 'False))
(define st-integer-behavior      (make-mDict-placeholder 'Integer))
(define st-real-behavior         (make-mDict-placeholder 'Float))
(define st-complex-behavior      (make-mDict-placeholder 'Complex))
(define st-fraction-behavior     (make-mDict-placeholder 'Fraction))
;; @@FIXME: Scaled Decimal
(define st-error-obj-behavior    (make-mDict-placeholder 'ErrorObject))
(define st-character-behavior    (make-mDict-placeholder 'Character))
(define st-string-behavior       (make-mDict-placeholder 'String))
(define st-symbol-behavior       (make-mDict-placeholder 'Symbol))
(define st-array-behavior        (make-mDict-placeholder 'Array))
(define st-list-behavior         (make-mDict-placeholder 'List))
(define st-bytevector-behavior   (make-mDict-placeholder 'Bytevector))
(define st-blockClosure-behavior (make-mDict-placeholder 'BlockClosure))
(define st-object-behavior       (make-mDict-placeholder 'Object))
(define st-byte-stream-behavior  (make-mDict-placeholder 'ByteStream))
(define st-char-stream-behavior  (make-mDict-placeholder 'CharStream))
(define st-date+time-behavior    (make-mDict-placeholder 'DateAndTime))
(define st-time-behavior         (make-mDict-placeholder 'Time))
(define st-duration-behavior     (make-mDict-placeholder 'Duration))
(define st-dictionary-behavior   (make-mDict-placeholder 'Dictionary))
(define st-identity-dictionary-behavior (make-mDict-placeholder 'IdentityDictionary))

(define (printString obj) ;; polymorphic
;; String streamContents: [:s | self printOn: s]
  (let ( (outport (open-output-string)) )
    (perform:with: obj 'printOn: outport)
    (get-output-string outport)))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printOn:
        (lambda (self port)
          (display "nil" port)))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'notNil
        (lambda (self)
          #false))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'asSymbol
        (lambda (self) 'nil)) ;; @@?? UndefinedObject ??@@


(primAddSelector:withMethod: 
 	st-nil-behavior
        'isNil
        (lambda (self)
          #true))

(primAddSelector:withMethod: 
 	st-true-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-true-behavior
        'printOn:
        (lambda (self port)
          (display "true" port)))

(primAddSelector:withMethod: 
 	st-false-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-false-behavior
        'printOn:
        (lambda (self port)
          (display "false" port)))

(primAddSelector:withMethod: 
 	st-string-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-string-behavior
        'printOn:
        (lambda (self port)
          (format port "'~a'" self))
)

(primAddSelector:withMethod: 
 	st-character-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-character-behavior
        'printOn:
        (lambda (self port)
          (display "$" port)
          (display self port))
)

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'printString
        printString)

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'printOn:
        (lambda (self port) ;;@@FIXME: elide #\'..' when all lower case..
          (format port "#'~a'" self))
)

(primAddSelector:withMethod: 
 	st-blockClosure-behavior
        'selector
        (lambda (self) (procedure-name self)))

(primAddSelector:withMethod: 
 	st-blockClosure-behavior
        'argumentCount
        (lambda (self) (procedure-arity self)))

(primAddSelector:withMethod:  ;; alias
 	st-blockClosure-behavior
        'numArgs
        (lambda (self) (procedure-arity self)))

;; (define allocate-classID
;;   (let ( (counter 0) )
;;     (lambda ()
;;       (set! counter (+ counter 1))
;;       counter)
;; ) )


;;;
;;; Smalltalk Object Representation
;;;

; Use Scheme immediates, bytevectors, & numbers
; See (define (behavior obj) ...) below
;	byte-tag + mask -> index into table of classes

; Use Larceny Structure as ST Object (named & indexed slots)
;   1st slot in Vector contains a mDict (method dictionary)
;	with a binding of 'class->(lambda (self) <class>)
;	to get the class

(define st-obj-behavior-index 0) ;; 1st slot in a st-object

(define (st-obj-behavior obj)
  (vector-like-refv obj st-obj-behavior-index))

(define (st-obj-behavior-set! obj new-behavior)
  (vector-like-set! obj st-obj-behavior-index new-behavior))

(define (st-object? thing)
  (and (structure? thing) ;;@@Fixme marker/tag check
       (< 0 (vector-like-length thing))
       (not (record? thing))))

(define (st-object-length obj)
  ;; @@NB: Unchecked
  (- (vector-like-length obj) 1))

;;; @@FIXME: convert to NATIVE TAG mask + index
;;;       ..into vector of behaviors :FIXME@@
(define num-header-slots 1) ;; behavior

;;; Generic ST Object representation:
;;; (vector:  behavior | optional-named-slots.. | optional-indexed-slots.. )

(define st-typetag     5) ;;  a Structure (See larceny/src/Lib/Common/layouts.sch)
(define vector-typetag 0)

(define (make-st-object Behavior num-object-slots)
  (let ( (st-obj (make-vector
                   (+ num-header-slots num-object-slots)
                   st-nil)) ;; init slots to UndefinedObject
       )
    (typetag-set! st-obj st-typetag)
    (vector-like-set! st-obj st-obj-behavior-index Behavior)
    st-obj)
)


;;; Behavior adds intelligence to structure

;;; (behavior obj)

;; @@FIXME: optimize to use primitive tag dispatch
;;   (See larceny/src/Lib/Common/layouts.sch)

(define (behavior thing)
  (case thing  
    ;; immediates -- tagtype -> err
    ((#true)  st-true-behavior)
    ((#false) st-false-behavior)
    (( () )   st-nil-behavior)
;; eof-object -- err
    (else
;; @@FIXME: rep tag -> index into vector of behaviors
     (cond  
      ((char?    thing)    st-character-behavior) ;; err
      ((integer? thing)    st-integer-behavior) ;;bignum->tag=4,else err
      ((number?  thing)
       (cond  ;; coalesc into a single type?
        ((ratnum?   thing) st-fraction-behavior) ;; tag=2
        ((real?     thing) st-real-behavior)     ;; tag=2
        ((complex?  thing) st-complex-behavior)  ;; tag=1
        ;; FIXME:: Scaled Decimal
        (else (error: "Unknown Scheme number representation" thing))
       ))
      ((vector? thing) st-array-behavior) ;; Scheme vector ;; tag=0
      ((string? thing)     st-string-behavior) ;; tag=5
      ((symbol? thing)     st-symbol-behavior) ;; tag=3
      ((procedure? thing)  st-blockClosure-behavior) ;; tag=7
      ((bytevector? thing) st-bytevector-behavior)   ;; tag=0
      ((port? thing)
       (cond
        ((textual-port? thing) st-char-stream-behavior)
        ((binary-port?  thing) st-byte-stream-behavior)
        (else
         (error "Wierd port: " thing)))
      )
      ((hashtable? thing)
       (if (eq? eq? (hashtable-equivalence-function thing))
           st-identity-dictionary-behavior
           st-dictionary-behavior)
       )
      ((pair? thing)  st-list-behavior)
      ((time? thing)
       (cond
        ((eq? 'time-duration (time-type thing))
         st-duration-behavior
         )
        ((eq? 'time-utc (time-type thing))
         st-time-behavior
         )
        ;; 'time-tai
        ;; 'time-monotonic
        ;; 'time-thread
        ;; 'time-process
        (else (error "Unhandled type" (time-type thing) thing)))
       )
      ((date? thing)          st-date+time-behavior)
      ((error-object? thing)  st-error-obj-behavior)
      ((st-object? thing) (vector-like-ref thing st-obj-behavior-index))
      ;; input-file 4
      ;; output-file 4
      ;; output-string 4
      ;; (current-*-port) 4
      ;; output-bytevector 4
      ;; hashtable; other records & record types 5
      ;; @@FIXME ...
      (else (error "#behavior can't deal with other Scheme types yet"
                 thing))
    ))
) )

;;; Message lookup

(define (lookupSelector: self selectorSym) ;; Polymorphic
  (primLookup: (behavior self) selectorSym))


;; Scheme immediates, vector-like, bytevector-like

(define (make-st-bytevector numBytes initialValue)
  (let ( (initVal (if (and (integer? initialValue)
                           (<= 0 initialValue 255))
                      initialValue
                      0)) ;; error?
       )
    (make-bytevector numBytes initVal)
) )


(primAddSelector:withMethod:
     st-bytevector-behavior
     'at:
     (lambda (self index)
       ;; NB: ST 1-based, Scheme 0-based
       (if (<= 1 index (bytevector-length self))
           (bytevector-u8-ref self (- index 1))
           (error "Index out of range" self index))))
     
(primAddSelector:withMethod:
     st-bytevector-behavior
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-u8-set! self (- index 1) newVal)
           (error "Index out of range" self index))))

(primAddSelector:withMethod:
     st-bytevector-behavior
     'size 
     (lambda (self)
       (bytevector-length self)))

(primAddSelector:withMethod:
     st-bytevector-behavior
     'basicSize
     (lambda (self)
       (bytevector-length self)))


  
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
             (vector-like-ref self index)))
          
          (primAddSelector:withMethod:
           behavior
           setter-name
           (lambda (self newVal)
             (vector-like-set! self index newVal)
             self))
          
          (loop (+ index 1) (cdr slot-names))))
  )
)

(define (add-array-accessors behavior start-index)
  (let ( (pre-start (- start-index 1)) )

    (primAddSelector:withMethod:
     behavior
     'at:
     (lambda (self user-index)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-like-length self))
             (vector-like-ref self vec-index)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
   )

    (primAddSelector:withMethod:
     behavior
     'at:put:
     (lambda (self user-index newVal)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-like-length self))
             (begin
               (vector-like-set! self vec-index newVal)
               self)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
     )

    (primAddSelector:withMethod:
     behavior
     'at:modify:
     (lambda (self user-index aBlock)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-like-length self))
             (let ( (original-elt (vector-like-ref self vec-index)) )
               (vector-like-set! self
                            vec-index
                            (aBlock original-elt))
               self)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
     )

    (primAddSelector:withMethod:
     behavior
     'basicSize
     (lambda (self)
       (- (vector-like-length self) start-index))
     )
) )

;; For Error Reporting (#doesNotUnderstand:)
;;  -- redefined in "st-error-obj.scm"
(define st-messageSend-behavior (make-mDict-placeholder 'MessageSend))

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
          (if (zero? (vector-length arguments))
              (perform: receiver selector)
              (perform:withArguments:
                  receiver
                  selector
                  newArgsArray))))
)


(define (make-messageSend receiver selector args-list)
  ;; args list was captured by a .rest
  (let* ( (argArray    (ensure-st-array args-list))
;;        (messageSend (make-st-object st-messageSend-behavior 3))
    ;; (perform:with: messageSend 'receiver:  receiver)
    ;; (perform:with: messageSend 'selector:  selector)
    ;; (perform:with: messageSend 'arguments: argArray)
    ;; messageSend)
          (messageSend
           (vector st-messageSend-behavior
                   receiver
                   selector
                   argArray))
         )
    (typetag-set! messageSend st-typetag)
    messageSend
) )


;;  ST Arrays are Scheme vectors..
(add-array-accessors st-array-behavior 0)

(primAddSelector:withMethod:
     st-array-behavior
     'size 
     (lambda (self)
       (vector-length self)))

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

;;;

(define (primSetClass: behavior class)
  (primSet:toValue: behavior 'class (lambda (self) class)))

(define (setClass: obj class)
  (primSetClass: (behavior obj) class))

;;;======================================================
;;; What do we have here?

;; Smalltalk

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (smalltalk-keys)
  (vector-sort
   symbol<?
   (hashtable-keys Smalltalk)))

;; What selectors does  obj  respond to?
(define (selectors obj)
  (list-sort
   symbol<?
   (primSelectors (behavior obj))))

(define (display-selectors obj)
  (display (selectors obj)))

(define (inst-method-names class)
  (list-sort
   symbol<?
   (perform: class 'myMethodNames)))
    
;; Most useful..
(define (display-ivars st-obj)
  (if (not (st-object? st-obj))
      (write st-obj)
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
             (format #t
                     "  ~a -> ~a~%"
                     ivarName
                     (safer-printString ($ st-obj ivarName)))
           )
           ivarNames))))
      )
  (newline)
)

(define (safer-printString obj)
  (if (not (st-object? obj))
      (format #f "~a" obj)
      (cond ;; smalltalk object
       ;; ((respondsTo: obj 'name)
       ;;  (format #f "'~a'" (perform: obj 'name))
       ;;  )
       ((respondsTo: obj 'printString)
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
) )

(define (display-obj obj) (display (safer-printString obj)))
  
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
   ((eq? obj #true)  (display "true")
    )
   ((eq? obj #false) (display "false")
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
   (else (write obj)) ;; procedures..
   )
  (newline)
 )

(define (stringify thunk)
  (parameterize ( (current-output-port
                   (open-output-string)) )
    (thunk)
    (get-output-string (current-output-port))))


(define (respondsTo: self selector)
  (primIncludesSelector: (behavior self) selector))

(define old-structure-printer (structure-printer))

(structure-printer
     (lambda (obj port quote?)
       (if (st-object? obj)
           (format port
                   (if quote? "~s" "~a")
                   (if (respondsTo: obj 'printString)
                       (perform: obj 'printString)
                       "#<an Object>"))
           (old-structure-printer obj port quote?))))


;;;======================================================


;; (provide 'st-kernel)

;;;			--- E O F ---			;;;
