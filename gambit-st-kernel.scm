;;; FILE: "st-kernel.scm"
;;; IMPLEMENTS: Basic Smalltalk object mechanics
;;; LANGUAGE: Scheme (R5RS)
;;; AUTHOR: Ken Dickey
;;; DATE: 26 April 2018


;;; We will start with object behaviors -- not the objects themselves.
;;; A behavior is just a (method) dictionary / hashtable

;;; Method Dictionarys are Scheme hashtables

;; Syntactic sugar tastes sweeter ;^)

(define (make-method-dictionary)
  (make-table (string->keyword "test") eq?))
(define method-dictionary? table?)
(define method-dictionary-size table-length)

;; methodDict primLookup: aSymbol
(define (primLookup: methodDict symbol)
  (table-ref methodDict
	     symbol
	     (lambda (self . rest-args)
	       (send-failed self symbol rest-args)))
            ;; (make-messageSend self symbol rest-args)))
)

(define primSet:toValue: table-set!)

(define (saferIsKindOf: self someClass)
  (let loop ( (super-class (perform: self 'class)) )
    (cond
     ((null? super-class) #f)
     ((eq? super-class someClass) #t)
     ((not (st-object? super-class)) #f)
     (else (loop (perform: super-class 'superclass))))
) )

(define (class? thing)
  (cond
   ((not (st-object? thing)) #f)
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
  (table-set! methodDict symbol methodClosure))

(define (table-keys table)   (map car (table->list table)))
(define (table-values table) (map cdr (table->list table)))
(define table-contains?
  (let ( (unfound '(unfound)) ) ;; not eq? to anything else
    (lambda (table key)
      (if (eq? unfound (table-ref table key unfound))
	  #f
	  #t))))
(define table-entries table->list)

;; methodDict selectors
(define (primSelectors methodDict) (table-keys methodDict))

(define primIncludesSelector: table-contains?)

(define (primSelectorsDo: methodDict closure)
  (for-each closure (table-keys methodDict)))

(define (primSelectorsAndMethodsDo: methodDict closure)
  (table-for-each closure methodDict))

(define (primMethodsDo: methodDict closure)
  (for-each closure (table-values methodDict)))

(define clone-method-dictionary table-copy)
(define clone-behavior clone-method-dictionary) ;; shorter to type

;; If closure name does not match its ##procedure-name
;; remember it in method->name-table.
(define method->name-table (make-table (string->keyword "weak-keys") #t))
(define (procedure-name method)
  (let ( (probe (table-ref method->name-table method #f)) )
    (if probe probe (##procedure-name method))))
(define (procedure-name-set! method nameSymbol)
  (if (not (eq? nameSymbol (##procedure-name method)))
      (table-set! method->name-table method nameSymbol)))


;;@@@FIXME very suspect..
(define procedure-arity ##subprocedure-nb-parameters)


;;; Basic Objects

(define true  #t)
(define false #f)
(define nil   '())

(define st-nil   nil)
(define st-true  #t)
(define st-false #f)

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
(define st-condition-behavior    (make-mDict-placeholder 'Condition))
;;(define st-record-behavior    (make-mDict-placeholder 'Record))
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
          #f))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'asSymbol
        (lambda (self) 'nil)) ;; @@?? UndefinedObject ??@@


(primAddSelector:withMethod: 
 	st-nil-behavior
        'isNil
        (lambda (self)
          #t))

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

; Use Gambit Structure as ST Object (named & indexed slots)
;   1st slot in Vector contains a mDict (method dictionary)
;	with a binding of 'class->(lambda (self) <class>)
;	to get the class

(define-structure st-object) ;; defines proc:  st-object?
(define st-object-type (##structure-type (make-st-object)))

(define st-obj-behavior-index 1) ;; slot0 is tag, slot1 is mDict

(define (st-obj-behavior obj)
  (##vector-ref obj st-obj-behavior-index))

(define (st-obj-behavior-set! obj new-behavior)
  (##vector-set! obj st-obj-behavior-index new-behavior))

(define num-header-slots 2)  ;; 1 slot for tag; 1 slot for mDict

(define (st-object-length obj)
  (- (##vector-length obj) num-header-slots))


;;; Generic ST Object representation:
;;; (vector:  behavior | optional-named-slots.. | optional-indexed-slots.. )

(define (make-st-object Behavior num-object-slots)
  (let* ( (vec-len (+ num-header-slots num-object-slots))
	  (st-obj (##make-structure st-object-type vec-len))
	)
    (##vector-set! st-obj st-obj-behavior-index Behavior)
    ;; initial slot values are: nil
    (let loop ( (idx num-header-slots) )
      (if (< idx vec-len)
	  (begin
	    (##vector-set! st-obj idx nil)
	    (loop (+ idx 1)))))
    st-obj)
)


;;; Behavior adds intelligence to structure

;;; (behavior obj)

;; @@FIXME: optimize for Gambit prims/layouts


(define (behavior thing)
  (if (st-object? thing)
      (st-obj-behavior thing)
      (case thing  
	;; immediates -- tagtype -> err
	((#t)  st-true-behavior)
	((#f) st-false-behavior)
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
	  ((table? thing)
	   (if (eq? eq? (table-equivalence-function thing))
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
	  ((condition? thing)     st-condition-behavior)
	  ((st-object? thing) (##vector-ref thing st-obj-behavior-index))
	  ;; input-file 
	  ;; output-file 
	  ;; output-string 
	  ;; (current-*-port) 
	  ;; output-bytevector 
	  ;; table; other records & record types 
	  ;; @@FIXME ...
	  (else (error "#behavior can't deal with other Scheme types yet"
		       thing))
	  ))
) ) )

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
           (bytevector-ref self (- index 1))
           (error "Index out of range" self index))))
     
(primAddSelector:withMethod:
     st-bytevector-behavior
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-set! self (- index 1) newVal)
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
             (##vector-ref self index)))
          
          (primAddSelector:withMethod:
           behavior
           setter-name
           (lambda (self newVal)
             (##vector-set! self index newVal)
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
         (if (< pre-start vec-index (##vector-length self))
             (##vector-ref self vec-index)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
   )

    (primAddSelector:withMethod:
     behavior
     'at:put:
     (lambda (self user-index newVal)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (##vector-length self))
             (begin
               (##vector-set! self vec-index newVal)
               self)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
     )

    (primAddSelector:withMethod:
     behavior
     'at:modify:
     (lambda (self user-index aBlock)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (##vector-length self))
             (let ( (original-elt (##vector-ref self vec-index)) )
               (##vector-set! self
                            vec-index
                            (aBlock original-elt))
               self)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
     )

    (primAddSelector:withMethod:
     behavior
     'basicSize
     (lambda (self)
       (- (##vector-length self) start-index))
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

;;;

(define (primSetClass: behavior class)
  (primSet:toValue: behavior 'class (lambda (self) class)))

(define (setClass: obj class)
  (primSetClass: (behavior obj) class))

;;;
;;;======================================================
;;; What do we have here?

;; Smalltalk

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (smalltalk-keys)
  (vector-sort
   symbol<?
   (table-keys Smalltalk)))

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
          (newline)
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


  ;; (structure-printer
  ;;    (lambda (obj port quote?)
  ;;      (if (st-object? obj)
  ;;          (format port
  ;;                  (if quote? "~s" "~a")
  ;;                  (if (respondsTo: obj 'printString)
  ;;                      (perform: obj 'printString)
  ;;                      "#<an Object>"))
  ;;          (old-structure-printer obj port quote?)))))

(define (unspecified? thing)
  (eq? thing (if #f 1)))


;;;======================================================


;; (provide 'st-kernel)

;;;			--- E O F ---			;;;
