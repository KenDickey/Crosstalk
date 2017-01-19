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

(define (send-failed receiver selector rest-args) ;; messageSend)
  ;; @@@@@FIXME: invoke debugger
  (let ( (messageSend (make-messageSend receiver selector rest-args)) )
    (error (string-append
            "Failed message send: #"
            (symbol->string selector)
            " to ")
           receiver
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
(define st-dictionary-behavior   (make-mDict-placeholder 'Dictionary))

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
          (format port "#'~a'" (symbol->string self)))
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

; Use tagged Vector as ST Object (named & indexed slots)
;   1st slot in Vector contains a mDict (method dictionary)
;	with a binding of 'class->(lambda (self) <class>)
;	to get the class

;;; See file 'st-object.sch'
;; (define st-object
;;  (vector %%st-object-tag%% st-object-behavior))

(define st-obj-tag-index      0)
(define st-obj-behavior-index 1) ;; 2nd slot in a st-object

;;; @@FIXME: Use typetag-set! and free one slot per object (adds up!)
;; Present simplification is useful for bootstrap debug.
(define %%st-object-tag%% (cons 'st-object '())) ;; not eq? to anything else

(define (st-obj-tag      obj) (vector-ref obj st-obj-tag-index))
(define (st-obj-behavior obj) (vector-ref obj st-obj-behavior-index))
(define (st-obj-behavior-set! obj new-behavior)
  (vector-set! obj st-obj-behavior-index new-behavior))

(define (st-object? thing)
  (and (vector? thing)
       (< 1 (vector-length thing))
       (eq? %%st-object-tag%% (st-obj-tag thing))))

;;; @@FIXME: convert to NATIVE TAG mask + index
;;;       ..into vector of behaviors :FIXME@@
(define num-header-slots 2) ;; tag + behavior; @@remove tag@@

;;; Generic ST Object representation:
;;; (vector:  tag |  behavior | optional-named-slots.. | optional-indexed-slots.. )

(define (make-st-object Behavior num-object-slots)
  (let ( (st-obj (make-vector
                   (+ num-header-slots num-object-slots)
                   st-nil))
       )
    ;; @@Change when switch to native tags@@
    (vector-set! st-obj st-obj-tag-index %%st-object-tag%%)
    (vector-set! st-obj st-obj-behavior-index Behavior)
    st-obj)
)
;; TEST -- zeros in indexed slota
;;     (if (zero? num-indexed-slots)
;;         st-obj
;;         (let* ( (vec-len (vector-length st-obj))
;;                 (start-index (- vec-len num-indexed-slots))
;;               )
;;           (let loop ( (index start-index) )
;;             (if (>= index vec-len)
;;                 st-obj
;;                 (begin
;;                   (vector-set! st-obj index 0)
;;                   (loop (+ 1 index)))))))
;; ) )


;;; Behavior adds intelligence to structure

;;; (behavior obj)
(define (behavior thing)
  (case thing  
    ;; immediates -- err
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
      ((vector? thing)
       (if (and (< 1 (vector-length thing))
                (eq? %%st-object-tag%% (st-obj-tag thing)))
           (vector-ref thing st-obj-behavior-index)
           st-array-behavior) ;; Scheme vector ;; tag=0
       )
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
      ((method-dictionary? thing) st-dictionary-behavior)
      ;; (pare? thing) ;; err
      ;; list -> err
      ;; input-file 4
      ;; output-file 4
      ;; output-string 4
      ;; (current-*-port) 4
      ;; output-bytevector 4
      ;; hashtable; other records & record types 5
      ;; @@FIXME port -> FileStream
      ;; @@FIXME ...
      (else (error "#behavior can't deal with other Scheme types yet"
                 thing)) ;; st-nil
    ))
) )

;;; Message lookup

(define (lookupSelector: self selectorSym) ;; Polymorphic
  (let ( (mDict (behavior self)) )
    (if (st-nil? mDict)
        (error "Missing method dictionary for: " self selectorSym)
        (primLookup: mDict selectorSym)
) ) )


;; immediates, vector-like, bytevector-like


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
  (let ( (pre-start (- start-index 1)) )

    (primAddSelector:withMethod:
     behavior
     'at:
     (lambda (self user-index)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-length self))
             (vector-ref self vec-index)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
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
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
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
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
     )

    (primAddSelector:withMethod:
     behavior
     'basicSize
     (lambda (self)
       (- (vector-length self) start-index))
     )
) )

;; For Error Reporting (#doesNotUnderstand:)
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
;;          (messageSend (make-st-object st-messageSend-behavior 3))
        )
    ;; (perform:with: messageSend 'receiver:  receiver)
    ;; (perform:with: messageSend 'selector:  selector)
    ;; (perform:with: messageSend 'arguments: argArray)
    ;; messageSend)
    (vector %%st-object-tag%%
            st-messageSend-behavior
            receiver
            selector
            argArray)
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
  (if (st-object? st-obj)
      (let* ( (obj-class (perform: st-obj 'class))
              (ivarNames
               (if (null? obj-class)
                   '()
                   (perform: obj-class 'allInstVarNames)))
           )
        (cond
         ((null? obj-class)
          (display "entity has no class!!")
          )
         (else
          (describe st-obj)
          (for-each
           (lambda (ivarName)
             (newline)
             (display ivarName)
             (display " -> ")
             (display-obj (perform: st-obj ivarName))
           )
           ivarNames))))
      (write st-obj))
  (newline)
)

(define (display-obj st-obj-or-list)
  (cond
   ((st-object? st-obj-or-list)
    (cond
     ((perform:with: st-obj-or-list 'respondsTo: 'name)
      (display "'")
      (display (perform: st-obj-or-list 'name))
      )
     ((perform:with: st-obj-or-list 'respondsTo: 'printString)
      (display "'")
      (display (perform: st-obj-or-list 'printString))
      )
     (else
      (display "instance of #'")
      (display
       (perform:
        (perform: st-obj-or-list 'class)
        'name))
      )
    )
    (display "' ")
   )
   ((and (list? st-obj-or-list)
         (every? st-object? st-obj-or-list))
    (display "(")
    (for-each display-obj st-obj-or-list)
    (display ")"))
   (else (write st-obj-or-list))
 ) )

(define (describe obj)
  (newline) ;; mostly harmless
  (cond
   ((null? obj) (display 'nil)
    )
   ((list? obj)
    (display "a list of length ")
    (display (length obj)))
   ((st-object? obj)
    (if (perform:with: obj 'respondsTo: 'name)
        (begin
          (display #\")
          (display (perform: obj 'name))
          (display #\")
          (display " is ")))
    (display "an instance of #'")
    (display (perform: (perform: obj 'class) 'name))
    (display "'")
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
;;  (newline)
 )



;;;======================================================


;; (provide 'st-kernel)

;;;			--- E O F ---			;;;
