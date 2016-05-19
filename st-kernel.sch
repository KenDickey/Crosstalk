;;; FILE: "st-kernel.sch"
;;; IMPLEMENTS: Basic Smalltalk object mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016


;;; Representations

; Use Scheme immediates & numbers
;	byte-tag + mask -> index into table of classes

; Use tagged Vector as ST Object (named & indexed slots)
;   1st slot in Vector contains a mDict (method dictionary)
;	with a binding of 'class->(lambda (self) <class>)
;	to get the class

;; NB: larceny -r7r6 ...

(import (primitives procedure-name procedure-name-set!))

;;; Method Dictionarys

;; Syntactic sugar tastes sweeter ;^)

(define (make-method-dictionary)
  (make-eq-hashtable))

(define (method-dictionary-size methodDict)
  (hashtable-size methodDict))

;; methodDict lookup: aSymbol
(define (lookup: methodDict symbol)
  (hashtable-ref methodDict
                 symbol
                 (lambda (self . rest-args)
                   (send-failed
                    (make-messageSend self symbol rest-args)))
) )

(define (send-failed messageSend)
  ;; @@@@@FIXME: invoke debugger
  (let* ( (receiver (perform: messageSend 'receiver))
          (selector (perform: messageSend 'selector))
          (perhaps-name
           (if (perform:with: receiver 'respondsTo: 'name)
               (let ( (n (perform: receiver 'name)) )
                 (cond
                  ((string? n) n)
                  ((null? n) "")
                  ((boolean? n) "")
                  (else
                   (symbol->string n))))
               ""))
         )
  (error (string-append
          "Failed message send: #"
          (symbol->string selector)
          " to "
          perhaps-name)
        messageSend)
) )

;; methodDict addSelector: selector withMethod: compiledMethod
(define (addSelector:withMethod: methodDict symbol methodClosure)
  (if (not (procedure? methodClosure))
      (error "Methods must be closures" methodClosure))
  (procedure-name-set! methodClosure symbol)
  (hashtable-set! methodDict symbol methodClosure))

;; methodDict selectors
(define (selectors methodDict)
  (hashtable-keys methodDict))

(define (includesSelector: methodDict symbol)
  (hashtable-contains? methodDict symbol))

(define (selectorsDo: methodDict closure)
  (for-each closure (hashtable-keys methodDict)))

(define (selectorsAndMethodsDo: methodDict closure)
  (vector-for-each closure (hashtable-entries methodDict)))

(define (selectorsAndMethodsDo: methodDict closure)
  (let-values ( ((selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure selectors methods)))

(define (methodsDo: methodDict closure)
  (let-values ( ((ignored-selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure methods))) 

(define (clone-method-dictionary mDict)
  (let ( (clone (make-eq-hashtable (hashtable-size mDict))) )
    (selectorsAndMethodsDo:
     	mDict
        (lambda (selector method)
          (addSelector:withMethod: mDict selector method)))
    clone)
)

;;; Basic Objects

(define st-nil  '())
(define st-true  #t)
(define st-false #f)

(define st-nil? null?)

(define (make-mDict-placeholder classNameSym)
  (let ( (mDict (make-method-dictionary)) )
    (addSelector:withMethod: mDict
                             'class
                             (lambda (self) classNameSym))
    mDict
) )

(define st-nil-behavior        (make-mDict-placeholder 'UndefinedObject))
(define st-true-behavior       (make-mDict-placeholder 'True))
(define st-false-behavior      (make-mDict-placeholder 'False))
(define st-integer-behavior    (make-mDict-placeholder 'Integer))
(define st-real-behavior       (make-mDict-placeholder 'Float))
(define st-complex-behavior    (make-mDict-placeholder 'Complex))
(define st-fraction-behavior   (make-mDict-placeholder 'Fraction))
;; @@FIXME: Scaled Decimal
(define st-character-behavior  (make-mDict-placeholder 'Character))
(define st-string-behavior     (make-mDict-placeholder 'String))
(define st-symbol-behavior     (make-mDict-placeholder 'Symbol))
(define st-array-behavior      (make-mDict-placeholder 'Array))
(define st-bytevector-behavior (make-mDict-placeholder 'Bytevector))
(define st-block-behavior      (make-mDict-placeholder 'Block))
(define st-object-behavior     (make-mDict-placeholder 'Object))

(addSelector:withMethod: 
 	st-nil-behavior
        'printString
        (lambda (self) "nil"))

(addSelector:withMethod: 
 	st-true-behavior
        'printString
        (lambda (self) "true"))

(addSelector:withMethod: 
 	st-false-behavior
        'printString
        (lambda (self) "false"))

(addSelector:withMethod: 
 	st-string-behavior
        'printString
        (lambda (self)
          (string-append "'" self "'")))

(addSelector:withMethod: 
 	st-character-behavior
        'printString
        (lambda (self)
          (string-append "$" (make-string 1 self))))

(addSelector:withMethod: 
 	st-symbol-behavior
        'printString
        (lambda (self) ;;@@FIXME: elide #\'..' when all lower case..
          (string-append "#'" (symbol->string self) "'")))

(addSelector:withMethod: 
 	st-block-behavior
        'selector
        (lambda (self) (procedure-name self)))

(addSelector:withMethod: 
 	st-block-behavior
        'argumentCount
        (lambda (self) (procedure-arity self)))

(addSelector:withMethod:  ;; alias
 	st-block-behavior
        'numArgs
        (lambda (self) (procedure-arity self)))

(define allocate-classID
  (let ( (counter 0) )
    (lambda ()
      (set! counter (+ counter 1))
      counter)
) )

;;; See file 'st-object.sch'
;; (define st-object
;;  (vector %%st-object-tag%% st-object-behavior))

(define st-obj-tag-index 0)
(define st-obj-behavior-index 1) ;; 2nd slot in a st-object

;;; @@FIXME: Use typetag-set! and free one slot per object (adds up!)
;; Present simplification is useful for bootstrap debug.
(define %%st-object-tag%% (cons 'st-object '())) ;; not eq? to anything else

(define (st-obj-tag obj)      (vector-ref obj st-obj-tag-index))
(define (st-obj-behavior obj) (vector-ref obj st-obj-behavior-index))

(define (st-object? thing)
  (and (vector? thing)
       (< 1 (vector-length thing))
       (eq? %%st-object-tag%% (st-obj-tag thing))))

;;; @@FIXME: convert to tag mask + index into vector of behaviors
(define (behavior thing)
  (case thing  
    ;; immediates
    ((#t)   st-true-behavior)
    ((#f)   st-false-behavior)
    ((())   st-nil-behavior)
    (else
     (cond
      ((integer? thing) st-integer-behavior)
      ((number?  thing)
       (cond
        ((ratnum?   thing) st-fraction-behavior)
        ((real?     thing) st-real-behavior)
        ((complex?  thing) st-complex-behavior)
        ;; FIXME:: Scaled Decimal
        (else st-nil))
       )
      ((st-object? thing) (vector-ref thing st-obj-behavior-index))
      ((char? thing)      st-character-behavior)
      ((string? thing)    st-string-behavior)
      ((symbol? thing)    st-symbol-behavior)
      ((procedure? thing) st-block-behavior)
      ;; @@FIXME port -> FileStream
      ;; @@FIXME ...
      (else (error "#behavior can't deal with other Scheme types yet"
                 thing)) ;; st-nil
    ))
) )

;; Message lookup

(define (lookupSelector: self selectorSym) ;; Polymorphic
  (let ( (mDict (behavior self)) )
    (if (st-nil? mDict)
        (doesNotUnderstand: self selectorSym) ;; Brokem Prototype
        (lookup: mDict selectorSym)
) ) )

;;; Objects

;; immediates, vector-like, bytevector-like

(define (make-st-bytevector numBytes initialValue)
  (let ( (initVal (if (and (integer? initialValue)
                           (<= 0 initialValue 255))
                      initialValue
                      0)) ;; error?
       )
    (vector %%st-object-tag%%
            st-bytevector-behavior
            (make-bytevector numBytes initVal)))
)

;; done at class creation
(define (add-bytevector-accessors behavior)

  (addSelector:withMethod:
     behavior
     'at:
     (lambda (self index)
       ;; @@FIXME: index range check
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (bvec (vector-ref self 2)) )
         (bytevector-ref bvec (- index 1))))
   )

  (addSelector:withMethod:
     behavior
     'at:put:
     (lambda (self index newVal)
       ;; @@FIXME: index range checkl newVal check
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (bvec (vector-ref self 2)) )
         (bytevector-set! bvec (- index 1) newVal)
         self)))
)

;; Done once
(add-bytevector-accessors st-bytevector-behavior)

(addSelector:withMethod:
     st-bytevector-behavior
     'size
     (lambda (self)
       (bytevector-length (vector-ref self 2))))


;;; (vector:  tag |  behavior | optional-named-slots.. | optional-indexed-slots.. )

(define (make-st-object  behavior
                         num-indexed+named-slots
                         num-indexed-slots)
  (let* ( (num-header-slots 2)
          (st-obj (make-vector
                   (+ num-header-slots num-indexed+named-slots)
                   st-nil))
        )
    (vector-set! st-obj 0 %%st-object-tag%%)
    (vector-set! st-obj 1 behavior)
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
  
;; done at class creation
(define (add-getters&setters behavior slot-names-list)
  (let loop ( (index 2) (slot-names slot-names-list) )
    (if (null? slot-names)
        'done
        (let* ( (getter-name (car slot-names))
                (setter-name
                 (string->symbol
                  (string-append
                   (symbol->string getter-name) ":")))
                )
          (addSelector:withMethod:
           behavior
           getter-name
           (lambda (self)
             (vector-ref self index)))
          
          (addSelector:withMethod:
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

    (addSelector:withMethod:
     behavior
     'at:
     (lambda (self user-index)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< pre-start vec-index (vector-length self))
             (vector-ref self vec-index)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
   )

    (addSelector:withMethod:
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
) )

;; For Error Reporting (#doesNotUnderstand:)
(define st-messageSend-behavior (make-mDict-placeholder 'MessageSend))

(add-getters&setters st-messageSend-behavior
                     '(receiver selector arguments))

(addSelector:withMethod: 
 	st-messageSend-behavior
        'value    ;; retry original message send
        (lambda (self)  
          (let ( (receiver  (perform: self 'receiver))
                 (selector  (perform: self 'selector))
                 (arguments (perform: self 'arguments))
               )
          (if (zero? (vector-length arguments))
              (perform: receiver selector)
              (perform:withArguments: receiver selector arguments))))
)

(addSelector:withMethod: 
 	st-messageSend-behavior
        'valueWithArguments:  ;; retry w dirrerent args
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
  (let* ( (argArray (ensure-st-array args-list))
          (messageSend (make-st-object st-messageSend-behavior 3 0))
        )
    (perform:with: messageSend 'receiver:  receiver)
    (perform:with: messageSend 'selector:  selector)
    (perform:with: messageSend 'arguments: argArray)
    messageSend)
)


;; Need to make st Arrays
(add-array-accessors st-array-behavior 2)

(addSelector:withMethod: 
 	st-array-behavior
        'size
        (lambda (self)  
          (- (vector-length self) 2))
)

(define (list->st-array someList)
  (list->vector
   (cons %%st-object-tag%%
         (cons st-array-behavior
               someList)))
)

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

;; (provide 'st-kernel)

;;;			--- E O F ---			;;;
