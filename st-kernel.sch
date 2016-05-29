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

;; (import (primitives procedure-name procedure-name-set!))

;;; Method Dictionarys

;; Syntactic sugar tastes sweeter ;^)

(define (make-method-dictionary)
  (make-eq-hashtable))

(define (method-dictionary-size methodDict)
  (hashtable-size methodDict))

;; methodDict primLookup: aSymbol
(define (primLookup: methodDict symbol)
  (hashtable-ref methodDict
                 symbol
                 (lambda (self . rest-args)
                   (send-failed self symbol rest-args)))
                   ;; (make-messageSend self symbol rest-args)))
)

(define (primSet:toValue: methodDict key value)
  (hashtable-set! methodDict key value))

(define (send-failed receiver selector rest-args) ;; messageSend)
  ;; @@@@@FIXME: invoke debugger
  (let* ( (messageSend (make-messageSend receiver selector rest-args))
          (perhaps-name "")
           ;; (if (perform:with: receiver 'respondsTo: 'name)
           ;;     (let ( (n (perform: receiver 'name)) )
           ;;       (cond
           ;;        ((string? n) n)
           ;;        ((null? n) "")
           ;;        ((boolean? n) "")
           ;;        (else
           ;;         (symbol->string n))))
           ;;     ""))
         )
  (error (string-append
          "Failed message send: #"
          (symbol->string selector)
          " to ")
         receiver
         rest-args)
) )

;; methodDict primAddSelector: selector withMethod: compiledMethod
(define (primAddSelector:withMethod: methodDict symbol methodClosure)
  (if (not (procedure? methodClosure))
      (error "Methods must be closures" methodClosure))
  (procedure-name-set! methodClosure symbol)
  (hashtable-set! methodDict symbol methodClosure))

;; methodDict selectors
(define (primSelectors methodDict)
  (hashtable-keys methodDict))

(define (primIncludesSelector: methodDict symbol)
  (hashtable-contains? methodDict symbol))

(define (primSelectorsDo: methodDict closure)
  (for-each closure (hashtable-keys methodDict)))

(define (primSelectorsAndMethodsDo: methodDict closure)
  (vector-for-each closure (hashtable-entries methodDict)))

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

;;; Basic Objects

(define st-nil  '())
(define st-true  #true)
(define st-false #false)

(define st-nil? null?)

(define (make-mDict-placeholder classNameSym)
  (let ( (mDict (make-method-dictionary)) )
    (primAddSelector:withMethod: mDict
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

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printString
        (lambda (self) "nil"))

(primAddSelector:withMethod: 
 	st-true-behavior
        'printString
        (lambda (self) "true"))

(primAddSelector:withMethod: 
 	st-false-behavior
        'printString
        (lambda (self) "false"))

(primAddSelector:withMethod: 
 	st-string-behavior
        'printString
        (lambda (self)
          (string-append "'" self "'")))

(primAddSelector:withMethod: 
 	st-character-behavior
        'printString
        (lambda (self)
          (string-append "$" (make-string 1 self))))

(primAddSelector:withMethod: 
 	st-symbol-behavior
        'printString
        (lambda (self) ;;@@FIXME: elide #\'..' when all lower case..
          (string-append "#'" (symbol->string self) "'")))

(primAddSelector:withMethod: 
 	st-block-behavior
        'selector
        (lambda (self) (procedure-name self)))

(primAddSelector:withMethod: 
 	st-block-behavior
        'argumentCount
        (lambda (self) (procedure-arity self)))

(primAddSelector:withMethod:  ;; alias
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

;;; @@FIXME: convert to NATIVE TAG mask + index into vector of behaviors
(define num-header-slots 2) ;; tag + behavior; @@remove tag@@
(define (behavior thing)
  (case thing  
    ;; immediates
    ((#true)  st-true-behavior)
    ((#false) st-false-behavior)
    (('())    st-nil-behavior)
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
      ((char?   thing)    st-character-behavior)
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
        (primLookup: mDict selectorSym)
) ) )

;;; Objects

;; immediates, vector-like, bytevector-like

;;; R7RS

(define bytevector-ref  bytevector-u8-ref)
(define bytevector-set! bytevector-u8-set!)

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

;; Done ONCE at class creation
(define (add-bytevector-accessors behavior)

  (primAddSelector:withMethod:
     behavior
     'at:
     (lambda (self index)
       ;; NB: ST 1-based, Scheme 0-based
       (let* ( (bvec (vector-ref self 2))
               (bvec-len (bytevector-length bvec))
             )
         (if (< 0 index (+ 1 bvec-len))
             (bytevector-ref bvec (- index 1))
             (error "Index out of range" index self))
   ) ) )

  (primAddSelector:withMethod:
     behavior
     'at:put:
     (lambda (self index newVal)
       ;; @@FIXME: newVal type check (byte)
       ;; NB: ST 1-based, Scheme 0-based
       (let* ( (bvec (vector-ref self 2))
               (bvec-len (bytevector-length bvec))
             )
         (if (< 0 index (+ 1 bvec-len))
             (begin
               (bytevector-set! bvec (- index 1) newVal)
               self)
             (error "Index out of range" index self newVal))
       ) )
) )

;; Done once
(add-bytevector-accessors st-bytevector-behavior)

(primAddSelector:withMethod:
     st-bytevector-behavior
     'size 
     (lambda (self)
       (bytevector-length (vector-ref self 2))))

(primAddSelector:withMethod:
     st-bytevector-behavior
     'basicSize
     (lambda (self)
       (bytevector-length (vector-ref self 2))))


;;; (vector:  tag |  behavior | optional-named-slots.. | optional-indexed-slots.. )

(define (make-st-object behavior num-indexed+named-slots)
  (let ( (st-obj (make-vector
                   (+ num-header-slots num-indexed+named-slots)
                   st-nil))
       )
    (vector-set! st-obj 0 %%st-object-tag%%)  ;; @@Change when switch to native tags@@
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
     'basicSize
     (lambda (self)
       (- (vector-length self) start-index))
     )
) )

;; For Error Reporting (#doesNotUnderstand:)
(define st-messageSend-behavior (make-mDict-placeholder 'MessageSend))

(add-getters&setters st-messageSend-behavior
                     num-header-slots ;; first slot as index skips header
                     '(receiver selector arguments))

(primAddSelector:withMethod: 
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

(primAddSelector:withMethod: 
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
  (let* ( (argArray    (ensure-st-array args-list))
          (messageSend (make-st-object st-messageSend-behavior 3))
        )
    (perform:with: messageSend 'receiver:  receiver)
    (perform:with: messageSend 'selector:  selector)
    (perform:with: messageSend 'arguments: argArray)
    messageSend)
)


;; Need to make st Arrays
(add-array-accessors st-array-behavior 2)

(primAddSelector:withMethod: 
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

(define (primSetClass: obj class)
  (primSet:toValue: (behavior obj) 'class (lambda (self) class)))


;; (provide 'st-kernel)

;;;			--- E O F ---			;;;
