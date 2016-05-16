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
                 (lambda (self . ignored-args) ;; @@FIXME: Message object
                   (doesNotUnderstand: self symbol)))
)

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

(define smalltalk-dictionary (make-eq-hashtable))

;;; See file 'st-object.sch'
;; (define st-object
;;  (vector %%st-object-tag%% st-object-behavior))

(define st-obj-tag-index 0)
(define st-obj-behavior-index 1) ;; 2nd slot in a st-object

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
        (doesNotUnderstand: self selectorSym)
        (lookup: mDict selectorSym)
) ) )

;;; Objects

;; immediates, vector-like, bytevector-like

(define (make-st-bytevector numBytes initialValue)
  (let ( (initVal (if (and (integer? initialValue) (<= 0 initialValue 255))
                      initialValue
                      0))
       )
    (vector %%st-object-tag%%
            st-bytevector-behavior
            (make-bytevector size initVal)))
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
         (bytevector-set! bvec (- index 1) newVal))))
)

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
    (if (zero? num-indexed-slots)
        st-obj
        (let* ( (vec-len (vector-length st-obj))
                (start-index (- vec-len num-indexed-slots))
              )
          (let loop ( (index start-index) )
            (if (>= index vec-len)
                st-obj
                (begin
                  (vector-set! st-obj index 0)
                  (loop (+ 1 index)))))))
) )
  
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
             (vector-set! self index newVal)))
          
          (loop (+ index 1) (cdr slot-names))))
  )
)

(define (add-array-accessors behavior start-index)

  (addSelector:withMethod:
     behavior
     'at:
     (lambda (self user-index)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< 0 vec-index (vector-length self))
             (vector-ref self vec-index)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
   )

  (addSelector:withMethod:
     behavior
     'at:put:
     (lambda (self user-index newVal)
       ;; NB: ST 1-based, Scheme 0-based
       (let ( (vec-index (+ start-index user-index -1)) )
         (if (< 0 vec-index (vector-length self))
             (vector-set! self vec-index newVal)
             (error "Index out of range" user-index)))) ;; @@FIXME: conditions
  )
)


;;; TEST
;; (define test-behavior (make-mDict-placeholder 'Test))

;; (add-getters&setters test-behavior '(foo bar baz))

;; (add-array-accessors test-behavior 5)

;; (define t-obj (make-st-object test-behavior 7 4)) ;; 4 indexed

;; (perform:with:with: t-obj 'at:put: 1 1)
;; (perform:with:with: t-obj 'at:put: 2 2)
;; (perform:with:with: t-obj 'at:put: 3 3)
;; (perform:with:with: t-obj 'at:put: 100 #f) ;; err
;; (perform:with: t-obj 'bar: "BarBar")
;; (perform:with: t-obj 'foo: "theFoo")
;; (perform:with: t-obj 'baz: "mobyBaz")
;; (perform: t-obj 'foo)
;; (perform:with: t-obj 'at: 2) 
;; t-obj


;;;			--- E O F ---			;;;
