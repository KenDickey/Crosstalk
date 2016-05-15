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

; Use byte-vector for ST bytevector w hidden bytes at end for
;	index into class/mTable-vector (#size elides this)

; For faster interpreted lookup: (trade space for speed)
;	Keep track of selectors for each class
;	Aggregate all methods into mDict at each level
;		=> one hashtable ref for any lookup
;	When methods added/removed/updated, fixup all mDicts
;		in lookup chain (closures maximally shared)
;; @@PROTOTYPE: Just use a hash table

; Bytevec tag at end after class/mDict index
; @@REVIST when switch to custom bytevec typetag
; BVecs rounded to even size
;   Use last byte to subtract 1 if required for
;   "official" size.
;; Wake up and smell the Coffee ! ;^)
(define byteVec-tag-even #u8( #xC0 #xFF #xEE #x00 ) )
(define byteVec-tag-odd  #u8( #xC0 #xFF #xEE #x01 ) )
; need to subtract tag + mDict-index size from bytevec bytes
(define byteVec-mDict-id-size 4)  ; 4 byte index
(define byteVec-mDict-id+tag-size 8)
; So sub tag + last-byte to get official bvec size for ST
; NB: ST indexes 1-based, Scheme 0-based !!

(define (st-make-byteVec+fill size fill mDict-id)
  (bytevector-append
   ;; round to even number of bytes
   (make-bytevector (if (even? size) size (+ 1 size)) fill)
   (u8->byteVec mDict-id)
   (if (even? size)
       byteVec-tag-even
       byteVec-tag-odd))
)

(define (st-make-byteVec size mDist-id)
  (st-make-byteVec+fill size 0 mDict-id))

(define (u8->bytevec u8int)
 'NYI ;;  @@@..@@@
)


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
                 (lambda (self . ignored-args)
                   (doesNotUnderstand: self symbol)))
)

;; methodDict addSelector: selector withMethod: compiledMethod
(define (addSelector:withMethod: methodDict symbol methodClosure)
  (hashtable-set! methodDict symbol methodClosure))

;; methodDict selectors
(define (selectors methodDict)
  (hashtable-keys methodDict))

(define (includesSelector: methodDict symbol)
  (hashtable-includes-key methodDict symbol))

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
(define st-rational-behavior   (make-mDict-placeholder 'Fraction))
;; @@FIXME: Scaled Decimal
(define st-character-behavior  (make-mDict-placeholder 'Character))
(define st-string-behavior     (make-mDict-placeholder 'String))
(define st-symbol-behavior     (make-mDict-placeholder 'Symbol))
(define st-array-behavior      (make-mDict-placeholder 'Array))
(define st-bytevector-behavior (make-mDict-placeholder 'Bytevector))
(define st-block-behavior      (make-mDict-placeholder 'Block))
(define st-object-behavior     (make-mDict-placeholder 'Object))

(define %%st-object-tag%% (cons 'object '())) ;; not eq? to anything else

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

(define (st-obj-tag obj)      (vector-ref obj st-obj-tag-index))
(define (st-obj-behavior obj) (vector-ref obj st-obj-behavior-index))

(define (st-object? thing)
  (and (vector? thing)
       (>= 2 (vector-length thing))
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
        ((real?     thing) st-real-behavior)
        ((rational? thing) st-rational-behavior)
        ((complex?  thing) st-compex-behavior)
        ;; FIXME:: Scaled Decimal
        (else st-nil))
       )
      ((st-object? thing) (vector-ref thing st-obj-behavior-index))
      ((char? thing) st-character-behavior)
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



;;;			--- E O F ---			;;;
