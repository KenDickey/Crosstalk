;;; FILE: "st-object.sch"
;;; IMPLEMENTS: Basic Smalltalk objects
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

(define st-nil-behavior        (make-method-dictionary))
(define st-true-behavior       (make-method-dictionary))
(define st-false-behavior      (make-method-dictionary))
(define st-integer-behavior    (make-method-dictionary))
(define st-real-behavior       (make-method-dictionary))
(define st-complex-behavior    (make-method-dictionary))
(define st-rational-behavior   (make-method-dictionary))
;; @@FIXME: Scaled Decimal
(define st-character-behavior  (make-method-dictionary))
(define st-string-behavior     (make-method-dictionary))
(define st-symbol-behavior     (make-method-dictionary))
(define st-array-behavior      (make-method-dictionary))
(define st-bytevector-behavior (make-method-dictionary))
(define st-block-behavior      (make-method-dictionary))
(define st-object-behavior     (make-method-dictionary))

(define %%st-object-tag%% (cons 'object '())) ;; not eq? to anything else

(define allocate-classID
  (let ( (counter 0) )
    (lambda ()
      (set! counter (+ counter 1))
      counter)
) )

(define smalltalk-dictionary (make-eq-hashtable))

(define st-object
  (vector %%st-object-tag%% st-object-behavior))

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

;; @@FIXME: make continuable
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
