#!r6rs
;;; FILE: "st-array.sls"
;;; IMPLEMENTS: Array, ByteArray
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-array)

  (export
   init-st-array
   
   Array
   ByteArray
   )
  
  (import
   (rnrs base)
   (rnrs control (6))
   (rnrs io simple (6))
   (rnrs mutable-strings (6))
   (rnrs bytevectors (6))
   (rename (rnrs bytevectors (6)) 
    (bytevector-u8-ref  bytevector-ref)
    (bytevector-u8-set! bytevector-set!)
    )
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)
   (st-sequence-coll)
   (st-array-coll)
   )


(define Array
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'Array '() '())
)

;;(define (vector-for-each proc vec)
;;  (for-each proc (vector->list vec)))

(define ByteArray
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'ByteArray '() '())
)

(define (bytevector-for-each proc bvec)
  (let ( (size (bytevector-length bvec)) )
    (let loop ( (index 0) )
      (when (< index size)
        (proc (bytevector-ref bvec index))
        (loop (+ 1 index)))
) ) )


;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-array)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-array-coll)

(rebase-mdict! Array	 st-array-behavior)
(rebase-mdict! ByteArray st-bytearray-behavior)
(primAppendLocalSelectors: Array
               ;; early bound
               '(at: at:put: size basicSize at:modify:))

(primAppendLocalSelectors: ByteArray
                ;; early bound
               '(at: at:put: size basicSize))

(addSelector:withMethod:
     Set
     'init:
     (lambda (self size)
       ;; make large enough to hold size elts
       ;; without growing -- see #fullCheck
       (let ( (initialSize
               (if (<= size 0)
                   1
                   (floor (/ (* (+ size 1) 4) 3))))
            )
         (superPerform: self 'initialize)
         (perform:with: self 'tally: 0)
         (perform:with: self
                        'array:
                        (perform:with: Array 'new: initialSize))
         self))
)

(addSelector:withMethod:
     Set
     'grow ;; private
     (lambda (self)
       (let* ( (old-array  (perform: self 'array))
               (array-size (perform: old-array 'size))
               (new-size (+ array-size (max array-size 2)))
               (new-array
                  (perform:with: Array 'new: new-size))
             )
         (perform:with: self 'array: new-array)
         (perform:with: self 'tally: 0)
         (perform:with: old-array
                        'do:
                        (lambda (elt)
                          (unless (st-nil? elt)
                            (perform:with: self
                                           'noCheckAdd:
                                           elt))))
         self)))



;;; Array 


(addSelector:withMethod:
     Array
     'asSet
     (lambda (self)
       (let ( (newSet
               (perform:with: Set
                              'new: (vector-length self)))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: newSet 'add: elt)))
          self)
         newSet)))

(addSelector:withMethod:
     Array
     'asIdentitySet
     (lambda (self)
       (let ( (newSet
               (perform:with: IdentitySet
                              'new: (vector-length self)))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: newSet 'add: elt)))
          self)
         newSet)))

(perform:with:
     Array
     'comment:
     "I present an ArrayedCollection whose elements are objects."
)

(perform:with:
     Array
     'category: 'Collections-Arrayed)

(addSelector:withMethod:
     Array
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Array)
           (superPerform:with: self 'is: symbol))))


;; Smalltalk Arrays are Scheme Vectors

(addSelector:withMethod:
     Array
     'size
     (lambda (self)
       (vector-length self)))

(addSelector:withMethod:
     Array
     'basicSize
     (lambda (self)
       (vector-length self)))

(addSelector:withMethod:
     (class Array)
     'basicNew:
     (lambda (self size)
       (make-vector size st-nil)))

(addSelector:withMethod:
     (class Array)
     'new:
     (lambda (self size)
       (perform: (perform:with: self 'basicNew: size)
                 'initialize)))

(addSelector:withMethod:
     (class Array)
     'new
     (lambda (self)
       (make-vector 0)))

(addSelector:withMethod:
     (class Array)
     'with:
     (lambda (self anObject)
       (vector anObject)))

(addSelector:withMethod:
     (class Array)
     'with:with:
     (lambda (self obj1 obj2)
       (vector obj1 obj2)))

(addSelector:withMethod:
     (class Array)
     'with:with:with:
     (lambda (self obj1 obj2 obj3)
       (vector obj1 obj2 obj3)))

(addSelector:withMethod:
     (class Array)
     'with:with:with:with:
     (lambda (self obj1 obj2 obj3 obj4)
       (vector obj1 obj2 obj3 obj4)))

(addSelector:withMethod:
     (class Array)
     'withAll:
     (lambda (self aCollection)
       (let ( (elts st-nil) )
         (perform:with: aCollection
                       'do:
                       (lambda (elt)
                         (set! elts (cons elt elts))))
         (list->vector (reverse elts)))))

(addSelector:withMethod:
     Array
     'do:
     (lambda (self aBlock)
       (vector-for-each aBlock self)
       self))

(addSelector:withMethod:
     Array
     'printString
     printString)

(addSelector:withMethod:
     Array
     'printOn:
     (lambda (self port)
       (display "#( " port)
       (vector-for-each
        (lambda (each)
          ($: each 'printOn: port)
          (display " " port))
        self)
       (display ")" port))
)

(addSelector:withMethod:
     Array
     'select:
     (lambda (self predicate?)
       (let ( (results '()) )
         (vector-for-each
          (lambda (each)
            (when (predicate? each)
              (set! results (cons each results))))
          self)
         (list->vector (reverse results))))
)

(addSelector:withMethod:
     Array
     'detect:  ;; here for testing
     (lambda (self predicate?)
       (let ( (myLen (vector-length self))
              (result #f)
            )
         (let loop ( (index 0) )
           (when (< index myLen)
            (if (predicate? (vector-ref self index))
                (set! result #t)
                (loop (+ 1 index)))))
         result))
)

(addSelector:withMethod:
     Array
     'asArray
     (lambda (self) ;; called by subclasses
       (if (eq? (class self) Array)
           self
           (superPerform:with: self 'asArray))))

(addSelector:withMethod:
     Array
     'asString
     (lambda (self)
       (list->string
        (map integer->char (vector->list self)))))

(addSelector:withMethod:
     Array
     'swap:with:
     (lambda (self oneIndex anotherIndex)
       (let* ( (index1 (- oneIndex 1)) ;; Scheme 0-based
               (index2 (- anotherIndex 1)) ;; ST 1 based
               (elt1 (vector-ref self index1))
               (elt2 (vector-ref self index2))
             )
         (vector-set! self index2 elt1)
         (vector-set! self index1 elt2)
         self)))

;;; Smalltalk ByteArrays are Scheme Bytevectors

(addSelector:withMethod:
     ByteArray
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ByteArray)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     ByteArray
     'comment:
"I present an ArrayedCollection whose elements are integers between 0 and 255."
)

(perform:with:
     ByteArray
     'category: 'Collections-Arrayed)

(addSelector:withMethod:
     (class ByteArray)
     'basicNew:
     (lambda (self size)
       (make-bytevector size 0)))

(addSelector:withMethod:
     (class ByteArray)
     'new:
     (lambda (self size)
       (perform:with: self 'basicNew: size)))

(addSelector:withMethod:
     (class ByteArray)
     'new 
     (lambda (self)
       (perform:with: self 'basicNew: 0)))

(addSelector:withMethod:
     (class ByteArray)
     'withAll:
     (lambda (self aCollection)
       (let* ( (size (perform: self 'size))
               (newByteArray (make-bytevector size 0))
               ;; cache constant method
               (at: (primLookup: (behavior aCollection) 'at:))
             )
         (let loop ( (index 0) )
           (when (< index size) ;; Scheme 0 based
             (bytevector-set!
                  newByteArray
                  index
                  (at: aCollection (+ 1 index))) ;; ST 1 based
             (loop (+ index 1)))
             newByteArray))))

(addSelector:withMethod:
     ByteArray
     'at:
     (lambda (self index)
       ;; NB: ST 1-based, Scheme 0-based
       (if (<= 1 index (bytevector-length self))
           (bytevector-ref self (- index 1))
           (error 'at:
                  "Index out of range"
                  self
                  index))))
     
(addSelector:withMethod:
     ByteArray
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-set! self (- index 1) newVal)
           (error 'at:put: "Index out of range" self index))))

(addSelector:withMethod:
     ByteArray
     'size 
     (lambda (self)
    ;; (perform: self 'basicSize)
       (bytevector-length self)))

(addSelector:withMethod:
     ByteArray
     'basicSize
     (lambda (self)
       (bytevector-length self)))

(addSelector:withMethod:
     ByteArray
     'printOn:
     (lambda (self port)
       (display "#[ " port)
       (bytevector-for-each
        (lambda (each)
          ($: each 'printOn: port)
          (display " " port))
        self)
       (display "]" port))
)

(addSelector:withMethod:
     ByteArray
     'do:
     (lambda (self aBlock)
       (bytevector-for-each aBlock self)
       self))


(addSelector:withMethod:
     ByteArray
     'asByteArray
     (lambda (self) self))

(addSelector:withMethod:
     ByteArray
     'asString
     (lambda (self)
       (let* ( (strLen (bytevector-length self))
               (result (make-string strLen #\space))
             )
         (let loop ( (index 0) )
           (when (< index strLen)
             (string-set! result
                          index
                          (integer->char (bytevector-ref self index)))
             (loop (+ index 1))))
         result))
     )

) ) )

;;;			--- E O F ---			;;;
