;;; FILE: "st-array.scm"
;;; IMPLEMENTS: Array, ByteArray
;;;             (Scheme vectors & bytevectors)
;;; AUTHOR: Ken Dickey
;;; DATE: 17 June 2016

;; (require 'st-collection)


(define Array
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'Array '() '())
)

(perform:with:
     Array
     'comment:
"I present an ArrayedCollection whose elements are objects."
)

(perform:with:
     Array
     'category: '|Collections-Arrayed|)

(addSelector:withMethod:
     Array
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Array)
           (superPerform:with: self 'is: symbol))))


;; Scheme Vectors
(set! st-array-behavior (perform: Array 'methodDict))

;; #at: #at:put: #basicSize #at:modify
(add-array-accessors st-array-behavior 0)

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
         (perform:with aCollection
                       'do:
                       (lambda (elt)
                         (set! elts (cons elt elts))))
         (list->vector (reverse elts)))))


(define (vector-for-each proc vec)
  (for-each proc (vector->list vec)))
  
(addSelector:withMethod:
     Array
     'do:
     (lambda (self aBlock)
       (vector-for-each aBlock self)
       self))

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


;;; Bytevector


(define ByteArray
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'ByteArray '() '())
)

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
     'category: '|Collections-Arrayed|)

;; Scheme bytevectors
(set! st-bytevector-behavior (perform: ByteArray 'methodDict))

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
           (error "Index out of range" self index))))
     
(addSelector:withMethod:
     ByteArray
     'at:put:
     (lambda (self index newVal)
       (if (<= 1 index (bytevector-length self))
           (bytevector-set! self (- index 1) newVal)
           (error "Index out of range" self index))))

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

(define (bytevector-for-each proc bvec)
  (let ( (size (bytevector-length bvec)) )
    (let loop ( (index 0) )
      (when (< index size)
        (proc (bytevector-ref bvec index))
        (loop (+ 1 index)))
) ) )

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

;; (provide 'st-array)

;;;			--- E O F ---			;;;
