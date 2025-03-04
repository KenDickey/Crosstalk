#!r6rs
;;; FILE: "st-core-classes4.sls"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-core-classes4)

  (export
   SequenceableCollection
   ArrayedCollection
   Array
   ByteArray
   )
  
  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs bytevectors (6))
   (rnrs io simple (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs sorting (6))
   (rnrs hashtables (6))
   (only (chezscheme)
         format
         vector-copy
       )
   (st-base)
   (st-core-classes)
   (st-core-classes2)
   (st-core-classes3)
   )


(define SequenceableCollection
  (newSubclassName:iVars:cVars:
   Collection
   'SequenceableCollection '() '())
)

(define ArrayedCollection
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'ArrayedCollection '() '())
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

(define bytevector-ref  bytevector-u8-ref)
(define bytevector-set! bytevector-u8-set!)


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


;;; Set; Identity Set

(perform:with:
     Set
     'category:
     'Collections-Unordered)

(perform:with:
     Set
     'comment:
"I am an unordered collection of non-nil objects
 which does not contain duplicates."
)

(perform:with:
     IdentitySet
     'category:
     'Collections-Unordered)

(perform:with:
     IdentitySet
     'comment:
"I am the same as a Set,
 but my comparisons are with #== not #="
)

(addSelector:withMethod:
     Set
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Set)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     (class Set)
     'new:
     (lambda (self size)
       (let ( (newInst (perform: self 'new)) )
         (perform:with: newInst 'init: size)
         newInst)))

(addSelector:withMethod:
     Set
     'initialize
     (let ( (defaultSize 4) )
       (lambda (self)
         (perform:with: self 'init: defaultSize)
)    ) )

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
     (class Set)
     'with:
     (lambda (self elt1)
       (let ( (aSet (perform:with: Set 'new: 1)) )
         (perform:with: aSet 'add: elt1)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:
     (lambda (self elt1 elt2)
       (let ( (aSet (perform:with: Set 'new: 2)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:with:
     (lambda (self elt1 elt2 elt3)
       (let ( (aSet (perform:with: Set 'new: 3)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         (perform:with: aSet 'add: elt3)
         aSet)))

(addSelector:withMethod:
     (class Set)
     'with:with:with:with:
     (lambda (self elt1 elt2 elt3 elt4)
       (let ( (aSet (perform:with: Set 'new: 4)) )
         (perform:with: aSet 'add: elt1)
         (perform:with: aSet 'add: elt2)
         (perform:with: aSet 'add: elt3)
         (perform:with: aSet 'add: elt4)
         aSet)))

(addSelector:withMethod:
     Set
     'fullCheck  ;; private
     (lambda (self)
       ;; Keep array at least 1/4 free for
       ;; better hash behavior
       (let ( (array-size
               (perform: (perform: self 'array)
                         'size))
              (tally (perform: self 'tally))
            )
         (unless (> (- array-size tally)
                    (max 1 (floor (/ array-size 4))))
           (perform: self 'grow)))))

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

(addSelector:withMethod:
     Set
     'noCheckAdd: ;; private -- obj not a duplicate
     (lambda (self elt)
       (let ( (index (perform:with: self 'findElementOrNil: elt)) )
         (perform:with:with:
              (perform: self 'array)
              'at:put: index elt)
         (perform:with: self 'tally:
                        (+ 1 (perform: self 'tally)))
         self)))

(addSelector:withMethod:
     Set
     'scanFor:
     (lambda (self obj)
       ; Scan key array for 1st slot containing nil
       ; or an element matching obj.  Answer index or zero.
       ; Subclasses may override me for different match predicates.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (mod (equal-hash obj) ;; hash fn
                               array-size))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
;;             (newline) (display index)
             (cond
              ((st-nil? elt)    (+ 1 index)) ;; Scheme->ST index
              ((equal? obj elt) (+ 1 index)) ;; Scheme->ST index ;; equal?
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
  ;;                 (newline) (display index)
                   (let ( (elt (vector-ref array index)) )
                     (cond
                      ((st-nil? elt)    (+ 1 index))
                      ((equal? obj elt) (+ 1 index))
                      ((= index mid-end)
                       0) ;; failed
                      (else (left-loop (+ 1 index))))))
               ))
              (else (right-loop (+ 1 index)))))))))


(addSelector:withMethod:
     Set
     'findElementOrNil: ;; private
     (lambda (self obj)
       ;; Answer first nil (empty) slot or
       ;; slot which contains obj
       ;; or 0.
       (let ( (index (perform:with: self 'scanFor: obj)) )
         (if (> index 0)
             index
             (error 'addSelector:withMethod:
                    "Internal error: No free space in set!"
                    self)))))

(addSelector:withMethod:
     Set
     'swap:with:
     (lambda (self oneIndex anotherIndex)
       (perform:with:with:
          (perform: self 'array)
          'swap:with: oneIndex anotherIndex)))

(addSelector:withMethod:
     Set
     'size ;; number of elements
     (lambda (self) (perform: self 'tally)))

(addSelector:withMethod:
     Set
     'do:
     (lambda (self aBlock)
       (if (zero? (perform: self 'tally))
           self
           (perform:with: (perform: self 'array)
                          'do:
                          (lambda (elt)
                            (unless (st-nil? elt)
                              (aBlock elt)))
       )  )
) )

(addSelector:withMethod:
     Set
     '=
     (lambda (self otherSet)
       (call/cc
        (lambda (return)
          (unless (perform:with: otherSet
                                 'isKindOf:
                                 Set)
            (return st-false))
          (unless (equal? (perform: self     'tally)
                          (perform: otherSet 'tally))
            (return st-false))
          (perform:with: self
                         'do:
                         (lambda (elt)
                           (unless (perform:with:
                                    otherSet
                                    'includes:
                                    elt)
                             (return st-false))))
          (return st-true)))))

(addSelector:withMethod:
     Set
     'includes:
     (lambda (self obj)
       (let ( (index
                 (perform:with: self 'findElementOrNil: obj))
            )
         (not (st-nil? (perform:with: self 'keyAt: index))))
)   )

(addSelector:withMethod:
     Set
     'occurrencesOf:
     (lambda (self elt)
       (if ($: self 'includes: elt)
           1
           0)))

(addSelector:withMethod:
     Set
     'keyAt:
     (lambda (self index)
       (perform:with:
          (perform: self 'array) 'at: index)))

(addSelector:withMethod:
     Set
     'addAll:
     (lambda (self aCollection)
       ($: aCollection
           'do:
           (lambda (elt) ($: self 'add: elt)))
       self))


(addSelector:withMethod:
     Set
     'add:
     (lambda (self newObj)
       (when (st-nil? newObj)
         (error
          'addSelector:withMethod:
          "Set's can't meaningly contain nil as an element"))
       (let ( (index (perform:with: self
                                    'findElementOrNil:
                                    newObj))
            )
         (when (st-nil? (perform:with: self 'keyAt: index))
           (perform:with:with:
               self
               'atNewIndex:put: index newObj))
         ;; else obj already present..
         newObj)))

(addSelector:withMethod:
     Set
     'atNewIndex:put:
     (lambda (self index obj)
       (perform:with:with:
          (perform: self 'array) 'at:put: index obj)
       (perform:with: self
                      'tally:
                      (+ 1 (perform: self 'tally)))
       (perform: self 'fullCheck)
       self))

(addSelector:withMethod:
     Set
     'remove:ifAbsent:
     (lambda (self oldObj absentBlock)
       (let ( (index (perform:with: self
                                    'findElementOrNil:
                                    oldObj))
              (array (perform: self 'array))
            )
       (if (st-nil? (perform:with: self 'keyAt: index))
           (absentBlock)
           (begin
             (perform:with:with:
              (perform: self 'array) 'at:put: index st-nil)
             (perform:with: self
                           'tally:
                           (- (perform: self 'tally) 1))
             (perform:with: self 'fixCollisionsFrom: index)
             oldObj)))))
                   
(addSelector:withMethod:
     Set
     'fixCollisionsFrom:
     (lambda (self oldIndex)
       ;; Removed elt at (ST) index.
       ;; Now relocate entries displaced by hash
       ;; collision at this index
       (let* ( (length (perform: (perform: self 'array) 'size))
               (fixupIndex
                 (if (= oldIndex length) 1 (+ 1 oldIndex)))
             )
         (let loop ( (oldIndex fixupIndex)
                     (elt (perform:with: self 'keyAt: fixupIndex)) )
           (unless (st-nil? elt)
             (let ( (newIndex (perform:with: self 'findElementOrNil: elt)) )
               (unless (= newIndex oldIndex)
                 (perform:with:with: self 'swap:with: oldIndex newIndex))
               (loop newIndex (perform:with: self 'keyAt: fixupIndex))))))     
)    )

(addSelector:withMethod:
     Set
     'collect:
     (lambda (self aBlock)
       (let ( (new-set (perform:with: (class self) ;NB: subclass may invoke
                                      'new:
                                      (perform: self 'size)))
              (array (perform: self 'array))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: new-set 'add: (aBlock elt))))
          array)
         new-set)))

(addSelector:withMethod:
     Set
     'copy
     (lambda (self)
       (let ( (the-copy (perform: (class self) 'new)) )
         (perform:with: the-copy 'tally: (perform: self 'tally))
         (perform:with: the-copy 'array:
                        (vector-copy (perform: self 'array)))
         the-copy)))

(addSelector:withMethod:
     Set
     'asArray
     (lambda (self)
       (let ( (elts '()) )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (set! elts (cons elt elts))))
          (perform: self 'array))
       (list->vector elts))))

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

(addSelector:withMethod:
     Set
     'scanFor:
     (lambda (self obj)
       ; Scan key array for 1st slot containing nil
       ; or an element matching obj.  Answer index or zero.
       ; Subclasses may override me for different match predicates.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (mod (equal-hash obj) ;; hash fn
                           array-size))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
;;             (newline) (display index)
             (cond
              ((st-nil? elt) (+ 1 index)) ;; Scheme->ST index
              ((eq? obj elt) (+ 1 index)) ;; Scheme->ST index ;; eq?
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
  ;;                 (newline) (display index)
                   (let ( (elt (vector-ref array index)) )
                     (cond
                      ((st-nil? elt) (+ 1 index))
                      ((eq? obj elt) (+ 1 index))
                      ((= index mid-end)
                       0) ;; failed
                      (else (left-loop (+ 1 index))))))
               ))
              (else (right-loop (+ 1 index)))))))))

;;; Collections


(perform:with:
     Collection
     'comment:
"I am the abstract superclass of all classes that represent a group of elements."
)

(perform:with:
     Collection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     Collection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Collection)
           (superPerform:with: self 'is: symbol))))


(perform:with:
     SequenceableCollection
     'comment:
"I am an abstract superclass for collections that have a well-defined order
 associated with their elements. Thus each element is externally-named by
 integers referred to as indices."
)

(perform:with:
     SequenceableCollection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     SequenceableCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'SequenceableCollection)
           (superPerform:with: self 'is: symbol))))


(perform:with:
     ArrayedCollection
     'comment:
"I am an abstract collection of elements with a fixed range
 of integers (from 1 to n>=0) as external keys."
)

(perform:with:
     ArrayedCollection
     'category: 'Collections-Abstract)

(addSelector:withMethod:
     ArrayedCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ArrayedCollection)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Collection
     'printElementsOn:
     (lambda (self port)
       (display "( " port)
       (perform:with: self
                      'do:
                      (lambda (elt)
                        (perform:with:
                           elt 'printOn: port)
                        (display " " port)))
       (display ")" port)))


(addSelector:withMethod:
     SequenceableCollection
     'withIndexDo:
     (lambda (self elementAndIndexBlock)
;; "Just like with:do: except that the iteration index
;;   supplies the second argument to the block."
       (let ( (limit ($ self 'size)) )
         (let loop ( (index 1) )
           (when (<= index limit)
             (elementAndIndexBlock
                 ($: self 'at: index)
                 index)
             (loop (+ index 1)))))))

(addSelector:withMethod:
     SequenceableCollection
     'beginsWith:
     (lambda (self aSequenceableCollection)
       (if (or ($ aSequenceableCollection 'isEmpty)
               (< ($ self 'size)
                  ($ aSequenceableCollection 'size)))
           st-false
           (let ( (max ($ aSequenceableCollection 'size)) )
             (let loop ( (index 1) )
               (cond
                ((> index max)
                 st-true) ;; all match
                (($: ($: self 'at: index)
                     '=
                     ($: aSequenceableCollection 'at: index))
                 (loop (+ index 1)))
                (else st-false)))
        ) ) )
)

($:: (smalltalkAt: 'SequenceableCollection)
     'addSelector:withMethod:
     'endsWith:
     (lambda (self aSequenceableCollection)
       (call/cc
         (lambda (return)
           (let ((start st-nil))
             ($: ($: ($ aSequenceableCollection 'isEmpty)
                     'or:
                     (lambda ()
                       ($: ($ self 'size)
                           '<
                           ($ aSequenceableCollection 'size))))
                 'ifTrue:
                 (lambda () (return st-false)))
             (let ((%%val%%
                     ($: ($ self 'size)
                         '-
                         ($ aSequenceableCollection 'size))))
               (set! start %%val%%)
               %%val%%)
             ($: aSequenceableCollection
                 'withIndexDo:
                 (lambda (each index)
                   ($: ($: ($: self 'at: ($: start '+ index)) '~= each)
                       'ifTrue:
                       (lambda () (return st-false)))))
             (return st-true))))))


)

;; Arrays


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
         (perform:with aCollection
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
