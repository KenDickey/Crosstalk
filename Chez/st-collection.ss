#!r6rs
;;; File: "st-collection.ss"
;;; IMPLEMENTS: Collection, SequenceableCollection, ArrayedCollection,
;;; 	Set, IdentitySet
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;; (load "st-core-classes.ss")
;; (load "st-core-methods.ss")

(define Collection
  (newSubclassName:iVars:cVars:
   Object
   'Collection '() '())
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

(define Set
  (newSubclassName:iVars:cVars:
   Collection
   'Set '(array tally) '())
)

(define IdentitySet
  (newSubclassName:iVars:cVars:
   Set
   'IdentitySet '() '())
)


;;;======================================================

(addSelector:withMethod:
     Behavior
     'selectors
     ;; Answer identSet of non-inherited method selectors
     (lambda (self)
       (let ( (superDict
               ($ (superclass self) 'instanceBehavior))
              (selfDict ($ self 'instanceBehavior))
              (iSet ($ IdentitySet 'new))
            )
         ($: selfDict
             'keysAndValuesDo:
             (lambda (k v)
               (cond
                ((hashtable-contains? superDict k)
                 (when (not  ;; not same v as super
                        (eq? v
                             (hashtable-ref superDict
                                            k
                                            st-nil)))
                   ($ iSet 'add: k)))
                ;; else must be local; add selector
                (else ($: iSet 'add: k)))))
         iSet
       ) )
)

(addSelector:withMethod:
     (class Behavior)
     'selectors
     ;; Answer identSet of non-inherited method selectors
     (lambda (self)
       (let ( (superDict
               ($ (superclass self) 'instanceBehavior))
              (selfDict ($ self 'instanceBehavior))
              (iSet ($ IdentitySet 'new))
            )
         ($: selfDict
             'keysAndValuesDo:
             (lambda (k v)
               (cond
                ((hashtable-contains? superDict k)
                 (when (not  ;; not same v as super
                        (eq? v
                             (hashtable-ref superDict
                                            k
                                            st-nil)))
                   ($: iSet 'add: k)))
                ;; else must be local; add selector
                (else ($: iSet 'add: k)))))
         iSet
      ) )
   )


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

;;; SequenceableCollection


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

(addSelector:withMethod:
      SequenceableCollection
     'endsWith:
     (lambda (self aSequenceableCollection)
       (call/cc
         (lambda (return)
           (let ((start nil))
             ($: ($: ($ aSequenceableCollection 'isEmpty)
                     'or:
                     (lambda ()
                       ($: ($ self 'size)
                           '<
                           ($ aSequenceableCollection 'size))))
                 'ifTrue:
                 (lambda () (return false)))
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
                       (lambda () (return false)))))
             (return true))))))


;;; ArrayedCollection

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

($:: ($ (smalltalkAt: 'Set) 'class)
       'addSelector:withMethod:
       'new  ;; 4 is default size
       (lambda (self) ($: self 'new: 4)))

(addSelector:withMethod:
     (class Set)
     'new:
     (lambda (self size)
       ;; make large enough to hold size elts
       ;; without growing -- see #fullCheck
       (let* ( (initialSize
                (if (<= size 0)
                    1
                    (floor (/ (* (+ size 1) 4) 3))))
               (newSet ($ self 'basicNew))
              )
;;       (superPerform: newSet 'initialize) ;; unneeded
         (perform:with: newSet 'tally: 0)
         (perform:with: newSet
                        'array:
                        (perform:with: Array 'new: initialSize))
         ($ newSet 'initialize)))
     )

(addSelector:withMethod:
     Set
     'initialize
     (lambda (self) self))

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
;;       (superPerform: self 'initialize) ;; unneeded
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
         (unless (>= (- array-size tally)
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
       (unless (zero? (perform: self 'tally))
           (perform:with: (perform: self 'array)
                          'do:
                          (lambda (elt)
                            (unless (st-nil? elt)
                              (aBlock elt)))
                          )  )
       self)
)

(addSelector:withMethod:arity:
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
          (return st-true))))
     2)

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
       (let ( (length (perform: (perform: self 'array) 'size)) )
	 
	 (let loop ( (oldIndex
		      (if (= oldIndex length) 1 (+ 1 oldIndex)))
		   )
	   (let ( (elt (perform:with: self 'keyAt: oldIndex)) )

             (unless (st-nil? elt)
               (let ( (newIndex (perform:with: self 'findElementOrNil: elt)) )
;; (format #t "~%fixCollisionsFrom: old=~a new=~a" oldIndex newIndex) ; @@debug
		 (unless (= newIndex oldIndex)
                   (perform:with:with: self 'swap:with: oldIndex newIndex))
		 (loop oldIndex))))))
       self)
     )

(addSelector:withMethod:
     Set
     'collect:
     (lambda (self aBlock)
       (let* ( (size ($ self 'tally))
               (new-set
                (if (zero? size)
                    ($: (class self) 'new)
                    ($: (class self) 'new: size)))
               (array (perform: self 'array))
             )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              ($: new-set 'add: (aBlock elt))))
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
     Set
     'scanFor:
     (lambda (self obj)
       ; Scan key array for 1st slot containing nil
       ; or an element matching obj.  Answer index or zero.
       ; Subclasses may override me for different match predicates.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (modulo (equal-hash obj) ;; hash fn
                                array-size))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
;; (newline) (display index) (display " ") ; @@debug
             (cond
              ((st-nil? elt)    (+ 1 index)) ;; Scheme->ST index
              ((equal? obj elt) (+ 1 index)) ;; Scheme->ST index ;; eqv?
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
;;  (newline) (display index) (display " ") ; @@debug
                   (let ( (elt (vector-ref array index)) )
                     (cond
                      ((st-nil? elt)    (+ 1 index))
                      ((equal? obj elt) (+ 1 index)) ;; eqv?
                      ((= index mid-end)
                       0) ;; failed [Smalltalk is 1 based]
                      (else (left-loop (+ 1 index))))))
               ))
              (else (right-loop (+ 1 index)))))))))

'st-collection

;;;			--- E O F ---			;;;
