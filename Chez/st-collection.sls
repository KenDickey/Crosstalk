#!r6rs
;;; FILE: "st-collection.sls"
;;; IMPLEMENTS: Collection, Set, IdentitySet
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-collection)

  (export
   Collection
   Set
   IdentitySet
   )
  
  (import
   (rnrs base)
   (rnrs hashtables (6))
   (rnrs control (6))
   (rnrs io simple (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   )


(define Collection
  (newSubclassName:iVars:cVars:
   Object
   'Collection '() '())
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
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(addSelector:withMethod:
     Behavior
     'selectors
     ;; Answer identSet of non-inherited method selectors
     (lambda (self)
       (let ( (superDict
               ($ (superclass self) 'methodDict))
              (selfDict ($ self 'methodDict))
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
               ($ (superclass self) 'methodDict))
              (selfDict ($ self 'methodDict))
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


)
