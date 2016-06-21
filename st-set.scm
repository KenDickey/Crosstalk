;;; FILE: "st-set.sch"
;;; IMPLEMENTS: Set
;;; AUTHOR: Ken Dickey
;;; DATE: 20 July 2016

;; Code transliterated from PharoCandle "PCSet"

;; (requires 'st-array)


(define Set
  (newSubclassName:iVars:cVars:
   Collection
   'Set '(array tally) '())
)

(perform:with:
     Set
     'category:
     '|Collections-Unordered|)

(perform:with:
     Set
     'comment:
     "I am an unordered collection of non-nil objects which does not contain duplicates."
)

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
;;       (superPerform: self 'initialize)
         (perform:with: self 'tally: 0)
         (perform:with: self
                        'array:
                        (perform:with: Array 'new: initialSize))
         self))
)

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
       (let* ( (old-array (perform: self 'array))
               (array-size (perform: oldArray 'size))
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
     'noCheckAdd: ;; private
     (lambda (self elt)
       (let ( (index (perform:with: self 'findElementOrNil elt)) )
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
       ; Subclasses may override me for different match functions.
       (let* ( (array (perform: self 'array))
               (array-size (vector-length array))
               (start (modulo (equal-hash obj) ;; hash fn
                              (+ 1 array-size)))
               (right-end (- array-size 1)) ;; Scheme index 0 based
             )
         (let right-loop ( (index start) ) ;; start to end
           (let ( (elt (vector-ref array index)) )
             (cond
              ((st-nil? elt) index)
              ((eq? obj elt) index)
              ((= index right-end)
               (let ( (mid-end (- start 1)) )
                 (let left-loop ( (index 0) ) ;; Scheme arrays 0 based
                 ;; look 1 to start-1
                   (cond
                    ((st-nil? elt) index)
                    ((eq? obj elt) index)
                    ((= index mid-end)
                     0) ;; failed
                    (else (left-loop (+ 1 index))))))
               )
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
             (+ index 1) ;; ST 1 based
             (error "Internal error: No free space in set!" self)))))

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
       (if (zero? (perform self 'tally))
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
       (call/cc (return)
          (unless (perform:with: otherSet
                                 'isKoneOf
                                 Set)
            (return st-false))
          (unless (equal? (perform self     'tally)
                          (perform otherSet 'tally))
            (return st-false))
          (perfrom:with: self
                         'do:
                         (lambda (elt)
                           (unless (perform:with:
                                    otherSet
                                    'includes
                                    elt)
                             (return st-false))))
          (return st-true))))

(addSelector:withMethod:
     Set
     'includes:
     (lambda (self obj)
       (let ( (index (perform:with: self
                                    'findElementOrNil: obj))
            )
         (not (st-nil?
                (perform:with: self 'key-at: index))))))

(addSelector:withMethod:
     Set
     'key-at:
     (lambda (self index)
       (vector-ref (perform: self 'array)
                           (- index 1))))

(addSelector:withMethod:
     Set
     'add:
     (lambda (self newObj)
       (when (st-nil? newObj)
         (error
          "Set's can't meaningly contain nil as an element"))
       (let ( (index (perform:with: self
                                    'findElementOrNil:
                                    newObj))
            )
         (when (st-nil? (perform:with: self 'key-at: index))
           (perform:with:with:
               self
               'atNewIndex:put: index newObj))
         newObj)))

(addSelector:withMethod:
     Set
     'atNewIndex:put:
     (lambda (self index obj)
       (vector-set! (perform: self 'array) (- index 1) obj)
       (perform:with: self
                      'tally:
                      (+ 1 (perform: self 'tally)))
       (perform: self 'fullCheck)
       self))

(addSelector:withMethod:
     Set
     'remove:ifAbsent:
     (lambda (self oldObj absentBlock)
       (let ( (index (perfrom:with: self
                                    'findElementOrNil:
                                    oldObj))
              (array (perform: self 'array))
            )
       (if (st-nil? (vector-ref array (- index 1)))
           (absentBlock)
           (begin
             (vector-set! array (- index 1) st-nil)
             (perform:with self
                           'tally:
                           (- (perform: self 'talley) 1))
             (perform:with: self 'fixCollisionsFrom: index)
             oldObj)))))
                   
(addSelector:withMethod:
     Set
     'collect:
     (lambda (self aBlock)
       (let ( (new-set (perform:with: (class self)
                                      'new:
                                      (perform: self 'size)))
              (array (perform: self 'array))
            )
         (vector-for-each
          (lambda (elt)
            (unless (st-nil? elt)
              (perform:with: new-set 'add: elt)))
          array)
         new-set)))

(addSelector:withMethod:
     Set
     'copy
     (lambda (self)
       (let ( (copy (perform:with: (self class) 'new: 0)) )
         (perfrom:with: copy 'tally: (perform: self 'tally))
         (perform:with: copy 'array:
                        (vector-copy (perform: self 'array)))
         copy)))

;; (provides st-set)

;;;			--- E O F ---			;;;
