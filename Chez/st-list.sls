;;; FILE: "st-list.sls"
;;; IMPLEMENTS: Scheme Pair, List
;;; AUTHOR: Ken Dickey
;;; DATE: 21 Januany 2017

(library (st-list)

  (export
   List
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs bytevectors (6))
   (rnrs hashtables (6)) ; string-hash
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs mutable-strings (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-character)
   (st-sequence-coll)
   (st-array-coll)
   (st-string)
   )


(define List
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'List '() '())
)


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(perform:with: List 'methodDict: st-list-behavior)

(perform:with:
     List
     'comment:
"I present Scheme proper immutable lists, which are
 composed of a chain of two element pairs
 with first and rest"
)

(perform:with:
     List
     'category: 'Collections-Sequenceable)

(addSelector:withMethod:
     List
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'List)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     List
     'size
     (lambda (self) (length self)))

(addSelector:withMethod:
     List
     'basicSize ;; NB: not really useful in this context
     (lambda (self)
       (length self)))

(addSelector:withMethod:
     (class List)
     'first:rest:
     (lambda (self the-car the-cdr)
       (unless (list? the-cdr)
         (error 'first:rest: "only supports proper lists" the-cdr))
       (cons the-car the-cdr)))

(addSelector:withMethod:
     Object
     'cons:
     (lambda (self other)
       (unless (list? other)
         (error 'cons: "only supports proper lists" other))
       (cons self other)))

(addSelector:withMethod:
     Object
     'asList
     (lambda (self)
       (cons self st-nil)))

(addSelector:withMethod:
     (class List)
     'with:
     (lambda (self anObject)
       (cons anObject st-nil)))

(addSelector:withMethod:
     (class List)
     'with:with:
     (lambda (self obj1 obj2)
       (list obj1 obj2)))

(addSelector:withMethod:
     (class List)
     'with:with:with:
     (lambda (self obj1 obj2 obj3)
       (list obj1 obj2 obj3)))

(addSelector:withMethod:
     (class List)
     'with:with:with:with:
     (lambda (self obj1 obj2 obj3 obj4)
       (list obj1 obj2 obj3 obj4)))


(addSelector:withMethod:
     (class List)
     'withAll:
     (lambda (self aCollection)
       (let ( (elts st-nil) )
         (perform:with: aCollection
                       'do:
                       (lambda (elt)
                         (set! elts (cons elt elts))))
         (reverse elts))))


(addSelector:withMethod:
     List
     'do:
     (lambda (self aBlock)
       (for-each aBlock self)
       self))

(addSelector:withMethod:
     List
     'asArray
     (lambda (self) ;; called by subclasses
       (list->vector self)))

(addSelector:withMethod:
     List
     'printString
     safer-printString)

(addSelector:withMethod:
     List
     'printOn:
     (lambda (self port)
       (display "List" port)
       (display "( " port)
       (perform:with:
           self 'printElementsOn: port)
       (display ")" port)))

(addSelector:withMethod:
     List
     'printElementsOn:
     (lambda (self port)
       (for-each (lambda (elt)
                   ($: elt 'printOn: port)
                   (display " " port))
                 self)))

(addSelector:withMethod:
     List
     'first
     (lambda (self) (car self)))

(addSelector:withMethod:
     List
     'rest
     (lambda (self) (cdr self)))

(addSelector:withMethod:
     List
     'value  ;; ValueLink protocol
     (lambda (self) (car self)))

(addSelector:withMethod:
     List
     'next  ;; ValueLink protocol
     (lambda (self) (cdr self)))

(addSelector:withMethod:
     List
     'second
     (lambda (self) (cadr self)))

(addSelector:withMethod:
     List
     'third
     (lambda (self) (caddr self)))

(addSelector:withMethod:
     List
     'last
     (lambda (self)
       (let loop ( (my-list self) )
         (cond
          ((null? my-list) '())
          ((null? (cdr my-list)) (car my-list))
          (else (loop (cdr my-list)))))))

(addSelector:withMethod:
     List
     'at:
     (lambda (self index)
       (when (< index 1)
         (error 'at: "Smalltalk indexes start at 1" index))
       (let loop ( (my-list self) (count 1) )
         (cond
          ((null? my-list)
           (error 'at: "fell off end of list" self index))
          ((= count index) (car my-list))
          (else (loop (cdr my-list) (+ count 1)))))))

(addSelector:withMethod:
     List
     'at:put:
     (lambda (self index whatever)
       (error 'at:put: "Lists are immutable" self)))

(addSelector:withMethod:
     List
     'collect:
     (lambda (self aBlock)
       (map aBlock self)))

(addSelector:withMethod:
     List
     'select:
     (lambda (self predicate?)
       (let loop ( (elts self) (result '()) )
         (cond
          ((null? elts) (reverse result)) ;; maintain order
          ((predicate? (car elts))
           (loop (cdr elts) (cons (car elts) result)))
          (else
           (loop (cdr elts) result))))))

(addSelector:withMethod:
     List
     'detect:
     (lambda (self predicate?)
       (let loop ( (elts self) )
         (cond
          ((null? elts) #f)
          ((predicate? (car elts)) #t)
          (else
           (loop (cdr elts)))))))


)

;;;			--- E O F ---			;;;
