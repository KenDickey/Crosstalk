#!r6rs
;;; File: "st-list.ss"
;;; IMPLEMENTS: List
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;; (load "st-core-classes.ss")
;; (load "st-core-methods.ss")
;; (load "st-collection.ss")

(define List
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'List '() '())
)
(rebase-mdict! List st-list-behavior)

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

(addSelector:withMethod:
     (class List)
     'with:
     (lambda (self elt1)
       (list elt1)))

(addSelector:withMethod:
     (class List)
     'with:with:
     (lambda (self elt1 elt2)
       (list elt1 elt2)))

(addSelector:withMethod:
     (class List)
     'with:with:with:
     (lambda (self elt1 elt2 elt3)
       (list elt1 elt2 elt3)))

(addSelector:withMethod:
     (class List)
     'with:with:with:with:
     (lambda (self elt1 elt2 elt3 elt4)
       (list elt1 elt2 elt3 elt4)))

'st-list

;;;			--- E O F ---			;;;
