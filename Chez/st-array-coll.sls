#!r6rs
;;; FILE: "st-array-coll.sls"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-array-coll)

  (export
   init-st-array-coll
   
   ArrayedCollection
   )
  
  (import
   (rnrs base)
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)
   (st-sequence-coll)
   )

  
(define ArrayedCollection
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'ArrayedCollection '() '())
)

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-array-coll)
  (unless (initialized?)
    (initialized? #t)
    (init-st-sequence-coll)
  
  (perform:with: ArrayedCollection
               'methodDict:
               (clone-method-dictionary
                ($ SequenceableCollection 'methodDict)))


  (addSelector:withMethod:
     ArrayedCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ArrayedCollection)
           (superPerform:with: self 'is: symbol))))

) ) )

;;;			--- E O F ---			;;;
