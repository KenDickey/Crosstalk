#!r6rs
;;; FILE: "st-array-coll.sls"
;;; IMPLEMENTS: Basic Class mechanics
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-array-coll)

  (export
   ArrayedCollection
   )
  
  (import
   (rnrs base)
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
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(addSelector:withMethod:
     ArrayedCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ArrayedCollection)
           (superPerform:with: self 'is: symbol))))

)
