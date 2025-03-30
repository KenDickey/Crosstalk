#!r6rs
;;; FILE: "st-sequence-coll.sls"
;;; IMPLEMENTS: SequenceableCollection & Set methods
;;; AUTHOR: Ken Dickey
;;; DATE: February 2025

(library (st-sequence-coll)

  (export
   init-st-sequence-coll

   SequenceableCollection
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
   )


(define SequenceableCollection
  (newSubclassName:iVars:cVars:
   Collection
   'SequenceableCollection '() '())
)


;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-sequence-coll)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-collection)
  
(perform:with: SequenceableCollection
               'methodDict:
               (clone-method-dictionary
                ($ Collection 'methodDict)))


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


'st-sequence-coll
) )

)

;;;			--- E O F ---			;;;
