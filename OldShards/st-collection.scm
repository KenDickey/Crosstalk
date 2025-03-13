;;; FILE: "st-collection.scm"
;;; IMPLEMENTS: Collection, SequenceableCollection, ArrayedCollection
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-core-classes)

(define Collection
  (newSubclassName:iVars:cVars:
   Object
   'Collection '() '())
)

(perform:with:
     Collection
     'comment:
"I am the abstract superclass of all classes that represent a group of elements."
)

(perform:with:
     Collection
     'category: '|Collections-Abstract|)

(addSelector:withMethod:
     Collection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Collection)
           (superPerform:with: self 'is: symbol))))

(define SequenceableCollection
  (newSubclassName:iVars:cVars:
   Collection
   'SequenceableCollection '() '())
)

(perform:with:
     SequenceableCollection
     'comment:
"I am an abstract superclass for collections that have a well-defined order
 associated with their elements. Thus each element is externally-named by
 integers referred to as indices."
)

(perform:with:
     SequenceableCollection
     'category: '|Collections-Abstract|)

(addSelector:withMethod:
     SequenceableCollection
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'SequenceableCollection)
           (superPerform:with: self 'is: symbol))))

(define ArrayedCollection
  (newSubclassName:iVars:cVars:
   SequenceableCollection
   'ArrayedCollection '() '())
)

(perform:with:
     ArrayedCollection
     'comment:
"I am an abstract collection of elements with a fixed range
 of integers (from 1 to n>=0) as external keys."
)

(perform:with:
     ArrayedCollection
     'category: '|Collections-Abstract|)

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

;; (provide 'st-collection)

;;;			--- E O F ---			;;;
