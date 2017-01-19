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
     Collection
     'printElementsOn:
     (lambda (self port)
       (display "(" port)
       (perform:with: self
                      'do:
                      (lambda (elt)
                        (perform:with:
                           elt 'printOn: port)
                        (display " " port)))
       (display ")" port)))


;; (provide 'st-collection)

;;;			--- E O F ---			;;;
