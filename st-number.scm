;;; FILE: "st-number.sch"
;;; IMPLEMENTS: Number abstract class
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-core-classes)

(define Number
  (newSubclassName:iVars:cVars:
   Magnitude
   'Number '() '())
)

(perform:with:
     Number
     'category: '|Kernel-Numbers|)

(perform:with:
     Number
     'comment:
"Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity."
)

(addSelector:withMethod: 
        Magnitude
        'between:and:
        (lambda (self min max)
          (<= min self max)))

;; (provide 'st-number)

;;;			--- E O F ---			;;;
