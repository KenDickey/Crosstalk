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

(addSelector:withMethod: 
        Magnitude
        'between:and:
        (lambda (self min max)
          (<= min self max)))

;; (provide 'st-number)

;;;			--- E O F ---			;;;
