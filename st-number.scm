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
"Class Number holds the most general methods for dealing with numbers."
;;"Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity."
)

(addSelector:withMethod: 
        Number
        'isNumber
        (lambda (self) st-true))

(addSelector:withMethod: 
        Number
        'isInteger
        (lambda (self)
          (integer? self)))

(addSelector:withMethod: 
        Number
        'isFraction
        (lambda (self)
          (rational? self)))

(addSelector:withMethod: 
        Number
        'numerator
        (lambda (aFraction)
          (numerator aFraction)))

(addSelector:withMethod: 
        Number
        'denominator
        (lambda (aFraction)
          (denominator aFraction)))

(addSelector:withMethod: 
        Number
        'isFloat
        (lambda (self)
          (real? self)))

(addSelector:withMethod: 
        Number
        'isComplex
        (lambda (self)
          (complex? self)))

(addSelector:withMethod: 
        Number
        'isComplex
        (lambda (self)
          (complex? self)))

(addSelector:withMethod: 
        (class Number)
        'real:imaginary:
        (lambda (self real imaginal)
          (make-rectangular real imaginal)))

(addSelector:withMethod: 
        (class Number)
        'rho:theta:
        (lambda (self fho theta)
          (make-polar rho theta)))

(addSelector:withMethod: 
        Number
        'realPart
        (lambda (aComplex)
          (real-part aCimplex)))

(addSelector:withMethod: 
        Number
        'imaginaryPart
        (lambda (aComplex)
          (imag-part aCimplex)))

(addSelector:withMethod: 
        Number
        'magnitude ;; NB: NOT #abs !!
        (lambda (aComplex)
          (magnitude aCimplex)))

(addSelector:withMethod: 
        Number
        'angle
        (lambda (aComplex)
          (angle aCimplex)))

(addSelector:withMethod: 
        Number
        '+
        (lambda (self aNumber)
          (+ self aNumber)))

(addSelector:withMethod: 
        Number
        '-
        (lambda (self aNumber)
          (- self aNumber)))

(addSelector:withMethod: 
        Number
        '*
        (lambda (self aNumber)
          (* self aNumber)))

(addSelector:withMethod: 
        Number
        '|/|
        (lambda (self aNumber)
          (/ self aNumber)))

(addSelector:withMethod: 
        Number
        '|//|
        (lambda (self aNumber)
          (floor (/ self aNumber))))

(addSelector:withMethod: 
        Number
        '=
        (lambda (self aNumber)
          (= self aNumber)))

(addSelector:withMethod: 
        Number
        '<
        (lambda (self aNumber)
          (< self aNumber)))

(addSelector:withMethod: 
        Number
        '>
        (lambda (self aNumber)
          (> self aNumber)))

(addSelector:withMethod: 
        Number
        '<=
        (lambda (self aNumber)
          (<= self aNumber)))

(addSelector:withMethod: 
        Number
        '>=
        (lambda (self aNumber)
          (>= self aNumber)))

;; ?correct?
;; (addSelector:withMethod: 
;;         Number
;;         '|\\|
;;         (lambda (self aNumber)
;;           (modulo self aNumber)))

(addSelector:withMethod: 
        Number
        'abs
        abs)

(addSelector:withMethod: 
        Number
        'between:and:
        (lambda (self min max)
          (<= min self max)))

(addSelector:withMethod: 
        Number
        'isZero
        (lambda (self)
          (zero? self)))

(addSelector:withMethod: 
        Number
        'even
        (lambda (self)
          (even? self)))

(addSelector:withMethod: 
        Number
        'odd
        (lambda (self)
          (odd? self)))

(addSelector:withMethod: 
        Number
        'negative
        (lambda (self)
          (negative? self)))

(addSelector:withMethod: 
        Number
        'strictlyPositive
        (lambda (self)
          (positive? self)))

(addSelector:withMethod: 
        Number
        'positive  ;; note #strictlyPositive
        (lambda (self)
          (>= self 0)))

(addSelector:withMethod: 
        Number
        'isNan
        (lambda (self)
          (nan? self)))

(addSelector:withMethod: 
        Number
        'isInfinite
        (lambda (self)
          (infinite? self)))

(addSelector:withMethod: 
        Number
        'isFinite ;; distinguish #isInfinite
        (lambda (self)
          (finite? self)))

(addSelector:withMethod: 
        Number
        'sign
        (lambda (self)
          (cond
           ((> self 0)  1)
           ((< self 0) -1)
           (else        0))))

(addSelector:withMethod: 
        Number
        'isDivisibleBy:
        (lambda (self aNumber)
          (cond
           ((zero? aNumber) st-false)
           ((integer? self)
            (zero? (modulo self aNumber)))
           (else  st-false))))

(addSelector:withMethod: 
        Number
        'floor floor)

(addSelector:withMethod: 
        Number
        'ceiling ceiling)

(addSelector:withMethod: 
        Number
        'truncate truncate)

(addSelector:withMethod: 
        Number
        'round round)

(addSelector:withMethod: 
        Number
        'sqrt sqrt)

(addSelector:withMethod: 
        Number
        'squared
        (lambda (self) (* self self)))

(addSelector:withMethod: 
        Number
        'cubed
        (lambda (self) (* self self self)))

(addSelector:withMethod: 
        Number
        'exp exp)

(addSelector:withMethod: 
        Number
        'ln
        (lambda (self) (log self)))

(addSelector:withMethod: 
        Number
        'log
        (lambda (self) (log self 10)))

(addSelector:withMethod: 
        Number
        'log:
        (lambda (self aNumber) (log self aNumber)))

(addSelector:withMethod: 
        Number
        'sin sin)

(addSelector:withMethod: 
        Number
        'cos cos)

(addSelector:withMethod: 
        Number
        'tan tan)

(addSelector:withMethod: 
        Number
        'arcSin
        (lambda (self) (asin self)))

(addSelector:withMethod: 
        Number
        'arcCos
        (lambda (self) (acos self)))

(addSelector:withMethod: 
        Number
        'arcTan
        (lambda (self) (atan self)))

(addSelector:withMethod: 
        Number
        'arcTan:
        (lambda (self aNUmber) (atan self aNumber)))

;; ##FIXME: Hyperbolics



;; (provide 'st-number)

;;;			--- E O F ---			;;;
