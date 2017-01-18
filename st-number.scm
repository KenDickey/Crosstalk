;;; FILE: "st-number.scm"
;;; IMPLEMENTS: Number abstract class
;;;		Complex, Float, Fraction, Integer
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-core-classes)

(define Number
  (newSubclassName:iVars:cVars:
   Magnitude
   'Number '() '())
)

;; Scheme numbers form a tower of types
;;  different than the Number hierarchy of
;;  Smalltalk.
;; It is a tower where each lower level is
;;  a restriction on those above 

;;  Number
;;   Complex
;;     Real  (st Float)
;;       Rational  (st Fraction)
;;         Integer

;; So an Integer isA Rational isA Real ..

(define Complex
  (newSubclassName:iVars:cVars:
   Number
   'Complex '() '())
)

(define Float ;; real
  (newSubclassName:iVars:cVars:
   Complex
   'Float '() '())
)

(define Fraction
  (newSubclassName:iVars:cVars:
   Float
   'Fraction '() '())
)

(define Integer
  (newSubclassName:iVars:cVars:
   Fraction
   'Integer '() '())
)

(set! st-integer-behavior (perform: Integer 'methodDict))
(set! st-fraction-behavior (perform: Fraction 'methodDict))
(set! st-real-behavior (perform: Float 'methodDict))
(set! st-complex-behavior (perform: Complex 'methodDict))

;;; Number

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
          (angle aComplex)))

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

;;Nota Bene:
;; (string-length "\\\\") --> 2
;; (string->list (symbol->string '|\\\\|)) --> (#\\ #\\)

(addSelector:withMethod: 
        Number 
        '|\\\\| ;; Smalltalk #'\\'
        (lambda (self aNumber)
          (modulo self aNumber)))

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
        'zero
        (lambda (self) 0))

(addSelector:withMethod: 
        Number
        'isZero
        (lambda (self)
          (zero? self)))

(addSelector:withMethod: 
        Number
        'one
        (lambda (self) 1))

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
        (lambda (self aNumber) (atan self aNumber)))

;; ##FIXME: Hyperbolics


;;; Complex

(addSelector:withMethod: 
        Number
        'i
        (lambda (self)
          (make-rectangular 0 self)))

(addSelector:withMethod: 
        Complex
        'i	;; ^ self * 1i
        (lambda (self) (* self (sqrt -1))))

(addSelector:withMethod: 
        Complex
        'realPart
        (lambda (self) (real-part self)))

(addSelector:withMethod: 
        Complex
        'imagePart
        (lambda (self) (imag-part self)))

(addSelector:withMethod: 
        (class Complex)
        'real:imaginary:
        (lambda (self re im)
          (make-rectangular re im)))

(addSelector:withMethod: 
        (class Complex)
        'abs:arg:
        (lambda (self distance angle)
          (make-polar distance angle)))

(addSelector:withMethod: 
        Complex
        'magnitude
        (lambda (self) (magnitude self)))

(addSelector:withMethod: 
        Complex
        'angle
        (lambda (self) (angle self)))

(addSelector:withMethod: 
 	Complex
        'printOn:
        (lambda (self port)
          (format port "(~a +~ai)"
                  (real-part self)
                  (imag-part self)))
)
; @@@ MORE ..@@@

;;; Float (Real)

(addSelector:withMethod: 
 	Float
        'printOn:
        (lambda (self port)
          ;;@@FIXME: number format scm->st
          (display self port))
)
                                        ;
; @@@ MORE ..@@@
;;; Rational (Fraction)
; @@@ MORE ..@@@
;;; Integer

; @@@ MORE ..@@@

(addSelector:withMethod: 
 	Integer
        'timesRepeat:
        (lambda (self aThunk)
          (let loop ( (count 0) )
            (when (< count self)
              (aThunk)
              (loop (+ count 1)))))
)

;; (provide 'st-number)

;;;			--- E O F ---			;;;
