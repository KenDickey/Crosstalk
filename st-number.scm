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
   'Float '() '(pi e))
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

(define pi (* 2 (acos 0)))

($: Float 'pi: pi)
($: Float 'e: (exp 1))

(define twicePi (* 4 (acos 0)))
;; 360 deg = 2 pi radians
(define (degreesToRadians deg)
  (/ (* twicePi deg) 360))
(define (radiansToDegrees rad)
  (/ (* 360 rad) twicePi))

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
        'degreesToRadians
        degreesToRadians)

(addSelector:withMethod: 
        Number
        'radiansToDegrees
        radiansToDegrees)

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
        'hash
        (lambda (self)
          (equal-hash self)))

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
        'min:
        (lambda (self other)
          (if (> self other)
              other
              self)))

(addSelector:withMethod: 
        Number
        'max:
        (lambda (self other)
          (if (< self other)
              other
              self)))

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

(addSelector:withMethod: 
        Number
        'from:to:
        (lambda (self start step)
          (Interval from: start to: stop by: 1)))

(addSelector:withMethod: 
        Number
        'to:do:
        (lambda (start stop aBlock)
          (let loop ( (index start) )
            (when (<= index stop)
              (aBlock index)
              (loop (+ index 1))))))


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
          (format port "(~a ~a~ai)"
                  (real-part self)
                  (if (negative? (image-part self)) #\- #\+)
                  (imag-part self)))
)

(define (printStringRadix: integer radix)
  (unless (integer? integer)
    (error "printWIthRadix: unsupported for this recevier" integer))
  (let ( (string-port (open-output-string)) )
    (printOn:base:showRadix: integer string-port radix #true)
    (get-output-string string-port)))
  ;; (format #f
  ;;         (if (negative? integer)
  ;;             "-~ar~a"
  ;;             "~ar~a")
  ;;         radix
  ;;         (number->string (abs integer) radix)))

(define (printOn:base:showRadix: integer port radix showRadix?)
  (unless (integer? integer)
    (error "printOn:base:showRadix: unsupported for this recevier" integer))
  (if showRadix?
      (format port
              (if (negative? integer) "-~ar~a" "~ar~a")
              radix
              (number->string (abs integer) radix))
      (format port
              (if (negative? integer) "-~a" "~a")
              (number->string (abs integer) radix)))
)
              

(addSelector:withMethod: 
 	Complex
        'printStringRadix:
        (lambda (self radix)
          (string-append
           "("
           (printStringRadix: (real-part self) radix)
           (if (negative? (imag-part self)) " " " +")
           (printStringRadix: (imag-part self) radix)
           " i)"))
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

(addSelector:withMethod: 
 	Float
        'printStringRadix:
        (lambda (self radix)
          (printStringRadix: self radix))
)

;
; @@@ MORE ..@@@
;;; Rational (Fraction)
; @@@ MORE ..@@@

(addSelector:withMethod: 
 	Fraction
        'printStringRadix:
        (lambda (self radix)
          (format #t "(~a / ~a)"
                  (printStringRadix: (numerator self) radix)
                  (printStringRadix: (denominator self) radix)))
)

;;; Integer


(addSelector:withMethod: 
 	Integer
        'printStringRadix:
        (lambda (self radix)
          (printStringRadix: self radix))
)



;;printOn: output base: base showRadix: flag
(addSelector:withMethod: 
 	Integer
        'printOn:base:showRadix:
        printOn:base:showRadix:
)

(addSelector:withMethod: 
 	Integer
        'bitAnd:
        (lambda (self other)
          (bitwise-and self other))
)

(addSelector:withMethod: 
 	Integer
        'bitOr:
        (lambda (self other)
          (bitwise-ior self other))
)

(addSelector:withMethod: 
 	Integer
        'bitXor:
        (lambda (self other)
          (bitwise-xor self other))
)

(addSelector:withMethod: 
 	Integer
        'bitInvert
        (lambda (self)
          (bitwise-not self))
)

(addSelector:withMethod: 
 	Integer
        'bitAt:
        (lambda (self index)
          (if (< index 1)
              0 ;; default if index out of range
              (if (bitwise-bit-set? self (- index 1))
                  1
                  0)))
)

(addSelector:withMethod: 
 	Integer
        'bitAt:put:
        (lambda (self index integer) ; index starts at 1
          (unless (integer? integer)
            (error "bitAt:put: bit to set must be integer" integer))
          (unless (> index 0)
            (error "bitAt:put: index ranges from 1" index))
          (let ( (mask (bitwise-arithmetic-shift-left 1 (- index 1)))
                 (bit  (bitwise-and integer 1)) ; use lowest bit
               )
            (if (zero? bit)
              (bitwise-and self (bitwise-not mask))
              (bitwise-ior self mask))))
)

(addSelector:withMethod: 
 	Integer
        'allMask:
        (lambda (self mask)
	  (= self (bitwise-and self mask)))
)

(addSelector:withMethod: 
 	Integer
        'anyMask:
        (lambda (self mask)
	  (not (zero? (bitwise-and self mask))))
)


(addSelector:withMethod: 
 	Integer
        'noMask:
        (lambda (self mask)
	  (zero? (bitwise-and self mask)))
)


(addSelector:withMethod: 
 	Integer
        'bitShift:
        (lambda (self shift)
          (bitwise-arithmetic-shift self shift))
)

(addSelector:withMethod: 
 	Integer
        'factorial
        (lambda (self)
          (unless (> self 0)
            (error "facorial: must be positive" self))
          (let loop ( (count 1) (result 1) )
            (if (<= count self)
                (loop (+ count 1) (* result count))
                result)))
)

(addSelector:withMethod: 
 	Integer
        'gcd
        gcd
)

(addSelector:withMethod: 
 	Integer
        'lcm
        lcm
)

(addSelector:withMethod: 
 	Integer
        'asInteger
        (lambda (self) self)
)

;; highBit

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
