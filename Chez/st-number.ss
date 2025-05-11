#!r6rs
;;; File: "st-number.ss"
;;; IMPLEMENTS: Magnitude, Number, Complex,
;;;		Float, Fraction, Integer
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

(define Magnitude
  (newSubclassName:iVars:cVars:
   Object
   'Magnitude st-nil st-nil))
(rebase-mdict! Magnitude st-magnitude-behavior)

(define Number
  (newSubclassName:iVars:cVars:
   Magnitude
   'Number st-nil st-nil))
(rebase-mdict! Number st-number-behavior)

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
   'Complex st-nil st-nil))
(rebase-mdict! Complex st-complex-behavior)

(define Float ;; real
  (newSubclassName:iVars:cVars:
   Complex
   'Float st-nil '(pi e)))
(rebase-mdict! Float st-float-behavior)


(define Fraction
  (newSubclassName:iVars:cVars:
   Float
   'Fraction st-nil st-nil))
(rebase-mdict! Fraction st-fraction-behavior)

(define Integer
  (newSubclassName:iVars:cVars:
   Fraction
   'Integer st-nil st-nil))
(rebase-mdict! Integer st-integer-behavior)

 

;;;======================================================

(perform:with: Magnitude
               'comment:
"I'm the abstract class Magnitude that provides common protocol for objects that have
the ability to be compared along a linear dimension, such as dates or times.
Subclasses of Magnitude include Date, ArithmeticValue, and Time, as well as LookupKey.
 
 
My subclasses should implement
  < aMagnitude 
  = aMagnitude 
  hash

Here are some example of my protocol:
     3 > 4
     5 = 6
     100 max: 9
	7 between: 5 and: 10 
")

(perform:with:
     Magnitude
     'category: 'Kernel-Magnitude)

(addSelector:withMethod:
     Magnitude
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Magnitude)
           (superPerform:with: self 'is: symbol))))

(for-each
   (lambda (selector)
       (addSelector:withMethod: 
        Magnitude
        selector
        (make-subclassResponsibility selector))) ; in st-object
   '( < > = <= >= hash between:and: min: max: min:max: ))


;;; Number

(define pi (* 2 (acos 0)))
(define e (exp 1))

(define twicePi (* 4 (acos 0)))
;; 360 deg = 2 pi radians

(define (degreesToRadians deg)
  (/ (* twicePi deg) 360))

(define (radiansToDegrees rad)
  (/ (* 360 rad) twicePi))


(define (printStringRadix: integer radix)
  (unless (integer? integer)
    (error "printWIthRadix: unsupported for this recevier" integer))
  (let-values ( ((string-port stream->string)
                 (open-string-output-port))
              )
    (printOn:base:showRadix: integer string-port radix #t)
    (stream->string)))
  ;; (format #f
  ;;         (if (negative? integer)
  ;;             "-~ar~a"
  ;;             "~ar~a")
  ;;         radix
  ;;         (number->string (abs integer) radix)))

(define (printOn:base:showRadix: integer port radix showRadix?)
  (unless (integer? integer)
    (error 'printOn:base:showRadix:
           "unsupported for this recevier"
           integer))
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
     Number
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Number)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     Number
     'category: 'Kernel-Numbers)

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
        (lambda (self rho theta)
          (make-polar rho theta)))

(addSelector:withMethod: 
        Number
        'realPart
        (lambda (aComplex)
          (real-part aComplex)))

(addSelector:withMethod: 
        Number
        'imaginaryPart
        (lambda (aComplex)
          (imag-part aComplex)))

(addSelector:withMethod: 
        Number
        'magnitude ;; NB: NOT #abs !!
        (lambda (aComplex)
          (magnitude aComplex)))

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

(addSelector:withMethod:arity:
        Number
        '+
        (lambda (self aNumber) (+ self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        '-
        (lambda (self aNumber) (- self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        '*
        (lambda (self aNumber) (* self aNumber))
	2)

(addSelector:withMethod:arity: ;; redefined in "st-erro-bj.scm"
        Number
        (string->symbol "/")
	(lambda (self aNumber) (/ self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        (string->symbol "//")
        (lambda (self aNumber) (floor (/ self aNumber)))
	2)

(addSelector:withMethod:arity:
        Number
        (string->symbol "=")
        (lambda (self aNumber)
          (= self aNumber))
	2)

(addSelector:withMethod: 
        Number
        'hash
        (lambda (self)
          (equal-hash self)))

(addSelector:withMethod:arity:
        Number
        '<
        (lambda (self aNumber) (< self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        '>
        (lambda (self aNumber) (> self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        '<=
        (lambda (self aNumber) (<= self aNumber))
	2)

(addSelector:withMethod:arity:
        Number
        '>=
        (lambda (self aNumber) (>= self aNumber))
	2)

(addSelector:withMethod:arity:
        Number 
        (string->symbol "\\\\") ;; #\\
        (lambda (self aNumber) (modulo self aNumber))
	2)

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
        'truncated
        (lambda (self) (exact (truncate self))))

(addSelector:withMethod: 
        Number
        'rounded
        ;; NB: Scheme rounds to even, St does NOT
        (lambda (self)
          (if (= 0.5 (abs (- self (truncate self))))
              (exact (round ((if (negative? self) - +) self 0.01)))
              (exact (round self)))))

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
        'to:
        (lambda (start stop)
          ($::: (smalltalkAt: 'Interval) 'from:to:by: start stop 1)))

(addSelector:withMethod: 
        Number
        'to:by:
        (lambda (start stop step)
          ($::: (smalltalkAt: 'Interval) 'from:to:by: start stop step)))

(addSelector:withMethod: 
        Number
        'to:count:
        (lambda (self stop count)
          ($::: (smalltalkAt: 'Interval) 'from:to:count: self stop count)))

(addSelector:withMethod: 
        Number
        'to:do:
        (lambda (start stop aBlock)
          (let loop ( (index start) )
            (when (<= index stop)
              (aBlock index)
              (loop (+ index 1))))))

;; (st-eval "[|n| n := 0. 3 to: 7 do: [:a| n := n + a]. n] value")
;; --> 25

(addSelector:withMethod: 
        Number
        'to:by:do:
        (lambda (start stop step aBlock)
          (if (< step 0)
              (let loop ( (index start) )
                (when (>= index stop)
                  (aBlock index)
                  (loop (+ index step))))
              (let loop ( (index start) )
                (when (<= index stop)
                  (aBlock index)
                  (loop (+ index step))))))
)

;; (st-eval "[|n| n := 0. 3 to: 7 by: 2 do: [:a| n := n + a]. n] value")
;; (st-eval "[|n| n := 0. 7 to: 3 by: -2 do: [:a| n := n + a]. n] value")
;; --> 15

;;; Complex

(addSelector:withMethod:
     Complex
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Complex)
           (superPerform:with: self 'is: symbol))))


;; ##FIXME: Hyperbolics

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
          (format port "~a~a~ai"
                  (real-part self)
                  (if (negative? (imag-part self)) #\- #\+)
                  (imag-part self)))
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

;;; Float

($: Float 'pi: (* 2 (acos 0)))
($: Float 'e: (exp 1))

(addSelector:withMethod:
     Float
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Float)
           (superPerform:with: self 'is: symbol))))

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


;;; Fraction

(addSelector:withMethod:
     Fraction
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Fraction)
           (superPerform:with: self 'is: symbol))))

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
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Integer)
           (superPerform:with: self 'is: symbol))))


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

(addSelector:withMethod: 
 	Integer
        'asCharacter
        (lambda (self) (integer->char self))
        )

(addSelector:withMethod: 
 	Integer
        'atRandom
        ;"Answer a random integer from 1 to self.
        (lambda (self) (+ 1 (random self))))

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


'st-number

;;;			--- E O F ---			;;;
