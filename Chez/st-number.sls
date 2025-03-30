;;; FILE: "st-number.sls"
;;; IMPLEMENTS: Number abstract class
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(library (st-number)

  (export
   init-st-number

   Number

   pi e
   degreesToRadians
   radiansToDegrees
   printStringRadix:
   printOn:base:showRadix:
   )

  (import
   (rnrs base)
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs mutable-strings (6))
   (rnrs unicode (6))
   (rnrs hashtables (6)) ; equal-hash
   (rnrs r5rs (6)) ; modulo
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-character)
   (st-magnitude)
   )


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

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-number)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-magnitude)
  
(rebase-mdict! Number st-number-behavior)

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

(addSelector:withMethod: 
        Number
        (string->symbol "=")
        (lambda (self aNumber)
          (= self aNumber)))

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
        (string->symbol "\\")
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


'st-number
) )

)

;;;			--- E O F ---			;;;
