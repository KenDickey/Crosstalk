;;; FILE: "st-fraction.scm"
;;; IMPLEMENTS: Fraction
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(library (st-fraction)

  (export
   Fraction
   )

  (import
   (rnrs base)
   (rnrs control (6))
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
   (st-number)
   (st-complex)
   (st-float)
   )


(define Fraction  ;; Scheme rational number
  (newSubclassName:iVars:cVars:
   Float
   'Fraction '() '())
)

;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(perform:with: Fraction 'methodDict: st-fraction-behavior)


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

)
;;;			--- E O F ---			;;;
