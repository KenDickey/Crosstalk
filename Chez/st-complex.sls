;;; FILE: "st-number.scm"
;;; IMPLEMENTS: Complex
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(library (st-complex)

  (export
   init-st-complex
   
   Complex
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
   (st-number)
   )

(define Complex
  (newSubclassName:iVars:cVars:
   Number
   'Complex '() '())
)

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-complex)
  (unless (initialized?)
    (initialized? #t)

    (init-st-number)
  
(rebase-mdict! Complex st-complex-behavior)

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
          (format port "(~a ~a~ai)"
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

'st-complex
) )

)

;;;			--- E O F ---			;;;
