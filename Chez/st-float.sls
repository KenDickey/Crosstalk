;;; FILE: "st-float.scm"
;;; IMPLEMENTS: Float
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(library (st-float)

  (export
   init-st-float

   Float
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
   (rnrs arithmetic flonums (6))
   (rnrs r5rs (6)) ; modulo
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-character)
   (st-magnitude)
   (st-number)
   (st-complex)
   )


(define Float ;; Scheme real number
  (newSubclassName:iVars:cVars:
   Complex
   'Float '() '(pi e))
)


;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-float)
  (unless (initialized?)
    (initialized? #t)

    (init-st-complex)

(rebase-mdict! Float st-float-behavior)

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

'st-float
) )

)

;;;			--- E O F ---			;;;
