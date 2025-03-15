;;; FILE: "st-arith-err-subs.sls"
;;; IMPLEMENTS: ZeroDivide
;;; See also "st-exception.sls", "st-error.sls" for superclasses.
;;; See also "st-blockClosure.scm" for exception handling code.
;;; See "st-conditions.scm" for #asException
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017; March 2025


(library (st-arith-err-subs)

  (export
   ZeroDivide
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs io simple (6))
   (rnrs hashtables (6))
   (rnrs exceptions (6))
   (rnrs conditions (6))
   (rnrs arithmetic bitwise (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-boolean) ;; UndefinedObject
   (st-number)
   (st-blockClosure)
   (st-exception)
   (st-error)
   (st-error-subs)
   )

(define ZeroDivide
  (newSubclassName:iVars:cVars:
   ArithmeticError
   'ZeroDivide '(dividend) '())
)

;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

(perform:with: ZeroDivide
               'methodDict:
               (clone-method-dictionary
                ($ ArithmeticError 'methodDict)))

(addSelector:withMethod:
     (class ZeroDivide)
     'dividend:
     (lambda (self dvdnd)
       (let ( (ex ($ ZeroDivide 'new)) )
         ($: ex 'receiver: dvdnd)
         ($: ex 'dividend: dvdnd)
         ($: ex 'messageText: (format #f "ZeroDivide: ~a / 0" dvdnd))
         ex)))

(addSelector:withMethod:
     (class ZeroDivide)
     'signalWithDividend:
     (lambda (self dvdnd)
       ($ ($: self 'dividend: dvdnd) 'signal)))

(addSelector:withMethod:  ;; REDEFINE
        Number
        (string->symbol "/")
        (lambda (self aNumber)
          (when (zero? aNumber)
              ($: ZeroDivide 'signalWithDividend: self))
          (/ self aNumber)))

)

;;;			--- E O F ---			;;;
