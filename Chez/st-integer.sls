;;; FILE: "st-integer.scm"
;;; IMPLEMENTS: Integer
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(library (st-integer)

  (export
   Integer
   )

  (import
   (rnrs base)
   (rnrs control (6))
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs mutable-strings (6))
   (rnrs unicode (6))
   (rnrs hashtables (6)) ; equal-hash
   (rnrs arithmetic bitwise (6))
   (rnrs arithmetic fixnums (6))
   (rnrs r5rs (6)) ; modulo
   (only (chezscheme)
         random ;; (random N) -> number from 0 to N-1
         )
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-character)
   (st-magnitude)
   (st-number)
   (st-complex)
   (st-float)
   (st-fraction)
   )

(define Integer
  (newSubclassName:iVars:cVars:
   Fraction
   'Integer '() '())
)

;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(perform:with: Integer 'methodDict: st-integer-behavior)


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

)

;;;			--- E O F ---			;;;
