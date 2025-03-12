;;; FILE: "st-character.sls"
;;; IMPLEMENTS: Character
;;; AUTHOR: Ken Dickey
;;; DATE: 12 June 2016



(library (st-character)

  (export
   Character
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs io simple (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   )

(define Character
  (newSubclassName:iVars:cVars:
   Object
   'Character '() '())
)

(define st-special-chars ;; "\\" = (string #\\)
  (string->list "+-/\\*~<>=@,%|&?!"))

(define named-characters
  '((aleph      . #x8D) ; math
    (arrowUp    .   30)
    (arrowLeft  .   28)
    (arrowRight .   29)
    (arrowDown  .   31)
    (backspace  .    8)
    (bullit     . #x9F)
    (circle     . #x9E) ; math
    (contourIntegral . #x93) ; math
    (cr         .   13)
    (delete     .  127)
    (doesNotExist . #x83) ; math
    (dot        . #xB7) ; math
    (end        .    4)
    (emptySet   . #x84) ; math
    (enter      .    3)
    (escape     .   27)
    (euro       .  164)
    (exists     . #x82) ; math
    (forAll     . #x80) ; math
    (greaterNotEqual  . #x9D) ; math
    (greaterOrEqual   . #x99) ; math
    (greaterOverEqual . #x9B) ; math
    (home       .    1)
    (identical  . #x95) ; math
    (infinity   . #x85) ; math; a.k.a. offscale
    (insert     .    5)
    (integral   . #x92) ; math
    (lessNotEqual  . #x9C) ; math
    (lessOrEqual   . #x98) ; math
    (lessOverEqual . #x9A) ; math
    (lf         .   10) ; line feed
    (nbsp       .  202) ; non-breakable space
    (newlineCharacter . 10) ; NB: lf NOT cr
    (newline . 10)    ; NB: lf NOT cr
    (nl . 10)         ; NB: lf NOT cr
    (newPage    .   12) ; form feed
    (notEqual     . #x94) ; math
    (notIdentical . #x96) ; math
    (null       .    0)
    (odot       . #x8E) ; math
    (oplus      . #x8F) ; math
    (otimes     . #x90) ; math
    (partial    . #x81) ; math
    (pageDown   .   12)
    (pageUp     .   11)
    (space      .   32)
    (strictlyEquivalent . #x97) ; math
    (summation          . #x91) ; math
    (tab        .    9)
    (times      . #xD7) ; math
   )
)


;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(addSelector:withMethod:
     Character
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Character)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     Character
     'category: "Kernel-Text")

(perform:with:
     Character
     'comment:
     "I support Unicode, rather than ASII characters.")

(addSelector:withMethod:
        Character
        'printOn:
        (lambda (self port)
          (display "$" port)
          (display self port)))


(addSelector:withMethod:
        Character
        '=
        (lambda (self otherChar)
          (char=? self otherChar)))

(addSelector:withMethod:
        Character
        '<
        (lambda (self otherChar)
          (char<? self otherChar)))

(addSelector:withMethod:
        Character
        '>
        (lambda (self otherChar)
          (char>? self otherChar)))

(addSelector:withMethod:
        Character
        '>=
        (lambda (self otherChar)
          (char>=? self otherChar)))

(addSelector:withMethod:
        Character
        '<=
        (lambda (self otherChar)
          (char<=? self otherChar)))


(addSelector:withMethod:
        Character
        'asLowercase
        (lambda (self)
          (char-downcase self)))

(addSelector:withMethod:
        Character
        ;; NB: Some Unicode chars have no upcase equivalent
        'asUppercase  
        (lambda (self)
          (char-upcase self)))

(addSelector:withMethod:
        Character
        'asString
        (lambda (self)
          (string self)))

(addSelector:withMethod:
        Character
        'asSymbol
        (lambda (self)
          (string->symbol (string self))))

(addSelector:withMethod:
        Character
        'asCharacter
        (lambda (self) self))

(addSelector:withMethod:
        Character
        'asInteger
        (lambda (self) (char->integer self)))

(addSelector:withMethod:
        Character
        'value   ;; NOT an instance variable !!
        (lambda (self)
          (char->integer self)))

(addSelector:withMethod:
        (class Character)
        'value:
        (lambda (self anInteger)
          (integer->char anInteger)))

(addSelector:withMethod:
        Character
        'isLetter
        (lambda (self)
          (char-alphabetic? self)))

(addSelector:withMethod:
        Character
        'isDigit
        (lambda (self)
          (char-numeric? self)))

(addSelector:withMethod:
        (class Character)
        'digitValue:
;"Answer the Character whose digit value is x.
; For example, answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."
        (lambda (self aChar)
          (let ( (val
                  ($::: "0123456789abcdefghijklmnopqrstuvwxyz"
                        'indexOf:startingAt:ifAbsent:
                        (char-downcase aChar)
                        1
                        (lambda () #f)))
               )
            (if val (- val 1) ; zero is index 1
                -1)))
)

(addSelector:withMethod:
        Character
        'digitValue
;"Answer the Character whose digit value is x.
; For example, answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."
        (lambda (self)
          (let ( (val
                  ($::: "0123456789abcdefghijklmnopqrstuvwxyz"
                        'indexOf:startingAt:ifAbsent:
                        (char-downcase self)
                        1
                        (lambda () #f)))
               )
            (if val (- val 1) ; zero is index 1
                -1)))
)

(addSelector:withMethod:
        Character
        'isSeparator
        (lambda (self)
          (char-whitespace? self)))

(addSelector:withMethod:
        Character
        'isAlphaNumeric
        (lambda (self)
          (or (char-alphabetic? self)
              (char-numeric? self))))

(addSelector:withMethod:
        Character
        'isValidInIdentifiers
        (lambda (self)
          (or (char-alphabetic? self)
              (char-numeric? self))))

(addSelector:withMethod:
        Character
        'tokenish ;; 'isTokenish ??
        (lambda (self)
          (or (char-alphabetic? self)
              (char-numeric? self)
              (char=? self #\:))))

(addSelector:withMethod:
        Character
        'isUppercase
        (lambda (self)
          (char-upper-case? self)))

(addSelector:withMethod:
        Character
        'isLowercase
        (lambda (self)
          (char-lower-case? self)))

(addSelector:withMethod:
        Character
        'isSpecial
        (lambda (self)
          ;; remember to return a boolean..
          (if (member self st-special-chars) st-true st-false)))

;;; Named characters

(for-each
   (lambda (pair)
     (let ( (char (integer->char (cdr pair))) )
       (addSelector:withMethod:
           (class Character)
           (car pair)
           (lambda (self) char))))
   named-characters)


(addSelector:withMethod:
          (class Character)
          'new
          (lambda (self)
            (error 'new "cannot create new characters" self)))

(addSelector:withMethod:
          (class Character)
          'new:
          (lambda (self value)
            (error 'new: "cannot create new characters" self value)))

(addSelector:withMethod:
          (class Character)
          'value:
          (lambda (self value)
            (unless (integer? value)
              (error 'value: "Character value: anInteger" value))
            (integer->char value)))

)

;;;			--- E O F ---			;;;
