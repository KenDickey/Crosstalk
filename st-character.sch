;;; FILE: "st-character.sch"
;;; IMPLEMENTS: Characters
;;; AUTHOR: Ken Dickey
;;; DATE: 12 June 2016

;; (require 'st-core-classes)

(import (scheme char))

(define Character
  (newSubclassName:iVars:cVars:
   Object
   'Character '() '())
)

(set! st-character-behavior  (perform: Character 'methodDict))

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
          (char-alphbetic? self)))

(addSelector:withMethod:
        Character
        'isDigit
        (lambda (self)
          (char-numeric? self)))

(addSelector:withMethod:
        Character
        'isSeparator
        (lambda (self)
          (char-whitespace? self)))

(addSelector:withMethod:
        Character
        'isAlphaNumeric
        (lambda (self)
          (or (char-alphbetic? self)
              (char-numeric? self))))

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

(define st-special-chars
  (string->list "+-/\*~<>=@,%|&?!"))

(addSelector:withMethod:
        Character
        'isSpecial
        (lambda (self)
          ;; remember to return a boolean..
          (if (member self st-special-chars) st-true st-false)))

;;; Named characters

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
    (newLineCharacter . 10) ; NB: lf NOT cr
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
          (lambda (self) (error "cannot create new characters" self)))

(addSelector:withMethod:
          (class Character)
          'new:
          (lambda (self value) (error "cannot create new characters" self value)))


;; (provide 'st-character)

;;;			--- E O F ---			;;;
