;;; FILE: "parsing.sch"
;;; IMPLEMENTS: Smalltalk source code tokenization
;;; AUTHOR: Ken Dickey
;;; DATE: 10 May 2016

(define reserved-words '(nil true false self super))

(define reserved-selectors
  '(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
    == and: or:
    basicSize basicAt: basicAt:put: basicNew:
    to:do: to:by:do: timesRepeat:
   )
)

(define upperCaseAlphaChars "abcdefghijklmnopqrstuvwxyz")
(define lowerCaseAplhaChars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define decimalDigitChars "0123456789")
(define nonCaseChar #\_) ; underscpre
(define exponentLetters "edg")
(define binaryCharacters "!%&*+,/<=>?@\~|-")
(define commentCharacter #\")
(define returnOperatorChar #\^)
(define assignmentString ":=")  ;; NB: 'foo:=' parses as #(foo :=) NOT #(foo: =)


;;;			--- E O F ---			;;;
