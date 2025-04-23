;;; FILE: "st-character-tests.scm"
;;; IMPLEMENTS: Unit tests for st-character.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(define (setup-st-character)   #f)
(define (cleanup-st-character) #f)

(add-test-suite 'st-character
                setup-st-character
                cleanup-st-character)

(add-equal-test 'st-character
  #\a
  (perform: #\A 'asLowercase)
  "$A asLowercase")

(add-equal-test 'st-character
  #\A
  (perform: #\a 'asUppercase)
  "$a asUppercase")

(add-equal-test 'st-character
  "a"
  (perform: #\a 'asString)
  "$a asString")

(add-equal-test 'st-character
  'a
  (perform: #\a 'asSymbol)
  "$a asSymbol")

(add-equal-test 'st-character
  97
  (perform: #\a 'value)
  "$a value")

(add-equal-test 'st-character
  #\a
  (perform:with: Character 'value: 97)
  "Character value: 97")

(add-equal-test 'st-character
  st-true
  (perform: #\a 'isLetter)
  "$a isLetter")

(add-equal-test 'st-character
  st-false
  (perform: #\8 'isLetter)
  "$8 isLetter")

(add-equal-test 'st-character
  st-false
  (perform: #\a 'isDigit)
  "$a isNumber")

(add-equal-test 'st-character
  st-true
  (perform: #\8 'isDigit)
  "$8 isNumber")

(add-equal-test 'st-character
  st-false
  (perform: #\a 'isSeparator)
  "$a isSeparator")

(add-equal-test 'st-character
  st-false
  (perform: #\< 'isSeparator)
  "$< isSeparator")

(add-equal-test 'st-character
  st-true
  (perform: #\  'isSeparator)
  "$  isSeparator")

(add-equal-test 'st-character
  st-true
  (perform: #\a 'isAlphaNumeric)
  "$a isAlphaNumeric")

(add-equal-test 'st-character
  st-true
  (perform: #\9 'isAlphaNumeric)
  "$9 isAlphaNumeric")

(add-equal-test 'st-character
  st-false
  (perform: #\< 'isAlphaNumeric)
  "$< isAlphaNumeric")

(add-equal-test 'st-character
  st-false
  (perform: #\a 'isUppercase)
  "$a isUppercase")

(add-equal-test 'st-character
  st-true
  (perform: #\a 'isLowercase)
  "$a isLowercase")

(add-equal-test 'st-character
  st-true
  (perform: #\A 'isUppercase)
  "$A isUppercase")

(add-equal-test 'st-character
  st-false
  (perform: #\A 'isLowercase)
  "$A isLowercase")

(add-equal-test 'st-character
  st-false
  (perform: #\a 'isSpecial)
  "$a isSpecial")

(add-equal-test 'st-character
  "$c"
  (perform: #\c 'printString)
  "$c printString")

(add-equal-test 'st-character
  st-true
  (perform: #\< 'isSpecial)
  "$< isSpecial")

;; (ensure-exception-raised 'st-character
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")



;; Need many more!

;;;			--- E O F ---			;;;
