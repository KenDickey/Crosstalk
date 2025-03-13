;;; FILE: "st-string-tests.scm"
;;; IMPLEMENTS: Unit tests for st-string.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-string)


(define (setup-st-string)   #f)
(define (cleanup-st-string) #f)

(add-test-suite 'st-string
                setup-st-string
                cleanup-st-string)

(add-equal-test 'st-string
  "this and that"
  (perform:with: "this " (string->symbol ",") "and that")
  "'this ' , ' and that'")

(add-equal-test 'st-string
  "this and that"
  (perform:with: String
                 'streamContents:
                 (lambda (port)
                   (display "this " port)
                   (display "and " port)
                   (display "that" port)))
  "String streamContents: oneArgBlock")

(add-equal-test 'st-string
  "howDoYouDo?"
  (perform: " how do you do? " 'asCamalCase)
  "' how do you do? ' asCamalCase")

(add-equal-test 'st-string
  "a"
  (perform:with: String 'value: 97)
  "String value: 97")

(add-equal-test 'st-string
  "a"
  (perform:with: String 'with: #\a)
  "String with: $a")

(add-equal-test 'st-string
  "ab"
  (perform:with:with: String 'with:with: #\a #\b)
  "String with: $a with: $b")

(add-equal-test 'st-string
  "abc"
  (perform:with:with:with: String 'with:with:with: #\a #\b #\c)
  "String with: $a with: $b with: $c")

(add-equal-test 'st-string
  "abc"
  (perform:with: String 'withAll: "abc")
  "String withAll: 'abc'")

(add-equal-test 'st-string
  #\b
  (perform:with: "abc" 'at: 2)
  "'abc' at: 2")

(add-equal-test 'st-string
  "a2c"
  (perform:with:with: "abc" 'at:put: 2 #\2)
  "'abc' at: 2 put: $2")

(add-equal-test 'st-string
  "abc"
  (perform: "AbC" 'asLowercase)
  "'AbC' asLowercase")

(add-equal-test 'st-string
  "ABCD"
  (perform: "aBcD" 'asUppercase)
  "'aBcD' asUppercase")

(add-equal-test 'st-string
  (string->symbol "this thing")
  (perform: "this thing" 'asSymbol)
  "'this thing' asSymbol")

;; (ensure-exception-raised 'st-string
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;; NYI

;;;			--- E O F ---			;;;
