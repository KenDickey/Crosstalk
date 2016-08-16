;;; IMPLEMENTS: Unit tests for st-parse.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 15 August 2016

;; (require 'st-parse)

(define (setup-st-parse)
  (debug-parser #false)
  (trace-parse-methods #false))

(define (cleanup-st-parse) #f)

(add-test-suite 'st-parse
                setup-st-parse
                cleanup-st-parse)

(add-equal-test 'st-parse
 '#(astSequence
    (#(astKeywordSend
       #(astLiteral
         #(token
           integer
           "3"
           #("3 perform: #between:and: with: 1 with: 5" 0 0))
         3)
       perform:with:with:
       (#(astLiteral
          #(token
            symbol
            "#between:and:"
            #("3 perform: #between:and: with: 1 with: 5"
              0
              11))
          between:and:)
        #(astLiteral
          #(token
            integer
            "1"
            #("3 perform: #between:and: with: 1 with: 5"
              0
              31))
          1)
        #(astLiteral
          #(token
            integer
            "5"
            #("3 perform: #between:and: with: 1 with: 5"
              0
              39))
          5)))))
  (begin
    (parse-test "3 perform: #between:and: with: 1 with: 5")
    (parse-st-code))
  "3 perform: #between:and: with: 1 with: 5")

(add-equal-test 'st-parse
   '#(astSequence
      (#(astKeywordSend
         #(astIdentifier
           #(token identifier "a" #("a max:b" 0 0))
           a)
         max:
         (#(astIdentifier
            #(token identifier "b" #("a max:b" 0 6))
            b)))))
   (begin
    (parse-test "a max:b")
    (parse-st-code))
   "a max:b")

(add-equal-test 'st-parse
  '#(astSequence
     (#(astKeywordSend
        #(astIdentifier
          #(token
            identifier
            "self"
            #("self perform: #add: with: anObject" 0 0))
          self)
        perform:with:
        (#(astLiteral
           #(token
             symbol
             "#add:"
             #("self perform: #add: with: anObject" 0 14))
           add:)
         #(astIdentifier
           #(token
             identifier
             "anObject"
             #("self perform: #add: with: anObject" 0 26))
           anObject)))))
   (begin
    (parse-test "self perform: #add: with: anObject")
    (parse-st-code))
   "self perform: #add: with: anObject")

;; (ensure-exception-raised 'st-*
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

