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

;; (ensure-exception-raised 'st-*
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

