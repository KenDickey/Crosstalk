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


(add-equal-test 'st-parse
  '#(astSequence
     (
      #(astAssignment
        #(astIdentifier
          #(token identifier "a" #("a := 5. a + 23." 0 0))
          a)
        #(astLiteral
          #(token integer "5" #("a := 5. a + 23." 0 5))
          5))

      #(astBinarySend
        #(astIdentifier
          #(token identifier "a" #("a := 5. a + 23." 0 8))
          a)
        +
        #(astLiteral
          #(token integer "23" #("a := 5. a + 23." 0 12))
          23))
    ) )
   (begin
    (parse-test "a := 5. a + 23.")
    (parse-st-code))
   "a := 5. a + 23.")


(add-equal-test 'st-parse
  '#(astSequence
     (#(astAssignment
        #(astIdentifier
          #(token identifier "x" #("x := 5. ^x + 3." 0 0))
          x)
        #(astLiteral
          #(token integer "5" #("x := 5. ^x + 3." 0 5))
          5))
      #(astReturn
        #(astBinarySend
          #(astIdentifier
            #(token identifier "x" #("x := 5. ^x + 3." 0 9))
            x)
          +
          #(astLiteral
            #(token integer "3" #("x := 5. ^x + 3." 0 13))
            3)))))
  (begin
    (parse-test "x := 5. ^x + 3.")
    (parse-st-code))
  "x := 5. ^x + 3."
)

(add-equal-test 'st-parse
  '#(astSequence
     (#(astLiteral
        #(token
          byteArrayLiteral
          "#[...]"
          #(" #[2 3 45 6 75 234 7] " 0 1))
        #u8(2 3 45 6 75 234 7))))
  (begin
    (parse-test " #[2 3 45 6 75 234 7] ")
    (parse-st-code))
  " #[2 3 45 6 75 234 7] ")

(add-equal-test 'st-parse
  '#(astSequence
  (#(astArray
     (#(astLiteral
        #(token
          integer
          "1"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            4))
        1)
      #(astLiteral
        #(token
          characterLiteral
          "$c"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            6))
        #\c)
      #(astArray
        (#(astLiteral
           #(token
             string
             "'b'"
             #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
               0
               14))
           "b")
         #(astLiteral
           #(token
             integer
             "3"
             #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
               0
               18))
           3)))
      #(astLiteral
        #(token
          byteArrayLiteral
          "#[...]"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            21))
        #u8(1 22 33))
      #(astLiteral
        #(token
          string
          "'five'"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            33))
        "five")
      #(astLiteral
        #(token
          integer
          "7"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            40))
        7)
      #(astLiteral
        #(token
          symbol
          "#foo"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            42))
        foo)))))
  (begin
    (parse-test
     " #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) ")
    (parse-st-code))
  " #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) ")


(add-equal-test 'st-parse
  '#(astSequence
  (#(astAssignment
     #(astIdentifier
       #(token
         identifier
         "block"
         #("block := [:a :b| ^(a foo: b bar: c + 7)]."
           0
           0))
       block)
     #(astBlock
       (#(astIdentifier
          #(token
            blockArg
            ":a"
            #("block := [:a :b| ^(a foo: b bar: c + 7)]."
              0
              10))
          a)
        #(astIdentifier
          #(token
            blockArg
            ":b"
            #("block := [:a :b| ^(a foo: b bar: c + 7)]."
              0
              13))
          b))
       ()
       (#(astReturn
          #(astSubexpression
            #(astKeywordSend
              #(astIdentifier
                #(token
                  identifier
                  "a"
                  #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                    0
                    19))
                a)
              foo:bar:
              (#(astIdentifier
                 #(token
                   identifier
                   "b"
                   #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                     0
                     26))
                 b)
               #(astBinarySend
                 #(astIdentifier
                   #(token
                     identifier
                     "c"
                     #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                       0
                       33))
                   c)
                 +
                 #(astLiteral
                   #(token
                     integer
                     "7"
                     #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                       0
                       37))
                   7)))))))
       #t))))
  (begin
    (parse-test
     "block := [:a :b| ^(a foo: b bar: c + 7)].")
    (parse-st-code))
  "block := [:a :b| ^(a foo: b bar: c + 7)].")

(add-equal-test 'st-parse
   '#(astSequence
  (#(astBinarySend
     #(astSubexpression
       #(astBinarySend
         #(astIdentifier
           #(token identifier "a" #("(a + b) + 3." 0 1))
           a)
         +
         #(astIdentifier
           #(token identifier "b" #("(a + b) + 3." 0 5))
           b)))
     +
     #(astLiteral
       #(token integer "3" #("(a + b) + 3." 0 10))
       3))))
  (begin
    (parse-test
     "(a + b) + 3.")
    (parse-st-code))
  "(a + b) + 3.")


(add-equal-test 'st-parse
   '#()
  (begin
    (parse-test
     "([:a| [:b| a + b]] value: 2) value: 3.")
    (parse-st-code))
  "([:a| [:b| a + b]] value: 2) value: 3.")

(add-equal-test 'st-parse
   '#()
  (begin
    (parse-test
     "[|a| a := 3. a+a] value.")
    (parse-st-code))
  "[|a| a := 3. a+a] value.")


;; (ensure-exception-raised 'st-*
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

