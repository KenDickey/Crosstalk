;;; IMPLEMENTS: Unit tests for st-parse.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 15 August 2016

;; (require 'st-parse)

(define (setup-st-parse)
  (debug-parser #f)
  (trace-parse-methods #f))

(define (cleanup-st-parse) #f)

(add-test-suite 'st-parse
                setup-st-parse
                cleanup-st-parse)

(add-equal-test 'st-parse
  '#(astMessageSend
  #(astLiteral
    #(token
      integer
      "3"
      #("3 perform: #between:and: with: 1 with: 5" 0 0))
    3)
  #(astKeywordMessage
    perform:with:with:
    (#(astLiteral
       #(token
         symbol
         "#between:and:"
         #("3 perform: #between:and: with: 1 with: 5"
           0
           11))
       'between:and:)
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
       5))))
  (begin
    (parse-test "3 perform: #between:and: with: 1 with: 5")
    (parse-st-code))
  "3 perform: #between:and: with: 1 with: 5")

(add-equal-test 'st-parse
   '#(astMessageSend
  #(astIdentifier
    #(token identifier "a" #("a max:b" 0 0))
    a)
  #(astKeywordMessage
    max:
    (#(astIdentifier
       #(token identifier "b" #("a max:b" 0 6))
       b))))
   (begin
    (parse-test "a max:b")
    (parse-st-code))
   "a max:b")

(add-equal-test 'st-parse
  '#(astMessageSend
  #(astIdentifier
    #(token
      identifier
      "self"
      #("self perform: #add: with: anObject" 0 0))
    self)
  #(astKeywordMessage
    perform:with:
    (#(astLiteral
       #(token
         symbol
         "#add:"
         #("self perform: #add: with: anObject" 0 14))
       'add:)
     #(astIdentifier
       #(token
         identifier
         "anObject"
         #("self perform: #add: with: anObject" 0 26))
       anObject))))
   (begin
    (parse-test "self perform: #add: with: anObject")
    (parse-st-code))
   "self perform: #add: with: anObject")


(add-equal-test 'st-parse
  '#(astSequence
  (#(astAssignment
     #(astIdentifier
       #(token identifier "a" #("a := 5. a + 23." 0 0))
       a)
     #(astLiteral
       #(token integer "5" #("a := 5. a + 23." 0 5))
       5))
   #(astMessageSend
     #(astIdentifier
       #(token identifier "a" #("a := 5. a + 23." 0 8))
       a)
     #(astBinaryMessage
       +
       #(astLiteral
         #(token integer "23" #("a := 5. a + 23." 0 12))
         23)))))
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
     #(astMessageSend
       #(astIdentifier
         #(token identifier "x" #("x := 5. ^x + 3." 0 9))
         x)
       #(astBinaryMessage
         +
         #(astLiteral
           #(token integer "3" #("x := 5. ^x + 3." 0 13))
           3))))))
  (begin
    (parse-test "x := 5. ^x + 3.")
    (parse-st-code))
  "x := 5. ^x + 3."
)

(add-equal-test 'st-parse
  '#(astLiteral
  #(token
    byteArrayLiteral
    "#[...]"
    #(" #[2 3 45 6 75 234 7] " 0 1))
  #u8(2 3 45 6 75 234 7))
  (begin
    (parse-test " #[2 3 45 6 75 234 7] ")
    (parse-st-code))
  " #[2 3 45 6 75 234 7] ")

(add-equal-test 'st-parse
  '#(astArray
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
          characterLiteral
          "$a"
          #(" #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) "
            0
            11))
        #\a)
      #(astLiteral
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
     'foo)))
  (begin
    (parse-test
     " #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) ")
    (parse-st-code))
  " #( 1 $c #($a 'b' 3) #[01 22 33] 'five' 7 #foo ) ")


(add-equal-test 'st-parse
  '#(astAssignment
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
         #(astMessageSend
           #(astIdentifier
             #(token
               identifier
               "a"
               #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                 0
                 19))
             a)
           #(astKeywordMessage
             foo:bar:
             (#(astIdentifier
                #(token
                  identifier
                  "b"
                  #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                    0
                    26))
                b)
              #(astMessageSend
                #(astIdentifier
                  #(token
                    identifier
                    "c"
                    #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                      0
                      33))
                  c)
                #(astBinaryMessage
                  +
                  #(astLiteral
                    #(token
                      integer
                      "7"
                      #("block := [:a :b| ^(a foo: b bar: c + 7)]."
                        0
                        37))
                    7)))))))))
    #t))
  (begin
    (parse-test
     "block := [:a :b| ^(a foo: b bar: c + 7)].")
    (parse-st-code))
  "block := [:a :b| ^(a foo: b bar: c + 7)].")

(add-equal-test 'st-parse
  '#(astMessageSend
  #(astSubexpression
    #(astMessageSend
      #(astIdentifier
        #(token identifier "a" #("(a + b) + 3." 0 1))
        a)
      #(astBinaryMessage
        +
        #(astIdentifier
          #(token identifier "b" #("(a + b) + 3." 0 5))
          b))))
  #(astBinaryMessage
    +
    #(astLiteral
      #(token integer "3" #("(a + b) + 3." 0 10))
      3)))
  (begin
    (parse-test
     "(a + b) + 3.")
    (parse-st-code))
  "(a + b) + 3.")


(add-equal-test 'st-parse
   '#(astMessageSend
     #(astSubexpression
       #(astMessageSend
         #(astBlock
           (#(astIdentifier
              #(token
                blockArg
                ":a"
                #("([:a| [:b| a + b]] value: 2) value: 3." 0 2))
              a))
           ()
           (#(astBlock
              (#(astIdentifier
                 #(token
                   blockArg
                   ":b"
                   #("([:a| [:b| a + b]] value: 2) value: 3." 0 7))
                 b))
              ()
              (#(astMessageSend
                 #(astIdentifier
                   #(token
                     identifier
                     "a"
                     #("([:a| [:b| a + b]] value: 2) value: 3." 0 11))
                   a)
                 #(astBinaryMessage
                   +
                   #(astIdentifier
                     #(token
                       identifier
                       "b"
                       #("([:a| [:b| a + b]] value: 2) value: 3." 0 15))
                     b))))
              #f))
           #f)
         #(astKeywordMessage
           value:
           (#(astLiteral
              #(token
                integer
                "2"
                #("([:a| [:b| a + b]] value: 2) value: 3." 0 26))
              2)))))
     #(astKeywordMessage
       value:
       (#(astLiteral
          #(token
            integer
            "3"
            #("([:a| [:b| a + b]] value: 2) value: 3." 0 36))
          3))))
  (begin
    (parse-test
     "([:a| [:b| a + b]] value: 2) value: 3.")
    (parse-st-code))
  "([:a| [:b| a + b]] value: 2) value: 3.")

(add-equal-test 'st-parse
   '#(astMessageSend
  #(astBlock
    ()
    (#(token
       identifier
       "a"
       #("[|a| a := 3. a+a] value." 0 2)))
    (#(astAssignment
       #(astIdentifier
         #(token
           identifier
           "a"
           #("[|a| a := 3. a+a] value." 0 5))
         a)
       #(astLiteral
         #(token
           integer
           "3"
           #("[|a| a := 3. a+a] value." 0 10))
         3))
     #(astMessageSend
       #(astIdentifier
         #(token
           identifier
           "a"
           #("[|a| a := 3. a+a] value." 0 13))
         a)
       #(astBinaryMessage
         +
         #(astIdentifier
           #(token
             identifier
             "a"
             #("[|a| a := 3. a+a] value." 0 15))
           a))))
    #f)
  #(astUnaryMessage
    #(token
      identifier
      "value"
      #("[|a| a := 3. a+a] value." 0 18))))
  (begin
    (parse-test
     "[|a| a := 3. a+a] value.")
    (parse-st-code))
  "[|a| a := 3. a+a] value.")


(add-equal-test 'st-parse
  '#(astMessageSend
  #(astIdentifier
    #(token
      identifier
      "self"
      #("self do: [:i | i do: [ :each | ] ] separatedBy: [] "
        0
        0))
    self)
  #(astKeywordMessage
    do:separatedBy:
    (#(astBlock
       (#(astIdentifier
          #(token
            blockArg
            ":i"
            #("self do: [:i | i do: [ :each | ] ] separatedBy: [] "
              0
              10))
          i))
       ()
       (#(astMessageSend
          #(astIdentifier
            #(token
              identifier
              "i"
              #("self do: [:i | i do: [ :each | ] ] separatedBy: [] "
                0
                15))
            i)
          #(astKeywordMessage
            do:
            (#(astBlock
               (#(astIdentifier
                  #(token
                    blockArg
                    ":each"
                    #("self do: [:i | i do: [ :each | ] ] separatedBy: [] "
                      0
                      23))
                  each))
               ()
               ()
               #f)))))
       #f)
     #(astBlock () () () #f))))
  (begin
    (parse-test
     "self do: [:i | i do: [ :each | ] ] separatedBy: [] ")
    (parse-st-code))
  "self do: [:i | i do: [ :each | ] ] separatedBy: [] ")

(add-equal-test 'st-parse
  '#(astBlock
  (#(astIdentifier
     #(token
       blockArg
       ":a"
       #("[:a||b| b := a. a+b] " 0 1))
     a))
  (#(token
     identifier
     "b"
     #("[:a||b| b := a. a+b] " 0 5)))
  (#(astAssignment
     #(astIdentifier
       #(token
         identifier
         "b"
         #("[:a||b| b := a. a+b] " 0 8))
       b)
     #(astIdentifier
       #(token
         identifier
         "a"
         #("[:a||b| b := a. a+b] " 0 13))
       a))
   #(astMessageSend
     #(astIdentifier
       #(token
         identifier
         "a"
         #("[:a||b| b := a. a+b] " 0 16))
       a)
     #(astBinaryMessage
       +
       #(astIdentifier
         #(token
           identifier
           "b"
           #("[:a||b| b := a. a+b] " 0 18))
         b))))
  #f)
  (begin
    (parse-test "[:a||b| b := a. a+b] ")
    (parse-st-code))
  "[:a||b| b := a. a+b] ")

(add-equal-test 'st-parse
 '#(astMessageSend
  #(astIdentifier
    #(token
      identifier
      "String"
      #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
        0
        0))
    String)
  #(astKeywordMessage
    addSelector:withMethod:
    (#(astLiteral
       #(token
         symbol
         "#contains:"
         #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
           0
           20))
       'contains:)
     #(astBlock
       (#(astIdentifier
          #(token
            blockArg
            ":self"
            #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
              1
              20))
          self)
        #(astIdentifier
          #(token
            blockArg
            ":aChar"
            #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
              1
              26))
          aChar))
       ()
       (#(astMessageSend
          #(astIdentifier
            #(token
              identifier
              "self"
              #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
                2
                20))
            self)
          #(astKeywordMessage
            detect:
            (#(astBlock
               (#(astIdentifier
                  #(token
                    blockArg
                    ":c"
                    #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
                      2
                      35))
                  c))
               ()
               (#(astMessageSend
                  #(astIdentifier
                    #(token
                      identifier
                      "c"
                      #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
                        2
                        40))
                    c)
                  #(astBinaryMessage
                    =
                    #(astIdentifier
                      #(token
                        identifier
                        "aChar"
                        #("String addSelector: #contains:\n\t     withMethod: [ :self :aChar |\n\t                   self detect: [ :c | c = aChar] ]."
                          2
                          44))
                      aChar))))
               #f)))))
       #f))))
   (begin
    (parse-test
     "String addSelector: #contains:
	     withMethod: [ :self :aChar |
	                   self detect: [ :c | c = aChar] ].")
    (parse-st-code))
   "String addSelector: #contains:
	     withMethod: [ :self :aChar |
	                   self detect: [ :c | c = aChar] ].")


;; (ensure-exception-raised 'st-*
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

