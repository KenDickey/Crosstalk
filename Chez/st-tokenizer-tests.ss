;;; IMPLEMENTS: Unit tests for st-tokenizer.sls
;;; AUTHOR: Ken Dickey
;;; DATE: 29 June 2016; March 2025

(define get-token1 #f)
(define get-token2 #f)
(define get-token3 #f)
(define (tokens-from get-tok)
  (let loop ( (result '())
              (tok (get-tok))
            )
    (if (eq? 'eof (token-kind tok))
        (list->vector (reverse result))
        (loop (cons
               (cons (token-kind tok)
                     (token-string tok))
               result)
              (get-tok)))
) )
                          

(define (setup-st-tokenizer)
  (set! get-token1
        (tokenizer-for-string
          "#'x y' #'pHunNy x' 'Joe''s string' $C"))
  (set! get-token2
        (tokenizer-for-string
         "23 12.3 12.3e4 16rBEEF 10s 12s12 1/2"))
  (set! get-token3
        (tokenizer-for-string
         "block := [:a :b| ^(a foo: b bar: c + 7)]."))
  )

(define (cleanup-st-tokenizer)
  (set! get-token1 #f)
  (set! get-token2 #f)
  (set! get-token3 #f)

  )

(add-test-suite 'st-tokenizer
                setup-st-tokenizer
                cleanup-st-tokenizer)

(add-equal-test 'st-tokenizer
  (list->vector
   `(
     (symbol . "#'x y'")
     (whitespace . " ")
     (symbol . "#'pHunNy x'")
     (whitespace . " ")
     (string . "'Joe''s string'")
     (whitespace . " ")
     (characterLiteral . "$C")))
  (tokens-from get-token1)
  "#'x y' #'pHunNy x' 'aString' $C")

(add-equal-test 'st-tokenizer
  (list->vector
   '((integer . "23")
     (whitespace . " ")
     (float . "12.3")
     (whitespace . " ")
     (floatWithExponent . "12.3e4")
     (whitespace . " ")
     (integerWithRadix . "16rBEEF")
     (whitespace . " ")
     (scaledDecimalWithFract . "10s")
     (whitespace . " ")
     (scaledDecimalWithFract . "12s12")
     (whitespace . " ")
     (integer . "1")
     (binarySelector . "/")
     (integer . "2")))
  (tokens-from get-token2)
  "23 12.3 12.3e4 16rBEEF 10s 12s12 1/2")

(add-equal-test 'st-tokenizer
   (list->vector
    '((identifier . "block")
      (whitespace . " ")
      (assignment . ":=")
      (whitespace . " ")
      (blockStart . "[")
      (blockArg . ":a")
      (whitespace . " ")
      (blockArg . ":b")
      (verticalBar . "|")
      (whitespace . " ")
      (carrot . "^")
      (leftParen . "(")
      (identifier . "a")
      (whitespace . " ")
      (keyword . "foo:")
      (whitespace . " ")
      (identifier . "b")
      (whitespace . " ")
      (keyword . "bar:")
      (whitespace . " ")
      (identifier . "c")
      (whitespace . " ")
      (binarySelector . "+")
      (whitespace . " ")
      (integer . "7")
      (rightParen . ")")
      (blockEnd . "]")
      (period . ".")))
   (tokens-from get-token3)
   "block := [:a :b| ^(a foo: b bar: c + 7)].")

(add-equal-test 'st-tokenizer
  (list->vector
   '((identifier . "Class")
     (whitespace . " ")
     (methDef . "~>")
     (whitespace . " ")
     (identifier . "foo")
     (whitespace . " ")
     (blockStart . "[")
     (whitespace . " ")
     (identifier . "x")
     (whitespace . " ")
     (assignment . ":=")
     (whitespace . " ")
     (integer . "3")
     (period . ".")
     (whitespace . " ")
     (identifier . "x")
     (whitespace . " ")
     (blockEnd . "]")
     (period . ".")))
  (tokens-from (tokenizer-for-string "Class ~> foo [ x := 3. x ]."))
  "Class ~> foo [ x := 3. x ].")


(add-equal-test 'st-tokenizer
  "Joe''s string"
  (token->native ((tokenizer-for-string "'Joe''s string'")))
  "Joe''s string")

(add-equal-test 'st-tokenizer
  123
  (token->native ((tokenizer-for-string "123")))
  "123")

(add-equal-test 'st-tokenizer
  12.3
  (token->native ((tokenizer-for-string "12.3")))
  "12.3")

(add-equal-test 'st-tokenizer
   (list->vector
    `((leftParen . "(")
      (litArrayStart . "#(")
      (blockStart . "[")
      (litByteArrayStart . "#[")
      (blockEnd . "]")
      (blockEnd . "]")
      (dynArrayStart . "{")
      (dynArrayEnd . "}")))
  (tokens-from (tokenizer-for-string "(#([#[]]{}" ))
  "(#([#[]]{}" )

(add-equal-test 'st-tokenizer
   (list->vector
    '((colon . ":")
      (verticalBar . "|")
      (cascade . ";")
      (binarySelector . "-")
      (period . ".")
      (carrot . "^")
      (minus . "-")
      (integer . "3")))
  (tokens-from (tokenizer-for-string ":|;-.^-3" ))
  ":|;-.^-3" )

(add-equal-test 'st-tokenizer
   (list->vector
    '((identifier . "True")
      (whitespace . " ")
      (methDef . "~>")
      (whitespace . " ")
      (identifier . "printString")
      (whitespace . " ")
      (blockStart . "[")
      (string . "'true'")
      (blockEnd . "]")
      (period . ".")))
   (tokens-from (tokenizer-for-string "True ~> printString ['true']."))
   "True ~> printString ['true'].")

(add-equal-test 'st-tokenizer
   (list->vector
    '((blockStart . "[")
      (blockArg . ":a")
      (whitespace . " ")
      (blockArg . ":b")
      (verticalBar . "|")
      (whitespace . " ")
      (identifier . "c")
      (assignment . ":=")
      (identifier . "a")
      (whitespace . " ")
      (binarySelector . "+")
      (whitespace . " ")
      (identifier . "b")
      (period . ".")
      (whitespace . " ")
      (identifier . "d")
      (whitespace . " ")
      (keyword . "foo:")
      (identifier . "e")
      (period . ".")
      (blockEnd . "]")))
   (tokens-from
      (tokenizer-for-string  "[:a :b| c:=a + b. d foo:e.]"))
   "True ~> printString ['true'].")

;; (ensure-exception-raised 'st-tokenizer
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

