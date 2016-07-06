;;; IMPLEMENTS: Unit tests for st-tokenizer.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 29 June 2016

;; (require 'st-tokenizer)

(define get-token1 #false)
(define get-token2 #false)
(define get-token3 #false)
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
  (set! get-token1 #false)
  (set! get-token2 #false)
  (set! get-token3 #false)

  )

(add-test-suite 'st-tokenizer
                setup-st-tokenizer
                cleanup-st-tokenizer)

(add-equal-test 'st-tokenizer
  #((symbol . "#'x y'")
    (whitespace . " ")
    (symbol . "#'pHunNy x'")
    (whitespace . " ")
    (string . "'Joe''s string'")
    (whitespace . " ")
    (characterLiteral . "$C"))
  (tokens-from get-token1)
  "#'x y' #'pHunNy x' 'aString' $C")

(add-equal-test 'st-tokenizer
  #((integer . "23")
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
    (integer . "2"))
  (tokens-from get-token2)
  "23 12.3 12.3e4 16rBEEF 10s 12s12 1/2")

(add-equal-test 'st-tokenizer
   #((identifier . "block")
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
     (period . "."))
   (tokens-from get-token3)
   "block := [:a :b| ^(a foo: b bar: c + 7)].")


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
                
;; (ensure-exception-raised 'st-tokenizer
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

