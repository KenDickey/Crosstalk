;; Method Grammar adapted from the ANSI ST Standard
;; <method definition> changed for Candle-like syntax.

;;; AST Nodes

(define-structure (astAssignment var val))
(define-structure (astBlock arguments temporaries statements returns))
(define-structure (astBrace       elements))
(define-structure (astCascade     receiver messages))
(define-structure (astIdentifier  token symbol))
(define-structure (astLiteral     token value)) ;; elide token
(define-structure (astSelector    value))
(define-structure (astUnarySend   receiver selector))
(define-structure (astBinarySend  receiver selector argument))
(define-structure (astKeywordSend receiver selector arguments))
(define-structure (astArray       elements)) ;; ??astLiteral
(define-structure (astMethod      selector block))
(define-structure (astSequence    statements))
(define-structure (astTemporaries identifiers))
(define-structure (astLetTemps    temps statements))
(define-structure (astSubexpression expression))
(define-structure (astReturn        expression))

;;; Token parsing

;; (define-structure (token kind string location))

;; token-kind is one of:
  ;; '( assignment
  ;;    badToken
  ;;    binarySelector blockArg blockStart blockEnd
  ;;    braceBegin braceEnd
  ;;    carrot cascade characterLiteral colon comment
  ;;    dynArrayStart dynArrayEnd
  ;;    eof
  ;;    float floatWithExponent
  ;;    identifier integer integerWithRadix
  ;;    keyword
  ;;    leftParen litArrayStart litByteArrayStart
  ;;    methDef minus
  ;;    period
  ;;    rightParen
  ;;    scaledDecimal scaledDecimalWithFract sharp string symbol
  ;;    verticalBar
  ;;    whitespace )

(define debug-parser          (make-parameter #true))
(define trace-parse-methods   (make-parameter #true))
(define trace-skip-whitespace (make-parameter #false))

;; Quick Test
(define next-st-token
  (lambda ignored (error "next-st-token: input not defined !!")))
(define curr-token #false)
(define prev-token #false)

(define (parse-test input-string)
  (set! next-st-token (tokenizer-for-string input-string))
  (set! curr-token #false)
  (set! prev-token #false)

)

(parse-test " anArray at: 3 put: 37 ") ;; quick check
;; Invoke (parse-st-code) in REPL to test.

(define (curr-token-kind)
  (if curr-token
      (token-kind curr-token)
      'whitespace)) ;; unset

(define (consume-token!)
  (when (debug-parser)
    (newline)
    (display "consumed ")
    (display (token-kind curr-token))
;;  (newline)
    (display "\t\t")
    (display (token-string curr-token)))
  (set! prev-token curr-token)
  (set! curr-token (next-st-token))
  (when (debug-parser)
    (newline)
    (display "scanned ")
    (display (token-kind curr-token)))
)

(define parse-error error)


;;; Recursive Descent Parser
;;
;; By convention, decisions made based on curr-token-kind do NOT
;; consume the token.  Tokens are consumed by the corresponding
;; (called) parsing routine.

;; In general, tokens are kept to preserve location information,
;; NOT converted to Scheme native values.

;; separator ::= (whitespace | comment)*

(define (skip-whitespace)
  (when (trace-skip-whitespace)
    (newline)
    (display " (skip-whitespace)"))
  (case (curr-token-kind)
    ((whitespace comment)
     (consume-token!)
     (skip-whitespace)))
)

;; <st-code> ::=
;; 	     [<temporaries>]
;; 	     [<statements>]
;; 	     eof

(define (parse-st-code)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-st-code)"))
  (set! curr-token (next-st-token)) ;; get 1st token
  (skip-whitespace)
  (let ( (result
          (case (curr-token-kind)
            ((verticalBar)
             (let* ( (temps      (parse-temps))
                     (statements (parse-statements))
                     )
               (astLetTemps temps statements))
             )
            ((eof)
             (astSequence '()) ;; no action!?!
             )
            (else
             (astSequence (parse-statements))
             )))
       )
    (when (debug-parser)
      (newline))
    result)
)

;; <temporaries> ::= '|' <temporary-variable-list> '|'
;; <temporary-variable-list> ::= identifier*

(define (parse-temps) ;; #\| seen (but not consumed)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-temps)"))
  (unless (eq? 'verticalBar (curr-token-kind))
    (parse-error "parse-temps: expected $|" curr-token))
  (consume-token!)
  (let loop ( (temps '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((identifier)
       (consume-token!)
       (loop (cons prev-token temps))
       )
      ((verticalBar)
       (reverse temps)
       )
      (else
       (parse-error "parse-temps: expected identifier or $|"
                    curr-token
                    (reverse temps))))
) )

;; <statements> ::=
;; 	     (<return-statement> ['.'] ) |
;; 	     (<expression> ['.' [<statements>]])

;; <return statement> ::= '^' <expression>

;; <expression> ::=
;; 	     <assignment> |
;; 	     <method-definition> |
;; 	     <basic-expression>
;; <assignment> ::= <assignment-target> ':=' <expression>
;; <method-definition> ::= <class-name> '~>' <message-pattern> <methBody>
;; <basic-expression> ::=
;;        <primary> [<messages> <cascaded-messages>]
;;        <assignment-target> := identifier
;; <primary> ::=
;; 	  identifier |
;; 	   <literal> |
;; 	   <block-constructor> |
;; 	   ( '(' <expression> ')' )

(define (parse-statements) ;; answer a list of ASTs
  (when (trace-parse-methods)
    (newline)
    (display " (parse-statements)"))
  (let loop ( (statements '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((period)
       (consume-token!)
       (loop (append (parse-statements) statements))
       )
      ((carrot)
       (consume-token!)
       (let ( (return-exp (parse-expression)) )
         (reverse (cons (astReturn return-exp) statements)))
       )
      ((semicolon)
       (when (null? statements)
         (parse-error "parse-statements: cascade without reciever" curr-token))
       (let ( (cascade-head (car statements))
              (cascade-tail (parse-cascade-tail))
              )
         (loop (cons (make-cascade cascade-head cascade-tail)
                                       (cdr statements))))
       )
      ((left-paren)
       (let ( (subexpression (parse-subexpression)) )
         (loop (cons subexpression statements)))
       )
      ((eof)
       (reverse statements)
       )
      (else
       (loop (cons (parse-expression) statements))
       )
      ) ; end-case
) )


(define (make-cascade c-head c-tail)
  (when (trace-parse-methods)
    (newline)
    (display " (make-cascade c-head c-tail)"))
  (unless (Message? c-head)
    (parse-error "make-cascade: expected receiver!!" c-head))
  (unless (every? Message? c-tail)
    (parse-error "make-cascade: bad cascade tail" c-tail))
  (astCascade (receiver c-head) (cons c-head ctail))
)

(define (parse-cascade-tail)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-cascade-tail)"))
  (error "@@NYI:  (parse-cascade-tail)"))


(define (parse-subexpression)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-subexpression)"))
  (unless (eq? 'leftParen (curr-token-kind))
    (parse-error "parse-subexpression: expected $(" curr-token))
  (consume-token!)
  (let ( (subexpression (parse-expression)) )
    (skip-whitespace)
    (if (eq? 'rightParen (curr-token-kind))
        (astSubexpression subexpression)
        (parse-error "parse-subexpression: expected $)" curr-token)))
)

(define (parse-expression)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-expression)"))
  (skip-whitespace)
  (let ( (receiver
          (case (curr-token-kind)
            ((integer integerWithRadix
              scaledDecimal scaledDecimalWithFract
              float floatWithExponent
              string
              symbol)
             (let ( (literal
                     (astLiteral curr-token (token->native curr-token)))
                  )
               (consume-token!)
               literal)
             )
            ((minus)
             (parse-negative-number)
             )
            ((litArrayStart)
             (parse-literal-array)
             )
            ((litByteArrayStart)
             (parse-literal-byte-array)
             )
            ((dynArrayStart)
             (parse-dynamic-array)
             )
            ((blockStart)
             (parse-block)
             )
            ((identifier)
             (let ( (identifier
                     (astIdentifier curr-token (token->native curr-token)))
                  )
               (consume-token!)
               identifier)
             )
            (else
             (parse-error "parse-expression: unexpected input" curr-token))
            ))
          )
    (skip-whitespace)
    (case (curr-token-kind)
      ((identifier)
       (parse-unary-send receiver)
       )
      ((binarySelector)
       (parse-binary-send receiver)
       )
      ((keyword)
       (parse-keyword-send receiver)
       )
      ((assignment)
       (parse-assignment receiver)
       )
      ((methDef)
       (parse-method-definition receiver)
       )
      (else receiver) ;; simple expression (ignore following)
      )
) )

(define (parse-negative-number)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-negative-number)"))
  (unless (eq? 'minus (curr-token-kind))
    (error "parse-negative-number: expected $-" curr-token))
  (let ( (minus-token curr-token) )
    (consume-token!)
    ;; numeric digit MUST immediately follow $-
    ;; So no use of (skip-whitespace)
    (unless (st-number-token? current-token)
      (error "parse-negative-number: expected a number to follow $-"
             current-token))
    (let ( (new-token  ;; @@REVISIT: could recognize neg numbers in tokenizer
            (token (token-kind current-token)
                   (string-append "-" (token-string current-token))
                   (token-location minus-token)))
         )
      (astLiteral new-token (- (token->native current-token)))))
)

(define (st-number-token? tok)
  (if (member (token-kind token)
              '(integer integerWithRadix
                scaledDecimal scaledDecimalWithFract
                float floatWithExponent))
      #true
      #false))


;; <array literal> ::= '#(' <array element>* ')'
;; <array element> ::= <literal> | identifier

(define (parse-literal-array)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-literal-array)"))
  (unless (eq? 'litArrayStart (curr-token-kind))
    (parse-error "parse-literal-array: expected '#('"
                 curr-token))
  (consume-token!)
  (let ( (start-location (token-location curr-token)) )
    (consume-token!)
    (let loop ( (elts '()) )
      (skip-whitespace)
      (case (curr-token-kind)
        ((integer integerWithRadix
          float floatWithExponent
          scaledDecimal scaledDecimalWithFract
          characterLiteral string symbol
          )
         (let ( (literal
                 (astLiteral curr-token
                             (token->native curr-token)))
              )
           (consume-token!)
           (loop (cons literal elts)))
         )
        ((identifier)
         (let ( (identifier
                 (astIdentifier curr-token
                             (token->native curr-token)))
              )
           (consume-token!)
           (loop (cons identifier elts)))
         )
        ((litArrayStart)
         (let ( (array (parse-literal-array)) )
           (consume-token!)
           (loop (cons array elts)))
         )
        ((litByteArrayStart)
         (let ( (byte-array (parse-literal-byte-array)) )
           (consume-token!)
           (loop (cons byte-array elts)))
         )
        ((rightParen)
         (consume-token!)
         (astArray (reverse elts))
         )
        (else
         (parse-error
          "parse-literal-array: expected literal value or ')'"
          curr-token)))
      ) ;; loop
) )

(define (parse-literal-byte-array)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-literal-byte-array)"))
  (unless (eq? 'litByteArrayStart (curr-token-kind))
    (parse-error "parse-literal-byte-array: expected '#['"
                 curr-token))
  (let ( (start-location (token-location curr-token)) )
    (consume-token!)
    (let loop ( (elts '()) )
      (skip-whitespace)
      (case (curr-token-kind)
        ((integer)
         (let ( (byteval (token->native curr-token)) )
           (if (<= 0 byteval 256)
               (begin
                 (consume-token!)
                 (loop (cons byteval elts)))
               (parse-error
                "parse-literal-byte-array: expected byte or ']'"
                curr-token)))
         )
        ((blockEnd)
         (consume-token!)
         (astLiteral (make-token 'byteArrayLiteral
                                 "#[...]"
                                 start-location)
                     (list->bytevector (reverse elts)))
         )
        (else
         (parse-error
          "parse-literal-byte-array: expected byte or ']'"
          curr-token)))
    ) )
)

;; (define (byte-value-token? tok)
;;   (and (eq? 'integer (token-kind tok))
;;        (<= 0 (token->native (astLiteral-token t)) 256)))

;; (define (byte-value-literal? ast)
;;   (and (astLiteral? ast)
;;        (<= 0 (astLiteral-value ast) 256)))

(define (parse-dynamic-array)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-dynamic-array)"))
  (error "@@NYI:  (parse-dynamic-array)"))

(define (parse-block)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-block)"))
  (error "@@NYI:  (parse-block)"))

(define (parse-unary-send receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-unary-send receiver)"))
  (unless (eq? 'identifier (curr-token-kind))
    (error "parse-unary-send: expected a unary selector" curr-token))
  (let ( (unary-message
          (astUnarySend receiver (token->native curr-token))) )
    (consume-token!)
    (skip-whitespace)
    (case (curr-token-kind)
      ((identifier) ;; followed by another unary selector
       (parse-unary-send unary-message)
       )
      (else unary-message)
      )
) )

(define (parse-binary-send receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-binary-send receiver)"))
  (unless (eq? 'binarySelector (curr-token-kind))
    (error "parse-binary-send: expected a binary selector" curr-token))
  (let ( (binarySelector (token->native curr-token)) )
    (consume-token!)
    (let ( (arg (parse-binary-argument)) )
      (astBinarySend receiver binarySelector arg)
) ) )

;; <binary-argument> ::= <primary> <unary-message>*
(define (parse-binary-argument)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-binary-argument)"))
  (skip-whitespace)
  (let loop ( (arg (parse-primary)) )
    (skip-whitespace)
    (if (eq? 'identifier (curr-token-kind))
        (loop (parse-unary-send arg))
        arg))
)

(define (parse-keyword-send receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-keyword-send receiver)"))
  (unless (eq? 'keyword (curr-token-kind))
    (error "parse-keyword-send: expected a keyword" curr-token))
  ;; parse one or more <keyword, expression> pairs
  (let loop ( (results '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((keyword)
       (let ( (key (token-string curr-token)) )
         (consume-token!)
         (let ( (arg-exp (parse-keyword-argument)) )
           (loop (cons (cons key arg-exp) results))))
       )
      (else
       ;; (when (debug-parser)
       ;;   (newline)
       ;;   (display (reverse results)))
       (let* ( (keys-and-args (reverse results))
               (selector
                (string->symbol
                 (apply string-append
                        (map car keys-and-args))))
               (args (map cdr keys-and-args))
             )
         (when (null? keys-and-args)
           (error "parse-keyword-send: expected a keyword" curr-token))
         (astKeywordSend receiver selector args)))
) ) )

; <keyword-argument> ::= <primary> <unary-message>* <binary-message>*
(define (parse-keyword-argument)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-keyword-argument)"))

    (skip-whitespace)
    (let ( (result
            (let uloop ( (arg (parse-primary)) )
              (skip-whitespace)
              (if (eq? 'identifier (curr-token-kind))
                  (uloop (parse-unary-send arg))
                  arg)))
         )
      (let bloop ( (final-result result) )
        (skip-whitespace)
        (if (eq? 'binarySelector (curr-token-kind))
            (bloop (parse-binary-send final-result))
            final-result)))
)

;; <primary> ::=
;; 	  identifier |
;; 	   <literal> |
;; 	   <block-constructor> |
;; 	   ( '(' <expression> ')' )
(define (parse-primary)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-primary)"))
  (skip-whitespace)
  (case (curr-token-kind)
    ((identifier)
     (consume!+return
      (astIdentifier curr-token (token->native curr-token)))
     )
    ((integer integerWithRadix
              scaledDecimal scaledDecimalWithFract
              float floatWithExponent
              string
              symbol)
     (consume!+return
      (astLiteral curr-token (token->native curr-token)))
     )
    ((minus)
     (parse-negative-number)
     )
    ((litArrayStart)
     (parse-literal-array)
     )
    ((litByteArrayStart)
     (parse-literal-byte-array)
     )
    ((dynArrayStart)
     (parse-dynamic-array)
     )
    ((blockStart)
     (parse-block)
     )
    ((leftParen)
     (parse-subexpression)
     )
    (else
     (parse-error "parse-primary: unexpected token"
                  curr-token)
     )
) )

;; elide
;;  (let ( (result value) ) (consume-token!) result)
(define (consume!+return value)
  (consume-token!)
  value)

(define (parse-assignment receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-assignment receiver)"))
  (unless (eq? 'assignment (curr-token-kind))
    (parse-error "parse-assignment: expected #:="
                 curr-token))
  (consume-token!) ;; ":="
  (let ( (right-hand-side (parse-expression)) )
    (astAssignment receiver right-hand-side))
)

(define (parse-method-definition receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-method-definition receiver)"))
  (error "@@NYI:  (parse-method-definition receiver)"))



  
;; <block constructor> ::= '[' <block body> ']'
;; <block body> ::= [<block-argument>* '|'] [<temporaries>] [<statements>]
;; <block-argument> ::= ':' identifier

;; <messages> ::=
;;    (<unary-message>+ <binary-message>* [<keyword-message>] ) |
;;    (<binary-message>+ [<keyword-message>] ) |
;;    <keyword-message>
;; <unary-message> ::= unarySelector
;; <binary-message> ::= binarySelector <binary-argument>
;; <binary-argument> ::= <primary> <unary-message>*
;; <keyword-message> ::= (keyword <keyword-argument> )+
;; <cascaded-messages> ::= (';' <messages>)*

;; <method-definition> ::= <class-name> '~>' <message-pattern> <methBody>
;; <methBody> := '[' [<temporaries>] [<statements>] ']'

;; <message-pattern> ::= <unary-pattern> | <binary-pattern> | <keyword-pattern>
;; <unary-pattern> ::= unarySelector
;; <binary-pattern> ::= binarySelector <method-argument>
;; <keyword-pattern> ::= (keyword <method-argument>)+

;;; below here, recognition is largely done by the st-tokenizer

;; <literal> ::=
;; 	  <number literal> |
;; 	  <string literal> |
;; 	  <character literal> |
;; 	  <symbol literal> |
;; 	  <selector literal> |
;; 	  <array literal>

;; <number literal> ::= ['-'] <number>
;; <number> ::= integer | float | scaledDecimal

;; integer ::= decimalInteger | radixInteger
;; decimalInteger ::= digits
;; digits ::= digit+
;; radixInteger ::= radixSpecifier 'r' radixDigits
;; radixSpecifier := digits
;; radixDigits ::= (digit | uppercaseAlphabetic)+

;; float ::= mantissa [exponentLetter exponent]
;; mantissa ::= digits '.' digits
;; exponent ::= ['-']decimalInteger
;; exponentLetter ::= 'e' | 'd' | 'q'

;; scaledDecimal ::= scaledMantissa 's' [fractionalDigits]
;; scaledMantissa ::= decimalInteger | mantissa
;; fractionalDigits ::= decimalInteger

;; <array literal> ::= '#(' <array element>* ')'
;; <array element> ::= <literal> | identifier

;; quotedCharacter ::= '$' character

;; quotedString ::= stringDelimiter stringBody stringDelimiter
;; stringBody ::= (nonStringDelimiter | (stringDelimiter stringDelimiter)*)
;; stringDelimiter ::= ''' "a single quote"
;; nonStringDelimiter ::= "any character except stringDelimiter"

;; hashedString ::= '#' quotedString

;; quotedSelector ::= '#' 
;; 	       (unarySelector |
;; 	        binarySelector |
;; 		keywordSelector)
;; keywordSelector ::= keyword+


;; binaryCharacter ::=
;; 	'!' | '%' | '&'' | '*' | '+' | ','' | 
;; 	'/' | '<' | '=' | '>' | '?' | '@' |
;; 	'\' | '~' | '|' | '-'
;; binarySelector ::= binaryCharacter+
;; returnOperator ::= '^'
;; assignmentOperator ::= ':='

;; keyword ::= identifier ':'

;; identifier ::= letter (letter | digit)*

;; commentDelimiter ::= '"'
;; nonCommentDelimiter::=
;; 	"any character that is not a commentDelimiter "
;; comment :=
;; 	commentDelimiter nonCommentDelimiter * commentDelimiter

;; character ::=
;; "Any character in the implementation-defined character set"
;; whitespace ::= "Any non-printing character interpreted as white space including spaces, tabs, and
;; line breaks"
;; digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
;; uppercaseAlphabetic ::=
;;     'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 
;;     'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
;;     'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' |
;;     'V' | 'W' | 'X' | 'Y' | 'Z'
;; lowercaseAlphabetic ::=
;;     'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 
;;     'h' | 'I' | 'j' | 'k' | 'l' | 'm' | 'n' | 
;;     'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 
;;     'v' | 'w' | 'x' | 'y' | 'z'
;; nonCaseLetter ::= '_'
;; letter ::=
;;        uppercaseAlphabetic |
;;        lowercaseAlphabetic |
;;        nonCaseLetter |
;;        "implementation defined letters"

;; reservedIdentifier := 
;;   ( 'nil' | 'true' | 'false' | 'self' | 'super' )


;;;    		     --- E O F ---			;;;
