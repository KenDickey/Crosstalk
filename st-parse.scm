;; Method Grammar adapted from the ANSI ST Standard
;; <method definition> changed for Candle-like syntax.

;;; AST Nodes

(define-structure (Assignment var val))
(define-structure (Block arguments temporaries statements returns))
(define-structure (Brace elements))
(define-structure (Cascade receiver messages))
(define-structure (Identifier token symbol))
(define-structure (Literal token value)) ;; elide token
(define-structure (Selector value))
(define-structure (Message receiver selector arguments precedence)) ; [0=subExp 1=unary 2=binary 3=keyword 4=other]
(define-structure (Array    elements))
(define-structure (ByteArry elements))
(define-structure (Method   selector block))
(define-structure (Sequence statements))
(define-structure (Temporaries identifiers))
(define-structure (LetTemps temps statements))
(define-structure (Subexpression expression))

;;; Token parsing

;; (define-structure (token kind string location))

;; token-kind is one of:
  ;; '( assignment
  ;;    badToken
  ;;    binarySelector blockArg blockStart blockEnd braceBegin braceEnd
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

(define debug-parser (make-parameter #true))
(define trace-parse-methods (make-parameter #true))

;; Quick Test
(define next-st-token (tokenizer-for-string " anArray at: 3 put: 37 "))

(define curr-token #false)
(define prev-token #false)

(define (curr-token-kind)
  (if curr-token
      (token-kind curr-token)
      'whitespace)) ;; unset

(define (consume-token!)
  (when (debug-parser)
    (newline)
    (display "consumed ")
    (display (token-kind curr-token))
    (newline)
    (display "  ")
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
  (when trace-parse-methods
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
  (when trace-parse-methods
    (newline)
    (display " (parse-st-code)"))
  (set! curr-token (next-st-token)) ;; get 1st token
  (skip-whitespace)
  (case (curr-token-kind)
    ((verticalBar)
     (let* ( (temps      (parse-temps))
             (statements (parse-statements))
           )
       (LetTemps temps statements))
     )
    ((eof)
     (Sequence '()) ;; no action!?!
     )
    (else
     (Sequence (parse-statements))
     )
   )
) 

;; <temporaries> ::= '|' <temporary-variable-list> '|'
;; <temporary-variable-list> ::= identifier*

(define (parse-temps) ;; #\| seen (but not consumed)
  (when trace-parse-methods
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
       ;;(LetTemps (reverse temps) '()))
       (reverse temps)
       )
      (else
       (parse-error "parse-temps: expected identifier or $|" curr-token (reverse temps))))
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

(define (parse-statements)
  (when trace-parse-methods
    (newline)
    (display " (parse-statements)"))
  (let loop ( (statements '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((period)
       (consume-token!)
       (let ( (statement (parse-statement)) )
         (loop (cons statement statements)))
       )
      ((carrot)
       (consume-token!)
       (let ( (return-exp (parse-expression)) )
         (loop (cons (Return return-exp) statements)))
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
       (Sequence (reverse statements))
       )
      (else
       (let ( (statement (parse-expression)) )
         (loop (cons statement statements)))
       )
      ) ; end-case
) )

(define (parse-statement)
  (when trace-parse-methods
    (newline)
    (display " (parse-statement)"))
  (error "@@NYI: (parse-statement)")
)

(define (make-cascade c-head c-tail)
  (when trace-parse-methods
    (newline)
    (display " (make-cascade c-head c-tail)"))
  (unless (Message? c-head)
    (parse-error "make-cascade: expected receiver!!" c-head))
  (unless (every? Message? c-tail)
    (parse-error "make-cascade: bad cascade tail" c-tail))
  (Cascade (receiver c-head) (cons c-head ctail))
)

(define (parse-cascade-tail)
  (when trace-parse-methods
    (newline)
    (display " (parse-cascade-tail)"))
  (error "@@NYI:  (parse-cascade-tail)"))

(define (parse-subexpression)
  (when trace-parse-methods
    (newline)
    (display " (parse-subexpression)"))
  (unless (eq? 'leftParen (curr-token-kind))
    (parse-error "parse-subexpression: expected $(" curr-token))
  (consume-token!)
  (let ( (subexpression (parse-expression)) )
    (skip-whitespace)
    (if (eq? 'rightParen (curr-token-kind))
        (Subexpression subexpression)
        (parse-error "parse-subexpression: expected $)" curr-token)))
)

(define (parse-expression)
  (when trace-parse-methods
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
                     (Literal curr-token (token->native curr-token)))
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
                     (Identifier curr-token (token->native curr-token)))
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
  (when trace-parse-methods
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
      (Literal new-token (- (token->native current-token)))))
)

(define (st-number-token? tok)
  (when trace-parse-methods
    (newline)
    (display " (st-number-token? tok)"))
  (if (member (token-kind token)
              '(integer integerWithRadix
                scaledDecimal scaledDecimalWithFract
                float floatWithExponent))
      #true
      #false))

(define (parse-literal-array)
  (when trace-parse-methods
    (newline)
    (display " (parse-literal-array)"))
  (error "@@NYI:  (parse-literal-array)"))

(define (parse-literal-byte-array)
  (when trace-parse-methods
    (newline)
    (display " (parse-literal-byte-array)"))
  (error "@@NYI:  (parse-literal-byte-array)"))

(define (parse-dynamic-array)
  (when trace-parse-methods
    (newline)
    (display " (parse-dynamic-array)"))
  (error "@@NYI:  (parse-dynamic-array)"))

(define (parse-block)
  (when trace-parse-methods
    (newline)
    (display " (parse-block)"))
  (error "@@NYI:  (parse-block)"))

(define (parse-unary-send receiver)
  (when trace-parse-methods
    (newline)
    (display " (parse-unary-send receiver)"))
  (unless (eq? 'identifier (curr-token-kind))
    (error "parse-unary-send: expected a unary selector" curr-token))
  (let ( (unary-message (Message receiver curr-token st-nil 1)) )
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
  (when trace-parse-methods
    (newline)
    (display " (parse-binary-send receiver)"))
  (unless (eq? 'binarySelector (curr-token-kind))
    (error "parse-binary-send: expected a binary selector" curr-token))
  (let ( (binarySelector (token->native curr-token)) )
    (consume-token!)
    (let ( (arg (parse-expression)) )
      (Message receiver binarySelector arg 2)
) ) )

(define (parse-keyword-send receiver)
  (when trace-parse-methods
    (newline)
    (display " (parse-keyword-send receiver)"))
  (unless (eq? 'keyword (curr-token-kind))
    (error "parse-keyword-send: expected a keyword" curr-token))
  ;; parse one or more <keyword, expression> pairs
  (let loop ( (results '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((keyword)
       (let ( (key (token->native curr-token)) )
         (consume-token!)
         (let ( (arg-exp (parse-expression)) ) ;; @@@parse-key-pairs@@@
           (loop (cons (cons key arg-exp) results))))
       )
      (else
       (when (debug-parser)
         (newline)
         (display (reverse results)))
       (let* ( (keys-and-args (reverse results))
               (selector
                (apply string-append
                 (map (lambda (pair) (symbol->string (car pair)))
                      keys-and-args)))
               (args (map cdr keys-and-args))
             )
         (when (null? keys-and-args)
           (error "parse-keyword-send: expected a keyword" curr-token))
         (Message receiver selector args 3)))
) ) )

(define (parse-assignment receiver)
  (when trace-parse-methods
    (newline)
    (display " (parse-assignment receiver)"))
  (error "@@NYI:  (parse-assignment receiver)"))

(define (parse-method-definition receiver)
  (when trace-parse-methods
    (newline)
    (display " (parse-method-definition receiver)"))
  (error "@@NYI:  (parse-method-definition receiver)"))



  
;; <block constructor> ::= '[' <block body> ']'
;; <block body> ::= [<block-argument>* '|'] [<temporaries>] [<statements>]
;; <block-argument> ::= ':' identifier

;; <messages> ::=
;; 	   (<unary-message>+ <binary-message>* [<keyword-message>] ) |
;; 	   (<binary-message>+ [<keyword-message>] ) |
;; 	   <keyword-message>
;; <unary-message> ::= unarySelector
;; <binary-message> ::= binarySelector <binary-argument>
;; <binary-argument> ::= <primary> <unary-message>*
;; <keyword-message> ::= (keyword <keyword-argument> )+
;; <keyword-argument> ::= <primary> <unary-message>* <binary-message>*
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
