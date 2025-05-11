;;; FILE: "st-parser.ss"
;;; IMPLEMENTS: Smalltalk source recursive descent parser
;;; AUTHOR: Ken Dickey
;;; DATE: 21 June 2016; March 2025

;; Method Grammar adapted from the ANSI ST Standard
;; <method definition> changed for Candle-like syntax.


  (import
   (rnrs base)
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs bytevectors (6))
   (rnrs records inspection (6))
   (rnrs records procedural (6))
   (rnrs unicode (6))
   (rnrs mutable-strings (6))
   (only (chezscheme)
         define-structure
         format
         make-parameter
         parameterize
         open-output-string
         get-output-string
         void
       )
   )


;;; AST Nodes
  
(define-structure (astAssignment  var val))
(define-structure (astBlock arguments temporaries statements hasReturn?))
(define-structure (astBrace       elements))
(define-structure (astCascade     receiver messages))
(define-structure (astIdentifier  token symbol))
(define-structure (astLiteral     token value)) ;; elide token
(define-structure (astSelector    value))
(define-structure (astUnarySend   receiver selector))
(define-structure (astBinarySend  receiver selector argument))
(define-structure (astKeywordSend receiver selector arguments))
(define-structure (astUnaryMessage   selector))
(define-structure (astBinaryMessage  selector argument))
(define-structure (astKeywordMessage selector arguments))
(define-structure (astMessageSend receiver messages))
(define-structure (astArray       elements)) ;; ??astLiteral
(define-structure (astDynamicArray element-expressions))
(define-structure (astSequence    statements))
(define-structure (astMessageSequence messages))
(define-structure (astLetTemps    temps statements))
(define-structure (astSubexpression expression))
(define-structure (astReturn        expression))

(define (ident-token->astLiteral ident-tok)
  (make-astLiteral ident-tok 
              `',(token->native ident-tok))
)

(define (ident-token->astIdentifier ident-tok)
  (make-astIdentifier ident-tok (token->native ident-tok)))

(define (ident-token->astBlockArg ident-tok)
  (let ( (ident-sym (token->native ident-tok)) )
    (make-astIdentifier
     (make-token 'blockArg
            (string-append ":" (token-string ident-tok))
            (token-location ident-tok))
     ident-sym)
) )
  
(define (astTag ast)
  (if (and (vector? ast)
           (> (vector-length ast) 0))
      (vector-ref ast 0)
      'bogus))

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

(define debug-parser          (make-parameter #f))
(define trace-parse-methods   (make-parameter #f))
(define trace-skip-whitespace (make-parameter #f))

;; Quick Test
(define next-st-token
  (lambda ignored (parse-error 'next-st-token: "input not defined !!")))
(define curr-token #f)
(define prev-token #f)

(define (set-parse-tokenizer tokenizer)
  (set! next-st-token tokenizer)
  (set!  curr-token #f)
  (set! prev-token #f))

(define (parse-test input-string)
  (set! next-st-token (tokenizer-for-string input-string))
  (set! curr-token #f)
  (set! prev-token #f)
)

(define (st->AST st-string)
;; SmallTalk -> Abstract Syntax Tree
  (parse-test st-string)
  (parse-st-code))

;;(parse-test " anArray at: 3 put: 37 ") ;; quick check
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
               (make-astLetTemps temps statements))
             )
            ((eof)
             (make-astSequence '()) ;; no action!?!
             )
            (else
             (let ( (statements (parse-statements)) )
               (if (= 1 (length statements))
                   (car statements)
                   (make-astSequence statements))
             ))
            )
          )
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
    (parse-error 'parse-temps: "expected $|" curr-token))
  (consume-token!)
  (let loop ( (temps '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((identifier)
       (consume-token!)
       (loop (cons prev-token temps))
       )
      ((verticalBar)
       (consume-token!)
       (reverse temps)
       )
      (else
       (parse-error 'parse-temps: "expected identifier or $|"
                    curr-token
                    (reverse temps))))
) )

;; <statements> ::=
;; 	     (<return-statement> ['.'] ) |
;; 	     (<expression> ['.' [<statements>]])

;; <return statement> ::= '^' <expression>

(define (parse-statements) ;; answer a list of ASTs
  (when (trace-parse-methods)
    (newline)
    (display " (parse-statements)"))
  (skip-whitespace)
  (case (curr-token-kind)
    ((eof) '())
    ((carrot)
     (consume-token!)
     (let ( (return-exp (parse-expression)) )
        (when (eq? 'period (curr-token-kind))
          (consume-token!))
        (list (make-astReturn return-exp)))
     )
    ((blockEnd) ;; don't process
     st-nil
     )
    (else
     (let ( (exp (parse-expression)) )
       (skip-whitespace)
       (if (eq? 'period (curr-token-kind))
           (begin
             (consume-token!)
             (skip-whitespace)
             (if (eq? 'eof (curr-token-kind))
                 (list exp)
                 (cons exp (parse-statements)))
             )
           (list exp)))
     )
    )
)


(define (parse-subexpression)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-subexpression)"))
  (unless (eq? 'leftParen (curr-token-kind))
    (parse-error 'parse-subexpression: "expected $("
                 curr-token))
  (consume-token!)
  (let ( (subexpression (parse-expression)) )
    (skip-whitespace)
    (if (eq? 'rightParen (curr-token-kind))
        (begin
          (consume-token!)
          (make-astSubexpression subexpression))
        (parse-error 'parse-subexpression: "expected $)"
                     curr-token)))
)


;; <expression> ::=
;; 	     <assignment> |
;; 	     <method-definition> |
;; 	     <basic-expression>
;; <assignment> ::= <assignment-target> ':=' <expression>
;; <method-definition> ::=
;;        <class-expr> '~>' <message-pattern> <methBody>
;; <basic-expression> ::=
;;        <primary> [<messages> <cascaded-messages>]
;;        <assignment-target> := identifier
;; <primary> ::=
;; 	  identifier |
;; 	   <literal> |
;; 	   <block-constructor> |
;; 	   ( '(' <expression> ')' )

(define (parse-expression)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-expression)"))
  (skip-whitespace)
  (let ( (receiver (parse-primary)) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((assignment)
       (parse-assignment receiver)
       )
      ((methDef)
       (parse-method-definition receiver)
       )
      (else
       (let ( (expr
               (parse-basic-expression receiver))
            )
        (skip-whitespace)
        (if (eq? 'methDef (curr-token-kind))
            (parse-method-definition expr)
            expr))
      ))
) )

(define (parse-basic-expression receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-basic-expression)"))
  (skip-whitespace)
  (when (eq? (curr-token-kind) 'cascade)
    (parse-error 'parse-basic-expression: "cascade without message "
                 curr-token))
  (let ( (message
          (case (curr-token-kind)
            ((identifier)
             (make-astMessageSend
              receiver
              (parse-unary-message))
             )
             ((binarySelector)
              (make-astMessageSend
              	receiver
                (parse-binary-message))
              )
             ((keyword)
              (make-astMessageSend
              	receiver
                (parse-keyword-message))
              )
             (else receiver)
             )
          ))
    (if (eq? 'cascade (curr-token-kind))
        (parse-cascade receiver message)
        message)
) )

;; Note: cascaded messages are a bit odd:
;;   34 squared; printString. '34' 
;;   34 squared + 2; printString. '1156' 
;;   34 squared sqrt; printString.  '1156' 
;;   34 squared sqrt + 2; printString. '34'
;;   34 squared sqrt + 2 + 4; printString. '36'
;; So the rule seems to be:
;;   the cascade-receiver is the pen-ultimate expression before $;
(define (parse-cascade receiver first-message)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-cascade)"))
  (unless (eq? 'cascade (curr-token-kind))
    (parse-error 'parse-cascade: "expected $;" curr-token))
  (let-values ( ((cascade-rcvr cascade-first-msg)
                 (cascade-fixup receiver first-message)) )
    (consume-token!)
    (let loop ( (reversed-messages (list cascade-first-msg)) )
      (let ( (message (parse-messages)) )
        (skip-whitespace)
        (if (eq? 'cascade (curr-token-kind))
            (begin
              (consume-token!)
              (loop (cons message reversed-messages)))
            (make-astCascade cascade-rcvr
                        (reverse (cons message reversed-messages))))
    ) )
  )
)

(define (cascade-fixup receiver first-message)
  (if (and (astMessageSend? first-message)
           (astMessageSequence? (astMessageSend-messages first-message)))
      (let ( (rev-msg-seq
              (reverse (astMessageSequence-messages
                        (astMessageSend-messages first-message))))
           )
;;      (format #t "~%rev-msg-seq: ~y~%" rev-msg-seq)
        (values (make-astMessageSend
                     receiver
                     (make-astMessageSequence (reverse (cdr rev-msg-seq))))
                (make-astMessageSend
                     (make-astLiteral (make-token 'identifier "receiver" (list "receiver" 0 0))
                                 'receiver)
                     (car rev-msg-seq))))
      (values receiver first-message)
) )

(define (pattern->ast selector args)
  (case (token-kind selector)
    ((unarySelector)
     (make-astUnaryMessage selector)
     )
    ((binarySelector)
     (make-astBinaryMessage selector (car args))
     )
    ((keywordSelector)
     (make-astKeywordMessage selector args)
     )
    (else (parse-error
           "pattern->ast: expected message send"
           selector
           args))
  )
)


;; <messages> ::=
;;    (<unary-message>+ <binary-message>* [<keyword-message>] ) |
;;    (<binary-message>+ [<keyword-message>] ) |
;;    <keyword-message>
;; <unary-message> ::= unarySelector
;; <binary-message> ::= binarySelector <binary-argument>
;; <binary-argument> ::= <primary> <unary-message>*
;; <keyword-message> ::= (keyword <keyword-argument> )+

(define (parse-messages)
  (skip-whitespace)
  (case (curr-token-kind)
    ((identifier)
     (parse-unary-message)
     )
    ((binarySelector)
     (parse-binary-message)
     )
    ((keyword)
     (parse-keyword-message)
     )
    (else (parse-error
           "parse-message: expected a message send here"
           curr-token)
    )
  )
)
    

(define (parse-unary-message)
;; ( <unary-message>+ <binary-message>* [<keyword-message>] )
;; <unary-message> ::= unarySelector
  (when (trace-parse-methods)
    (newline)
    (display " (parse-unary-message)"))
  (skip-whitespace)
  (unless (eq? 'identifier (curr-token-kind))
    (parse-error 'parse-unary-message: "expected unary selector" curr-token))
  (consume-token!)
  (let unary-loop ( (reversed-messages (list (make-astUnaryMessage prev-token))) )
    (skip-whitespace)
    (if (eq? 'identifier (curr-token-kind))
        (begin
          (consume-token!)
          (unary-loop (cons (make-astUnaryMessage prev-token)
                            reversed-messages)))
        (let* ( (rev-added-binary
                 (if (eq? 'binarySelector (curr-token-kind))
                     (cons (parse-binary-message) reversed-messages)
                     reversed-messages))
                (rev-added-keyword
                 (if (eq? 'keyword (curr-token-kind))
                     (cons (parse-keyword-message) rev-added-binary)
                     rev-added-binary))
                (sequence (reverse rev-added-keyword))
             )
          (if (= 1 (length sequence))
              (car sequence)
              (make-astMessageSequence sequence))
       )
    )
  )
)


(define (parse-binary-message)
;; ( <binary-message>+ [<keyword-message>] )
;; <binary-message> ::= binarySelector <binary-argument>
;; <binary-argument> ::= <primary> <unary-message>*
  (when (trace-parse-methods)
    (newline)
    (display " (parse-binary-message)"))
  (skip-whitespace)
  (unless (eq? 'binarySelector (curr-token-kind))
    (parse-error 'parse-binary-message: "expected binary selector" curr-token))
  (let bin-loop ( (reversed-messages
                   (list (prim-parse-binary-message)))
                )
    (skip-whitespace)
    (if (eq? 'binarySelector (curr-token-kind))
        (bin-loop (cons (prim-parse-binary-message) reversed-messages))
        (if (eq? 'keyword (curr-token-kind))
            (make-astMessageSequence
             (reverse (cons (parse-keyword-message) reversed-messages)))
            (if (= 1 (length reversed-messages))
                (car reversed-messages) ;; unwrap single message
                (make-astMessageSequence
                 (reverse reversed-messages))
            )
        )
    )
  )
)


(define (prim-parse-binary-message)
  ;; Invariant: (eq? 'binarySelector (current-token))
  ;; i.e. a binarySelector was seen but not consumed.
  (let ( (binary-selector (token->native curr-token)) )
    (consume-token!)
    (make-astBinaryMessage binary-selector (parse-binary-argument))
  )
)

(define (parse-keyword-message)
;; <keyword-message>
;; <keyword-message> ::= (keyword <keyword-argument> )+
  (when (trace-parse-methods)
    (newline)
    (display " (parse-keyword-message)"))
  (skip-whitespace)
  (unless (eq? 'keyword (curr-token-kind))
    (parse-error 'parse-keyword-message: "expected a keyword" curr-token))
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
      (else ;; done
       (let* ( (keys-and-args (reverse results))
               (selector
                (string->symbol
                 (apply string-append
                        (map car keys-and-args))))
               (args (map cdr keys-and-args))
             )
         (when (null? keys-and-args)
           (parse-error 'parse-keyword-send: "expected a keyword" curr-token))
         (make-astKeywordMessage selector args))
      )
    )
  )
)


(define (parse-negative-number)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-negative-number)"))
  (unless (eq? 'minus (curr-token-kind))
    (parse-error 'parse-negative-number: "expected $-" curr-token))
  (let ( (minus-token curr-token) )
    (consume-token!)
    ;; numeric digit MUST immediately follow $-
    ;; So no use of (skip-whitespace)
    (unless (st-number-token? curr-token)
      (parse-error 'parse-negative-number: "expected a number to follow $-"
             curr-token))
    (let ( (new-token  ;; @@REVISIT: could recognize neg numbers in tokenizer
            (make-token (token-kind curr-token)
                   (string-append "-" (token-string curr-token))
                   (token-location minus-token)))
         )
      (consume-token!)
      (make-astLiteral new-token (- (token->native prev-token)))))
)


(define (st-number-token? token)
  (if (member (token-kind token)
              '(integer integerWithRadix
                scaledDecimal scaledDecimalWithFract
                float floatWithExponent))
      #t
      #f))


;; <array literal> ::= '#(' <array element>* ')'
;; <array element> ::= <literal> | identifier

(define (parse-literal-array)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-literal-array)"))
  (unless (eq? 'litArrayStart (curr-token-kind))
    (parse-error 'parse-literal-array: "expected '#('"
                 curr-token))
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
                 (make-astLiteral curr-token
                             (token->native curr-token)))
              )
           (consume-token!)
           (loop (cons literal elts)))
         )
        ((identifier)
         (let ( (identifier ;; a Symbol within an Array is unquoted
                 (make-astLiteral curr-token (token->native curr-token)))
              )
           (consume-token!)
           (loop (cons identifier elts)))
         )
        ((minus)
         (loop (cons (parse-negative-number) elts))
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
         (make-astArray (reverse elts))
         )
        (else
         (parse-error
          "parse-literal-array: expected literal value or ')'"
          curr-token)))
      ) ;; loop
) )

(define list->bytevector u8-list->bytevector)

(define (parse-literal-byte-array)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-literal-byte-array)"))
  (unless (eq? 'litByteArrayStart (curr-token-kind))
    (parse-error 'parse-literal-byte-array: "expected '#['"
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
         (make-astLiteral (make-token 'byteArrayLiteral
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
;;        (<= 0 (token->native (make-astLiteral-token t)) 256)))

;; (define (byte-value-literal? ast)
;;   (and (astLiteral? ast)
;;        (<= 0 (astLiteral-value ast) 256)))

(define (parse-dynamic-array)
  " { expr [. expr]* } "
  (when (trace-parse-methods)
    (newline)
    (display " (parse-dynamic-array)"))
  (unless (eq? 'dynArrayStart (curr-token-kind))
    (parse-error 'parse-dynamic-array: "expected ${" curr-token))
  (consume-token!)
  (let loop ( (expressions '()) )
    (skip-whitespace)
    (if (eq? 'dynArrayEnd (curr-token-kind))
        (begin
          (consume-token!) ;; "#\}"
          (make-astDynamicArray (reverse expressions)))
        (let ( (exp (parse-expression)) )
          (skip-whitespace)
          (if (eq? 'period (curr-token-kind))
              (consume-token!))
          (loop (cons exp expressions)))
  ) )
)


;; <block constructor> ::= '[' <block body> ']'
;; <block body> ::= [<block-argument>* '|']
;;                  [<temporaries>]
;;                  [<statements>]
;; <block-argument> ::= ':' identifier
;; <temporaries> ::= '|' <temporary-variable-list> '|'
;; <temporary-variable-list> ::= identifier*

(define (parse-block)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-block)"))
   (unless (eq? 'blockStart (curr-token-kind))
    (parse-error 'parse-block: "expected $[" curr-token))
  (consume-token!)
  (skip-whitespace)
  (let ( (args '())
         (temps '())
       )
  (when (eq? 'blockArg (curr-token-kind))
    (set! args (parse-block-args)))
  (skip-whitespace)
  (when (eq? 'verticalBar (curr-token-kind))
    (set! temps (parse-block-temps)))
  (skip-whitespace)
  (if (eq? 'blockEnd (curr-token-kind))
      (begin ;; no statements
        (consume-token!)
        (make-astBlock args temps '() #f))
      (let ( (statements (parse-statements)) )
        (skip-whitespace)
        (unless (eq? 'blockEnd (curr-token-kind))
          (parse-error 'parse-block: "expected $]" curr-token))
        (consume-token!)
        (make-astBlock args
                  temps
                  statements
                  (block-statements-have-return statements))))
) )

(define (block-statements-have-return ast)
;; traverse AST and check for internal RETURNs
  (call/cc
   (lambda (return)
     (letrec ( (check-for-returns
        (lambda (ast)
          (cond
           ((list? ast) (for-each check-for-returns ast) #f)
           ((astSequence? ast)
            (check-for-returns (astSequence-statements ast)))
           ((astSubexpression? ast)
            (check-for-returns (astSubexpression-expression ast)))
           ((astReturn? ast) (return #t))
           ((astAssignment? ast)
            (check-for-returns (astAssignment-val ast)))
           ((astBlock? ast)
            (if (astBlock-hasReturn? ast) (return #t))
            (check-for-returns (astBlock-statements ast)))
           ((astCascade? ast)
            (check-for-returns (astCascade-messages ast)))
           ((astIdentifier? ast) #f)
           ((astLiteral? ast) #f)
           ((astSelector? ast) #f)
           ((astUnarySend? ast) #f)
           ((astKeywordSend? ast) #f)
           ((astUnaryMessage? ast) #f)
           ((astBinaryMessage? ast) 
            (check-for-returns (astBinaryMessage-argument ast)))
           ((astKeywordMessage? ast)
            (check-for-returns (astKeywordMessage-arguments ast)))
           ((astMessageSequence? ast)
            (check-for-returns (astMessageSequence-messages ast)))
           ((astMessageSend? ast)
            (unless (check-for-returns (astMessageSend-receiver ast))
              (check-for-returns (astMessageSend-messages ast))))
           ((astArray? ast) #f)
           ((astDynamicArray? ast) #f)
           (else
            (error
             "check-for-returns: unhandled AST case" ast)
            )
           ) )
        ) )
       (check-for-returns ast)

) ) ) )


(define parse-block-temps parse-temps) ;; just an alias


(define (parse-block-args) ;; blockArg seen (but not consumed)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-block-args)"))
  (let loop ( (args '()) )
    (skip-whitespace)
    (case (curr-token-kind)
      ((blockArg)
       (consume-token!)
       (loop (cons (ident-token->astIdentifier prev-token)
                   args))
       )
      ((verticalBar)
       (consume-token!)
       (reverse args)
       )
      (else
       (parse-error 'parse-block-args: "expected :identifier or $|"
                    curr-token
                    (reverse args))))
) )


(define (parse-unary-send receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-unary-send receiver)"))
  (skip-whitespace)
  (unless (eq? 'identifier (curr-token-kind))
    (parse-error 'parse-unary-send: "expected a unary selector" curr-token))
  (let ( (unary-message
          (make-astUnarySend receiver (token->native curr-token))) )
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
  (skip-whitespace)
  (unless (eq? 'binarySelector (curr-token-kind))
    (parse-error 'parse-binary-send: "expected a binary selector"
           curr-token))
  (let bin-loop ( (reversed-messages
                   (list (prim-parse-binary-message)))
                )
    (skip-whitespace)
    (if (eq? 'binarySelector (curr-token-kind))
        (bin-loop (cons (prim-parse-binary-message) reversed-messages))
        (make-astMessageSend receiver
                        (if (= 1 (length reversed-messages))
                            ;; unwrap single message
                            (car reversed-messages)
                            (make-astMessageSequence
                             (reverse reversed-messages))
        )               )

    )
  )
)
  

;; <binary-argument> ::= <primary> <unary-message>*
(define (parse-binary-argument)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-binary-argument)"))
  (skip-whitespace)
  (let ( (primary (parse-primary)) )
    (skip-whitespace)
    (if (not (eq? 'identifier (curr-token-kind)))
        primary
        (let unary-loop ( (msg
                           (make-astUnarySend primary
                                         (token->native curr-token)))
                        )
          (consume-token!)
          (skip-whitespace)
          (if (eq? 'identifier (curr-token-kind))
              (unary-loop (make-astUnarySend msg
                                        (token->native curr-token)))
              msg)
        )
    )
  )
)


(define (parse-keyword-send receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-keyword-send receiver)"))
  (unless (eq? 'keyword (curr-token-kind))
    (parse-error 'parse-keyword-send: "expected a keyword" curr-token))
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
           (parse-error 'parse-keyword-send: "expected a keyword" curr-token))
         (make-astKeywordSend receiver selector args)))
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
     (consume-token!)
     (ident-token->astIdentifier prev-token)
     )
    ((integer integerWithRadix
              scaledDecimal scaledDecimalWithFract
              float floatWithExponent
              string characterLiteral)
     (consume-token!)
     (make-astLiteral prev-token (token->native prev-token))
     )
    ((symbol)
     (consume-token!) ;; quote it!
     (make-astLiteral prev-token `,(token->native prev-token)))
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
    ((sharp) ;; symbol
     (consume-token!) ;; #
     (consume-token!) ;; <a symbol>
     (make-astLiteral prev-token `',(token->native prev-token))
     )
    (else
     (parse-error 'parse-primary: "unexpected token"
                  curr-token)
     )
) )


(define (parse-assignment receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-assignment receiver)"))
  (unless (eq? 'assignment (curr-token-kind))
    (parse-error 'parse-assignment: "expected #:="
                 curr-token))
  (consume-token!) ;; ":="
  (let ( (right-hand-side (parse-expression)) )
    (make-astAssignment receiver right-hand-side))
)

;; <method-definition>
;;       ::= <class> '~>' <message-pattern> <methBody>
;; <methBody> := '[' [<temporaries>] [<statements>] ']'


;; 'String ~> contains: aChar
;;    [ self detect: [ :c | c = aChar ]].'

;;  logically rewrites to:

;; 'String
;;    addSelector: #contains:
;;    withMethod: [:self :aChar| self detect: [:c|c = aChar]].'

(define (parse-method-definition receiver)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-method-definition receiver)"))
  (unless (eq? 'methDef (curr-token-kind))
    (parse-error 'parse-method-definition: "expected #~>"
                 curr-token))
  (consume-token!)
  (skip-whitespace)

  (let-values ( ((selector args) (parse-message-pattern)) ) 
    (skip-whitespace)
    (unless (eq? 'blockStart (curr-token-kind))
      (parse-error 'parse-method-definition: "expected $["
                   curr-token))
    (let* ( (method-block (parse-block))
            (method-args ;; Prepend #:self block-arg
             (cons
              (make-astIdentifier
                 (make-token 'blockArg ":self" (vector ":self" 0 0))
                 'self)
              args))
          )
      ;; (make-astBlock arguments temporaries statements hasReturn?)
      (vector-set! method-block 1 method-args) 
      ;; Unlike block-closures, which return their last value,
      ;; method blocks return #self unless explicit ^return.
      (unless (astBlock-hasReturn? method-block)
        (set-astBlock-statements!
         method-block
         (append (astBlock-statements method-block)
                 (list (make-astIdentifier
                        (make-token 'identifier
                                    "self"
                                    (vector "self" 0 0))
                        'self))))
      )
;;    (format #t "selector token: ~a" (astLiteral-token selector))
      (if (eq? (token-kind (astLiteral-token selector)) 'binarySelector)
          (make-astKeywordSend receiver
                      'addSelector:withMethod:arity:
                      (list selector
                            method-block
                            (make-astLiteral
                             (make-token 'integer
                                         "2"
                                         (vector "2" 0 0))
                             2)))
          (make-astKeywordSend receiver
                               'addSelector:withMethod:
                               (list selector method-block)))
) ) )
                          

;; <message-pattern>
;;       ::= <unary-pattern> | <binary-pattern> | <keyword-pattern>
;; <unary-pattern> ::= unarySelector
;; <binary-pattern> ::= binarySelector <method-argument>
;; <keyword-pattern> ::= (keyword <method-argument>)+

(define (parse-message-pattern) ;; Answer ('selector args)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-message-pattern)"))
  (skip-whitespace)
  (case (curr-token-kind)
    ((identifier) ;; unarySelector
     (consume-token!)
     (values (ident-token->astLiteral prev-token) '())
     )
    ((binarySelector)
     (let ( (selector-tok curr-token) )
       (consume-token!)
       (skip-whitespace)
       (unless (eq? 'identifier (curr-token-kind))
         (parse-error
          "parse-message-pattern: expected binarySelector identifier"
          selector-tok
          curr-token))
       (consume-token!)
       (values (ident-token->astLiteral selector-tok)
               (list (ident-token->astBlockArg prev-token))))
     )
    ((keyword)
     (parse-keyword-pattern)
     )
    (else
     (parse-error
      "parse-message-pattern: expected a send-pattern"
      curr-token))
    )
)

(define (parse-keyword-pattern)
  (when (trace-parse-methods)
    (newline)
    (display " (parse-keyword-pattern)"))
  (unless (eq? 'keyword (curr-token-kind))
    (parse-error 'parse-keyword-pattern: "expected a keyword" curr-token))
  ;; parse one or more <keyword, identifier> pairs
  (let ( (start-token-location
          (token-location curr-token))
       )
    (let loop ( (results '()) )
      (skip-whitespace)
      (case (curr-token-kind)
        ((keyword)
         (let ( (key (token-string curr-token)) )
           (consume-token!) ; keyword
           (skip-whitespace)
           (unless (eq? 'identifier (curr-token-kind))
             (parse-error
              "parse-keyword-pattern: expected an identifier"
              key
              curr-token))
           (consume-token!) ; identifier
           (loop (cons (cons key prev-token) results)))
         )
        (else
         ;; (when (debug-parser)
         ;;   (newline)
         ;;   (display (reverse results)))
         (let* ( (keys-and-args (reverse results))
                 (selector-string
                  (apply string-append
                         (map car keys-and-args)))
                 (args (map cdr keys-and-args))
              )
           (values (make-astLiteral
                    (make-token 'symbol
                           (string-append "#" selector-string)
                           start-token-location)
                    `',(string->symbol selector-string))
                   (map ident-token->astBlockArg args)))
      ) )
) ) )

  
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

'st-parser

;;;    		     --- E O F ---			;;;
