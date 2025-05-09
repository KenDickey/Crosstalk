;;; FILE: "st-xlate.ss"
;;; IMPLEMENTS: xlate -- translate st -> scheme
;;; AUTHOR: Ken Dickey
;;; DATE: 04 January 2017; March 2025

  (import
   (rnrs base)
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs files (6))
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs records inspection (6))
   (rnrs records procedural (6))
   (rnrs unicode (6))
   (rnrs mutable-strings (6))
   (rnrs eval (6))
   (rnrs exceptions (6))
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

(define (AST->scm ast)
;; transliterate Abstract Syntax Tree
;; ..into Scheme syntax
  (cond
   ((astSequence? ast)
    (xlateSequence ast))
   ;; ((astMessageSequence? ast) ...)
   ;; ((astTemporaries? ast) ...)
   ((astLetTemps? ast)
    (xlateLetTemps ast))
   ((astSubexpression? ast)
    (AST->scm (astSubexpression-expression ast)))
   ((astReturn? ast)
    (xlateReturn ast))
   ((astAssignment? ast)
    (xlateAssignment ast))
   ((astBlock? ast)
    (xlateBlock ast))
   ;; ((astBrace? ast) ...)
   ((astCascade? ast)
    (xlateCascade ast))
   ((astIdentifier? ast)
    (xlateIdentifier ast))
   ((astLiteral? ast)
    (astLiteral-value ast))
   ((astSelector? ast)
    `',(astSelector-value ast)) ;; NB: quoted
   ((astUnarySend? ast)
    (xlateUnarySend ast))
   ;; ((astBinarySend? ast) ...)
   ((astKeywordSend? ast) 
    (xlateKeywordSend ast))
   ;; ((astUnaryMessage? ast) ...)
   ;; ((astBinaryMessage? ast) ...)
   ;; ((astKeywordMessage? ast) ...)
   ((astMessageSend? ast)
    (xlateMessageSend ast))
   ((astArray? ast)
    (xlateArray ast)
    )
   ((astDynamicArray? ast)
    (xlateDynamicArray ast)
    )
   ;; ((astMethod ast) ...)
   (else
    (error
     "unhandled/malformed AST" ast)
    )
  )
)

(define (st->scm aString)
  (unless (string? aString)
    (error 'st->scm
           "requires a Smalltalk string to translate"
           aString))
  (AST->scm (st->AST aString)))


(define (identifier-token? t)
  (and (token? t)
       (eq? 'identifier (token-kind t)))
)


;;; Temps Statements

(define (xlateLetTemps ast)
  (let ( (temps (->scm-temps (astLetTemps-temps ast)))
         (statements (->scm-statements (astLetTemps-statements ast)))
       )
    `(let ,temps ,@statements)
) )

;;; Sequence

(define (xlateSequence ast)
  (let ( (ast-statements
          (astSequence-statements ast))
       )
    (if (null? ast-statements)
        '(begin '()) ;; answer st-nil
        (let ( (statements
                (map xlateStatement ast-statements))
             )
          (if (= 1 (length statements))
              (car statements)
              `(begin ,@statements)
          )
       )
    )
  )
)

;;; Statement

(define xlateStatement AST->scm) ;; @@FIXME: checks

;;; Assignment

(define (xlateAssignment ast)
  (let ( (var (AST->scm (astAssignment-var ast)))
         (val (AST->scm (astAssignment-val ast)))
       )
;; st semantics; assignment returns value
    `(let ( (%%val%% ,val) )  
       (set! ,var %%val%%)
       %%val%%)
  )
)

;;; Block

;; dynamic var
(define within-return? (make-parameter #f))

(define (xlateBlock ast)
  (let* ( (arguments
           (->scm-args  (astBlock-arguments ast)))
          (temps
           (->scm-temps (astBlock-temporaries ast)))
          (hasReturn?   (astBlock-hasReturn?  ast))
          (addReturn?
           (and hasReturn?
                (not (within-return?))
                (not (simple-return? (astBlock-statements ast))))
           )
          (statements
           (cond
;;;simplify:
;;;  (call/cc (return) (return foo)) -> foo
            ((and (simple-return? (astBlock-statements ast))
                  (not (within-return?)))
             (simplified-return (astBlock-statements ast))
             )
            (hasReturn?
             (parameterize ((within-return? #t))
               (->scm-statements (astBlock-statements ast)))
             )
            (else
             (->scm-statements (astBlock-statements ast)))
            ))
       )
    (cond
     ((null? statements)
      `(lambda ,arguments ,st-nil)
      )
     ((null? temps)
      (if addReturn?
          `(lambda ,arguments
             (call/cc (lambda (return) ,@statements)))
          `(lambda ,arguments ,@statements))
      )
     (else
      (if addReturn?
           `(lambda ,arguments
              (call/cc (lambda (return)
                (let ,temps ,@statements))))
          `(lambda ,arguments
             (let ,temps ,@statements))
      ))
    )
  )
)

(define (simple-return? ast)
  (and (list? ast)
       (= 1 (length ast))
       (astReturn? (car ast))))

(define (simplified-return ast)
  (list (AST->scm (astReturn-expression (car ast)))))

;;;; Return

(define (xlateReturn ast)
  `(return ,(AST->scm (astReturn-expression ast)))
)


;;; Identifier
(define (xlateIdentifier ast)
   (let ( (ident (astIdentifier-symbol ast)) )
     (cond
      ((capitalized-symbol? ident) ;; => Smalltalk Global
       `(smalltalkAt: ',ident))
      ((memq ident '(nil true false self super))
       ident) ;; normal Scheme identifier (less #super)
      (else
       ident) ;; local XOR instance accessor
 ) ) )

(define (capitalized-symbol? ident)
  (and (symbol? ident)  ;; invariant: symbol-length > 0
       (char-upper-case? (string-ref (symbol->string ident) 0))))


(define (->scm-args ast-args-list)
  (unless (every? astIdentifier? ast-args-list)
    (error "Block arguments must be identifiers"
           ast-args-list))
  (map astIdentifier-symbol ast-args-list)
)

(define (->scm-temps ast-temps-list)
  (unless (every? identifier-token? ast-temps-list)
    (error "Block temporaries must be identifiers"
           ast-temps-list))
  (map (lambda (ident-tok)
         `(,(token->native ident-tok) nil))
       ast-temps-list)
)

(define (->scm-statements ast-list)
;;@@@FIXME: checks
  (let ( (statements
          (map AST->scm ast-list))
       )
    (if (null? statements)
        '(nil)
        statements)
  )
)

;;; Message Sends

(define (astMessage? ast)
  (or (astUnaryMessage?   ast)
      (astBinaryMessage?  ast)
      (astKeywordMessage? ast)
  )
)

(define (xlateUnarySend ast)
  (let ( (receiver (AST->scm (astUnarySend-receiver ast)))
         (selector (astUnarySend-selector ast))
       )
    (if (eq? receiver 'super)
        `(% self ',selector)
        `($ ,receiver ',selector))
) )

(define (xlateKeywordSend ast)
  (let ( (rcvr (AST->scm (astKeywordSend-receiver ast)))
         (selector (astKeywordSend-selector ast))
         (arguments
          (map AST->scm
               (astKeywordSend-arguments ast)))
       )
    (if (eq? rcvr 'super)
        (rsa->superPerform 'self selector arguments)
        (rsa->perform rcvr selector arguments))
  )
)


(define (xlateCascade ast)
  (let* ( (rcvr (AST->scm (astCascade-receiver ast)))
          (messages (astCascade-messages ast))
        )
;; Presume #super NEVER allowed in this context
    `(let ( (receiver ,rcvr) )
       ,(AST->scm (car messages))
       ,@(map (lambda (msg) (m->send 'receiver msg))
              (cdr messages)))
  )
)

(define (rsa->perform rcvr selector arguments)
  (case (length arguments)
    ((0) `($ ,rcvr ',selector)
     )
    ((1) `($: ,rcvr ',selector ,(car arguments))
     )
    ((2) `($::
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments))
     )
    ((3) `($:::
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments))
     )
    ((4) `($::::
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments)
           ,(cadddr arguments))
     
     )
    (else         
     `($*
       ,rcvr
       ',selector
       ,(cons 'list arguments))
     )
  )
)

(define (rsa->superPerform rcvr selector arguments)
  (case (length arguments)
    ((0) `(% self ',selector)
     )
    ((1) `(%: self ',selector ,(car arguments))
     )
    ((2) `(%::
           self
           ',selector
           ,(car arguments)
           ,(cadr arguments))
     )
    ((3) `(%:::
           self
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments))
     )
    ((4) `(%::::
           self
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments)
           ,(cadddr arguments))
     
     )
    (else         
     `(%&
       self
       ',selector
       ,(list->vector arguments))
     )
  )
)


(define (xlateMessageSend ast)
  (let ( (receiver
          (AST->scm
           (astMessageSend-receiver ast)))
         (messages
          (astMessageSend-messages ast))
       )
    (m->send receiver messages)
  )
)
    
(define (m->send rcvr ast-message)
  (let ( (ast-msg
          (if (and (list? ast-message)
                   (= 1 (length ast-message)))
              (car ast-message)
              ast-message))
         (superSend? (eq? rcvr 'super))
       )
    (cond
     ((astMessageSequence? ast-msg)
      (let ( (messages
              (astMessageSequence-messages ast-msg))
           )
        (if (null? messages)
            rcvr
            (let loop ( (composed (m->send rcvr (car messages)))
                        (msgs (cdr messages))
                      )
              (if (null? msgs)
                  composed
                  (loop (m->send composed (car msgs)) (cdr msgs)))
            )
        )
      ))
     ((astUnaryMessage?   ast-msg)
      (if superSend?
          `(% self
          ',(token->native (astUnaryMessage-selector ast-msg)))
          `($ ,rcvr
              ',(token->native (astUnaryMessage-selector ast-msg)))
      ))
     ((astBinaryMessage?  ast-msg)
      (if superSend?
          `(%: self
               ',(astBinaryMessage-selector ast-msg)
               ,(AST->scm (astBinaryMessage-argument ast-msg)))
          `($: ,rcvr
               ',(astBinaryMessage-selector ast-msg)
               ,(AST->scm (astBinaryMessage-argument ast-msg)))
      ))
     ((astKeywordMessage? ast-msg)
      (let ( (arguments
              (map AST->scm
                   (astKeywordMessage-arguments ast-msg)))
           )
        ((if superSend? rsa->superPerform rsa->perform)
         rcvr
         (astKeywordMessage-selector ast-msg)
         arguments))
      )
     ((and (token? ast-msg)
           (eq? 'identifier (token-kind ast-msg)))
      ;; unary message
      (if superSend?
          `(% self ',(token->native ast-msg))
          `($ ,rcvr ',(token->native ast-msg))
      ))
     (else
      (error
       "xlateMessageSend: does not understand"
       ast-msg)
      )
    )
  )
)

;;; Static Array
(define (xlateArray ast)
  (list 'quote (list->vector (map AST->scm (astArray-elements ast))))
)


(define (xlateDynamicArray ast)
  `(vector ,@(map AST->scm (astDynamicArray-element-expressions ast)))
)


(define (xlate-st-file->scm infile-name)
  (set-parse-tokenizer (tokenizer-for-file-named infile-name))
  (call-with-port (current-output-port)
    (lambda (outp)
      (let loop ( (form (parse-st-code)) )
        (format outp
                "~%~y" ;; pretty-print
                (AST->scm form))
        (skip-whitespace)
        (unless (eq? 'eof (curr-token-kind))
          (loop (parse-st-code)))
) ) ) )


(define (xlate-st-file->scm-file infile-name outfile-name)
  (set-parse-tokenizer (tokenizer-for-file-named infile-name))
  (when (file-exists? outfile-name)
    (delete-file  outfile-name))
  (call-with-output-file outfile-name
    (lambda (outp)
      (format outp "~%;; \"~a\" (translated)~%" outfile-name)
      (let loop ( (form (parse-st-code)) )
        (format outp
                "~%~y" ;; pretty-print
                (AST->scm form))
        (skip-whitespace)
        (unless (eq? 'eof (curr-token-kind))
          (loop (parse-st-code)))
        )
      (format outp "~%;;   -- e o f ---    ;;~%")
) ) )

;;; st-eval

;;(define %%escape%% (make-parameter (lambda whatever '%%escape%%)))

(define st-environment
  (environment
   '(st-core-mechanics)
   )
)

(define (st-eval st-string)
  (call/cc
   (lambda (exit)
     (parameterize ( (%%escape%% exit) ) ;; for send-failed
       (with-exception-handler
        (lambda (anException)
;;          (($ ($ anException 'asException) 'defaultAction))
          (exit
           (let ( (exception ($ anException 'asException)) ) ; condition->ex
;;@@@DEBUG{
;;(display-ivars ex)
;;@@@DEBUG}
             (cond
;;              ((and (isKindOf: anException MessageNotUnderstood)
;;                    ($ anException 'reachedDefaultHandler))
              ((isKindOf: exception MessageNotUnderstood)
	       ($ exception 'messageText)
               ;; (error ($ anException 'messageText)
               ;;        ($ anException 'printString)
               ;;        anException))
               )
              (else
;;@@@DEBUG{
(when (debug-st-runtime)
  (format #t "about to ~a>>defaultAction"
          ($ exception 'printString)))
;;@@@DEBUG}
               ($ exception 'defaultAction))))))
        (lambda () (eval (st->scm st-string) (scheme-environment)))
        )
) ) ) )


(define initialized? (make-parameter #f))

(define (init-st-xlate)
  (unless (initialized?)
    (initialized? #t)
;;; DEBUG -- fake a Transcript
    (smalltalkAt:put: 'Transcript (current-output-port))
    'st-xlate
  ) )

'st-xlate

;;;			--- E O F ---			;;;
