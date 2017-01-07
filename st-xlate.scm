;;; FILE: "st-xlate.scm"
;;; IMPLEMENTS: xlate -- translate st -> scheme
;;; AUTHOR: Ken Dickey
;;; DATE: 04 January 2017

;; (requires 'st-core-classes)

(define (AST->scm ast)
;; transliterate Abstract Syntax Tree
;; ..into Scheme syntax
  (cond
   ((astSequence? ast)
    (xlateSequence ast))
   ;; ((astMessageSequence? ast) ...)
   ;; ((astTemporaries? ast) ...)
   ;; ((astLetTemps? ast) ...)
   ((astSubexpression? ast)
    (AST->scm (astSubexpression-expression ast)))
   ;; ((astReturn? ast) ...)
   ((astAssignment? ast)
    (xlateAssignment ast))
   ((astBlock? ast)
    (xlateBlock ast))
   ;; ((astBrace? ast) ...)
   ((astCascade? ast)
    (xlateCascade ast))
   ((astIdentifier? ast)
    (astIdentifier-symbol ast))
   ((astLiteral? ast)
    (astLiteral-value ast))
   ((astSelector? ast)
    `',(astSelector-value ast)) ;; NB: quoted
   ;; ((astUnarySend? ast) ...)
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
   ;; ((astMethod? ast) ...)
   (else
    (error
     "unhandled/malformed AST" ast)
    )
  )
)


;;; Sequence

(define (xlateSequence ast)
  (let ( (ast-statements
          (astSequence-statements ast))
       )
    (if (null? ast-statements)
        '(begin st-nil)
        (map xlateStatement ast-statements)
    )
  )
)

;;; Statement

(define xlateStatement AST->scm) ;; @@FIXME: checks

;;; Assignment

(define (xlateAssignment ast)
  (let ( (var (astAssignment-var ast))
         (val (astAssignment-val ast))
       )
    `(set! ,(AST->scm var)
           ,(AST->scm val))
  )
)

;;; Block

(define (xlateBlock ast)
  (let ( (arguments
          (->scm-args  (astBlock-arguments ast)))
         (temps
          (->scm-temps (astBlock-temporaries ast)))
         (statements
          (->scm-statements (astBlock-statements ast)))
         (hasReturn?   (astBlock-hasReturn?  ast))
       )
    (cond
     ((null? statements)
      `(lambda ,arguments ,st-nil)
      )
     ((null? temps)
      (if hasReturn?
          `(lambda ,arguments
             (call/cc (return) ,@statements))
          `(lambda ,arguments ,@statements))
      )
     (else
      (if hasReturn?
           `(lambda ,arguments
              (call/cc (return)
                (let ,temps ,statements)))
          `(lambda ,arguments
             (let ,temps ,statements))
      ))
    )
  )
)

(define (->scm-args ast-args-list)
  (unless (every? astIdentifier? ast-args-list)
    (error "Block arguments must be identifiers"
           ast-args-list))
  (map astIdentifier-symbol ast-args-list)
)

(define (->scm-temps ast-temps-list)
  (unless (every? astIdentifier? ast-temps-list)
    (error "Block temporaries must be identifiers"
           ast-temps-list))
  (map (lambda (ast-tmp)
         (list (astIdentifier-symbol ast-tmp)
               st-nil))
       ast-temps-list)
)

(define (->scm-statements ast-list)
;;@@@FIXME: checks
  (let ( (statements
          (map AST->scm ast-list))
       )
    (if (null? statements)
        (list st-nil)
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

(define (xlateKeywordSend ast)
  (let ( (rcvr (AST->scm (astKeywordSend-receiver ast)))
         (selector (astKeywordSend-selector ast))
         (arguments
          (map AST->scm
               (astKeywordSend-arguments ast)))
       )
    (rsa->perform rcvr selector arguments)
  )
)

(define (xlateCascade ast)
  (let ( (rcvr     (astCascade-receiver ast))
         (messages (astCascade-messages ast))
       )
    `(begin ,@(map (lambda (msg) (m->send rcvr msg))
                   messages))
  )
)

(define (rsa->perform rcvr selector arguments)
  (case (length arguments)
    ((0) `(perform: ,rcvr ',selector)
     )
    ((1) `(perform:with: ,rcvr ',selector ,(car arguments))
     )
    ((2) `(perform:with:with:
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments))
     )
    ((3) `(perform:with:with:with:
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments))
     )
    ((4) `(perform:with:with:with:with
           ,rcvr
           ',selector
           ,(car arguments)
           ,(cadr arguments)
           ,(caddr arguments)
           ,(car (cdddr arguments)))
     
     )
    (else         
     `(perform:withArguments:
       ,rcvr
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
       )
    (cond
     ((astMessageSequence? ast-msg)
      `(begin
         ,@(map
            (lambda (m) (m->send rcvr m))
            (astMessageSequence-messages ast-msg)))
      )
     ((astUnaryMessage?   ast-msg)
      `(perform: ,rcvr
                 ',(token->native (astUnaryMessage-selector ast-msg)))
      )
     ((astBinaryMessage?  ast-msg)
      `(perform:with:
        ,rcvr
        ',(astBinaryMessage-selector ast-msg)
        ,(AST->scm (astBinaryMessage-argument ast-msg)))
      )
     ((astKeywordMessage? ast-msg)
      (let ( (arguments
              (map AST->scm
                   (astKeywordMessage-arguments ast-msg)))
           )
        (rsa->perform rcvr
                      (astKeywordMessage-selector ast-msg)
                      arguments))
      )
     ((and (token? ast-msg)
           (eq? 'identifier (token-kind ast-msg)))
      ;; unary message
      `(perform: ,rcvr ',(token->native ast-msg))
      )
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
  (list->vector (map AST->scm (astArray-elements ast)))
)

;; (define xlate
;;   (newSubclassName:iVars:cVars:
;;    Object
;;    'xlate '() '())
;; )

;; (perform:with:
;;      xlate
;;      'category:
;;      '|xlate@|)

;; (perform:with:
;;      xlate
;;      'comment:
;;      "xlate@"
;; )

;; @@@FillMeIn

;; (provides st-xlate)

;;;			--- E O F ---			;;;
