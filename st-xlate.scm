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
   ;; ((astSubexpression? ast) ...)
   ;; ((astReturn? ast) ...)
   ((astAssignment? ast)
    (xlateAssignment ast))
   ((astBlock? ast)
    (xlateBlock ast))
   ;; ((astBrace? ast) ...)
   ;; ((astCascade? ast) ...)
   ((astIdentifier? ast)
    (astIdentifier-symbol ast))
   ((astLiteral? ast)
    (astLiteral-value ast))
   ((astSelector? ast)
    (astSelector-value ast))
   ;; ((astUnarySend? ast) ...)
   ;; ((astBinarySend? ast) ...)
   ;; ((astKeywordSend? ast) ...)
   ;; ((astUnaryMessage? ast) ...)
   ;; ((astBinaryMessage? ast) ...)
   ;; ((astKeywordMessage? ast) ...)
   ((astMessageSend? ast)
    (xlateMessageSend ast))
   ;; ((astArray? ast) ...)
   ;; ((astMethod? ast) ...)
   (else
    (error
     "unhandled/malformed AST" ast)
    )
  )
)

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

(define xlateStatement AST->scm) ;; @@FIXME: checks

;;; xlateAssignment

(define (xlateAssignment ast)
  (let ( (var (astAssignment-var ast))
         (val (astAssignment-val ast))
       )
    `(set! ,(AST->scm var)
           ,(AST->scm val))
  )
)

;;; xlateBlock

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
    
(define (m->send receiver ast-msg)
  (cond
   ((astMessageSequence? ast-msg)
    `(begin
       ,@(map
          (lambda (m) (m->send receiver m))
          (astMessageSequence-messages ast-msg)))
    )
   ((astUnaryMessage?   ast-msg)
    (list (token->native (astUnaryMessage-selector ast-msg))
          receiver)
    )
   ((astBinaryMessage?  ast-msg)
    `(,(astBinaryMessage-selector ast-msg)
      ,receiver
      ,(AST->scm (astBinaryMessage-argument ast-msg)))
    )
   ((astKeywordMessage? ast-msg)
    (let ( (arguments
            (map AST->scm
                 (astKeywordMessage-arguments ast-msg)))
           )
      `(,(astKeywordMessage-selector ast-msg)
        ,receiver
        ,@arguments))
    )
   (else
    (error
     "xlateMessageSend: does not understand"
     ast-msg)
    )
  )
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
