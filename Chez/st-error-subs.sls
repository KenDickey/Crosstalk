;;; FILE: "st-error-subs.sls"
;;; IMPLEMENTS: Warning, MessageNotUnderstood,
;;;		ArithmeticError, AssertionFailure
;;; See also "st-exception.sls", "st-error.sls" for superclasses.
;;; See also "st-blockClosure.scm" for exception handling code.
;;; See "st-conditions.scm" for #asException
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017; March 2025


(library (st-error-subs)

  (export
   init-st-error-subs

   Warning
   MessageNotUnderstood
   ArithmeticError
   AssertionFailure
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (rnrs unicode (6))
   (rnrs io simple (6))
   (rnrs hashtables (6))
   (rnrs exceptions (6))
   (rnrs conditions (6))
   (rnrs arithmetic bitwise (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-boolean) ;; UndefinedObject
   (st-blockClosure)
   (st-exception)
   (st-error)
   )

(define Warning
  (newSubclassName:iVars:cVars:
   Notification
   'Warning '() '())
)

(define MessageNotUnderstood
  (newSubclassName:iVars:cVars:
   Error
   'MessageNotUnderstood
   '(message reachedDefaultHandler)
   '())
)

(define ArithmeticError
  (newSubclassName:iVars:cVars:
   Error
   'ArithmeticError '() '())
)

(define AssertionFailure
  (newSubclassName:iVars:cVars:
   Halt
   'AssertionFailure '() '())
)


;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-error-subs)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-error)
  
(perform:with: Warning
               'methodDict:
               (behavior-add-from-other
                ($ Warning 'methodDict)
                ($ Notification 'methodDict)))

(perform:with: MessageNotUnderstood
               'methodDict:
               (behavior-add-from-other
                ($ MessageNotUnderstood 'methodDict)
                ($ Error 'methodDict)))

(perform:with: ArithmeticError
               'methodDict:
               (behavior-add-from-other
                ($ ArithmeticError 'methodDict)
                ($ Error 'methodDict)))

(perform:with: AssertionFailure
               'methodDict:
               (behavior-add-from-other
                ($ AssertionFailure 'methodDict)
                ($ Halt 'methodDict)))

(perform:with:
     Warning
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     AssertionFailure
     'category: (string->symbol "Exceptions Extensions"))

(perform:with:
     MessageNotUnderstood
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     Warning
     'comment:
"A Warning is a Notification which by default should be brought
 to the attention of the user.")

(perform:with:
     AssertionFailure
     'comment:
"AssertionFailure is the exception signaled from Object>>assert:
 when the assertion block evaluates to false."
)

;;; MessageNotUnderstood

(addSelector:withMethod:
     MessageNotUnderstood
     'initialize
     (lambda (self)
       (superPerform: self 'initialize)
       ($: self 'reachedDefaultHandler: st-false)
       self))

(addSelector:withMethod:
     MessageNotUnderstood
     'defaultAction
     (lambda (self)
       ($: self 'reachedDefaultHandler: st-true)
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t "~%MessageNotUnderstood defaultAction"))
;;@@DEBUG}
       (superPerform: self 'defaultAction)))

(addSelector:withMethod:
     MessageNotUnderstood
     'isResumable
     (lambda (self) st-true))

(addSelector:withMethod:
     MessageNotUnderstood
     'messageText
     (lambda (self)
       (let ( (myMessage ($ self 'message)) )
         (if (not (null? myMessage))
             (format #f
                     "~a doesNotUnderstand: #~a"
                     (safer-printString ($ myMessage 'receiver))
                     ($ myMessage 'selector))
             (let ( (inherited-msg (superPerform: self 'messageText)) )
               (if (null? inherited-msg)
                   (format #f
                           "~%~a doesNotUnderstand: <something>"
                           (safer-printString ($ self 'receiver)) )
                   inherited-msg)))))
)

;;;; redefine Object>>doesNotUnderstand:
(addSelector:withMethod:
  Object
  'doesNotUnderstand:
  (lambda (self aMessageSend) ;; NB: class == MessageSend
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t "~%doesNotUnderstand: ~a ~%" aMessageSend))
;;(safer-printString aMessageSend))
;;@@DEBUG}
    (let ( (ex ($ MessageNotUnderstood 'new)) )
      ($: ex 'message: aMessageSend)
      ($: ex 'receiver: self)
      ($: ex 'reachedDefaultHandler: st-false)
      (let ( (resumeValue ($ ex 'signal)) )
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t "~%doesNotUnderstand>>signal returned: ~a " resumeValue)
  (format #t "~%doesNotUnderstand>>reachedDefaultHandler: ~a ~%"
          ($ ex 'reachedDefaultHandler)))
;;@@DEBUG}
        (if ($ ex 'reachedDefaultHandler)
            (%%escape%% (format #f
                           "~a doesNotUnderstand: ~a ~a"
                           ($ ($ aMessageSend 'receiver) 'printString)
                           ($ aMessageSend 'selector)
                           ($ aMessageSend 'arguments)))
            ;;($: aMessageSend 'sendTo: self) ;; retry
            resumeValue)))))

;;; Warning

(addSelector:withMethod:
     Warning
     'defaultAction
     (lambda (self)
       (format #f "~%Warning: ~a" ($ self 'messageText))
       ($ self 'resume))
)

;;; ASSERT

(addSelector:withMethod:
     Object
     'assert:
     (lambda (self aBlock)
; "Throw an assertion error if aBlock does not evaluates to true."
       (when (st-false? (aBlock)) ;; aBlock value == false
          ($: AssertionFailure 'signal: "Assertion failed"))))


(addSelector:withMethod:
     BlockClosure
     'assert
     (lambda (self)
       ($: self 'assert: self)))

) ) )

;;;			--- E O F ---			;;;
