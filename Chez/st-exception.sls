;;; FILE: "st-exception.sls"
;;; IMPLEMENTS: Exception, ExceptionSet, MessageSend
;;; See also "st-errors.sls" and "st-exceptions.sls"
;;; See also "st-blockClosure.sls" for exception handling code.
;;; See "st-conditions.sls" for #asException
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017; March 2025


(library (st-exception)

  (export
   init-st-exception

   Exception
   ExceptionSet
   MessageSend
   Message

   in-send-failed?
   %%escape%%
   debug-st-runtime
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs io simple (6))
   (rnrs hashtables (6))
   (rnrs exceptions (6))
   (rnrs conditions (6))
   (rnrs arithmetic bitwise (6))
   (only (chezscheme)
         vector-append
         make-parameter
         make-thread-parameter
         parameterize)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-boolean) ;; UndefinedObject
   (st-blockClosure)
   )


(define ExceptionSet
  (newSubclassName:iVars:cVars:
   Object
   'ExceptionSet '(exceptions) '())
)

(define Exception
  (newSubclassName:iVars:cVars:
   Object
   'Exception
   '(receiver messageText myTag
     signalContext resignalContext signalExn
     handlerContext retryContext blockSetter
     conditionDict)
   '())
)

(define MessageSend
    (newSubclassName:iVars:cVars:
    Object
    'MessageSend
    '(receiver selector arguments)
    '())
)

(define Message
    (newSubclassName:iVars:cVars:
    Object
    'Message
    '(selector args lookupClass)
    '())
)

(define in-send-failed? (make-parameter #f))

(define %%escape%%
  (make-thread-parameter (lambda whatever '%%escape%%)))

(define debug-st-runtime (make-parameter #t))

(define (saferIsKindOf: self someClass)
  (let loop ( (super-class (perform: self 'class)) )
    (cond
     ((null? super-class) #f)
     ((eq? super-class someClass) #t)
     ((not (st-object? super-class)) #f)
     (else (loop (perform: super-class 'superclass))))
) )

(define (class? thing)
  (cond
   ((not (st-object? thing)) #f)
   (else (saferIsKindOf: thing Class))))

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-exception)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-blockClosure)  

(rebase-mdict! MessageSend st-messageSend-behavior)

(primAppendLocalSelectors: MessageSend   ;; early bound
               '(value valueWithArguments:))

(perform:with: Message      'methodDict:
               (behavior-add-from-other
                ($ Message 'methodDict)
                st-object-behavior))

(perform:with: ExceptionSet 'methodDict:
               (behavior-add-from-other
                ($ ExceptionSet 'methodDict)
                st-object-behavior))

(perform:with: Exception    'methodDict:
               (behavior-add-from-other
                ($ Exception 'methodDict)
                st-object-behavior))


; receiver -- nil or the receiver
; signalContext  -- Continuation captured at point of #signal 
; handlerContext -- Continuation captured at handler invocation in #on:do:
;                   See "st-blockClosure.sls"
; conditionDict -- nil or an IdentityDictionary/eqHashtable of condition values

;;; Exception>>doesNotUnderstand:
; Allow conditionDict identifiers as selectors to Exceptions
(addSelector:withMethod:
     Exception
     'doesNotUnderstand:
     (lambda (self aMessageSend)
       (let (  (condDict ($ self 'conditionDict))
               (selector ($ aMessageSend 'selector))
              )
         (if (and (hashtable? condDict)
                  (hashtable-contains? condDict selector))
             (hashtable-ref condDict selector st-nil)
             (%:  self 'doesNotUnderstand: aMessageSend)))) ;; super send
)


(perform:with:
     ExceptionSet
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     Exception
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     MessageSend
     'category: (string->symbol "Kernel-Objects")) ;; N.B.

(perform:with:
     Message
     'category: (string->symbol "Kernel-Methods")) ;;N.B.


;;; comment:

(perform:with:
     ExceptionSet
     'comment:
"An ExceptionSet is a grouping of exception handlers
 which acts as a guard for a single handler.
 Within the group, the most recently added handler
 will be the last handler found during a handler search
 (in the case where more than one handler in the group
 is capable of handling a given exception). ")

(perform:with:
     Exception
     'comment:
"This is the main class used to implement the exception handling system (EHS).
 It plays two distinct roles:  that of the exception, and that of the exception handler.
 More specifically, it implements the bulk of the protocols laid out in the ANSI
 specification - those protocol names are reflected in the message categories.

 Exception is an abstract class.  Instances should neither be created nor trapped.
 In most cases, subclasses should inherit from Error or Notification rather than
 directly from Exception.")

 
(perform:with:
     MessageSend
     'comment:
"Instances of MessageSend encapsulate message sends to objects.
 Arguments can be either predefined or supplied when the
 message send is performed.
 MessageSends are used to implement the #when:send:to: event system.

 Use #value to perform a message send with its predefined arguments
 and #valueWithArguments: if additonal arguments have to supplied.

Structure:
 receiver		Object -- object receiving the message send
 selector		Symbol -- message selector
 arguments		Array -- bound arguments"
)

(perform:with:
     Message
     'comment:
"I represent a selector and its argument values.
	
 Generally, the system does not use instances of Message for efficiency reasons.
 However, when a message is not understood by its receiver, the runtime system
 will make up an instance of me in order to capture the information
 involved in an actual message transmission.
 This instance is sent it as an argument with the message doesNotUnderstand:
 to the receiver."
)



(addSelector:withMethod:
     (class Class)
     'handles:  ;; base case
     (lambda (self anException) st-false))

(addSelector:withMethod:
     Object
     'handles:  ;; base case
     (lambda (self anException) st-false))



;;; ExceptionSet


(addSelector:withMethod:
     (class ExceptionSet)
     'with:with:
     (lambda (self exClass1 exClass2)
       (let ( (newInst ($ self 'new)) )
         ($: newInst 'exceptions: (vector exClass1 exClass2))
         newInst)))

(addSelector:withMethod:
     ExceptionSet
     'initialize
     (lambda (self)
       ($: self 'exceptions: '#())
       self))


(addSelector:withMethod:arity:
     ExceptionSet
     (string->symbol ",")
     (lambda (self anExceptionClass)
       ($: self 'add: anExceptionClass)
       self)
     2)

(addSelector:withMethod:
     ExceptionSet
     'add:
     (lambda (self anExceptionClass)
       ($: self
           'exceptions:
           (vector-append ($ self 'exceptions)
                          (vector anExceptionClass)))
       self))

(addSelector:withMethod:
     ExceptionSet
     'handles:
     (lambda (self anException)
       ($: ($ self 'exceptions)
           'detect:
           (lambda (ec) ($: ec 'handles: anException)))))

(addSelector:withMethod:
     ExceptionSet
     'printOn:
     (lambda (self aStream)
       ($: aStream 'nextPutAll: (className: self))
       ($: aStream 'nextPutAll: "( " )
       ($: ($ self 'exceptions)
           'do:
           (lambda (elt) (format aStream "[~a] " (className: elt))))
       ($: aStream 'nextPut: #\) ))
)

;;; Exception

(addSelector:withMethod:
     (class Exception)
     'handles:
     (lambda (self anException)
       (isKindOf: anException self)))

(addSelector:withMethod:
     Exception
     'asException  
     (lambda (self) self))
;; See BlockClosure>>on:do: in "st-blockClosure.scm"

(addSelector:withMethod:
     Object
     'asException
     (lambda (self)
       (let ( (ex ($ (smalltalkAt: 'Error) 'new)) )
         ($: ex 'receiver: self)
         ($: ex 'messageText: "Huh?: non-exception object #asException")
         ex)))

(addSelector:withMethod:
     Exception
     'isResumable
     (lambda (self) st-true))

(addSelector:withMethod:
     Exception
     'resume:
     (lambda (self resumptionValue)
;;   "Return resumptionValue as the value of the signal message."
       (when (not ($ self 'isResumable))
           ($ (smalltalkAt: 'IllegalResumeAttempt) 'signal)
	($ self 'resumeUnchecked: resumptionValue))))

(addSelector:withMethod:
     Exception
     'resumeUnchecked:
     (lambda (self resumptionValue)
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t
          "~%~a>>resumeUnchecked ~a"
          ($ self 'printString)
          resumptionValue))
;;@@DEBUG}
       (($ self 'signalContext) resumptionValue)))


(addSelector:withMethod:
     Exception
     'resignalAs:
     (lambda (self newException)
       (let ( (newEx
               (if (class? newException)
                   ($ newException 'new)
                   newException))
            )
;;@@@DEBUG{
;;(display-ivars self)
;;@@@DEBUG}
         ($: newEx 'handlerContext:  ($ self 'handlerContext))
         ($: newEx 'signalContext:   ($ self 'signalContext))
         ($: newEx 'resignalContext: ($ self 'resignalContext))
;;       ($: newEx 'conditionDict:  ($ self 'conditionDict))
;;       ($: newEx 'messageText:    ($ self 'messageText))
;;@@@DEBUG{
;;(display-ivars newEx)
;;@@@DEBUG}
         ($: self 'signalExn: newEx)
         ( ($  self 'resignalContext) 'resignalAs )
     ) ) )


(addSelector:withMethod:
     Exception
     'signal
     (lambda (self)
       ($: self 'signalExn: self)
       (call/cc
        (lambda (resignal)
          ($: self 'resignalContext: resignal)))
        (call/cc
         (lambda (returnToSignal)
           ($: ($ self 'signalExn) 'signalContext: returnToSignal)
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t "~a" ($ ($ self 'signalExn) 'printString)))
;;@@DEBUG}
             (raise-continuable ($ self 'signalExn))
	)))
)

(addSelector:withMethod:
     Exception
     'signal:
     (lambda (self aMessage)
       ($: self 'messageText: aMessage)
       ($  self 'signal)))

(addSelector:withMethod:
     (class Exception)
     'signal
     (lambda (self)
       ($ ($ self 'new) 'signal)))

(addSelector:withMethod:
     (class Exception)
     'signal:
     (lambda (self aMessage)
       ($: ($ self 'new) 'signal: aMessage)))

(addSelector:withMethod:arity:
     (class Exception)
     (string->symbol ",")
     (lambda (self exceptionClass)
       ($:: ExceptionSet 'with:with: self exceptionClass))
     2)

(addSelector:withMethod:
     Exception
     'tag:
     (lambda (self aMessage)
       ($: self 'myTag: aMessage)
       self))

(addSelector:withMethod:
     Exception
     'tag
     (lambda (self)
       (let ( (tag ($ self 'myTag)) )
         (if (not (st-nil? tag))
             tag
             ($: self 'messageText)))))

(addSelector:withMethod:
     Exception
     'description
     (lambda (self)
       (let ( (msg ($ self 'messageText))
              (name ($ (className: self) 'asString))
            )
         (if (st-nil? msg)
             name
             (string-append name ": " msg)))))

(addSelector:withMethod:
     Exception
     'defaultAction
     (lambda (self)
       (make-subclassResponsibility 'defaultAction)))

(addSelector:withMethod:
     Exception
     'defaultResumeValue
     (lambda (self) st-nil))

(addSelector:withMethod:
     Exception
     'return
     (lambda (self)
       ( ($ self 'handlerContext) st-nil )))

(addSelector:withMethod:
     Exception
     'return:
     (lambda (self aValue)
       ( ($ self 'handlerContext) aValue )))

(addSelector:withMethod:
     Exception
     'resume
     (lambda (self)
       ( ($ self 'signalContext) st-nil )))

(addSelector:withMethod:
     Exception
     'resume:
     (lambda (self aValue)
       ( ($ self 'signalContext) aValue )))

(addSelector:withMethod:
     Exception
     'retry
     (lambda (self)
       ( ($ self 'retryContext) 'retry ) )) ;; arg ignored

(addSelector:withMethod:
     Exception
     'retryUsing:
     (lambda (self newProtectedBlock)
       ( ($ self 'blockSetter) newProtectedBlock )
       ( ($ self 'retryContext) 'retryUsing: ))) ;; arg ignored

(addSelector:withMethod:
     Exception
     'pass   ;; re-raise
     (lambda (self) 
       (raise-continuable self)))

(addSelector:withMethod:
     Exception
     'outer   ;; re-raise, but come back here
     (lambda (self)
       (let ( (oldSignalContext ($ self 'signalContext)) )
         (call/cc
          (lambda (return-here)
            ($: self 'signalContext: return-here)
            (raise-continuable self)
            ($: self 'signalContext: oldSignalContext)))))
)


(addSelector:withMethod:
     Exception
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Exception)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Exception
     '=
     (lambda (self other)
       (and
        (eq?  ($ self 'species )  ($ other 'species))
        (eq?  ($ self 'receiver)  ($ other 'receiver))
        (eq?  ($ self 'selector)  ($ other 'selector))
        (eqv? ($ self 'arguments) ($ other 'arguments)))))

(addSelector:withMethod:
     Exception
     'hash
     (lambda (self)
       (bitwise-xor ($ ($ self 'receiver) 'hash)
                    ($ ($ self 'selector) 'hash))))

(addSelector:withMethod:
     Exception
     'noHandler
;; "No one has handled this error, but now give them a chance
;; to decide how to debug it.
;; If none handle this either then open debugger (see UnhandedError-defaultAction)"
     (lambda (self)
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t "~%Exception>>noHandler for ~a~%" ($ self 'description)))
;;@@DEBUG}
       ($: (smalltalkAt: 'UnhandledError) 'signalForException: self)))




;;; MessageSend

(addSelector:withMethod:
     (class MessageSend)
     'receiver:selector:arguments:
     (lambda (self rcvr aSymbol anArray)
       (let ( (newInst ($ self 'new)) )
         ($: newInst 'receiver:  rcvr)
         ($: newInst 'selector:  aSymbol)
         ($: newInst 'arguments: anArray)
         newInst)))

(addSelector:withMethod:
     (class MessageSend)
     'receiver:selector:argument:
     (lambda (self rcvr aSymbol anArg)
       ($::: self
             'receiver:selector:arguments:
             rcvr
             aSymbol
             (vector anArg))))

(addSelector:withMethod:
     (class MessageSend)
     'receiver:selector:
     (lambda (self rcvr aSymbol)
       ($::: self
             'receiver:selector:arguments:
             rcvr
             aSymbol
             '#())))

(addSelector:withMethod:
     MessageSend
     'collectArguments: ;; PRIVATE
     ;; Answer Array with first N args substituted from new
     (lambda (self newArgArray)
       (let* ( (origArgArray ($ self 'arguments))
               (origArgsLen (vector-length origArgArray))
               (newArgsLen  (vector-length newArgArray))
             )
         (cond
          ((= origArgsLen newArgsLen)
           newArgArray
           )
          ((> origArgsLen newArgsLen) ;; drop
           (vector-copy newArgArray 0 origArgsLen))
          (else ;; (> origArgsLen newArgsLen)
           (vector-append newArgArray ;; augment
                          (vector-copy origArgArray
                                       newArgsLen
                                       origArgsLen)))))))

(addSelector:withMethod:
     MessageSend
     'resend
     (lambda (self)
       ($:: ($ self 'recevier)
            'perform:withArguments:
            ($ self 'selector)
            ($ self 'args))))

(addSelector:withMethod:
     MessageSend
     'sendTo:
     (lambda (self newReceiver)
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t
          "~%** ~a sentTo: ~a"
          (safer-printString self)
          (safer-printString newReceiver)))
;;@@DEBUG}
       ($:: newReceiver 
            'perform:withArguments:
            ($ self 'selector)
            ($ self 'args)))
)

(addSelector:withMethod:
     MessageSend
     'value  ;; resend
     (lambda (self)
       (if (zero? (vector-length ($ self 'arguments)))
           (perform: ($ self 'receiver) ($ self 'selector))
           (perform:withArguments:
              ($ self 'recevier)
              ($ self 'selector)
              ($: self 'collectArguments: ($ self 'arguments))))))

(addSelector:withMethod:
     MessageSend
     'value:withArguments: ;; resend with new arguments
     (lambda (self anArray)
       (perform:withArguments:
          ($ self 'recevier)
          ($ self 'selector)
          ($: self 'collectArguments: anArray))))


(addSelector:withMethod:
     MessageSend
     'printOn:
     (lambda (self aStream)
       (format aStream
               "~%~a( ~a -> ~a )"
               (className: self)
               ($ self 'selector)
               (if (isKindOf: ($ self 'receiver) MessageSend)
                   "a MessageSend" ;; avoid recursion
                   ($ ($ self 'receiver) 'printString))))
)

(addSelector:withMethod:
     MessageSend
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'MessageSend)
           (superPerform:with: self 'is: symbol))))



;;; Message


(addSelector:withMethod:
     (class Message)
     'selector:arguments:
     (lambda (self selector argsArray)
       (let ( (newInst ($ self 'new)) )
         ($: newInst 'selector: selector)
         ($: newInst 'args: argsArray)
         newInst)))

(addSelector:withMethod:
     (class Message)
     'lookupClass:selector:arguments:
     (lambda (self aClass aSelector argsArray)
       (let ( (newInst ($ self 'new)) )
         ($: newInst 'lookupClass: aClass)
         ($: newInst 'selector: aSelector)
         ($: newInst 'args: argsArray)
         newInst)))

(addSelector:withMethod:
     Message
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Message)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Message
     'sendTo:
     (lambda (self receiver)
; "answer the result of sending this message to receiver"
       (let ( (someClass ($ self 'lookupClass)) )
         (if (null? someClass)
             ($:: receiver
                  'perform:withArguments:
                  ($ self 'args))
             ($::: ($ self receiver)
                   'perform:withArguments:inSuperclass:
                   ($ self 'args)
                   someClass))))
)



;;; send-failed


(addSelector:withMethod:
     Message
     'sendFailed 
     (lambda (self) ;; self knows rcvr, selector, rest-args
       (let ( (rcvr ($ self 'receiver)) )
         (let ( (receiver 
                 (if (condition? rcvr)
                     ($ rcvr 'asException)
                     rcvr))
                (selector ($ self 'selector))
                (arguments ($ self 'arguments))
              )
;;@@DEBUG{
(when (debug-st-runtime)
  (format #t
          "~%send failed: ~a >> ~a ~a~%"
          receiver selector arguments))
;;@@DEBUG}
    (cond
     ((in-send-failed?)
      (let ( (msg 
              (format #f
                      "~%send-failed recursion: ~a >> ~a"
                      ($ receiver 'printString)
                      selector))
           )
      (error msg receiver selector arguments)
      (%%escape%% msg)
     ))
    ((unspecified? receiver)
     (%%escape%% (format #f "~%Unspecified reciever for #~a" selector))
     )
    (else
     (parameterize ( (in-send-failed? #t) )
      ($: receiver
          'doesNotUnderstand:
          ($::: MessageSend
                'receiver:selector:arguments:
                receiver
                selector
                arguments))
     ))
  ) )
) ) )


'st-exception
) )

)

;;;			--- E O F ---			;;;
