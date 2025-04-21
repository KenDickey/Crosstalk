#!r6rs
;;; File: "st-error.ss"
;;; IMPLEMENTS: Exception, ExceptionSet, MessageSend
;;;		Error, Notification, Halt, Condition
;;;		UnhandledError, IllegalResumeAttempt
;;;		Warning, MessageNotUnderstood,
;;;		ArithmeticError, AssertionFailure
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025


(define ExceptionSet
  (newSubclassName:iVars:cVars:
   Object
   'ExceptionSet '(exceptions) st-nil)
)

(define Exception
  (newSubclassName:iVars:cVars:
   Object
   'Exception
   '(receiver messageText myTag
     signalContext resignalContext signalExn
     handlerContext retryContext blockSetter
     conditionDict)
   st-nil))

(define Condition
  (newSubclassName:iVars:cVars:
   Object
   'Condition st-nil st-nil))
(rebase-mdict! Condition st-condition-behavior)

(define MessageSend
    (newSubclassName:iVars:cVars:
    Object
    'MessageSend
    '(receiver selector arguments)
    st-nil))
(rebase-mdict! MessageSend st-messageSend-behavior)

(define Message
    (newSubclassName:iVars:cVars:
    Object
    'Message
    '(selector args lookupClass)
    st-nil))

(define Error
  (newSubclassName:iVars:cVars:
   Exception
   'Error st-nil st-nil))

(define Notification
  (newSubclassName:iVars:cVars:
   Exception
   'Notification st-nil st-nil))

(define Halt
  (newSubclassName:iVars:cVars:
   Exception
   'Halt st-nil st-nil))

(define UnhandledError
  (newSubclassName:iVars:cVars:
   Exception
   'UnhandledError '(exception) st-nil))

(define IllegalResumeAttempt ;; internal/private
  (newSubclassName:iVars:cVars:
   Exception
   'IllegalResumeAttempt st-nil st-nil))

(define Warning
  (newSubclassName:iVars:cVars:
   Notification
   'Warning st-nil st-nil))

(define MessageNotUnderstood
  (newSubclassName:iVars:cVars:
   Error
   'MessageNotUnderstood
   '(message reachedDefaultHandler)
   st-nil))

(define ArithmeticError
  (newSubclassName:iVars:cVars:
   Error
   'ArithmeticError st-nil st-nil))

(define AssertionFailure
  (newSubclassName:iVars:cVars:
   Halt
   'AssertionFailure st-nil st-nil))

;;;======================================================


(define in-send-failed? (make-parameter #f))

(define %%escape%%
  (make-thread-parameter (lambda whatever '%%escape%%)))

(define debug-st-runtime (make-parameter #t)) ;; @@DEBUG

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


(perform:with:
     Error
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     Notification
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     Halt
     'category: (string->symbol "Exceptions Extensions"))

(perform:with:
     UnhandledError
     'category: (string->symbol "Exceptions Kernel"))

(perform:with:
     IllegalResumeAttempt
     'category: (string->symbol "Exceptions Kernel"))


(perform:with:
     Error
     'comment: ;; ANSI
"This protocol describes the behavior of instances of class Error.
 These are used to represent error conditions that prevent the
 normal continuation of processing.

 Actual error exceptions used by an application may be subclasses of this class.
 As Error is explicitly specified to be subclassable, conforming implementations
 must implement its behavior in a non-fragile manner."
)

(perform:with:
     Notification
     'comment:
"A Notification is an indication that something interesting has occurred.
 If it is not handled, it will pass by without effect.")

(perform:with:
     UnhandledError
     'comment:
"This exception represents an error exception that has gone unhandled.
 Unhandled error conditions are fundamentally different from error exceptions,
 because error exceptions may have default handlers that address the error
 condition (by e.g. retrying the operation).
 The job of unhandled errors is to report the original problem.
 This reporting can be done in a variety of ways.
 For example, in everyday practice, unhandled errors open the debugger.

 Note the common practice of \"catching all errors\" with code such as this:

	[some code]
		on: Error
		do: [:ex | ex return]
		
 is doubly problematic.

 First, there is no specificity to the expected exceptions arising from the protected block.
 Second, the handler block will prevent the exception's default handler from running,
 which may resolve or otherwise react to the error condition.

 If one really wants to catch unhandled errors, the code should read like this instead:

	[some code]
		on: UnhandledError
		do: [:ex | ex return]"
)


(perform:with:
     Halt
     'comment:
"Halt is provided to support Object>>halt."
)

(perform:with:
      IllegalResumeAttempt
      'comment:
"This class is private to the exception implementation.
 An instance of it is signaled whenever an attempt is made
 to resume from an exception which answers false to #isResumable.")




;;; Error

(addSelector:withMethod:
     Error
     'defaultAction
     (lambda (self)
;;@@@DEBUG{
(when (debug-st-runtime)
  (format #t "Error defaultAction: noHandler: ~a"
          ($ self 'printString)))
;;@@@DEBUG}
       ($ self 'noHandler)))

(addSelector:withMethod:
     Error
     'isResumable
     (lambda (self) st-false))


;;; Notification

(addSelector:withMethod:
     Notification
     'defaultAction
;; "No action is taken. The value nil is returned as the value
;; of the message that signaled the exception."
     (lambda (self)
       ($: self 'resume: st-nil)))

(addSelector:withMethod:
     Notification
     'isResumable
     (lambda (self) st-true))

;;; UnhandledError

(addSelector:withMethod:
     (class UnhandledError)
     'signalForException:
     (lambda (self anException)
       ($ ($: ($ self 'new) 'exception:  anException) 'signal)))

(addSelector:withMethod:
     UnhandledError
     'defaultAction
     (lambda (self)
;;(when (debug-st-runtime)
       (format #t "~%UnhandledError>>defaultAction ~a ~%" self)
       (format #t "~%@@FIXME: Log Backtrace OR Open a Debugger @@@")
       ;; log to file or open debugger
       (%%escape%% ($ self 'description)))) ;; open Scheme debugger

(addSelector:withMethod:
     UnhandledError
     'isResumable
     (lambda (self) st-false))


;;; HALT

(addSelector:withMethod:
     Halt
     'isResumable
     (lambda (self) st-true))

(addSelector:withMethod:
     Halt
     'defaultAction
     (lambda (self) ($ self 'noHandler)))

(addSelector:withMethod:
     Object
     'halt
     (lambda (self)
       ($ Halt 'signal)))

(addSelector:withMethod:
     Object
     'halt:
     (lambda (self aMessage)
       ($: Halt 'signal: aMessage)))


;;; IllegalResumeAttempt

(addSelector:withMethod:
      IllegalResumeAttempt
      'defaultAction
      (lambda (self) ($ self 'noHandler)))

(addSelector:withMethod:
      IllegalResumeAttempt
      'isResumable
      (lambda (self) st-false))

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

;;; Conditions

(addSelector:withMethod:
     Condition
     'asException  
     (lambda (self) (asException self)))

;; (primAddSelector:withMethod:
;;      st-condition-behavior
;;      'asException  ;; def'ed in "st-condition.scm"
;;      (lambda (self)
;;        (asException self)))


;; &condition condition simple-conditions condition?
;; condition-predicate condition-accessor

;; &message make-message-condition message-condition? condition-message

;; &warning make-warning warning?

;; &serious make-serious-condition serious-condition?

;; &error make-error error?

;; &violation make-violation violation?

;; &assertion make-assertion-violation assertion-violation?

;; &irritants make-irritants-condition irritants-condition? condition-irritants

;; &who make-who-condition who-condition? condition-who

;; &non-continuable make-non-continuable-violation non-continuable-violation?

;; &implementation-restriction make-implementation-restriction-violation
;; implementation-restriction-violation?

;; &lexical make-lexical-violation lexical-violation?

;; &syntax make-syntax-violation syntax-violation?
;; syntax-violation-form syntax-violation-subform

;; &undefined make-undefined-violation undefined-violation?

;; &i/o make-i/o-error i/o-error?

;; &i/o-read make-i/o-read-error i/o-read-error?

;; &i/o-write make-i/o-write-error i/o-write-error?

;; &i/o-invalid-position make-i/o-invalid-position-error
;; i/o-invalid-position-error? i/o-error-position

;; &i/o-filename make-i/o-filename-error i/o-filename-error?
;; i/o-error-filename

;; &i/o-file-protection make-i/o-file-protection-error
;; i/o-file-protection-error?

;; &i/o-file-is-read-only make-i/o-file-is-read-only-error
;; i/o-file-is-read-only-error?

;; &i/o-file-already-exists make-i/o-file-already-exists-error
;; i/o-file-already-exists-error?

;; &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
;; i/o-file-does-not-exist-error?

;; &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

;; &i/o-decoding make-i/o-decoding-error i/o-decoding-error?

;; &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
;; i/o-encoding-error-char

;; &no-infinities make-no-infinities-violation no-infinities-violation?

;; &no-nans make-no-nans-violation no-nans-violation?)



;|============================================================|
;; Conditions don't carry slots for st Exception values.
;; Idea: use a compound condition with added st-condition
;; Idea: 'asException -> transduce into St object as required
;; ?? keep condition (for Scm) but add St Exception context ??
;; ?? Use a compound condition to add Exception's extra ivars?

;; (define-condition-type &st &condition
;;   make-st-condition st-condition?
;;   (obj st-obj))

;; (st-obj (make-st-condition (vector 1 2 3))) ;; --> #(1 2 3)

;; (st-obj
;;     (condition
;;        zero-divide
;;        cfrob
;;        (make-st-condition (vector 1 2 3)))) ;; --> #(1 2 3)

;; (condition-message
;;     (condition
;;        zero-divide
;;        (make-st-condition (vector 1 2 3)))) ;; --> "/: zero divisor: 3 0 \n"


'st-error

;;;			--- E O F ---			;;;
