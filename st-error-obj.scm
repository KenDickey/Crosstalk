;;; FILE: "st-error-obj.scm"
;;; IMPLEMENTS: Scheme: ErrorObject;
;;;          Smalltalk: Exception, ExceptionSet, UnhandledError, Error,
;;;                     Message, MessageSend, MessageNotUnderstood.
;;; See also "st-blockClosure.scm" for exception handling code.
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017

(define ErrorObject
  (newSubclassName:iVars:cVars:
   Object
   'ErrorObject '() '()) ;; Scheme Error Object
)

(define ExceptionSet
  (newSubclassName:iVars:cVars:
   Object
   'ExceptionSet '(exceptions) '())
)

(define Exception
  (newSubclassName:iVars:cVars:
   Object
   'Exception '(messageText myTag signalContext handlerContexts outerContext) '())
)

(define Error
  (newSubclassName:iVars:cVars:
   Exception
   'Error '() '())
)

(define UnhandledError
  (newSubclassName:iVars:cVars:
   Exception
   'UnhandledError '(exception) '())
)

(define MessageNotUnderstood
  (newSubclassName:iVars:cVars:
   Error
   'MessageNotUnderstood '(message receiver reachedDefaultHandler) '())
)

(define MessageSend
    (newSubclassName:iVars:cVars:
    Object
    'MessageSend
    '(receiver selector arguments)
    '())
)

(define Halt
  (newSubclassName:iVars:cVars:
   Exception
   'Halt '() '())
)


(define AssertionFailure
  (newSubclassName:iVars:cVars:
   Halt
   'AssertionFailure '() '())
)


(set! st-messageSend-behavior ($ MessageSend 'methodDict))

(define Message
    (newSubclassName:iVars:cVars:
    Object
    'Message
    '(selector args lookupClass)
    '())
)

;;; categoriy:

(perform:with:
     ErrorObject
     'category: '|Exceptions Kernel|) ;; ?? '|Exceptions Scheme| ??

(perform:with:
     ExceptionSet
     'category: '|Exceptions Kernel|)

(perform:with:
     Exception
     'category: '|Exceptions Kernel|)

(perform:with:
     Error
     'category: '|Exceptions Kernel|)

(perform:with:
     UnhandledError
     'category: '|Exceptions Kernel|)

(perform:with:
     MessageNotUnderstood
     'category: '|Exceptions Kernel|)

(perform:with:
     Halt
     'category: '|Exceptions Extensions|)

(perform:with:
     AssertionFailure
     'category: '|Exceptions Extensions|)

(perform:with:
     MessageSend
     'category: '|Kernel-Objects|) ;; N.B.

(perform:with:
     Message
     'category: '|Kernel-Methods|) ;;N.B.


;;; comment:

(perform:with:
     ErrorObject
     'comment: "I present Scheme error objects")

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
     Error
     'comment: ;; ANSI
"This protocol describes the behavior of instances of class Error.
 These are used to represent error conditions that prevent the
 normal continuation of processing.

 Actual error exceptions used by an application may be subclasses of this class.
 As Error is explicitly specified  to be subclassable, conforming implementations
 must implement its behavior in a non-fragile manner."
)

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
     MessageNotUnderstood
     'comment:
 "This exception is provided to support Object>>doesNotUnderstand:.")
 
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

(perform:with:
     Halt
     'comment:
"Halt is provided to support Object>>halt."
)

(perform:with:
     AssertionFailure
     'comment:
"AssertionFailure is the exception signaled from Object>>assert:
 when the assertion block evaluates to false."
)


;;; Scheme: ErrorObject

(set! st-error-obj-behavior (perform: ErrorObject 'methodDict))

(addSelector:withMethod:
     ErrorObject
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ErrorObject)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     ErrorObject
     'message ;; Scheme
     (lambda (self) (error-object-message self)))

(addSelector:withMethod:
     ErrorObject
     'irritants ;; Scheme
     (lambda (self) (error-object-irritants self)))

(addSelector:withMethod:
     ErrorObject
     'isReadError
     (lambda (self) (read-error? self)))

(addSelector:withMethod:
     ErrorObject
     'isFileError
     (lambda (self) (file-error? self)))


(addSelector:withMethod:
     (class ErrorObject)
     'error:
     (lambda (self message)
       (error message)))

(addSelector:withMethod:
     (class ErrorObject)
     'error:withIrritants:
     (lambda (self message irritants)
       ;; @@FIXME: generalize
       (apply error (cons message (vector->list irritants)))))

(addSelector:withMethod: 
    ErrorObject
    'printOn:
    (lambda (self port)
      (display (error-object-message self) port)
      (for-each
       (lambda (irritant)
         (display " ") (display irritant))
       (error-object-irritants self)))
)

;; Emulate St exceptions

(addSelector:withMethod:
     (class Class)
     'handles:  ;; base case
     (lambda (self anException) st-false))

(addSelector:withMethod:
     (class ErrorObject)
     'handles:
     (lambda (self anException)
       (error-object? anException)))

(addSelector:withMethod:
     (class ErrorObject)
     'signal:
     (lambda (self message)
       (error message)))

(addSelector:withMethod:
     (class ErrorObject)
     'signal
     (lambda (self)  ;; Not very useful..
       (error "An error was signalled")))

(addSelector:withMethod:
     ErrorObject
     'isResumable
     (lambda (self) st-false))

(addSelector:withMethod:
     ErrorObject
     'description
     (lambda (self) ($ self 'message)))

(addSelector:withMethod:
     ErrorObject
     'messageText
     (lambda (self) ($ self 'message)))

(addSelector:withMethod:
     ErrorObject
     'tag
     (lambda (self) ($ self 'message)))

(addSelector:withMethod:
     (class ErrorObject)
     '|,|
     (lambda (self exceptionClass)
       ($:: ExceptionSet 'with:with: self exceptionClass)))


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


(addSelector:withMethod:
     ExceptionSet
     '|,|
     (lambda (self anExceptionClass)
       ($: self 'add: anExceptionClass)
       self))

(addSelector:withMethod:
     ExceptionSet
     'add:
     (lambda (self anExceptionClass)
       ($: self
           'exceptions:
           (vector-append ($ self exceptions) (vector anExceptionClass)))
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
     'isResumable
     (lambda (self) st-true))

(addSelector:withMethod:
     Exception
     'signal
     (lambda (self)
       ((if ($ self 'isResumable) raise-continuable raise) self)))

(addSelector:withMethod:
     (class Exception)
     'signal
     (lambda (self)
       (let ( (newEx ($ self 'new)) )
         ((if ($ newEx 'isResumable) raise-continuable raise) newEx))))

(addSelector:withMethod:
     Exception
     'signal:
     (lambda (self aMessage)
       ($: self 'messageText: aMessage)
       ($ self 'signal)))

(addSelector:withMethod:
     (class Exception)
     'signal:
     (lambda (self aMessage)
       (let ( (newEx ($ self 'new)) )
         ($: newEx 'messageText: aMessage)
         ($ newEx 'signal))))

(addSelector:withMethod:
     (class Exception)
     '|,|
     (lambda (self exceptionClass)
       ($:: ExceptionSet 'with:with: self exceptionClass)))

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
     (lambda (self) st-nil))

(addSelector:withMethod:
     Exception
     'return:
     (lambda (self aValue) aValue))

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
       ($: UnhandledError 'signalForException: self)))



;;; Error

(addSelector:withMethod:
     Error
     'defaultAction
     (lambda (self) ($ self 'noHandler)))

(addSelector:withMethod:
     Error
     'isResumable
     (lambda (self) st-false))


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
       ;; log to file or open debugger
       (error ($ self 'description) self))) ;; open Scheme debugger

(addSelector:withMethod:
     UnhandledError
     'isResumable
     (lambda (self) st-false))

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
                     (className: ($ myMessage 'receiver))
                     ($ myMessage 'selector))
             (let ( (inherited-msg (superPerform: self 'messageText)) )
               (if (null? inherited-msg)
                   (format #f
                           "~a doesNotUnderstand: <something>"
                           (safer-printString ($ self 'receiver)) )
                   inherited-msg)))))
)

;; redefine
(addSelector:withMethod:
  Object
  'doesNotUnderstand:
  (lambda (self aMessageSend) ;; NB: class == MessageSend
;;@@DEBUG{
; (format #t "~%doesNotUnderstand: ~a ~%" (safer-printString aMessageSend))
;;@@DEBUG}
    (let ( (ex ($ MessageNotUnderstood 'new)) )
      ($: ex 'message: aMessageSend)
      ($: ex 'receiver: self)
      ($: ex 'reachedDefaultHandler: st-false)
      (let ( (resumeValue ($ ex 'signal)) )
;;@@DEBUG{
; (format #t "~%doesNotUnderstand>>signal returned: ~a ~%" resumeValue)
;;@@DEBUG}
        (if ($ ex 'reachedDefaultHandler)
            ($: aMessageSend 'sendTo: self)
            resumeValue)))))


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
       ($: aStream 'nextPutAll: (className: self))
       ($: aStream 'nextPutAll: "( " )
       ($: ($ self 'selector) 'printOn: aStream)
       ($: aStream 'nextPutAll: " -> ")
       (if (isKindOf: ($ self 'receiver) MessageSend)
           ($: aStream 'nextPutAll: "a MessageSend") ;; avoid recursion
           ($: ($ self 'receiver) 'printOn: aStream))
       ($: aStream 'nextPutAll: " )" ))
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

;; (addSelector:withMethod:
;;      TestCase
;;      'assert:description:resumable:
;;      (lambda (self aBoolean aString resumableBoolean)
;;        (when (st-false? aBoolean)
;;          ($: self failureString: aString)
;;          ($: self logFailure: aString)
;;          ($: self exception: 
;;                  (if (st-true? resumableBoolean)
;;                      ($ TestResult 'resumableFailure)
;;                      ($ TestResult 'failure)))
    
;;          ($: ($ self exception)
;;              'signalWith:
;;              aString))))


;;; send-failed

(define in-send-failed? (make-parameter #false))

(set! send-failed ;; def'ed in "st-kernel.scm"
  (lambda (receiver selector rest-args)
    ;;@@DEBUG{
;    (format #t "~%send failed ~a >> ~a ~a~%"
;            receiver selector (list->vector rest-args))
    ;;@@DEBUG}
    (cond
     ((in-send-failed?)
      (format #f "send-failed recursion: ~a >> ~a" (className: receiver) selector)
     )
    ((unspecified? receiver)
     (%%escape%% (format #f "Unspecified reciever for #~a" selector))
     )
    (else
     (parameterize ( (in-send-failed? #true) )
      ($: receiver
          'doesNotUnderstand:
          ($::: MessageSend
                'receiver:selector:arguments:
                receiver
                selector
                (list->vector rest-args)))
     ))
  ) )
)


(define (unspecified? thing)
  (eq? thing #!unspecified))



;; (provide 'st-error-obj)

;;;			--- E O F ---			;;;
