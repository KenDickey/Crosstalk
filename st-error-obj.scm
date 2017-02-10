;;; FILE: "st-error-obj.scm"
;;; IMPLEMENTS: Scheme: ErrorObject;
;;;          Smalltalk: Exception, ExceptionSet, UnhandledError, Error
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
       (let ( (newInst ($ ($ self 'new) 'initialize)) )
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
       (let ( (newEx ($ ($ self 'new) 'initialize)) )
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
       (let ( (newEx ($ ($ self 'new) 'initialize)) )
         ($: newEx 'messageText: aMessage)
         ($ newEx 'signal))))

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
              (name ($ ($ ($ self 'class) 'name) 'asString))
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



;;; Error


;; (provide 'st-error-obj)

;;;			--- E O F ---			;;;
