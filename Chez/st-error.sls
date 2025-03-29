;;; FILE: "st-error.sls"
;;; IMPLEMENTS: Error, Notification, Halt,
;;;		UnhandledError, IllegalResumeAttempt
;;; See also "st-exception.sls"
;;; See also "st-blockClosure.sls" for exception handling code.
;;; See "st-conditions.sls" for #asException
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017; March 2025


(library (st-error)

  (export
   init-st-error

   Error
   Notification
   Halt
   IllegalResumeAttempt
   UnhandledError
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
   )



(define Error
  (newSubclassName:iVars:cVars:
   Exception
   'Error '() '())
)

(define Notification
  (newSubclassName:iVars:cVars:
   Exception
   'Notification '() '())
)

(define Halt
  (newSubclassName:iVars:cVars:
   Exception
   'Halt '() '())
)

(define UnhandledError
  (newSubclassName:iVars:cVars:
   Exception
   'UnhandledError '(exception) '())
)

(define IllegalResumeAttempt ;; internal/private
  (newSubclassName:iVars:cVars:
   Exception
   'IllegalResumeAttempt '() '())
)

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-error)
  (unless (initialized?)
    (initialized? #t)

    (init-st-exception)
  
(let ( (exn-mdict ($ Exception 'methodDict)) )
  (perform:with: Error
                 'methodDict:
                 (behavior-add-from-other
                  ($ Error 'methodDict)
                  exn-mdict))
  (perform:with: Notification
                 'methodDict:
                 (behavior-add-from-other
                  ($ Notification 'methodDict)
                  exn-mdict))
  (perform:with: Halt
                 'methodDict:
                 (behavior-add-from-other
                  ($ Halt 'methodDict)
                  exn-mdict))
  (perform:with: UnhandledError
                 'methodDict:
                 (behavior-add-from-other
                  ($ UnhandledError 'methodDict)
                  exn-mdict))
  (perform:with: IllegalResumeAttempt
                 'methodDict:
                 (behavior-add-from-other
                  ($ IllegalResumeAttempt 'methodDict)
                  exn-mdict))
)


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


) ) )

;;;			--- E O F ---			;;;
