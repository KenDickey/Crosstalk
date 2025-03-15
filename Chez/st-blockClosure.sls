;;; FILE: "st-blockClosure.sls"
;;; IMPLEMENTS: BlockClosure
;;; AUTHOR: Ken Dickey
;;; DATE: 16 July 2016; March 2025

(library (st-blockClosure)

  (export
   BlockClosure
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs unicode (6))
   (rnrs io simple (6))
   (rnrs exceptions (6))
   (only (chezscheme)
         vector-append)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-boolean) ;; UndefinedObject
   )


(define BlockClosure
  (newSubclassName:iVars:cVars:
   Object
   'BlockClosure '() '())
)

;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================

(perform:with: BlockClosure 'methodDict: st-blockClosure-behavior)


(addSelector:withMethod:
     BlockClosure
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'BlockClosure)
           (superPerform:with: self 'is: symbol))))

(perform:with:
     BlockClosure
     'category:
     'Kernel-Methods)

(perform:with:
     BlockClosure
     'comment:
"I am a block closure -- a bit of code which remembers the environment in which I was created.
 I can be created, saved, stored in a variable, and used without being named."
)

(addSelector:withMethod: 
 	BlockClosure
        'selector
        (lambda (self)
          (let ( (selector (method-name self)) )
            ;; Scheme returns #f if unnamed.
            ;; Smalltalk returns nil in this case.
            (if selector
                selector
                st-nil))))

(addSelector:withMethod: 
 	BlockClosure
        'argumentCount
        (lambda (self) (method-arity self)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'numArgs
        (lambda (self) (method-arity self)))


(addSelector:withMethod:  ;; alias
 	BlockClosure
        'value
        (lambda (self) (self)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'value:
        (lambda (self arg1) (self arg1)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'value:value:
        (lambda (self arg1 arg2)
          (self arg1 arg2)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'value:value:value:
        (lambda (self arg1 arg2 arg3)
          (self arg1 arg2 arg3)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'value:value:value:value:
        (lambda (self arg1 arg2 arg3 arg4)
          (self arg1 arg2 arg3 arg4)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'valueWithArguments:
        (lambda (self args-array)
          (apply self (vector->list args-array))))

(addSelector:withMethod:
 	UndefinedObject
        'valueWithArguments:
        (lambda (self args-array) st-nil))

(addSelector:withMethod:
 	BlockClosure
        'valueWithPossibleArgument:  
        (lambda (self anArg)
          (if (= 1 (method-arity self))
              (self anArg)
              (self))))

(addSelector:withMethod:
 	UndefinedObject
        'valueWithPossibleArgument:  
        (lambda (self anArg) st-nil))

(addSelector:withMethod:
 	BlockClosure
        'valueWithPossibleArgument:and:
        (lambda (self anArg anotherArg)
          (let ( (numArgs (method-arity self)) )
            (cond
             ((zero? numArgs) (self))
             ((= 1 numArgs) (self anArg))
             ((= 2 numArgs) (self anArg anotherArg))
             (else
              (error "valueWithPossibleArgument:and: bad arity" numArgs self)))))
)              

(addSelector:withMethod:
 	UndefinedObject
        'valueWithPossibleArgument:and:
        (lambda (self anArg anotherArg) st-nil))

(addSelector:withMethod:
 	BlockClosure
        'valueWithPossibleArgs:
        (lambda (self anArray)
          (let ( (numArgs (method-arity self))
                 (vecLen  (vector-length anArray))
               )
            (cond
             ((zero? numArgs) (self))
             ((= numArgs vecLen)
              (apply self (vector->list anArray)))
             ((> numArgs vecLen) ; pad with nils
              (apply self
                     (vector->list
                      (vector-append anArray
                                     (make-vector (- numArgs vecLen) st-nil))))
              )
             (else ; (< numArgs vecLen) l drop
              (apply self (vector->list (vector-copy anArray 0 numArgs)))))))
)

(addSelector:withMethod:
 	UndefinedObject
        'valueWithPossibleArgs: 
        (lambda (self anArray) st-nil))


(addSelector:withMethod:
 	BlockClosure
        'whileFalse:
        (lambda (self thunk)
          (let loop ( (value (thunk)) )
            (if (st-false? value)
                (begin
                  (thunk)
                  (loop (self))
                value)))))

(addSelector:withMethod:
 	BlockClosure
        'whileTrue:
        (lambda (self thunk)
          (let loop ( (value (self)) )
            (if (st-true? value)
                (begin
                 (thunk)
                 (loop (self)))
                value))))

(addSelector:withMethod:
 	BlockClosure
        'whileFalse
        (lambda (self)
          (let loop ( (value (self)) )
            (if (st-false? value)
                (loop (self))
                value))))

(addSelector:withMethod:
 	BlockClosure
        'whileTrue
        (lambda (self)
          (let loop ( (value (self)) )
            (if (st-true? value)
                (loop (self))
                value))))

(addSelector:withMethod:
 	BlockClosure
        'whileNil
        (lambda (self)
          (let loop ( (value (self)) )
            (if (st-nil? value)
                (loop (self))
                value))))

(addSelector:withMethod:
 	BlockClosure
        'whileNotNil
        (lambda (self)
          (let loop ( (value (self)) )
            (if (st-nil? value)
                value
                (loop (self))))))

(addSelector:withMethod:
 	BlockClosure
        'ensure:
        (lambda (self afterThunk)
          (dynamic-wind
            (lambda () #f) ;; before
            self ; thunk
            afterThunk))) ;; after: always executed

;; NB: (Scm) raise is non-continuable and a caught exception
;;   will cause a "handler returned" exception.
;; Use (Scm) raise-continuable for on:to: to enable
;; the handler to return a value.

(addSelector:withMethod:
 	BlockClosure
        'on:do:
        (lambda (self exceptionClassOrSet handler)
          (let ( (protectedBlock self)
                 (retry #f)
               )
            (call/cc
             (lambda (return-from-on:do:)

               (with-exception-handler
                
                (lambda (exceptionOrCondition)
                  (let ( (anException ($ exceptionOrCondition 'asException)) )
                    ;; Note file "st-conditions.scm"
                    (if (not ($: exceptionClassOrSet 'handles: anException))
                        ;; re-raise if not handled here
                        (raise-continuable anException)
                        (begin  ;; the exception carries the context
                          ($: anException
                              'handlerContext:
                              return-from-on:do:)
                          ($: anException
                              'retryContext:
                              retry)
                          ($: anException
                              'blockSetter:
                              (lambda (aBlock) (set! protectedBlock aBlock)))
                          (return-from-on:do:
                           (if (= 1 (method-arity handler))
                               ;; valueWithPossibleArgument:
                               (handler anException)
                               (handler)))))))
 
                (lambda ()
                  (call/cc
                   (lambda (retry-again)
                     (set! retry retry-again)))
                  (protectedBlock))
 ) ) ) ) ) )

;;    signalContext handlerContext retryContext blockSetter


(addSelector:withMethod:
	BlockClosure
        'ifCurtailed:
        (lambda (self action)
          ;;  to class based system
          (let ( (curtailed? #t)
                 (result '()) )
            (dynamic-wind
              (lambda () #f) ;; before
              (lambda ()
                (set! result (self))
                (set! curtailed? #f))
              (lambda ()
                (if curtailed? (action))))
            result)
          ))

(addSelector:withMethod:
	BlockClosure
        'ifError:
        (lambda (self errorHandlerBlock)
          ($:: self
               'on:do:
               (smalltalkAt: 'Error) ;; to be defined in st-error
               (lambda (ex)
                 ($:: errorHandlerBlock
                      'valueWithPossibleArgument:and:
                      ($ ex 'description)
                      ($ ex 'receiver)))
       ) )
)

;; "Evaluate the block represented by the receiver, and normally return it's value.
;;  If an error occurs, the errorHandlerBlock is evaluated, and it's value
;;  is instead returned.
;;  The errorHandlerBlock must accept zero, one, or two parameters
;;  (the error message and the receiver)."
;; "Examples:
;; 	[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?'].
;; 	[1 / 0] ifError: [:err :rcvr | { rcvr. err. } ]

)

;;;			--- E O F ---			;;;
