;;; FILE: "st-blockClosure.scm"
;;; IMPLEMENTS: BlockClosure
;;; AUTHOR: Ken Dickey
;;; DATE: 16 July 2016

;; (requires 'st-core-classes)

(define BlockClosure
  (newSubclassName:iVars:cVars:
   Object
   'BlockClosure '() '())
)

(set! st-blockClosure-behavior
      (perform: BlockClosure 'methodDict))

(perform:with:
     BlockClosure
     'category:
     '|Kernel-Methods|)

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
          (let ( (selector (procedure-name self)) )
            ;; Scheme returns #false if unnamed.
            ;; Smalltalk returns nil in this case.
            (if selector
                selector
                st-nil))))

(addSelector:withMethod: 
 	BlockClosure
        'argumentCount
        (lambda (self) (procedure-arity self)))

(addSelector:withMethod:  ;; alias
 	BlockClosure
        'numArgs
        (lambda (self) (procedure-arity self)))


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
            (lambda () #false) ;; before
            self ; thunk
            afterThunk))) ;; after: always executed



(addSelector:withMethod:
 	BlockClosure
        'on:do:
        (lambda (self exception action)
; NB: exception can be a single class or a set
          
          ;; @@FIXME: adapt condition hierarchy
          ;;  to class based system
          (with-exception-handler
             (lambda (condition)
               (if (conforms-to? condition exception)
                   (action)
             ) )
             self)))

(define (conforms-to? condition exceptionClassOrSet)
  (if (class? exceptionClassOrSet)
      (isKindOf: condition exceptionClassOrSet) ;; @@Fixme: error-object?
      ($: exceptionClassOrSet 'detect: (lambda (ex) (isKindOf: condition ex))))
)

(define (class? thing)
  (cond
   ((not (st-object? thing)) #false)
   (else (isKindOf: thing Class))))

(addSelector:withMethod:
 	BlockClosure
        'ifCurtailed:
        (lambda (self action)
          ;;  to class based system
          (let ( (curtailed? #true)
                 (result '()) )
            (dynamic-wind
              (lambda () #false) ;; before
              (lambda ()
                (set! result (self))
                (set! curtailed? #false))
              (lambda ()
                (if curtailed? (action))))
            result)
          ))
            

;;@@@



;; (provides st-blockClosure)

;;;			--- E O F ---			;;;
