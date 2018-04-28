;; Gambit macros

(define-macro (unless test . body)
  `(if ,test #f (begin ,@body)))

(define-macro (when test . body)
  `(if ,test (begin ,@body)))

(define-macro (import . ignored) #f) ;; ignore R7RS imports

(define-macro (capture-condition form) ;; used in st-conditions-tests.scm
  `(call/cc
    (lambda (exit)
      (with-exception-handler
       (lambda (c) (exit c))
       (lambda ()  ,form))))
)

(define-macro (vector-for-each proc . vecs)
  `(apply for-each (cons ,proc (map vector->list (list ,@vecs)))))
