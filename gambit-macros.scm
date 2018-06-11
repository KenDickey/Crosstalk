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

;; really let*-values, but used as let-values
(define-macro (let-values bindings . body)
  (if (null? bindings)
      (cons 'begin body)
      (apply (lambda (vars initializer)
               (let ( (cont (cons 'let-values (cons (cdr bindings) body))))
                 (cond
                  ((not (pair? vars))
                   `(let ((,vars ,initializer)) ,cont)
                   )
                  ((null? (cdr vars))
                   `(let ((,(car vars) ,initializer)) ,cont)
                   )
                  (else
                   `(call-with-values
                        (lambda () ,initializer)
                        (lambda ,vars ,cont))))))
               (car bindings))))
                   
