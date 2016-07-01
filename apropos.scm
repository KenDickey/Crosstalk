(import (primitives interaction-environment environment-variables)
        (srfi :13) ;; string library
        (rnrs sorting)
        )

(define (apropos string-or-symbol)
  (let ( (target
            (if (symbol? string-or-symbol)
                (symbol->string string-or-symbol)
                string-or-symbol))
         (env-syms (environment-variables (interaction-environment)))
       )
    (let loop ( (results '()) (syms env-syms) )
      (cond
       ((null? syms)
        (list-sort symbol<? results))
       ((funky-symbol? (car syms))
        (loop results (cdr syms)))
       ((string-contains (symbol->string (car syms)) target)
        (loop (cons (car syms) results) (cdr syms)))
       (else (loop results (cdr syms)))))))

(define (funky-symbol? s)
  (let ( (str (symbol->string s)) )
    (and (> (string-length str) 0)
         (not (char-alphabetic? (string-ref str 0))))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))
