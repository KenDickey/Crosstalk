;;; FILE: "apropos.scm"
;;; IMPLEMENTS: apropos function for Scheme globals
;;; AUTHOR: Ken Dickey
;;; DATE: 30 June 2016

;; Run from command line via "larceny -r7rs", then
;;   (import (scheme load))
;;   (load "apropos.scm")
;;
;; (apropos 'cons)
;; (apropos "char")

(import (primitives interaction-environment environment-variables interaction-environment)
        (srfi :13) ;; string library -- for  string-contains
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
         (not (ascii-letter? (string-ref str 0))))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))

(define (ascii-letter? char)
  (if (char? char) ;; eof protect
      (let ( (charcode (char->integer char)) )
        (or (<= 97 charcode 122)  ;; a..z
            (<= 65 charcode  90))) ;; A..Z
      #false))

