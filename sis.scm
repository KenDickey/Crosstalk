;;; FILE: "sis.scm"
;;; IMPLEMENTS: Smalltalk in Scheme -- kernel ST bootstrap
;;; AUTHOR: Ken Dickey
;;; DATE: 14 May 2016

; Implementation Language/platform: Larceny R7RS Scheme
;   http://larcenists.org/
;   https://github.com/larcenists/larceny

; ANSI Smalltalk
;   https://en.wikipedia.org/wiki/Smalltalk
;   http://object-arts.com/downloads/papers/AnsiSmalltalkDraft1-9.pdf

;; Run from command line via "larceny -r7rs", then
;;   (import (scheme load))
;;   (load "sis.scm")
;;   (load-source-bootstrap)
;; Optional:
;;   (load "sis-tests.scm")
;;   (run-source-tests)

(import
    (rnrs hashtables)
    (rnrs sorting)
    (rnrs io ports)
    (rnrs io simple)
    (rnrs files)
    (rnrs syntax-case)
    (scheme char)
    (scheme inexact)
    (scheme complex)
    (scheme time)
    (primitives
       vector-like-ref
       load compile-file
       procedure-name
       procedure-name-set!
       procedure-arity
       port-position
       port-has-set-port-position!?
       ratnum?
       ;; r5rs:require
       ;; apropos
       )
)

;; (r5rs:require 'apropos) ;; NB: need both IMPORT and REQUIRE.

(load "apropos.scm")
(load "define-structure.scm")

;; Helpers

(define (every? proc? list)
  (if (null? list)
      #t
      (and (proc? (car list))
           (every? proc? (cdr list)))))

(define (any? proc? list)
  (if (null? list)
      #f
      (or (proc? (car list))
          (any? proc? (cdr list)))))

;;; R7RS bytevector accessors named differently

(define bytevector-ref  bytevector-u8-ref)
(define bytevector-set! bytevector-u8-set!)

(define (list->bytevector list-of-bytes)
  (unless (list? list-of-bytes)
    (error "list->bytevector requires a list of bytes"
           list-of-bytes))
  (unless (every? (lambda (b) (<= 0 b 256)) list-of-bytes)
    (error "list->bytevector requires a list of bytes"
           list-of-bytes))
  (let* ( (bvec-len (length list-of-bytes))
          (bvec (make-bytevector bvec-len 0))
        )
    (let loop ( (index 0) (byte-list list-of-bytes) )
      (if (>= index bvec-len)
          bvec
          (begin
            (bytevector-set! bvec index (car byte-list))
            (loop (+ index 1) (cdr byte-list))))))
)

(define port.iodata 7)    ; ouch

(define (string-output-port? port)
  (and (output-port? port)
       (let ((d (vector-like-ref port port.iodata)))
         (and (vector? d)
              (> (vector-length d) 0)
              (eq? (vector-ref d 0) 'string-output-port)))))

;;;

(define st-root-directory-prefix "/home/kend/SiS/")

(define st-bootstrap-files
  '( "st-kernel"       ;; message mechanics
     "st-object"       ;; Object behavior
     "st-core-classes" ;; Object Class MetaClass ClassDescription Behavior
     "st-boolean"      ;; Boolean True False UndefinedObject (nil)
     "st-character"    ;; Character
     "st-magnitude"
     "st-number"
     "st-collection"
     "st-string"       ;; String
     "st-blockClosure" ;; BlockClosure
     "st-array"        ;; Array
     "st-set"          ;; Set
     "st-tokenizer"    ;; Stream -> tokens
     "st-parse"        ;; tokens -> AST
;;     @@@more to come...
    )
 )

(define (source-files)
  (map (lambda (file-name)
         (string-append st-root-directory-prefix file-name ".scm"))
       st-bootstrap-files)
)

(define (compiled-files)
  (map (lambda (file-name)
         (string-append st-root-directory-prefix file-name ".fasl"))
       st-bootstrap-files)
)

(define (remove-compiled)
  (for-each delete-file (compiled-files)))

(define (compile-bootstrap)
  (for-each (lambda (fn) (compile-file fn)) (source-files)))

(define (load-source-bootstrap)
  (for-each load (source-files)))

(define (load-compiled-bootstrap)
  (for-each load (compiled-files)))



;;;			--- E O F ---			;;;
