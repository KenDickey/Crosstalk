;;; FILE: "sis.sch"
;;; IMPLEMENTS: Smalltalk in Scheme -- kernel ST bootstrap
;;; AUTHOR: Ken Dickey
;;; DATE: 14 May 2016

; Implementation Language/platform: Larceny R7RS Scheme
;   http://larcenists.org/
;   https://github.com/larcenists/larceny
;
; ANSI Smalltalk
;   https://en.wikipedia.org/wiki/Smalltalk
;   http://object-arts.com/downloads/papers/AnsiSmalltalkDraft1-9.pdf

;; larceny -r7rs, then (import (scheme load)) (load "sis.sch")

(import
    (rnrs hashtables (6))
    (primitives load compile-file procedure-name procedure-name-set!))

(define st-root-directory-prefix "/home/kend/SiS/")

(define st-bootstrap-files
  '( "st-kernel"     ;; message mechanics
     "st-object"     ;; Object behavior
     "st-class"      ;; Object Class MetaClass ClassDescription Behavior
     "st-boolean"    ;; Boolean True False
    )
 )

(define (source-files)
  (map (lambda (file-name)
         (string-append st-root-directory-prefix file-name ".sch"))
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
