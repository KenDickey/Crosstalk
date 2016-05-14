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

(define files
  '( "st-object.sch"
     "st-boolean.sch"
     )
 )

(define (bootstrap)
  (for-each load files))



;;;			--- E O F ---			;;;
