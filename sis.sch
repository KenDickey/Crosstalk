;;; FILE: "sis.sch"
;;; IMPLEMENTS: Scheme in Smalltalk -- kernel ST bootstrap
;;; AUTHOR: Ken Dickey
;;; DATE: 14 May 2016

; Implementation Language/platform: Larceny R7RS Scheme
;   http://larcenists.org/
;   https://github.com/larcenists/larceny
;
; ANSI Smalltalk
;
;

(define files
  '( "object.sch"
     "boolean.sch"
     )
 )

(define (bootstrap)
  (for-each load files))



;;;			--- E O F ---			;;;
