;;; FILE: "st-boolean.sch"
;;; IMPLEMENTS: Boolean True False
;;; AUTHOR: Ken Dickey
;;; DATE: 10 May 2016

;; (require 'st-object)

(define st-boolean-behavior (clone-method-dictionary (behavior Object)))
;; st-true-behavior and st-false-behavior are def'ed in "st_kernel.sch"
;; reset their behaviors
(set! st-true-behavior  (clone-method-dictionary st-boolean-behavior))
(set! st-false-behavior (clone-method-dictionary st-boolean-behavior))

;; Boolean logic is typically inlined, but as a bootstrap..




(primAddSelector:withMethod: 
 	st-true-behavior
        'printString
        (lambda (self) "true"))

(primAddSelector:withMethod: 
 	st-false-behavior
        'printString
        (lambda (self) "false"))

(primAddSelector:withMethod: 
 	st-nil-behavior
        'printOn:
        (lambda (self port)
          (display "nil" port)))

(primAddSelector:withMethod: 
 	st-true-behavior
        'printOn:
        (lambda (self port)
          (display "true" port)))

(primAddSelector:withMethod: 
 	st-false-behavior
        'printOn:
        (lambda (self port)
          (display "false" port)))

(primAddSelector:withMethod: 
 	st-string-behavior
        'printString
        printString)



;;;			--- E O F ---			;;;
