;;; IMPLEMENTS: Unit tests for st-conditions.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-conditions)

(define zero-divide #f)
(define frob-error  #f)
(define write-to-non-port #f)

(define-syntax capture-condition ;; to explore
  (syntax-rules ()
    ((capture-condition form)
     (call/cc
      (lambda (exit)
        (with-exception-handler
         (lambda (c) (exit c))
         (lambda ()  form)))))))


(define (setup-st-conditions)
  (set! zero-divide
        (capture-condition (/ 3 0)))
  (set! frob-error
        (capture-condition
         (error "frob" 'a "bee" $c 47)))
  (set! write-to-non-port
        (capture-condition (write 3 0)))
)

(define (cleanup-st-conditions)
  (set! zero-divide #f)
  (set! frob-error  #f)
  (set! write-to-non-port #f)
)

(add-test-suite 'st-conditions
                setup-st-conditions
                cleanup-st-conditions)

(define (dict->alist dict)
  (let-values ( ((keys-vec vals-vec)
                 (hashtable-entries dict)) )
   (vector-map cons keys-vec vals-vec)))

(add-equal-test 'st-conditions
 #((isMessage . #t)
   (message . "/: zero divisor: 3 0 \n")
   (isWho . #t)
   (isAssertion . #t)
   (who . "/"))
 (dict->alist (condition->dictionary zero-divide))
 "zero-divide condition asDictionary")

(add-equal-test 'st-conditions
 #((isMessage . #t)
   (isError . #t)
   (message . "not a textual output port")
   (isWho . #t)
   (who . write-char)
   (irritants 0)
   (isIrritants . #t))
 (dict->alist (condition->dictionary write-to-non-port))
 "write-to-non-port condition asDictionary")

;; (ensure-exception-raised 'st-conditions
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

