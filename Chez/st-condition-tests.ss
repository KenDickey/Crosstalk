;;; IMPLEMENTS: Unit tests for st-conditions.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

(define zero-divide #f)
(define frob-error  #f)
(define write-to-non-port #f)

(define-syntax capture-condition  ;; to explore
  (syntax-rules ()
    ((capture-condition form)
     ;;==>
     (call/cc
      (lambda (exit)
        (with-exception-handler
         (lambda (c) (exit c))
         (lambda ()  form))))
) ) )

(define (setup-st-conditions)
  (set! zero-divide
        (capture-condition (/ 3 0)))
  (set! frob-error
        (capture-condition
         (error "frob" 'a "bee" #\c 47)))
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

(add-equivalent-alist-test 'st-conditions
;;(add-equal-test 'st-conditions                           
 '((message . "undefined for ~s") (isAssertion . #t) (isIrritants . #t)
  (k . #<system continuation in dynamic-wind>) (isWho . #t)
  (irritants 0) (isMessage . #t) (isContinuation . #t)
  (who . /) (isFormat . #t))
 (dict->alist (condition->dictionary zero-divide))
 "zero-divide condition asDictionary")

(add-equivalent-alist-test 'st-conditions
  '((message
   .
   "invalid message argument ~s (who = ~s, irritants = ~s)") (isAssertion . #t) (isIrritants . #t)
  (k . #<system continuation in dynamic-wind>) (isWho . #t)
  (irritants a "frob" ("bee" #\c 47)) (isMessage . #t)
  (isContinuation . #t) (who . error) (isFormat . #t))
 (dict->alist (condition->dictionary frob-error))
 "Scheme error condition asDictionary")

(add-equivalent-alist-test 'st-conditions
 '((message . "~s is not a textual output port") (isAssertion . #t) (isIrritants . #t)
  (k . #<system continuation in dynamic-wind>) (isWho . #t)
  (irritants 0) (isMessage . #t) (isContinuation . #t)
  (who . write) (isFormat . #t))
 (dict->alist (condition->dictionary write-to-non-port))
 "write-to-non-port condition asDictionary")

;; (ensure-exception-raised 'st-conditions
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

