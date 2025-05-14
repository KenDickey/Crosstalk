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

;; (dict->alist (condition->dictionary zero-divide))
;; ==>
;; ( (isMessage . #t) (isFormat . #t)
;;   (message . "undefined for ~s")
;;   (isIrritants . #t) (irritants 0)
;;   (isWho . #t)       (who . /)
;;   (isContinuation . #t)  (k . #<system continuation in dynamic-wind>)
;;   (isAssertion . #t)
;; )

;; (dict->alist (condition->dictionary frob-error))
;; ==>
;; ( (isMessage . #t) (isFormat . #t)
;;   (message . "invalid message argument ~s (who = ~s, irritants = ~s)")
;;   (isIrritants . #t) (irritants a "frob" ("bee" #\c 47))
;;   (isWho . #t)       (who . error) 
;;   (isContinuation . #t) (k . #<system continuation in dynamic-wind>)
;;   (isAssertion . #t) 
;; )

;; (dict->alist (condition->dictionary write-to-non-port))
;; ==>
;; ( (isMessage . #t) (isFormat . #t)
;;   (message . "~s is not a textual output port")
;;   (isIrritants . #t) (irritants 0)
;;   (isWho . #t)       (who . write)
;;   (isContinuation . #t) (k . #<system continuation in dynamic-wind>)
;;   (isAssertion . #t) 
;;   )


(define (cleanup-st-conditions)
  (set! zero-divide #f)
  (set! frob-error  #f)
  (set! write-to-non-port #f)
)

(add-test-suite 'st-conditions
                setup-st-conditions
                cleanup-st-conditions)

(add-equal-test 'st-conditions
   st-true
  (let ( (zd (condition->dictionary zero-divide)) )
    ($: ($ (hashtable-keys zd) 'asSet)
	'=
	($ (list->vector
       '(isMessage isFormat irritants who k isContinuation
		 isAssertion isIrritants message isWho))
     'asSet)))
  "zero-divide condition asDictionary")

(add-equal-test 'st-conditions
  "undefined for ~s"
  (let ( (zd (condition->dictionary zero-divide)) )
    ($: zd 'at: 'message))
  "zero-divide condition asDictionary")

(add-equal-test 'st-conditions
  "invalid message argument ~s (who = ~s, irritants = ~s)"
  (let ( (fe (condition->dictionary frob-error)) )
    ($: fe 'at: 'message))
  "Scheme error condition asDictionary")

(add-equal-test 'st-conditions
   st-true
  (let ( (fe (condition->dictionary frob-error)) )
    ($: ($ (hashtable-keys fe) 'asSet)
	'=
	($ (list->vector
	       '(isMessage isFormat irritants who k isContinuation
	         isAssertion isIrritants message isWho))
      'asSet)))
  "Scheme error condition asDictionary")

(add-equal-test 'st-conditions
  "~s is not a textual output port"
  (let ( (wtnp (condition->dictionary write-to-non-port)) )
    ($: wtnp 'at: 'message))
  "write-to-non-port condition asDictionary")

;; (ensure-exception-raised 'st-conditions
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

