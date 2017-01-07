;;; IMPLEMENTS: Unit tests for st-xlate.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 29 June 2016

;; (require 'st-xlate)

(define (setup-st-xlate)   #f)
(define (cleanup-st-xlate) #f)

(add-test-suite 'st-xlate
                setup-st-xlate
                cleanup-st-xlate)

(add-equal-test 'st-xlate
  '(perform:with: a '+ b)
  (AST->scm (st->AST " a + b. "))
  "a + b")


(add-equal-test 'st-xlate
  '(perform:with:with:
    (lambda (a b) (perform:with: a '+ b))
    'value:value:
    2
    3)
  (AST->scm (st->AST "[ :a :b | a + b] value: 2 value: 3."))
  "[ :a :b | a + b] value: 2 value: 3.")


(add-equal-test 'st-xlate
  '(perform:with:with:
    String
    'addSelector:withMethod:
    'contains:
    (lambda (self aChar)
      (perform:with:
       self
       'detect:
       (lambda (c) (perform:with: c '= aChar)))))
  (AST->scm
   (st->AST  "String addSelector: #contains:
	     withMethod: [ :self :aChar |
	                   self detect: [ :c | c = aChar] ]."))
  "String contains: ..")


(add-equal-test 'st-xlate
  '(perform:with:with:
    String
    'addSelector:withMethod:
    'contains:
    (lambda (self aChar)
      (perform:with:
       self
       'detect:
       (lambda (c) (perform:with: c '= aChar)))))
  (AST->scm
   (st->AST "String ~> contains: aChar
	[ self detect: [ :c | c = aChar] ]."))
)


(add-equal-test 'st-xlate
  '(lambda (a)
     (call/cc (return)
      (let ((b ()))
        (set! b (begin (perform: a 'sin)))
        (return b))))
  (AST->scm (st->AST "[ :a| | b | b := a sin. ^ b ]"))
  "[ :a| | b | b := a sin. ^ b ]")

;; (ensure-exception-raised 'st-xlate
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

