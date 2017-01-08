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
  "String contains: ... (2)"
)


(add-equal-test 'st-xlate
  '(lambda (a)
     (call/cc (return)
      (let ((b ()))
        (set! b (perform: a 'sin))
        (return b))))
  (AST->scm (st->AST "[ :a| | b | b := a sin. ^ b ]"))
  "[ :a| | b | b := a sin. ^ b ]")

(add-equal-test 'st-xlate
  '(perform:
    (lambda ()
      (let ((a ())) (set! a 3) (perform:with: a '+ a)))
    'value)
  (AST->scm (st->AST  "[|a| a := 3. a+a] value."))
  "[|a| a := 3. a+a] value.")

(add-equal-test 'st-xlate
  '(perform:with:with:
  Object
  'addSelector:withMethod:
  'exampleWithNumber:
  (lambda (self x)
    (call/cc
      (return)
      (let ((y ()))
        (perform:with:
          (perform:with:
            (perform:with: true '& (perform: false 'not))
            '&
            (perform: nil 'isNil))
          'ifFalse:
          (lambda () (perform: self 'halt)))
        (set! y
          (perform:with:
            (perform: self 'size)
            '+
            (perform: super 'size)))
        (perform:with:
          #(#\a 'a "a" 1 1.0)
          'do:
          (lambda (each)
            (let ((receiver Transcript))
              (perform:with:
                Transcript
                'show:
                (perform: (perform: each 'class) 'name))
              (perform:with:
                recevier
                'show:
                (perform: each 'printString))
              (perform:with: recevier 'show: " "))))
        (return (perform:with: x '< y))))))
  (AST->scm (st->AST
"Object ~> exampleWithNumber: x
[ |y|
  true & false not & (nil isNil)
        ifFalse: [self halt].
  y := self size + super size.
  #($a #a 'a' 1 1.0) do: [:each |
    Transcript
    	show: (each class name);
        show: (each printString);
        show: ' '
        ].
   ^ x < y
].
"))
	"Smalltalk syntax on a postcard"
)

  

;; (ensure-exception-raised 'st-xlate
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

