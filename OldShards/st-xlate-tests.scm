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
  '($: a '+ b)
  (AST->scm (st->AST " a + b. "))
  "a + b")


(add-equal-test 'st-xlate
  '($::
    (lambda (a b) ($: a '+ b))
    'value:value:
    2
    3)
  (AST->scm (st->AST "[ :a :b | a + b] value: 2 value: 3."))
  "[ :a :b | a + b] value: 2 value: 3.")


(add-equal-test 'st-xlate
  '($:: (smalltalkAt: 'String)
     'addSelector:withMethod:
     'contains:
     (lambda (self aChar)
       ($: self 'detect: (lambda (c) ($: c '= aChar)))))
  (AST->scm
   (st->AST  "String addSelector: #contains:
	     withMethod: [ :self :aChar |
	                   self detect: [ :c | c = aChar] ]."))
  "String contains: ..")


(add-equal-test 'st-xlate
  '($:: (smalltalkAt: 'String)
     'addSelector:withMethod:
     'contains:
     (lambda (self aChar)
       ($: self 'detect: (lambda (c) ($: c '= aChar)))))
  (AST->scm
   (st->AST "String ~> contains: aChar
	[ ^ self detect: [ :c | c = aChar] ]."))
  "String contains: ... (2)"
)


(add-equal-test 'st-xlate
  '(lambda (a)
  (call/cc
   (lambda (return)
    (let ((b nil))
      (let ((%%val%% ($ a 'sin)))
        (set! b %%val%%)
        %%val%%)
      (return b)))))
  (st->scm "[ :a| | b | b := a sin. ^ b ]")
  "[ :a| | b | b := a sin. ^ b ]")

(add-equal-test 'st-xlate
  (vector 1 2 3 'a #\b "c")
  (st-eval "{ 1. 2. 3. #a. $b. 'c'.}")
  "{ 1. 2. 3. #a. $b. 'c'.}")

(add-equal-test 'st-xlate
  (vector 1 5 9 'a #\b "c")
  (st-eval "{ 1. 2 + 3. 3 squared. #a. $b. 'c'.}")
  "{ 1. 2 + 3. 3 squared. #a. $b. 'c'.}")

(add-equal-test 'st-xlate
  '(let ((%%val%%
          (lambda (a b) ($:: a 'foo:bar: b ($: c '+ 7)))))
     (set! block %%val%%)
     %%val%%)
  (st->scm "block := [:a :b| ^(a foo: b bar: c + 7)].")
  "block := [...]")

(add-equal-test 'st-xlate
   '($ (lambda ()
     (let ((a nil))
       (let ((%%val%% 3)) (set! a %%val%%) %%val%%)
       ($: a '+ a)))
   'value)
  (st->scm  "[|a| a := 3. a+a] value.")
  "[|a| a := 3. a+a] value.")

(add-equal-test 'st-xlate
  '($:: (smalltalkAt: 'Object)
     'addSelector:withMethod:
     'exampleWithNumber:
     (lambda (self x)
       (call/cc
         (lambda (return)
           (let ((y nil))
             ($: ($: ($: true '& ($ false 'not))
                     '&
                     ($ nil 'isNil))
                 'ifFalse:
                 (lambda () ($ self 'halt)))
             (let ((%%val%% ($: ($ self 'size) '+ (@ self 'size))))
               (set! y %%val%%)
               %%val%%)
             ($: '#( #\a 'a "a" 1 1.0 )
                 'do:
                 (lambda (each)
                   (let ((receiver (smalltalkAt: 'Transcript)))
                     ($: (smalltalkAt: 'Transcript)
                         'show:
                         ($ ($ each 'class) 'name))
                     ($: receiver 'show: ($ each 'printString))
                     ($: receiver 'show: " "))))
             (return ($: x '< y)))))))
     (st->scm
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
")
	"Smalltalk syntax on a postcard"
)



(add-equal-test 'st-xlate
  '($::
  (smalltalkAt: 'Collection)
  'addSelector:withMethod:
  'printOn:
  (lambda (self aStream)
    ($:
      aStream
      'nextPutAll:
      ($:
        ($ ($ self 'class) 'name)
        '|,|
        " ("))
    ($:
      self
      'do:
      (lambda (element)
        ($: element 'printOn: aStream)
        ($ aStream 'space)))
    ($: aStream 'nextPut: #\))
    self))
  (st->scm "Collection ~> printOn: aStream
[
\"Refer to the comment in Object|printOn:.\"
  aStream nextPutAll: self class name , ' ('.
  self
	do: [ :element | 
		element printOn: aStream.
		aStream space ].
  aStream nextPut: $)
].
")
  "Collection ~? printOn:")

(add-equal-test 'st-xlate
  '($:: ($ (smalltalkAt: 'Collection) 'class)
     'addSelector:withMethod:
     'with:
     (lambda (self anObject)
       (call/cc
        (lambda (return)
         (let ((newCollection nil))
           (let ((%%val%% ($ self 'new)))
             (set! newCollection %%val%%)
             %%val%%)
           ($: newCollection 'add: anObject)
           (return newCollection))))))
  (st->scm "Collection class ~> with: anObject
[
\"Answer an instance of me containing anObject.\"
  | newCollection |
  newCollection := self new.
  newCollection add: anObject.
  ^ newCollection
].
")
  "Collection class ~> with:")


;; (ensure-exception-raised 'st-xlate
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    ($ %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;
