;;; FILE: "st-set-tests.scm"
;;; IMPLEMENTS: UNit tests for st-set
;;; AUTHOR: Ken Dickey
;;; DATE: 21 July 2016

;; (requires 'st-set)

(define set1 #false)
(define set2 #false)

(define (setup-st-set)
  (set! set1 (perform: Set 'new))
  (set! set2 (perform: Set 'new)))

(define (cleanup-st-set) 
  (set! set1 #false)
  (set! set2 #false))

(add-test-suite 'st-set
                setup-st-set
                cleanup-st-set)

(add-equal-test 'st-set
  st-true
  (perform:with: set1 '= set2)
  "(set1 = set2) --> true")

(add-equal-test 'st-set
  st-false
  (begin
    (perform:with: set1 'add: "some string")
    (perform:with: set1 '= set2))
  "(set1 = set2) --> false")


(add-equal-test 'st-set
  9
  (begin
    (perform:with: set1 'add: "another string")
    (perform:with: set1 'add: 'aSymbol)
    (perform:with: set1 'add: 1/2)
    (perform:with: set1 'add: 1/2)
    (perform:with: set1 'add: "another string")
    (perform:with: set1 'add: #true)
    (perform:with: set1 'add: 23.4)
    (perform:with: set1 'add: (sqrt -4))
    (perform:with: set1 'add: 'mySetSymbol)
    (perform:with: set1 'add: 347)
    (perform: set1 'tally))
  "(set1 tally) --> 9")

(add-equal-test 'st-set
  st-true
  (begin
    (set! set2 (perform: set1 'copy))
    (perform:with: set1 '= set2))
  "After copy: (set1 = set2) --> true")


(add-equal-test 'st-set
  9
  (perform: set2 'tally)
  "(set2 tally) --> 9")

(add-equal-test 'st-set
  9
  (let ( (count 0) )
    (perform:with: set2
                   'do:
                   (lambda (elt)
                     (set! count (+ 1 count))))
    count)
  "via #do: (set2 size) --> 9")

(add-equal-test 'st-set
  8
  (begin
    (perform:with:with:
       set2
       'remove:ifAbsent: 'aSymbol (lambda () #false))
    (perform: set2 'tally))
  "after #remove: (set2 tally) --> 8")

(add-equal-test 'st-set
  #false
  (perform:with:with:
     set2
     'remove:ifAbsent: 'aSymbol (lambda () #false))
  "after #remove:, 2nd #remove: block --> #false")

(add-equal-test 'st-set
  8
  (begin
    (perform:with:with:
       set2
       'remove:ifAbsent: 'aSymbol (lambda () #false))
    (perform: set2 'tally))
  "after failed #remove: (set2 tally) --> 8")

(add-equal-test 'st-set
  6
  (let ( (aSet
            (perform:with:with:with:
               Set
               'with:with:with:
               1 2 3))
         (sum 0)
       )
    (perform:with: aSet
                   'do:
                   (lambda (elt)
                     (set! sum (+ elt sum))))
    sum)
  "sum Set( 1 2 3 ) --> 6")

(add-equal-test 'st-set
  #(6 4 2)
  (perform:
   (perform:with:
    (perform:with:with:with: Set 'with:with:with: 1 2 3)
    'collect: (lambda (elt) (* 2 elt)))
   'asArray)
  "(Set with: 1 with: 2 with: 3) collect: [:e |2 * e]) asArray")

(add-equal-test 'st-set
  #(6 4 2)
  (perform:
    (perform:with: (perform: (vector 1 2 3) 'asSet)
                   'collect: (lambda (elt) (* 2 elt)))
    'asArray)
  "(#(1 2 3) asSet collect: [:e |2 * e]) asArray")

;;;			--- E O F ---			;;;
