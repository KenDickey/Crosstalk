;;; FILE: "st-set-tests.ss"
;;; IMPLEMENTS: Unit tests for Sets
;;; AUTHOR: Ken Dickey
;;; DATE: 21 July 2016; March 2025


(define set1 #f)
(define set2 #f)

(define (setup-st-set)
  (set! set1 ($ Set 'new))
  (set! set2 ($ Set 'new)))

(define (cleanup-st-set) 
  ;; (set! set1 #f)
  ;; (set! set2 #f)
  33
  )

(add-test-suite 'st-set
                setup-st-set
                cleanup-st-set)

(add-equal-test 'st-set
  st-true
  ($: set1 '= set2)
  "(set1 = set2) --> true")

(add-equal-test 'st-set
  st-false
  (begin
    ($: set1 'add: "some string")
    ($: set1 '= set2))
  "(set1 = set2) --> false")


(add-equal-test 'st-set
  9
  (let ( (aString  "another string") )
    ($: set1 'add: aString)
    ($: set1 'add: 'aSymbol)
    ($: set1 'add: 1/2)
    ($: set1 'add: 1/2)
    ($: set1 'add: aString)
    ($: set1 'add: #t)
    ($: set1 'add: 23.4)
    ($: set1 'add: (sqrt -4))
    ($: set1 'add: 'mySetSymbol)
    ($: set1 'add: 347)
    ($ set1 'tally))
  "(set1 tally) --> 9")

(add-equal-test 'st-set
  st-true
  (begin
    (set! set2 ($ set1 'copy))
    ($: set1 '= set2))
  "After copy: (set1 = set2) --> true")


(add-equal-test 'st-set
  9
  ($ set2 'tally)
  "(set2 tally) --> 9")

(add-equal-test 'st-set
  9
  (let ( (count 0) )
    ($: set2
        'do:
        (lambda (elt)
          (set! count (+ 1 count))))
    count)
  "via #do: (set2 size) --> 9")

(add-equal-test 'st-set
  8
  (begin
    ($::
       set2
       'remove:ifAbsent: 'aSymbol (lambda () #f))
    ($ set2 'tally))
  "after #remove: (set2 tally) --> 8")

(add-equal-test 'st-set
  #f
  ($::
     set2
     'remove:ifAbsent: 'aSymbol (lambda () #f))
  "after #remove:, 2nd #remove: block --> #f")

(add-equal-test 'st-set
  8
  (begin
    ($::
       set2
       'remove:ifAbsent: 'aSymbol (lambda () #f))
    ($ set2 'tally))
  "after failed #remove: (set2 tally) --> 8")

(add-equal-test 'st-set
  6
  (let ( (aSet
            ($:::
               Set
               'with:with:with:
               1 2 3))
         (sum 0)
       )
    ($: aSet
                   'do:
                   (lambda (elt)
                     (set! sum (+ elt sum))))
    sum)
  "sum Set( 1 2 3 ) --> 6")

(add-equal-test 'st-set
  st-true
  ($: ($::: Set 'with:with:with: 2 4 6)
      '=
      ($: ($::: Set 'with:with:with: 1 2 3)
          'collect: (lambda (elt) (* 2 elt)))
  )
  "(Set with: 1 with: 2 with: 3) collect: [:e |2 * e])")

(add-equal-test 'st-set
  st-true
  ($: ($::: Set 'with:with:with: 6 4 2)
      '=
      ($: ($ (vector 1 2 3) 'asSet)
          'collect: (lambda (elt) (* 2 elt)))
   )
  "(#(1 2 3) asSet collect: [:e |2 * e]) asArray")

;;;			--- E O F ---			;;;
