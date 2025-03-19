;;; FILE: "st-array-tests.ss"
;;; IMPLEMENTS: Unit tests for st-array.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016; March 2025

(import (st-array))

(define (setup-st-array)   #f)
(define (cleanup-st-array) #f)

(add-test-suite 'st-array
                setup-st-array
                cleanup-st-array)

(add-equal-test 'st-array
  (vector 3)
  (perform:with: Array
                 'with: 3)
  "Array with: 3")

(add-equal-test 'st-array
  (vector 1 2)
  (perform:with:with:
       Array
      'with:with: 1 2)
  "Array with: 1 with: 2")

(add-equal-test 'st-array
  (vector 1 2 3)
  (perform:with:with:with:
      Array
      'with:with:with: 1 2 3)
  "Array with: 1 with: 2 with: 3")

(add-equal-test 'st-array
  6
  (let ( (array (vector 1 2 3))
         (sum 0)
       )
    (perform:with: array
              'do:
              (lambda (elt) (set! sum (+ sum elt))))
    sum)
  "#(1 2 3) do: --> sum=6")

(add-equal-test 'st-array
  3
  (perform: (vector 1 2 3)
            'size)
  "#(1 2 3) size --> 3")

(add-equal-test 'st-array
  2
  (perform:with: (vector 1 2 3)
                 'at: 2)
  "#(1 2 3) at: 2 --> 2")

(add-equal-test 'st-array
  2
  (perform:with: (vector 1 2 3)
                 'at: 2)
  "#(1 2 3) at: 2 --> 2")

(add-eq-test 'st-array
  'two
  (let ( (array (vector 1 2 3)) )
    (perform:with:with:
       array
       'at:put: 2 'two)
    (perform:with: array 'at: 2))
  "#(1 2 3) at: 2 put: 'two")

  (add-eq-test 'st-array
  4
  (let ( (array (vector 1 2 3)) )
    (perform:with:with:
       array
       'at:modify: 2  (lambda (elt) (* elt 2)))
    (perform:with: array 'at: 2))
  "#(1 2 3) at: 2 modify: '[:x| 2 * x ]")

(add-equal-test 'st-array
  "#( $a $b $c )"
  (perform: (vector #\a #\b #\c) 'printString)
  "#($a $b $c) printString")

;; (ensure-exception-raised 'st-array
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;
