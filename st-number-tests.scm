;;; FILE: "st-number-tests.scm"
;;; IMPLEMENTS: Unit tests for st-number.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-number)

(define (setup-st-number)   #f)
(define (cleanup-st-number) #f)

(add-test-suite 'st-number
                setup-st-number
                cleanup-st-number)

(add-equal-test 'st-number
  (+ 2 3)
  (perform:with: 2 '+ 3)
  "2 + 3")

(add-equal-test 'st-number
  (* 2 3)
  (perform:with: 2 '* 3)
  "2 * 3")

(add-equal-test 'st-number
  (/ 2 3)
  (perform:with: 2 '/ 3)
  "2 / 3")

(add-equal-test 'st-number
  (- 2 3)
  (perform:with: 2 '- 3)
  "2 - 3")

(add-equal-test 'st-number
  #true
  (perform:with: 2 'isKindOf: Number)
  "2 isKindOf: Number")

(add-equal-test 'st-number
  #false
  (perform:with: 1/2 'isKindOf: Integer)
  "1/2 isKindOf: Integer")

(add-equal-test 'st-number
  #true
  (perform:with: 1/2 'isKindOf: Magnitude)
  "1/2 isKindOf: Magnitude")

(add-equal-test 'st-number
  "3r1200201"
  (st-eval "1234 printStringRadix: 3")
  "1234 printStringRadix: 3")

(add-equal-test 'st-number
  "(12r3 +12r44a i)"
  ($: 3+634i 'printStringRadix: 12)
  "(3 + 634 i) printStringRadix: 3")

;; @@@FIXME: many more tests needed!!

;;;			--- E O F ---			;;;
