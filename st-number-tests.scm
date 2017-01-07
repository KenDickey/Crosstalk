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


;; @@@FIXME: many more tests needed!!

;;;			--- E O F ---			;;;
