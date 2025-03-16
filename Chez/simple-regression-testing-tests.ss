#!r6rs

(import (simple-regression-testing))

(define do-nothing (lambda whatever 'ignored))

(add-test-suite 'one do-nothing do-nothing)

(add-eq-test 'one #t (= 1 1) "equal")

(add-eq-test 'one #f (< 2 1) "less")

(add-eq-test 'one 'foo (string->symbol "foo") "eq?")

(add-equal-test 'one "foo" (symbol->string 'foo) "equal?")

(add-test 'one
          'expected-to-fail
          (/ 7 0)
          eq?
          "test uncaught exception")

(add-test 'one 37 (+ 36 1) = "addition")

(add-test-suite 'two do-nothing do-nothing)

(add-test 'two 54 (max 32 1 54 7 23 7 21) = "max")

(add-test 'two
          'yes
          (if (> 2 1) 'yes 'no)
          eq?
          "if")

(add-test 'two
          'always-fails
          (if (> 2 1) 'yes 'no)
          eq?
          "test a failure case")

(ensure-exception-raised 'two
                         (lambda (c) (equal? / (condition-who c)))
                         (/ 7 0)
                         "zero divisor exception")

(ensure-exception-raised 'two
                         (lambda (c)
                           (equal? '(0) (condition-irritants c)))
                         (/ 7 0)
                         "test exception predicate failure")

(ensure-exception-raised 'two
                         serious-condition?
                         (/ 7 0)
                         "test exception predicate failure")


(verbose-test-output? #t)
(break-on-test-error? #f) ; keep going
(run-all-tests)
(display "Expected Result: 9 Passed, 2 Failed, 1 Excepted.")
(newline)
(newline)

;; EOF ;;
