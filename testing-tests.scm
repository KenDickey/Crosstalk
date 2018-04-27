
(include "testing-macros.scm")
(load "testing")

(add-test-suite 'one)

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

(add-test-suite 'two)

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
                         divide-by-zero-exception?
                         (/ 7 0)
                         "zero divisor exception")

(ensure-exception-raised 'two
                         error-exception?
                         (/ 7 0)
                         "test exception predicate failure")


(verbose? #t)
(run-all-tests)
(display "Expected Result: 8 Passed, 2 Failed, 1 Excepted.")
(newline)
(newline)

;; EOF ;;
