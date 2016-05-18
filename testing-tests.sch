#!r6rs
;; FILE: "testing-tests.ss"
;; IMPLEMENTS: Quick check for simple-regression-testing library
;; LANGUAGE: Larceny Scheme
;; AUTHOR: Ken [dot] Dickey [at] Whidbey [dot] Com
;; UPDATED:  9 January 2008 for r6rs
;; UPDATED: 17 May 2016 for Larceny testing (ARM)

(import (rnrs)
;;      (srfi parameters)
        (kend simple-regression-testing))

(remove-all-test-suites) ;; start afresh

(add-test-suite 'one default-setup-thunk default-teardown-thunk)

(add-eq-test 'one #t (= 1 1) "equal")

(add-eq-test 'one #f (< 2 1) "less")

(add-eq-test 'one 'foo (string->symbol "foo") "eq?")

(add-equal-test 'one "foo" (symbol->string 'foo) "equal?")

(add-test 'one
          'test-expected-to-fail
          (sqrt 'foo) ;; (/ 7 0)
          eq?
          "test uncaught exception")

(add-test 'one 37 (+ 36 1) = "addition")

(add-test-suite 'two default-setup-thunk default-teardown-thunk)

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
           (lambda (exn)
             (and (message-condition? exn)
                  (string=? "real-part: not a number: foo \n"
                            (condition-message exn))))
           (sqrt 'foo) ;; (/ 7 0)
           "real-part: not a number: foo \n")
           ;;"zero divisor exception")

(ensure-exception-raised 'two
                         (lambda (whatever) #f)
                         (sqrt 'foo) ;; (/ 7 0)
                         "test exception predicate failure")


(break-on-test-error? #f)
(verbose-test-output? #f)

(run-all-tests)

(display "Expected Result: 8 Passed, 2 Failed, 1 Excepted.")

(newline)
(newline)

;;#!eof
;; EOF ;;
