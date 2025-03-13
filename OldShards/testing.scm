;;; FILE: "testing.scm"
;;; IMPLEMENTS: Regression Test System
;;; LANGUAGE: Gambit Scheme (v4 beta12)
;;; AUTHOR: Ken Dickey
;;;
;;; COPYRIGHT (c) 2005 by Kenneth Alan Dickey
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.
;==============================================================;
(declare (standard-bindings) (fixnum))
;==============================================================;
;;
;;;USAGE SYNOPSIS
;;
;; (include "testing-macros.scm")
;; (load "testing")
;; <define one or more test suites & tests>
;; (run-all-tests)
;;
;; Tests are first created and added to a global TESTS "database".
;; Tests are arranged by SUITE-NAME (just a symbol naming a set of tests).
;;
;; SPECIAL FORMS:
;;
;; (add-test       suite-name expect form equivalent? . message)
;; (add-eq-test    suite-name expect form . message)
;; (add-equal-test suite-name expect form . message)
;; (ensure-exception-raised suite-name type-pred? form . message)
;; 
;;  All forms are "thunkified" by being wrapped in zero argument lambdas.
;;  Internal usage is: (equivalent? expected (thunk))
;;
;;
;; TESTING OPERATIONS:
;;
;;  (add-test-suite suite-name setup-thunk teardown-thunk)
;;   => Creates a test suite for the suite-name, which must be a symbol.
;; 
;;  (run-all-tests) => Run all suites of tests.
;;
;;  (run-tests-for suite-name) => Run tests for the named test-suite.
;;  (remove-suite  suite-name) => Remove the named test-suite.
;;
;;  (verbose?) => If #t, displayes tests that pass as well as fail or cause exceptions
;;  (verbose? <bool>) => sets verbose?
;;
;;  (break-on-error?) => if #t, will call ERROR when a test fails,
;;  otherwise tests report & continue.
;;  (break-on-error? <bool>) => sets break-on-error?
;;
;;
;; Tests are typically written as separate files containing set-up & tear-down code.
;;
;; Nota Bene:  Currently all output  goes to (current-output-port).
;;             Rebind this port to redirect output elsewhere,

;==============================================================;
;; EXPORTED BINDINGS


(define verbose?        (make-parameter #f))
(define break-on-error? (make-parameter #f))
(define add-test-suite  'defined-below)
(define remove-suite    'defined-below)
(define run-tests-for   'defined-below)
(define run-all-tests   'defined-below)

;; for the macros
(define-structure test expected thunk compare? message)

(define test-db-add-test 'defined-below)


;==============================================================;
;;(let ()  ;; keep the namespace minimal
;==============================================================;
;;(include "../src/common-macros.scm")
;;(include "../src/format.scm") ;; load does not work here..
;==============================================================;

;;;A TEST-COUNTER keeps track of number passed,
;;;    failed (actual != expected), excepted (signalled exception)
;;;    and reports on these statistics.

;; test object system => no objects yet to depend on, so use structs

(define-structure test-counter name num-passed num-failed num-excepted)

(define (counter-zero-counters counter)
  (test-counter-num-passed-set!   counter 0)
  (test-counter-num-failed-set!   counter 0)
  (test-counter-num-excepted-set! counter 0)
  counter)

(define counter-initialize counter-zero-counters)

(define (counter-increment-failed counter)
  (test-counter-num-failed-set!
   counter
   (+ 1 (test-counter-num-failed counter))))

(define (counter-increment-excepted counter)
  (test-counter-num-excepted-set!
   counter
   (+ 1 (test-counter-num-excepted counter))))

(define (counter-increment-passed counter)
  (test-counter-num-passed-set!
   counter
   (+ 1 (test-counter-num-passed counter))))

(define (counter-display-results counter port)
  (format port "~%TOTAL PASSED:     ~D"     (test-counter-num-passed   counter))
  (format port "~%TOTAL FAILED:     ~D"     (test-counter-num-failed   counter))
  (format port "~%TOTAL EXCEPTIONS: ~D~%~%" (test-counter-num-excepted counter))
)

;;;======================================================================
;;;A TEST-SUITE is an UNnamed container of unit tests, setup and 
;;;   teardown code.  Tests are a reversed list of test
;;;   instances (see below).  A test-container maintains the (name ->
;;;   unit-test-suite) bindings.


(define-structure test-suite name test-list setup-thunk teardown-thunk)

;;; A test-suite is a container for a (hopefully) related set of tests
(define-structure test expected thunk compare? message)

(define (test-suite-initialize suite setup teardown)
   (test-suite-test-list-set!      suite '())
   (test-suite-setup-thunk-set!    suite setup)
   (test-suite-teardown-thunk-set! suite teardown)
   suite)

(define (test-suite-add-test suite test)
  (unless (and (test-suite? suite) (test? test))
    (call-error "require a suite and a test"
                test-suite-add-test
                suite test))
  (test-suite-test-list-set! suite (cons test (test-suite-test-list suite))))

(define (test-suite-remove-tests suite)
  (test-suite-test-list-set! suite '()))

(define (test-suite-run-tests suite result-counter)
  (unless (test-suite? suite)
    (call-error "require a test-suite"
                run-all-tests
                suite))
  (let ( (test-name      (test-suite-name           suite))
         (test-list      (test-suite-test-list      suite))
         (setup-thunk    (test-suite-setup-thunk    suite))
         (teardown-thunk (test-suite-teardown-thunk suite))
       )
   (if (null? test-list)
       ((if (break-on-error?) error warn)
        "HUH?  No tests found for" test-name)
       (begin
        (setup-thunk)
        (format #t "~%===> Starting  Tests for ~a" test-name)
        (for-each 
         (lambda (test)
           (run-test test result-counter))
         (reverse test-list))
        (format #t "~&===> Completed Tests for ~a~%" test-name)
        (teardown-thunk)))
)  )


;;;======================================================================
;;; A TEST is a single test

;;(define-structure test expected thunk compare? message) -- above

;; (define-syntax (add-test suite-name expect form equivalent? . message)
;;   (let ( (msg (if (pair? message) (car message) "")) )
;;     `(test-container-add-test **tests**
;;                                ,suite-name
;;                                (make-test
;;                                  ,expect
;;                                  (lambda () ,form)
;;                                  ,equivalent?
;;                                  ,msg)
;;   ) ) )

(define-macro (add-test suite-name expect form equivalent? . message)
  (let ( (msg (if (pair? message) (car message) "")) )
    `(test-container-add-test **tests**
                               ,suite-name
                               (make-test
                                 ,expect
                                 (lambda () ,form)
                                 ,equivalent?
                                 ,msg)
) ) )

;;(define-syntax (add-eq-test unit-name expect form . message)
;;   `(add-test ,unit-name ,expect ,form eq?  ,message)) 
(define-macro (add-eq-test suite-name expect form . message)
  (let ( (msg (if (pair? message) (car message) "")) )
    `(test-container-add-test **tests**
                              ,suite-name
                              (make-test ,expect
                                         (lambda () ,form)
                                         eq?
                                         ,msg))
) )

;; (define-syntax (add-equal-test suite-name expect form . message)
;;    `(add-test ,suite-name ,expect ,form equal? ,message))
(define-macro (add-equal-test suite-name expect form . message)
   (let ( (msg (if (pair? message) (car message) "")) )
     `(test-container-add-test **tests**
                               ,suite-name
                               (make-test ,expect
                                          (lambda () ,form)
                                          equal?
                                          ,msg))
) )

;; (define-syntax (ensure-exception-raised suite-name exception-type form . message)
;;    (let ( (msg (if (pair? message) (car message) "")) )
;;      `(add-test **tests**
;;                      ,suite-name
;;                      (make <exception-test>
;;                        ,exception-type
;;                        (lambda () ,form)
;;                        ,msg)
;;  ) ) )

(define-macro (ensure-exception-raised suite-name type-pred? form . message)
   (let ( (msg (if (pair? message) (car message) "")) )
     `(test-container-add-test **tests**
                               ,suite-name
                               (make-test
                               'some-kind-of-exception
                               (lambda ()
                                 (call-with-current-continuation
                                  (lambda (return)
                                    (with-exception-catcher
                                     (lambda (exn) (return exn)) ; capture & return
                                     (lambda () ,form)))))
                               (lambda (actual ignored)
                                 (,type-pred? actual))
                               ,msg))
) )


(define (warn . args)
  (format #t "~a" args))


;;;======================================================================
;;;A TESTS struct contains and runs named test suites,
;;; mapping test-suite names to their associated suites.

(define-structure test-container table)

(define (test-container-initialize container)
   (test-container-table-set! container (make-symbol-table))
)


(define (test-container-add! container name suite)
  (unless (and (test-container? container)
               (test-suite? suite)
               (symbol? name))
    (call-error "expected: container name suite"
                test-container-add!
                container name suite))
  (table-set! (test-container-table container) name suite))

(define do-nothing (lambda ignore-args 'nothing-happening))
(define default-setup-thunk    do-nothing)
(define default-teardown-thunk do-nothing)
(define (test-container-make-suite container
                                    test-suite-name setup teardown)
  (let ( (test-suite
          (make-test-suite test-suite-name '() setup teardown))
       )
    (test-container-add! container test-suite-name test-suite)
    test-suite
    ) )

(define (make-error-string-predicate str)
  (lambda (errObj)
    (cond
     ((uncaught-exception? errObj)
      (string=? str (uncaught-exception-reason errObj)))
     ((error-exception? errObj)
      (string=? str (error-exception-message errObj)))
     ((noncontinuable-exception? errObj)
      (string=? str (noncontinuable-exception-reason errObj)))
     ((os-exception? errObj)
      (string=? str (os-exception-message errObj)))
     ((type-exception? errObj)
      (string=? str "type-exception"))
     ((range-exception? errObj)
      (string=? str "range-exception"))
     ((unbound-global-exception? errObj)
      (string=? str "unbound-global"))
     ((wrong-number-of-arguments-exception? errObj)
      (string=? str "wrong-number-of-arguments"))
     ((divide-by-zero-exception? errObj)
      (string=? str "divide-by-zero"))
     ((nonprocedure-operator-exception? errObj)
      (string=? str "not-a-method"))
     (else #f))
) )


(define (test-container-add-test container suite-name test-case)
  (unless (and (test-container? container)
               (symbol? suite-name)
               (test?   test-case))
    (call-error "expected: container suite-name test"
                test-container-add-test
                container suite-name test-case))
  (cond ((table-ref (test-container-table container) suite-name)
         => (lambda (suite) (test-suite-add-test suite test-case)))
        (else
         (let ( (suite (test-container-make-suite
                        container
                        suite-name
                        default-setup-thunk
                        default-teardown-thunk))
              )
           (test-suite-add-test suite test-case)
           (warn (format #f "~&Created test suite named: ~a" suite-name)))))
  test-case
)

(define (test-container-remove-tests-for container suite-name)
  (unless (and (test-container? container)
               (symbol? suite-name))
    (call-error "expected: container suite-name"
                test-container-remove-tests-for
                container suite-name))
  (table-set! (test-container-table container) suite-name) ;; table-remove!
)



;;;======================================================================
;;; RUNNING TESTS

;; Run a test
;; If no error, don't report unless VERBOSE?
;; If error or exception, break into debugger if BREAK-ON-ERROR?, else continue
;; Result-counter is a test-counter

(define (run-test test result-counter)
   (let* ( (caught-exception #f) 
           (actual 
            (with-exception-catcher
             (lambda (exn)
               (set! caught-exception exn)
               (counter-increment-excepted result-counter)
               ((if (break-on-error?) error warn) 
                (format #f
                        "~&--> ~s:~%*** EXCEPTION: ~s, expected: ~s"
                        (test-message  test)
                        exn
                        (test-expected test)))
               )
             (test-thunk test)))
         )
     (cond
      (caught-exception)
      (((test-compare? test) actual (test-expected test))
       (counter-increment-passed result-counter)
       (if (verbose?)
           (format #t "~&--> ~s:~%PASSED: Expected: ~s ~%             Got: ~s"
                   (test-message  test)
                   (test-expected test)
		   actual)
           #t) ;; compare => #t
       )
      (else
       (counter-increment-failed result-counter)
       ((if (break-on-error?) error warn)
        (format #f "~&--> ~s:~%*** FAILED:  Expected ~s  Got ~s"
                (test-message  test)
                (test-expected test)
                actual))
       )
) ) )

(define **tests** #f)

(define init-test-db
  (lambda ()
    (set! **tests**
          (make-test-container (make-table (string->keyword "test") eq?)))))

(define (remove-all-test-suites) (init-test-db))


;==============================================================;
;;;EXPORTED BINDINGS


(set! add-test-suite
  (lambda (suite-name setup-thunk teardown-thunk)
    (test-container-make-suite
                        **tests**
                        suite-name
                        setup-thunk
                        teardown-thunk)
) )

(set! remove-suite
  (lambda (suite-name)
    (test-container-remove-tests-for **tests** suite-name)))

(set! run-tests-for
  (lambda (suite-name)
    (let ( (suite (table-ref (test-container-table **tests**) suite-name))
           (result-counter  (make-test-counter suite-name 0 0 0))
         )
      (if suite
          (begin
            (test-suite-run-tests suite result-counter)
            (counter-display-results    result-counter #t))
          ((if (break-on-error?) error warn)
           "HUH?  No tests found for:" suite-name)))
) )

(set! run-all-tests
  (lambda ()
    (let ( (result-counter (make-test-counter 'testing 0 0 0)) )
      (table-for-each
       (lambda (suite-name test-suite)
         (if test-suite
             (test-suite-run-tests test-suite result-counter)
             ((if (break-on-error?) error warn)
              "HUH?  No tests found for"
              suite-name)))
        (test-container-table **tests**))
      (counter-display-results result-counter #t)
) ) )

;; really internal to the macros
(set! test-db-add-test
  (lambda (suite-name test-case)
    (test-container-add-test **tests** suite-name test-case)))

(init-test-db)

(define (equivalent-alist? l1 l2)
  (and (list? l1)
       (list? l2)
       (= (length l1) (length l2))
       (every?
        (lambda (bucket)
          (if (not (pair? bucket))
              #f
              (let ( (probe (assq (car bucket) l2)) )
                (and (pair? probe)
                     (equal? (car probe) (car bucket))
                     (equal? (cdr probe) (cdr bucket))))))
        l1))
)

;;) ;; end (let () ..);; end-module testing

;===========================E=O=F==============================
