;;; FILE: "sis-tests.sch"
;;; IMPLEMENTS: SiS unit test admin
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'sis) ;; "sis.sch"

;; Nota Bene: larceny -r7r6 ...

(import (rnrs)
        (kend simple-regression-testing))

(define (test-files)
  (map (lambda (file-name)
         (string-append st-root-directory-prefix file-name "-tests.sch"))
       st-bootstrap-files)
)

(remove-all-test-suites) ;; start afresh

(for-each load (test-files))

(break-on-test-error? #f)
(verbose-test-output? #f)

;; (run-all-tests)

(newline)
(display "Don't forget to (run-all-tests)")
(newline)



;;;			--- E O F ---			;;;
