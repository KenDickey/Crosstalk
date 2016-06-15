;;; FILE: "sis-tests.sch"
;;; IMPLEMENTS: SiS unit test admin
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'sis) ;; "sis.sch"

;; Nota Bene: larceny -r7r6 ...

(import 
        (primitives load compile-file procedure-name procedure-name-set!)
        (kend simple-regression-testing))

(break-on-test-error? #false)
(verbose-test-output? #false)

(define (source-test-files)
  (map (lambda (file-name)
         (string-append st-root-directory-prefix file-name "-tests.sch"))
       st-bootstrap-files)
)

(define (run-source-tests)
  (remove-all-test-suites) ;; start afresh
  (for-each load (source-test-files))
  (run-all-tests)
)

;; (define (compile-tests)
;;   (for-each compile-file (source-test-files)))

;; (define (compiled-test-files)
;;   (map (lambda (file-name)
;;          (string-append st-root-directory-prefix file-name "-tests.fasl"))
;;        st-bootstrap-files)
;; )
  
;; (define (run-compiled-tests)
;;   (remove-all-test-suites) ;; start afresh
;;   (for-each load (compiled-test-files))
;;   (run-all-tests)
;; )




;;;			--- E O F ---			;;;
