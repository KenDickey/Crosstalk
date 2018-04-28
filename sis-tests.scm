;;; FILE: "sis-tests.scm"
;;; IMPLEMENTS: SiS unit test admin
;;; AUTHOR: Ken Dickey
;;; DATE: 18 May 2016

;; (require 'sis) ;; "sis.scm"

;; Nota Bene: larceny -r7r6 ...

(import 
        (primitives load compile-file procedure-name procedure-name-set!)
        (kend simple-regression-testing))

(define (source-test-file-names)
  (map (lambda (file-name)
         (string-append scm-root-directory-prefix "/" file-name "-tests.scm"))
       scm-raw-bootstrap-file-names)
)

(define (run-source-tests)
  (call/cc
   (lambda (exit)
     (parameterize ( (%%escape%% exit) )
       (remove-all-test-suites) ;; start afresh
       (for-each load (source-test-file-names))
       (run-all-tests))))
)

;; (define (compile-tests)
;;   (for-each compile-file (source-test-file-names)))

;; (define (compiled-test-file-names)
;;   (map (lambda (file-name)
;;          (string-append st-root-directory-prefix file-name "-tests.fasl"))
;;        st-bootstrap-file-names)
;; )
  
;; (define (run-compiled-tests)
;;   (remove-all-test-suites) ;; start afresh
;;   (for-each load (compiled-test-file-names))
;;   (run-all-tests)
;; )

;;;			--- E O F ---			;;;
