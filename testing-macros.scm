;;; FILE: "testing-macros.scm"
;;; IMPLEMENTS: testing macros [for Oops]
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
;;(include "../src/common-macros.scm")
;==============================================================;


(define-macro (add-test suite-name expect form equivalent? . message)
  (let ( (msg (if (pair? message) (car message) `',form)) )
    `(test-db-add-test ,suite-name
                       (make-test
                        ,expect
                        (lambda () ,form)
                        ,equivalent?
                        ,msg)
 ) ) )

(define-macro (add-eq-test suite-name expect form . message)
  (let ( (msg (if (pair? message) (car message) `',form)) )
    `(test-db-add-test ,suite-name
                       (make-test ,expect
                                  (lambda () ,form)
                                  eq?
                                  ,msg))
) )


(define-macro (add-equal-test suite-name expect form . message)
  (let ( (msg (if (pair? message) (car message) `',form)) )
    `(test-db-add-test ,suite-name
                       (make-test ,expect
                                  (lambda () ,form)
                                  equal?
                                  ,msg))
 ) )

(define-macro (add-equivalent-alist-test suite-name expect form . message)
  (let ( (msg (if (pair? message) (car message) `',form)) )
    `(test-db-add-test ,suite-name
		       (make-test ,expect
                                  (lambda () ,form)
                                  equivalent-alist?
                                  ,msg))
) )

(define-macro (ensure-exception-raised suite-name type-pred? form . message)
   (let ( (msg (if (pair? message) (car message) `',form)) )
     `(test-db-add-test ,suite-name
                        (make-test
                         'some-kind-of-exception
                         (lambda ()
                           (call-with-current-continuation
                            (lambda (return)
                              (with-exception-handler
                               (lambda (exn) (return exn)) ; capture & return
                               (lambda () ,form)))))
                         (lambda (actual ignored)
                           (,type-pred? actual))
                         ,msg))
) )


;===========================E=O=F==============================;
