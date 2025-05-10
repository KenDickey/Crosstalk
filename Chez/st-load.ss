
(compile-imported-libraries #t)
(print-length 30)
(print-level 6)

(define st-base-lib-name "st-core-mechanics.sls")

(define st-files
  (list ;; order matters
   "st-core-classes"
   "st-create-subclass"
   "st-core-methods"
   "st-boolean"
   "st-collection"
   "st-array"
   "st-list"
   "st-blockClosure"
   "st-character"
   "st-string"
   "st-symbol"
   "st-number"
   "st-error"
   "st-dictionary"
   "st-stream"
   "st-date-time"
   "st-tokenizer"
   "st-parser"
   "st-xlate"
   ) )

(define (loadss base-name)
  (format #t "~%About to load ~a.ss" base-name)
  (load (string-append base-name ".ss")))

(begin
  (for-each loadss st-files)
  (newline))

(import (simple-regression-testing))
(verbose-test-output? #f)
(break-on-test-error? #f)

(define (test-from core-nameSym)
  (load (string-append
         "st-"
         (symbol->string core-nameSym)
         "-tests.ss")))

(define test-names
  '(
    base
    metaclass
    object
    boolean
    number
    character
    string
    symbol
    array
    list
    set
    stream
;   blockClosure
;    error
;    condition
    dictionary
    tokenizer
    parser
    xlate
)  )

;; (for-each test-from test-names)
;; (run-all-tests)

;; (describe obj)
;; (display-obj obj)
;; (display-selectors obj)
;; (display-ivars class)
;; (inst-method-names class)
;; (display-subclasses class)
;; (display-allSupers class)
;; (selectors class)
;; (hashtable-keys mdict)
;; (name obj) ; (printString obj)
;; (map name (allInstVarNames class))
;; (map name (allSuperclasses class))
;; (map name (allSubclasses class))

(define source-names st-files)

(define scm-root-directory-prefix ".") ;; "/home/kend/Crosstalk")

(define st-kernel-prefix
  (string-append scm-root-directory-prefix
                 "/../SmalltalkKernel/"))

(define st-unit-test-prefix
  (string-append scm-root-directory-prefix
                 "/UnitTests/"))

(define temp-dir-prefix
  (string-append scm-root-directory-prefix "/../Temp/"))

;; xlate uses pretty-print, so
(print-level 100)


(define (xlate-st-file fname)
  (format #t "~%St->Scm translate ~a" fname)
  (xlate-st-file->scm-file
   (string-append st-kernel-prefix  fname ".st")
   (string-append temp-dir-prefix   fname ".ss"))
)


;;;			--- E O F ---			;;;
