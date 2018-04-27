;;; FILE: "sis.scm"
;;; IMPLEMENTS: Crosstalk: Smalltalk in Scheme -- kernel ST bootstrap
;;; AUTHOR: Ken Dickey
;;; DATE: 14 May 2016

; Implementation Language/platform: Gambit R5RS Scheme
;   http://gambitscheme.org/
;   https://github.com/gambit/gambit

; ANSI Smalltalk
;   https://en.wikipedia.org/wiki/Smalltalk
;   http://object-arts.com/downloads/papers/AnsiSmalltalkDraft1-9.pdf

;; Run from command line via "gsi", then
;;      (load "gambit-sis.scm")
;;      (load-source-bootstrap)

;; Scheme core unit tests:
;;      (load "sis-tests.scm")
;;          (run-source-tests)

;;             (add-st-kernel)
;; (add-and-run-st-unit-tests)

;; (display-subclasses Object)

;; (require: format/srfi-48)

;; (import
;;     (rnrs hashtables)
;;     (rnrs sorting)
;;     (rnrs io ports)
;;     (rnrs io simple)
;;     (rnrs files)
;;     (rnrs syntax-case)
;;     (rnrs arithmetic bitwise) ;; st-number.scm
;;     (scheme char)
;;     (scheme inexact)
;;     (scheme complex)
;;     (scheme time)
;;     (primitives
;;        structure-printer structure? make-structure record?
;;        vector-like-ref vector-like-set! vector-like-length
;;        typetag typetag-set!
;;        load compile-file
;;        object-hash
;;        equal-hash string-hash symbol-hash
;;        procedure-name procedure-name-set! procedure-arity
;;        port-position port-has-set-port-position!?
;;        ratnum?
;;        current-directory
;;        system ;; for directory creation
;;        getprop putprop ;; for symbol-value
;;        )
;;     (srfi :48) ;; intermediate-format-strings
;;     (only (srfi :13) ;; String library (see "st-string.scm")
;;           string-contains string-contains-ci)
;;     (srfi :19) ;; date and time (see "st-data-time.scm")
;;     (srfi :27) ;; random-integer
;;     ;; For st-eval (in "st-xlate.scm")
;;     (scheme eval) 
;;     (scheme repl) ; (interaction-environment)
;;     (rnrs conditions)
;; ;;  (rnrs records syntactic)
;;     (rnrs records procedural)
;;     (rnrs records inspection)
;; )

;; keywords interfere with Crosstalk symbol usage
(input-port-readtable-set!
 (repl-input-port)
 (readtable-keywords-allowed?-set (current-readtable) #f))

(current-readtable
 (readtable-keywords-allowed?-set (current-readtable) #f))



;; Helpers

(define (list-copy to-copy)
  (let loop ( (so-far to-copy) (result '()) )
    (if (null? so-far)
	(reverse result)
	(loop (cdr so-far) (cons (car so-far) result)))
) )

(load "sort.scm") ;; list-sort, vector-sort

(define (every? proc? list)
  (if (null? list)
      #t
      (and (proc? (car list))
           (every? proc? (cdr list)))))

(define (any? proc? list)
  (if (null? list)
      #f
      (or (proc? (car list))
          (any? proc? (cdr list)))))

(define-macro (unless test . body)
  `(if ,test #f (begin ,@body)))

(define-macro (when test . body)
  `(if ,test (begin ,@body)))

(define-macro (import . ignored) #f) ;; ignore R7RS imports

(define (make-eq-hashtable) (make-table (string->keyword "test") eq?))
(define hashtable-ref    table-ref)
(define hashtable-set!   table-set!)
(define hashtable-length table-length)
(define hashtable?       table?)

(define make-bytevector   make-u8vector)
(define bytevector?       u8vector?)
(define bytevector-ref    u8vector-ref)
(define bytevector-set!   u8vector-set!)
(define bytevector-length u8vector-length)
(define list->bytevector  list->u8vector)
(define bytevector->list  u8vector->list)

(define string-hash string=?-hash)


;;;

(define scm-root-directory-prefix ".") ;; "/home/kend/Crosstalk")

(define st-kernel-prefix
  (string-append scm-root-directory-prefix
                 "/SmalltalkKernel/"))

(define st-unit-test-prefix
  (string-append scm-root-directory-prefix
                 "/UnitTests/"))

(define temp-dir-prefix
  (string-append scm-root-directory-prefix "/Temp/"))



(define scm-bootstrap-file-names
  '( "gambit-st-kernel" ;; message mechanics
     "st-object"       ;; Object behavior
     "st-core-classes" ;; Object Class MetaClass ClassDescription Behavior
     "st-boolean"      ;; Boolean True False UndefinedObject (nil)
     "st-character"    ;; Character
     "st-magnitude"
     "st-number"
     "st-collection"
     "st-string"       ;; String
     "st-symbol"       ;; Symbol
     "st-list"         ;; proper, immutable lists (interoperate w Scheme)
     "st-blockClosure" ;; BlockClosure
     "st-array"        ;; Array
     "st-error-obj"    ;; Scheme error objects
     "st-tokenizer"    ;; Stream -> tokens
     "st-parse"        ;; tokens -> AST
     "st-xlate"	       ;; AST -> Scheme
     "st-stream"       ;; Stream CharStream ByteStream
     "st-set"          ;; Set
     "st-dictionary"   ;; Dictionary
     "st-date-time"    ;; PointInTime Duration DateAndTime
     "st-conditions"   ;; Map Scheme Conditions to St Exceptions
    )
 )

(define (source-scm-file-names)
  (map (lambda (file-name)
         (string-append scm-root-directory-prefix "/" file-name ".scm"))
       scm-bootstrap-file-names)
)

(define st-bootstrap-file-names
  '( "Object"
     "Behavior"
     "Association"
     "Collection"
     "SequenceableCollection"
     "ArrayedCollection"
     "Dictionary"
     "OrderedCollection"
     "SortedCollection"
     "String"
     "Character"
     "Interval"
     "Point"
     "ValueLink"
     "LinkedList"
     "Time"
     "TimeSpan"
     "WeakSend"  ;; WeakMessageSend, WeakActionSequence
     "Events"   ;; ActiveModel; when:send:to:
     "SUnit" 
     "Numbers"
     ;;    @@@more to come...
    )
 )


(define st-unit-test-file-names
  '( "SUnitTests"
     "ExceptionTests"
     "IntervalTests"
;; More to come..     
) )


(define (xlate-st-file fname)
  (format #t "~%St->Scm translate ~a" fname)
  (xlate-st-file->scm-file
   (string-append st-kernel-prefix "/" fname ".st")
   (string-append temp-dir-prefix "/" fname ".scm"))
)
    
(define (xlate-st-unit-test-file fname)
  (format #t "~%St->Scm translate ~a" fname)
  (xlate-st-file->scm-file
   (string-append st-unit-test-prefix "/" fname ".st")
   (string-append temp-dir-prefix "/" fname ".scm"))
)

(define (compiled-file-names)
  (map (lambda (file-name)
         (string-append scm-root-directory-prefix "/" file-name ".fasl"))
       scm-bootstrap-file-names)
)

(define (remove-compiled)
  (for-each delete-file (compiled-file-names)))

(define (compile-bootstrap)
  (for-each (lambda (fn) (compile-file fn)) (source-scm-file-names)))

(define (load-source-bootstrap)
  (for-each load (source-scm-file-names)))

(define (load-compiled-bootstrap)
  (for-each load (compiled-file-names)))

(define (xlate-st-bootstrap)
  (for-each xlate-st-file  st-bootstrap-file-names))

(define (load-st-bootstrap)
  (format #t "~%Loading translated files:~%")
  (for-each
   (lambda (fn)
     (format #t "~t~a~%" fn)
     (load (string-append temp-dir-prefix fn ".scm")))
   st-bootstrap-file-names))

(define (xlate-st-unit-tests)
  (for-each xlate-st-unit-test-file st-unit-test-file-names))

(define (load-st-unit-tests)
  (format #t "~%Loading translated files:~%")
  (for-each
   (lambda (fn)
     (format #t "~t~a~%" fn)
     (load (string-append temp-dir-prefix fn ".scm")))
   st-unit-test-file-names))

;; By convention, each UnitTest suite has a 'Run' block 
;; which corresponds to its name symbol in Smalltalk.
;; E.g. "SUnitTests.st" -> #RunSUnitTests
(define (run-st-unit-tests)
  (for-each
   (lambda (name)
     (let ( (st-runCommand
             (format #f "(Smalltalk at: #Run~a) value." name))
          )
       (format #t "~%~%~a~%" st-runCommand)
       (st-eval st-runCommand)))
   st-unit-test-file-names)
)



;;;

(define (add-st-kernel)
  (xlate-st-bootstrap)
  (load-st-bootstrap))

(define (add-and-run-st-unit-tests)
  (xlate-st-unit-tests)
  (load-st-unit-tests)
  (run-st-unit-tests))



;; for send-failed (see st-err-obj.scm)
(define %%escape%% (make-parameter (lambda whatever '|%%escape%%|)))


(define debug-st-runtime (make-parameter #f))

;; Make sure we have a Temp dir;; ignore if exists
(shell-command (string-append "mkdir -p " temp-dir-prefix))

;;;			--- E O F ---			;;;
