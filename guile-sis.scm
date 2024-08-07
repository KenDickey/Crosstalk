;;; FILE: "guile-sis.scm"
;;; IMPLEMENTS: Crosstalk: Smalltalk in Scheme -- kernel ST bootstrap
;;; AUTHOR: Ken Dickey
;;; DATE:   17 May 2024

; Implementation Language/platform: Guile Scheme: Guile 3.0.7
;   https://www.gnu.org/software/guile/
;   https://www.gnu.org/software/guile/learn/

; ANSI Smalltalk
;   https://en.wikipedia.org/wiki/Smalltalk
;   http://object-arts.com/downloads/papers/AnsiSmalltalkDraft1-9.pdf

;; Run from command line via `guile`, then
;;      (load "guile-sis.scm")
;;      (load-source-bootstrap)

;; Scheme core unit tests:
;;      (load "guile-sis-tests.scm")
;;      (break-on-test-error? #f) ;; test all
;;      (run-source-tests)

;; Smalltalk
;;  (add-st-kernel)
;;  (add-and-run-st-unit-tests)

;; (display-subclasses Object)


(import
;;    (rnrs hashtables) NO; use guile natives
    (rnrs sorting)
    (rnrs io ports)
    (rnrs io simple)
    (rnrs files)
    (rnrs syntax-case)
    (rnrs arithmetic bitwise) ;; st-number.scm
    (rnrs bytevectors)
;;    (scheme base)
;;    (scheme char)
;;    (scheme inexact)
;;    (scheme complex)
    (scheme time)
    ;; (primitives
    ;;    structure-printer structure? make-structure record?
    ;;    vector-like-ref vector-like-set! vector-like-length
    ;;    typetag typetag-set!
    ;;    load compile-file
    ;;    object-hash
    ;;    equal-hash string-hash symbol-hash
    ;;    procedure-name procedure-name-set! procedure-arity
    ;;    port-position port-has-set-port-position!?
    ;;    rational?
    ;;    current-directory
    ;;    file-exists?
    ;;    system ;; for directory creation
    ;;    getprop putprop ;; for symbol-value
    ;;    )
;;  (srfi :48) ;; intermediate-format-strings
    (only (srfi :13) ;; String library (see "st-string.scm")
          string-contains string-contains-ci)
    (srfi :19) ;; date and time (see "st-data-time.scm")
    (srfi :27) ;; random-integer
;;    (srfi :69) ;; hash-by-identity (eq? hash)
    ;; For st-eval (in "st-xlate.scm")
;;    (scheme eval) 
;;    (scheme repl) ; (interaction-environment)
;;  (rnrs conditions)
    (srfi :34)
    (srfi :35)
    (rnrs exceptions (6))
;; (use-modules (srfi srfi-35))
;;  (ice-9 exceptions)
;;  (rnrs records syntactic)
    (rnrs records procedural)
    (rnrs records inspection)
)

;;(load "apropos.scm") -- imports wrong (interaction-environment)

(load "define-structure.scm")

;; Helpers

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

(define (delete-file-if-exists path)
  (if (file-exists? path)
      (delete-file path)))

;;; R7RS bytevector accessors named differently

(define bytevector-ref  bytevector-u8-ref)
(define bytevector-set! bytevector-u8-set!)

(define (list->bytevector list-of-bytes)
  (unless (list? list-of-bytes)
    (error "list->bytevector requires a list of bytes"
           list-of-bytes))
  (unless (every? (lambda (b) (<= 0 b 256)) list-of-bytes)
    (error "list->bytevector requires a list of bytes"
           list-of-bytes))
  (let* ( (bvec-len (length list-of-bytes))
          (bvec (make-bytevector bvec-len 0))
        )
    (let loop ( (index 0) (byte-list list-of-bytes) )
      (if (>= index bvec-len)
          bvec
          (begin
            (bytevector-set! bvec index (car byte-list))
            (loop (+ index 1) (cdr byte-list))))))
)

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


(define scm-raw-bootstrap-file-names
  '( "guile-st-kernel"       ;; message mechanics
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

(define scm-bootstrap-file-names scm-raw-bootstrap-file-names)

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
  (for-each delete-file-if-exists (compiled-file-names)))

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

(define structure-printer-set #false) ;; to set once

(define debug-st-runtime (make-parameter #false))

;; Make sure we have a Temp dir
(system (string-append "mkdir -p "
                       temp-dir-prefix))


(define-syntax capture-condition ;; used in st-conditions-tests.scm
  (syntax-rules ()
    ((capture-condition form)
     (call/cc
      (lambda (exit)
        (with-exception-handler
         (lambda (c) (exit c))
         (lambda ()  form)))))))


;;;			--- E O F ---			;;;
