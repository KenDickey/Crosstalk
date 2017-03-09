;;; FILE: "st-conditions.scm"
;;; IMPLEMENTS: Mapping Scheme Conditions to St Exceptions
;;; See also "st-blockClosure.scm" and "st-error-obj.scm"
;;; AUTHOR: Ken Dickey
;;; DATE: 08 March 2017

;; (require 'st-error-obj)

;;; Scheme Conditions
;;
;; Nota Bene:
;;   Scheme compound-conditions differ from St ExceptionSets
;; They are more like a Bag.  No one-to-one mapping exists.
;; We do the simple thing here..

(define condition-transducers (make-eq-hashtable))

(define (asException condition)
  (let ( (exception-transducer
          (hashtable-ref
           	condition-transducers
                (condition-name condition)
                (lambda (self . ignored-rest)
                   (send-failed condition 'asException))))
       )
    (exception-transducer condition)))

(define (condition-name simple-condition)
  (record-type-name
   (record-rtd simple-condition)))

(define (condition-names condition)
  (map condition-name
       (simple-conditions condition)))

(define (simple-condition? condition)
  (and (condition? condition)
       (= 1 (length (simple-conditions condition)))))

(define (condition-name->predicate-symbol simple-condition)
  (let loop ( (source-chars
               (string->list
                (symbol->string
                 (condition-name simple-condition))))
              (dest-chars '(#\s #\i))
              (char-case-fn char-upcase)
            )
    (cond
     ((null? source-chars)
      (string->symbol
       (list->string
        (reverse dest-chars)))
      )
     ((let ( (next-char (car source-chars)) )
        (not (or (char-alphabetic? next-char)
                 (char-numeric? next-char))))
      (loop (cdr source-chars)
            dest-chars
            char-upcase)
      )
     (else
      (loop (cdr source-chars)
            (cons (char-case-fn (car source-chars))
                  dest-chars)
            char-downcase)))))

(define (condition->dictionary condition)
  (let ( (dict (make-eq-hashtable))
         (conditions
          ;; In case of same field names,
          ;; first use of field name predominates,
          ;; so add back to front..
          (reverse (simple-conditions condition)))
       )
      (for-each
       (lambda (cdn)
         (let ( (rtd (record-rtd cdn)) )
           (hashtable-set! dict
                           (condition-name->predicate-symbol cdn)
                           #true)
           (let loop ( (field-names
                        (vector->list
                         (record-type-field-names rtd)))
                       (index 0)
                     )
             (if (>= index (length field-names))
                 dict
                 (begin
                   (hashtable-set! dict
                                   (car field-names)
                                   ((record-accessor rtd index) cdn))
                   (loop (cdr field-names) (+ index 1))))
         ) ) )
         conditions)
      dict)
)

;;; Conditions

;; &condition condition simple-conditions condition?
;; condition-predicate condition-accessor

;; &message make-message-condition message-condition? condition-message

;; &warning make-warning warning?

;; &serious make-serious-condition serious-condition?

;; &error make-error error?

;; &violation make-violation violation?

;; &assertion make-assertion-violation assertion-violation?

;; &irritants make-irritants-condition irritants-condition? condition-irritants

;; &who make-who-condition who-condition? condition-who

;; &non-continuable make-non-continuable-violation non-continuable-violation?

;; &implementation-restriction make-implementation-restriction-violation
;; implementation-restriction-violation?

;; &lexical make-lexical-violation lexical-violation?

;; &syntax make-syntax-violation syntax-violation?
;; syntax-violation-form syntax-violation-subform

;; &undefined make-undefined-violation undefined-violation?

;; &i/o make-i/o-error i/o-error?

;; &i/o-read make-i/o-read-error i/o-read-error?

;; &i/o-write make-i/o-write-error i/o-write-error?

;; &i/o-invalid-position make-i/o-invalid-position-error
;; i/o-invalid-position-error? i/o-error-position

;; &i/o-filename make-i/o-filename-error i/o-filename-error?
;; i/o-error-filename

;; &i/o-file-protection make-i/o-file-protection-error
;; i/o-file-protection-error?

;; &i/o-file-is-read-only make-i/o-file-is-read-only-error
;; i/o-file-is-read-only-error?

;; &i/o-file-already-exists make-i/o-file-already-exists-error
;; i/o-file-already-exists-error?

;; &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
;; i/o-file-does-not-exist-error?

;; &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

;; &i/o-decoding make-i/o-decoding-error i/o-decoding-error?

;; &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
;; i/o-encoding-error-char

;; &no-infinities make-no-infinities-violation no-infinities-violation?

;; &no-nans make-no-nans-violation no-nans-violation?)



;|============================================================|
;; Conditions don't carry slots for st Exception values.
;; Idea: use a compound condition with added st-condition
;; Idea: 'asException -> transduce into St object as required
;; ?? keep condition (for Scm) but add St Exception context ??
;; ?? Use a compound condition to add Exception's extra ivars?

;; (define-condition-type &st &condition
;;   make-st-condition st-condition?
;;   (obj st-obj))

;; (st-obj (make-st-condition (vector 1 2 3))) ;; --> #(1 2 3)

;; (st-obj
;;     (condition
;;        zero-divide
;;        cfrob
;;        (make-st-condition (vector 1 2 3)))) ;; --> #(1 2 3)

;; (condition-message
;;     (condition
;;        zero-divide
;;        (make-st-condition (vector 1 2 3)))) ;; --> "/: zero divisor: 3 0 \n"

;; (provide 'st-conditions)

;;;			--- E O F ---			;;;
