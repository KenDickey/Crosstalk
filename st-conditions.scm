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
          (hashtable-ref condition-transducers
                 symbol
                 (lambda (self . ignored-rest)
                   (send-failed condition 'asException))))
         )
    (exception-transducer condition)))

(define (condition-name->predicate-symbol simple-condition)
  (let loop ( (source-chars
               (string->list
                (symbol->string
                 (record-type-name
                  (record-rtd simple-condition)))))
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
          ;; first use of name predominates..
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


;;;            (record-type-name rtd)


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
