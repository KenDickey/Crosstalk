
;;; FILE: "selector-lookup2.scm"
;;; IMPLEMENTS: Selector->Method lookup strategy
;;; AUTHOR: Ken Dickey
;;; DATE: 10 December 2017

;; Strategy for selector lookup
;;
;; In place of a Class or hash-table, each St object has a slot with
;; a vector of closures which are its methods.
;;
;; Selectors are assigned an id when first used to refer to a method.
;; The id is used to find the appropriate method in the method vector.
;; Holes in the vector are kept track of in slot zero.
;;
;; No hashes.  No caches.  Holes elided.

;; E.g.  (#f ==> unused)
;;
;;  Logical      Table     Actual   
;;    _____  
;; 0 |  #f |    1  4  1     _____
;; 1 |  m1 |    7  8  3   0|  m1 |
;; 2 |  m2 |   12 14  6   1|  m2 |
;; 3 |  m3 |   16 16  7   2|  m3 |
;; 4 |  m4 |  min max sub 3|  m4 |
;; 5 |  #f |              4|  m7 |
;; 6 |  #f |              5|  m8 |
;; 7 |  m7 |              6| m12 |
;; 8 |  m8 |              7| m13 |
;; 9 |  #f |              8| m14 |
;;10 |  #f |              9| m16 |
;;11 |  #f |                -----
;;12 | m12 |
;;13 | m13 |
;;14 | m14 |
;;15 |  #f |
;;16 | m16 |
;;    ----- 

;; Dispatch function refers adjacent used idx locations
;;  1st-used last-used num-to-subtract
;;
;; Search for selection index
;; -> (min <= idx <= max)
;;       then index = (idx - sub)
;;       else hole => doesNotUnderstand:

;; FIXME: Binary search when table exceeds threshold

;;  (import (primitives getprop putprop))

(define (make-selector-table) (vector nil))

(define assign-id-to-selector ;; return the selector id
  (let ( (counter 0) )
    (lambda (sym)
      (cond
       ((not (symbol? sym))
        doesNotUnderstand:
        )
       ((selector->id sym)) ;; Answer the index
       (else
        (set! counter (+ 1 counter))
        (set-selector-index! sym counter)
        counter)
) ) ) )
         

(define (selector->id sym)
  (getprop sym '%%method-index%%))

(define (set-selector-index! sym idx)
  (putprop sym '%%method-index%% idx))


;; linear search
(define (selector+table->method sym table-as-list mvec)
  (let ( (idx (selector->id sym)) )
    (if (not idx)
        doesNotUnderstand:
        (let search ( (bucket-list table-as-list) )
          (if (null? bucket-list)
              doesNotUnderstand:
              (let ( (bucket (car bucket-list)) )
                ;; (display idx) (display " ")
                ;; (display bucket) (newline)
                (cond
                 ((< idx (car bucket))
                  doesNotUnderstand:
                  )
                 ((<= idx (cadr bucket))
                  ;; (display (- idx (cddr bucket))) (newline)
                  (vector-ref mvec (- idx (cddr bucket)))
                  )
                 (else
                  (search (cdr bucket-list)))))
       ) )
) ) )

;;;=================Quick Checks===============;;;

(define doesNotUnderstand: 'doesNotUnderstand: )

(map assign-id-to-selector
     '(m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17))

(define mvec (vector 'm1 'm2 'm3 'm4 'm7 'm8 'm12 'm13 'm14 'm16))

(define mvec-tab '((1 4 . 1) (7 8 . 3) (12 14 . 6) (16 16 . 7)))

(map (lambda (s) (selector+table->method s mvec-tab mvec))
     '(m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17))

;;  Expect:
;; '(doesNotUnderstand:
;;  m1
;;  m2
;;  m3
;;  m4
;;  doesNotUnderstand:
;;  doesNotUnderstand:
;;  m7
;;  m8
;;  doesNotUnderstand:
;;  doesNotUnderstand:
;;  doesNotUnderstand:
;;  m12
;;  m13
;;  m14
;;  doesNotUnderstand:
;;  m16
;;  doesNotUnderstand:)


;;; FIXME: Add & Remove Methods

;;	--- E O F ---	;;
