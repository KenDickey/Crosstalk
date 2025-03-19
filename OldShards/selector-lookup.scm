;;; FILE: "selector-lookup.scm"
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
;;
;; E.g.  (#f ==> unused)
;;  Logical    Actual   ..(hole-index num-adjacent-holes)..
;; 0 |  #f |   | --> | ((5 . 1) (9 . 2) (15 . 0))
;; 1 |  m1 |   |  m1 |
;; 2 |  m2 |   |  m2 |
;; 3 |  m3 |   |  m3 |
;; 4 |  m4 |   |  m4 |
;; 5 |  #f |   |  m7 |
;; 6 |  #f |   |  m8 |
;; 7 |  m7 |   | m12 |
;; 8 |  m8 |   | m13 |
;; 9 |  #f |   | m14 |
;;10 |  #f |   | m16 |
;;11 |  #f |
;;12 | m12 |
;;13 | m13 |
;;14 | m14 |
;;15 |  #f |
;;16 | m16 |
;;
;; Tuning: Could use method Object>>doesNotUnderstand: in unused
;; slots, only condensing when the number of unused 'holes'
;; exceeds a given threshold.
;;
;; Alt: Check against Larceny's optimized case dispatch.
;; Alt: Large # of gaps -> binary search

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

(define (selector+table->method sym vec)
  (let ( (idx (selector->id sym)) )
    (if (not idx)
        doesNotUnderstand:
        (let loop ( (hole-count 0)
                    (holes-alist (vector-ref vec 0))
                  )
          (cond
           ((null? holes-alist)
            (let ( (index (- idx hole-count)) )
              (if (< index  (vector-length vec))
                (vector-ref vec index)
                doesNotUnderstand:)
            ))
           (else
            (let  ( (next-hole    (caar holes-alist))
                    (num-adjacent (cdar holes-alist))
                  )
              (cond
               ((< idx next-hole)
                (vector-ref vec (- idx hole-count))
                )
               ((<= idx (+ next-hole num-adjacent))
                doesNotUnderstand: ;; not in table
                )
               (else (loop
                      (+ hole-count 1 num-adjacent) 
                      (cdr holes-alist)))))))
) ) ) )

;;;=================Quick Checks===============;;;

;; (define doesNotUnderstand: 'doesNotUnderstand: )

;; (map assign-id-to-selector
;;      '(m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17))

;; (define mvec (vector '((5 . 1) (9 . 2) (15 . 0))
;;                      'm1 'm2 'm3 'm4 'm7 'm8 'm12 'm13 'm14 'm16))

;; (map (lambda (s) (selector+table->method s mvec))
;;      '(m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17))

;;;;  Expect:
;;'(m1
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
(define (add-selector+method-to-mvec selector method mvec)
  ;; (unless (and (symbol? selector)
  ;;              (procedure? method)
  ;;              (vector? mvec))
  ;;   (error @@FIXME: checks@@)
  (let ( (idx (assign-id-to-selector selector))
         (mvec-len (vector-length mvec))
       )
    (let loop ( (hole-count 0)
                (holes-alist (vector-ref mvec 0)) ;; FIXME: checks
              )
      (cond
       ((null? holes-alist) ;; go ahead and add
        (let ( (index (- idx hole-count)) )
          (if (< index mvec-len)
              ;; replace
              (vector-set! mvec index method)
              ;; add at end 
              (let ( (result-vec 
                      (vector-add-at-end method))
                     )
                (cond
                 ((= index mvec-len)
                  result-vec  ;; no change to holes-alist
                  )
                 (else
                  (vector-set! result-vec
                               0
                               (append ;; add to end of alist
                                (vector-ref mvec 0)
                                (list
                                 (cons
                                  ;; new hole
                                  mvec-len
                                  ;; num adjacent holes
                                  (- index mvec-len 1)
                                  ))))
                  result-vec)
                 )))
          ) )
       (else
        (let  ( (next-hole    (caar holes-alist))
                (num-adjacent (cdar holes-alist))
              )
          (cond
           ((< index next-hole)
            (vector-ref vec (- idx hole-count))
            )
           ((<= idx (+ next-hole num-adjacent))
            doesNotUnderstand: ;; not in table
            )
           (else (loop
                  (+ hole-count 1 num-adjacent) 
                  (cdr holes-alist)))))))

) ) ) )
    

;;	--- E O F ---	;;
