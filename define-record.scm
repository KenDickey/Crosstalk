(import (rnrs syntax-case))

;; (define-structure name (field ...))

;; (define-structure (Block args temps seq))
;; (define b (Block '(1 2) '(a b) '((1)(2)(3))))
;;  (Block-args b) --> (1 2)
;;  (Block-seq  b) --> ((1) (2) (3))
;;  b -->  #(Block (1 2) (a b) ((1) (2) (3)))

;; This is an altered form of an example in
;; _The Scheme Programming Language_, 4th Ed.
;; by R Kent Dyvbig

(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
          (string->symbol
            (apply string-append
              (map (lambda (x)
                     (if (string? x)
                         x
                         (symbol->string (syntax->datum x))))
                   args))))))
    (syntax-case x ()
      [(_ (name field ...))
       (with-syntax ([constructor #'name]
                     [predicate (gen-id #'name #'name "?")]
                     [(access ...)
                      (map (lambda (x) (gen-id x #'name "-" x))
                           #'(field ...))]
                     [(assign ...)
                      (map (lambda (x)
                             (gen-id x "set-" #'name "-" x "!"))
                           #'(field ...))]
                     [structure-length (+ (length #'(field ...)) 1)]
                     [(index ...)
                      (let f ([i 1] [ids #'(field ...)])
                        (if (null? ids)
                            '()
                            (cons i (f (+ i 1) (cdr ids)))))])
         #'(begin
             (define constructor
               (lambda (field ...)
                 (vector 'name field ...)))
             (define predicate
               (lambda (x)
                 (and (vector? x)
                      (= (vector-length x) structure-length)
                      (eq? (vector-ref x 0) 'name))))
             (define access
               (lambda (x)
                 (vector-ref x index)))
             ...
             (define assign
               (lambda (x update)
                 (vector-set! x index update)))
             ...))])))


;;;			--- E O F ---			;;;
