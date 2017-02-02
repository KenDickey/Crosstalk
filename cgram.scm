; Author: Mohd Hanafiah Abdullah (napi@cs.indiana.edu or napi@ms.mimos.my)
;
; f-f-d.s
;
; Computation of the LL(1) condition, LL(1) director sets,
; and FIRST and FOLLOW sets.
;
; Grammars are represented as a list of entries, where each
; entry is a list giving the productions for a nonterminal.
; The first entry in the grammar must be for the start symbol.
; The car of an entry is the nonterminal; the cdr is a list
; of productions.  Each production is a list of grammar symbols
; giving the right hand side for the production; the empty string
; is represented by the empty list.
; A nonterminal is represented as a Scheme symbol.
; A terminal is represented as a Scheme string.
;
; Example:
;
;  (define g
;    '((S ("id" ":=" E "\;")
;         ("while" E S)
;         ("do" S A "od"))
;      (A ()
;         (S A))
;      (E (T E'))
;      (E' () ("+" T E') ("-" T E'))
;      (T (F T'))
;      (T' () ("*" F T') ("/" F T'))
;      (F ("id") ("(" E ")"))))

; Given a grammar, returns #t if it is LL(1), else returns #f.

(define (LL1? g)
  (define (loop dsets)
    (cond ((null? dsets) #t)
          ((disjoint? (cdr (car dsets))) (loop (cdr dsets)))
          (else (display "Failure of LL(1) condition ")
                (write (car dsets))
                (newline)
                (loop (cdr dsets)))))
  (define (disjoint? sets)
    (cond ((null? sets) #t)
          ((null? (car sets)) (disjoint? (cdr sets)))
          ((member-remaining-sets? (caar sets) (cdr sets))
           #f)
          (else (disjoint? (cons (cdr (car sets)) (cdr sets))))))
  (define (member-remaining-sets? x sets)
    (cond ((null? sets) #f)
          ((member x (car sets)) #t)
          (else (member-remaining-sets? x (cdr sets)))))
  (loop (director-sets g)))

; Given a grammar, returns the director sets for each production.
; In a director set, the end of file token is represented as the
; Scheme symbol $.

(define (director-sets g)
  (let ((follows (follow-sets g)))
    (map (lambda (p)
           (let ((lhs (car p))
                 (alternatives (cdr p)))
             (cons lhs
                   (map (lambda (rhs)
                          (let ((f (first rhs g '())))
                            (if (member "" f)
                                (union (lookup lhs follows)
                                       (remove "" f))
                                f)))
                        alternatives))))
         g)))

; Given a string of grammar symbols, a grammar, and a list of nonterminals
; that have appeared in the leftmost position during the recursive
; computation of FIRST(s), returns FIRST(s).
; In the output, the empty string is represented as the Scheme string "".
; Prints a warning message if left recursion is detected.

(define (first s g recursion)
  (cond ((null? s) '(""))
        ((memq (car s) recursion)
         (display "Left recursion for ")
         (write (car s))
         (newline)
         '())
        ((and (null? (cdr s)) (string? (car s))) s)
        ((and (null? (cdr s)) (symbol? (car s)))
         (let ((p (assoc (car s) g))
               (newrecursion (cons (car s) recursion)))
           (cond ((not p)
                  (error "No production for " (car s)))
                 (else (apply union
                              (map (lambda (s) (first s g newrecursion))
                                   (cdr p)))))))
        (else (let ((x (first (list (car s)) g recursion)))
                (if (member "" x)
                    (append (remove "" x)
                            (first (cdr s) g recursion))
                    x)))))

; Given a grammar g, returns FOLLOW(g).
; In the output, the end of file token is represented as the Scheme
; symbol $.
; Warning messages will be printed if left recursion is detected.

(define (follow-sets g)
  
  ; Uses a relaxation algorithm.
  
  (define (loop g table)
    (let* ((new (map (lambda (x) (cons x (fol x g table)))
                     (map car g)))
           (new (cons (cons (caar new) (union '($) (cdar new)))
                      (cdr new))))
      (if (equal-table? table new)
          table
          (loop g new))))
  
  ; Given a nonterminal, a grammar, and a table giving
  ; preliminary follow sets for all nonterminals, returns
  ; the next approximation to the follow set for the given
  ; nonterminal.
  
  (define (fol x g t)
    (define (fol-production p)
      (let ((lhs (car p))
            (alternatives (cdr p)))
        (do ((l alternatives (cdr l))
             (f '() (union (fol-alternative x (car l)) f)))
            ((null? l)
             (if (member "" f)
                 (union (lookup lhs t)
                        (remove "" f))
                 f)))))
    (define (fol-alternative x rhs)
      (cond ((null? rhs) '())
            ((eq? x (car rhs))
             (union (first (cdr rhs) g '())
                    (fol-alternative x (cdr rhs))))
            (else (fol-alternative x (cdr rhs)))))
    (apply union (map fol-production g)))
  
  (loop g
        (cons (list (caar g) '$)
              (map (lambda (p) (cons (car p) '()))
                   (cdr g)))))

; Tables represented as association lists using eq? for equality.

(define (lookup x t)
  (cdr (assq x t)))

(define (equal-table? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y)) #f)
        (else (let ((entry (assoc (caar x) y)))
                (if entry
                    (and (equal-as-sets? (cdr (car x)) (cdr entry))
                         (equal-table? (cdr x) (remove entry y)))
                    #f)))))

; Sets represented as lists.

(define (equal-as-sets? x y)
  (and (every? (lambda (a) (member a y)) x)
       (every? (lambda (a) (member a x)) y)))

(define (union . args)
  (define (union2 x y)
    (cond ((null? x) y)
          ((member (car x) y)
           (union (cdr x) y))
          (else (cons (car x)
                      (union (cdr x) y)))))
  (cond ((null? args) '())
        ((null? (cdr args)) (car args))
        ((null? (cddr args)) (union2 (car args) (cadr args)))
        (else (union2 (union2 (car args) (cadr args))
                      (apply union (cddr args))))))

(define (every? p? l)
  (cond ((null? l) #t)
        ((p? (car l)) (every? p? (cdr l)))
        (else #f)))

 (define remove
   (lambda (item ls)
    (cond
       ((null? ls) '())
       ((equal? (car ls) item) (remove item (cdr ls)))
       (else (cons (car ls) (remove item (cdr ls)))))))
 
  (define pp-director-sets
    (lambda (g)
      (pp (director-sets g))))
    
  (define pp-follow-sets
    (lambda (g)
      (pp (follow-sets g))))

;;;			--- E O F ---			;;;
