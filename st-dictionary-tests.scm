;;; IMPLEMENTS: Unit tests for st-dictionary.scm
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (require 'st-dictionary)

(define %%dict%% #f)
(define (setup-st-dictionary)
  (set! %%dict%% ($ Dictionary 'new))
  ($:: %%dict%% 'at:put: 'a 1)
  ($:: %%dict%% 'at:put: 'b 2)
  ($:: %%dict%% 'at:put: 'c 3)
)
(define (cleanup-st-dictionary)
  (set! %%dict%% #f)
)

;; NOTA BENE:
;; (hash-fold (lambda (k v a) (cons (cons k v) a)) '() %%dict%%)
;; $25 = ((a . 1) (c . 3) (b . 2))
;; scheme@(guile-user)> (hash-ref %%dict%% 'b)
;; $26 = 2
;; scheme@(guile-user)> (hashq-ref %%dict%% 'b)
;; $27 = #f

(add-test-suite 'st-dictionary
                setup-st-dictionary
                cleanup-st-dictionary)

(add-equal-test 'st-dictionary
  3
  ($ %%dict%% 'size)
  "size")

(add-equal-test 'st-dictionary
  2
  ($: %%dict%% 'at: 'b)
  "at:")

(add-equal-test 'st-dictionary
  '(b c a)
  (let ( (keys '()) )
    ($: %%dict%%
      'keysDo:
      (lambda (k)
        (set! keys (cons k keys))))
    keys)
  "keysDo:")

(add-equal-test 'st-dictionary
  6
  (let ( (total 0) )
    ($: %%dict%%
      'valuesDo:
      (lambda (v)
        (set! total (+ total v))))
     total)
  "valuesDo:")

(add-equal-test 'st-dictionary
  (vector '(a c b) 6)
  (let ( (keys '()) (total 0) )
    ($: %%dict%%
      'keysAndValuesDo:
      (lambda (k v)
        (set! keys (cons k keys))
        (set! total (+ total v))))
    (vector keys total))
  "keysAndValuesDo:")

;; (ensure-exception-raised 'st-dictionary
;;    (make-error-string-predicate   "Failed message send: #glerph to ")
;;    (perform: %%test-object 'glerph)
;;    "obj glerph -> doesNotUnderstand")


;;;			--- E O F ---			;;;

