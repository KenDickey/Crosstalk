;;; FILE: "apropos.scm"
;;; IMPLEMENTS: apropos function for Scheme globals
;;; AUTHOR: Ken Dickey
;;; DATE: 30 June 2016

;; Run from command line via "larceny -r7rs", then
;;   (import (scheme load))
;;   (load "apropos.scm")
;;
;; (apropos 'cons)
;; (apropos "char")

(import (primitives interaction-environment environment-variables)
	(rnrs sorting)
        )

(define (apropos string-or-symbol)
  (let ( (contains-substring?
          (substring-search-maker
           (if (symbol? string-or-symbol)
               (symbol->string string-or-symbol)
               string-or-symbol)))
         (env-syms (environment-variables (interaction-environment)))
       )
    (let loop ( (results '()) (syms env-syms) )
      (if (null? syms)
          (list-sort symbol<? results)
          (let ( (var-name (symbol->string (car syms))) )
            (cond
             ((contains-substring? var-name)
              (loop (cons (sanitize var-name) results) (cdr syms))
              )
             (else
              (loop results (cdr syms)))))
) ) ) )

(define (sanitize name-string)
  ;; NB: takes a string & returns a symbol
  (string->symbol 
   (if (< (char->integer (string-ref name-string 0)) 32)
       (substring name-string 1 (string-length name-string))
       name-string))
)

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))


;; SUBSTRING-SEARCH-MAKER takes a string (the "pattern") and returns a function
;; which takes a string (the "target") and either returns #f or the index in
;; the target in which the pattern first occurs as a substring.
;;
;; E.g. ((substring-search-maker "test") "This is a test string")  -> 10
;;      ((substring-search-maker "test") "This is a text string")  -> #f

; NOTES	
;	Based on "A Very Fast Substring Search Algorithm", Daniel M. Sunday,
;	CACM v33, #8, August 1990.

(define (substring-search-maker pattern-string)

  (define num-chars-in-charset 256)

  (define (build-shift-vector pattern-string)
    (let* ( (pat-len (string-length pattern-string))
  	    (shift-vec (make-vector num-chars-in-charset (+ pat-len 1)))
	    (max-pat-index (- pat-len 1))
          )
      (let loop ( (index 0) )
        (vector-set! shift-vec 
		     (char->integer (string-ref pattern-string index))
		     (- pat-len index)
        )
	(if (< index max-pat-index)
	    (loop (+ index 1))
	    shift-vec)
  ) ) )

  (let ( (shift-vec (build-shift-vector pattern-string))
	 (pat-len   (string-length pattern-string))
       )

   (lambda (target-string)
     
      (let* ( (tar-len (string-length target-string))
	      (max-tar-index (- tar-len 1))
	      (max-pat-index (- pat-len 1))
            )
	(let outer ( (start-index 0) )
           (if (> (+ pat-len start-index) tar-len)
	       #f
	       (let inner ( (p-ind 0) (t-ind start-index) )
	          (cond
	  	   ((> p-ind max-pat-index)  ; nothing left to check
		    #f  		     ; fail
		   )
                   ((char=? (string-ref pattern-string p-ind)
		 	    (string-ref target-string  t-ind))
		    (if (= p-ind max-pat-index)
		        start-index  ;; success -- return start index of match
		        (inner (+ p-ind 1) (+ t-ind 1)) ; keep checking
                    )
                   )
                   ((> (+ pat-len start-index) max-tar-index) #f) ; fail
		   (else
		     (outer (+ start-index
			       (vector-ref shift-vec
					   (char->integer 
				 		(string-ref target-string
							    (+ start-index pat-len)
                   ) )      )  )           )    )
                  ) ; end-cond
          ) ) )
   )  ) ; end-lambda
) )

;;				--- E O F ---
