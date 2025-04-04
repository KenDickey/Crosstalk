;;; FILE: "st-tokenizer.sls"
;;; IMPLEMENTS: Smalltalk source code tokenization
;;; AUTHOR: Ken Dickey
;;; DATE: 21 June 2016; March 2025

(library (st-tokenizer)

  (export
   init-st-tokenizer

   ;; token ; a structure
   make-token	  ;; constructor
   token-kind	  ;; accessor
   token-string	  ;; accessor
   token-location ;; accessor
   token?	  ;; predicate
   token-kinds

   ;; (tokenizer-for-string aString)
   tokenizer-for-string
   ;; (tokenizer-for-file-named fileNameString)
   tokenizer-for-file-named
   ;; (token-parser-for-port inport source line column)
   token-parser-for-port

   token->native ; (token->native token)
   )

  (import
   (rnrs base)
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs lists (6))
   (rnrs control (6))
   (rnrs records inspection (6))
   (rnrs records procedural (6))
   (rnrs unicode (6))
   (rnrs mutable-strings (6))
   (only (chezscheme)
         define-structure
         format
         make-parameter
         parameterize
         open-output-string
         get-output-string
         void
       )
   )


(define-structure (token kind string location))

;; (define token-tag (cons 'token '())) ;; not eq? to anything else

;; (define (token? thing)
;;   (and (vector? thing)
;;        (= 4 (vector-length thing))
;;        (eq? token-tag
;;             (vector-ref thing 0))))

;; (define (make-token kind string location)
;;   (vector token-tag kind string location))

;; (define (token-kind token)
;;   (vector-ref token 1))

;; (define (token-string token)
;;   (vector-ref token 2))

;; (define (token-location token)
;;   (vector-ref token 3))

;; location is (source, line, col)
;; Where source may be
;;    string
;;  ('file . filename)
;;  st-object
;;  database-id
;;  ...

(define token-kinds
  '( assignment
     badToken
     binarySelector blockArg blockStart blockEnd
     braceBegin braceEnd 
     carrot cascade characterLiteral colon comment
     dynArrayStart dynArrayEnd
     eof
     float floatWithExponent
     identifier integer integerWithRadix
     keyword
     leftParen litArrayStart litByteArrayStart
     methDef minus
     period
     rightBrace rightParen
     scaledDecimal scaledDecimalWithFract sharp string symbol
     verticalBar
     whitespace )
)

(define open-input-string open-string-input-port)

(define (tokenizer-for-string aString)
  (token-parser-for-port
     (open-input-string aString)
     aString ;; source
     0
     0))

(define (tokenizer-for-file-named fileNameString)
  (token-parser-for-port
     (open-input-file fileNameString)
     (vector 'file fileNameString) ;; source
     0
     0))

(define (safe-char=? one another)
  (and (char? one)
       (char? another)
       (char=? one another)))

;; Keywords are recognized in scan-identifier
;;  'foo:= 3'
;; is    'foo := 3'
;; NEVER 'foo: = 3'

;; Integer vs float in scan-float:
;;  '3. ' is integer 3 followed by period and whitespace
;; NOT float 3.0
;; E.g. in "x := 3. ^x + 5"

;; This scanner needs to look 2 chars ahead
;; to decide these cases.

(define saved-char #f) ;; false or a character


;;; TOKEN-PARSER-FOR-PORT returns a function/thunk
;;; which returns tokens
;; Note that token-strings are returned, NOT objects.
;; Numbers, literal arrays, symbols, ... are NOT created
;; here, but by the caller of this function
(define (token-parser-for-port inport source line column)
  (let* ( (line    line)   ;; May start at position > 0.
          (column  column) ;; Likewise..
          (buffer  (make-string 1024 #\space))
          (buf-len (string-length buffer))
          (token-len      #f)
          (token-location #f)
          (first-char     #f)
          (next-char      #f)
          (port-closed    #f)
       )

    (define (add-to-buffer char)
      (string-set! buffer token-len char)
      (set! token-len (+ 1 token-len)))
    
    (define (next-token)
      (set! token-len 0)
      ;; location is start of scanned token
      (set! token-location (vector source line column))
      (if saved-char
          (begin
            (set! first-char saved-char)
            (set! saved-char #f))
          (set! first-char (read-next-char)))
      (unless (eof-object? first-char)
        ;; Can't store eof in a string.
        (add-to-buffer first-char)) 

      (cond
       ((eof-object? first-char)
        (new-token 'eof)
        )
       ((whitespace? first-char)
        (consume-whitespace)
        )
       ((letter? first-char)
        (scan-identifier 'identifier)
        )
       ((digit? first-char)
        (scan-number)
        )
       ((binary-operator-char? first-char)
        (scan-binary-operator)
        )
       (else
        (case first-char
          ((#\") (consume-comment)) 
          ((#\#) (scan-sharp-literal))
          ((#\') (scan-string 'string))
          ((#\:) (scan-colon))
          ((#\$) (scan-character-literal))
          ((#\[) (new-token 'blockStart))
          ((#\]) (new-token 'blockEnd))
          ((#\{) (new-token 'dynArrayStart))
          ((#\}) (new-token 'dynArrayEnd))
          ((#\() (new-token 'leftParen))
          ((#\)) (new-token 'rightParen))
          ((#\|) (new-token 'verticalBar))
          ((#\;) (new-token 'cascade))
          ((#\.) (new-token 'period))
          ((#\^) (new-token 'carrot))
          ((#\-) (new-token 'minus)) ;; binaryOperatorChar
          (else
           (error 'next-token
                  "Unexpected input"
                  (new-token 'badToken))
          )
        ))))

    ;; read a char and add to buffer
    (define (next-char-keep)
      (when (>= token-len buf-len)
        (error 'next-char-keep
               "Token size exceeds max"
               (new-token 'badToken)))
      (add-to-buffer (read-next-char)))

    ;; read a char and forget it
    (define (next-char-skip)
      (read-next-char)) ;; resets next-char

    ;; read a character
    ;; track line & col
    ;; peek ahead
    (define (read-next-char)
      (if port-closed
          (eof-object)
          (let ( (char (read-char inport)) )
            (cond
             ((eof-object? char)
              (close-port inport)
              (set! port-closed #t)
              (set! next-char (eof-object))
              )
             ((safe-char=? char #\newline)
              (set! line (+ 1 line))
              (set! column 0)
              (set! next-char (peek-char inport))
              )
             (else
              (set! column (+ 1 column))
              (set! next-char (peek-char inport)
              )))
            char)
    ) )

    ;; make a new token using buffer for string
    (define (new-token kind)
      (let ( (token
              (make-token kind
                          (substring buffer 0 token-len)
                          token-location))
           )
        (set! token-len 0)
        token))

    (define (consume-whitespace) ;; and return token
      (let loop ()
        (if (whitespace? next-char)
            (begin
              (next-char-skip)
              (loop))
            (new-token 'whitespace))))

    (define (scan-identifier token-kind)
      (let loop () ;; pick off identifier chars
        (if (or (letter? next-char)
                (digit?  next-char)
                (safe-char=? #\_ next-char))
            (begin
              (next-char-keep)
              (loop))))
      ;; Check for keyword
      (if (safe-char=? #\: next-char)
          (begin
            (set! saved-char next-char)
            (read-next-char)
            (if (safe-char=? #\= next-char) ;; ":="
                (new-token token-kind)
                (begin ;; found a keyword
                  (add-to-buffer saved-char)
                  (set! saved-char #f)
                  (new-token 'keyword))))
          (new-token token-kind)))
              

    (define (scan-symbol)
      (let loop ()
        (if ;; identifier plus $:
         (or (letter? next-char)
             (digit?  next-char)
             (and (char? next-char) ;; eof protect
                  (safe-char=? #\: next-char)))
         (begin
           (next-char-keep)
           (loop))))
      (new-token 'symbol))


    (define (scan-number)
      (let loop ()
        (cond
         ((digit? next-char)
          (next-char-keep)
          (loop)
          )
         ((eof-object? next-char)
          (new-token 'integer)
          )
         ((safe-char=? #\. next-char)
          (scan-float)
          )
         ((safe-char=? #\r next-char)
          (next-char-keep)
          (scan-radix)
          )
         ((safe-char=? #\s next-char)
          (next-char-keep)
          (scan-scaled-decimal)
          )
         (else
          (new-token 'integer)))
        ) )

    (define (scan-float)
      ;; seen: digit+ '.'
      ;; want: digit+ [(e|d|g) [-] digit+]
      ;; avoid: digit+ '.' non-digit
      ;; Look ahead
      (set! saved-char #\.)
      (read-next-char)
      (if (not (digit? next-char))
          (new-token 'integer)	;; e.g. "3. " int period white
          (begin		;; e.g. "3.0" float
            (add-to-buffer #\.)
            (set! saved-char #f)
            (let loop ()
              (cond
               ((digit? next-char)
                (next-char-keep)
                (loop)
                )
               ((eof-object? next-char)
                (new-token 'float)
                )
               ((member next-char '(#\e #\d #\g))
                (next-char-keep)
                (scan-exponent)
                )
               ((safe-char=? #\s next-char)
                (next-char-keep)
                (scan-scaled-decimal)
                )
               (else
                (new-token 'float)))))
      ) )

    (define (scan-exponent)
      ;; seen: digit+ '.' digit+ (e|d|g)
      ;; want: [-] digit+
      (when (safe-char=? #\- next-char)
        (next-char-keep))
      (if (digit? next-char)
          (next-char-keep)
          (error 'scan-exponent
                 "badly formed exponent"
                 (new-token 'badToken)))
      (let loop ()
        (cond
         ((eof-object? next-char)
          (new-token 'floatWithExponent)
          )
         ((digit? next-char)
          (next-char-keep)
          (loop))
         (else
          (new-token 'floatWithExponent))))
      )

    (define (scan-radix)
      ;; seen digit+ 'r'
      ;; want: radixDigit+
      (if (radixDigit? next-char)
          (next-char-keep)
          (error 'scan-radix
                 "badly formed radix"
                 (new-token 'badToken)))
      (let loop ()
        (cond
         ((eof-object? next-char)
          (new-token 'integerWithRadix)
          )
         ((radixDigit? next-char)
          (next-char-keep)
          (loop))
         (else
          (new-token 'integerWithRadix)
          )))
      )
    
    (define (scan-scaled-decimal)
      ;; Seen: digit+ ['.' digit+] 's'
      (let loop ()
        (cond
         ((eof-object? next-char)
          (new-token 'scaledDecimalWithFract)
          )
         ((digit? next-char)
          (next-char-keep)
          (loop))
         (else
          (new-token 'scaledDecimalWithFract))))
      )

    (define (consume-comment)
      (let loop ()
        (cond
         ((eof-object? next-char)
          (error 'consume-comment
                 "fell off end of input in comment"
                 (new-token 'badToken))
          )
         ((safe-char=? #\" next-char)
          (next-char-skip)
          (new-token 'comment)
         )
         (else
          (next-char-skip)
          (loop)))
      ) )
      
    (define (scan-string token-kind)
      (let loop ()
        (cond
         ((eof-object? next-char)
          (error 'scan-string
                 "fell off end of input in string"
                 (new-token 'badToken))
          )
         ((safe-char=? #\' next-char)
          (next-char-keep)
          (cond
           ((and (char? next-char) ;; eof protect
                 (char=? #\' next-char))
            ;; '...''... '
            (next-char-keep)
            (loop))
           (else (new-token token-kind)))
          )
         (else
          (next-char-keep)
          (loop)))
      ) )

    (define (scan-sharp-literal)
      ;; Symbol, Literal Array or ByteArray,
      ;; Dynamic Dictionary
      (cond
       ((eof-object? next-char)
        (new-token 'sharp) ;; probable error
       )
       ((letter? next-char)
        (scan-symbol)
        )
       ((char=? #\' next-char)
        (next-char-keep)
        (scan-string 'symbol)
        )
       ((char=? #\( next-char)
        (next-char-keep)
        (new-token 'litArrayStart)
        )
       ((char=? #\[ next-char)
        (next-char-keep)
        (new-token 'litByteArrayStart)
        )
       ((char=? #\{ next-char)
        (next-char-keep)
        (new-token 'dynamicDictStart)
        )
       (else
        (new-token 'sharp)) ;; probably an error!
    ) )

    (define (scan-colon)
      ;; colon, assignment, blockArg
      (cond
       ((eof-object? next-char)
        (new-token 'colon)
       )
       ((safe-char=? #\= next-char)
        (next-char-keep) ;; read $=
        (new-token 'assignment)
        )
       ((letter? next-char)
        (scan-identifier 'blockArg)
        )
       (else
        (new-token 'colon)
        )
     ) )

    (define (scan-character-literal) 
      (next-char-keep)
      (new-token 'characterLiteral)
      )

    (define (scan-binary-operator)
      (if (and (safe-char=? #\- first-char)
               (digit? next-char))
          (new-token 'minus)
          (let loop ()
            (if (binary-operator-char? next-char)
                (begin
                  (next-char-keep)
                  (loop))
                (if (string=? "~>"
                              (substring buffer 0 token-len))
                    (new-token 'methDef)
                    (new-token 'binarySelector)))
      ) ) )
    
  next-token ;; return the access function
) )

;;@@@

(define reserved-words '(nil true false self super))

(define reserved-selectors
  '(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
    == and: or:
    basicSize basicAt: basicAt:put: basicNew:
    to:do: to:by:do: timesRepeat:
   )
)

;; char -> any Unicode character

(define (ascii-letter? char)
  (if (char? char) ;; eof protect
      (let ( (charcode (char->integer char)) )
        (or (<= 97 charcode 122)  ;; a..z
            (<= 65 charcode  90))) ;; A..Z
      #f))

(define (unicode-letter? char)
  (if (char? char)
      (char-alphabetic? char)
      #f))

(define (ascii-digit? char)
  (if (char? char) ;; eof protect
      (char-numeric? char)
      #f))

(define (unicode-digit? char) ;; Note: char-numeric?
  (if (char? char)
      (<= 48 (char->integer char) 57) ;; 0..9
      #f))

(define ascii-binop-chars
  (string->list "!%&*+,/<=>?@\\~-")) ;; NB: without $|
(define (ascii-binop-char? char)
  (cond
   ((member char ascii-binop-chars) #t)
   (else #f)))

(define (unicode-binop-char? char)
  (error 'unicode-binop-char?
         "Need to implement test for Unicode math symbol chars"
         char))

;; Default is ASCII (portable)
(define unicode-in-identifiers #f)
(define letter? ascii-letter?)
(define digit?  ascii-digit?)
(define binary-operator-char? ascii-binop-char?)

;;;  (use-unicode-in-identifiers aBool)
(define (use-unicode-in-identifiers aBool)
  (set! unicode-in-identifiers aBool)
  (cond
   (aBool
    (set! letter? unicode-letter?)
    (set! digit?  unicode-digit?)
    (set! binary-operator-char? unicode-binop-char?)
    )
   (else
    (set! letter? ascii-letter?)
    (set! digit?  ascii-digit?)
    (set! binary-operator-char? ascii-binop-char?)
    )
   )
)


(define (whitespace? char)
  (if (char? char)
      (char-whitespace? char)
      #f))

(define (numeric? char)
  (if (char? char)
      (char-numeric? char)
      #f))

(define (radixDigit? char)
  (cond
   ((eof-object? char)
    #f)
   ((digit? char)
    #t)
   ((upcaseLetter? char)
    #t)
   (else
    #f)))

(define (upcaseLetter? char)
  (if (char? char) ;; eof protect
      (<= 65 (char->integer char) 90) ;; A..Z
      #f))


(define (token->native token)
  (let* ( (tok-str (token-string  token))
          (str-len (string-length tok-str))
        )
    (case (token-kind token)
      ((identifier binarySelector keyword) ; a symbol
       (string->symbol tok-str)
      )
      ((string) ; "'strval'"
       (if (zero? str-len)
           ""
           (substring
               tok-str
               1
               (- (string-length tok-str) 1)))
       ;;@@Fixme: fold '' and clean #\space et al
      )
    ((symbol) ; #'sym' or #sym
     (if (zero? str-len)
         (string->symbol "")
         (let* ( (start-index
                  (if (char=? #\' (string-ref tok-str 1))
                      2
                      1))
                 (end-index (- str-len start-index -1))
               )
           `',(string->symbol (substring tok-str
                                      start-index
                                      end-index))))
     )
    ((characterLiteral)
     (string-ref tok-str 1)
     )
    ((integer float) ;; 'simple' numbers
     (string->number tok-str)
     )

    ((blockArg)
     ;; answer symbol name for argument
     (string->symbol
        (substring tok-str 1 (string-length tok-str)))
     )
    ;; @@ OTHER CASES @@
    (else
     (error 'token->native: "unhandled token kind" token))
     )
  ) )

(define initialized? (make-parameter #f))
  
(define (init-st-tokenizer)
  (unless (initialized?)
    (initialized? #t)
    ; nothing to do here..
    'st-tokenizer
    ))

)

;;;			--- E O F ---			;;;
