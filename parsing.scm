;;; FILE: "parsing.sch"
;;; IMPLEMENTS: Smalltalk source code tokenization
;;; AUTHOR: Ken Dickey
;;; DATE: 10 May 2016


(define token-tag (cons 'token '())) ;; not eq? to anyhthing else

(define (token? thing)
  (and (vector? thing)
       (= 4 (vector-length thing))
       (eq? token-tag
            (vector-ref thing 0))))

(define (make-token kind string location)
  (vector token-tag kind string location))

(define (token-kind token)
  (vector-ref token 1))

(define (token-string token)
  (vector-ref token 2))

(define (token-location token)
  (vector-ref token 3))

;; location is (source, line, col)
;; Where source may be
;;    string
;;  ('file . filename)
;;  st-object
;;  database-id
;;  ...

(define token-kinds
;; NB: token-kind is never 'keyword
;; Keywords are recognized by caller (parser).
;; Let parser deal with <identifier> <colon>
;;  'foo:= 3'
;; is    'foo := 3'
;; NEVER 'foo: = 3'
;; The scanner would need to look 2 chars ahead
;; to resolve this, but the parser only needs
;; one token (<colon> vs <whitespace> ..)
  '( assignment
     badToken blockArg blockStart blockEnd braceBegin braceEnd
     carrot cascade characterLiteral colon comment
     dynamicDictStart
     eof
     keyword
     leftParen litArrayStart litByteArrayStart
     minus
     period
     rightParen
     sharp string symbol
     verticalBar
     whitespace )
)

(define (tokenizer-for-string aString)
  (token-parser-for-port
     (open-input-string aString)
     aString ;; source
     0
     0))

(define (tokenizer-for-file-named fileNameString)
  (token-parser-for-port
     (open-input-file aString)
     aString ;; source
     0
     0))

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
          (token-len      #false)
          (token-location #false)
          (first-char     #false)
          (next-char      #false)
          (port-closed    #false)
       )

    (define (add-to-buffer char)
      (string-set! buffer token-len char)
      (set! token-len (+ 1 token-len)))
    
    (define (next-token)
      (set! token-len 0)
      ;; location is start of scanned token
      (set! token-location (vector source line column))
      (set! first-char (read-next-char))
      (unless (eof-object? first-char)
        ;; Can't store eof in a string.
        (add-to-buffer first-char)) 

      (cond
       ((eof-object? first-char)
        ;; (close-port port)
        ;; (set! port-closed #t)
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
       (else
        (case first-char
          ((#\") (consume-comment)) 
          ((#\#) (scan-sharp-literal))
          ((#\') (scan-string 'string))
          ((#\:) (scan-colon))
          ((#\$) (scan-character-literal))
          ((#\[) (new-token 'blockStart))
          ((#\]) (new-token 'blockEnd))
          ((#\() (new-token 'leftParen))
          ((#\)) (new-token 'rightParen))
          ((#\|) (new-token 'verticalBar))
          ((#\;) (new-token 'cascade))
          ((#\.) (new-token 'period))
          ((#\^) (new-token 'carrot))
          ((#\-) (new-token 'minus))
          (else  (scan-binary-selector-or-unexpected))
          )
        )))

    ;; read a char and add to buffer
    (define (next-char-keep)
      (when (>= token-len buf-len)
        (error "Token size exceeds max"
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
            (if (eof-object? char)
                (begin
                  (close-port inport)
                  (set! port-closed #true))
                (begin
                  (if (char=? char #\newline)
                      (begin
                        (set! line (+ 1 line))
                        (set! column 0))
                      (set! column (+ 1 column)))
                  (set! next-char (peek-char inport))))
            char)))

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
      (let loop ()
        (if (or (alphabetic? next-char)
                (numeric?    next-char))
            (begin
              (next-char-keep)
              (loop))))
      (new-token token-kind))

    (define (scan-symbol)
      (let loop ()
        (if ;; identifier plus $:
         (or (alphabetic? next-char)
             (numeric?    next-char)
             (and (char? next-char) ;; eof protect
                  (char=? #\: next-char)))
         (begin
           (next-char-keep)
           (loop))))
      (new-token 'symbol))

    (define (scan-number)
      (error "NYI: scan-number")
      )

    (define (consume-comment)
      (let loop ()
        (cond
         ((eof-object? next-char)
          (error "fell off end of input in comment"
                 (new-token 'badToken))
          )
         ((char=? #\" next-char)
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
          (error "fell off end of input in string"
                 (new-token 'badToken))
          )
         ((char=? #\' next-char)
          (next-char-keep)
          (if (char=? #\' next-char) ;; "...''... "
              (begin
                (next-char-keep)
                (loop))
              (new-token token-kind))
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
       ((char-alphabetic? next-char)
        (scan-symbol)
        )
       ((char=? #\' next-char)
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
       ((char=? #\= next-char)
        (next-char-keep) ;; read $=
        (new-token 'assignment)
        )
       ((char-alphabetic? next-char)
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

    (define (scan-binary-selector-or-unexpected) 
      (error "NYI: scan-binary-selector-or-unexpected")
      )

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

(define (letter? char) ;; Note char-alphabetic?
  (if (char? char) ;; eof protect
      (let ( (charcode (char->integer char)) )
        (or (<= 97 charcode 122)  ;; a..z
            (<= 65 charcode  90))) ;; A..Z
      #false)
)

(define (digit? char) ;; Note: char-numeric?
  (if (char? char)
      (<= 48 (char->integer char) 57) ;; 0..9
      #false))

(define (whitespace? char)
  (if (char? char)
      (char-whitespace? char)
      #false))

(define (alphabetic? char)
  (if (char? char)
      (char-alphabetic? char)
      #false))

(define (numeric? char)
  (if (char? char)
      (char-numeric? char)
      #false))

;;;

(define upperCaseLetters "abcdefghijklmnopqrstuvwxyz")
(define lowerCaseLetters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define decimalDigitLetters "0123456789")
(define nonCaseChar #\_) ; underscore
(define exponentLetters "edg")
(define binaryCharacters "!%&*+,/<=>?@\~|-")
(define commentCharacter #\")
(define stringCharacter #\')
(define returnOperatorChar #\^)
(define assignmentString ":=")  ;; NB: 'foo:=' parses as #(foo :=) NOT #(foo: =)


;;;			--- E O F ---			;;;
