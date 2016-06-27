;;; FILE: "parsing.sch"
;;; IMPLEMENTS: Smalltalk source code tokenization
;;; AUTHOR: Ken Dickey
;;; DATE: 10 May 2016


(define token-tag (cons token nil)) ;; not eq? to anyhthing else

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
  '( carrot cascade badToken blockStart blockEnd comment eof keyword leftParen period rightParen string verticalBar whitespace )
)

;;; TOKEN-PARSER-FOR-PORT returns a function/thunk
;;; which returns tokens
;; Note that token-strings are returned, NOT objects.
;; Numbers, literal arrays, symbols, ... are NOT created
;; here, but by the caller of this function
(define (token-parser-for-port inport source line column)
  (let* ( (line line)   ;; May start at position > 0.
          (col  column) ;; Likewise..
          (buffer  (make-string 1024 #\space))
          (buf-len (string-length buffer))
          (token-len      #false)
          (token-location #false)
          (first-char     #false)
          (next-char      #false)
       )

    (define (add-to-buffer char)
      (string-set! buffer token-len char)
      (set! token-len (+ 1 token-len)))
    
    (define (next-token)
      (set! token-len 0)
      ;; location is start of scanned token
      (set! token-location (vector source line column))
      (set! first-char (read-char inport))
      (add-to-buffer first-char)

      (cond
       ((eof-object? first-char)
        (new-token 'eof)
        )
       ((whitespace? first-char)
        (consume-whitespace)
        )
       ((letter? first-char)
        (scan-id-or-keyword)
        )
       ((digit? first-char)
        (scan-number)
        )
       (else
        (case first-char
          ((#\") (consume-comment)) 
          ((#\#) (scan-sharp-literal))
          ((#\') (scan-string))
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
      (let ( (char (read-char inport)) )
        (if (newline-char? char)
          (begin
            (set! line (+ 1 line))
            (set! col 0))
          (set! col (+ 1 col)))
        (set! next-char (peek-char inport))
        char))
 
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
        (if (whitespace? current-char)
            (begin
              (next-char)
              (loop))
            (new-token 'whitespace))))

    (define (scan-id-or-keyword)
      (let loop ()
        (cond
         ((or (char-alphabetic? next-char)
              (char-numeric? next-char))
          (next-char-keep)
          (loop))))
      ; id or keydword?
;;  (if (char=? #\: next-char)
      ;; keyword or id:= assignment
      ;; DO NOT READ the $:
      ;; -- let parser deal with it
      ;;  'foo:= 3'
      ;; is    'foo := 3'
      ;; NEVER 'foo: = 3'
      (new-token 'identifier)) ;; NB: NEVER keyword
      

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
  (let ( (charcode (char->integer char)) )
    (or (<= 97 charcode 122)  ;; a..z
        (<= 65 charcode  90)) ;; A..Z
        
) )

(define (digit? char) ;; Note: char-numeric?
  (<= 48 (char->integer char) 57)) ;; 0..9

(define whitespace? char-whitespace?)

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
