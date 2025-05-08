#!r6rs
;;; File: "st-string.ss"
;;; IMPLEMENTS: String
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;; (load "st-core-classes.ss")
;; (load "st-core-methods.ss")

(define String
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'String '() '())
)
(rebase-mdict! String st-string-behavior)

;;;======================================================


;; simple, brute force compare
;; Answers #f or St source index of first match
;; (Smalltalk indexing is based 1, Scheme based 0)
(define (string-contains aString wanted startIndex case-sensitive?)
  (let* ( (compare? (if case-sensitive? char=? char-ci=?))
          (endIndex (- (string-length aString) (string-length wanted)))
          (wanted-stop-index (- (string-length wanted) 1))
          (match-from?
              (lambda (indx)
                (let loop ( (strIndex indx) (wantedIndex 0) )
                  (if (compare? (string-ref aString strIndex)
                                (string-ref wanted wantedIndex))
                      (if (< wantedIndex wanted-stop-index)
                          (loop (+ strIndex 1) (+ wantedIndex 1))
                          #t) ;; substring matches wanted
                      #f))))  ;; not a match
       )
    (let loop ( (src-index startIndex) )
      (if (> src-index endIndex)
          #f ; failed -- too short to compare
          (if (match-from? src-index)
              src-index
              (loop (+ src-index 1))))
 ) ) )


;;;======================================================


(perform:with:
     String
     'category: 'Kernel-Text)

(perform:with:
     String
     'comment:
     "A String is an indexed collection of Characters.")

(addSelector:withMethod:
     String
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'String)
           (superPerform:with: self 'is: symbol))))


(addSelector:withMethod: 
    String
    'printOn:
    (lambda (self port)
      (format port "'~a'" self))
      ;; @@FIXME: ''' & Scheme specifics
)

(addSelector:withMethod: 
    String
    'asString
    (lambda (self) self))

(addSelector:withMethod: 
    String
    'asSymbol
    (lambda (self) (string->symbol self)))

(addSelector:withMethod: 
    String
    'asLowercase
    (lambda (self) (string-downcase self)))

(addSelector:withMethod: 
    String
    'asUppercase
    (lambda (self) (string-upcase self)))

(addSelector:withMethod: 
    String
    'asFoldcase ;; Unicode foldcase
    (lambda (self) (string-foldcase self)))

(addSelector:withMethod: 
    String
    'at:
    (lambda (self index)
      ;; Scheme 0 based; ST 1-based
      (if (<= 1 index (string-length self))
          (string-ref self (- index 1))
          (error 'at: "Index out of range" self index))))

(addSelector:withMethod: 
    String
    'at:put:
    (lambda (self index aChar)
      ;; Scheme 0 based; ST 1-based
      (cond
       ((not (<= 1 index (string-length self)))
        (error 'at:put: "Index out of range" self index)
        )
       ((not (char? aChar))
        (error 'at:put:
               "Strings only contain characters"
               aChar self index)
        )
       (else
        (string-set! self (- index 1) aChar)
        self))))

(addSelector:withMethod:
     String
     'size ;; self basicSize
     (lambda (self) (string-length self)))

(addSelector:withMethod:
     String
     'basicSize 
     (lambda (self) (string-length self)))

(addSelector:withMethod: 
    String
    'do:
    (lambda (self aBlock)
      (string-for-each aBlock self)))

(addSelector:withMethod:arity:
    String
    (string->symbol ",")
    (lambda (self aString)
      (string-append self
                     (if (symbol? aString)
                         (symbol->string aString)
                         aString)))
    2)

(addSelector:withMethod: 
    (class String)
    'streamContents:
    (lambda (self blockWithArg)
      (let-values ( ((stream stream->string)
                     (open-string-output-port))
                   )
        (blockWithArg stream)
        (stream->string))))

(addSelector:withMethod: 
    (class String)
    'new  ;; empty string
    (lambda (self) ""))

(addSelector:withMethod:
     (class String)
     'basicNew:
     (lambda (self size)
       (make-string size #\space)))

(addSelector:withMethod: 
    (class String)
    'value:
    (lambda (self anInteger)
      ;; String with: (Character value: anInteger)
      (string (integer->char anInteger))))

(addSelector:withMethod: 
    (class String)
    'with:
    (lambda (self char1)
      (string char1)))

(addSelector:withMethod: 
    (class String)
    'with:with:
    (lambda (self char1 char2)
      (string char1 char2)))

(addSelector:withMethod: 
    (class String)
    'with:with:with:
    (lambda (self char1 char2 char3)
      (string char1 char2 char3)))

(addSelector:withMethod: 
    (class String)
    'withAll:
    (lambda (self aCollection)
      (let ( (result '()) )
        (perform:with:
           aCollection
           'do:
           (lambda (c) (set! result (cons c result))))
        (list->string (reverse result)))))

(addSelector:withMethod: 
    String
    '=
    (lambda (self other)
      (and (string? other)
           (string=? self other))))

(addSelector:withMethod: 
    String
    'caseInsensitiveEqual:
    (lambda (self other)
      (and (string? other)
           (string-ci=? self other))))

(addSelector:withMethod:
    String
    'hash
    string-hash)

(addSelector:withMethod:arity:
    String
    '<
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string<? self other))
    2)

(addSelector:withMethod:arity:
    String
    '<=
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string<=? self other))
    2)

(addSelector:withMethod:arity:
    String
    '>
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string>? self other))
    2)`

(addSelector:withMethod:arity:
    String
    '>=
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string>=? self other))
    2)


(addSelector:withMethod: 
    String
    'caseInsensitiveLess:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci<? self other)))

(addSelector:withMethod: 
    String
    'caseInsensitiveLessOrEqual:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci<=? self other)))

(addSelector:withMethod: 
    String
    'caseInsensitiveGreater:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci>? self other)))

(addSelector:withMethod: 
    String
    'caseInsensitiveGreaterOrEqual:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci>=? self other)))

(addSelector:withMethod: 
    String
    'copy
    (lambda (self)
      (string-copy self)))

(addSelector:withMethod: 
    String
    'copy:from:to:
    (lambda (self startIndex stopIndex)
      (let ( (start (- startIndex 1)) )
        (unless (<= start stopIndex (string-length self))
          (error 'copy:from:to:
                 "index out of range"
                 startIndex stopIndex))
        (substring self start stopIndex))) ;; @@CHECKME: indexes
)

(addSelector:withMethod: 
    String
    'copy:from:count:
    (lambda (self startIndex count)
      (let* ( (start (- startIndex 1))
              (stopIndex (+ start count))
            )
      (unless (<= start stopIndex (string-length self))
        (error 'copy:from:count:
               "index out of range"
               startIndex stopIndex))
      (substring self start stopIndex)))  ;; @@CHECKME: indexes
)

(addSelector:withMethod: 
    String
    'copyWithout:
    (lambda (self aChar)
      (let loop ( (chars (string->list self)) (rev-chars '()) )
        (cond
         ((null? chars) (list->string (reverse rev-chars)))
         ((char=? aChar (car chars)) (loop (cdr chars) rev-chars))
         (else (loop (cdr chars) (cons (car chars) rev-chars)))))))


(addSelector:withMethod: 
    String
    'asCamalCase
    (lambda (self)
      (perform:with:
         String
         'streamContents:
         (lambda (outport)
           (let ( (length (string-length self))
                  (after-first-char #f)
                )
             (let loop ( (index 0) (upcaseMe #f) )
               (when (< index length)
                 (let ( (char (string-ref self index)) )
                   (cond
                    ((char-whitespace? char)
                      (loop (+ index 1) #t)
                      )
                    ((and upcaseMe after-first-char)
                     (write-char (char-upcase char) outport)
                     (loop (+ index 1) #f)
                     )
                    (else
                     (write-char char outport)
                     (unless after-first-char
                       (set! after-first-char #t))
                     (loop (+ index 1) #f)
                     )))))))
  ) ) )


(addSelector:withMethod:
     String
     'asByteArray
     (lambda (self)
       (string->utf16 self)) ;; @@ASSUMPTION: UTF-16
)

(addSelector:withMethod:
     String
     'asArray
     (lambda (self)
       (list->vector
        (map char->integer (string->list self))))
)

(addSelector:withMethod:
     String
     'findString:startingAt:caseSensitive:
     (lambda (self key start caseSensitive?)
       ;; Nota Bene:
       ;;   Scheme strings 0 based; Smalltalk strings 1 based.
       (let ( (result
               (string-contains self key (- start 1) caseSensitive?))
            )
         (if result
             (+ 1 result)
             0) ; Answer 0 if no match
         ))
     )

(addSelector:withMethod:
     String
     'indexOf:startingAt:ifAbsent:
     (lambda (self aChar startIndex failureThunk)
       (let ( (strLen (string-length self)) )
         (let loop ( (index (- startIndex 1)) )
           (cond
            ((>= index strLen) ;; failed
             (failureThunk))
            ((char=? aChar (string-ref self index)) ;; match
             (+ 1 index))
            (else (loop (+ 1 index)))))))
)

(addSelector:withMethod:
     String
     'indexOf:startingAt:
     (lambda (self aChar startIndex)
       (let ( (strLen (string-length self)) )
         (let loop ( (index (- startIndex 1)) )
           (cond
            ((>= index strLen) ;; failed
             0)
            ((char=? aChar (string-ref self index)) ;; match
             (+ 1 index))
            (else (loop (+ 1 index)))))))
     )

(addSelector:withMethod:
     (class String)
     'newline
     (lambda (self) (string #\newline))
     )

'st-string

;;;			--- E O F ---			;;;
