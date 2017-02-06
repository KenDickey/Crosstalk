;;; FILE: "st-string.scm"
;;; IMPLEMENTS: Strings
;;; AUTHOR: Ken Dickey
;;; DATE: 14 June 2016

;; (require 'st-collection)

(define String
  (newSubclassName:iVars:cVars:
   ArrayedCollection
   'String '() '())
)

(set! st-string-behavior (perform: String 'methodDict))

(perform:with:
     String
     'category: '|Kernel-Text|)

(perform:with:
     String
     'comment:
     "A String is an indexed collection of Characters.")

;; (addSelector:withMethod: 
;;     String
;;     'printString
;;     printString)

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
          (error "Index out of range" self index))))

(addSelector:withMethod: 
    String
    'at:put:
    (lambda (self index aChar)
      ;; Scheme 0 based; ST 1-based
      (cond
       ((not (<= 1 index (string-length self)))
        (error "Index out of range" self index)
        )
       ((not (char? aChar))
        (error "Strings only contain characters" aChar self index)
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

(addSelector:withMethod: 
    String
    '|,|
    (lambda (self aString)
      (string-append self
                     (if (symbol? aString)
                         (symbol->string aString)
                         aString))))

(addSelector:withMethod: 
    (class String)
    'streamContents:
    (lambda (self blockWithArg)
      (let ( (stream (open-output-string)) )
        (blockWithArg stream)
        (get-output-string stream))))

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
           (string-ci-=? self other))))

(addSelector:withMethod:
    String
    'hash
    string-hash)

(addSelector:withMethod: 
    String
    '<
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string<? self other)))

(addSelector:withMethod: 
    String
    '<=
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string<=? self other)))

(addSelector:withMethod: 
    String
    '>
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string>? self other)))

(addSelector:withMethod: 
    String
    '>=
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string>=? self other)))

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
      (string-ci-<=? self other)))

(addSelector:withMethod: 
    String
    'caseInsensitiveGreater:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci->? self other)))

(addSelector:withMethod: 
    String
    'caseInsensitiveGreaterOrEqual:
    (lambda (self other)
      ;; @@FIXME: (string? other)
      (string-ci->=? self other)))

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
          (error "copy:from:to: index out of range"
                 startIndex stopIndex))
        (string-copy self start stopIndex)))
)

(addSelector:withMethod: 
    String
    'copy:from:count:
    (lambda (self startIndex count)
      (let* ( (start (- startIndex 1))
              (stopIndex (+ start count))
            )
      (unless (<= start stopIndex (string-length self))
        (error "copy:from:count: index out of range"
               startIndex stopIndex))
      (string-copy self start stopIndex)))
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
                  (after-first-char #false)
                )
             (let loop ( (index 0) (upcaseMe #false) )
               (when (< index length)
                 (let ( (char (string-ref self index)) )
                   (cond
                    ((char-whitespace? char)
                      (loop (+ index 1) #true)
                      )
                    ((and upcaseMe after-first-char)
                     (write-char (char-upcase char) outport)
                     (loop (+ index 1) #false)
                     )
                    (else
                     (write-char char outport)
                     (unless after-first-char
                       (set! after-first-char #true))
                     (loop (+ index 1) #false)
                     )))))))
  ) ) )


(addSelector:withMethod:
     String
     'asByteArray
     (lambda (self)
       (let* ( (strLen (string-length self))
               (result (make-bytevector strLen 0))
             )
         (let loop ( (index 0) )
           (when (< index strLen)
             (bytevector-set! result
                              index
                              (char->integer (string-ref self index)))
             (loop (+ index 1))))
         result))
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
       (let ( (result
               ((if caseSensitive?
                   string-contains
                   string-contains-ci)
                 self
                 key
                 (- start 1) ;; Scheme 0-based, St 1-based
                 (string-length self)))
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

;; (provide 'st-string)

;;;			--- E O F ---			;;;
