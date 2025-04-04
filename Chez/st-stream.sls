;;; FILE: "st-stream.sls"
;;; IMPLEMENTS: Stream, CharStream, ByteStream
;;; AUTHOR: Ken Dickey
;;; DATE: 24 July 2016; March 2025


;;; ST Streams are similar to Scheme ports, which
;;; are exposed here.
;;;
;;; This implementation hierarchy is non-standard! (not ANSI ST)

(library (st-stream)

  (export
   init-st-stream

   Stream
   CharStream
   ByteStream
   )
  
  (import
   (rnrs base)
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)
   (st-sequence-coll)
   (st-array-coll)
   (st-array)
   (st-boolean)
   (st-character)
   (st-string)
   (st-symbol)
   )

;;; Stream 
(define Stream
  (newSubclassName:iVars:cVars:
   Object
   'Stream '() '())
)

(define CharStream
  (newSubclassName:iVars:cVars:
   Stream
   'CharStream '() '())
)

(define ByteStream
  (newSubclassName:iVars:cVars:
   Stream
   'ByteStream '() '())
)


;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-stream)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-symbol)
  
(perform:with: Stream
               'methodDict:
               (behavior-add-from-other
                ($ Stream 'methodDict)
                ($ Object 'methodDict)))

(rebase-mdict! CharStream st-char-stream-behavior)

(rebase-mdict! ByteStream st-byte-stream-behavior)

;;; Stream

(perform:with:
     Stream
     'category:
     'Collections-Streams)

(perform:with:
     Stream
     'comment:
"I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my \"contents\"."
)

(addSelector:withMethod:
     Stream
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Stream)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
        Stream
        'atEnd
        (lambda (self)
          (if (input-port? self)
              (eof-object? (peek-char self))
              #t))) ;; write-only ports always at end


;;; CharStream


(perform:with:
     CharStream
     'category:
     'Collections-Streams)

(perform:with:
     CharStream
     'comment:
"I am a stream of Unicode characters.  Use a ByteStream if you want bytes."
)

(addSelector:withMethod:
     CharStream
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'CharStream)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
        CharStream
        'isBinary
        (lambda (self) st-false))

(addSelector:withMethod:
        CharStream
        'isCharacters
        (lambda (self) st-true))

(addSelector:withMethod:
        CharStream
        'isText
        (lambda (self) st-false))

(addSelector:withMethod:
        CharStream
        'isReadOnly
        (lambda (self) (not (output-port? self))))

(addSelector:withMethod:
        CharStream
        'next
        (lambda (self) (read-char self))) ;; @@??handle EOF??@@

(addSelector:withMethod:
        CharStream
        'next:
        (lambda (self numChars) (get-string-n numChars self)))

(addSelector:withMethod:
        CharStream
        'nextPut:
        (lambda (self aChar) (write-char aChar self)))

(addSelector:withMethod:
        CharStream
        'nextPutAll:
        (lambda (self aCollection)
          (perform:with:
             ($ aCollection 'asString)
             'do:
             (lambda (aChar) (write-char aChar self)))))

(addSelector:withMethod:
        CharStream
        'newLine
        (lambda (self) (write-char #\newline self)))

(addSelector:withMethod:
        CharStream
        'space
        (lambda (self) (write-char #\space self)))

;;; ByteStream

(perform:with:
     ByteStream
     'category:
     'Collections-Streams)

(perform:with:
     ByteStream
     'comment:
"I am a stream of bytes.  Use CharStream if you want Unicode characters."
)

(addSelector:withMethod:
     ByteStream
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ByteStream)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
        ByteStream
        'isBinary
        (lambda (self) st-true))

(addSelector:withMethod:
        ByteStream
        'isCharacters
        (lambda (self) st-false))

(addSelector:withMethod:
        ByteStream
        'isText
        (lambda (self) st-false))

(addSelector:withMethod:
        ByteStream
        'isReadOnly
        (lambda (self) (not (output-port? self))))


'st-stream
) )

)

;;;			--- E O F ---			;;;
