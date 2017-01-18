;;; FILE: "st-stream.scm"
;;; IMPLEMENTS: Stream, TextStream, ByteStream
;;; AUTHOR: Ken Dickey
;;; DATE: 24 July 2016

;; (requires 'st-core-classes)
;; (requires 'st-string)
;; (requires 'st-array) ;; for bytevectors

;;; ST Streams are similar to Scheme ports, which
;;; are exposed here.
;;;
;;; This implementation hierarchy is non-standard! (not ANSI ST)

;;; Stream 
(define Stream
  (newSubclassName:iVars:cVars:
   Object
   'Stream '() '())
)

(perform:with:
     Stream
     'category:
     '|Collections-Streams|)

(perform:with:
     Stream
     'comment:
"I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my \"contents\"."
)

(addSelector:withMethod:
        Stream
        'atEnd
        (lambda (self)
          (if (input-stream? self)
              (eof-object? (peek-char self))
              #true))) ;; write-only ports always at end


;;; CharStream

(define CharStream
  (newSubclassName:iVars:cVars:
   Stream
   'CharStream '() '())
)
(set! st-text-stream-behavior (perform: CharStream 'methodDict))

(perform:with:
     (class CharStream)
     'category:
     '|Collections-Streams|)

(perform:with:
     (class CharStream)
     'comment:
"I am a stream of Unicode characters.  Use a ByteStream if you want bytes."
)

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
        (lambda (self numChars) (read-string numChars self)))

(addSelector:withMethod:
        CharStream
        'nextPut:
        (lambda (self aChar) (write-char aChar self)))

(addSelector:withMethod:
        CharStream
        'nextPutAll:
        (lambda (self aCollection)
          (perform:with:
             aColection
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

(define ByteStream
  (newSubclassName:iVars:cVars:
   Stream
   'ByteStream '() '())
)
(set! st-byte-stream-behavior (perform: ByteStream 'methodDict))

(perform:with:
     (class ByteStream)
     'category:
     '|Collections-Streams|)

(perform:with:
     (class ByteStream)
     'comment:
"I am a stream of bytes.  Use CharStream if you want Unicode characters."
)

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


;; (provides st-stream)

;;;			--- E O F ---			;;;
