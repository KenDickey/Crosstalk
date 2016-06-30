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
     'category: "Kernel-Text")

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
      (display "'" port)
      ;; @@FIXME: ''' & Scheme specifics
      (display self port)
      (display "'" port))
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
      (string-append self aString)))

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

                    

;; (provide 'st-string)

;;;			--- E O F ---			;;;
