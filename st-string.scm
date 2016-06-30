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
