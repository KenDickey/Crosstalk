;;; FILE: "st-string.sch"
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
    'asStymbol
    (lambda (self) (string->symbol self)))




;; (provide 'st-string)

;;;			--- E O F ---			;;;
