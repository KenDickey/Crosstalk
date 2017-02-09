;;; FILE: "st-error-obj.scm"
;;; IMPLEMENTS: Scheme Error objects
;;; AUTHOR: Ken Dickey
;;; DATE: 08 February 2017

(define ErrorObject
  (newSubclassName:iVars:cVars:
   Object
   'ErrorObject '() '())
)

(perform:with:
     ErrorObject
     'comment: "I present Scheme error objects")

(perform:with:
     ErrorObject
     'category: '|Exceptions Kernel|)

(addSelector:withMethod:
     ErrorObject
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'ErrorObject)
           (superPerform:with: self 'is: symbol))))

;; Scheme Vectors
(set! st-error-obj-behavior (perform: ErrorObject 'methodDict))

(addSelector:withMethod:
     ErrorObject
     'message
     (lambda (self) (error-object-message self)))

(addSelector:withMethod:
     ErrorObject
     'irritants
     (lambda (self) (error-object-irritants self)))

(addSelector:withMethod:
     ErrorObject
     'isReadError
     (lambda (self) (read-error? self)))

(addSelector:withMethod:
     ErrorObject
     'isFileError
     (lambda (self) (file-error? self)))


(addSelector:withMethod:
     (class ErrorObject)
     'error:
     (lambda (self message)
       (error message)))

(addSelector:withMethod:
     (class ErrorObject)
     'error:withIrritants:
     (lambda (self message irritants)
       ;; @@FIXME: generalize
       (apply error (cons message (vector->list irritants)))))

(addSelector:withMethod: 
    ErrorObject
    'printOn:
    (lambda (self port)
      (display (error-object-message self) port)
      (for-each
       (lambda (irritant)
         (display " ") (display irritant))
       (error-object-irritants self)))
)


;; (provide 'st-error-obj)

;;;			--- E O F ---			;;;
