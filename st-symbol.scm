;;; FILE: "st-symbol.scm"
;;; IMPLEMENTS: symbol
;;; AUTHOR: Ken Dickey
;;; DATE: 15 January 2017

;; (requires 'st-core-classes)


(define Symbol
  (newSubclassName:iVars:cVars:
   String
   'Symbol '() '())
)

(set! st-symbol-behavior (perform: Symbol 'methodDict))

(addSelector:withMethod:
     Symbol
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Symbol)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod: 
    Symbol
    'printOn:
    (lambda (self port)
      (format port "#'~a'" self))
      ;; @@FIXME: ''' & Scheme specifics
)

(addSelector:withMethod: 
    Symbol
    '=
    (lambda (self other)
      (and (symbol? other)
           (symbol=? self other)))
)

(addSelector:withMethod: 
    Symbol
    'hash
    symbol-hash
)

(addSelector:withMethod: 
    Symbol
    '<
    (lambda (self other)
      ;; @@FIXME: (symbol? other)
      (symbol<? self other))
)

(addSelector:withMethod: 
    Symbol
    '<
    (lambda (self other)
      ;; @@FIXME: (symbol? other)
      (symbol<? self other))
)

(addSelector:withMethod: 
     Symbol
     'copy ;; override string-copy
     (lambda (self) self))

(perform:with:
     Symbol
     'category:
     '|Kernel-Text|)

(perform:with:
     Symbol
     'comment:
"I represent Strings that are created uniquely.
 Symbols which are spelled the same are == to each other"
)

(addSelector:withMethod: 
    Symbol
    'asString
    (lambda (self) (symbol->string self)))

(addSelector:withMethod: 
    Symbol
    'asSymbol
    (lambda (self) self))

(addSelector:withMethod:
     Symbol
     'size ;; self basicSize
     (lambda (self) (string-length (symbol->string self))))

(addSelector:withMethod: 
    Symbol
    '|,|
    (lambda (self aString)
      (string-append (symbol->string self)
                     (if (symbol? aString)
                         (symbol->string aString)
                         aString))))

(addSelector:withMethod: 
    Symbol
    'at:
    (lambda (self index)
      (string-ref (symbol->string self) (- index 1))))

(addSelector:withMethod: 
    String
    'do:
    (lambda (self aBlock)
      (string-for-each aBlock (symbol->string self))))

;;; symbol@FillMeIn

;; (provides st-symbol)

;;;			--- E O F ---			;;;
