;;; FILE: "st-symbol.sls"
;;; IMPLEMENTS: Symbol
;;; AUTHOR: Ken Dickey
;;; DATE: 15 January 2017; March 2025

(library (st-symbol)

  (export
   init-st-symbol

   Symbol
   )

  (import
   (rnrs base)
   (rnrs lists (6))
   (rnrs control (6))
   (only (chezscheme)
         make-parameter)
   (rnrs unicode (6))
   (rnrs bytevectors (6))
   (rnrs hashtables (6)) ; string-hash
   (rnrs io simple (6))
   (rnrs io ports (6))
   (rnrs mutable-strings (6))
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-character)
   (st-array-coll)
   (st-string)
   )



(define Symbol
  (newSubclassName:iVars:cVars:
   String
   'Symbol '() '())
)

;;;======================================================

(define initialized? (make-parameter #f))

(define (init-st-symbol)
  (unless (initialized?)
    (initialized? #t)
    
    (init-st-string)
  
(rebase-mdict! Symbol st-symbol-behavior)
(primAppendLocalSelectors: Symbol '(printOn: )) ; early bound

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

(addSelector:withMethod:arity:
    Symbol
    '=
    (lambda (self other)
      (and (symbol? other)
           (symbol=? self other)))
    2)

(addSelector:withMethod: 
    Symbol
    'hash
    symbol-hash
)

(addSelector:withMethod:arity:
    Symbol
    '<
    (lambda (self other)
      ;; @@FIXME: (symbol? other)
      (symbol<? self other))
    2)

(addSelector:withMethod:arity:
    Symbol
    '<
    (lambda (self other)
      ;; @@FIXME: (symbol? other)
      (symbol<? self other))
    2)

(addSelector:withMethod: 
     Symbol
     'copy ;; override string-copy
     (lambda (self) self))

(perform:with:
     Symbol
     'category:
     'Kernel-Text)

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

(addSelector:withMethod:arity:
    Symbol
    (string->symbol ",")
    (lambda (self aString)
      (string-append (symbol->string self)
                     (if (symbol? aString)
                         (symbol->string aString)
                         aString)))
    2)

(addSelector:withMethod: 
    Symbol
    'at:
    (lambda (self index)
      (string-ref (symbol->string self) (- index 1))))

(addSelector:withMethod: 
    String
    'do:
    (lambda (self aBlock)
      (string-for-each
       aBlock
       (if (string? self) self (symbol->string self)))))

;;; symbol@FillMeIn

'st-symbol
) )

)

;;;			--- E O F ---			;;;
