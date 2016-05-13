;;; FILE: "system-dictionary.sch"
;;; IMPLEMENTS: message lookup
;;; AUTHOR: Ken Dickey
;;; DATE: 10 May 2016

;; (require "object.sch")


;; Syntactic sugar taists sweeter

(define (make-method-dictionary)
  (make-eq-hashtable))

(define (dictionarySize methodDict)
  (hashtable-size methodDict))

;; methodDict lookupSelector: aSymbol
(define (lookupSelector: methodDict symbol)
  (hashtable-ref methodDict symbol nil)) ;; smalltalk nil

;; methodDict addSelector: selector withMethod: compiledMethod
(define (addSelector:withMethod: methodDict symbol methodClosure)
  (hashtable-set! methodDict symbol methodClosure))

;; methodDict selectors
(define (selectors methodDict)
  (hashtable-keys methodDict))

(define (includesSelector: methodDict symbol)
  (hashtable-includes-key methodDict symbol))


(define (selectorsDo: methodDict closure)
  (for-each closure (hashtable-keys methodDict)))

(define (selectorsAndMethodsDo: methodDict closure)
  (vector-for-each closure (hashtable-entries methodDict)))

(define (selectorsAndMethodsDo: methodDict closure)
  (let-values ( ((selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure selectors methods)))

(define (methodsDo: methodDict closure)
  (let-values ( ((ignored-selectors methods) (hashtable-entries methodDict)) )
    (vector-for-each closure methods))) 


;;;			--- E O F ---			;;;
