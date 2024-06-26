;;; FILE: "st-dictionary.scm"
;;; IMPLEMENTS: Dictionary (= eqv-hashtable)
;;;             IdentityDictionary (== eq-hashtable)
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (requires 'st-core-classes)
;; (requires 'st-collection)
;; (requires 'st-set)

;; It's a desert topping _and_ a floor wax.
;; Er, its a Dictionary _and_ a Set of Associations.
;; Huh? Man, this be confuzin!

;;; @@FIXME: Associations
;; This class definition is completed at the Smalltalk
;; level because Associations are defined in the
;; Smalltalk layer.

;; One is tempted to alias Association with a Cons cell
;; but this punning could lead to strange bugs, so
;; Associations are full St objects at this time.

;; (requires 'st-set)

(define Dictionary ;; equal-hashtable
  (newSubclassName:iVars:cVars:
   Set
   'Dictionary '() '())
)

(define IdentityDictionary  ;; eq-hashtable
  (newSubclassName:iVars:cVars:
   Dictionary
   'IdentityDictionary '() '())
)

(set! st-dictionary-behavior
      (perform: Dictionary         'methodDict))

(set! st-identity-dictionary-behavior
      (perform: IdentityDictionary 'methodDict))

(perform:with:
     Dictionary
     'category:
     (string->symbol "Collections-Unordered"))

(perform:with:
     Dictionary
     'comment:
"I represent a set of elements that can be viewed from one of
 two perspectives: a set of associations, or a container of values
 that are externally named where the name can be any object that
 responds to =. The external name is referred to as the key.
 I inherit many operations from Set."
)

(perform:with:
     IdentityDictionary
     'comment:
"I am like a Dictionary, except that keys are compared with #== instead of #="
)

(addSelector:withMethod:
     Dictionary
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Dictionary)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     IdentityDictionary
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'IdentityDictionary)
           (superPerform:with: self 'is: symbol))))


(addSelector:withMethod:
     (class Dictionary)
     'new:
     (lambda (self size) ;; eqv -> #=
       (make-hashtable size)))

(addSelector:withMethod:
     (class IdentityDictionary)
     'new:
     (lambda (self size) ;; eq -> #==
       (make-eq-hashtable size)))


(addSelector:withMethod:
     (class Dictionary)
     'new
     (lambda (self)
       (make-hashtable)))

(addSelector:withMethod:
     (class IdentityDictionary)
     'new
     (lambda (self)
       (make-eq-hashtable)))


(addSelector:withMethod:
     Dictionary
     'size
     hashtable-size)

(addSelector:withMethod:
     Dictionary
     'at:ifAbsent:
     (lambda (self key absentThunk)
       (if (hashtable-contains? self key)
           (hash-ref self key nil)
           (if (st-nil? absentThunk) ;; St ideom
               st-nil
               (absentThunk))))
)

(addSelector:withMethod:
     IdentityDictionary
     'at:ifAbsent:
     (lambda (self key absentThunk)
       (if (eq-hashtable-contains? self key)
           (hashq-ref self key nil)
           (if (st-nil? absentThunk) ;; St ideom
               st-nil
               (absentThunk))))
)

(addSelector:withMethod:
     Dictionary
     'at:
     (lambda (self key)
       ($:: self
            'at:ifAbsent:
            key
            (lambda () (error "key not found" self key))))
)

(addSelector:withMethod:
     Dictionary
     'at:ifAbsentPut:
     (lambda (self key valueThunk)
       (when (hashtable-contains? self key)
         (hash-set! self key (valueThunk)))
       self)
)

(addSelector:withMethod:
     IdentityDictionary
     'at:ifAbsentPut:
     (lambda (self key valueThunk)
       (when (eq-hashtable-contains? self key)
         (hashq-set! self key (valueThunk)))
       self)
)

(addSelector:withMethod:
     Dictionary
     'at:ifPresent:ifAbsent:
     (lambda (self key presentThunk absentThunk)
       (if (hashtable-contains? self key)
           (presentThunk)
           (absentThunk)))
)

(addSelector:withMethod:
     Dictionary
     'at:ifPresent:
     (lambda (self key presentThunk)
       (if (eq-hashtable-contains? self key)
           (presentThunk)
           self))
)

(addSelector:withMethod:
     Dictionary
     'includesKey:
     (lambda (self key)
       (and (hash-get-handle self key) #t))
)

(addSelector:withMethod:
     IdentityDictionary
     'includesKey:
     (lambda (self key)
       (and (hashq-get-handle self key) #t))
)

(addSelector:withMethod:
     Dictionary
     'at:put:
     (lambda (self key value)
       (hash-set! self key value)
       value)
)

(addSelector:withMethod:
     IdentityDictionary
     'at:put:
     (lambda (self key value)
       (hashq-set! self key value)
       value)
)

(addSelector:withMethod:
     Dictionary
     'copy
     (lambda (self)
       (hashtable-copy self))
)

(addSelector:withMethod:
     Dictionary
     'keysDo:
     (lambda (self aBlock)
       (for-each aBlock (hashtable-keys self)))
)

(addSelector:withMethod:
     Dictionary
     'keysAndValuesDo: 
     (lambda (self twoArgBlock)
       (hash-for-each twoArgBlock self))
)


(addSelector:withMethod:
     Dictionary
     'valuesDo: 
     (lambda (self aBlock)
       (let ( (values (hash-fold (lambda (key val accum) (cons val accum)) '() self)) )
         (for-each aBlock values)))
)

(addSelector:withMethod:
     Dictionary
     'keysArray
     (lambda (self) (list->vector (hashtable-keys self)))
)

(addSelector:withMethod:
     Dictionary
     'keys   ;; Answer an IdentitySet of Keys
     ;; @@FIXME: this should answer the set
     ;;          of non-inherited selectors.
     (lambda (self)
       (let ( (iSet ($ IdentitySet 'new)) )
         ($: self
             'keysDo:
             (lambda (sel) ($: iSet 'add: sel)))))
)

(addSelector:withMethod:
     Dictionary
     'valuesArray
     (lambda (self)
       (list->vector
        (hash-fold (lambda (key val accum) (cons val accum)) '() self)))
)

(addSelector:withMethod:
     Dictionary
     'removeKey:ifAbsent:
     (lambda (self key absentThunk)
       (if (hashtable-contains? self key)
           (hashtable-remove! self key)
           (absentThunk)))
)

(addSelector:withMethod:
     IdentityDictionary
     'removeKey:ifAbsent:
     (lambda (self key absentThunk)
       (if (eq-hashtable-contains? self key)
           (eq-hashtable-remove! self key)
           (absentThunk)))
)

(addSelector:withMethod:
     Dictionary
     'removeKey:
     (lambda (self key)
       ($:: self
            'removeKey:ifAbsent:
            key
            (lambda () (error "key not found" self key))))
)

(addSelector:withMethod:
     Dictionary
     'keysAndValuesRemove: 
     (lambda (self twoArgPredicate?)
       (let-values ( ((keys-vec vals-vec) (hashtable-entries self)) )
         (let ( (keys-to-remove '()) )
           (vector-for-each
            (lambda (k v)
              (when (twoArgPredicate? k v)
                (set! keys-to-remove
                      (cons k keys-to-remove))))
            keys-vec
            vals-vec)
           (for-each (lambda (k) ($: self 'removeKey k))
                     keys-to-remove))))
)

;;; fillIn@@dictionary

;; (provides st-dictionary)

;;;			--- E O F ---			;;;
