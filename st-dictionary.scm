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

(define Dictionary
  (newSubclassName:iVars:cVars:
   Set
   'Dictionary '() '())
)

(define IdentityDictionary
  (newSubclassName:iVars:cVars:
   Dictionary
   'IdentityDictionary '() '())
)

(set! st-dictionary-behavior  (perform: IdentityDictionary 'methodDict))

(perform:with:
     Dictionary
     'category:
     '|Collections-Unordered|)

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
     (class Dictionary)
     'new:
     (lambda (self size) ;; eqv -> #=
       (make-eqv-hashtable size)))

(addSelector:withMethod:
     (class IdentityDictionary)
     'new:
     (lambda (self size) ;; eq -> #==
       (make-eq-hashtable size)))


(addSelector:withMethod:
     (class Dictionary)
     'new
     (lambda (self)
       (make-eqv-hashtable)))

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
           (hashtable-ref self key nil)
           (absentThunk)))
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
         (hashtable-set! self key (valueThunk))))
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
       (if (hashtable-contains? self key)
           (presentThunk)
           st-nil))
)


(addSelector:withMethod:
     Dictionary
     'includesKey:
     (lambda (self key)
       (hashtable-contains? self key))
)

(addSelector:withMethod:
     Dictionary
     'at:put:
     (lambda (self key value)
       (hashtable-set! self key value)
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
       (let ( (keys-vec (hashtable-keys self)) )
         (vector-for-each aBlock keys-vec)))
)

(addSelector:withMethod:
     Dictionary
     'keysAndValuesDo: 
     (lambda (self twoArgBlock)
       (let-values ( ((keys-vec vals-vec)(hashtable-entries self)) )
         (vector-for-each twoArgBlock keys-vec vals-vec)))
)


(addSelector:withMethod:
     Dictionary
     'valuesDo: 
     (lambda (self aBlock)
       (let-values ( ((keys-vec vals-vec)(hashtable-entries self)) )
         (vector-for-each aBlock vals-vec)))
)

(addSelector:withMethod:
     Dictionary
     'keysArray
     (lambda (self) (hashtable-keys self))
)

(addSelector:withMethod:
     Dictionary
     'valuesArray
     (lambda (self)
       (let-values ( ((keys-vec vals-vec)(hashtable-entries self)) )
         vals-vec))
)

(addSelector:withMethod:
     Dictionary
     'removeKey:ifAbsent:
     (lambda (self key absentThunk)
       (if (hashtable-contains? self key)
           (hashtable-delete! self key)
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
       (let-values ( ((keys-vec vals-vec)(hashtable-entries self)) )
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
