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
     'at:
     (lambda (self key)
       (let ( (result (hashtable-ref self key '%%bogus%%)) )
         (if (eq? result '%%bogus%%)
             (error "Key not found" self key) ;;@@FIXME: keyNotFound error
             result)))
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


;;; fillIn@@dictionary

;; (provides st-dictionary)

;;;			--- E O F ---			;;;
