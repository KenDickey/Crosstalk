#!r6rs
;; Import order important

(import
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)	
   (st-sequence-coll)
   (st-array-coll)
   (st-array)
   (st-boolean)
   (st-character)
   (st-string)
   (st-symbol)
   (st-list)
   (st-magnitude)
   (st-number)
)

;; (define scm-bootstrap-file-names
;;   '( "st-kernel"       ;; message mechanics
;;      "st-object"       ;; Object behavior
;;      "st-core-classes" ;; Object Class MetaClass ClassDescription Behavior
;;      "st-boolean"      ;; Boolean True False UndefinedObject (nil)
;;      "st-character"    ;; Character
;;      "st-magnitude"
;;      "st-number"
;;      "st-collection"
;;      "st-string"       ;; String
;;      "st-symbol"       ;; Symbol
;;      "st-list"         ;; proper, immutable lists (interoperate w Scheme)
;;      "st-blockClosure" ;; BlockClosure
;;      "st-array"        ;; Array
;;      "st-error-obj"    ;; Scheme error objects
;;      "st-tokenizer"    ;; Stream -> tokens
;;      "st-parse"        ;; tokens -> AST
;;      "st-xlate"	       ;; AST -> Scheme
;;      "st-stream"       ;; Stream CharStream ByteStream
;;      "st-set"          ;; Set
;;      "st-dictionary"   ;; Dictionary
;;      "st-date-time"    ;; PointInTime Duration DateAndTime
;;      "st-conditions"   ;; Map Scheme Conditions to St Exceptions
;;     )
;;  )

;;;			--- E O F ---			;;;
