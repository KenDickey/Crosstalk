;;; FILE: "object.sch"
;;; IMPLEMENTS: Smalltalk objects
;;; AUTHOR: Ken Dickey
;;; DATE: 12 May 2016

(define nil  '())
(define true  #t)
(define false #f)

(define nil? null?)

;;; Representations
;
; Use Scheme immediates & numbers
;	byte-tag + mask -> index into table of classes
; Use tagged Vector as ST Object (named & indexed slots)
;   1st slot in Vector contains a mDict (method dictionary)
;	with a binding of 'class->(lambda (self) <class>) to get the class
; Use byte-vector for ST bytevector w hidden bytes at end for
;	index into class/mTable-vector (#size elides this)

; For faster interpreted lookup: (trade space for speed)
;	Keep track of selectors for each class
;	Aggregate all methods into mDict at each level
;		=> one hashtable ref for any lookup
;	When methods added/removed/updated, fixup all mDicts
;		in lookup chain (closures maximally shared)

; Bytevec tag at end after class/mDict index
; @@REVIST when switch to custom bytevec typetag
; BVecs rounded to even size
;   Use last byte to subtract 1 if required for
;   "official" size.
;; Wake up and smell the Coffee ! ;^)
(define byteVec-tag-even #u8( C0 FF EE 00 ) )
(define byteVec-tag-odd  #u8( C0 FF EE 01 ) )
; need to subtract tag + mDict-index size from bytevec bytes
(define byteVec-mDict-id-size 4)  ; 4 byte index
(define byteVec-mDict-id+tag-size 8)
; So sub tag + last-byte to get official bvec size for ST
; NB: ST indexes 1-based, Scheme 0-based !!

(define (st-make-byteVec+fill size fill mDict-id)
  (bytevector-append
   ;; round to even number of bytes
   (make-bytevector (if (even? size) size (+ 1 size)) fill)
   (u8->byteVec mDict-id)
   (if (even? size)
       byteVec-tag-even
       byteVec-tag-odd))
)

(define (st-make-byteVec size mDist-id)
  (st-make-byteVec+fill size 0 mDict-id))

(define (u8->bytevec u8int)
  @@@..@@@
)


;;;			--- E O F ---			;;;
