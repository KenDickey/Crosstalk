;; "SampleSubclass.ss"
;; Create a SampleClass, subclass of Object

 ;; (load "st-core-classes.ss")
 ;; (load "st-create-subclass.ss")
 ;; (load "st-core-methods.ss")

(define SampleClass
  (newSubclassName:iVars:cVars:
   Object
   'SampleClass
   '(ivar1 ivar2 ivar3)
   '(cvar1 cvar2)))

(addSelector:withMethod:
 	(class SampleClass) ;; class init
        'initialize
        (lambda (SampleClass)
	  ($: SampleClass 'cvar1: 'a)
	  ($: SampleClass 'cvar2: 'b)
	  SampleClass))

(addSelector:withMethod:
 	SampleClass ;; instance init
        'initialize
        (lambda (aSample)
	  ($: aSample 'ivar1: 11)
	  ($: aSample 'ivar2: 22)
	  ($: aSample 'ivar3: 33)
	  aSample))

;;; E O F ;;;
