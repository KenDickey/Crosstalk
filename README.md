# Crosstalk

## Smalltalk in Scheme -- Simple bootstrap of Smalltalk kernel in R6RS Scheme
currently being ported to Chez Scheme 10 (in progress)

Basically an exercise in deeply understanding Smalltalk.

I like to build an executable model to get the semantics down properly.

Doing so in a different language with a well understood semantics is a good exercise.  It allows one to concentrate on language differences.

This is very much a work in progress.


### If you are looking for Smalltalk-80 for Raspberry Pi see 
  https://github.com/michaelengel/crosstalk

## Basics:
  - Message send 
    - Object behaviors are dictionaries/hash-tables
    - Scheme values are presented as Smalltalk Objects by supplying their behaviors
  - Leverage Scheme runtime system
    - Scheme numbers, strings, characters, vectors/arrays, bytevectors ..
    - Closures, ports, GC, exceptions, finalization..
    - Dynamic compilation to native machine code
  - Keep things as simple as possible
    - Comprehension over speed, but don't be excessvely dumb

## The story so far..
  1. Dispatch mechanics (message send with #perform:)
  2. Class Structure (bootstrap + fixup; class/metaClass hierarchy setup)
  3. Basic unit tests -- both Scheme core tests and SUnit
     + SUnit self test passes
     + Exceptions unit tests pass (call/cc is very useful!)
  4. Translation of Smalltalk kernel code into Scheme **[basics in place]**
     + needs Environments for name lookup [basics working w/o analysis]
     + needs Return anaylsis & simplification
     + needs to inline primops (e.g. +, ifTrue:)
     + convert procedures into syntaxtic transforms (macros) for speed


# Object behaviors are dictionaries/hash-tables 
````Scheme
(define (perform: self selectorSym)
  ((lookupSelector: self selectorSym) self))

(define (perform:with: self selectorSym arg)
  ((lookupSelector: self selectorSym) self arg))
...
(define (lookupSelector: self selectorSym) ;; Polymorphic
  (primLookup: (behavior self) selectorSym))
...
(define $ perform:)
(define $: perform:with:)
...

(define st-source
 "[|d| 
      d := Dictionary new. 
      d at: #a put: 1;
        at: #b put: 2;
        at: #c put: 3. 
      d collect: [ :val | val squared ]
   ] value printString."
)

(st-eval st-source)
==>  "Dictionary( (#'c'->9) (#'a'->1) (#'b'->4) )"


(st->scm st-source)
==> 
($ ($ (lambda ()
        (let ((d nil))
          (let ((%%val%% ($ (smalltalkAt: 'Dictionary) 'new)))
            (set! d %%val%%)
            %%val%%)
          (let ((receiver d))
            ($:: d 'at:put: 'a 1)
            ($:: receiver 'at:put: 'b 2)
            ($:: receiver 'at:put: 'c 3))
          ($: d 'collect: (lambda (val) ($ val 'squared)))))
      'value)
   'printString)
...
(addSelector:withMethod:
     Array
     'basicSize
     (lambda (self)
       (vector-length self)))

(addSelector:withMethod:
     (class Array)
     'basicNew:
     (lambda (self size)
       (make-vector size st-nil)))

(st-eval "(Array new: 3)")
--> #(() () ())  ;; Scheme value

(st-eval "(Array new: 3) printString")
--> "#( nil nil nil )" ;; As seen in Smalltalk

($ (vector 1 2 3 'a "bee" #\c) 'printString)
--> "#( 1 2 3 #'a' 'bee' $c )"  ;; Scheme values are St Objects

(st->scm "ArrayedCollection ~> includes: anObject
[
\"Answer whether anObject is one of the receiver's elements.\"
  self do: [ :each | 
		anObject = each
			ifTrue: [ ^ true ] 
  ].
  ^ false
]")

-->
($:: (smalltalkAt: 'ArrayedCollection)
     'addSelector:withMethod:
     'includes:
     (lambda (self anObject)
       (call/cc
         (lambda (return)
           ($: self
               'do:
               (lambda (each)
                 ($: ($: anObject '= each)
                     'ifTrue:
                     (lambda () (return true)))))
           (return false)))))
````

## Upcoming
  - Fill in the class structure (approx ANSI)
    + Present Scheme native objects [**Basic mechanics working; needs API fill-in, dispatch optimization**]
  - Parsing Smalltalk with location/position info
    + Need to pass location info thru for debug [TO DO!]
  - Read and execute Smalltalk code directly
    + Fill in bootstrap by transliterating Smalltalk runtime [*in progress*]
    + Check baseline by porting unit test suites.
  - Look at runtime issues
  - Develop sensible combined Foreign Function Interface & Debugging infrastructure
  - **Bootstrap Morphic** !    

## Processing Notes [outdated]
  - The control file is **"guile-sis.scm"**.  This imports Scheme functions and code to load the system.
  - There are two phases: the Scheme bootstrap **(load-source-bootstrap)** and the Smalltalk core classes **(add-st-kernel)**.
    + The Scheme bootstrap implements Smalltalk semanics and exposes Scheme datatypes.
    This code also includes a Smalltalk parser and translator into Scheme which allows for the Smalltalk bootstrap to be translated.
    + The Smalltalk bootstrap is the Smalltalk kernel code which is translated and loaded. 
	+ Note expressions in file "debug.txt"

One goal is to maximize the Smalltalk code and keep the Scheme kernel small.

How small can we make the Scheme support core?

How best to stage in Smalltalk functionality?
  - Compiling (St in St)
  - Debug (w St sources)
  - Foreign Function Interface
  - Morphic UI/graphic framework
  - Host Cuis Smalltalk on top of this St kernel

## Open Questions/Problems:
  - #thisContext: How to introduce decent debugging?
  - What kinds of caching would make significant performance difference?
      + Cache single assignment multiple use (behavior, method)
      - Symbol property-list
      - PIC at call sites
      - Hashtable cache
	  - Make fast without caches at all..?
  - Finalization pecularities (expectation of early finalization)
  - How best to make Smalltalk object structures native to Larceny?
  - What compiler/runtime changes would help?
  - #allInstancesDo:
  - #become:
  - Dynamic class changes & schema evolution (w lazy instance update)


## In Progress -- Chez Scheme port (basics, then read, compile ..)

https://github.com/cisco/ChezScheme
