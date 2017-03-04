# Crosstalk

## Smalltalk in Scheme -- clean room bootstrap of Smalltalk kernel in R7RS Larceny Scheme

Basically an exercise in deeply understanding Smalltalk.

I like to build an executable model to get the semantics down properly.

Doing so in a different language with a well understood semantics is a good exercise.  It allows one to concentrate on language differences.

This is very much a work in progress.

## Basics:
  - Message send 
    - Object behaviors are dictionaries/hash-tables
  - Leverage Scheme runtime
    - Scheme numbers, strings, characters, vectors/arrays, bytevectors ..
    - Closures, GC, exceptions, finalization
    - Dynamic compilation to native machine code
  - Keep things as simple as possible
    - Comprehension over speed, but don't be odiously dumb

## Open Questions/Problems:
  - Can Larceny Scheme's *Environments* be leveraged to provide cleanly separated Scheme and Smalltalk namespaces?
  - #thisContext: How to introduce decent debugging?
  - What kinds of caching would make significant performance difference?
      + Cache single assignment multiple use (behavior, method)
      - Symbol property-list
      - PIC at call sites
      - Hashtable cache
  - Finalization pecularities (expectation of early finalization)
  - How best to make Smalltalk object structures native to Larceny?
  - What compiler/runtime changes would help?
  - #allInstancesDo:
  - #become:
  - Dynamic class changes & schema evolution (w lazy instance update)

## The story so far..
  1. Dispatch mechanics (message send with #perform:)
  2. Class Structure (bootstrap + fixup; class/metaClass hierarchy setup)
  3. Basic unit tests (need to be populated)
  4. Translation of st kernel code into scheme. [***In Progress***]
     + needs Environments for name lookup [basics working w/o analysis]
     + needs Return anaylsis & simplification [call/cc proto working]
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
````
````Smalltalk
 "[|d| 
      d := Dictionary new. 
      d at: #a put: 1;
        at: #b put: 2;
        at: #c put: 3. 
      d collect: [ :val | val squared ]
   ] value printString."
)
````
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
````

## Upcoming
  - Fill in the class structure (approx ANSI)
    + Present Scheme native objects [**Basics working; needs fill-in**]
  - Parsing Smalltalk with location/position info [**Mostly working**]
    + Need to pass location info thru for debug [to do!]
  - Read and execute Smalltalk code directly
    + Fill in bootstrap by transliterating Smalltalk runtime [*in progress*]
    + Check baseline by porting ANSI unit test suite.
  - Look at runtime issues
  - _Bootstrap Morphic_
    
## Processing Notes
  - The control file is "sis.scm".  This imports Scheme functions and loads the system.
  - There are two phases: the Scheme bootstrap and the Smalltalk bootstrap
    + The Scheme bootstrap implements St code function and exposes Scheme datatypes.
    This code also includes a St parser and translator into Scheme which allows for the St bootstrap to be translated.
    + The Smalltalk bootstrap is the St kernel code which is xlated and loaded. 

The main idea here is to minimize the Scheme code and max the Smalltalk code.

How small can we make the Scheme support core?

How best to stage in Smalltalk functionality?
  - Compiling (St in St)
  - Debug (w St sources)
  - Unit testing; ANSI & system self test
  - Morphic UI/graphic framework
  - Host Cuis Smalltalk on top of this St kernel
