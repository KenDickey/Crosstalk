# Smalltalk Kernel

## Conventions

The *.st files here augment those *.scm files to
 create a baseline for reading in more interesting
 Smalltalk code.

The pattern is
````Smalltalk
SuperClass newSubclassName: #NewSub iVars: #(foo bar) cVars: nil.

NewSub ~> message-pattern
[
  ..code..
  ..self foo..
  self foo: ..
].
````

## Restrictions:
+ Must use setters and getters for instance variable access.
+ Capitalized identifier -> global (in Smalltalk dictionary)
+ Lower case identifier -> defined local/block-formal or (self|super|nil|true|false)
+ A period is required after method blocks (note that the message pattern preceeds a method block).

Note: the **#'~>'** syntax was chosen to avoid confusing use of the **#'>>'** selector sent to a class to return a method.  Also, **#'~>'** is a clue that the syntax differs slightly from that used in GNU Smalltak, PharoCandle, or PharoKernel.

Note driver file **"guile-sis.scm"** in our containing directory.  

Use **(add-st)** which assumes a Crosstalk/Temp directory exists.

