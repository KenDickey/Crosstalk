# Smalltalk Kernel

## Conventions

The *.st files here augment those *.scm files to
 create a baseline for reading in more interesting SMalltalk code.

The convention is
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
+ Lower case identifier -> local or (self|super|nil|true|false)


