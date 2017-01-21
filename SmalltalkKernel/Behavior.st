"FILE: 'Behavior.st'
 IMPLEMENTS: fill in class Behavior
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"


"Behavior ~> >> aSelector
[
	^ self methodDict at: aSelector
]."


Behavior ~> canUnderstand: selector
[
"Answer whether the receiver can respond to the message whose selector is the argument.
 The selector can be in the method dictionary of the receiver's class or any of its superclasses."
	^ self methodDict includesKey: selector
].

Behavior ~> classPool "DEPRICATED"
[
  self error: 'Class Pools are DEPRICATED'
].


Behavior ~> inheritsFrom: aClass
[
"Answer whether the argument is on the receiver's superclass chain."
  | this |
  this := self superclass.
  [ this == nil ]
    whileFalse: [ 
      this == aClass
        ifTrue: [ ^ true ].
        this := this superclass
  ].

  ^ false
].

Behavior ~> instSize
[
"Answer the number of named instance variables of the receiver."
  ^ self allInstVarNames size
].

Behavior ~> isBehavior
[
  "Answer true if I am a subclass of Behavior."
  ^ true
].


Behavior ~> isVariable
[
  "Answer true if the receiver has indexable fields."
  ^ self isKindOf: ArrayedCollection  "@@@FIXME@@@"
].


Behavior ~> printOn: aStream
[
  aStream nextPutAll: self name
].


Behavior ~> sharedPools "DEPRICATED"
[
  self error: 'Shared Pools are DEPRICATED'
].


"		E O F 		"

