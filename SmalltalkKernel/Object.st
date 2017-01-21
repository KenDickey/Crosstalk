"FILE: 'Object.st'
 IMPLEMENTS: Augments Object protocol
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 19 January 2017
"

Object ~> -> anObject
[
"Answer an Association with myself as its key and anObject as its value."

	^Association new key: self value: anObject
].


Object ~> asLink
[
	^ ValueLink value: self
].


Object ~> asString
[
	^ self printString
].

Object ~> species
[
"Answer the preferred class for reconstructing the receiver.  For example, 
 collections create new collections whenever enumeration messages such as 
 collect: or select: are invoked.  The new kind of collection is determined by 
 the species of the original collection.  Species and class are not always the 
 same.  For example, the species of Interval is Array."

	^self class
].

Object ~> errorImproperStore
[
"Error: an improper store was attempted."
	self error: 'Improper store into indexable object.'
].

Object ~> errorNonIntegerIndex
[
"Error: attempting to use a non-integer object as an index."
	self error: 'Only integers should be used as indices.'
].

Object ~> errorSubscriptBounds: index
[
"Error: the given index is out of range."
	self error: 'Subscript is out of bounds: ' , index printString
].


Object ~> isBehavior
[
"Answer true if I am a subclass of Behavior."
	^ false
].

Object ~> isCompiledMethod
[
	^ false
].

Object ~> isContextPart
[
	^ false
].

Object ~> isInteger
[
"Overridden to return true in Integer."
	^ false
].


Object ~> isNumber
[
"Overridden to return true in Number, natch"
	^ false
].

Object ~> isSelfEvaluating
[
	^ false
].

Object ~> isSmallInteger
[
	^ false
].

Object ~> mustBeBoolean
[
"Error: attempt to use a non-Boolean object as if it were true or false.
 This message is sent by the virtual machine."
self error: 'NonBoolean receiver.'.
	^ true
].

Object ~> nextInstance
[
"Primitive. Answer the next instance after the receiver
 in the enumeration of all instances of this class.
 Fails if all instances have been enumerated. Essential.
 See Object documentation whatIsAPrimitive."
	self error: '<primitive: 78> nextInstance NYI'.
	^ nil
].

Object ~> nextObject
[
"Primitive. Answer the next object after the receiver in the
 enumeration of all objects.
 Answer 0 when all objects have been enumerated."
	self error: '<primitive: 139> nextObject NYI'.
	"<primitive: 139>"
	self primitiveFailed
].

Object ~> primitiveFailed
[
"Announce that a primitive has failed."
	self error: 'a primitive has failed'
].

"
Object ~> printOn: aStream
[
""Print a description of me on the given stream.""
	| n |
	n := self class name.
	aStream
		nextPutAll:
			(n first isVowel
				ifTrue: [ 'an ' ]
				ifFalse: [ 'a ' ]) , n
]."


Object ~> putcr
[
	"Write a carriage return to the standard output stream."
	self putAscii: 13
].

Object ~> shouldBePrintedAsLiteral
[
	^ false
].

Object ~> shouldNotImplement
[
"Announce that, although the receiver inherits this message,
 it should not implement it."
	self error: 'This message is not appropriate for this object'
].

Object ~> someObject
[
"Primitive. Answer the first object in the enumeration of all objects."
	self error: '<primitive: 138> NYI'.
	self primitiveFailed
].

Object ~> species
[
"Answer the preferred class for reconstructing the receiver,
 which is typically some kind of collection. 
For example, the response to this message determines the type
 of object returned by the collect: or select: messages. 
Species and class are not always the same. 
For example, the species of Interval is Array."
	^ self class
].

Object ~> subclassResponsibility
[
"This message sets up a framework for the behavior of subclasses
 of this class.
Announce that the subclass should have implemented this message."
	self error: 'My subclass should have overridden one of my messages.'
].

Object ~> yourself
[
"Answer the receiver. Useful in cascaded message expressions."
	^ self
].


Object class ~> cannotInstantiate
[
"This message is sent if you try to create new instances
 of classes with unique instances such as booleans or characters."
	self error: 'You cannot create new instances of ' , self name
].


"		E O F 		"
