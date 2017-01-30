"FILE: 'Dictionary.st'
 IMPLEMENTS: fill in class Dictionary
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"

"Dictionary category: #'Collections-Unordered'."

Dictionary ~> add: anAssociation
[
  self at: anAssociation key put: anAssociation value.
  ^ anAssociation
].

Dictionary ~> associationAt: key
[
  ^ self associationAt: key ifAbsent: [ self errorKeyNotFound ]
].

Dictionary ~> associationAt: key ifAbsent: aBlock
[
"Answer the association with the given key.
If key is not found, return the result of evaluating aBlock."
  (self includesKey: key)
	ifTrue:  [ Association key: key value: (self at: key) ]
	ifFalse: [ aBlock value ].
].

Dictionary ~> associationsDo: aBlock
[
"Evaluate aBlock for each of the receiver's
 elements (key/value associations)."
  self keysAndValuesDo: [ :key :value |
     aBlock value: (Association key: key value: value)
  ].
  ^ self
].

Dictionary ~> collect: aBlock
[
"Evaluate aBlock with each of my values as the argument.
 Collect the resulting values into a collection that is like me.
 Answer with the new collection."
  | newCollection |
  newCollection := self species new: self size.
  self keysAndValuesDo: [ :key :value | 
	newCollection at: key put: (aBlock value: value)
  ].
  ^ newCollection
].

Dictionary ~> do: aBlock
[
  self associationsDo: aBlock "@@?? #valuesDo: ??@@"
].

Dictionary ~> errorKeyNotFound
[
  self error: 'key not found'
].

Dictionary ~> errorValueNotFound
[
  self error: 'value not found'
].


Dictionary ~> includes: anObject
[
  self valuesDo: [ :aValue | 
	(anObject = aValue)
		ifTrue: [ ^ true ]
  ].
  ^ false
].


Dictionary ~> keyAtValue: value
[
"Answer the key that is the external name for the argument, value. 
 If none, answer nil."
  ^ self keyAtValue: value ifAbsent: [ nil ]
].

Dictionary ~> keyAtValue: value ifAbsent: exceptionBlock
[
"Answer the key that is the external name for the argument, value. If 
 there is none, answer the result of evaluating exceptionBlock.
 : Use =, not ==, so stings like 'this' can be found. 
 Note that MethodDictionary continues to use == so it will be fast."
  self keysAndValuesDo: [ :aKey :aValue |
	(value = aValue)
	    ifTrue: [ ^ aKey ] 
  ].
  ^ exceptionBlock value
].

Dictionary ~> keys
[
	"Answer a Set containing the receiver's keys."
  ^ self keysArray asSet
].


Dictionary ~> printOn: aStream
[
  self size < 10
	ifTrue: [
	  aStream nextPutAll: self class name , '( '.
	  self associationsDo: [ :element | 
		element printOn: aStream.
		aStream space
	  ].
	  aStream nextPut: $)
	]
	ifFalse: [
		aStream nextPutAll: self class name, ' of size '.
		aStream nextPutAll: self size printString.
	]
].


Dictionary ~> remove: anObject
[
  self shouldNotImplement
].

Dictionary ~> remove: anObject ifAbsent: exceptionBlock
[
  self shouldNotImplement
].


Dictionary ~> select: aBlock
[
"Evaluate aBlock with each of my associations as the argument. Collect into a
new dictionary, only those associations for which aBlock evaluates to
true."
  | newCollection |
  newCollection := self species new.
  self associationsDo: [ :each | 
	(aBlock value: each value)
		ifTrue: [ newCollection add: each ] ].
^ newCollection
].


"		E O F 		"
