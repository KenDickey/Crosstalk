"FILE: 'Collection.st'
 IMPLEMENTS: Collection (abstract) protocol
 AUTHOR: Ken Dickey
 DATE: 08 January 2017

 NOTE: Collection, SequenceableCollection, ArrayedCollection def'ed in st-collection.scm
"


Collection >> add: newObject
[
	"Include newObject as one of my elements. Answer newObject. ArrayedCollections cannot respond to this message."
    self subclassResponsibility
]

Collection >> asArray
[
	"Answer an Array whose elements are the elements of this collection. The order in which elements are added depends on the order in which this collection enumerates its elements. In the case of unordered collections, the ordering is not necessarily the same for multiple requests for the conversion."
| result i |
  result := Array new: self size.
  i := 0.
  self do: [ :each | result at: (i := i + 1) put: each ].
  ^ result
]

Collection >> asByteArray
[
	"Answer a ByteArray containing my elements."
| result i |
  result := ByteArray new: self size.
  i := 0.
  self do: [ :each | result at: (i := i + 1) put: each ].
  ^ result
]

Collection >> asSet
[
	"Answer a Set whose elements are the unique elements of the receiver."
| aSet |
  aSet := Set new: self size.
  self do: [ :each | aSet add: each ].
  ^ aSet
]

Collection >> collect: aBlock
[
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect the resulting values into a collection like the receiver. Answer 
	the new collection."
  | newCollection |
  newCollection := self species new.
  self do: [ :each | newCollection add: (aBlock value: each) ].
  ^ newCollection
]

Collection >> detect: aBlock ifNone: exceptionBlock
[
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."
  self do: [ :each | 
		(aBlock value: each)
			ifTrue: [ ^ each ] ].
  ^ exceptionBlock value
]

Collection >> do: aBlock
[
	"Evaluate aBlock with each of the receiver's elements as the argument."
  self subclassResponsibility
]

Collection >> emptyCheck
[
  self isEmpty
	ifTrue: [ self errorEmptyCollection ]
]

Collection >> errorEmptyCollection
[
  self error: 'this collection is empty'
]

Collection >> errorNotFound
[
  self error: 'Object is not in the collection.'
]

Collection >> includes: anObject
[
	"Answer whether anObject is one of the receiver's elements."
  self
	do: [ :each | 
		anObject = each
			ifTrue: [ ^ true ] ].
  ^ false
]

Collection >> isEmpty
[
	"Answer whether the receiver contains any elements."
  ^ self size = 0
]

Collection >> printOn: aStream
[
	"Refer to the comment in Object|printOn:."
  aStream nextPutAll: self class name , ' ('.
  self
	do: [ :element | 
		element printOn: aStream.
		aStream space ].
  aStream nextPut: $)
]

Collection >> remove: oldObject
[
	"Remove oldObject as one of the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, create an error 
	notification."
  ^ self remove: oldObject ifAbsent: [ self errorNotFound ]
]

Collection >> remove: oldObject ifAbsent: anExceptionBlock
[
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. SequenceableCollections 
	cannot respond to this message."
  self subclassResponsibility
]

Collection >> select: aBlock
[
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for 
	which aBlock evaluates to true. Answer the new collection."
  | newCollection |
  newCollection := self species new.
  self
	do: [ :each | 
		(aBlock value: each)
			ifTrue: [ newCollection add: each ] ].
  ^ newCollection
]

Collection >> size
[
	"Answer how many elements the receiver contains."
  | count |
  count := 0.
  self do: [ :each | count := count + 1 ].
  ^ count
]

Collection >> sum
[
	"Answer the sum of the elements of this collection. If the collection is empty, answer zero."
	"Details: Use an arbitrary element of the collection as the initial value so this method will work for collections of any kind of object that understands + and -."
  | total seed |
  total := seed := self detect: [ :x | true ] ifNone: [ ^ 0 ].
  self do: [ :el | total := total + el ].
  ^ total - seed	"subtract the seed value from the total"
]


Collection class >> with: anObject
[
	"Answer an instance of me containing anObject."
  | newCollection |
  newCollection := self new.
  newCollection add: anObject.
  ^ newCollection
]

Collection class >> with: firstObject with: secondObject
[
	"Answer an instance of me containing the two arguments as elements."
  | newCollection |
  newCollection := self new.
  newCollection add: firstObject.
  newCollection add: secondObject.
  ^ newCollection
]

Collection class >> with: firstObject with: secondObject with: thirdObject
[
	"Answer an instance of me containing the three arguments as elements."
  | newCollection |
  newCollection := self new.
  newCollection add: firstObject.
  newCollection add: secondObject.
  newCollection add: thirdObject.
  ^ newCollection
]


"		--- E O F ---		"
