"FILE: 'OrderedCollection.st'
 IMPLEMENTS: class OrderedCollection
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 24 January 2017
"

SequenceableCollection newSubclassName: #OrderedCollection
	 iVars: #(array firstIndex lastIndex) cVars: nil.

OrderedCollection category: #'Collections-Ordered'.


OrderedCollection ~> add: newObject
[
"Append newObject to me. Equivalent to addLast:. Answer newObject."
  self lastIndex = self array size
	ifTrue: [ self makeRoomAtLast ].
  self lastIndex: self lastIndex + 1.
  self array at: self lastIndex put: newObject.
  ^ newObject
].


OrderedCollection ~> addFirst: newObject
[
"Add newObject to the beginning of the receiver. Answer newObject."
  self firstIndex = 1
	ifTrue: [ self makeRoomAtFirst ].
  self firstIndex: self firstIndex - 1.
  self array at: self firstIndex put: newObject.
  ^ newObject
].


OrderedCollection ~> at: anInteger
[
"Answer my element at index anInteger. at: is used by a knowledgeable
 client to access an existing element"
  (anInteger < 1 or: [ anInteger + self firstIndex - 1 > self lastIndex ])
	ifTrue: [ self errorNoSuchElement ]
	ifFalse: [ ^ self array at: anInteger + firstIndex - 1 ]
].


OrderedCollection ~> at: anInteger put: anObject
[
"Put anObject at element index anInteger. at:put: cannot be used to
 append, front or back, to an ordered collection; it is used by a
 knowledgeable client to replace an element."
  | index |
  index := anInteger asInteger.
  (index < 1 or: [ index + self firstIndex - 1 > self lastIndex ])
	ifTrue: [ self errorNoSuchElement ]
	ifFalse: [ ^ self array at: index + self firstIndex - 1 put: anObject ]
].

OrderedCollection ~> collect: aBlock
[
"Evaluate aBlock with each of my elements as the argument. Collect the 
 resulting values into a collection that is like me. Answer the new 
 collection. Override superclass in order to use add:, not at:put:."
  | newCollection |
  newCollection := self species new.
  self do: [ :each | newCollection add: (aBlock value: each) ].
  ^ newCollection
].

OrderedCollection ~> copyFrom: startIndex to: endIndex
[
"Answer a copy of the receiver that contains elements from position
 startIndex to endIndex."
  | targetCollection |
  endIndex < startIndex
	ifTrue: [ ^ self species new: 0 ].
  targetCollection := self species new: endIndex + 1 - startIndex.
  startIndex to: endIndex do: [ :index | targetCollection add: (self at: index) ].
  ^ targetCollection
].

OrderedCollection ~> copyReplaceFrom: start to: stop with: replacementCollection
[
"Answer a copy of the receiver with replacementCollection's elements in
 place of the receiver's start'th to stop'th elements. This does not expect
 a 1-1 map from replacementCollection to the start to stop elements, so it
 will do an insert or append."
  | newOrderedCollection delta startIndex stopIndex |
"if start is less than 1, ignore stop and assume this is inserting at the front. 
 if start greater than self size, ignore stop and assume this is appending. 
 otherwise, it is replacing part of me and start and stop have to be within my 
 bounds. "
  delta := 0.
  startIndex := start.
  stopIndex := stop.
  start < 1
	ifTrue: [ startIndex := stopIndex := 0 ]
	ifFalse: [ 
		startIndex > self size
			ifTrue: [ startIndex := stopIndex := self size + 1 ]
			ifFalse: [ 
				(stopIndex < (startIndex - 1) or: [ stopIndex > self size ])
					ifTrue: [ self error: 'indices are out of bounds' ].
				delta := stopIndex - startIndex + 1 ] ].
  newOrderedCollection := self species new: self size + replacementCollection size - delta.
  1 to: startIndex - 1 do: [ :index | newOrderedCollection add: (self at: index) ].
  1 to: replacementCollection size do: [ :index |
	 newOrderedCollection add: (replacementCollection at: index) ].
  stopIndex + 1 to: self size do: [ :index | newOrderedCollection add: (self at: index) ].
  ^ newOrderedCollection
].

OrderedCollection ~> do: aBlock
[
"Override the superclass for performance reasons."
  | index |
  index := self firstIndex.
  [ index <= self lastIndex ]
	whileTrue: [ 
		aBlock value: (self array at: index).
		index := index + 1 
	]
].

OrderedCollection ~> errorNoSuchElement
[
  self error: 'attempt to index non-existent element in an ordered collection'
].

OrderedCollection ~> first
[
"Answer the first element. If the receiver is empty, create an errror
 message. This is a little faster than the implementation in the superclass."
  self emptyCheck.
  ^ self array at: self firstIndex
].


OrderedCollection ~> grow
[
"Become larger.
 Typically, a subclass must override this method if it adds instance variables."
  | newArray |
  newArray := Array new: self size + (self array size max: 2).
  newArray
	replaceFrom: 1
	to: self array size
	with: self array
	startingAt: 1.
  self array: newArray
].


OrderedCollection ~> insert: anObject before: spot
[
  | index delta spotIndex |
  spotIndex := spot.
  delta := spotIndex - self firstIndex.
  self firstIndex = 1
	ifTrue: [ 
		self makeRoomAtFirst.
		spotIndex := self firstIndex + delta ].
  index := (self firstIndex: self firstIndex - 1).
  [ index < (spotIndex - 1) ]
	whileTrue: [ 
		self array at: index put: (self array at: index + 1).
		index := index + 1 ].
  self array at: index put: anObject.
  ^ anObject
].

OrderedCollection ~> last
[
"Answer the last element. If the receiver is empty, create an errror
 message. This is a little faster than the implementation in the superclass."
  self emptyCheck.
  ^ self array at: lastIndex
].

OrderedCollection ~> makeRoomAtFirst
[
  | delta index |
  delta := self array size - self size.
  (delta = 0)
	ifTrue: [ 
		self grow.
		delta := self array size - self size 
	].
  (self lastIndex = self array size)
	ifTrue: [ ^ self ].	"just in case we got lucky"
  index := self array size.
  [ index > delta ]
      whileTrue: [ 
	self array at: index
	          put: (self array at: index - delta + self firstIndex - 1).
	self array at: index - delta + self firstIndex - 1 put: nil.
	index := index - 1 .
      ].
  self firstIndex: delta + 1.
  self lastIndex: self array size
].


OrderedCollection ~> makeRoomAtLast
[
  | newLast delta |
  newLast := self size.
  self array size - self size = 0
	ifTrue: [ self grow ].
  (delta := self firstIndex - 1) = 0
   "we might be here under false premises or grow did the job for us"
	ifTrue: [ ^ self ].

  1 to: newLast do: [ :index | 
	self array at: index put: (self array at: index + delta).
	self array at: index + delta put: nil 
  ].
  self firstIndex: 1.
  self lastIndex: newLast
].


OrderedCollection ~> remove: oldObject ifAbsent: absentBlock
[
  | index |
  index := self firstIndex.
  [ index <= self lastIndex ]
	whileTrue: [ 
		oldObject = (self array at: index)
			ifTrue: [ 
				self removeIndex: index.
				^ oldObject ]
			ifFalse: [ index := index + 1 ] ].
  ^ absentBlock value
].

OrderedCollection ~> removeFirst
[
"Remove the first element of the receiver and answer it. If the receiver is 
 empty, create an error notification."
  | firstObject |
  self emptyCheck.
  firstObject := self array at: self firstIndex.
  self array at: self firstIndex put: nil.
  self firstIndex: self firstIndex + 1.
  ^ firstObject
].

OrderedCollection ~> removeIndex: removedIndex
[
  | index |
  index := removedIndex.
  [ index < self lastIndex ]
	whileTrue: [ 
		self array at: index 
			  put: (self array at: index + 1).
		index := index + 1 ].
  self array at: self lastIndex put: nil.
  self lastIndex: self lastIndex - 1
].

OrderedCollection ~> removeLast
[
"Remove the last element of the receiver and answer it. If the receiver is 
 empty, create an error notification."
  | lastObject |
  self emptyCheck.
  lastObject := self array at: self lastIndex.
  self array at: self lastIndex put: nil.
  self lastIndex: self lastIndex - 1.
  ^ lastObject
].

OrderedCollection ~> select: aBlock
[
"Evaluate aBlock with each of my elements as the argument.
 Collect into a new collection like the receiver, only those
 elements for which aBlock evaluates to true.
 Override the superclass in order to use add:, not at:put:."
  | newCollection |
  newCollection := self species new.
  self do: [ :each | 
	(aBlock value: each)
		ifTrue: [ newCollection add: each ] 
  ].
  ^ newCollection
].

OrderedCollection ~> setCollection: anArray
[
  self array: anArray.
  self firstIndex: (self array size // 3 max: 1).
  self lastIndex: self firstIndex - 1
].

OrderedCollection ~> size
[
  ^ self lastIndex - self firstIndex + 1
].


OrderedCollection class ~> new
[
  ^ self new: 8
].

OrderedCollection class ~> new: anInteger
[
"If a subclass adds fields, then that subclass must reimplement new:."
  ^ self basicNew setCollection: (Array new: anInteger)
].


"		E O F 		"
