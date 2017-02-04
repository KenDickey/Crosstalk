"FILE: 'ArrayedCollection.st'
 IMPLEMENTS: fill in class ArrayedCollection
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"

ArrayedCollection ~> add: newObject
[
  self shouldNotImplement
].

ArrayedCollection ~> mergeFirst: first middle: middle last: last into: dst by: aBlock
[
"Private! Merge the sorted ranges [first..middle] and [middle+1..last]
 of the receiver into the range [first..last] of dst."
  | i1 i2 val1 val2 out |
  i1 := first.
  i2 := middle + 1.
  val1 := self at: i1.
  val2 := self at: i2.
  out := first - 1.	"will be pre-incremented"
  "select 'lower' half of the elements based on comparator"
  [ i1 <= middle and: [ i2 <= last ] ]
	whileTrue: [ 
	  (aBlock value: val2 value: val1)
		ifTrue: [ 
			dst at: (out := out + 1) put: val2.
			i2 := i2 + 1.
			i2 <= last
				ifTrue: [ val2 := self at: i2 ]
		]
		ifFalse: [ 
			dst at: (out := out + 1) put: val1.
			val1 := self at: (i1 := i1 + 1) 
		] 
	].
  "copy the remaining elements"
  (i1 <= middle)
	ifTrue: [ 
		dst replaceFrom: out + 1
		    to: last
		    with: self
		    startingAt: i1 
	]
	ifFalse: [ 
		dst replaceFrom: out + 1
		    to: last
		    with: self
		    startingAt: i2 
	]
].

ArrayedCollection ~> mergeSortFrom: startIndex to: stopIndex by: aBlock
[
"Sort the given range of indices using the mergesort algorithm.
 Mergesort is a worst-case O(N log N) sorting algorithm that
 usually does only half as many comparisons as heapsort or quicksort."
"Details:
  recursively split the range to be sorted into two halves, 
  mergesort each half, 
  then merge the two halves together. 
An extra copy of the data is used as temporary storage
 and successive merge phases copy data back and forth
 between the receiver and this copy.
 The recursion is set up so that the final merge
 is performed into the receiver, resulting in the
 receiver being completely sorted."
  | temp |
  self size <= 1
	ifTrue: [ ^ self ].	"nothing to do"
  startIndex = stopIndex
	ifTrue: [ ^ self ].
  (startIndex >= 1 and: [ startIndex < stopIndex ])
	ifFalse: [ self error: 'bad start index' ].
  stopIndex <= self size
	ifFalse: [ self error: 'bad stop index' ].
  temp := self basicCopy.
  self 	mergeSortFrom: startIndex
	to: stopIndex
	src: temp
	dst: self
	by: aBlock
].

ArrayedCollection ~> mergeSortFrom: first to: last src: src dst: dst by: aBlock
[
"Private! Split the range to be sorted in half, sort each half, and merge the two half-ranges into dst."
  | middle |
  first = last
	ifTrue: [ ^ self ].
  middle := (first + last) // 2.
  self  mergeSortFrom: first
	to: middle
	src: dst
	dst: src
	by: aBlock.
  self	mergeSortFrom: middle + 1
	to: last
	src: dst
	dst: src
	by: aBlock.
  src	mergeFirst: first
	middle: middle
	last: last
	into: dst
	by: aBlock
].

ArrayedCollection ~> size
[
"Primitive.
 Answer the number of indexable fields in the receiver. 
This value is the same as the largest legal subscript. 
Specified here to override SequenceableCollection size.
 Essential. See Object documentation whatIsAPrimitive. "
  ^ self basicSize
].

ArrayedCollection ~> sort
[
"Sort this array into ascending order using the '<' operator."
  self mergeSortFrom: 1 to: self size by: [ :el1 :el2 | el1 < el2 ]
].

ArrayedCollection ~> sort: aBlock
[
"Sort this array using the given comparision block. The block should take two arguments and return true if the first element should precede the second in the sorted result."
  self mergeSortFrom: 1 to: self size by: aBlock
].

ArrayedCollection class ~> new
[
"Answer a new instance of me, with size = 0."
  ^ self species new: 0
].

ArrayedCollection class ~> with: anObject
[
"Answer a new instance of me, containing only anObject."
  | newCollection |
  newCollection := self species new: 1.
  newCollection at: 1 put: anObject.
  ^ newCollection
].

ArrayedCollection class ~> with: firstObject with: secondObject
[
"Answer a new instance of me containing the two arguments as elements."
  | newCollection |
  newCollection := self species new: 2.
  newCollection at: 1 put: firstObject.
  newCollection at: 2 put: secondObject.
  ^ newCollection
].

ArrayedCollection class ~> with: firstObject with: secondObject with: thirdObject
[
"Answer a new instance of me, containing the three arguments as elements."
  | newCollection |
  newCollection := self species new: 3.
  newCollection at: 1 put: firstObject.
  newCollection at: 2 put: secondObject.
  newCollection at: 3 put: thirdObject.
  ^ newCollection
].

ArrayedCollection ~> detect: aBlock ifNone: exceptionBlock
[
"Evaluate aBlock with each of the receiver's elements as the argument.
 Answer the first element for which aBlock evaluates to true. If none 
 evaluate to true, then evaluate the argument, exceptionBlock."
  self do: [ :each | 
		(aBlock value: each)
			ifTrue: [ ^ each ] ].
  ^ exceptionBlock value
].


ArrayedCollection ~> detect: aBlock 
[
"Evaluate aBlock with each of the receiver's elements as the argument. 
 Answer the first element for which aBlock evaluates to true."

  ^ self detect: aBlock ifNone: [self errorNotFound: aBlock]
].

ArrayedCollection ~> collect: aBlock
[
"Evaluate aBlock with each of the receiver's elements as the argument.
 Collect the resulting values into a collection like the receiver.
 Answer the new collection."
  | newCollection |
  newCollection := self species new: self size.
  1 to: self size do: [ :index |
        newCollection at: index put: (aBlock value: (self at: index)) 
  ].
  ^ newCollection
].

ArrayedCollection ~> includes: anObject
[
"Answer whether anObject is one of the receiver's elements."
  self do: [ :each | 
		anObject = each
			ifTrue: [ ^ true ] 
  ].
  ^ false
].



