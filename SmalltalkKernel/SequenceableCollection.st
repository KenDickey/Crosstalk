"FILE: 'SequenceableCollection.st'
 IMPLEMENTS: fill in class SequenceableCollection
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"

SequenceableCollection ~> , otherCollection
[
"Concatenate two Strings or Collections."
  ^ self copyReplaceFrom: self size + 1 to: self size with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 printStringBase: 16) copyFrom: 4 to: 6) , ' boy!'
"
].

SequenceableCollection ~> = otherCollection
[
"Answer whether the species of the receiver is the same as
 otherCollection's species, and the receiver's size is the same as
 otherCollection's size, and each of the receiver's elements equal the
 corresponding element of otherCollection."
  | size |
  (size := self size) = otherCollection size
	ifFalse: [ ^ false ].
  self species == otherCollection species
	ifFalse: [ ^ false ].
  1 to: size do: [ :index | 
	(self at: index) = (otherCollection at: index)
		ifFalse: [ ^ false ]
  ].
  ^ true
].

SequenceableCollection ~> asArray
[
"Answer an Array whose elements are the elements of the receiver, in the same order."
  | newArray |
  newArray := Array new: self size.
  1 to: self size do: [ :index | newArray at: index put: (self at: index) ].
  ^ newArray
].

SequenceableCollection ~> at: index ifAbsent: absentBlock
[
"Answer the element at the given index.
 If I do not contain an element at that index,
 answer the result of evaluating the argument, absentBlock."
  (index between: 1 and: self size)
	ifTrue: [ ^ self at: index ].
  ^ absentBlock value
].

SequenceableCollection ~> collect: aBlock
[
"Refer to the comment in Collection>>collect:."
  | result |
  result := self species new: self size.
  1 to: self size do: [ :index | 
	result at: index put: (aBlock value: (self at: index)) 
  ].
  ^ result
].

SequenceableCollection ~> copyFrom: start to: stop
[
"Answer a copy of a subset of the receiver, starting from element at 
index start until element at index stop."
  | newSize |
  newSize := stop - start + 1.
  ^ (self species new: newSize)
	  replaceFrom: 1
	  to: newSize
	  with: self
	  startingAt: start
].

SequenceableCollection ~> copyReplaceFrom: start to: stop with: replacementCollection
[
"Answer a copy of the receiver satisfying the following conditions: If 
 stop is less than start, then this is an insertion; stop should be exactly 
 start-1, start = 1 means insert before the first character, start = size+1 
 means append after last character. Otherwise, this is a replacement; start 
 and stop have to be within the receiver's bounds."
  | newSequenceableCollection newSize endReplacement |
  newSize := self size - (stop - start + 1) + replacementCollection size.
  endReplacement := start - 1 + replacementCollection size.
  newSequenceableCollection := self species new: newSize.
  newSequenceableCollection
	replaceFrom: 1
	to: start - 1
	with: self
	startingAt: 1.
  newSequenceableCollection
	replaceFrom: start
	to: endReplacement
	with: replacementCollection
	startingAt: 1.
  newSequenceableCollection
	replaceFrom: endReplacement + 1
	to: newSize
	with: self
	startingAt: stop + 1.
  ^ newSequenceableCollection
].

SequenceableCollection ~> copyWith: newElement
[
"Answer a copy of the receiver that is 1 bigger than the receiver and has 
 newElement at the last element."
  | newIC |
  newIC := self species new: self size + 1.
  newIC replaceFrom: 1
	to: self size
	with: self
	startingAt: 1.
  newIC at: newIC size put: newElement.
  ^ newIC
].

SequenceableCollection ~> do: aBlock
[
"Refer to the comment in Collection|do:."
  1 to: self size do: [ :index | aBlock value: (self at: index) ]
].

SequenceableCollection ~> first
[
"Answer the first element of the receiver. Create an error notification if 
 the receiver contains no elements."
  self emptyCheck.
  ^ self at: 1
].

SequenceableCollection ~> indexOf: anObject ifAbsent: absentBlock
[
"Answer the index of the given object within me.
 If I do not contain the given object,
 answer the result of evaluating the given block."
  ^ self indexOf: anObject startingAt: 1 ifAbsent: absentBlock
].

SequenceableCollection ~> indexOf: anObject startingAt: startIndex ifAbsent: absentBlock
[
"Answer the index of the given object within me starting the
 search at the given index.
 If I do not contain the given object,
 answer the result of evaluating the given block."
  startIndex to: self size do: [ :i | 
	(self at: i) = anObject
		ifTrue: [ ^ i ]
  ].
  ^ absentBlock value
].

SequenceableCollection ~> last
[
"Answer the last element of the receiver.
 Create an error notification if the receiver contains no elements."
  self emptyCheck.
  ^ self at: self size
].

SequenceableCollection ~> remove: oldObject ifAbsent: anExceptionBlock
[
"SequencableCollections cannot implement removing."
  self shouldNotImplement
].

SequenceableCollection ~> replaceFrom: start to: stop with: replacement
[
"This destructively replaces elements from start to stop in the receiver. 
 Answer the receiver itself. Use copyReplaceFrom:to:with: for 
 insertion/deletion which may alter the size of the result."
  replacement size = (stop - start + 1)
	ifFalse: [ self error: 'Size of replacement doesnt match' ].
  ^ self
	replaceFrom: start
	to: stop
	with: replacement
	startingAt: 1
].

SequenceableCollection ~> 
replaceFrom: start to: stop with: replacement startingAt: repStart
[
"This destructively replaces elements from start to stop in the receiver 
 starting at index, repStart, in the sequenceable collection, 
 replacementCollection.
 Answer the receiver.
 No range checks are performed."
  | index repOff |
  repOff := repStart - start.
  index := start - 1.
  [ (index := index + 1) <= stop ]
     whileTrue: [ self at: index  put: (replacement at: repOff + index) ]
].

SequenceableCollection ~> select: aBlock
[
"Refer to the comment in Collection|select:."
  | s |
  s := WriteStream on: (self species new: self size).
  1 to: self size do: [ :i | 
	(aBlock value: (self at: i))
		ifTrue: [ s nextPut: (self at: i) ] 
  ].
  ^ s contents
].

SequenceableCollection ~> size
[
  self subclassResponsibility
].

SequenceableCollection ~> swap: oneIndex with: anotherIndex
[
"Move the element at oneIndex to anotherIndex, and vice-versa."
  | element |
  element := self at: oneIndex.
  self at: oneIndex put: (self at: anotherIndex).
  self at: anotherIndex put: element
].


"		E O F 		"
