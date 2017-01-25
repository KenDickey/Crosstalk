"FILE: 'LinkedList.st'
 IMPLEMENTS: class LinkedList, a doubly linked list.
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 21 January 2017
"

SequenceableCollection newSubclassName: #LinkedList iVars: #(firstLink lastLink) cVars: nil.

LinkedList category: #'Collections-Ordered'.

LinkedList comment: 
'I represent a collection of links, which are containers for other objects.
 Using the message sequence addFirst:/removeLast causes the receiver to behave as a stack;
 using addLast:/removeFirst causes the receiver to behave as a queue.'.

LinkedList ~> add: aLinkOrObject
[
"Add aLink to the end of the receiver's list. Answer aLink."
  ^ self addLast: aLinkOrObject
].

LinkedList ~> add: link after: otherLinkOrObject
[
"Add otherLink  after link in the list. Answer aLink."
  | otherLink savedLink |
  otherLink := self linkAt: (self indexOf: otherLinkOrObject).
  ^ self add: link afterLink: otherLink
].

LinkedList ~> add: aLinkOrObject afterLink: otherLink
[
"Add otherLink  after link in the list. Answer aLink."
  | savedLink aLink |
  self lastLink == otherLink
	ifTrue: [ ^ self addLast: aLinkOrObject ].
  savedLink := otherLink nextLink.
  aLink := aLinkOrObject asLink.
  otherLink nextLink: aLink.
  aLink nextLink: savedLink.
  ^ aLink
].

LinkedList ~> add: link before: otherLinkOrObject
[
"Add otherLink  after link in the list. Answer aLink."
  | otherLink savedLink |
  otherLink := self linkAt: (self indexOf: otherLinkOrObject).
  ^ self add: link beforeLink: otherLink
].

LinkedList ~> add: aLinkOrObject beforeLink: otherLink
[
  | currentLink |
  self firstLink == otherLink
	ifTrue: [ ^ self addFirst: aLinkOrObject ].
  currentLink := self firstLink.
  [ currentLink == nil ]
  whileFalse: [ 
	currentLink nextLink == otherLink
	  ifTrue: [ 
		| aLink |
		aLink := aLinkOrObject asLink.
		aLink nextLink: currentLink nextLink.
		currentLink nextLink: aLink.
		^ aLink 
	  ].
	currentLink := currentLink nextLink
  ].
  ^ self errorNotFound: otherLink
].

LinkedList ~> addFirst: aLinkOrObject
[
"Add aLink to the beginning of the receiver's list. Answer aLink."
  | aLink |
  aLink := aLinkOrObject asLink.
  self isEmpty
	ifTrue: [ self lastLink: aLink ].
  aLink nextLink: self firstLink.
  self firstLink: aLink.
  ^ aLink
].

LinkedList ~> addLast: aLinkOrObject
[
"Add aLink to the end of the receiver's list. Answer aLink."
  | aLink |
  aLink := aLinkOrObject asLink.
  self isEmpty
	ifTrue:  [ self firstLink: aLink ]
	ifFalse: [ self lastLink nextLink: aLink ].
  self lastLink: aLink.
  ^ aLink
].

LinkedList ~> at: index
[
  ^ (self linkAt: index) value
].

LinkedList ~> at: index put: anObject
[
  ^ self at: index putLink: (self linkOf: anObject ifAbsent: [ anObject asLink ])
].

LinkedList ~> at: index putLink: aLink
[
  | previousLink nextLink |
"Please don't put a link which is already in the list, or you will create an infinite loop"
  (self validIndex: index)
	ifTrue: [ ^ self errorOutOfBounds ].
  index = 1
	ifTrue: [ 
		aLink nextLink: self self firstLink nextLink.
		self firstLink: aLink.
		aLink nextLink ifNil: [ self lastLink: aLink ].
		^ aLink ].
  previousLink := self linkAt: index - 1.
  nextLink := previousLink nextLink nextLink.
  nextLink ifNil: [ aLink nextLink: self lastLink ] ifNotNil: [ aLink nextLink: nextLink ].
  previousLink nextLink: aLink.
  nextLink
	ifNil: [ 
		self lastLink: aLink.
		aLink nextLink: nil ].
  ^ aLink
].

LinkedList ~> collect: aBlock
[
"Evaluate aBlock with each of the receiver's elements as the argument.  
 Collect the resulting values into a collection like the receiver. Answer  
 the new collection."
  | aLink newCollection |
  newCollection := self class new.
  aLink := self firstLink.
  [ aLink == nil ]
	whileFalse: [ 
		newCollection add: (aBlock value: aLink value).
		aLink := aLink nextLink ].
  ^ newCollection
].

LinkedList ~> copyWith: newElement
[
  ^ self copy
      	add: newElement;
	yourself
].

LinkedList ~> copyWithout: oldElement
[
  | newInst |
  newInst := self class new.
  self
	do: [ :each | 
		each = oldElement
			ifFalse: [ newInst add: each ] ].
  ^ newInst
].

LinkedList ~> do: aBlock
[
  | aLink |
  aLink := self firstLink.
  [ aLink == nil ]
	whileFalse: [ 
		aBlock value: aLink value.
		aLink := aLink nextLink 
	]
].

LinkedList ~> first
[
"Answer the first link. Create an error notification if the receiver is  empty."
  ^ self firstLink value
].

LinkedList ~> firstLink
[
"Answer the first link. Create an error notification if the receiver is empty."
  self emptyCheck.
  ^ self firstLink
].

LinkedList ~> indexOf: anElement startingAt: start ifAbsent: exceptionBlock
[
"Answer the index of the first occurence of anElement after start
 within the receiver. If the receiver does not contain anElement, 
 answer the result of evaluating the argument, exceptionBlock."
  | currentLink index |
  currentLink := self linkAt: start ifAbsent: [ nil ].
  index := start.
  [ currentLink isNil ]
	whileFalse: [ 
		currentLink value = anElement value
			ifTrue: [ ^ index ].
		currentLink := currentLink nextLink.
		index := index + 1 ].
  ^ exceptionBlock value
].

LinkedList ~> isEmpty
[
  ^ self firstLink isNil
].

LinkedList ~> last
[
"Answer the last link. Create an error notification if the receiver is empty."
  ^ self lastLink value
].

LinkedList ~> lastLink
[
"Answer the last link. Create an error notification if the receiver is empty."
  self emptyCheck.
  ^ self lastLink
].

LinkedList ~> linkAt: index
[
  ^ self linkAt: index ifAbsent: [ self errorSubscriptBounds: index ]
].

LinkedList ~> linkAt: index ifAbsent: errorBlock
[
  | i |
  i := 0.
  self
	linksDo: [ :link | 
		(i := i + 1) = index
			ifTrue: [ ^ link ] ].
  ^ errorBlock value
].

LinkedList ~> linkOf: anObject
[
  ^ self linkOf: anObject ifAbsent: [ self error: 'No such element' ]
].

LinkedList ~> linkOf: anObject ifAbsent: errorBlock
[
  self
	linksDo: [ :el | 
		el value = anObject
			ifTrue: [ ^ el ] ].
  ^ errorBlock value
].

LinkedList ~> linksDo: aBlock
[
| aLink |
  aLink := self firstLink.
  [ aLink == nil ]
	whileFalse: [ 
		aBlock value: aLink.
		aLink := aLink nextLink ]
].

LinkedList ~> postCopy
[
  | aLink |
  super postCopy.
  self firstLink isNil
	ifFalse: [ 
		aLink := self firstLink: self firstLink copy.
		[ aLink nextLink isNil ] whileFalse: [ aLink nextLink: (aLink := aLink nextLink copy) ].
		self lastLink: aLink ].
].

LinkedList ~> remove: aLinkOrObject ifAbsent: aBlock
[
"Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock."
  | link |
  link := self linkOf: aLinkOrObject ifAbsent: [ ^ aBlock value ].
  self removeLink: link ifAbsent: [ ^ aBlock value ].
  ^ aLinkOrObject
].

LinkedList ~> removeAll
[
"Implementation note: this has to be fast"
  self firstLink: (self lastLink: nil)
].

LinkedList ~> removeAllSuchThat: aBlock
[
"Evaluate aBlock for each element and remove all that elements from
  the receiver for that aBlock evaluates to true. 
 For LinkedLists, it's safe to use do:."
  self
	do: [ :each | 
		(aBlock value: each)
			ifTrue: [ self remove: each ] ]
].

LinkedList ~> removeFirst
[
"Remove the first element and answer it. If the receiver is empty, create 
 an error notification."
  | oldLink |
  self emptyCheck.
  oldLink := self firstLink.
  self firstLink == self lastLink
	ifTrue: [ 
		self firstLink: nil.
		self lastLink: nil 
	]
	ifFalse: [ self firstLink: oldLink nextLink ].
  oldLink nextLink: nil.
  ^ oldLink value
].

LinkedList ~> removeLast
[
"Remove the receiver's last element and answer it. If the receiver is 
 empty, create an error notification."
  | oldLink aLink |
  self emptyCheck.
  oldLink := self lastLink.
  self firstLink == self lastLink
	ifTrue: [ 
		self firstLink: nil.
		self lastLink: nil 
	]
	ifFalse: [ 
		aLink := self firstLink.
		[ aLink nextLink == oldLink ] whileFalse: [ aLink := aLink nextLink ].
		aLink nextLink: nil.
		self lastLink: aLink 
	].
  oldLink nextLink: nil.
  ^ oldLink value
].

LinkedList ~> removeLink: aLink
[
  ^ self removeLink: aLink ifAbsent: [ self error: 'no such method!' ]
].

LinkedList ~> removeLink: aLink ifAbsent: aBlock
[
"Remove aLink from the receiver. If it is not there, answer the result of
 evaluating aBlock."
  | tempLink |
  aLink == self firstLink
	ifTrue: [ 
		self firstLink: aLink nextLink.
		aLink == self lastLink
			ifTrue: [ self lastLink: nil ] 
	]
	ifFalse: [ 
		tempLink := self firstLink.
		[ 
		    tempLink == nil
			ifTrue: [ ^ aBlock value ].
		tempLink nextLink == aLink ] whileFalse: [ tempLink := tempLink nextLink ].
		tempLink nextLink: aLink nextLink.
		aLink == self lastLink
			ifTrue: [ self lastLink: tempLink ]
		"Not nilling the link enables us to delete while iterating"	
		"aLink nextLink: nil."
	].
  ^ aLink
].

LinkedList ~> species
[
  ^ Array
].

LinkedList ~> swap: ix1 with: ix2
[
"Reimplemented, super would create an infinite loop"
  | minIx maxIx link1Prev link2Prev link1 link2 link1Next link2Next newLink2Next |
  ((self validIndex: ix1) and: [ self validIndex: ix2 ])
	ifFalse: [ ^ self errorOutOfBounds ].	"Get edge case out of the way"
  ix1 = ix2
	ifTrue: [ ^ self ].	"Sort indexes to make boundary-checks easier"
  minIx := ix1 min: ix2.
  maxIx := ix2 max: ix1.
  link1Prev := minIx = 1
	ifFalse: [ self linkAt: minIx - 1 ].
  link1 := link1Prev ifNotNil: [ link1Prev nextLink ] ifNil: [ self linkAt: minIx ].
  link1Next := link1 nextLink.
  link2Prev := self linkAt: maxIx - 1.
  link2 := link2Prev nextLink.
  link2Next := link2 nextLink.	"Link at start being swapped"
  link1 = self firstLink
	ifTrue: [ self firstLink: link2 ]
	ifFalse: [ link1Prev nextLink: link2 ].	"Link at end being swapped"
  link2 = self lastLink
	ifTrue: [ self lastLink: link1 ]
	ifFalse: [  ].	"Links  being swapped adjacent"
  newLink2Next := link1 nextLink = link2
	ifTrue: [ link1 ]
	ifFalse: [ 
		link2Prev nextLink: link1.
		link1Next ].
  link1 nextLink: link2Next.
  link2 nextLink: newLink2Next
].

LinkedList ~> validIndex: index
[
  ^ index > 0 and: [ index <= self size ]
].

LinkedList class ~> new: anInt
[
"LinkedList don't need capacity"
  ^ self new
].

LinkedList class ~> newFrom: aCollection
[
"Answer an instance with same elements as aCollection."
  ^ self new
	addAll: aCollection;
	yourself
].


"		E O F 		"
