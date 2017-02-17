"FILE: 'SortedCollection.st'
 IMPLEMENTS: SortedCollection class
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 17 February 2017
"

OrderedCollection newSubclassName: #SortedCollection iVars: #(sortBlock) cVars: nil.


SortedCollection category: #'Collections-Sequenceable'.

SortedCollection ~> = aSortedCollection
[
"Answer true if my and aSortedCollection's
 species are the same,
 and if our blocks are the same,
 and if our elements are the same."

  self species = aSortedCollection species
	ifFalse: [ ^ false ].
  sortBlock = aSortedCollection sortBlock
	ifTrue: [ ^ super = aSortedCollection ]
	ifFalse: [ ^ false ]
].

SortedCollection ~> add: newObject
[
  ^ super insert: newObject before: (self indexForInserting: newObject)
].

SortedCollection ~> addAll: aCollection
[
  aCollection size > (self size // 3)
	ifTrue: [
		aCollection do: [ :each | self addLast: each ].
		self reSort 
	]
	ifFalse: [
		aCollection do: [ :each | self add: each ] 
	].
	^ aCollection
].

SortedCollection ~> addFirst: newObject
[
  self shouldNotImplement
].

SortedCollection ~> at: anInteger put: anObject
[
  self shouldNotImplement
].

SortedCollection ~> collect: aBlock
[
"Evaluate aBlock with each of my elements as the argument.
 Collect the resulting values into an OrderedCollection.
 Answer the new collection. 
 Override the superclass in order to produce an OrderedCollection
 instead of a SortedCollection."

  | newCollection |
  newCollection := OrderedCollection new: self size.
  self do: [ :each | newCollection addLast: (aBlock value: each) ].
  ^ newCollection
].

SortedCollection ~> copyEmpty
[
"Answer a copy of the receiver without any of the receiver's elements."

  ^ self species sortBlock: sortBlock
].

SortedCollection ~> defaultSort: i to: j
[
"Sort elements i through j of self to be nondescending
 according to sortBlock."

"Assume the default sort block ([:x :y | x <= y])."

  | di dij dj tt ij k l n |
  "The prefix d means the data at that index."
  (n := j + 1 - i) <= 1
	ifTrue: [ ^ self ].	"Nothing to sort."	"Sort di,dj."
  di := array at: i.
  dj := array at: j.
  (di <= dj) ifFalse: [
		array swap: i with: j.
		tt := di.
		di := dj.
		dj := tt 	"i.e., should di precede dj?"
  ].

  (n > 2) ifTrue: [ 
	"More than two elements."
	ij := (i + j) // 2.	"ij is the midpoint of i and j."
	dij := array at: ij.	"Sort di,dij,dj.  Make dij be their median."
	(di <= dij) ifTrue: [ 
			(dij <= dj) ifFalse: [ 
				"i.e., should dij precede dj?" 
				array swap: j with: ij.
				   dij := dj 	
			]
	]
	ifFalse: [ "i.e. di should come after dij"
		array swap: i with: ij.
		dij := di 	"i.e. should di precede dij?"
	].
  	(n > 3) ifTrue: [ 
		"More than three elements."
		"Find k>i and l<j such that dk,dij,dl are in reverse order.
		Swap k and l.  Repeat this procedure until k and l pass each other."
		k := i.
		l := j.
		[ [ l := l - 1.
		    k <= l and: [ dij <= (array at: l) ] ] whileTrue.
			"i.e. while dl succeeds dij"
		[ k := k + 1.
		  k <= l and: [ (array at: k) <= dij ] ] whileTrue.	"i.e. while dij succeeds dk"
		  k <= l ]
			whileTrue: [ array swap: k with: l ].
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
		self defaultSort: i to: l.
		self defaultSort: k to: j 
	] 
  ]
].

SortedCollection ~> flatCollect: aBlock
[
  ^ self flatCollect: aBlock as: OrderedCollection
].

SortedCollection ~> indexForInserting: newObject
[
  | index low high array sortBlock | 
  low   := self firstIndex.
  high  := self lastIndex.
  array := self array.
  sortBlock := self sortBlock.
  self sortBlock
	ifNil: [ [ index := (high + low) // 2.
		   low > high ]
		  whileFalse: [ (array at: index) <= newObject
				ifTrue:  [ low  := index + 1 ]
				ifFalse: [ high := index - 1 ] 
		  ] 
	]
	ifNotNil: [ [ index := (high + low) // 2.
		      low > high ]
		    whileFalse: [ (sortBlock value: (array at: index) value: newObject)
				ifTrue:  [ low  := index + 1 ]
				ifFalse: [ high := index - 1 ] 
		    ] 
	].
  ^ low
].

SortedCollection ~> insert: anObject before: spot
[
  self shouldNotImplement
].

SortedCollection ~> median
[
"Return the middle element, or as close as we can get."

  ^ self at: (self size + 1) // 2
].

SortedCollection ~> reSort
[
  self sort: firstIndex to: lastIndex
].

SortedCollection ~> sort: aSortBlock
[
"Sort this array using aSortBlock. The block should take two arguments
 and return true if the first element should preceed the second one."

  super sort: aSortBlock.
  self sortBlock: aSortBlock
].

SortedCollection ~> sort: i to: j
[
"Sort elements i through j of self to be nondescending according to
 sortBlock."

  | di dij dj tt ij k l n array sortBlock |
  array := self array.
  sortBlock := self sortBlock.

  sortBlock ifNil: [ ^ self defaultSort: i to: j ]. 
  "The prefix d means the data at that index."
  ((n := j + 1 - i) <= 1) ifTrue: [ ^ self ].  "Nothing to sort."

  "Sort di,dj."
  di := array at: i.
  dj := array at: j.
  (sortBlock value: di value: dj) "i.e., should di precede dj?"
	ifFalse: [ 
		array swap: i with: j.
		tt := di.
		di := dj.
		dj := tt 
	].
  (n > 2) ifTrue: [ 
	"More than two elements."
	ij := (i + j) // 2.	"ij is the midpoint of i and j."
	dij := array at: ij.	"Sort di,dij,dj.  Make dij be their median."
	(sortBlock value: di value: dij)
		ifTrue: [ (sortBlock value: dij value: dj)
		ifFalse: [ array swap: j with: ij.
	  		   dij := dj 
		]	
		"i.e., should dij precede dj?" 
	]
	ifFalse: [ "i.e. di should come after dij"
		array swap: i with: ij.
		dij := di
	].	"i.e. should di precede dij?"
	(n > 3) ifTrue: [ 
		"More than three elements."
		"Find k>i and l<j such that dk,dij,dl are in reverse order.
		Swap k and l.  Repeat this procedure until k and l pass each other."
		k := i.
		l := j.
		[ [ l := l - 1.
		    k <= l and: [ sortBlock value: dij value: (array at: l) ] ] whileTrue.
		"i.e. while dl succeeds dij"
		[ k := k + 1.
		  k <= l and: [ sortBlock value: (array at: k) value: dij ] ] whileTrue.
		"i.e. while dij succeeds dk"
		  k <= l 
		] whileTrue: [ array swap: k with: l ].	
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
	self sort: i to: l.
	self sort: k to: j 
	] 
  ]
].

SortedCollection class ~> sortBlock: aBlock
[
"Answer an instance of me such that its elements are sorted according to 
 the criterion specified in aBlock."

  ^ (super new: 10) sortBlock: aBlock
].

"		E O F 		"
