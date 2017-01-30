"FILE: 'Interval.st'
 IMPLEMENTS: class Interval
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 21 January 2017
"

SequenceableCollection newSubclassName: #Interval iVars: #(start stop step) cVars: nil.

Interval category: #'Collections-Ordered'.

Interval comment: 'I represent a finite arithmetic progression.'.

Interval ~> = anInterval
[
"Answer true if my species and anInterval species are equal, and
if our starts, steps and sizes are equal."
  (self species == anInterval species)
  ifTrue: [ ^ (self start = anInterval first)
 	and: [ (self step = anInterval increment)
		and: [ self size = anInterval size ] ] 
  ]
  ifFalse: [ ^ false ]
].

Interval ~> add: newObject
[
"Adding to an Interval is not allowed."
  self shouldNotImplement
].

Interval ~> at: anInteger
[
"Answer the anInteger'th element."
  (anInteger >= 1 and: [ anInteger <= self size ])
	ifTrue: [ ^ self start + (self step * (anInteger - 1)) ]
	ifFalse: [ self errorSubscriptBounds: anInteger ]
].

Interval ~> at: anInteger put: anObject
[
  "Storing into an Interval is not allowed."
  self error: 'you can not store into an interval'
].

Interval ~> collect: aBlock
[
  | nextValue result |
  result := self species new: self size.
  nextValue := self start.
  1 to: result size do: [ :i | 
	result at: i put: (aBlock value: nextValue).
	nextValue := nextValue + self step 
  ].

  ^ result
].

Interval ~> do: aBlock
[
  | aValue |
  aValue := self start.
  (self step < 0)
	ifTrue: [ 
		[ self stop <= aValue ]
		whileTrue: [ 
			aBlock value: aValue.
			aValue := aValue + self step 
		] 
	]
	ifFalse: [ 
		[ self stop >= aValue ]
		whileTrue: [ 
			aBlock value: aValue.
			aValue := aValue + self step 
		] 
	].
	self
].

Interval ~> first
[
"Refer to the comment in SequenceableCollection|first."
  ^ self start
].

Interval ~> hash
[
"Hash is reimplemented because = is implemented."
  ^ (((self start hash bitShift: 2) bitOr: self stop hash) bitShift: 1) bitOr: self size
].

Interval ~> includes: aNumber
[
  ^ aNumber between: self first and: self last
].

Interval ~> increment
[
"Answer the receiver's interval increment."
  ^ self step
].

Interval ~> last
[
"Refer to the comment in SequenceableCollection|last."
  ^ self stop - ((self stop - self start) \\ self step)
].

Interval ~> printOn: aStream
[
  aStream nextPut: $(.
  aStream nextPutAll: self class name asString.
  aStream nextPutAll: ' from: '.
  self start printOn: aStream.
  aStream nextPutAll: ' to: '.
  self stop printOn: aStream.
  (self step ~= 1)
	ifTrue: [ 
		aStream nextPutAll: ' by: '.
		self step printOn: aStream 
	].
  aStream nextPut: $)
].

Interval ~> remove: newObject
[
"Removing from an Interval is not allowed."
  self error: 'elements cannot be removed from an Interval'
].

Interval ~> setFrom: startInteger to: stopInteger by: stepInteger
[
  self start: startInteger.
  self stop:  stopInteger.
  self step:  stepInteger
].

Interval ~> size
[
  self step < 0
    ifTrue: [ 
	self start < self stop
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ (self stop - self start) // self step + 1 ] ]
    ifFalse: [ 
	self stop < self start
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ (self stop - self start) // self step + 1 ] ]
].

Interval ~> species
[
  ^ Array
].

Interval class ~> from: startInteger to: stopInteger
[
"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of 1."
  ^ self basicNew setFrom: startInteger to: stopInteger by: 1
].

Interval class ~> from: startInteger to: stopInteger by: stepInteger
[
"Answer an instance of me,
 starting at startNumber,\
 ending at stopNumber, and
 with an interval increment of stepNumber."
  ^ self basicNew setFrom: startInteger to: stopInteger by: stepInteger
].

Interval class ~> from: startInteger to: stopInteger count: count
[
 | step |
  step := (stop - start) / (count - 1).
  ^ self basicNew setFrom: startInteger to: stopInteger by: step
].


"		E O F 		"
