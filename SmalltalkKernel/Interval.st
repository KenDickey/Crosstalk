"FILE: 'Interval.st'
 IMPLEMENTS: class Interval
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 21 January 2017
"

SequenceableCollection newSubclassName: #Interval 
	iVars: #(start stop step) 
	cVars: nil.

Interval category: #'Collections-Ordered'.

Interval comment: 'I represent a finite arithmetic progression.'.

Interval ~> is: aSymbol
[
  ^aSymbol == #Interval or: [ super is: aSymbol ]
].

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

Interval ~> last
[
"Refer to the comment in SequenceableCollection|last."
  ^ self stop
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
  ^ self stop
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
		ifFalse: [ ^ ((self stop - self start) / self step + 1) rounded ] ]
    ifFalse: [ 
	self stop < self start
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ ((self stop - self start) / self step + 1) rounded ] ]
].

Interval ~> species
[
  ^ Array
].

Interval ~> + number
[
  ^ self start + number to: self stop + number count: self count
].

Interval ~> - number
[
  ^ self start - number to: self stop - number count: self count
].

Interval ~> reverseDo: aBlock 
["Evaluate aBlock for each element of my interval, in reverse order."
  seld count to: 1 by: -1 do: [ :i | aBlock value: (self at: i) ]
].

" -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Interval class ~> from: startInteger to: stopInteger
[
"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of 1."
  ^ self basicNew setFrom: startInteger to: stopInteger by: 1
].

Interval class ~> from: start to: stop by: step
[
"Answer an instance of me,
 starting at startNumber,
 ending at stopNumber, and
 with an interval increment of stepNumber."
  | count end |
  count := stop - start / step + 1.
  (count closeTo: count rounded)
	ifTrue: [
		count := count rounded.
		end := stop 
	]
	ifFalse: [
		count := count truncated.
		end := count - 1 * step + start 
	].
  ^self from: start to: end count: count
].

Interval class ~> from: start to: stop count: count
[
 | step |
  step := (stop - start) / (count - 1).
  ^ self basicNew setFrom: start to: stop by: step
].

Interval class ~> newFrom: aCollection 
[ "Answer an instance of me containing the same elements as aCollection."
"	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval    (an error)
	(-4 to: -12 by: -1) as: Interval
"
    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self 
			from: aCollection first 
			to: aCollection last 
			by: (aCollection last - aCollection first) // (n - 1).
	aCollection ~= newInterval
		ifTrue: [self error: 'The argument is not an arithmetic progression'].
	^newInterval
].


"		E O F 		"
