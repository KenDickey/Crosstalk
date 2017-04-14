"FILE: 'Numbers.st'
 IMPLEMENTS: Augments Number and its subclasses
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence (note class comment)]
 Modifications by: Ken Dickey
 DATE: 03 Febuary 2017
"

Object ~> closeTo: anObject
["Answer whether the receiver and the argument represent the same
 object. If = is redefined in any subclass, consider also redefining the
 message hash."

  ^ [self = anObject] ifError: [:aString :aReceiver | ^ false ]
].

Number ~> closeTo: aNumber
["are these two numbers close?" 
 "NB: Scheme: (= 1 1.0) --> #true"

  aNumber isNumber ifFalse: [^super closeTo: aNumber].
  self = 0.0 ifTrue: [^aNumber abs < 0.0001].
  aNumber = 0 ifTrue: [^self abs < 0.0001].
  ^ (self = aNumber)
	or: [(self - aNumber) abs / (self abs max: aNumber abs) < 0.0001]
].

Number ~> isCloseTo: aNumber
[
  ^ self closeTo: aNumber
].



"		E O F 		"
