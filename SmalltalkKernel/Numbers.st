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

Number ~> truncateTo: aNumber 
["Answer the next multiple of aNumber toward zero that is nearest the receiver."
 "Please use this method when you actually want a numeric result.
 If what you need is a string representation with certain precision,
 consider using #printOn:fractionDigits: or some other method in the 'printing' category."
 "Examples:
	3.1479 truncateTo: 0.01 -> 3.14
	3.1479 truncateTo: 0.1 -> 3.1
	1923 truncateTo: 10 -> 1920
	3.1479 truncateTo: 0.005 -> 3.145
	-3.1479 truncateTo: 0.01 -> -3.14"

	^ (self quo: aNumber) * aNumber
].

Number ~> quo: aNumber
["Integer division with truncation toward zero.
 (-9 quo: 4) = -2
 (-0.9 quo: 0.4) = -2
 #rem: answers the remainder from this division.
 See #//, #quo:, #div:"

  ^ (self / aNumber) truncated
].

Number ~> rem:  divisor
["Modulo operation. Remainder of the integer division #quo: (division truncated to zero).
 Answer a Number with the same  sign as dividend (i.e.,  self).
 (9 rem: 4) = 1.
 (-9 rem: 4) = -1.
 (0.9 rem: 0.4) = 0.1.
See http://en.wikipedia.org/wiki/Modulo_operation
See #\\, #rem: and #mod:
See detailed discussion at http://mathforum.org/library/drmath/view/52343.html "

  ^self - ((self quo: divisor) * divisor)
].

Number ~> withNegativeSign
["Answer a number with same magnitude than receiver and negative sign."
  ^self abs negated
].

Number ~> negated
["Answer a Number that is the negation of the receiver."

 ^ (0 - self)
].


"		E O F 		"
