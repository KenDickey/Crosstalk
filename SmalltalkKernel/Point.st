"FILE: 'Point.st'
 IMPLEMENTS: Collection (abstract) protocol
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 15 January 2017
"
"
Point
	superclass: #Object;
	instanceSpecification: #(#pointers #words );
	instanceVariables: #(#x #y );
	classVariables: #();
	package: #'PharoCandle-Kernel-Optional'.
"

Object newSubclassName: #Point iVars: #(#x #y) cVars: nil.


Point ~> * arg
[
"Answer a Point that is the product of the receiver and arg."
  arg isPoint
	ifTrue: [ ^ (self x * arg x) @ (self y * arg y) ].
  ^ arg adaptToPoint: self andSend: #*
]

Point ~> + arg
[
"Answer a Point that is the sum of the receiver and arg."
  arg isPoint
	ifTrue: [ ^ (self x + arg x) @ (self y + arg y) ].
  ^ arg adaptToPoint: self andSend: #+
]

Point ~> - arg
[
"Answer a Point that is the difference of the receiver and arg."
  arg isPoint
	ifTrue: [ ^ (self x - arg x) @ (self y - arg y) ].
  ^ arg adaptToPoint: self andSend: #-
]

Point ~> / arg
[
"Answer a Point that is the quotient of the receiver and arg."
  arg isPoint
	ifTrue: [ ^ (self x / arg x) @ (self y / arg y) ].
  ^ arg adaptToPoint: self andSend: #/
]

Point ~> // arg
[
"Answer a Point that is the quotient of the receiver and arg."
  arg isPoint
	ifTrue: [ ^ (self x // arg x) @ (self y // arg y) ].
  ^ arg adaptToPoint: self andSend: #//
]

Point ~> = aPoint
[
  self species = aPoint species
	ifTrue: [ ^ x = aPoint x and: [ y = aPoint y ]	"Refer to the comment in Object|=." ]
	ifFalse: [ ^ false ]
]

Point ~> abs
[
"Answer a Point whose x and y are the absolute values of the receiver's x and y."
  ^ x abs @ y abs
]

Point ~> adaptToFloat: rcvr andSend: selector
[
"If I am involved in arithmetic with a Float, convert it to a Point."
  ^ rcvr @ rcvr perform: selector with: self
]

Point ~> adaptToInteger: rcvr andSend: selector
[
"If I am involved in arithmetic with an Integer, convert it to a Point."
  ^ rcvr @ rcvr perform: selector with: self
]

Point ~> asPoint
[
"Answer the receiver itself."
  ^ self
]

Point ~> crossProduct: aPoint
[
"Answer a number that is the cross product of the receiver and the argument, aPoint."
  ^ self x * aPoint y - (self y * aPoint x)
]

Point ~> degrees
[
"Answer the angle the receiver makes with origin in degrees. Right is 0; down is 90."
  | tan theta |
  self x = 0
	ifTrue: [ 
		self y >= 0
		  ifTrue: [ ^ 90.0 ]
		  ifFalse: [ ^ 270.0 ] ]
	ifFalse: [ 
		tan := self y asFloat / self x asFloat.
		theta := tan arcTan.
		self x >= 0
		  ifTrue: [ 
			self y >= 0
			  ifTrue: [ ^ theta radiansToDegrees ]
			  ifFalse: [ ^ 360.0 + theta radiansToDegrees ] ]
		  ifFalse: [ ^ 180.0 + theta radiansToDegrees ] ]
]

Point ~> dist: aPoint
[
"Answer the distance between aPoint and the receiver."
 ^ (aPoint - self) r
]

Point ~> dotProduct: aPoint
[
"Answer a number that is the dot product of the receiver and the argument."
  ^ self x * aPoint x + (self y * aPoint y)
]

Point ~> hash
[
"Hash is reimplemented because = is implemented."
^ (self x hash bitShift: 2) bitXor: self y hash
]

Point ~> max: aPoint
[
"Answer a new Point whose x and y are the maximum 
 of the receiver and the argument point x and y."
^ (self x max: aPoint x) @ (self y max: aPoint y)
]

Point ~> min: aPoint
[
"Answer a new Point whose x and y are the minimum
 of the receiver's and the argument point's x and y."
  ^ (self x min: aPoint x) @ (self y min: aPoint y)
]

Point ~> negated
[
"Answer a point whose x and y coordinates are
 the negatives of those of the receiver."
  ^ self x negated @ self y negated
]

Point ~> printOn: aStream
[
"The receiver prints on aStream in terms of infix notation."
  self x printOn: aStream.
  aStream nextPut: $@.
  self y printOn: aStream
]

Point ~> r
[
"Answer the receiver's radius in polar coordinate system."
  ^ (self x squared + (self y squared)) sqrt
]

Point ~> rounded
[
"Answer a Point that is the receiver's x and y rounded.
 Answer the receiver if its coordinates are already integral."
  (self x isInteger and: [ y isInteger ])
	ifTrue: [ ^ self ].
  ^ self x rounded @ self y rounded
]

Point ~> setR: rho degrees: theta
[
  | radians |
  radians := theta asFloat degreesToRadians.
  self x: rho asFloat * radians cos.
  self y: rho asFloat * radians sin
]

Point ~> setX: xValue setY: yValue
[
  self x: xValue.
  self y: yValue
]

Point ~> theta
[
"Answer the angle the receiver makes with origin in radians.
 See degrees."
  | tan theta |
  self x = 0
    ifTrue: [ 
	self y >= 0
	  ifTrue:  [ ^ 1.5708	"90.0 degreesToRadians" ]
	  ifFalse: [ ^ 4.71239	"270.0 degreesToRadians" ] ]
    ifFalse: [ 
	tan := self y asFloat / x asFloat.
	theta := tan arcTan.
	self x >= 0
	  ifTrue: [ 
		self y >= 0
		  ifTrue: [ ^ theta ]
		  ifFalse: [ ^ 360.0 degreesToRadians + theta ] ]
	  ifFalse: [ ^ 180.0 degreesToRadians + theta ] ]
]

Point ~> truncated
[
"Answer a Point whose x and y coordinates are integers.
 Answer the receiver if its coordinates are already integral."
(self x isInteger and: [ self y isInteger ])
	ifTrue: [ ^ self ].
  ^ self x truncated @ self y truncated
]
"
Point ~> x
[
^ x
]

Point ~> y
[
^ y
]
"

Point class ~> r: rho degrees: theta
[
"Answer an instance of me with polar coordinates rho and theta."
  ^ self new setR: rho degrees: theta
]

Point class ~> x: xValue y: yValue
[
"Answer an instance of me with the given coordinates."
  ^ self new setX: xValue setY: yValue
]

