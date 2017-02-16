"FILE: 'WeakSend.st'
 IMPLEMENTS: Classes WeakMessageSend, WeakActionSequence
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence (note class comment)]
 Modifications by: Ken Dickey
 DATE: 16 Febuary 2017
"


Array newSubclassName: #WeakActionSequence iVars: #() cVars: #().

Object newSubclassName: #WeakMessageSend iVars: #(selector shouldBeNil arguments) cVars: #().

WeakActionSequence category: #'Kernel-Objects'.

WeakMessageSend    category: #'Kernel-Objects'.


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

WeakActionSequence ~> asActionSequence
[
  ^self 
].

WeakActionSequence ~> asMinimalRepresentation
[
  | valid |
  valid := self reject: [:e | e isReceiverOrAnyArgumentGarbage ].
  valid size = 0
	ifTrue: [ ^nil ].
  valid size = 1
	ifTrue: [ ^valid first ].
  ^valid 
].

WeakActionSequence ~> value
[
"Do the same as my parent, but make sure that all actions that do not  
give errors are evaluated before resignaling the ones that gave errors  
(giving the chance to clients to handle them)."

  ^self valueStartingFrom: 1 
].

WeakActionSequence ~> valueStartingFrom: startIndex
[
"Do the same as my parent, but make sure that all actions that do not 
give errors are evaluated before resignaling the ones that gave errors 
(giving the chance to clients to handle them)."

  | each answer |
  startIndex to: self size do: [:index |
	each := self at: index.
	[ answer := each value ]
		on: UnhandledError
		do: [:exc | 
		      self valueStartingFrom: index + 1.
		      exc pass
		]
  ].
  ^ answer 
].

WeakActionSequence ~> valueWithArguments: anArray 
[
"Do the same as my parent, but make sure that all actions that do not 
give errors are evaluated before resignaling the ones that gave errors 
(giving the chance to clients to handle them)."

  ^self valueWithArguments: anArray startingFrom: 1 
].

WeakActionSequence ~> valueWithArguments: anArray startingFrom: startIndex
[
"Do the same as my parent, but make sure that all actions that do not 
give errors are evaluated before resignaling the ones that gave errors 
(giving the chance to clients to handle them)."

  | each answer |
  startIndex to: self size do: [:index |
	each := self at: index.
	[ answer := each valueWithArguments: anArray ]
		on: UnhandledError
		do: [:exc | 
			self valueWithArguments: anArray startingFrom: index + 1.
			exc pass]
  ].
  ^ answer 
].

WeakActionSequence ~> printOn: aStream
[
  self size < 2 ifTrue: [^super printOn: aStream].
  aStream nextPutAll: '#('.
  self
	do: [:each | each printOn: aStream]
	separatedBy: [aStream newLine].
  aStream nextPut: $) 
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "


WeakMessageSend comment:
'Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend.
 Unlike MessageSend it is not necessarily a valid mesage. 
 A request to value only results in a send if in fact it is valid. 

 See MessageSendComments also. WeakMessageSend is used primarily for event regristration. 

 Unlike MessageSend WeakMessageSend stores receiver (object receiving the message send)
 as a the first and only element of its array as opposed to a named ivar.
 But like MessageSend, it does have
   selector		Symbol -- message selector
   arguments		Array -- bound arguments
 and it also has
   shouldBeNil		Boolean --  used to ensure array of arguments is not all nils
'.

WeakMessageSend ~> initialize
[ 
  self arguments: Array new
].

"WeakMessageSend ~> arguments: anArray
[
  self arguments: WeakArray withAll: anArray.
 ""no reason this should be a WeakArray""
  self shouldBeNil: Array withAll: (anArray collect: [ :ea | ea isNil ]).
].
"

WeakMessageSend ~> receiver
[
  ^self arguments at: 1
].

WeakMessageSend ~> receiver: anObject
[
  self arguments at: 1 put: anObject
].
 

WeakMessageSend ~> = anObject
[
"Any object is equal to itself"
  self == anObject ifTrue: [ ^ true ].

"Compare equal to equivalent MessageSend"
  ^ (anObject is: #MessageSend)
	and: [self receiver == anObject receiver
	and: [selector == anObject selector
	and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]
]. 

WeakMessageSend ~> hash
[
"work like MessageSend>>hash"
  ^self receiver hash bitXor: selector hash
].


WeakMessageSend ~> asActionSequence
[
	^WeakActionSequence with: self 
].

WeakMessageSend ~> asMessageSend
[
  ^MessageSend 
	receiver: self receiver 
	selector: self selector
	arguments: (Array withAll: self arguments) 
].

WeakMessageSend ~> asMinimalRepresentation
[
  ^self isReceiverOrAnyArgumentGarbage
	ifTrue:  [ nil  ]
	ifFalse: [ self ] 
].

WeakMessageSend ~> value
[
  ^ self arguments
	ifNil: [
		self withEnsuredReceiverDo: [ :r | r perform: self selector ]
	]
	ifNotNil: [
		self withEnsuredReceiverAndArgumentsDo: [ :r :a |
		r perform: self selector withArguments: a ]
	]
]. 

WeakMessageSend ~> valueWithArguments: anArray
[
  | argsToUse |
"Safe to use, because they are built before ensureing receiver and args..."
  argsToUse := self collectArguments: anArray.
  ^ self withEnsuredReceiverAndArgumentsDo: [ :r :a |
	r perform: self selector withArguments: argsToUse 
  ] 
].

WeakMessageSend ~> printOn: aStream
[
  aStream nextPutAll: self class name;
          nextPut: $(.
  self selector printOn: aStream.
  aStream nextPutAll: ' -> '.
  self receiver printOn: aStream.
  aStream nextPut: $)
].

WeakMessageSend ~> collectArguments: anArgArray
[
"Private"
  | staticArgs |
  staticArgs := self arguments.
  ^(anArgArray size = staticArgs size)
      ifTrue: [anArgArray]
      ifFalse: [
            (staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs copy]
	     )
               replaceFrom: 1
               to: (anArgArray size min: staticArgs size)
               with: anArgArray
               startingAt: 1
      ]
].
 

WeakMessageSend ~> isAnyArgumentGarbage
[
  "Make sure that my arguments haven't gone away"
  self arguments ifNotNil: [
	self arguments with: self shouldBeNil do: [ :arg :flag |
		(flag not and: [arg isNil]) ifTrue: [^true]
	]
  ].
  ^false
].

WeakMessageSend ~> isReceiverGarbage
[
"Make sure that my receiver hasn't gone away"
  ^self receiver isNil
].

WeakMessageSend ~> isReceiverOrAnyArgumentGarbage
[
"Make sure that my receiver hasn't gone away"
  ^self isReceiverGarbage or: [self isAnyArgumentGarbage]
]. 

WeakMessageSend ~> withEnsuredReceiverAndArgumentsDo: aBlock
[
"Grab real references to receiver and arguments. If they still exist, evaluate aBlock."
"Return if my receiver has gone away"
  | r a |
  r := self receiver.
  r ifNil: [ ^nil ].

	
"Make sure that my arguments haven't gone away"
  a := Array withAll: self arguments.
  a with: self shouldBeNil do: [ :arg :flag |
	arg ifNil: [ flag ifFalse: [ ^nil ]]
  ].

  ^aBlock value: r value: a 
].

WeakMessageSend ~> withEnsuredReceiverDo: aBlock
[
"Grab a real reference to receive. If still there, evaluate aBlock."
"Return nil if my receiver has gone away"

  ^ self receiver ifNotNil: [ :r | aBlock value: r ] 
].

WeakMessageSend ~> is: aSymbol
[
  ^ aSymbol == #MessageSend or: [ super is: aSymbol ] 
].


WeakMessageSend class ~> new
[
  ^self new: 1
].

WeakMessageSend class ~> receiver: anObject selector: aSymbol
[ 
  ^ self receiver: anObject selector: aSymbol arguments: #()
].

WeakMessageSend class ~> receiver: anObject selector: aSymbol argument: aParameter
[
  ^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)
].

WeakMessageSend class ~> receiver: anObject selector: aSymbol arguments: anArray
[
  ^ self new
	receiver: anObject;
	selector: aSymbol;
	arguments: anArray
].


"		E O F 		"
