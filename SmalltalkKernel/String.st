"FILE: 'String.st'
 IMPLEMENTS: Augments class String
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 27 January 2017
"

String ~> substrings
[
	^ self findTokens: ' '
].

String ~> findDelimiters: delimiters startingAt: start
[
"Answer the index of the character within the receiver,
 starting at start, that matches one of the delimiters.
 If the receiver does not contain any of the delimiters,
 answer size + 1."
  start to: self size do: [ :i | 
	delimiters
		do: [ :delim | 
			delim = (self at: i)
				ifTrue: [ ^ i ] ] ].
  ^ self size + 1
].

"String ~> findString: key startingAt: start caseSensitive: caseSensitive
 Answer the index in this String at which the substring key
 first occurs at or after the given starting index.
 The match can be case-sensitive or not.
 Answer zero if no match is found."

String ~> findTokens: delimiters
[
"Answer the collection of tokens that result from parsing self.
 Any character in the String delimiters marks a border.
 Several delimiters in a row are considered as just one separation."
  | tokens keyStart keyStop |
  tokens := OrderedCollection new.
  keyStop := 1.
  [ keyStop <= self size ]
	whileTrue: [ 
	  keyStart := self skipDelimiters: delimiters startingAt: keyStop.
	  keyStop := self findDelimiters: delimiters startingAt: keyStart.
	  keyStart < keyStop
		ifTrue: [ tokens add: 
				(self copyFrom: keyStart to: keyStop - 1) 
		] 
	].
  ^ tokens
].


String ~> indexOfAscii: anInteger inString: aString startingAt: start
[
"@@@?? Is this useful ??@@@"
  ^ aString indexOf: anInteger asCharacter startingAt: start ifAbsent: [0]
].

String ~> numArgs
[
"Answer the number of arguments that the receiver
 would take considered as a selector
 or -1 if it couldn't be a selector."
  | firstChar numColons |
  firstChar := self at: 1.
  firstChar isLetter
	ifTrue: [ 
		numColons := 0.
		self  do: [ :ch | 
			ch tokenish
				ifFalse: [ ^ -1 ].
			ch = $:
				ifTrue: [ numColons := numColons + 1 ]
		].
		numColons > 0 & (self last ~= $:)
			ifTrue: [ ^ -1 ].
		^ numColons 
	].
  firstChar isSpecial
	ifTrue: [ 
		self size = 1
			ifTrue: [ ^ 1 ].
		(self size = 2 and: [ (self at: 2) isSpecial ])
			ifTrue: [ ^ 1 ].
		^ -1 
	].
  ^ -1
].

String ~> printOn: aStream
[
"Print inside string quotes, doubling inbedded quotes."
  | x |
  aStream nextPut: $'.
  1 to: self size do: [ :i | 
	aStream nextPut: (x := self at: i).
	x == $'
		ifTrue: [ aStream nextPut: x ] ].
  aStream nextPut: $'
].


String ~> skipDelimiters: delimiters startingAt: start
[
"Answer the index of the first character in this string
 starting at the given index
 that does NOT match one of the delimiters.
 If the receiver does not contain any of the delimiters,
 answer size + 1.
 Assumes the delimiters to be a non-empty string."
  start to: self size do: [ :i | 
	(delimiters indexOf: (self at: i) startingAt: 1) = 0
		ifTrue: [ ^ i ] 
  ].
  ^ self size + 1
].

String class ~> cr
[
"Answer a string containing a carriage return character."
  ^ self with: Character cr
].

String class ~> newline
[
"Answer a string containing a carriage return character."
  ^ self with: Character newline
].

"		--- E O F ---		"
