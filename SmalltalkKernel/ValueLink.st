"FILE: 'ValueLink.st'
 IMPLEMENTS: ValueLink
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"

Object newSubclassName: #ValueLink iVars: #(nextLink value) cVars: nil.

ValueLink category: #'Collections-Ordered'.

ValueLink ~> = anotherObject
[
^ self species == anotherObject species
	and: [ self value = anotherObject value 
		and: [ self nextLink == anotherObject nextLink ] ]
].

ValueLink ~> asLink
[
  ^ self
].

ValueLink ~> hash
[
  ^ self value hash bitXor: self nextLink identityHash
].


ValueLink ~> printOn: aStream
[
  aStream nextPutAll: 'ValueLink('.
  self value printOn: aStream.
  aStream nextPut: $)
].


ValueLink class ~> value: aValue
[
  ^ self new value: aValue
].


"		E O F 		"
