"FILE: 'Association.st'
 IMPLEMENTS: Association
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"
Magnitude newSubclassName: #Association iVars: #(key value) cVars: nil.

Association category: #'Collections-Support'.

Association comment:
'I represent a pair of associated objects--a key and a value.'.

Association ~> < aLookupKey
[
  "For sort by keys."
  ^ key < aLookupKey key
].

"@@FIXME?? 
#= and #hash differ from Cuis, which must match both key & value"
Association ~> = anAssociation
[
"True if the receiver and argument have equal keys."
  self species = anAssociation species
	ifTrue: [ ^ key = anAssociation key ]
	ifFalse: [ ^ false ]
].

Association ~> hash
[
"Hash is reimplemented because = is implemented."
  ^ key hash
].


Association ~> key: aKey value: anObject
[
  self key:   aKey.
  self value: anObject
].

Association ~> printOn: aStream
[
"Print in the format (key->value)."
  aStream nextPut: $(.
  key printOn: aStream.
  aStream nextPutAll: '->'.
  value printOn: aStream.
  aStream nextPut: $)
].

Association class ~> key: newKey value: newValue
[
 "Answer a new Association with the given key and value."
  ^ self new key: newKey value: newValue
].


"		E O F 		"
