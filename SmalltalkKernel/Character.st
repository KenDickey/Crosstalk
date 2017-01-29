 "FILE: 'Character.st'
 IMPLEMENTS: fill in class Character
 AUTHOR: Guillermo Polito [Note LICENCE.md in this directory]
 Modifications by: Ken Dickey
 DATE: 20 January 2017
"


Character ~> isVowel
[
"Answer true if the receiver is one of the vowels AEIOU (either upper- or lowercase)."
"@@ ??Unicode not supported here?? @@"
  ^ 'AEIOU' includes: self asUppercase
].

Character ~> printOn: aStream
[
	aStream nextPut: $$.
	aStream nextPut: self
].

Character ~> to: other
[
	"Answer with a collection of all characters in the given ASCII range. For example, $a to: $z"
	^ (self asInteger to: other asInteger) collect: [ :i | i asCharacter ]
].


"		E O F 		"
