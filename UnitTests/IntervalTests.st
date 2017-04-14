"FILE: 'ExceptionTests.st'
 IMPLEMENTS: Exceptions unit tests (less the GUI)
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 18 March 2017
"

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
TestCase newSubclassName: #IntervalTest  iVars: #() cVars: #().
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

IntervalTest category: #'Tests-Collections'.

IntervalTest ~> assert: aNumber isCloseTo: anotherNumber
[
  self assert: (aNumber isCloseTo: anotherNumber) 
].

IntervalTest ~> testAt
[
"
  IntervalTest new testAt
"
  self assert:  (2 to: 5 by: 2) first = 2.
  self assert: ((2 to: 5 by: 2) at: 1) = 2.
  self assert: ((2 to: 5 by: 2) at: 2) = 4.
  self assert:  (2 to: 5 by: 2) last = 4.

  self assert:  (0 to: 2.4 by: 0.1) first = 0.
  self assert: ((0 to: 2.4 by: 0.1) at:  1) = 0.
  self assert: ((0 to: 2.4 by: 0.1) at:  2) isCloseTo: 0.1.
  self assert: ((0 to: 2.4 by: 0.1) at:  3) isCloseTo: 0.2.
  self assert: ((0 to: 2.4 by: 0.1) at: 23) isCloseTo: 2.2.
  self assert: ((0 to: 2.4 by: 0.1) at: 24) = 2.3.
  self assert: ((0 to: 2.4 by: 0.1) at: 25) = 2.4.
  self assert:  (0 to: 2.4 by: 0.1) last = 2.4. 
].

IntervalTest ~> testFloatInterval
[
"
  IntervalTest new testFloatInterval
" 
  self assert: (0 to: 2.4 by: 0.1) size = 25.
  self assert: (0 to: 2.4 by: 0.1) first = 0.
  self assert: (0 to: 2.4 by: 0.1) last isCloseTo: 2.4.

  2 to: 200 do: [ :n |
  	self assert: (0.0 to: 1.0 count: n) size = n.
  	self assert: (0.0 to: 1.0 count: n) first = 0.0.
  	self assert: (0.0 to: 1.0 count: n) last isCloseTo: 1.0].
  2 to: 200 do: [ :n |
  	self assert: (1.0 to: 0.0 count: n) size = n.
  	self assert: (1.0 to: 0.0 count: n) first = 1.0.
  	self assert: (1.0 to: 0.0 count: n) last isCloseTo: 0.0] 
].

"IntervalTest ~> testFloatToByDo
[
""
  IntervalTest new testFloatToByDo
""
  | interval toByDo |

  interval := 0 to: 2.4 by: 0.1.
  toByDo := Array streamContents: [ :strm |
  	0 to: 2.4 by: 0.1 do: [ :each |
  		strm nextPut: each ]].
  self assert: toByDo size = interval size.
  1 to: toByDo size do: [ :i |
  	self assert: (toByDo at: i) = (interval at: i) ] 
]."

IntervalTest ~> testIntegerInterval
[
"
  IntervalTest new testIntegerInterval
"
  | z |
  self assert: (2 to: 5 by: 2) size = 2.
  self assert: (2 to: 5 by: 2) last = 4.
  z := 0.
  (2 to: 5 by: 2) do: [ :i | z := i ].
  self assert: z = 4 
].

"
IntervalTest ~> testIntegerToByDo
[
""
  IntervalTest new testIntegerToByDo
""
  | interval toByDo |
  interval := 2 to: 5 by: 2.
  toByDo := Array streamContents: [ :strm |
  	2 to: 5 by: 2 do: [ :each |
  		strm nextPut: each ]].
  self assert: toByDo size = interval size.
  1 to: toByDo size do: [ :i |
  	self assert: (toByDo at: i) = (interval at: i) ] 
]."

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"File 'IntervalTests' -> #RunIntervalTests block"
Smalltalk at: #RunIntervalTests put:
[
   Transcript 
	newLine;
	nextPutAll: (IntervalTest buildSuite run)
].


"    E O F     "

