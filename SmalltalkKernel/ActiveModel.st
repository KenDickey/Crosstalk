"FILE: 'ActiveModel.st'
 IMPLEMENTS: Class ActiveModel
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence (note class comment)]
 Modifications by: Ken Dickey
 DATE: 03 Febuary 2017
"

Object newSubclassName: #ActiveModel iVars: #(actionMap) cVars: #(ActionMaps).


ActiveModel category: #'Kernel-Objects'.

ActiveModel comment:
'Change Set:		Events
Date:			11 February 2002
Author:			Rob Withers et al.

The new event model, originally from VisualSmalltalk.
This includes #when:send:to: and friends, in Object (generic implementation)
 and ActiveModel (optimized implementation).
It renders the old change/update mechanism in Object and Model as obsolete,
 although still used.'.


Object ~> actionMap
[
  ^ActiveModel actionMapFor: self
].

Object ~> removeActionsForEvent: anEventSelector
[
  | map |
  map := self actionMap.
  map ifNotNil: [
	map removeKey: anEventSelector asSymbol ifAbsent: nil.
	map isEmpty ifTrue: [ self releaseActionMap ]
  ]
].

Object ~> releaseActionMap
[
  ActiveModel releaseActionMapFor: self
].

Object ~> updateableActionMap
[
  ^ActiveModel updateableActionMapFor: self
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

ActiveModel ~> postCopy
[
  self actionMap: nil
].


ActiveModel ~> releaseActionMap
[
    self actionMap: nil 
].

ActiveModel ~> updateableActionMap
[
  self actionMap ifNil: [
	self actionMap: IdentityDictionary new 
  ].
  ^self actionMap 
].


ActiveModel class ~> actionMapFor: anObject
[
    ^self actionMaps
        at: anObject
        ifAbsent: nil 
].

ActiveModel class ~> actionMaps
[
	self ActionMaps ifNil: [
	"@@ FIXME: WeakIdentityKeyDictionary new @@"
		self ActionMaps: IdentityDictionary new ].
	^self ActionMaps 
].

ActiveModel class ~> updateableActionMapFor: anObject
[
  ^self actionMaps
	at: anObject
	ifAbsentPut: [ IdentityDictionary new ] 
].

ActiveModel class ~> releaseActionMapFor: anObject
[
  self actionMaps
	removeKey: anObject
	ifAbsent: nil 
].

ActiveModel class ~> flushEventSystem
[
  self actionMaps keysAndValuesDo: [ :rcvr :evtDict | 
	rcvr ifNotNil:[
		"make sure we don't modify evtDict while enumerating"
		evtDict keys do: [ :evtName | | msgSet |
			msgSet := evtDict at: evtName ifAbsent: nil.
			msgSet ifNil: [ rcvr removeActionsForEvent: evtName]]]].
  self actionMaps finalizeValues.  
].


ActiveModel class ~> initialize
[
  "ActiveModel Class init"
  ActiveModel ActionMaps: IdentityDictionary new.
].

ActiveModel initialize. "Do the deed!"


"		E O F 		"
