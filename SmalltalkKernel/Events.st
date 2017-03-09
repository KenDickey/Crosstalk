"FILE: 'Events.st'
 IMPLEMENTS: Object events & Class ActiveModel
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
This includes #when:send:to: and friends, in
 Object (generic implementation)
 and ActiveModel (optimized implementation).'.


" === Events === "

" events-accessing "

Object ~> actionForEvent: anEventSelector
[
"Answer the action to be evaluated when <anEventSelector> has been triggered."

  ^self actionMap ifNotNil: [ :map |
        map
	    at: anEventSelector asSymbol
	    ifAbsent: nil
  ]
]. 

Object ~> actionMap
[
  ^ActiveModel actionMapFor: self
].

Object ~> actionSequenceForEvent: anEventSelector
[
  ^self actionMap
	ifNotNil: [ :map |
	    (map
		at: anEventSelector asSymbol
		ifAbsent: [ WeakActionSequence new ]
	    ) asActionSequence 
	]
	ifNil: [ WeakActionSequence new ] 
].

Object ~> actionsDo: aBlock
[
  self actionMap ifNotNil: [ :map | map do: aBlock ] 
].

Object ~> hasActionForEvent: anEventSelector
[
"Answer true if there is an action associated with anEventSelector"

  ^(self actionForEvent: anEventSelector) notNil 
].

Object ~> setActionSequence: actionSequence forEvent: anEventSelector
[
  | action |
"This is a good time to compact the action sequence of old, garbage collected stuff."
"@@FIXME  action := actionSequence asMinimalRepresentation."
  action
	ifNil: [ self removeActionsForEvent: anEventSelector]
	ifNotNil: [
		self updateableActionMap
			at: anEventSelector asSymbol
			put: action
	]
]. 

Object ~> updateableActionMap
[
  ^ActiveModel updateableActionMapFor: self
].


" events-registering "


Object ~> when: anEventSelector evaluate: anAction 
[
  | actions |
  actions := self actionSequenceForEvent: anEventSelector.
  (actions includes: anAction) ifTrue: [^ self].
  self 
	setActionSequence: (actions copyWith: anAction)
	forEvent: anEventSelector 
].

Object ~> 
	when: anEventSelector
	send: aMessageSelector
	to: anObject
 [
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector) 
 ].

Object ~>
	when: anEventSelector
	send: aMessageSelector
	to: anObject
	with: anArg
 [
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
	    arguments: (Array with: anArg)) 
].

Object ~>
	when: anEventSelector
	send: aMessageSelector
	to: anObject
	withArguments: anArgArray
[
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
	    arguments: anArgArray) 
].


" events-removing "


Object ~> releaseActionMap
[
  ActiveModel releaseActionMapFor: self 
].

Object ~> removeAction: anAction forEvent: anEventSelector
[
    self
        removeActionsSatisfying: [:action | action = anAction]
        forEvent: anEventSelector 
].

Object ~> removeActionsForEvent: anEventSelector
[
  | map |
  map := self actionMap.
  map ifNotNil: [
	map removeKey: anEventSelector asSymbol ifAbsent: nil.
	(map isEmpty) ifTrue: [ self releaseActionMap ]
  ] 
].

Object ~> removeActionsSatisfying: aBlock
[
  self actionMap ifNotNil: [ :map |
	map keys do: [ :eachEventSelector |
		self
			removeActionsSatisfying: aBlock
			forEvent: eachEventSelector
	]
  ]
]. 

Object ~> removeActionsSatisfying: aOneArgBlock forEvent: anEventSelector
[
  self
	setActionSequence:
		((self actionSequenceForEvent: anEventSelector)	reject: aOneArgBlock)
	forEvent: anEventSelector 
].

Object ~> removeActionsWithReceiver: anObject
[
  self actionMap ifNotNil: [ :map |
	map keys do: [ :eachEventSelector |
	    self
		removeActionsSatisfying: [:anAction | anAction receiver == anObject]
		forEvent: eachEventSelector
	]
  ]
]. 

Object ~> removeActionsWithReceiver: anObject forEvent: anEventSelector
[
    self
        removeActionsSatisfying: [:anAction | anAction receiver == anObject]
        forEvent: anEventSelector 
].


" events-triggering "

Object ~> triggerEvent: anEventSelector
[
"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

  ^(self actionForEvent: anEventSelector) value 
].

Object ~> triggerEvent: anEventSelector with: anObject
[
    ^self 
	triggerEvent: anEventSelector
	withArguments: {anObject} 
].

Object ~> triggerEvent: anEventSelector withArguments: anArgumentList
[
  ^ (self actionForEvent: anEventSelector)
	valueWithArguments: anArgumentList 
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

ActiveModel initialize. "Do the deed"



"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" Old Dependency Protocol "
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object ~> addDependent: anObject
[
"Make the given object one of the receiver's dependents."

  self
	when: #changed:
	send: #update:
	to: anObject.
  ^anObject
].

Object ~> breakDependents
[
"Remove all of the receiver's dependents."

  self removeActionsForEvent: #changed:
].

Object ~> changed
[
"Receiver changed in a general way; inform all the dependents by 
 sending each dependent an update: message."

  self changed: self
].

Object ~> changed: aParameter 
[
"Receiver changed. The change is denoted by the argument aParameter. 
 Usually the argument is a Symbol that is part of the dependent's change 
 protocol. Inform all of the dependents."

  self 
	triggerEvent: #changed:
	with: aParameter
].

Object ~> removeDependent: anObject
[
"Remove the given object as one of the receiver's dependents."

  self 
	removeActionsWithReceiver: anObject
	forEvent: #changed:.
  ^ anObject
].

Object ~> update: aParameter 
[
"Receive a change notice from an object of whom the receiver is a 
 dependent. The default behavior is to do nothing; a subclass might want 
 to change itself in some way."

  ^self
].





"		E O F 		"
