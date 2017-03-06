"FILE: 'ExceptionTests.st'
 IMPLEMENTS: Exceptions unit tests (less the GUI)
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 06 MArch 2017
"

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Error newSubclassName: #MyResumableTestError iVars: #() cVars: #().

MyResumableTestError category: #'Tests-Exceptions'.


MyResumableTestError ~> isResumable
[
  ^true 
].


Error newSubclassName: #MyTestError iVars: #() cVars: #().

MyTestError category: #'Tests-Exceptions'.

classDefinition: #MyTestNotification category: #'Tests-Exceptions'
Notification subclass: #MyTestNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Exceptions'


classDefinition: #ExceptionTests category: #'Tests-Exceptions'
TestCase subclass: #ExceptionTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Exceptions'

ExceptionTests methodsFor: 'private' stamp: 'md 3/25/2003 23:40'
assertSuccess: anExceptionTester
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded']. 


ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 6/1/2004 21:54'
testDoubleOuterPass
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'dtl 6/1/2004 21:54'
testDoublePassOuter
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:43'
testDoubleResume
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'
testNonResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 10/10/2015 16:22'
testResignalAs

	| answer |
	answer := [
		[3 zork]
			on: ZeroDivide
			do: [:ex | ex return: 5]
	] on: Error do: [:ex | ex resignalAs: ZeroDivide].
	self assert: answer == 5 

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'SqR 11/15/2015 11:42'
testResignalAsUnwinds

	| unwound answer |
	unwound := false.
	answer := [
		[3 zork]
			on: ZeroDivide do: [:ex | self assert: unwound.  ex return: 5]
	] on: Error do: [:ex | [ex resignalAs: ZeroDivide] ifCurtailed: [unwound := true]].
	self assert: answer == 5 

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'
testResumableFallOffTheEndHandler
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:44'
testSignalFromHandlerActionTest
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'
testSimpleEnsure
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:45'
testSimpleEnsureTestWithError
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:46'
testSimpleEnsureTestWithNotification
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:45'
testSimpleEnsureTestWithUparrow
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:46'
testSimpleIsNested
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:41'
testSimpleOuter
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:42'
testSimplePass
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:43'
testSimpleResignalAs
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'
testSimpleResume
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'
testSimpleRetry
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:47'
testSimpleRetryUsing
	self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest )  

ExceptionTests methodsFor: 'testing-ExceptionTester' stamp: 'md 3/25/2003 23:48'
testSimpleReturn
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest )  


ExceptionTests methodsFor: 'testing-outer' stamp: 'SqR 8/28/2014 22:45'
testHandlerFromAction
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [
		[
			[self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
		] on: Error do: [ :ex | 3 / 0 ]
	] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer' 

ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 6/1/2004 21:59'
testNonResumableOuter

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error
 

ExceptionTests methodsFor: 'testing-outer' stamp: 'dtl 6/1/2004 22:00'
testNonResumablePass

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error
 

ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 11/15/2010 11:39'
testResumableOuter

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result = 5
 

ExceptionTests methodsFor: 'testing-outer' stamp: 'ul 11/15/2010 11:39'
testResumablePass

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
	self assert: result = 4
 


ExceptionTests methodsFor: 'testing' stamp: 'brp 10/21/2004 16:42'
testNoTimeout
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest )  

ExceptionTests methodsFor: 'testing' stamp: 'brp 10/21/2004 16:41'
testTimeoutWithZeroDuration
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest )  


classDefinition: #ProcessTerminateBug category: #'Tests-Exceptions'
TestCase subclass: #ProcessTerminateBug
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Exceptions'

ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'
testSchedulerTermination
	| process sema gotHere sema2 |
	gotHere := false.
	sema := Semaphore new.
	sema2 := Semaphore new.
	process := [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere := true ] newProcess.
	process priority: Processor activeProcess priority.
	process name: 'testSchedulerTermination'.
	process resume.
	sema wait.
	"until process gets scheduled"
	process terminate.
	sema2 signal.
	Processor yield.
	"will give process a chance to continue and horribly screw up"
	self assert: gotHere not. 

ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 11/4/2011 22:00'
testTerminationDuringUnwind
	"An illustration of the issue of process termination during unwind.
	This uses a well-behaved unwind block that we should allow to complete
	if at all possible."
	"
	self new testTerminationDuringUnwind
	"
	| unwindStarted unwindFinished p |
	unwindStarted := unwindFinished := false.
	p := [[] ensure:[
			unwindStarted := true.
			Processor yield.
			unwindFinished := true.
		]] newProcess.
	p resume.
	self deny: unwindStarted.
	Processor yield.
	self assert: unwindStarted.
	self deny: unwindFinished.
	p terminate.
	self assert: unwindFinished. 

ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'
testUnwindFromActiveProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert:(sema isSignaled).
	process := [
		sema critical:[
			self deny: sema isSignaled.
			Processor activeProcess terminate.
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromActiveProcess'.
	process resume.
	self assert: sema isSignaled 

ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:14'
testUnwindFromForeignProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: sema isSignaled.
	process := [
		sema critical:[
			self deny: sema isSignaled.
			sema wait. "deadlock"
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromForeignProcess'.
	process resume.
	self deny: sema isSignaled.
	"This is for illustration only - the BlockCannotReturn cannot 
	be handled here (it's truncated already)"
	self shouldnt: [process terminate] raise: BlockCannotReturn.
	self assert: sema isSignaled 


classDefinition: #ExceptionTester category: #'Tests-Exceptions'
Object subclass: #ExceptionTester
	instanceVariableNames: 'log suiteLog iterationsBeforeTimeout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Exceptions'

ExceptionTester methodsFor: 'accessing' stamp: 'dtl 6/1/2004 21:53'
basicANSISignaledExceptionTestSelectors

	^#( simpleIsNestedTest simpleOuterTest doubleOuterTest doubleOuterPassTest doublePassOuterTest simplePassTest simpleResignalAsTest simpleResumeTest simpleRetryTest simpleRetryUsingTest simpleReturnTest) 

ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:54'
basicTestSelectors
	^ #(#simpleEnsureTest #simpleEnsureTestWithNotification #simpleEnsureTestWithUparrow #simpleEnsureTestWithError #signalFromHandlerActionTest #resumableFallOffTheEndHandler #nonResumableFallOffTheEndHandler #doubleResumeTest #simpleTimeoutWithZeroDurationTest #simpleTimeoutTest simpleNoTimeoutTest) 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:14'
doSomethingElseString

	^'Do something else.' 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:14'
doSomethingExceptionalString

	^'Do something exceptional.' 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:13'
doSomethingString

	^'Do something.' 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:15'
doYetAnotherThingString

	^'Do yet another thing.' 

ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:15'
iterationsBeforeTimeout

	^ iterationsBeforeTimeout 

ExceptionTester methodsFor: 'accessing' stamp: 'brp 10/21/2004 17:16'
iterationsBeforeTimeout: anInteger

	iterationsBeforeTimeout := anInteger 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/7/1999 15:03'
log

	log == nil
		ifTrue: [log := OrderedCollection new].
	^log 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:30'
suiteLog

	suiteLog == nil
		ifTrue: [suiteLog := OrderedCollection new].
	^suiteLog 

ExceptionTester methodsFor: 'accessing' stamp: 'tfei 6/8/1999 09:15'
testString

	^'This is only a test.' 


ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/8/1999 09:17'
clearLog

	log := nil 

ExceptionTester methodsFor: 'logging' stamp: 'jmv 3/13/2012 12:33'
contents

	^( self log
		inject: (WriteStream on: (String new: 80))
		into: [ :result :item |
			result 
				newLine; 
				nextPutAll: item;
				yourself] ) contents 

ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/7/1999 15:03'
log: aString

	self log add: aString 

ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/12/1999 23:07'
logTest: aSelector

	self suiteLog add: aSelector 

ExceptionTester methodsFor: 'logging' stamp: 'tfei 6/8/1999 09:38'
logTestResult: aString

	| index |
	index := self suiteLog size.
	self suiteLog 
		at: index
		put: ((self suiteLog at: index), ' ', aString) 


ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:13'
doSomething

	self log: self doSomethingString 

ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:14'
doSomethingElse

	self log: self doSomethingElseString 

ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:14'
doSomethingExceptional

	self log: self doSomethingExceptionalString 

ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:15'
doYetAnotherThing

	self log: self doYetAnotherThingString 

ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:16'
methodWithError

	MyTestError signal: self testString 

ExceptionTester methodsFor: 'pseudo actions' stamp: 'tfei 6/8/1999 09:16'
methodWithNotification

	MyTestNotification signal: self testString 


ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:51'
doubleOuterPassTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer.
			self doSomethingElse]]
			on: MyTestNotification
			do: [:ex | ex pass.
				self doSomethingExceptional]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:49'
doubleOuterTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer.
			self doSomethingExceptional]]
			on: MyTestNotification
			do: [:ex | ex outer.
				self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'dtl 6/1/2004 21:52'
doublePassOuterTest
	"uses #resume"

	[[[self doSomething.
	MyTestNotification signal.
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex pass.
			self doSomethingExceptional]]
			on: MyTestNotification
			do: [:ex | ex outer.
				self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:27'
simpleIsNestedTest
	"uses resignalAs:"

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			ex isNested "expecting to detect handler in #runTest:"
				ifTrue:
					[self doYetAnotherThing.
					ex resignalAs: MyTestNotification new]] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tpr 5/27/2004 21:50'
simpleOuterTest
	"uses #resume"

	[[self doSomething.
	MyTestNotification signal.
	"self doSomethingElse"
	self doSomethingExceptional]
		on: MyTestNotification
		do: [:ex | ex outer. self doSomethingElse]]
				on: MyTestNotification
				do: [:ex | self doYetAnotherThing. ex resume] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 00:37'
simplePassTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex pass "expecting handler in #runTest:"] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 02:12'
simpleResignalAsTest
	"ExceptionTester new simpleResignalAsTest"

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do:
			[:ex | ex resignalAs: MyTestError new] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'nice 12/27/2009 21:06'
simpleResumeTest
	"see if we can resume twice"
	
	[ | it |
	self doSomething.
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	]
		on: MyResumableTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex resume: 3] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:02'
simpleRetryTest

	| theMeaningOfLife |
	theMeaningOfLife := nil.
	[self doSomething.
	theMeaningOfLife == nil
		ifTrue: [MyTestError signal]
		ifFalse: [self doSomethingElse]]
			on: MyTestError
			do:
				[:ex |
				theMeaningOfLife := 42.
				self doYetAnotherThing.
				ex retry] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 01:03'
simpleRetryUsingTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex | ex retryUsing: [self doYetAnotherThing]] 

ExceptionTester methodsFor: 'signaledException tests' stamp: 'tfei 6/13/1999 00:59'
simpleReturnTest

	| it |
	it :=
		[self doSomething.
		MyTestError signal.
		self doSomethingElse]
			on: MyTestError
			do: [:ex | ex return: 3].
	it = 3 ifTrue: [self doYetAnotherThing] 


ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 6/1/2004 21:56'
doubleOuterPassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'dtl 6/1/2004 21:56'
doublePassOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:09'
simpleIsNestedTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:10'
simpleOuterTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: self doSomethingElseString;
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:10'
simplePassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:11'
simpleResignalAsTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'RAA 12/8/2000 12:59'
simpleResumeTestResults

	"see if we can resume twice"

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:23'
simpleRetryTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingString;
			add: self doSomethingElseString;
			yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 01:23'
simpleRetryUsingTestResults

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			yourself 

ExceptionTester methodsFor: 'signaledException results' stamp: 'tfei 6/13/1999 02:22'
simpleReturnTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		yourself 


ExceptionTester methodsFor: 'tests' stamp: 'tfei 11/14/1999 17:26'
doubleResumeTest

       [self doSomething.
       MyResumableTestError signal.
       self doSomethingElse.
       MyResumableTestError signal.
       self doYetAnotherThing]
               on: MyResumableTestError
               do: [:ex | ex resume]. 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/7/1999 13:43'
nonResumableFallOffTheEndHandler
	
	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/9/1999 16:07'
resumableFallOffTheEndHandler

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 8/19/1999 01:39'
signalFromHandlerActionTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[self doYetAnotherThing.
			MyTestError signal] 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 09:44'
simpleEnsureTest

	[self doSomething.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 12:50'
simpleEnsureTestWithError

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/8/1999 10:15'
simpleEnsureTestWithNotification

	[self doSomething.
	self methodWithNotification.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/9/1999 16:04'
simpleEnsureTestWithUparrow

	[self doSomething.
	true ifTrue: [^nil].
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	 

ExceptionTester methodsFor: 'tests' stamp: 'jmv 5/7/2012 21:38'
simpleNoTimeoutTest

	[ self doSomething ]
		valueWithin: 1 days onTimeout:
			[ self doSomethingElse ] 

ExceptionTester methodsFor: 'tests' stamp: 'brp 10/22/2004 12:00'
simpleTimeoutTest

	| n |
	[1 to: 1000000 do: [ :i | n := i. self doSomething ] ]
		valueWithin: 50 milliSeconds onTimeout:
			[ self iterationsBeforeTimeout: n.
			self doSomethingElse ] 

ExceptionTester methodsFor: 'tests' stamp: 'brp 10/22/2004 12:00'
simpleTimeoutWithZeroDurationTest

	[ self doSomething ]
		valueWithin: 0 seconds onTimeout:
			[ self doSomethingElse ].
	 

ExceptionTester methodsFor: 'tests' stamp: 'tfei 6/7/1999 14:28'
warningTest

	self log: 'About to signal warning.'.
	Warning signal: 'Ouch'.
	self log: 'Warning signal handled and resumed.' 


ExceptionTester methodsFor: 'results' stamp: 'tfei 11/14/1999 17:29'
doubleResumeTestResults

       ^OrderedCollection new
               add: self doSomethingString;
               add: self doSomethingElseString;
               add: self doYetAnotherThingString;
               yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 09:21'
nonResumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 8/19/1999 02:39'
resumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 8/19/1999 01:51'
signalFromHandlerActionTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 09:47'
simpleEnsureTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 6/9/1999 17:44'
simpleEnsureTestWithErrorResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 10:13'
simpleEnsureTestWithNotificationResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingElseString;
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'tfei 6/8/1999 18:55'
simpleEnsureTestWithUparrowResults

	^OrderedCollection new
		add: self doSomethingString;
"		add: self doSomethingElseString;"
		add: self doYetAnotherThingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 16:54'
simpleNoTimeoutTestResults

	^OrderedCollection new
		add: self doSomethingString;
		yourself 

ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 17:44'
simpleTimeoutTestResults

	| things |
	things := OrderedCollection new: self iterationsBeforeTimeout.

	self iterationsBeforeTimeout timesRepeat: [ things add: self  doSomethingString ].
	things add: self doSomethingElseString.

	^ things 

ExceptionTester methodsFor: 'results' stamp: 'brp 10/21/2004 16:52'
simpleTimeoutWithZeroDurationTestResults

	^OrderedCollection new
		add: self doSomethingElseString;
		yourself 


ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/13/1999 01:25'
runAllTests
	"ExceptionTester new runAllTests"

	self
		runBasicTests;
		runBasicANSISignaledExceptionTests 

ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/12/1999 23:54'
runBasicANSISignaledExceptionTests

	self basicANSISignaledExceptionTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector] 

ExceptionTester methodsFor: 'suites' stamp: 'tfei 6/9/1999 16:06'
runBasicTests

	self basicTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector] 


ExceptionTester methodsFor: 'testing' stamp: 'brp 10/21/2004 17:40'
runTest: aSelector

	| actualResult expectedResult |
	[ self 
		logTest: aSelector;
		clearLog;
		perform: aSelector ]
			on: MyTestError do: 
				[ :ex | self log: 'Unhandled Exception'.
					ex return: nil ].

	actualResult	:= self log.
	expectedResult := self perform: (aSelector, #Results) asSymbol.

	actualResult = expectedResult
		ifTrue: [self logTestResult: 'succeeded']
		ifFalse: [self logTestResult: 'failed' ].
 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" Class initializers "


			 
"		E O F 		"
