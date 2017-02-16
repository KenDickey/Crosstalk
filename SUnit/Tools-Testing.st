"FILE: 'Tools-Testing.st'
 IMPLEMENTS: SUnit testing (less the GUI)
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 15 Febuary 2017
"

ActiveModel 
	newSubclassName: #TestRunner 
	iVars: #(result details passFail failures errors tests lastPass
		 selectedFailureTest selectedErrorTest selectedSuite filter
		 selectedSuites running runSemaphore completedTests totalTests
		 progressLabel) 
	cVars: nil.


TestRunner category: #'Tools-Testing'.

TestRunner commentStamp: 
'This is a user interface for the SUnit TestCase and TestSuite classes.
 It lets you run tests in the background, and you can select subsets to run.'.

TestRunner ~> completedFraction
[
  ^completedTests / totalTests 
].

TestRunner ~> errorsList
[
  ^self errors collect: [:error | error printString] 
].

TestRunner ~> failuresList
[
  ^self failures collect: [:failure | failure printString] 
].

TestRunner ~> listSelectionAt: index
[
	^selectedSuites at: index 
].

TestRunner ~> listSelectionAt: index put: aBoolean
[
  ^selectedSuites at: index put: aBoolean
].

TestRunner ~> selectedTests
[
  | retval |
  retval := OrderedCollection new.
  self tests with: self selectedSuites do: [ :str :sel |
	 sel ifTrue: [ retval add: str ]
  ].
  ^retval
].

TestRunner ~> timeSinceLastPassAsString: aResult
[
  (lastPass isNil or: [aResult hasPassed not]) ifTrue: [^ ''].
  ^ ', ' , (DateAndTime now - lastPass) printString , '
since last Pass' 
].

TestRunner ~> runButtonState
[
"Appear 'presed' while running."
  ^ running notNil 
].

TestRunner ~> stopButtonState
[
  ^self runButtonState not
].

TestRunner ~> deselectAll
[
  self selectedSuites: tests collect: [ :ea | false ].
  self selectedSuite: 0.
  self changed: #allSelections.
].  

TestRunner ~> invertSelections
[
  self selectedSuites: selectedSuites collect: [ :ea | ea not ].
  self selectedSuite:  selectedSuites indexOf: true ifAbsent: [0].
  self changed: #allSelections .
].

TestRunner ~> selectAll
[
  | sel |
  sel := self selectedSuite.
  self selectedSuites: self selectedSuites collect: [ :ea | true ].
  self selectedSuites size isZero 
  ifFalse: [
	sel isZero 
		ifTrue:  [ self selectedSuite: 1 ]
 		ifFalse: [ self changed: #allSelections ]
  ].
].

TestRunner ~> setFilter
[
  self  filter: FillInTheBlankMorph
	request: 'Pattern for added test cases (#* OK)' 
	initialAnswer: '*'.
  (self filter endsWith: '*') ifFalse: [ self filter: self filter, '*' ].
  self selectedSuites: (tests asOrderedCollection 
	with: selectedSuites collect: [ :ea :sel |
		sel or: [ filter match: ea asString ]
	]).
  self selectedSuite: self selectedSuites indexOf: true ifAbsent: [0].
  self changed: #allSelections.
].

TestRunner ~> terminateRun
[
  running ifNotNil: [ running terminate. running := nil ].
].

TestRunner ~> addTestsFor: testName toSuite: suite 
[
  | cls |
  (testName indexOf: $() > 0
    ifFalse: [
	cls := SUnitNameResolver classNamed: testName asSymbol.
	cls isAbstract
		ifTrue: [cls allSubclasses
			do: [:each | each isAbstract
				ifFalse: [each addToSuiteFromSelectors: suite]
			]
		]
		ifFalse: [cls addToSuiteFromSelectors: suite]
    ].
  ^ suite 
].

TestRunner ~> debugErrorTest: anInteger
[
  self selectedErrorTest: anInteger.   "added rew"
  self selectedFailureTest: 0.         "added rew"
  self changed: #selectedFailureTest.  "added rew"
  self changed: #selectedErrorTest.    "added rew"
  (anInteger ~= 0)
      ifTrue: [(result errors at: anInteger) debug]
].

TestRunner ~> debugFailureTest: anInteger
[
  (anInteger ~= 0)
     ifTrue: [(self failures at: anInteger) debugAsFailure].

  self selectedFailureTest: anInteger.
  self selectedErrorTest: 0.
  self changed: #selectedErrorTest.
  self changed: #selectedFailureTest.
]. 

TestRunner ~> refreshTests
[
  | preselected |
  self selectedSuite: 0.
  self selectedFailureTest: 0.
  self selectedErrorTest: 0.
  preselected := Set new.
  self tests
	with: selectedSuites
	do: [ :t :f | f ifTrue: [ preselected add: t ]].
  tests := self gatherTestNames.
  self selectedSuites: self tests collect: [ :ea |
 	preselected includes: ea 
  ].
  self changed: #tests.
  self changed: #selectedFailureTest.		"added rew"
  self changed: #selectedErrorTest.			"added rew"
  self changed: #selectedSuite. 
].

TestRunner ~> runOneTest
[
  | testSuite |
  self runWindow.
  self selectedSuite isZero 
	ifTrue: [ ^ self displayPassFail: 'No Test Suite Selected' ].
  self testSuite:  TestSuite new name: 'TestRunner Suite'.
  self addTestsFor: (self tests at: self selectedSuite) toSuite: self testSuite.
  self runSemaphore initSignals.
  self runSuite: testSuite 
].

TestRunner ~> runProfiledTests
[
  | suite |
  suite := TestSuite new name: 'TestRunner Suite'.
  self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
  self runSemaphore initSignals.
  self runSuiteProfiled: suite 
].

TestRunner ~> runSuite: suite
[
  self running ifNotNil: [ ^self inform: 'already running' ].
  suite addDependent: self.
  self totalTests: suite tests size.
  self completedTests: 0.
  self running: [
     [ result := suite run ]
     ensure: [
        self running: nil.
	suite removeDependent: self.
	self runSemaphore signal.
	WorldState addDeferredUIMessage: [
		self updateWindow: result.
	      	self changed: #runTests.
	      	self changed: #runOneTest.
	].
	            ].
  ] newProcess.
  self runWindow.
  self changed: #runTests.
  self changed: #runOneTest.
  self running
	name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
      resume.
]. 

TestRunner ~> runSuiteProfiled: suite
[
  self running ifNotNil: [ ^self inform: 'already running' ].
  suite addDependent: self.
  self totalTests: suite tests size.
  self completedTests: 0.
  running := [
     [ result := MessageTally spyOn: [suite run] ]
     ensure: [
       self running := nil.
       suite removeDependent: self.
       self runSemaphore signal.
       WorldState addDeferredUIMessage: [
	self updateWindow: result.
      	self changed: #runTests.
      	self changed: #runOneTest.
       ].
     ].
  ] newProcess.
  self runWindow.
  self changed: #runTests.
  self changed: #runOneTest.
  self running
	name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
      resume.
]. 

TestRunner ~> runTests
[
  | suite |
  suite := TestSuite new name: 'TestRunner Suite'.
  self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
  self runSemaphore initSignals.
  self runSuite: suite 
].

@@@@ @@@@
TestRunner methodsFor: 'processing' stamp: 'nk 12/15/2002 09:46'
selectedSuite: anInteger
	anInteger > 0 ifTrue: [ | selected |
		selected := selectedSuite ~= anInteger.
		selectedSuites at: anInteger put: selected.
	] ifFalse: [
		"selectedSuite > 0 ifTrue: [ selectedSuites at: selectedSuite put: false ]."
	].
	selectedSuite := anInteger.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	self changed: #selectedFailureTest.             "added rew"
	self changed: #selectedErrorTest.               "added rew" 
	self changed: #selectedSuite.
	self changed: #allSelections.
 



TestRunner methodsFor: 'updating' stamp: 'Sames 4/11/2000 17:38'
displayDetails: aString
        details := aString.
        self changed: #details 

TestRunner methodsFor: 'updating' stamp: 'jp 3/17/2003 13:49'
displayErrors: anOrderedCollection

        errors := anOrderedCollection.
        self changed: #errorsList 

TestRunner methodsFor: 'updating' stamp: 'jp 3/17/2003 10:51'
displayFailures: anOrderedCollection

        failures := anOrderedCollection.
        self changed: #failuresList 

TestRunner methodsFor: 'updating' stamp: 'Sames 4/11/2000 17:36'
displayPassFail: aString
        passFail := aString.
        self changed: #passFail 

TestRunner methodsFor: 'updating' stamp: 'jmv 4/16/2011 14:08'
refreshTR
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: 'N/A'.
	self displayDetails: '...' 

TestRunner methodsFor: 'updating' stamp: 'jmv 4/16/2011 14:28'
runWindow
	
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: 'Running...'.
	self displayDetails: '...' 

TestRunner methodsFor: 'updating' stamp: 'jmv 7/4/2016 22:41'
update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestCase)
		ifTrue: [
			WorldState addDeferredUIMessage: [
				completedTests := completedTests + 1.
				progressLabel := aParameter printString.
				self changed: #progress ]]
		ifFalse: [ super update: aParameter ] 

TestRunner methodsFor: 'updating' stamp: 'jmv 4/9/2010 13:29'
updateDetails: aTestResult 
	self displayDetails: aTestResult printString
			, (self timeSinceLastPassAsString: aTestResult).
	aTestResult hasPassed
		ifTrue: [lastPass := DateAndTime now] 

TestRunner methodsFor: 'updating' stamp: 'jp 3/17/2003 13:49'
updateErrors: aTestResult

        self displayErrors: aTestResult errors 

TestRunner methodsFor: 'updating' stamp: 'jmv 5/27/2014 22:59'
updateFailures: aTestResult

        self displayFailures: aTestResult failures 

TestRunner methodsFor: 'updating' stamp: 'Sames 4/11/2000 18:27'
updatePassFail: aTestResult
        | message |
        message := aTestResult hasPassed
                                ifTrue: ['Pass']
                                ifFalse: ['Fail'].
        self displayPassFail: message 

TestRunner methodsFor: 'updating' stamp: 'jmv 4/16/2011 14:17'
updateWindow: aTestResult
	self updatePassFail: aTestResult.
	self updateDetails: aTestResult.
	self updateFailures: aTestResult.
	self updateErrors: aTestResult 


TestRunner methodsFor: 'initialization' stamp: 'jmv 8/23/2010 09:56'
gatherTestNames
	^ (self testCases collect: [ :each |
		each name ]) asArray sort. 

TestRunner methodsFor: 'initialization' stamp: 'nk 8/6/2003 09:02'
initialize

	result := TestResult new.
	passFail := 'N/A'.
	details := '...'.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	tests := self gatherTestNames.
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	selectedSuites := tests collect: [:ea | true].
	running := nil.
	runSemaphore := Semaphore new 

TestRunner methodsFor: 'initialization' stamp: 'cwp 2/5/2004 21:35'
testCases
	^ TestCase allSubclasses reject: [:cls | cls isAbstract] 


TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 09:44'
errorLog
	^Transcript 

TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'
showResult

	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.
	self
		showResultSummary;
		showResultDefects.
	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine. 

TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'
showResultDefects

	(self result failureCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ----- Failures ----'.
			self result failures do: [:failure |
				self errorLog newLine; tab; show: failure printString]].
	(self result errorCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ------- Errors ----'.
			self result errors do: [:error |
				self errorLog newLine; tab; show: error printString]]. 

TestRunner methodsFor: 'logging' stamp: 'jmv 3/27/2015 11:40'
showResultSummary

	| message summary |
	message := (self result runCount = self result correctCount)
		ifTrue: ['succeeded']
		ifFalse: ['failed'].
	Transcript newLine; tab; show: message.
	summary :=
		self result runCount printString, ' run, ',
		self result failureCount printString, ' failed, ',
		self result errorCount printString, ' errors'.
	Transcript newLine; tab; show: summary. 


classDefinition: #TestFailure category: #'Tools-Testing'
Exception subclass: #TestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'
TestFailure commentStamp: '<historical>' prior: 0
Signaled in case of a failed test (failure). The test framework distinguishes between failures and errors. A failure is anticipated and checked for with assertions. Errors are unanticipated problems like a division by 0 or an index out of bounds ...


TestFailure methodsFor: 'Camp Smalltalk' stamp: 'ajh 1/24/2003 19:23'
defaultAction

	Processor activeProcess
		debug: self signalerContext
		title: self description 

TestFailure methodsFor: 'Camp Smalltalk' stamp: 'ajh 2/1/2003 00:58'
isResumable
	
	^ false 


classDefinition: #ResumableTestFailure category: #'Tools-Testing'
TestFailure subclass: #ResumableTestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'
ResumableTestFailure commentStamp: '<historical>' prior: 0
A ResumableTestFailure triggers a TestFailure, but lets execution of the TestCase continue. this is useful when iterating through collections, and #assert: ing on each element. in combination with methods like testcase>>#assert:description:, this lets you run through a whole collection and note which tests pass.

here''s an example:

	

	(1 to: 30) do: [ :each |
		self assert: each odd description: each printString, ' is even' resumable: true]

for each element where #odd returns <false>, the element will be printed to the Transcript. 


ResumableTestFailure methodsFor: 'Camp Smalltalk'
isResumable
	"Of course a ResumableTestFailure is resumable ;-)"

	^true 

ResumableTestFailure methodsFor: 'Camp Smalltalk'
sunitExitWith: aValue
	self resume: aValue 


classDefinition: #SUnitNameResolver category: #'Tools-Testing'
Object subclass: #SUnitNameResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'SUnitNameResolver class' category: #'Tools-Testing'
SUnitNameResolver class
	instanceVariableNames: ''

SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jmv 3/2/2010 10:06'
classNamed: aSymbol

        ^Smalltalk
                at: aSymbol
                ifAbsent: nil 

SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'
errorObject
	^Error 

SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'
mnuExceptionObject
	^MessageNotUnderstood new 

SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'
notificationObject
	^Notification new 


classDefinition: #TestCase category: #'Tools-Testing'
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'
TestCase commentStamp: '<historical>' prior: 0
A TestCase is a Command representing the future running of a test case. Create one with the class method #selector: aSymbol, passing the name of the method to be run when the test case runs.

When you discover a new fixture, subclass TestCase, declare instance variables for the objects in the fixture, override #setUp to initialize the variables, and possibly override# tearDown to deallocate any external resources allocated in #setUp.

When you are writing a test case method, send #assert: aBoolean when you want to check for an expected value. For example, you might say "self assert: socket isOpen" to test whether or not a socket is open at a point in a test.


TestCase methodsFor: 'Dependencies'
addDependentToHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			 

TestCase methodsFor: 'Dependencies'
removeDependentFromHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			 


TestCase methodsFor: 'Accessing' stamp: 'jmv 5/10/2015 15:29'
assert: aBoolean description: aStringOrBlock
	aBoolean ifFalse: [
		failureString := aStringOrBlock value.
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]
			 

TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:41'
assert: aBoolean description: aString resumable: resumableBoolean 
	| exception |
	aBoolean
		ifFalse: [		
			failureString := aString.
			self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]
			 

TestCase methodsFor: 'Accessing'
deny: aBoolean

	self assert: aBoolean not
			 

TestCase methodsFor: 'Accessing'
deny: aBoolean description: aString
	self assert: aBoolean not description: aString
			 

TestCase methodsFor: 'Accessing'
deny: aBoolean description: aString resumable: resumableBoolean 
	self
		assert: aBoolean not
		description: aString
		resumable: resumableBoolean
			 

TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:45'
errored: anError
	raisedError := anError 

TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:48'
failureString
	^failureString 

TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:49'
raisedError
	^raisedError 

TestCase methodsFor: 'Accessing'
resources
	| allResources resourceQueue |
	allResources := Set new.
	resourceQueue := OrderedCollection new.
	resourceQueue addAll: self class resources.
	[resourceQueue isEmpty] whileFalse: [
		| next |
		next := resourceQueue removeFirst.
		allResources add: next.
		resourceQueue addAll: next resources].
	^allResources
			 

TestCase methodsFor: 'Accessing'
selector
	^testSelector
			 

TestCase methodsFor: 'Accessing'
should: aBlock
	self assert: aBlock value
			 

TestCase methodsFor: 'Accessing'
should: aBlock description: aString
	self assert: aBlock value description: aString
			 

TestCase methodsFor: 'Accessing'
should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
			 

TestCase methodsFor: 'Accessing'
should: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString
			 

TestCase methodsFor: 'Accessing'
shouldnt: aBlock
	self deny: aBlock value
			 

TestCase methodsFor: 'Accessing'
shouldnt: aBlock description: aString
	self deny: aBlock value description: aString
			 

TestCase methodsFor: 'Accessing'
shouldnt: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
			 

TestCase methodsFor: 'Accessing'
shouldnt: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not 		description: aString
			 

TestCase methodsFor: 'Accessing' stamp: 'jmv 3/27/2015 10:42'
signalFailure: aString

	failureString := aString.
	TestResult failure sunitSignalWith: aString 


TestCase methodsFor: 'Running' stamp: 'jmv 12/29/2009 10:42'
debug
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[(self class selector: testSelector) runCase] 
		ensure: [self resources do: [:each | each reset]]
			 

TestCase methodsFor: 'Running' stamp: 'nk 1/22/2004 22:49'
debugAsFailure
	| semaphore |
	semaphore := Semaphore new.
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[semaphore wait. 
	self tearDown.
	self resources do: [:each | each reset]] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore. 

TestCase methodsFor: 'Running' stamp: 'jmv 3/27/2015 09:44'
failureLog	
	^Transcript	 

TestCase methodsFor: 'Running' stamp: 'jmv 1/3/2013 16:28'
isLogging
	"Log to Transcript"
	^true 

TestCase methodsFor: 'Running' stamp: 'jmv 1/3/2013 16:33'
logFailure: aString
	| log |
	self isLogging ifTrue: [
		log := self failureLog.
		log newLine.
		self printOn: log.
		log nextPutAll: ' -- '.
		log nextPutAll: aString ] 

TestCase methodsFor: 'Running' stamp: 'jmv 2/2/2014 00:44'
openDebuggerOnFailingTestMethod
	| guineaPig context compiledMethod debugger |
	compiledMethod := self class lookupSelector: self selector.
	guineaPig := [ self performTest ] newProcess.
	context := guineaPig suspendedContext.
	debugger := Debugger new
		process: guineaPig
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.
	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
 

TestCase methodsFor: 'Running' stamp: 'jmv 2/2/2014 00:44'
openDebuggerOnFailingTestMethod: semaphore
	| guineaPig context compiledMethod debugger |
	compiledMethod := self class lookupSelector: self selector.
	guineaPig := [ self performTest ] newProcess.
	context := guineaPig suspendedContext.
	debugger := TestCaseDebugger new
		process: guineaPig
		context: context.
	debugger doneSemaphore: semaphore.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.
	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
 

TestCase methodsFor: 'Running'
run
	| result |
	result := TestResult new.
	self run: result.
	^result
			 

TestCase methodsFor: 'Running' stamp: 'jmv 5/13/2012 23:05'
run: aResult
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ]
			 

TestCase methodsFor: 'Running' stamp: 'jmv 12/29/2009 10:41'
runCase

	[
		self setUp.
		self performTest] ensure: [self tearDown]
			 

TestCase methodsFor: 'Running' stamp: 'nk 1/22/2004 22:50'
runCaseAsFailure: aSemaphore
	self setUp.
	self openDebuggerOnFailingTestMethod: aSemaphore. 

TestCase methodsFor: 'Running'
setUp
			 

TestCase methodsFor: 'Running'
tearDown
			 


TestCase methodsFor: 'Private' stamp: 'jmv 12/29/2009 10:41'
executeShould: aBlock inScopeOf: anExceptionalEvent 
	^[
		aBlock value.
 		false]
			on: anExceptionalEvent
			do: [:ex | ex sunitExitWith: true]
			 

TestCase methodsFor: 'Private' stamp: 'jmv 2/2/2010 09:23'
performTest

	self perform: testSelector asSymbol
			 

TestCase methodsFor: 'Private'
setTestSelector: aSymbol
	testSelector := aSymbol
			 


TestCase methodsFor: 'Printing' stamp: 'jmv 9/22/2009 09:00'
printOn: aStream

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: (testSelector ifNil: [ 'unknown' ])
			 


TestCase methodsFor: 'accessing' stamp: 'md 8/2/2006 10:59'
assert: aBooleanOrBlock

	aBooleanOrBlock value ifFalse: [self signalFailure: 'Assertion failed']
			 

TestCase methodsFor: 'accessing' stamp: 'dc 4/2/2007 18:38'
assert: expected equals: actual
	^ self
		assert: (expected = actual)
		description: (self comparingStringBetween: expected and: actual)
 


TestCase methodsFor: 'private' stamp: 'jmv 8/10/2010 07:33'
comparingStringBetween: expected and: actual
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Expected ';
			nextPutAll: (expected printStringLimitedTo: 50);
			nextPutAll: ' but was ';
			nextPutAll: (actual printStringLimitedTo: 50);
			nextPutAll: '.'
		] 


TestCase methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:41'
is: aSymbol
	^aSymbol == #TestCase or: [ super is: aSymbol ] 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'TestCase class' category: #'Tools-Testing'
TestCase class
	instanceVariableNames: ''

TestCase class methodsFor: 'Accessing'
resources

	^#()
			 

TestCase class methodsFor: 'Accessing'
sunitVersion
	^'3.1'
			 

TestCase class methodsFor: 'Accessing' stamp: 'jmv 2/2/2010 09:23'
testSelectors

	^(self selectors asArray select: [ :each | 
		(each beginsWith: 'test') and: [ each numArgs isZero ] ]) sort 


TestCase class methodsFor: 'Building Suites' stamp: 'nk 4/21/2002 10:59'
addTestsFor: classNameString toSuite: suite

	| cls  |
	cls := Smalltalk at: classNameString ifAbsent: [ ^suite ].
	^cls isAbstract 
		ifTrue:  [
			cls allSubclasses do: [ :each |
				each isAbstract ifFalse: [
					each addToSuiteFromSelectors: suite ] ].
			suite]
		ifFalse: [ cls addToSuiteFromSelectors: suite ]
 

TestCase class methodsFor: 'Building Suites' stamp: 'nk 4/21/2002 10:51'
addToSuite: suite fromMethods: testMethods 
	testMethods do:  [ :selector | 
			suite addTest: (self selector: selector) ].
	^suite 

TestCase class methodsFor: 'Building Suites' stamp: 'nk 4/21/2002 16:37'
addToSuiteFromSelectors: suite
	^self addToSuite: suite fromMethods: (self shouldInheritSelectors
		ifTrue: [ self allTestSelectors ]
		ifFalse: [self testSelectors ]) 

TestCase class methodsFor: 'Building Suites' stamp: 'nk 12/23/2002 07:40'
buildSuite
	| suite |
	suite := TestSuite new.
	^ self isAbstract
		ifTrue: [
			suite name: self name asString.
			self allSubclasses
				do: [:each | each isAbstract
						ifFalse: [each addToSuiteFromSelectors: suite]].
			suite]
		ifFalse: [self addToSuiteFromSelectors: suite] 

TestCase class methodsFor: 'Building Suites'
buildSuiteFromAllSelectors

	^self buildSuiteFromMethods: self allTestSelectors
			 

TestCase class methodsFor: 'Building Suites'
buildSuiteFromLocalSelectors

	^self buildSuiteFromMethods: self testSelectors
			 

TestCase class methodsFor: 'Building Suites' stamp: 'nk 4/21/2002 10:52'
buildSuiteFromMethods: testMethods 
	| suite |
	suite := (TestSuite new)
				name: self name asString;
				yourself.
	^self addToSuite: suite fromMethods: testMethods 

TestCase class methodsFor: 'Building Suites'
buildSuiteFromSelectors

	^self shouldInheritSelectors
		ifTrue: [self buildSuiteFromAllSelectors]
		ifFalse: [self buildSuiteFromLocalSelectors]
			 

TestCase class methodsFor: 'Building Suites'
suiteClass
	^TestSuite
			 


TestCase class methodsFor: 'Instance Creation'
debug: aSymbol

	^(self selector: aSymbol) debug
			 

TestCase class methodsFor: 'Instance Creation'
run: aSymbol

	^(self selector: aSymbol) run
			 

TestCase class methodsFor: 'Instance Creation'
selector: aSymbol

	^self new setTestSelector: aSymbol
			 


TestCase class methodsFor: 'Testing' stamp: 'HernanWilkinson 1/10/2017 16:29:48'
is: aSymbol

	^aSymbol == #TestCaseClass or: [ super is: aSymbol ] 

TestCase class methodsFor: 'Testing' stamp: 'jmv 2/2/2010 09:27'
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestCase
			 

TestCase class methodsFor: 'Testing'
shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."

	^self superclass isAbstract
		or: [self testSelectors isEmpty]

"$QA Ignore:Sends system method(superclass)$"
			 


TestCase class methodsFor: 'accessing' stamp: 'ul 12/4/2009 03:44'
allTestSelectors

	^(self allSelectors asArray select: [ :each | 
		(each beginsWith: 'test') and: [ each numArgs isZero ] ]) sort
			 


classDefinition: #TestResource category: #'Tools-Testing'
Object subclass: #TestResource
	instanceVariableNames: 'name description'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'

TestResource methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:40'
description

	^description ifNil: [ '' ] 

TestResource methodsFor: 'Accessing'
description: aString

	description := aString
			 

TestResource methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:40'
name

	^name ifNil: [ self printString] 

TestResource methodsFor: 'Accessing'
name: aString

	name := aString
			 

TestResource methodsFor: 'Accessing'
resources
	^self class resources
			 


TestResource methodsFor: 'Init / Release'
initialize
	self setUp

			 


TestResource methodsFor: 'Testing'
isAvailable
	"override to provide information on the
	readiness of the resource"
	
	^true
			 

TestResource methodsFor: 'Testing'
isUnavailable
	"override to provide information on the
	readiness of the resource"
	
	^self isAvailable not
			 


TestResource methodsFor: 'Printing'
printOn: aStream

	aStream nextPutAll: self class printString
			 


TestResource methodsFor: 'Running'
setUp
	"Does nothing. Subclasses should override this
	to initialize their resource"
			 

TestResource methodsFor: 'Running'
signalInitializationError
	^self class signalInitializationError
			 

TestResource methodsFor: 'Running'
tearDown
	"Does nothing. Subclasses should override this
	to tear down their resource"
			 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'TestResource class' category: #'Tools-Testing'
TestResource class
	instanceVariableNames: 'current'

TestResource class methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:40'
current

	current
		ifNil: [ current := self new].

	^current
			 

TestResource class methodsFor: 'Accessing'
current: aTestResource

	current := aTestResource
			 

TestResource class methodsFor: 'Accessing'
resources
	^#()
			 


TestResource class methodsFor: 'Testing' stamp: 'jmv 2/2/2010 09:27'
isAbstract
	"Override to true if a TestResource subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestResource
			 

TestResource class methodsFor: 'Testing'
isAvailable
	^self current notNil and: [self current isAvailable]
			 

TestResource class methodsFor: 'Testing'
isUnavailable

	^self isAvailable not
			 


TestResource class methodsFor: 'Creation' stamp: 'jmv 3/2/2010 17:20'
reset

	current ifNotNil: [
		[current tearDown] ensure: [
			current := nil]]
			 

TestResource class methodsFor: 'Creation'
signalInitializationError
	^TestResult signalErrorWith: 'Resource ' , self name , ' could not be initialized'
			 


classDefinition: #TestResult category: #'Tools-Testing'
Object subclass: #TestResult
	instanceVariableNames: 'failures errors passed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'
TestResult commentStamp: '<historical>' prior: 0
This is a Collecting Parameter for the running of a bunch of tests. TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. Kent has seen TestResults that recorded coverage information and that sent email when they were done.


TestResult methodsFor: 'Accessing'
correctCount
	"depreciated - use #passedCount"

	^self passedCount
			 

TestResult methodsFor: 'Accessing'
defects
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself
			 

TestResult methodsFor: 'Accessing'
errorCount

	^self errors size
			 

TestResult methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:41'
errors

	errors
		ifNil: [errors := OrderedCollection new].
	^errors
			 

TestResult methodsFor: 'Accessing'
failureCount

	^self failures size
			 

TestResult methodsFor: 'Accessing' stamp: 'jmv 5/27/2014 22:57'
failures
	failures
		ifNil: [failures := OrderedCollection new].
	^failures
			 

TestResult methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:41'
passed

	passed
		ifNil: [passed := OrderedCollection new].

	^passed
			 

TestResult methodsFor: 'Accessing'
passedCount

	^self passed size
			 

TestResult methodsFor: 'Accessing'
runCount

	^self passedCount + self failureCount + self errorCount
			 

TestResult methodsFor: 'Accessing'
tests

	^(OrderedCollection new: self runCount)
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself
			 


TestResult methodsFor: 'Testing'
hasErrors

	^self errors size > 0
			 

TestResult methodsFor: 'Testing'
hasFailures

	^self failures size > 0
			 

TestResult methodsFor: 'Testing'
hasPassed

	^self hasErrors not and: [self hasFailures not]
			 

TestResult methodsFor: 'Testing'
isError: aTestCase

	^self errors includes: aTestCase
			 

TestResult methodsFor: 'Testing'
isFailure: aTestCase
	^self failures includes: aTestCase
			 

TestResult methodsFor: 'Testing'
isPassed: aTestCase

	^self passed includes: aTestCase
			 


TestResult methodsFor: 'Printing'
printOn: aStream

	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self correctCount printString;
		nextPutAll: ' passed, ';
		nextPutAll: self failureCount printString;
		nextPutAll: ' failed, ';
		nextPutAll: self errorCount printString;
		nextPutAll: ' error'.

	self errorCount ~= 1
		ifTrue: [aStream nextPut: $s]
			 


TestResult methodsFor: 'Running' stamp: 'jmv 3/27/2015 10:45'
runCase: aTestCase

	| testCasePassed |

	testCasePassed :=
		[
			[
				aTestCase runCase.
				true]
					on: self class failure
					do: [ :signal |
						(self failures isEmpty or: [ (failures last == aTestCase) not ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: self class error
							do: [ :signal |
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ] 



TestResult methodsFor: 'Inspecting' stamp: 'HernanWilkinson 1/10/2017 16:33:03'
forDebuggingAndInspection

	^TestResultForDebuggingAndInspection on: self 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'TestResult class' category: #'Tools-Testing'
TestResult class
	instanceVariableNames: ''

TestResult class methodsFor: 'Exceptions'
error
	^self exError
			 

TestResult class methodsFor: 'Exceptions'
exError
	^SUnitNameResolver errorObject
			 

TestResult class methodsFor: 'Exceptions'
failure
	^TestFailure
			 

TestResult class methodsFor: 'Exceptions'
resumableFailure
	^ResumableTestFailure
			 

TestResult class methodsFor: 'Exceptions'
signalErrorWith: aString 
	self error sunitSignalWith: aString
			 

TestResult class methodsFor: 'Exceptions'
signalFailureWith: aString 
	self failure sunitSignalWith: aString
			 


classDefinition: #TestResultForDebuggingAndInspection category: #'Tools-Testing'
Object subclass: #TestResultForDebuggingAndInspection
	instanceVariableNames: 'testResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'

TestResultForDebuggingAndInspection methodsFor: 'initialization' stamp: 'HernanWilkinson 1/10/2017 16:34:56'
initializeOn: aTestResult

	testResult := aTestResult 


TestResultForDebuggingAndInspection methodsFor: 'printing' stamp: 'HernanWilkinson 1/10/2017 17:49:59'
print: aCollectionOfTestCases startingWith: header on: aStream
	
	aCollectionOfTestCases
		do: [ :aTestCase |
			aStream 
				nextPutAll: header;
				space;
				nextPutAll: aTestCase class name;
				nextPutAll: ' debug: #';
				nextPutAll: aTestCase selector ]
		separatedBy: [ aStream newLine ].
		
	 

TestResultForDebuggingAndInspection methodsFor: 'printing' stamp: 'HernanWilkinson 1/10/2017 17:49:05'
printOn: aStream

	aStream print: testResult.
	aStream newLine.
	
	self print: testResult errors startingWith: '"E"' on: aStream.
	self print: testResult failures startingWith: '"F"' on: aStream.
	 


TestResultForDebuggingAndInspection methodsFor: 'running' stamp: 'HernanWilkinson 1/10/2017 18:14:09'
reRun

	| suite |
	 
	suite := TestSuite new.
	suite addTests: testResult tests.
	
	testResult := suite run. 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'TestResultForDebuggingAndInspection class' category: #'Tools-Testing'
TestResultForDebuggingAndInspection class
	instanceVariableNames: ''

TestResultForDebuggingAndInspection class methodsFor: 'instance creation' stamp: 'HernanWilkinson 1/10/2017 16:34:28'
on: aTestResult

	^self new initializeOn: aTestResult 


classDefinition: #TestSuite category: #'Tools-Testing'
Object subclass: #TestSuite
	instanceVariableNames: 'tests resources name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'
TestSuite commentStamp: '<historical>' prior: 0
This is a Composite of Tests, either TestCases or other TestSuites. The common protocol is #run: aTestResult and the dependencies protocol


TestSuite methodsFor: 'Dependencies' stamp: 'jmv 2/2/2010 09:25'
addDependentToHierachy: anObject
	self addDependent: anObject.
	self tests do: [ :each | each addDependentToHierachy: anObject] 

TestSuite methodsFor: 'Dependencies' stamp: 'jmv 2/2/2010 09:25'
removeDependentFromHierachy: anObject
	self removeDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]
			 


TestSuite methodsFor: 'Accessing'
addTest: aTest
	self tests add: aTest
			 

TestSuite methodsFor: 'Accessing'
addTests: aCollection 
	aCollection do: [:eachTest | self addTest: eachTest]
			 

TestSuite methodsFor: 'Accessing'
defaultResources
	^self tests 
		inject: Set new
		into: [:coll :testCase | 
			coll
				addAll: testCase resources;
				yourself]
			 

TestSuite methodsFor: 'Accessing'
name

	^name
			 

TestSuite methodsFor: 'Accessing'
name: aString

	name := aString
			 

TestSuite methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:41'
resources
	resources ifNil: [resources := self defaultResources].
	^resources
			 

TestSuite methodsFor: 'Accessing'
resources: anObject
	resources := anObject
			 

TestSuite methodsFor: 'Accessing' stamp: 'jmv 3/2/2010 16:41'
tests
	tests ifNil: [tests := OrderedCollection new].
	^tests
			 


TestSuite methodsFor: 'Running' stamp: 'jmv 12/29/2009 10:42'
run
	| result |
 	result := TestResult new.
	self resources do: [ :res |
		res isAvailable ifFalse: [^res signalInitializationError]].
	[self run: result] ensure: [self resources do: [:each | each reset]].
	^result
			 

TestSuite methodsFor: 'Running' stamp: 'jmv 2/2/2010 09:25'
run: aResult 
	self tests do: [:each | 
		self changed: each.
		each run: aResult]
			 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

classDefinition: 'TestSuite class' category: #'Tools-Testing'
TestSuite class
	instanceVariableNames: ''

TestSuite class methodsFor: 'Creation'
named: aString

	^self new
		name: aString;
		yourself

			 
"		E O F 		"
