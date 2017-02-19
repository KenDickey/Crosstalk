"FILE: 'SUnit.st'
 IMPLEMENTS: SUnit testing (less the GUI)
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 18 Febuary 2017
"

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Exception newSubclassName: #TestFailure iVars: #() cVars: #().

TestFailure newSubclassName: #ResumableTestFailure iVars: #() cVars: #().

TestFailure category: #'Tools-Testing'.
ResumableTestFailure category: #'Tools-Testing'.

TestFailure comment:
'Signaled in case of a failed test (failure).
 The test framework distinguishes between failures and errors.\

 A failure is anticipated and checked for with assertions.
 Errors are unanticipated problems like a division by 0
 or an index out of bounds ...'.

TestFailure ~> defaultAction
[
	Processor activeProcess
		debug: self signalerContext
		title: self description 
].

TestFailure ~> isResumable
[
  ^ false 
].

ResumableTestFailure comment: 
'A ResumableTestFailure triggers a TestFailure,
 but lets execution of the TestCase continue.
 This is useful when iterating through collections, and #assert: ing
 on each element. 
 In combination with methods like testcase>>#assert:description:,
 this lets you run through a whole collection and note which tests pass.

here''s an example:

  (1 to: 30) do: [ :each |
    self assert: each odd description: each printString, 
	'' is even'' resumable: true]

 for each element where #odd returns <false>,
 the element will be printed to the Transcript. '.

ResumableTestFailure ~> isResumable
[
"Of course a ResumableTestFailure is resumable ;-)"
  ^ true 
].

ResumableTestFailure ~> sunitExitWith: aValue
[
  self resume: aValue 
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object newSubclassName: #SUnitNameResolver iVars: #() cVars: #().

SUnitNameResolver category: #'Tools-Testing'.


SUnitNameResolver class ~> classNamed: aSymbol
[
  ^ Smalltalk
        at: aSymbol
        ifAbsent: nil 
].

SUnitNameResolver class ~> errorObject
[
  ^ Error 
].

SUnitNameResolver class ~> mnuExceptionObject
[
  ^ MessageNotUnderstood new 
].

SUnitNameResolver class ~> notificationObject
[
  ^ Notification new 
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object  newSubclassName: #TestCase 
	iVars: #(testSelector failureString raisedError) 
	cVars: #().


TestCase category: #'Tools-Testing'.

TestCase comment:
'A TestCase is a Command representing the future running of a test case.
 Create one with the class method #selector: aSymbol,
 passing the name of the method to be run when the test case runs.

When you discover a new fixture, subclass TestCase,
 declare instance variables for the objects in the fixture,
 override #setUp to initialize the variables, and possibly
 override #tearDown to deallocate any external resources allocated in #setUp.

When you are writing a test case method, 
send #assert: aBoolean when you want to check for an expected value. 
For example, you might say "self assert: socket isOpen" to test
 whether or not a socket is open at a point in a test.'.

TestCase ~> addDependentToHierachy: anObject 
[
 "an empty method. for Composite compability with TestSuite"
].

TestCase ~> removeDependentFromHierachy: anObject 
[
 "an empty method. for Composite compability with TestSuite"
].

TestCase ~> assert: aBoolean description: aStringOrBlock
[
  aBoolean ifFalse: [ | failureReason |
	failureReason := aStringOrBlock value.
	self failureString: failureReason.
	self logFailure: failureReason.
	TestResult failure sunitSignalWith: failureReason
  ]
].			 

TestCase ~> assert: aBoolean description: aString resumable: resumableBoolean 
[
  | exception |
  aBoolean
	ifFalse: [		
		self failureString: aString.
		self logFailure: aString.
		exception := resumableBoolean
			ifTrue: [TestResult resumableFailure]
			ifFalse: [TestResult failure].
		exception sunitSignalWith: aString
	]
].		 

TestCase ~> deny: aBoolean
[
  self assert: aBoolean not
].			 

TestCase ~> deny: aBoolean description: aString
[
  self assert: aBoolean not description: aString
].
			 
TestCase ~> 
  deny: aBoolean 
  description: aString 
  resumable: resumableBoolean 
[
  self
	assert: aBoolean not
	description: aString
	resumable: resumableBoolean
].			 

TestCase ~> errored: anError
[
  self raisedError: anError 
].

TestCase ~> resources
[
  | allResources resourceQueue |
  allResources := Set new.
  resourceQueue := OrderedCollection new.
  resourceQueue addAll: self class resources.
  [resourceQueue isEmpty] whileFalse: [
	| next |
	next := resourceQueue removeFirst.
	allResources add: next.
	resourceQueue addAll: next resources
  ].
  ^ allResources
].			 

TestCase ~> should: aBlock
[
  self assert: aBlock value
].			 

TestCase ~> should: aBlock description: aString
[
  self assert: aBlock value description: aString
].
			 
TestCase ~> should: aBlock raise: anExceptionalEvent 
[
  ^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
].			 

TestCase ~> should: aBlock raise: anExceptionalEvent description: aString 
[
  ^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
	description: aString
].			 

TestCase ~> shouldnt: aBlock
[
  self deny: aBlock value
].			 

TestCase ~> shouldnt: aBlock description: aString
[
  self deny: aBlock value description: aString
].			 

TestCase ~> shouldnt: aBlock raise: anExceptionalEvent 
[
  ^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
].
			 

TestCase ~> shouldnt: aBlock raise: anExceptionalEvent description: aString 
[
  ^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not 		   description: aString
].
			 
TestCase ~> signalFailure: aString
[
  self failureString: aString.
  TestResult failure sunitSignalWith: aString 
].

TestCase ~> debug
[
  self resources do: [:res | 
	res isAvailable ifFalse: [^res signalInitializationError]
  ].
  [ (self class selector: testSelector) runCase ] 
	ensure: [self resources do: [:each | each reset]]
].			 

TestCase ~> debugAsFailure
[
  | semaphore |
  semaphore := Semaphore new.
  self resources do: [:res | 
	res isAvailable ifFalse: [^res signalInitializationError]
  ].
  [semaphore wait. 
   self tearDown.
   self resources do: [:each | each reset]] fork.
  (self class selector: testSelector) runCaseAsFailure: semaphore. 
].

TestCase ~> failureLog	
[
  ^Transcript	 
].

TestCase ~> isLogging
[
"Log to Transcript"
  ^true 
].

TestCase ~> logFailure: aString
[
  | log |
  self isLogging ifTrue: [
	log := self failureLog.
	log newLine.
	self printOn: log.
	log nextPutAll: ' -- '.
	log nextPutAll: aString 
  ] 
].

TestCase ~> openDebuggerOnFailingTestMethod
[
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
]. 

TestCase ~> openDebuggerOnFailingTestMethod: semaphore
[
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
]. 

TestCase ~> run
[
  | result |
  result := TestResult new.
  self run: result.
  ^result
].

TestCase ~> run: aResult
[
 ChangeSet
	runningTest: self printString
	do: [ aResult runCase: self ]
].

TestCase ~> runCase
[
  [ self setUp.
    self performTest] ensure: [self tearDown]
].
			 

TestCase ~> runCaseAsFailure: aSemaphore
[
  self setUp.
  self openDebuggerOnFailingTestMethod: aSemaphore. 
].

TestCase ~> setUp
[]. "default is no action"
			 
TestCase ~> tearDown
[]. "default is no action"
			 
TestCase ~> executeShould: aBlock inScopeOf: anExceptionalEvent 
[
  ^[ aBlock value.
     false ]
	on: anExceptionalEvent
	do: [:ex | ex sunitExitWith: true]
].		 

TestCase ~> performTest
[
  self perform: self testSelector asSymbol
].			 

TestCase ~> setTestSelector: aSymbol
[
  self testSelector: aSymbol
].
			 
TestCase ~> printOn: aStream
[
  aStream
	nextPutAll: self class printString;
	nextPutAll: '>>#';
	nextPutAll: (self testSelector ifNil: [ 'unknown' ])
].			 

TestCase ~> assert: aBooleanOrBlock
[
  aBooleanOrBlock value ifFalse: [self signalFailure: 'Assertion failed']
].			 

TestCase ~> assert: expected equals: actual
[
  ^ self
	assert: (expected = actual)
	description: (self comparingStringBetween: expected and: actual)
]. 

TestCase ~> comparingStringBetween: expected and: actual
[
  ^ String streamContents: [:stream |
     stream
	nextPutAll: 'Expected ';
	nextPutAll: (expected printStringLimitedTo: 50);
	nextPutAll: ' but was ';
	nextPutAll: (actual printStringLimitedTo: 50);
	nextPutAll: '.'
  ] 
].


TestCase ~> is: aSymbol
[
  ^aSymbol == #TestCase or: [ super is: aSymbol ] 
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TestCase class ~> resources
[
  ^#() "base case"
].			 

TestCase class ~> sunitVersion
[
  ^ '3.1'
].			 

TestCase class ~> testSelectors
[
  ^(self selectors asArray 
	select: [ :each | 
 	  (each beginsWith: 'test') and: [ each numArgs isZero ] 
	])
    sort
].

TestCase class ~> addTestsFor: classNameString toSuite: suite
[
  | cls  |
  cls := Smalltalk at: classNameString ifAbsent: [ ^suite ].
  ^cls isAbstract 
	ifTrue:  [
		cls allSubclasses do: [ :each |
			each isAbstract ifFalse: [
				each addToSuiteFromSelectors: suite ] ].
		suite]
	ifFalse: [ cls addToSuiteFromSelectors: suite ]
]. 

TestCase class ~> addToSuite: suite fromMethods: testMethods 
[
  testMethods do:  [ :selector | 
		suite addTest: (self selector: selector) 
  ].
^suite 
].

TestCase class ~> addToSuiteFromSelectors: suite
[
  ^self addToSuite: suite 
	fromMethods: (self shouldInheritSelectors
		ifTrue:  [self allTestSelectors]
		ifFalse: [self testSelectors] ) 
].

TestCase class ~> buildSuite
[
  | suite |
  suite := TestSuite new.
  ^ self isAbstract
	ifTrue: [
		suite name: self name asString.
		self allSubclasses do: [:each | 
			(each isAbstract)
			  ifFalse: [each addToSuiteFromSelectors: suite]].
		suite]
	ifFalse: [self addToSuiteFromSelectors: suite] 
].

TestCase class ~> buildSuiteFromAllSelectors
[
  ^self buildSuiteFromMethods: self allTestSelectors
].			 

TestCase class ~> buildSuiteFromLocalSelectors
[
  ^self buildSuiteFromMethods: self testSelectors
].			 

TestCase class ~> buildSuiteFromMethods: testMethods 
[
  | suite |
  suite := (TestSuite new)
	name: self name asString;
	yourself.
  ^self addToSuite: suite fromMethods: testMethods 
].

TestCase class ~> buildSuiteFromSelectors
[
  ^self shouldInheritSelectors
	ifTrue:  [self buildSuiteFromAllSelectors]
	ifFalse: [self buildSuiteFromLocalSelectors]
].			 

TestCase class ~> suiteClass
[
  ^TestSuite
].			 


TestCase class ~> debug: aSymbol
[
  ^(self selector: aSymbol) debug
].			 

TestCase class ~> run: aSymbol
[
  ^(self selector: aSymbol) run
].			 

TestCase class ~> selector: aSymbol
[
  ^self new setTestSelector: aSymbol
].			 


TestCase class ~> is: aSymbol
[
  ^aSymbol == #TestCaseClass or: [ super is: aSymbol ] 
].

TestCase class ~> isAbstract
[
"Override to true if a TestCase subclass is Abstract and should not have
 TestCase instances built from it"

  ^self name = #TestCase
].
			 

TestCase class ~> shouldInheritSelectors
[
"I should inherit from an Abstract superclass but not from a concrete one by default,
 unless I have no testSelectors in which case I must be expecting
 to inherit them from my superclass. 
 If a test case with selectors wants to inherit selectors from a concrete superclass,
 override this to true in that subclass."

  ^self superclass isAbstract
	or: [self testSelectors isEmpty]

"$QA Ignore:Sends system method(superclass)$"
].			 

TestCase class ~> allTestSelectors
[
  ^(self allSelectors asArray select: [ :each | 
	(each beginsWith: 'test') and: [ each numArgs isZero ] ]) sort
].			 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object  newSubclassName: #TestResource 
	iVars: #(name description) 
	cVars: #(current).

TestResource category: #'Tools-Testing'.

TestResource ~> initialize
[
  self description: ' '.
  self name: self printString.

  self setUp
].

TestResource ~> resources
[
  ^self class resources
].			 

TestResource ~> isAvailable
[
"override to provide information on the
 readiness of the resource"
	
  ^true
].			 

TestResource ~> isUnavailable
[
"override to provide information on the
 readiness of the resource"
	
  ^self isAvailable not
].

TestResource ~> printOn: aStream
[
  aStream nextPutAll: self class printString
].			 

TestResource ~> setUp
[
"Does nothing. Subclasses should override this
 to initialize their resource"
].			 

TestResource ~> signalInitializationError
[
  ^self class signalInitializationError
].			 

TestResource ~> tearDown
[
"Does nothing. Subclasses should override this
 to tear down their resource"
].
			 
TestResource class ~> initialize
[
  self current: self new.
].			 

TestResource class ~> resources
[
  ^#()  "base case"
].

TestResource class ~> isAbstract
[
"Override to true if a TestResource subclass is Abstract and should not have
 TestCase instances built from it"

  ^self name = #TestResource
].			 

TestResource class ~> isAvailable
[
  ^self current notNil and: [self current isAvailable]
].			 

TestResource class ~> isUnavailable
[
  ^self isAvailable not
].			 


TestResource class ~> reset
[
  self current ifNotNil: [
	[ self current tearDown ]
	 ensure: [ self current: nil ]
  ]
].
			 

TestResource class ~> signalInitializationError
[
  ^TestResult signalErrorWith: 'Resource ' , self name , ' could not be initialized'
].			 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object  newSubclassName: #TestResult
	iVars: #(failures errors passed) 
	cVars: #().

TestResult category: #'Tools-Testing'.

TestResult comment:
'This is a Collecting Parameter for the running of a bunch of tests.
 TestResult is an interesting object to subclass or substitute.
 #runCase: is the external protocol you need to reproduce.
 Kent has seen TestResults that recorded coverage information
 and that sent email when they were done.'.

TestResult ~> initialize
[
  self errors:   OrderedCollection new
  self failures: OrderedCollection new
  self passed:   OrderedCollection new
].

TestResult ~> defects
[
  ^OrderedCollection new
	addAll: self errors;
	addAll: self failures; yourself
].			 

TestResult ~> errorCount
[
  ^self errors size
].			 

TestResult ~> failureCount
[
  ^self failures size
].			 

TestResult ~> passedCount
[
  ^self passed size
].			 

TestResult ~> runCount
[
  ^self passedCount + self failureCount + self errorCount
].			 

TestResult ~> tests
[
  ^(OrderedCollection new: self runCount)
	addAll: self passed;
	addAll: self errors;
	addAll: self failures;
	yourself
].			 

TestResult ~> hasErrors
[
  ^self errors size > 0
].			 

TestResult ~> hasFailures
[
  ^self failures size > 0
].			 

TestResult ~> hasPassed
[
  ^self hasErrors not and: [self hasFailures not]
].			 

TestResult ~> isError: aTestCase
[
  ^self errors includes: aTestCase
].			 

TestResult ~> isFailure: aTestCase
[
  ^self failures includes: aTestCase
].			 

TestResult ~> isPassed: aTestCase
[
  ^self passed includes: aTestCase
].			 

TestResult ~> printOn: aStream
[
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
].			 


TestResult ~> runCase: aTestCase
[
  | testCasePassed |

 testCasePassed :=
	[ [ aTestCase runCase. true ]
	   on: self class failure
	   do: [ :signal |
		(self failures isEmpty or: [ (failures last == aTestCase) not ])
		ifTrue: [ failures add: aTestCase ].
		signal sunitExitWith: false ]
	]
	 on: self class error
	 do: [ :signal |
		aTestCase errored: signal.
		self errors add: aTestCase.
		signal sunitExitWith: false 
	 ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ] 
].


TestResult ~> forDebuggingAndInspection
[
  ^TestResultForDebuggingAndInspection on: self 
].

TestResult class ~> error
[
	^self exError
].			 

TestResult class ~> exError
[
  ^SUnitNameResolver errorObject
].			 

TestResult ~> failure
[
  ^TestFailure
].			 

TestResult ~> resumableFailure
[
  ^ResumableTestFailure
].			 

TestResult class ~> signalErrorWith: aString 
[
  self error sunitSignalWith: aString
].			 

TestResult class ~> signalFailureWith: aString 
[
  self failure sunitSignalWith: aString
].			 


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object  newSubclassName: #TestResultForDebuggingAndInspection
	iVars: #(testResult) 
	cVars: #().

TestResultForDebuggingAndInspection category: #'Tools-Testing'.

TestResultForDebuggingAndInspection ~> initializeOn: aTestResult
[
  self testResult: aTestResult 
].

TestResultForDebuggingAndInspection ~>
   print: aCollectionOfTestCases
   startingWith: header
   on: aStream
[	
  aCollectionOfTestCases
	do: [ :aTestCase |
		aStream 
			nextPutAll: header;
			space;
			nextPutAll: aTestCase class name;
			nextPutAll: ' debug: #';
			nextPutAll: aTestCase selector ]
	separatedBy: [ aStream newLine ].
].		
	 
TestResultForDebuggingAndInspection ~> printOn: aStream
[
  aStream print: testResult.
  aStream newLine.
	
  self print: testResult errors   startingWith: '"E"' on: aStream.
  self print: testResult failures startingWith: '"F"' on: aStream.
].	 

TestResultForDebuggingAndInspection ~> reRun
[
  | suite |
  suite := TestSuite new.
  suite addTests: self testResult tests.
	
  self testResult: suite run. 
].

TestResultForDebuggingAndInspection class ~> on: aTestResult
[
   ^self new initializeOn: aTestResult 
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Object  newSubclassName: #TestSuite
	iVars: #(tests resources name) 
	cVars: #().

TestSuite category: #'Tools-Testing'.

TestSuite comment:
'This is a Composite of Tests, either TestCases or other TestSuites.
 The common protocol is #run: aTestResult and the dependencies protocol.'.

TestSuite ~> addDependentToHierachy: anObject
[
  self addDependent: anObject. "@@@FIXME: #when:on:do: .."
  self tests do: [ :each | each addDependentToHierachy: anObject] 
].

TestSuite ~> removeDependentFromHierachy: anObject
[
  self removeDependent: anObject.
  self tests do: [ :each | each removeDependentFromHierachy: anObject]
].			 

TestSuite ~> addTest: aTest
[
  self tests add: aTest
].			 

TestSuite ~> addTests: aCollection 
[
  aCollection do: [ :eachTest | self addTest: eachTest ]
].			 

TestSuite ~> defaultResources
[
  ^self tests 
	inject: Set new
	into: [:coll :testCase | 
		coll addAll: testCase resources;
		yourself 
	]
].	 

TestSuite ~> initialize
[
  self resources: self defaultResources.
  self tests: OrderedCollection new.
  self name: self printString.
].

TestSuite ~> run
[
  | result |
  result := TestResult new.
  self resources do: [ :res |
	res isAvailable ifFalse: [^res signalInitializationError]].
  [self run: result] ensure: [self resources do: [:each | each reset]].
  ^result
].			 

TestSuite ~> run: aResult 
[
  self tests do: [:each | 
	self changed: each.
	each run: aResult
  ]
].			 

TestSuite class ~> named: aString
[
  ^self new
	name: aString;
	yourself
].

TestResource class initialize.
			 
"		E O F 		"
