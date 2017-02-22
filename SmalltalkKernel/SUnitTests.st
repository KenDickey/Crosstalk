"FILE: 'SUnitTests.st'
 IMPLEMENTS: Unit tests for SUnit itself.
 AUTHOR: ported from Camp Smalltalk ver 3.1
 Modifications by: Ken Dickey
 DATE: 19 Febuary 2017
"

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TestCase newSubclassName: #SUnitTest 
	iVars: #(hasRun hasSetup hasRanOnce) 
	cVars: #().


SUnitTest comment:
'This is both an example of writing tests and a self test for the SUnit.
 The tests here are pretty strange, since you want to make sure things blow up.
 You should not generally have to write tests this complicated in structure,
 although they will be far more complicated in terms of your own objects-
 more assertions, more complicated setup.

 Kent says: 
    "Never forget, however, that if the tests are hard 
      to write, something is probably wrong with the design." '.

SUnitTest ~>  
  assertForTestResult: aResult
  runCount: aRunCount
  passed:   aPassedCount
  failed:   aFailureCount
  errors:   anErrorCount
[
  self
	assert: aResult     runCount = aRunCount;
	assert: aResult  passedCount = aPassedCount;
	assert: aResult failureCount = aFailureCount;
	assert: aResult   errorCount = anErrorCount
].

SUnitTest ~>  error
[
  3 zork
].

SUnitTest ~> errorShouldntRaise
[
  self 
	shouldnt: [self someMessageThatIsntUnderstood] 
	raise: SUnitNameResolver notificationObject
].

SUnitTest ~> fail
[
  self assert: [false]
].

SUnitTest ~> noop
[].

SUnitTest ~> setRun
[
  self hasRun: true
].

SUnitTest ~> setUp
[
  self hasSetup: true
].

SUnitTest ~> testAssert
[
  self assert: true.
  self deny: false
].

SUnitTest ~> testDefects
[
  | result suite error failure |
  suite := TestSuite new.
  suite addTest: (error   := self class selector: #error).
  suite addTest: (failure := self class selector: #fail).
  result := suite run.
  self assert: result defects asArray = (Array with: error with: failure).
  self
	assertForTestResult: result
	runCount: 2
	passed: 0
	failed: 1
	errors: 1
].

SUnitTest ~> testDialectLocalizedException
[
  Self
	should: [TestResult signalFailureWith: 'Foo']
	raise: TestResult failure.
  self
	should: [TestResult signalErrorWith: 'Foo']
	raise: TestResult error.
].

SUnitTest ~> testError
[
  | case result |

  case := self class selector: #error.
  result := case run.
  self
	assertForTestResult: result
	runCount: 1
	passed: 0
	failed: 0
	errors: 1.

  case := self class selector: #errorShouldntRaise.
  result := case run.
  self 
	assertForTestResult: result
	runCount: 1
	passed: 0
	failed: 0
	errors: 1
].

SUnitTest ~> testException
[
  self
	should: [self error: 'foo']
	raise: TestResult error
].

SUnitTest ~> testFail
[
  | case result |

  case := self class selector: #fail.
  result := case run.

  self
	assertForTestResult: result
	runCount: 1
	passed: 0
	failed: 1
	errors: 0
].

SUnitTest ~> testIsNotRerunOnDebug
[
  | case |

  case := self class selector: #testRanOnlyOnce.
  case run.
  case debug
].

SUnitTest ~> testRan
[
  | case |

  case := self class selector: #setRun.
  case run.
  self assert: case hasSetup.
  self assert: case hasRun
].

SUnitTest ~> testRanOnlyOnce
[
  self assert: self hasRanOnce ~= true.
  self hasRanOnce: true
].

SUnitTest ~> testResult
[
  | case result |

  case := self class selector: #noop.
  result := case run.

  self
	assertForTestResult: result
	runCount: 1
	passed: 1
	failed: 0
	errors: 0
].

SUnitTest ~> testRunning
[
  (SUnitDelay forSeconds: 2) wait
].

SUnitTest ~> testShould
[
  self
	should: [true];
	shouldnt: [false]
].

SUnitTest ~> testSuite
[
  | suite result |

  suite := TestSuite new.
  suite 
	addTest: (self class selector: #noop);
	addTest: (self class selector: #fail);
	addTest: (self class selector: #error).

  result := suite run.

  self
	assertForTestResult: result
	runCount: 3
	passed: 1
	failed: 1
	errors: 1
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" SimpleTestResource "
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TestResource newSubclassName: #SimpleTestResource 
	iVars: #(runningState hasRun hasSetup hasRanOnce) 
	cVars: #().

SimpleTestResource ~> isAvailable
[
  ^self runningState == self startedStateSymbol
].


SimpleTestResource ~> setRun
[
  self hasRun: true
].

SimpleTestResource ~> setUp
[
  self runningState: self startedStateSymbol.
  self hasSetup: true
].


SimpleTestResource ~> startedStateSymbol
[
  ^#started
].

SimpleTestResource ~> stoppedStateSymbol
[
  ^#stopped
].

SimpleTestResource ~> tearDown
[
  self runningState: self stoppedStateSymbol
].



"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" SimpleTestResourceTestCase "
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TestCase newSubclassName: #SimpleTestResourceTestCase
	iVars: #()
	cVars: #().

SimpleTestResourceTestCase class ~> resources
[
  ^Set new add: SimpleTestResource; yourself
].


SimpleTestResourceTestCase ~> dummy
[
  self assert: true
].

SimpleTestResourceTestCase ~> error
[
  'foo' odd
].


SimpleTestResourceTestCase ~> fail
[
  self assert: false
].

SimpleTestResourceTestCase ~> setRun
[
  self resource setRun
].

SimpleTestResourceTestCase ~> setUp
[
  self resource: SimpleTestResource current
].


SimpleTestResourceTestCase ~> testRan
[
  | case |

  case := self class selector: #setRun.
  case run.
  self assert: resource hasSetup.
  self assert: resource hasRun
].

SimpleTestResourceTestCase ~> testResourceInitRelease
[
  | result suite error failure |
  suite := TestSuite new.
  suite addTest: (error   := self class selector: #error).
  suite addTest: (failure := self class selector: #fail).
  suite addTest: (self class selector: #dummy).
  result := suite run.
  self assert: resource hasSetup
].

SimpleTestResourceTestCase ~> testResourcesCollection
[
  | collection |
  collection := self resources.
  self assert: collection size = 1
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" ResumableTestFailureTestCase"
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "


TestCase newSubclassName: #ResumableTestFailureTestCase
	iVars: #()
	cVars: #().

ResumableTestFailureTestCase ~> errorTest
[
  1 zork
].

ResumableTestFailureTestCase ~> failureLog
[
  ^SUnitNameResolver defaultLogDevice
].

ResumableTestFailureTestCase ~> failureTest
[
  self
	assert: false description: 'You should see me'      resumable: true; 
	assert: false description: 'You should see me too'  resumable: true; 
	assert: false description: 'You should see me last' resumable: false; 
	assert: false description: 'You should not see me'  resumable: true
].


ResumableTestFailureTestCase ~> isLogging
[
  ^false
].

ResumableTestFailureTestCase ~> okTest
[
  self assert: true
].

ResumableTestFailureTestCase ~> regularTestFailureTest
[
  self assert: false description: 'You should see me'
].

ResumableTestFailureTestCase ~> resumableTestFailureTest
[
  self
	assert: false description: 'You should see me'      resumable: true; 
	assert: false description: 'You should see me too'  resumable: true; 
	assert: false description: 'You should see me last' resumable: false; 
	assert: false description: 'You should not see me'  resumable: true
].

ResumableTestFailureTestCase ~> testResumable
[
  | result suite |
  suite := TestSuite new.
  suite addTest: (self class selector: #errorTest).
  suite addTest: (self class selector: #regularTestFailureTest).
  suite addTest: (self class selector: #resumableTestFailureTest).
  suite addTest: (self class selector: #okTest).
  result := suite run.
  self  assert: result failures size = 2;
 	assert: result errors size = 1
].



"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
" ExampleSetTest "
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TestCase newSubclassName: #ExampleSetTest
	iVars: #(full empty)
	cVars: #().

ExampleSetTest ~> setUp
[
  self empty: Set new.
  self full:  Set with: 5 with: #abc
].

ExampleSetTest~> testAdd
[
  self empty add: 5.
  self assert: (self empty includes: 5)
].

ExampleSetTest~> testGrow
[
  self empty addAll: (1 to: 100).
  self assert: self empty size = 100 "@@?? 101 ??@@"
].

ExampleSetTest~> testIllegal
[
  self 
	should: [self empty at: 5] 
	raise: TestResult error.
  self 
	should: [self empty at: 5 put: #abc] 
	raise: TestResult error
].

ExampleSetTest~> testIncludes
[
  self assert: (self full includes: 5).
  self assert: (self full includes: #abc)
].


ExampleSetTest~> testOccurrences
[
  self assert: (self empty occurrencesOf: 0) = 0.
  self assert: (self full occurrencesOf: 5) = 1.
  self full add: 5.
  self assert: (self full occurrencesOf: 5) = 1
].

ExampleSetTest ~> testRemove
[
  self full remove: 5.
  self assert: (self full includes: #abc).
  self deny: (self full includes: 5)
].

			 
"		E O F 		"
