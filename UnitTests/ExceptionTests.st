"FILE: 'ExceptionTests.st'
 IMPLEMENTS: Exceptions unit tests (less the GUI)
 AUTHOR: ported from Cuis 5.0r3035 [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 06 March 2017
"

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
Error newSubclassName: #MyResumableTestError iVars: #() cVars: #().
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

MyResumableTestError category: #'Tests-Exceptions'.

MyResumableTestError ~> isResumable
[
  ^true 
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
Error newSubclassName: #MyTestError   iVars: #() cVars: #().
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Notification newSubclassName: #MyTestNotification   iVars: #() cVars: #().


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
TestCase newSubclassName: #ExceptionTests   iVars: #() cVars: #().
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

MyTestError        category: #'Tests-Exceptions'.
MyTestNotification category: #'Tests-Exceptions'.
ExceptionTests     category: #'Tests-Exceptions'.

ExceptionTests ~> assertSuccess: anExceptionTester
[
  self should: [
    ( anExceptionTester suiteLog first)
	 endsWith: 'succeeded'
  ]. 
].

ExceptionTests ~> testDoubleOuterPass
[
  self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest )  
].

ExceptionTests ~> testDoublePassOuter
[
  self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest )  
].

ExceptionTests ~> testDoubleResume
[
  self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest )  
].

ExceptionTests ~> testNonResumableFallOffTheEndHandler
[
  self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler )  
].

ExceptionTests ~> testResignalAs
[
  | answer |
  answer := [ [3 zork]
		on: ZeroDivide
		do: [:ex | ex return: 5]
            ] on: Error do: [:ex | ex resignalAs: ZeroDivide].
  self assert: answer == 5 
].

ExceptionTests ~> testResignalAsUnwinds
[
  | unwound answer |
  unwound := false.
  answer := [ [3 zork]
		on: ZeroDivide 
                do: [:ex | 
                     self assert: unwound.  
                     ex return: 5]
	    ] on: Error 
              do: [:ex | 
                 [ex resignalAs: ZeroDivide] 
                  ifCurtailed: [unwound := true]].
  self assert: answer == 5 
].

ExceptionTests ~> testResumableFallOffTheEndHandler
[
  self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler )  
].

ExceptionTests ~> testSignalFromHandlerActionTest
[
  self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest )  
].

ExceptionTests ~> testSimpleEnsure
[
  self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest )  
].

ExceptionTests ~> testSimpleEnsureTestWithError
[
  self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError )  
].

ExceptionTests ~> testSimpleEnsureTestWithNotification
[
  self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification )  
].

ExceptionTests ~> testSimpleEnsureTestWithUparrow
[
  self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow )  
].

ExceptionTests ~> testSimpleOuter
[
  self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest )  
].

ExceptionTests ~> testSimplePass
[
  self assertSuccess: (ExceptionTester new runTest: #simplePassTest )  
].

ExceptionTests ~> testSimpleResignalAs
[
  self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest )  
].

ExceptionTests ~> testSimpleResume
[
  self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest )  
].

ExceptionTests ~> testSimpleRetry
[
  self assertSuccess: (ExceptionTester new runTest: #simpleRetryTest )  
].

ExceptionTests ~> testSimpleRetryUsing
[
  self assertSuccess: (ExceptionTester new runTest: #simpleRetryUsingTest )  
].

ExceptionTests ~> testSimpleReturn
[
  self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest )  
].

ExceptionTests ~> testHandlerFromAction
[
  "A test ensuring that nested exceptions work as expected."
  | result |
  result := 
   [
	[ [self error: 'trigger error'] on: ZeroDivide do: [ :ex | 'inner' ]
	] on: Error do: [ :ex | 3 / 0 ]
   ] on: ZeroDivide do: [ :ex | 'outer' ].
	self assert: result = 'outer' 
].

ExceptionTests ~> testNonResumableOuter
[
  self should: [
	[Error signal. 4] 
		on: Error 
		do: [:ex | ex outer. ex return: 5]
	] raise: Error
].
 
ExceptionTests ~> testNonResumablePass
[
  self should: [
	[Error signal. 4] 
		on: Error 
		do: [:ex | ex pass. ex return: 5]
	] raise: Error
]. 

ExceptionTests ~> testResumableOuter
[
  | result |
  result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
  self assert: result = 5
].

ExceptionTests ~> testResumablePass
[
  | result |
  result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
  self assert: result = 4
].

ExceptionTests ~> testNoTimeout
[
  self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest )  
].

ExceptionTests ~> testTimeoutWithZeroDuration
[
  self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest )  
].


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
Object newSubclassName: #ExceptionTester  
	iVars: #(myLog suiteLog iterationsBeforeTimeout)
	cVars: #().
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

ExceptionTester category: #'Tests-Exceptions'.

ExceptionTester ~> initialize
[
  super initialize.
  self myLog:    OrderedCollection new.
  self suiteLog: OrderedCollection new.
  self iterationsBeforeTimeout: 1.
].

ExceptionTester ~> basicANSISignaledExceptionTestSelectors
[
  ^#( simpleOuterTest  doubleOuterPassTest doublePassOuterTest
      simplePassTest simpleResignalAsTest simpleResumeTest 
      simpleRetryTest simpleRetryUsingTest simpleReturnTest) 
"doubleOuterTest"
].

ExceptionTester ~> basicTestSelectors
[
  ^ #(simpleEnsureTest simpleEnsureTestWithNotification simpleEnsureTestWithUparrow 
     simpleEnsureTestWithError signalFromHandlerActionTest 
     resumableFallOffTheEndHandler nonResumableFallOffTheEndHandler doubleResumeTest
"timeouts NYI:  simpleTimeoutWithZeroDurationTest simpleTimeoutTest simpleNoTimeoutTest "
    )
].

ExceptionTester ~> doSomethingElseString
[
  ^'Do something else.' 
].

ExceptionTester ~> doSomethingExceptionalString
[
  ^'Do something exceptional.' 
].

ExceptionTester ~> doSomethingString
[
  ^'Do something.' 
].

ExceptionTester ~> doYetAnotherThingString
[
  ^'Do yet another thing.' 
].

ExceptionTester ~> testString
[
  ^'This is only a test.' 
].

ExceptionTester ~> clearLog
[
  self myLog: OrderedCollection new.
].

ExceptionTester ~> contents
[
  ^( self myLog
    inject: (WriteStream on: (String new: 80))
    into: [ :result :item |
      result 
	        newLine; 
        	nextPutAll: item;
	      	yourself
    ] ) contents 
].

ExceptionTester ~> log: aString
[
  self myLog add: aString 
].

ExceptionTester ~> log
[
  ^self myLog
].

ExceptionTester ~> logTest: aSelector
[
  self suiteLog add: aSelector 
].

ExceptionTester ~> logTestResult: aString
[
  | index |
  index := self suiteLog size.
  self suiteLog 
	  at: index
  	 put: ((self suiteLog at: index), ' ', aString) 
].

ExceptionTester ~> doSomething
[
  self log: self doSomethingString 
].

ExceptionTester ~> doSomethingElse
[
  self log: self doSomethingElseString 
].

ExceptionTester ~> doSomethingExceptional
[
  self log: self doSomethingExceptionalString 
].

ExceptionTester ~> doYetAnotherThing
[
  self log: self doYetAnotherThingString 
].

ExceptionTester ~> methodWithError
[
  MyTestError signal: self testString 
].

ExceptionTester ~> methodWithNotification
[
  MyTestNotification signal: self testString 
].

ExceptionTester ~> doubleOuterPassTest
[
  "uses #resume"
  [
   [ [ self doSomething.
       MyTestNotification signal.
       self doSomethingExceptional
     ] on: MyTestNotification do: [:ex | ex outer. self doSomethingElse]
   ] on: MyTestNotification
     do: [:ex | ex pass.  self doSomethingExceptional]
  ] on: MyTestNotification 
    do: [:ex | self doYetAnotherThing. ex resume] 
].

ExceptionTester ~> doubleOuterTest
[
  "uses #resume"
 [
  [ [ self doSomething.
      MyTestNotification signal.
      self doSomethingExceptional
    ] on: MyTestNotification
      do: [:ex | ex outer. self doSomethingExceptional]
  ] on: MyTestNotification
    do: [:ex | ex outer. self doSomethingElse]
 ] on: MyTestNotification do: [:ex | self doYetAnotherThing. ex resume] 
].

ExceptionTester ~> doublePassOuterTest
[
  "uses #resume"
  [
    [ [ self doSomething.
        MyTestNotification signal.
        self doSomethingExceptional
       ] on: MyTestNotification do: [:ex | ex pass. self doSomethingExceptional]
    ] on: MyTestNotification
      do: [:ex | ex outer. self doSomethingElse]
  ] on: MyTestNotification do: [:ex | self doYetAnotherThing. ex resume] 
].

ExceptionTester ~> simpleOuterTest
[
  "uses #resume"
  [ [ self doSomething.
      MyTestNotification signal.
      "self doSomethingElse"
      self doSomethingExceptional
    ] on: MyTestNotification
      do: [:ex | ex outer. self doSomethingElse]
  ] on: MyTestNotification
    do: [:ex | self doYetAnotherThing. ex resume] 
].

ExceptionTester ~> simplePassTest
[
  [ self doSomething.
    MyTestError signal.
    self doSomethingElse
  ] on: MyTestError
    do:
      [:ex |
      self doYetAnotherThing.
      ex pass "expecting handler in #runTest:"] 
].

ExceptionTester ~> simpleResignalAsTest
[
  "ExceptionTester new simpleResignalAsTest"
  [ self doSomething.
    MyTestNotification signal.
    self doSomethingElse
  ] on: MyTestNotification
    do: [:ex | ex resignalAs: MyTestError new] 
].

ExceptionTester ~> simpleResumeTest
[
  "see if we can resume twice"
   [ | it |
    self doSomething.
    it := MyResumableTestError signal.
    it = 3 ifTrue: [self doSomethingElse].
    it := MyResumableTestError signal.
    it = 3 ifTrue: [self doSomethingElse].
   ] on: MyResumableTestError
     do: [:ex |
        self doYetAnotherThing.
        ex resume: 3
     ] 
].

ExceptionTester ~> simpleRetryTest
[
  | theMeaningOfLife |
  theMeaningOfLife := nil.
  [ self doSomething.
    (theMeaningOfLife == nil)
    ifTrue:  [MyTestError signal]
    ifFalse: [self doSomethingElse]
  ] on: MyTestError
    do: [:ex |
        theMeaningOfLife := 42.
        self doYetAnotherThing.
        ex retry
    ] 
].

ExceptionTester ~> simpleRetryUsingTest
[
  [ self doSomething.
    MyTestError signal.
    self doSomethingElse
  ] on: MyTestError
    do: [:ex | ex retryUsing: [self doYetAnotherThing] ] 
].

ExceptionTester ~> simpleReturnTest
[
  | it |
  it := [
	self doSomething.
	MyTestError signal.
  	self doSomethingElse
  ] on: MyTestError
    do: [:ex | ex return: 3].

  (it = 3) ifTrue: [self doYetAnotherThing] 
].

ExceptionTester ~> doubleOuterPassTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    add: self doSomethingElseString;
    yourself 
].

ExceptionTester ~> doublePassOuterTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    add: self doSomethingElseString;
    yourself 
].

ExceptionTester ~> simpleOuterTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    add: self doSomethingElseString;
    yourself 
].

ExceptionTester ~> simplePassTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    add: 'Unhandled Exception';
    yourself 
].

ExceptionTester ~> simpleResignalAsTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: 'Unhandled Exception';
    yourself 
].

ExceptionTester ~> simpleResumeTestResults
[
  "see if we can resume twice"

  ^OrderedCollection new
      add: self doSomethingString;
      add: self doYetAnotherThingString;
      add: self doSomethingElseString;
      add: self doYetAnotherThingString;
      add: self doSomethingElseString;
      yourself 
].

ExceptionTester ~> simpleRetryTestResults
[
  ^OrderedCollection new
      add: self doSomethingString;
      add: self doYetAnotherThingString;
      add: self doSomethingString;
      add: self doSomethingElseString;
      yourself 
].

ExceptionTester ~> simpleRetryUsingTestResults
[
  ^OrderedCollection new
      add: self doSomethingString;
      add: self doYetAnotherThingString;
      yourself 
].

ExceptionTester ~> simpleReturnTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> doubleResumeTest
[
   [ self doSomething.
     MyResumableTestError signal.
     self doSomethingElse.
     MyResumableTestError signal.
     self doYetAnotherThing
   ] on: MyResumableTestError
     do: [:ex | ex resume]. 
].

ExceptionTester ~> nonResumableFallOffTheEndHandler
[
  
  [ self doSomething.
    MyTestError signal.
    self doSomethingElse
  ] on: MyTestError
    do: [:ex | self doSomethingExceptional].

  self doYetAnotherThing 
].

ExceptionTester ~> resumableFallOffTheEndHandler
[
  [ self doSomething.
    MyTestNotification signal.
    self doSomethingElse
  ] on: MyTestNotification
    do: [:ex | self doSomethingExceptional].

  self doYetAnotherThing 
].

ExceptionTester ~> signalFromHandlerActionTest
[
  [ self doSomething.
    MyTestError signal.
    self doSomethingElse
  ] on: MyTestError
    do: [ self doYetAnotherThing.
          MyTestError signal
    ] 
].

ExceptionTester ~> simpleEnsureTest
[
  [ self doSomething.
    self doSomethingElse
  ] ensure: [self doYetAnotherThing].
].

ExceptionTester ~> simpleEnsureTestWithError
[
  [ self doSomething.
    MyTestError signal.
    self doSomethingElse
  ] ensure: [self doYetAnotherThing].
].

ExceptionTester ~> simpleEnsureTestWithNotification
[
  [ self doSomething.
    self methodWithNotification.
    self doSomethingElse
  ] ensure: [self doYetAnotherThing].
].

ExceptionTester ~> simpleEnsureTestWithUparrow
[
  [ self doSomething.
    true ifTrue: [^nil].
    self doSomethingElse 
  ] ensure: [self doYetAnotherThing].
].   

ExceptionTester ~> warningTest
[
  self log: 'About to signal warning.'.
  Warning signal: 'Ouch'.
  self log: 'Warning signal handled and resumed.' 
].

ExceptionTester ~> doubleResumeTestResults
[
       ^OrderedCollection new
               add: self doSomethingString;
               add: self doSomethingElseString;
               add: self doYetAnotherThingString;
               yourself 
].

ExceptionTester ~> nonResumableFallOffTheEndHandlerResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doSomethingExceptionalString;
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> resumableFallOffTheEndHandlerResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doSomethingExceptionalString;
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> signalFromHandlerActionTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doYetAnotherThingString;
    add: 'Unhandled Exception';
    yourself 
].

ExceptionTester ~> simpleEnsureTestResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doSomethingElseString;
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> simpleEnsureTestWithErrorResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: 'Unhandled Exception';
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> simpleEnsureTestWithNotificationResults
[
  ^OrderedCollection new
    add: self doSomethingString;
    add: self doSomethingElseString;
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> simpleEnsureTestWithUparrowResults
[
  ^OrderedCollection new
    add: self doSomethingString;
"    add: self doSomethingElseString;"
    add: self doYetAnotherThingString;
    yourself 
].

ExceptionTester ~> runAllTests
[
  "ExceptionTester new runAllTests"
  self
    runBasicTests;
    runBasicANSISignaledExceptionTests 
].

ExceptionTester ~> runBasicANSISignaledExceptionTests
[
  self basicANSISignaledExceptionTestSelectors
    do:
      [:eachTestSelector |
      self runTest: eachTestSelector] 
].

ExceptionTester ~> runBasicTests
[
  self basicTestSelectors
    do:
      [:eachTestSelector |
      self runTest: eachTestSelector] 
].

ExceptionTester ~> runTest: aSelector
[
  | actualResult expectedResult |
  [ self clearLog;
         logTest: aSelector;
         perform: aSelector 
  ] on: MyTestError 
    do: [ :ex | self log: 'Unhandled Exception'.
          ex return: nil 
    ].

  actualResult   := self log.
  expectedResult := self perform: (aSelector, #Results) asSymbol.

  actualResult = expectedResult
    ifTrue:  [self logTestResult: 'succeeded']
    ifFalse: [self logTestResult: 'failed' ].

  Transcript newLine;
	nextPutAll: self suiteLog last printString.
 ].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"File 'ExceptionTests' -> #RunExceptionTests block"
Smalltalk 
	at: #RunExceptionTests 
	put: [ ExceptionTester new runAllTests ].

"    E O F     "
