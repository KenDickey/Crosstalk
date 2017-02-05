"FILE: 'Time.st'
 IMPLEMENTS: Class Time (NB: should be named #TimeOfDay)
 AUTHOR: Cuis [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 02 Febuary 2017
"

Magnitude newSubclassName: #Time iVars: #(totalSeconds nanos) cVars: nil.

"@@ Used #totalSeconds to avoid privateSeconds confusion @@"

Time category: #'Kernel-Chronology'.

Time comment:
'This represents a particular point in time during any given day. 
 For example, ''5:19:45 pm''.

 If you need a point in time on a particular day, use DateAndTime. 
 If you need a duration of time, use Duration.'.

Time ~> initialize
[
  self totalSeconds: 0.
  self nanos:        0.
].


Time ~> < aTime
[
  ^(self totalSeconds < aTime totalSeconds) or: [ 
	(self totalSeconds = aTime totalSeconds)
 	    and: [self nanos < aTime privateNanos ]
  ]
].

Time ~> = aTime
[
"Any object is equal to itself"
  self == aTime ifTrue: [ ^ true ].

  self class == aTime class ifFalse: [ ^false ].

  ^ self totalSeconds = aTime totalSeconds 
	and: [ nanos = aTime privateNanos ] 
].

Time ~> hash
[
  ^ self totalSeconds bitXor: self nanos 
].

Time ~> hour
[
  ^ self hour24
].

Time ~> hour12
[
"Answer an <integer> between 1 and 12, inclusive, representing the hour 
 of the day in the 12-hour clock of the local time of the receiver."
  ^ self hour24 - 1 \\ 12 + 1 
].

Time ~> hour24
[
  ^ (self asDurationSinceMidnight) 
	daysHoursMinutesSecondsNanosDo: [ :d :h :m :s :n | h ]
].

Time ~> meridianAbbreviation
[
  ^ self hour < 12 ifTrue: ['AM'] ifFalse: ['PM'].
].

Time ~> minute
[
  ^ self asDurationSinceMidnight
	daysHoursMinutesSecondsNanosDo: [ :d :h :m :s :n | m ]
].

Time ~> second
[
  ^ self asDurationSinceMidnight
	daysHoursMinutesSecondsNanosDo: [ :d :h :m :s :n | s ]
].

Time ~> days
[
  ^ (self asDurationSinceMidnight) 
	daysHoursMinutesSecondsNanosDo: [ :d :h :m :s :n | d ]
].

Time ~> hhmm24
[
"Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm),
 of exactly 4 digits"

  ^(String streamContents: 
	[ :aStream | self print24: true showSeconds: false on: aStream ])
		copyWithout: $: 
].

Time ~> print24
[
"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"

  ^String streamContents: [ :aStream |
	self print24: true showSeconds: true on: aStream ] 
].

Time ~> print24: hr24 on: aStream 
[
"Format is 'hh:mm:ss' or 'h:mm:ss am' "

	self print24: hr24 showSeconds: true on: aStream 
]. 

Time ~> print24: hr24 showSeconds: showSeconds on: aStream
[
"Format is 'hh:mm:ss' or 'h:mm:ss am' 
 or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

  ^self print24: hr24 
	showSeconds: showSeconds 
	showSecondsFraction: false 
	on: aStream 
].

Time ~> 
 print24: hr24 
 showSeconds: showSeconds 
 showSecondsFraction: showSecondsFraction 
 on: aStream
[
"Format is 'hh:mm:ss' or 'h:mm:ss am' 
 or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

  | h m s |
  h := self hour.
  m := self minute.
  s := self second.
  hr24
  	ifTrue: [
  	    h < 10 ifTrue: [ aStream nextPutAll: '0' ].
  	    h printOn: aStream 
	]
  	ifFalse: [
  	    h > 12
  		ifTrue: [h - 12 printOn: aStream]
  		ifFalse: [h < 1
  				ifTrue: [ 12 printOn: aStream ]
  				ifFalse: [ h printOn: aStream ]
		]
	].

  aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
  m printOn: aStream.

  showSeconds ifTrue: [
     aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
     (self nanoSecond = 0 or: [ showSecondsFraction not ])
  	ifTrue: [s asInteger printOn: aStream]
  	ifFalse: [(s + (1.0 * self nanoSecond / Time nanosInSecond))
		   printOn: aStream]
  ].

   hr24 ifFalse: [
	aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) 
   ] 
].

Time ~> print24: hr24 showSecondsFraction: showSecondsFraction on: aStream
[
"Format is 'hh:mm:ss.ssssss'
 or 'h:mm:ss.ssssss am' 
 or, if showSecondsFraction is false, 'hh:mm:ss' or 'h:mm:ss am'"

  ^self print24: hr24 showSeconds: true showSecondsFraction: showSecondsFraction on: aStream 
].

Time ~> printMinutes
[
"Return as string 'hh:mm pm'  "

  ^String streamContents:
	[ :aStream | self print24: false showSeconds: false on: aStream ]
].

Time ~> printOn: aStream
[
   self print24: false
	showSeconds: ((self seconds ~= 0) or: [ self nanos ~= 0])
	showSecondsFraction: self nanos ~= 0
	on: aStream 
].

Time ~> storeOn: aStream
[
  aStream print: self printString; nextPutAll: ' asTime' 
].

Time ~> nanoSeconds
[
  ^ self nanos
].


Time ~> seconds: secondCount nanoSeconds: nanoCount 
[
"Private - only used by Time class."
"@@@FIXME:  self assert: nanoCount < Time nanosInSecond."
  self seconds: secondCount.
  self nanos:   nanoCount 
].

Time ~> - aTime
[
  ^ self asDurationSinceMidnight - aTime asDurationSinceMidnight 
].

Time ~> asDurationSinceMidnight
[
"Answer the duration since midnight"

  ^ Duration seconds: self seconds nanoSeconds: self nanos 
].

Time class ~> microsecondsToRun: timedBlock 
[
 "Answer the number of microseconds timedBlock takes to return its value."

  | startTime endTime duration |
  startTime := PointInTime now.
  timedBlock value.
  endTime := PointInTime now.
  duration := endTime - startTime.
  ^(duration nanos * 1000000000 + duration seconds) / 1000
].

"Time ~> dateAndTimeFromSeconds: secondCount
[
  ^ Array
  	with: (Date fromSeconds: secondCount)
  	with: (self fromSeconds: secondCount \\ self secondsInDay)
]."

Time class ~> dateAndTimeNow
[
"Answer a two-element Array of (Date today, Time now)."

  | dateAndTime |
  dateAndTime := DateAndTime now.
"  ^{ dateAndTime date . dateAndTime time } "
  ^dateAndTime "@@FIXME!!@@"
].

Time class ~> fromSeconds: secondCount 
[
 "Answer an instance of me that is secondCount number of seconds since midnight."

  | integerSeconds nanos |
  integerSeconds := secondCount truncated.
  integerSeconds = secondCount
      ifTrue: [nanos := 0]
      ifFalse: [nanos := 
	  (secondCount - integerSeconds * self nanosInSecond) asInteger
	].
  ^ self seconds: integerSeconds nanoSeconds: nanos
]. 

Time class ~> midnight
[
  ^ self totalSeconds: 0
].

Time class ~> new
[
 "Answer a Time representing midnight"

  ^ self midnight 
].

Time ~> readFrom: aStream
[
 "Read a Time from the stream in the form:
  	<hour>:<minute>:<second> <am/pm>

  <minute>, <second> or <am/pm> may be omitted. 
 e.g. 1:59:30 pm; 8AM; 15:30"

  | hour minute second ampm nanos nanosBuffer |
  hour := Integer readFrom: aStream.
  minute := 0.
  second := 0.
  nanosBuffer := '000000000' copy.
  nanos := WriteStream on: nanosBuffer.
  (aStream peekFor: $:) 
  	ifTrue: [
  		minute := Integer readFrom: aStream.
  		(aStream peekFor: $:) 
  			ifTrue: [
  				second := Integer readFrom: aStream].
  				(aStream peekFor: $.)
  					ifTrue: [
  						[aStream atEnd not and: [aStream peek isDigit]]
  							whileTrue: [nanos nextPut: aStream next]]].
  aStream skipSeparators.
  (aStream atEnd not and: [aStream peek isLetter]) ifTrue: 
  	[ampm := aStream next asLowercase.
  
  (ampm = $p and: [hour < 12]) ifTrue: [hour := hour + 12].
 	(ampm = $a and: [hour = 12]) ifTrue: [hour := 0].
  
  (aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
  ^ self 
  	hour: hour 
  	minute: minute 
  	second: second 
  	nanoSecond: nanosBuffer asInteger

  "Time readFrom: (ReadStream on: '2:23:09 pm')" 
].

Time class ~> afterMidnight: aDuration
[
  "Answer a Time aDuration after midnight.
  If aDuration is longer than a day, take the day fraction."
  | nanoCount secondCount |
  secondCount := aDuration seconds \\ self secondsInDay.
  nanoCount := aDuration nanoSeconds.
  ^ self basicNew initialize seconds: secondCount nanoSeconds: nanoCount  
].

Time class ~> fromString: aString
[
  ^ self readFrom: (ReadStream on: aString) 
].

Time class ~> hour: hour minute: minute second: second
[
"Answer a Time"

  ^ self hour: hour minute: minute second: second nanoSecond: 0 
].

Time class ~> hour: hour minute: minute second: second nanoSecond: nanoCount
[
"Answer a Time"

  ^ self 
  	seconds: (hour * self secondsInHour)
		 + (minute * self secondsInMinute)
		 + second 
  	nanoSeconds: nanoCount 
].

Time class ~> midnight
[
  ^ self seconds: 0
].

Time class ~> noon
[
  ^ self seconds: self secondsInDay / 2 
].

Time class ~> seconds: seconds
[
"Answer a Time from midnight."

  ^ self seconds: seconds nanoSeconds: 0
].

Time class ~> seconds: seconds nanoSeconds: nanoCount
[
"Answer a Time from midnight."

  ^ self afterMidnight: (Duration seconds: seconds nanoSeconds: nanoCount) 
].

Time class ~> now
[
"Answer a Time representing the time right now - this is a 24 hour clock.
 Precision is microsecond if the VM supports it, millisecond otherwise"

  | timeAtNow |
  timeAtNow := (DateAndTime now - DateAndTime midnight).
  ^ self totalSeconds: timeAtNow totalSeconds nanoSeconds: timeAtNow nanoSeconds
].

Time class ~> microsecondsInDay
[
  ^86400000000 
].

Time class ~> millisecondsInDay
[
  ^86400000 
].

Time class ~> nanosInSecond
[
  ^ 1000000000 
].

Time class ~> nanosecondsInDay
[
  ^86400000000000 
].

Time class ~> secondsInDay
[
  ^86400 
].

Time class ~> secondsInHour
[
  ^3600 
].

Time class ~> secondsInMinute
[
  ^60 
].

Time class ~> squeakEpoch
[
  ^ 2415386. 		"Julian day number of 1 Jan 1901" 
].


"		E O F 		"
