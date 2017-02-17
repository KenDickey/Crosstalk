"FILE: 'DateAndTime.st'
 IMPLEMENTS: Augments Class DateAndTime (NB: point in time -- vs Duration)
 AUTHOR: Cuis [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 05 Febuary 2017
"

"I represent a point in UTC time as defined by ISO 8601. I have zero duration."

DateAndTime ~> + operand
[
"operand conforms to protocol Duration"

"@@fixme self assert: operand class == Duration."

  ^ self class
	julianDayNumber: jdn
	seconds:     self totalSeconds + operand totalSeconds
	nanoseconds: self nanoSeconds  + operand nanoSeconds
	offset: offset 
].

DateAndTime ~> - operand
[
"operand is a DateAndTime or a Duration"

	| lvalue rvalue |
	(operand is: #DateAndTime)
		ifTrue: [
			offset = operand offset
				ifTrue: [
					lvalue := self.
					rvalue := operand ]
				ifFalse: [
					lvalue := self asUTC.
					rvalue := operand asUTC ].
			^ Duration
 				seconds: (Time secondsInDay *(lvalue julianDayNumber - rvalue julianDayNumber)) + 
							(lvalue secondsSinceMidnight - rvalue secondsSinceMidnight)
 				nanoSeconds: lvalue nanoSecond - rvalue nanoSecond ].
	^self + operand negated 
].

DateAndTime ~> < comparand
[
	"comparand is a DateAndTime."
	| lvalue rvalue |
	self assert: (comparand is: #DateAndTime).
	offset = comparand offset
		ifTrue: [
			lvalue := self.
			rvalue := comparand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := comparand asUTC ].
	^ lvalue julianDayNumber < rvalue julianDayNumber
		or: [
			lvalue julianDayNumber > rvalue julianDayNumber
				ifTrue: [ false ]
				ifFalse: [
					lvalue secondsSinceMidnight < rvalue secondsSinceMidnight
						or: [
							lvalue secondsSinceMidnight > rvalue secondsSinceMidnight
								ifTrue: [ false ]
								ifFalse: [ lvalue nanoSecond < rvalue nanoSecond ]]]] 
].

DateAndTime ~> = comparand
[
	"comparand is a DateAndTime."
	| lvalue rvalue |

	"Any object is equal to itself"
	self == comparand ifTrue: [ ^ true ].

	self class == comparand class ifFalse: [ ^false ].

	offset = comparand offset
		ifTrue: [
			lvalue := self.
			rvalue := comparand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := comparand asUTC ].
	^ lvalue julianDayNumber = rvalue julianDayNumber
		and: [ lvalue secondsSinceMidnight = rvalue secondsSinceMidnight
			and: [ lvalue nanoSecond = rvalue nanoSecond ] ] 
].

DateAndTime ~> asLocal
[
	

	^ (self offset = self class localOffset)

		ifTrue: [self]
		ifFalse: [self utcOffset: self class localOffset]
 
].

DateAndTime ~> asUTC
[

	^ offset isZero
		ifTrue: [self]
		ifFalse: [self utcOffset: 0 hours]
 
].

DateAndTime ~> dayOfMonth
[
	"Answer which day of the month is represented by the receiver."

	^ self
		dayMonthYearDo: [ :d :m :y | d ] 
].

DateAndTime ~> dayOfWeek
[

	"Sunday=1, ... , Saturday=7"

	^ (jdn + 1 rem: 7) + 1 
].

DateAndTime ~> dayOfWeekAbbreviation
[

	^ self dayOfWeekName copyFrom: 1 to: 3 
].

DateAndTime ~> dayOfWeekName
[

	^ Week nameOfDay: self dayOfWeek
 
].

DateAndTime ~> dayOfYear
[
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	| monthStart |
	^ self dayMonthYearDo:
		[ :d :m :y |
			monthStart := #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]] 
].

DateAndTime ~> hash
[

	^(jdn bitXor: seconds) bitXor: nanos 
].

DateAndTime ~> hour
[

	^ self hour24
 
].

DateAndTime ~> hour12
[
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	^ self hour24 - 1 \\ 12 + 1 
].

DateAndTime ~> hour24
[


	^ (Duration seconds: seconds) hours
 
].

DateAndTime ~> isLeapYear
[


	^ Year isLeapYear: self yearNumber 
].

DateAndTime ~> meridianAbbreviation
[

	^ self time meridianAbbreviation 
].

DateAndTime ~> minute
[


	^ (Duration seconds: seconds) minutes
 
].

DateAndTime ~> monthAbbreviation
[


	^ self monthName copyFrom: 1 to: 3
 
].

DateAndTime ~> monthName
[


	^ Month nameOfMonth: self monthIndex
 
].

DateAndTime ~> offset
[

	^ offset
 
].

DateAndTime ~> second
[


	^ (Duration seconds: seconds) seconds
 
].

DateAndTime ~> timeZoneAbbreviation
[

	^ self class localTimeZone abbreviation
 
].

DateAndTime ~> timeZoneName
[

	^ self class localTimeZone name
 

].

DateAndTime ~> dayMonthYearDo: aBlock
[
	"Evaluation the block with three arguments: day month, year."

	| l n i j dd mm yyyy |
	l := jdn + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	dd := l - (2447 * j // 80).
	l := j // 11.
	mm := j + 2 - (12 * l).
	yyyy := 100 * (n - 49) + i + l.

	^ aBlock
		value: dd
		value: mm
		value: yyyy. 
].

DateAndTime ~> julianDayNumber
[


	^ jdn
 
].

DateAndTime ~> midnight
[
	"Answer a DateAndTime starting at midnight local time"

	^self class basicNew
		setJdn: jdn
		seconds: 0
		nano: 0
		offset: self class localOffset
 
].

DateAndTime ~> nanoSecond
[


	^ nanos
 
].

DateAndTime ~> noon
[
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: [ :d :m :y |
		self class year: y month: m day: d hour: 12 minute: 0 second: 0 ] 
].

DateAndTime ~> printHMSOn: aStream
[
	"Print just hh:mm:ss"
	self printHMSOn: aStream separator: $: 
].

DateAndTime ~> printHMSOn: aStream separator: aChar
[
	"Print just hh:mm:ss"
	aStream
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self second asString padded: #left to: 2 with: $0).
 
].

DateAndTime ~> printOn: aStream
[
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: true 
].

DateAndTime ~> printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
[
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c numericValue > $0 numericValue ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs asString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs asString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated asString) ]] 
].

DateAndTime ~> printWithMsOn: aStream
[
	"Print with millisecond resolution, no leading space, no offset."

	| ps |
	self printYMDOn: aStream withLeadingSpace: false.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	ps := (self nanoSecond // 1000000) printString padded: #left to: 3 with: $0.
	aStream nextPut: $..
	aStream nextPutAll: ps 
].

DateAndTime ~> printYMDOn: aStream
[
	"Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	^self printYMDOn: aStream withLeadingSpace: false.
 
].

DateAndTime ~> printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo
[
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ]].
	aStream
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPut: $-;
		nextPutAll: (month asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (day asString padded: #left to: 2 with: $0)
 
].

DateAndTime ~> setJdn: j seconds: s nano: n offset: o
[

jdn := j.
seconds := s.
nanos :=  n.
offset :=  o
 
].

DateAndTime ~> to: anEnd
[
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	^ Timespan starting: self ending: anEnd 
].

DateAndTime ~> to: anEnd by: aDuration
[
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.
 
].

DateAndTime ~> to: anEnd by: aDuration do: aBlock
[
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	| schedule |
	schedule := self to: anEnd by: aDuration.
	schedule scheduleDo: aBlock.
	"Timespan are open on the right side. But include requested end if appropriate, like implementation in Number
	See DateAndTimeTest>>#testToByDo
	"
	schedule end + DateAndTime clockPrecision = anEnd ifTrue: [
		aBlock value: anEnd ] 
].

DateAndTime ~> utcOffset: anOffset
[

	"Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset"

	| diff |
	self assert: anOffset class == Duration.

	diff := anOffset - self offset.
	^ self class
		julianDayNumber: jdn
		seconds: seconds + diff totalSeconds
		nanoseconds: nanos + diff nanoSeconds
		offset: anOffset 

].

DateAndTime ~> daysInMonth
[
	"Answer the number of days in the month represented by the receiver."


	^ self month daysInMonth 
].

DateAndTime ~> daysInYear
[

	"Answer the number of days in the year represented by the receiver."

	^ (Year including: self) daysInYear 
].

DateAndTime ~> daysLeftInYear
[
	"Answer the number of days in the year after the date of the receiver."

	^ self daysInYear - self dayOfYear
 
].

DateAndTime ~> firstDayOfMonth
[

	^ self month start dayOfYear 

].

DateAndTime ~> julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset
[
	"secs could span more than one day. nsecs could span more than one second."
	
	| daysOverflow nanosSinceSec secsOverflow secsSinceMidnight totalSecs totalDays |
	"Add to totalSecs any whole seconds in nsecs"
	secsOverflow := nsecs // Time nanosInSecond.
	nanosSinceSec := nsecs - (secsOverflow * Time nanosInSecond).
	totalSecs := secs + secsOverflow.

	"Add to totalDays any whole days in totalSecs"
	daysOverflow := totalSecs // Time secondsInDay.
	secsSinceMidnight := totalSecs - (daysOverflow * Time secondsInDay).
	totalDays := day + daysOverflow.

	jdn := totalDays.
	seconds := secsSinceMidnight.
	nanos := nanosSinceSec.
	offset := utcOffset 
].

DateAndTime ~> secondsSinceMidnight
[

	^ seconds 

].

DateAndTime ~> date
[

	^Date including: self 
].

DateAndTime ~> filenamishPrintOn: aStream
[
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName := Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: $m 
].

DateAndTime ~> month
[

	^Month including: self 
].

DateAndTime ~> monthIndex
[
	^ self dayMonthYearDo:
		[ : d : m : y |  m ] 
].

DateAndTime ~> printStringWithoutOffset
[
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^String streamContents: [ :strm | self printWithoutOffsetOn: strm ] 
].

DateAndTime ~> printWithoutOffsetOn: aStream
[
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: false 
].

DateAndTime ~> secondsSinceSqueakEpoch
[
	"Return the number of seconds since the Squeak epoch"

	^ (self - (self class epoch)) totalSeconds 
].

DateAndTime ~> time
[

	^Time seconds: seconds nanoSeconds: nanos 
].

DateAndTime ~> unixTimeSeconds
[
	"Return the number of seconds since the Unix epoch as an integer number

	DateAndTime now  unixTimeSeconds
	
	To check consistency with https://en.wikipedia.org/wiki/Unix:=time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSeconds
	1462118860
	"

	| elapsed |
	elapsed := self - self class unixEpoch.
	^ elapsed totalSeconds 
].

DateAndTime ~> unixTimeSecondsFloat
[
	"Return the number of seconds since the Unix epoch.
	Answer an instance of Float, including fraction of a second

	DateAndTime now  unixTimeSecondsFloat
	
	To check consistency with https://en.wikipedia.org/wiki/Unix:=time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSecondsFloat
	1.46211886e9
	"

	"We know that DateAndTime unixEpoch has nanos set to zero"
	^ self unixTimeSeconds + (nanos * 1.0e-9) 
].

DateAndTime ~> week
[

	^Week including: self 
].

DateAndTime ~> year
[

	^Year including: self 
].

DateAndTime ~> yearNumber
[
	^ self
		dayMonthYearDo: [ :d :m :y | y ] 

].

DateAndTime ~> is: aSymbol
[
	^ aSymbol == #DateAndTime or: [ super is: aSymbol ] 

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

DateAndTime class ~> now
[
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps"

	| days nanoseconds remainingNanoseconds remainingSeconds |
	nanoseconds := Time localMicrosecondClock * 1000.
	(LastTick < nanoseconds)
		ifTrue: [LastTick := nanoseconds]
		ifFalse: [
			LastTickSemaphore critical: [
				LastTick :=  LastTick + 1.
				nanoseconds := LastTick ]].

	days := nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds := nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds := remainingNanoseconds // 1000000000.
	remainingNanoseconds := remainingNanoseconds \\ 1000000000.
	^ self basicNew
		setJdn: Time squeakEpoch + days 
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: self localOffset 
].

DateAndTime class ~> year: year day: dayOfYear hour: hour minute: minute second: second
[

	^ self
		year: year
		day: dayOfYear
		hour: hour
		minute: minute
		second: second
		offset: self localOffset.
 
].

DateAndTime class ~> year: year day: dayOfYear hour: hour minute: minute second: second offset: offset 
[
	"Return a DataAndTime"

	| y d |
	y := self
		year: year
		month: 1
		day: 1
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset.

	d := Duration days: (dayOfYear - 1).

	^ y + d 
].

DateAndTime class ~> year: year month: month day: day hour: hour minute: minute second: second
[
	"Return a DateAndTime"

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		offset: self localOffset
 
].

DateAndTime class ~> year: year month: month day: day hour: hour minute: minute second: second offset: offset
[

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset
 

].

DateAndTime class ~> date: aDate time: aTime
[

	^ self 
		year: aDate yearNumber 
		day: aDate dayOfYear 
		hour: aTime hour 
		minute: aTime minute 
		second: aTime second 
].

DateAndTime class ~> epoch
[
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self julianDayNumber: Time squeakEpoch 
].

DateAndTime class ~> fromString: aString
[


	^ self readFrom: (ReadStream on: aString)
 
].

DateAndTime class ~> julianDayNumber: aJulianDayNumber
[

	^ self
		julianDayNumber: aJulianDayNumber
		seconds: 0
		nanoseconds: 0
		offset: self localOffset 
].

DateAndTime class ~> localOffset
[
	"Answer the duration we are offset from UTC"

	^ self localTimeZone offset
 
].

DateAndTime class ~> localTimeZone
[
	"Answer the local time zone"

	^ LocalTimeZone ifNil: [ LocalTimeZone := TimeZone default ]

 
].

DateAndTime class ~> localTimeZone: aTimeZone
[
	"Set the local time zone"

	"
	DateAndTime localTimeZone: (TimeZone offset:  0 hours name: 'Universal Time' abbreviation: 'UTC').
	DateAndTime localTimeZone: (TimeZone offset: -8 hours name: 'Pacific Standard Time' abbreviation: 'PST').
	"

	LocalTimeZone := aTimeZone


 
].

DateAndTime class ~> midnight
[

	^ self now midnight
 
].

DateAndTime class ~> new
[
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self epoch
	 
].

DateAndTime class ~> noon
[

	^ self now noon 
].

DateAndTime class ~> readFrom: aStream
[

	| offset date time ch |

	date := Date readFrom: aStream.
	[aStream peek isDigit]
		whileFalse: [aStream next].
	time := Time readFrom: aStream.
	aStream atEnd
		ifTrue: [ offset := self localOffset ]
		ifFalse: [
			ch := aStream next.
			ch = $+ ifTrue: [ch := Character space].
			offset := Duration fromString: ch asString, '0:', aStream upToEnd, ':0'].
		
	^ self
		year: date yearNumber
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset


	"
		DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.02+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.003+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0004+01:01' readStream
  		DateAndTime readFrom: ' 2002-05-16T17:20:45.00005' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000006+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000007+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.00000008-01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000000009+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000000001+01:01' readStream

 		DateAndTime readFrom: ' 2002-05-16T17:20' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45+01:57' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45-02:34' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45+00:00' readStream
		DateAndTime readFrom: ' 1997-04-26T01:02:03+01:02:3' readStream
 	" 
].

DateAndTime class ~> today
[

	^ self midnight
 
].

DateAndTime class ~> tomorrow
[

	^ self today date next start 
].

DateAndTime class ~> year: year day: dayOfYear
[
	"Return a DateAndTime"

	^ self
		year: year
		day: dayOfYear
		hour: 0
		minute: 0
		second: 0 
].

DateAndTime class ~> year: year month: month day: day
[
	"Return a DateAndTime, midnight local time" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: 0
		minute: 0 
].

DateAndTime class ~> year: year month: month day: day hour: hour minute: minute
[

	"Return a DateAndTime" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0 
].

DateAndTime class ~> year: year month: month day: day hour: hour minute: minute second: second nanoSecond: nanoCount offset: offset
[
	"Return a DateAndTime"

	| monthIndex daysInMonth p q r s julianDayNumber |

	monthIndex := month isInteger ifTrue: [month] ifFalse: [Month indexOfMonth: month].
	daysInMonth := Month
		daysInMonth: monthIndex
		forYear: year.
	day < 1 ifTrue: [self error: 'day may not be zero or negative'].
	day > daysInMonth ifTrue: [self error: 'day is after month ends']. 	
	
	p := (monthIndex - 14) quo: 12.
	q := year + 4800 + p.
	r := monthIndex - 2 - (12 * p).
	s := (year + 4900 + p) quo: 100.

	julianDayNumber :=
 		( (1461 * q) quo: 4 ) +
			( (367 * r) quo: 12 ) -
 				( (3 * s) quo: 4 ) +
 					( day - 32075 ).

	^self basicNew
		setJdn: julianDayNumber 
		seconds: hour * 60 + minute * 60 + second
		nano: nanoCount
		offset: offset;
		yourself 
].

DateAndTime class ~> yesterday
[

	^ self today date previous start 

].

DateAndTime class ~> fromSeconds: seconds 
[
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| integerSeconds nanos |
	integerSeconds := seconds truncated.
	nanos := integerSeconds = seconds
		ifTrue: [ 0 ]
		ifFalse: [ (seconds - integerSeconds * Time nanosInSecond) asInteger ].
	^ self
		julianDayNumber: Time squeakEpoch
		seconds: integerSeconds
		nanoseconds: nanos
		offset: self localOffset 

].

DateAndTime class ~> initialize
[
	"
	self initialize
	"
	self initClassCachedState 

].

DateAndTime class ~> julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset
[

	^self basicNew julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset 
].

DateAndTime class ~> nowUpToSeconds
[
	"Resolution is up to one second. Don't use as a TimeStamp
	DateAndTime nowUpToSeconds
	"

	| days seconds remainingSeconds |
	seconds := Time localMicrosecondClock // 1000000.

	days := seconds // Time secondsInDay.
	remainingSeconds := seconds \\ Time secondsInDay.
	^ self basicNew
		setJdn: Time squeakEpoch + days 
		seconds: remainingSeconds 
		nano: 0 
		offset: self localOffset 
].

DateAndTime class ~> unixTimeSeconds: secsSinceUnixEpoch
[
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds 
].

DateAndTime class ~> unixTimeSeconds: secsSinceUnixEpoch milliseconds: millisecs
[
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds + millisecs milliSeconds 

].

DateAndTime class ~> initClassCachedState
[

	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0 
].

DateAndTime class ~> releaseClassCachedState
[

	LastTickSemaphore := nil.
	LastTick := nil.
	LocalTimeZone := nil 

].

DateAndTime class ~> unixEpoch
[
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	(DateAndTime now - DateAndTime unixEpoch) days / 365.25
	"
	^ self
		julianDayNumber: 2440588
		seconds: 0
		nanoseconds: 0
		offset: Duration zero 
].

DateAndTime initialize.

"		E O F 		"
