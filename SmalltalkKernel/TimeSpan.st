"FILE: 'Time.st'
 IMPLEMENTS: Classes TimeSpan Date Year Month Week; TimeZone
 AUTHOR: Cuis [Juan Vuletich; MIT Licence]
 Modifications by: Ken Dickey
 DATE: 03 Febuary 2017
"
 
Object   newSubclassName: #Timespan iVars: #(start duration) cVars: nil.
Timespan newSubclassName: #Date  iVars: nil cVars: nil.
Timespan newSubclassName: #Year  iVars: nil cVars: nil.
Timespan newSubclassName: #Month iVars: nil cVars: nil.
Timespan newSubclassName: #Week  iVars: nil cVars: nil.
Object   newSubclassName: #TimeZone iVars: #(offset abbreviation name) cVars: nil.


" === Class Timespan === "

Timespan category: #'Kernel-Chronology'.

Timespan comment:
'I represent a duration starting on a specific DateAndTime.'.

Timespan ~> is: aSymbol
[
  ^aSymbol == #Timespan or: [ super is: aSymbol ]
].

Timespan ~> + aDuration
[
  ^self class classDefinesDuration
	ifTrue: [  self class including: self start + aDuration ]
	ifFalse: [ self class starting: self start + aDuration duration: duration ]
].

Timespan ~> - aDuration
[
  ^self class classDefinesDuration
	ifTrue: [  self class including: self start - aDuration ]
	ifFalse: [ self class starting: self start - aDuration duration: duration ]
].

Timespan ~> = comparand
[
  self == comparand ifTrue: [ ^ true ].
  ^ self class == comparand class 
 	and: [ self start = comparand start ]
	and: [ self duration = comparand duration ]
].

Timespan ~> hash
[
  ^ self start hash + self duration hash
].


Timespan ~> end
[
	^ self duration totalNanoSeconds = 0
		ifTrue: [ self start ]
		ifFalse: [ self next start - DateAndTime clockPrecision ]
].

Timespan ~> includes: aDateAndTime
[

  ^ (aDateAndTime is: #Timespan)
	ifTrue: [ (self includes: aDateAndTime start)
		  and: [ self includes: aDateAndTime end ] ]
	ifFalse: [ aDateAndTime between: self start and: self end ]
].

Timespan ~> includesAllOf: aCollection 
[
"Answer whether all the elements of aCollection are in the receiver."

  aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
  ^ true
].

Timespan ~> includesAnyOf: aCollection 
[
"Answer whether any element of aCollection is included in the receiver"

  aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].
  ^false
].

Timespan ~> intersection: aTimespan
[
 "Return the Timespan both have in common, or nil"
 "Warning: There's something possibly wrong here. See 
  http://lists.gforge.inria.fr/pipermail/pharo-project/2011-May/048253.html
	
It seems reasonable to expect this to answer true. However it answers false:
 |start end span|
 start :=DateAndTime now.
 end := start + 1 hour.
 span := Timespan starting: start ending: end.
 (span intersection: span) = span
"
  | aBegin anEnd |
   aBegin := self start max: aTimespan start.
   anEnd := self end min: aTimespan end.
   anEnd < aBegin ifTrue: [^nil].

   ^ Timespan starting: aBegin ending: anEnd
].

Timespan ~> printOn: aStream
[

  super printOn: aStream.
  aStream 
	nextPut: $(;
	print: self start;
	nextPut: $D;
	print: self duration;
	nextPut: $).
].


Timespan ~> to: anEnd
[
"Answer an Timespan. anEnd must be aDateAndTime"

"@@@Fixme self assert: (anEnd is: #DateAndTime)."
  ^ Timespan starting: self start ending: anEnd
].

Timespan ~> union: aTimespan
[
 "Return the Timespan spanned by both"
"
| union timespan |
timespan := Timespan starting:
	(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0)
			duration: (Duration hours: 100).
union := timespan union: timespan.
	
self 
  assert: (union start = timespan start);
  assert: (union duration = timespan duration)
"

| aBegin anEnd |

  aBegin := self start min: aTimespan start.
  anEnd := self end max: aTimespan end.
  ^ Timespan starting: aBegin ending: anEnd + DateAndTime clockPrecision
].


Timespan ~> every: aDuration do: aBlock
[
  | element end |
  element := self start.
  end := self end.
  [ element <= end ] whileTrue:
    [ aBlock value: element.
	element := element + aDuration. 
    ]
].

Timespan ~> workDatesDo: aBlock
[
"Exclude Saturday and Sunday"
  self do: aBlock with: start date when: [ :d | d dayOfWeek < 6 ].
].


Timespan ~> do: aBlock with: aFirstElement
[
  self do: aBlock with: aFirstElement when: [ :t | true ].
].

Timespan ~> do: aBlock with: aFirstElement when: aConditionBlock
[
  | element end |
  element := aFirstElement.
  end := self end.
  [ element start <= end ] whileTrue:
    [(aConditionBlock value: element)
	ifTrue: [ aBlock value: element ].
    element := element next. 
    ]
].

Timespan ~> next
[
  | myStart myDuration |
  myStart := self start.
  myDuration := self duration.
  ^self class classDefinesDuration
    ifTrue:  [ self class including: myStart + myDuration ]
    ifFalse: [ self class starting: myStart + myDuration duration: myDuration ]
].

Timespan ~> previous
[
  | myStart myDuration |
  myStart := self start.
  myDuration := self duration.
  ^self class classDefinesDuration
	ifTrue:  [ self class including: myStart - myDuration ]
	ifFalse: [ self class starting: myStart - myDuration duration: myDuration ]
].


Timespan ~> is: aSymbol
[
  ^aSymbol == #Timespan or: [ super is: aSymbol ]
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Timespan class ~> current
[
"
Date current
Week current
Month current
"

  ^ self including: DateAndTime now
].

Timespan class ~> new
[
"Answer a Timespan starting on the Squeak epoch: 1 January 1901"
"Perhaps you need
	Timespan current
"
  ^ self shouldNotImplement
].

Timespan class ~> starting: aDateAndTime duration: aDuration
[
  self classDefinesDuration ifTrue: [
	self shouldNotImplement ].
"@@fixme  self assert: (aDateAndTime is: #DateAndTime)."

  ^ self basicNew
 	start: aDateAndTime;
	duration: aDuration;
	yourself
].

Timespan class ~> start: aDateAndTime duration: aDuration
[
  ^ self starting: aDateAndTime duration: aDuration
].


Timespan class ~> classDefinesDuration
[
"Answer yes if we must have a fixed duration.
Examples of this are Date, Month, Week, Year"
  ^ false
].

Timespan class ~> mustEncompassWholeDates
[
"Answer yes if we must start and end at midnight.
Examples of this are Date, Month, Week, Year"
  ^ false
].


Timespan class ~> including: aDateAndTime
[

  ^ self starting: aDateAndTime duration: Duration zero
].

Timespan class ~> starting: startDateAndTime ending: endDateAndTime
[
  self classDefinesDuration ifTrue: [ self shouldNotImplement ].
"@@@fixme self assert: (startDateAndTime is: #DateAndTime).
 @@@fixme self assert: (endDateAndTime is: #DateAndTime)."
  ^ self starting: startDateAndTime 
	 duration: endDateAndTime - startDateAndTime
].

" === Class Date === "

Date category: 'Kernel-Chronology'.

Date comment: 
'Instances of Date are Timespans with duration of 1 day.
Their default creation assumes a start of midnight in the local time zone.'.

Date ~> is: aSymbol
[
  ^aSymbol == #Date or: [ super is: aSymbol ]
].

Date ~> mmddyyyy
[
"Answer the receiver rendered in standard fmt mm/dd/yyyy. 
 Good for avoiding year 2000 bugs. 
 Note that the name here is slightly misleading --
 the month and day numbers don't show leading zeros,
 so that for example feb 1 1996 is 2/1/96"

"Date today mmddyyyy"
  ^ self printFormat: #(2 1 3 $/ 1 1)
].

Date ~> printFormat: formatArray 
[
"Answer a String describing the receiver using the format denoted by the 
 argument, formatArray."

  | aStream |
  aStream := WriteStream on: (String new: 16).
  self printOn: aStream format: formatArray.
  ^aStream contents
].

Date ~> printOn: aStream
[
  self printOn: aStream format: #(1 2 3 $  3 1 )
].

Date ~> printOn: aStream format: formatArray 
[
"Print a description of the receiver on aStream using the format 
 denoted the argument, formatArray: 
	
	#(item item item sep monthfmt yearfmt twoDigits) 
	
	items: 1=day 2=month 3=year will appear in the order given, 
	
	separated by sep which is eaither an ascii code or character. 
	
	monthFmt: 1=09 2=Sep 3=September 
	
	yearFmt: 1=1996 2=96 
	
	digits: (missing or)1=9 2=09. 
	
See the examples in printOn: and mmddyy"
  | gregorian twoDigits element monthFormat |
  gregorian := self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
  twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
  1 to: 3 do: 
    [ :i | 
	element := formatArray at: i.
	element = 1
		ifTrue: [twoDigits
			ifTrue: [aStream
					nextPutAll: (gregorian first asString
								padded: #left
								to: 2
								with: $0)]
			ifFalse: [gregorian first printOn: aStream]].
	element = 2
		ifTrue: [monthFormat := formatArray at: 5.
			monthFormat = 1
				ifTrue: [twoDigits
					ifTrue: [aStream
							nextPutAll: (gregorian middle asString
									padded: #left
									to: 2
									with: $0)]
					ifFalse: [gregorian middle printOn: aStream]
				].
			monthFormat = 2
				ifTrue: [aStream
						nextPutAll: ((Month nameOfMonth: gregorian middle)
								copyFrom: 1
								to: 3)
				].
			monthFormat = 3
				ifTrue: [aStream nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6) = 1
					ifTrue: [gregorian last printOn: aStream]
					ifFalse: [aStream
							nextPutAll: ((gregorian last \\ 100) asString
									padded: #left
									to: 2
									with: $0)]
				].
			i < 3
				ifTrue: [(formatArray at: 4) ~= 0
					ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]
				]
    ]

].

Date ~> storeOn: aStream
[

  aStream print: self printString; nextPutAll: ' asDate'
].

Date ~> yyyymmdd
[
"Format the date in ISO 8601 standard like '2002-10-22'."

  ^ self printFormat: #(3 2 1 $- 1 1 2)
].


Date ~> previous: dayName 
[
"Answer the previous date whose weekday name is dayName."

  | n |
  n := 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
  n = 0 ifTrue: [ n := 7 ].
  ^ self - n days
].

Date ~> weekday
[
  "Answer the name of the day of the week on which the receiver falls."

  ^ self dayOfWeekName
].

Date ~> weekdayIndex
[
"Sunday=1, ... , Saturday=7"

  ^ self dayOfWeek
].


Date ~> < aDate
[
"@@fixme self assert: aDate class == Date."
  ^ self start < aDate start
].

Date ~> dayMonthYearDo: aBlock 
[
"Supply integers for day, month and year to aBlock and return the result"

  ^ self start dayMonthYearDo: aBlock
].


Date ~> dayOfMonth
[
"Answer the day of the month represented by the receiver."

  ^ self start dayOfMonth
].

Date ~> dayOfWeek
[
"Answer the day of the week represented by the receiver."

  ^ self start dayOfWeek
].

Date ~> dayOfWeekName
[
"Answer the day of the week represented by the receiver."

  ^ self start dayOfWeekName
].

Date ~> dayOfYear
[
"Answer the day of the year represented by the receiver."

  ^ self start dayOfYear
].

Date ~> daysInMonth
[
  ^ self start daysInMonth
].

Date ~> daysInYear
[
"Answer the number of days in the month represented by the receiver."

  ^ self start daysInYear
].

Date ~> daysLeftInYear
[
  ^ self start daysLeftInYear
].

Date ~> firstDayOfMonth
[
  ^ self start firstDayOfMonth
].

Date ~> isLeapYear
[

  ^ self start isLeapYear
].

Date ~> julianDayNumber
[

  ^ self start julianDayNumber
].

Date ~> month
[

  ^start month
].

Date ~> monthAbbreviation
[

  ^ self start monthAbbreviation
].

Date ~> monthIndex
[
  ^ self start monthIndex
].

Date ~> monthName
[
  ^ self start monthName
].

Date ~> secondsSinceSqueakEpoch
[
"Answer the seconds since the Squeak epoch: 1 January 1901"

  ^ self start secondsSinceSqueakEpoch
].

Date ~> week
[
  ^start week
].

Date ~> year
[
  ^start year
].

Date ~> yearNumber
[
  ^ self start yearNumber
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Date class ~> dayOfWeek: dayName 
[
  ^ Week indexOfDay: dayName
].

Date class ~> daysInMonth: monthName forYear: yearInteger 
[
  ^ Month daysInMonth: monthName forYear: yearInteger.
].

Date class ~> daysInYear: yearInteger 
[
  ^ Year daysInYear: yearInteger.
].

Date class ~> firstWeekdayOfMonth: month year: year
[
"Answer the weekday index of the first day in <month> in the <year>."

  ^ (self newDay: 1 month: month year: year) weekdayIndex

].

Date class ~> fromDays: dayCount 
[
"Days since 1 January 1901"

  ^ self julianDayNumber: dayCount + Time squeakEpoch
].

Date class ~> fromSeconds: seconds
[
"Answer an instance of me which is 'seconds' seconds after January 1, 1901."

  ^ self fromDays: ((Duration seconds: seconds) days)
].

Date class ~> indexOfMonth: aMonthName 
[
  ^ Month indexOfMonth: aMonthName.
].

Date class ~> leapYear: yearInteger 
[
  ^ Year leapYear: yearInteger
].

Date class ~> nameOfDay: dayIndex 
[
  ^ Week nameOfDay: dayIndex 
].

Date class ~> nameOfMonth: anIndex 
[
  ^ Month nameOfMonth: anIndex.
].

Date class ~> newDay: day month: month year: year 
[
  ^ self year: year month: month day: day
].

Date class ~> newDay: dayCount year: yearInteger
[
  ^ self year: yearInteger day: dayCount
].

Date class ~> today
[
  ^ DateAndTime now date
].


Date class ~> fromString: aString
[
"Answer an instance of created from a string with format dd.mm.yyyy."

  ^ self readFrom: aString readStream.

].

Date class ~> julianDayNumber: aJulianDayNumber
[
  ^ (DateAndTime julianDayNumber: aJulianDayNumber) date
].

Date class ~> readFrom: aStream 
[
"Read a Date from the stream in any of the forms:  
	<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
	<month> <day> <year>		(April 15, 1982; 4/15/82)
	<year>-<month>-<day>			(1982-04-15) (ISO8601)"
  | day month year parsedNumber prefix |
  aStream peek = $-
	ifTrue: [prefix := -1]
	ifFalse: [prefix := 1].
  [aStream peek isAlphaNumeric]
	whileFalse: [aStream skip: 1].
  aStream peek isDigit
	ifTrue: [
		parsedNumber := (Integer readFrom: aStream) * prefix.
		(parsedNumber < 0 or: [parsedNumber > 31])
			ifTrue: [year := parsedNumber]].
  [aStream peek isAlphaNumeric]
	whileFalse: [aStream skip: 1].
  aStream peek isLetter
	ifTrue: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
		month := WriteStream on: (String new: 10).
		[aStream peek isLetter]
			whileTrue: [month nextPut: aStream next].
		month := month contents.
		[aStream peek isAlphaNumeric]
			whileFalse: [aStream skip: 1].
		parsedNumber
			ifNil: ["MM DD YY"
				day := Integer readFrom: aStream]
			ifNotNil: [
				year
				ifNil: ["DD MM YY"
					day := parsedNumber]
			]
	]
	ifFalse: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
		year ifNil: ["MM-DD-YY or DD-MM-YY"
			parsedNumber > 12
			ifTrue: ["DD-MM-YY"
				day := parsedNumber.
				month := Month nameOfMonth: (Integer readFrom: aStream)
			]
			ifFalse: ["MM-DD-YY"
				month := Month nameOfMonth: parsedNumber.
				day := Integer readFrom: aStream
			]
		]
		ifNotNil: ["YY-MM-DD"
			month := Month nameOfMonth: (Integer readFrom: aStream)]].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	year 
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0]) 
		ifTrue: [
			year < 69 
				ifTrue: [ year := 2000 + year]
				ifFalse: [year := 1900 + year]
		].

  ^ self
	year: year
	month: month
	day: day
].

Date class ~> tomorrow
[
  ^ self today next
].

Date class ~> year: year day: dayOfYear
[

  ^ (DateAndTime year: year day: dayOfYear) date
].

Date class ~> year: year month: month day: day
[
  ^ (DateAndTime year: year month: month day: day) date
].

Date class ~> yesterday
[
  ^ self today previous
].


Date class ~> dateAndTimeNow
[
"Answer an Array whose first element is Date today and second element 
 is Time now."

  ^Time dateAndTimeNow
].

Date class ~> easterDateFor: year
[

"  compute the easter date.
   source: Physikalisch-Technische Bundesanstalt Braunschweig.
   Lichtenberg, H.: Zur Interpretation der Gaussschen Osterformel
                    und ihrer Ausnahmeregeln,
                    Historia Mathematica 24 (1997), pp. 441-444
  
   http://www.ptb.de/de/org/4/44/441/oste.htm
 "

  | k m s a d r og sz oe day |

  k := year // 100.
  m := 15 + (3*k + 3//4) - (8*k + 13//25).
  s := 2 - (3*k + 3// 4).
  a := year \\ 19.
  d := 19*a + m \\ 30.
  r := d//29 + ((d//28) - (d//29)* (a// 11)).

  og := 21 + d - r.
  sz := 7 - (year//4 + year + s\\7).
  oe := 7 - (og - sz\\7).
  day := og + oe.
  ^day <= 31
    ifTrue:  [Date newDay: day      month: 3 year: year ]
    ifFalse: [Date newDay: day - 31 month: 4 year: year].
].

Date class ~> orthodoxEasterDateFor: year
[
 "  compute the easter date according to the rules of the orthodox calendar.
    source: 
    http://www.smart.net/~mmontes/ortheast.html 
  "
     | r1 r2 r3 r4 ra rb r5 rc date |

    r1 := year \\ 19.
    r2 := year \\ 4.
    r3 := year \\ 7.
    ra := 19*r1 + 16.
    r4 := ra \\ 30.
    rb := r2 + r2 + (4*r3) + (6*r4).
    r5 := rb \\ 7.
    rc := r4 + r5.
    date := Date newDay: 3 month: 4 year: year.
    ^date + rc days
].


Date class ~> classDefinesDuration
[
  ^ true
].

Date class ~> mustEncompassWholeDates
[
  ^ true
].


Date class ~> including: aDateAndTime
[

  ^self basicNew
 	start: aDateAndTime midnight;
	duration: (Duration days: 1);
	yourself
].

" === Class Year === "

Year comment: 'I represent a year.'.


Year ~> is: aSymbol
[
  ^aSymbol == #Year or: [ super is: aSymbol ]
].

Year ~> daysInMonth
[
  self shouldNotImplement 
].

Year ~> daysInYear
[
  ^ self duration days
].

Year ~> printOn: aStream
[
  aStream nextPutAll: 'a Year ('.
  self start yearNumber printOn: aStream.
  aStream nextPutAll: ')'.
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Year class ~> current
[
   ^ self yearNumber: (DateAndTime now yearNumber)
].

Year class ~> including: aDateAndTime
[
"Warning: a Year can start at any date (different from Month and Week)"
  | midnight |
  midnight := aDateAndTime midnight.

  ^ self basicNew
 	start: midnight;
	duration: (Duration days: (self daysInYear: midnight yearNumber));
	yourself
].

Year class ~> isLeapYear: aYearInteger
[
  | adjustedYear |
  adjustedYear := aYearInteger > 0
  	ifTrue:  [aYearInteger]
	ifFalse: [(aYearInteger + 1) negated].

  "There was no year 0"
  ^ ((adjustedYear \\ 4 ~= 0) 
	or: [(adjustedYear \\ 100 = 0) 
		and: [adjustedYear \\ 400 ~= 0]]) not.
].

Year class ~> yearNumber: aYear
[
  ^ self including: (DateAndTime year: aYear month: 1 day: 1).
].


Year class ~> daysInYear: yearInteger
[
  ^ 365 + ((self isLeapYear: yearInteger) ifTrue: [1] ifFalse: [0]).
].

Year class ~> leapYear: yearInteger 
[
  ^ (self isLeapYear: yearInteger)
	ifTrue: [1]
	ifFalse: [0]
].


Year class ~> classDefinesDuration
[
  ^ true
].

Year class ~> mustEncompassWholeDates
[
  ^ true
].

" === Class Month === "

Month comment: 'I represent a month.'.


Month ~> is: aSymbol
[
  ^aSymbol == #Month or: [ super is: aSymbol ]
].

Month ~> daysInMonth
[
  ^ self duration days
].


Month ~> name
[
  ^ self monthName
].


Month ~> printOn: aStream
[
  aStream nextPutAll: self monthName, ' ', self yearNumber printString
].


Month ~> daysInYear
[
"Answer the number of days in the month represented by the receiver."

  ^ self start daysInYear
].

Month ~> isLeapYear
[
  ^ self start isLeapYear
].

Month ~> monthAbbreviation
[
  ^ self start monthAbbreviation
].

Month ~> monthIndex
[
  ^ self start monthIndex
].

Month ~> monthName
[
  ^ self start monthName
].

Month ~> year
[
  ^ self start year
].

Month ~> yearNumber
[
  ^ self start yearNumber
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Month class ~> including: aDateAndTime
[
"Months start at day 1"
  | monthStart days |
  monthStart := DateAndTime
			year:  aDateAndTime yearNumber
			month: aDateAndTime monthIndex
			day: 1.
  days := self daysInMonth: monthStart monthIndex forYear: monthStart yearNumber.
  ^ self basicNew
	start: monthStart;
	duration: (Duration days: days);
	yourself
].

Month class ~> month: month year: year
[
"Create a Month for the given <year> and <month>.
 <month> may be a number or a String with the
 name of the month. <year> should be with 4 digits."

  ^ self including: (DateAndTime year: year month: month day: 1)
].

Month class ~> readFrom: aStream
[
  | m y c |
  m := (ReadWriteStream with: '') reset.
  [(c := aStream next) isSeparator] whileFalse: [m nextPut: c].
  [(c := aStream next) isSeparator] whileTrue.
  y := (ReadWriteStream with: '') reset.
  y nextPut: c.
  [aStream atEnd] whileFalse: [y nextPut: aStream next].

  ^ self 
	month: m contents
	year: y contents asInteger

"Month readFrom: 'July 1998' readStream"
].


Month class ~> daysInMonth: indexOrName forYear: yearInteger 
[
  | index |
  (index := indexOrName isInteger)
	ifTrue: [indexOrName]
	ifFalse: [self indexOfMonth: indexOrName].
^ (self daysInMonth at: index)
	+ ((index = 2 and: [Year isLeapYear: yearInteger])
		ifTrue: [1] ifFalse: [0])
].

Month class ~> indexOfMonth: aMonthName
[
  1 to: 12 do: [ :i |  (aMonthName, '*' match: (self monthNames at: i)) ifTrue: [^i] ].
  self error: aMonthName , ' is not a recognized month name'
].

Month class ~> nameOfMonth: anIndex
[
  ^ self monthNames at: anIndex
].


Month class ~> classDefinesDuration
[
  ^ true
].

Month class ~> daysInMonth
[
  ^ #(31 28 31 30 31 30 31 31 30 31 30 31)
].

Month class ~> monthNames
[
  ^ #(January February March April May June July August September October November December)
].

Month class ~> mustEncompassWholeDates
[
  ^ true
].


" === Class Week === "

Week comment: 'I represent a week.'.


Week ~> is: aSymbol
[
  ^aSymbol == #Week or: [ super is: aSymbol ]
].

Week ~> printOn: aStream
[
  aStream nextPutAll: 'a Week starting: '.
  self start printOn: aStream.
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

Week class ~> including: aDateAndTime
[
"Week will start from the 
	Week startDay
"
  | midnight weekStart |
  midnight := aDateAndTime midnight.
  weekStart := midnight - (midnight dayOfWeek - 1) days.

  ^ self basicNew
 	start: weekStart;
	duration: (Duration weeks: 1);
	yourself
].

Week class ~> indexOfDay: aSymbol
[
  ^ self dayNames indexOf: aSymbol
].


Week class ~> nameOfDay: anIndex
[
  ^ self dayNames at: anIndex
].


Week class ~> classDefinesDuration
[
  ^ true
].

Week class ~> dayNames
[
  ^ #(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)
].

Week class ~> mustEncompassWholeDates
[
  ^ true
].



" === Class TimeZome === "

TimeZone comment: 
'TimeZone is a simple class to colect the information identifying a UTC time zone.

offset		-	Duration	- the time zone''s offset from UTC
abbreviation	-	String		- the abbreviated name for the time zone.
name		-	String		- the name of the time zone.

TimeZone class >> #timeZones returns an array of the known time zones
TimeZone class >> #default returns the default time zone (Grenwich Mean Time)'.


TimeZone ~> is: aSymbol
[
  ^aSymbol == #TimeZone or: [ super is: aSymbol ]
].

TimeZone ~> printOn: aStream
[
  super printOn: aStream.
  aStream
	nextPut: $(;
	nextPutAll: self abbreviation;
	nextPut: $).
].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

TimeZone class ~> default
[
"Answer the default time zone - UTC"

  ^ self utc
].

TimeZone class ~> timeZones
[ "@@@FIXME: add others"
  ^ {
  self offset:  0 hours name: 'Universal Time'              abbreviation: 'UTC'.
  self offset:  0 hours name: 'Greenwich Mean Time'         abbreviation: 'GMT'.
  self offset:  1 hours name: 'British Summer Time'         abbreviation: 'BST'.
  self offset:  2 hours name: 'South African Standard Time' abbreviation: 'SAST'.
  self offset: -8 hours name: 'Pacific Standard Time'       abbreviation: 'PST'.
  self offset: -7 hours name: 'Pacific Daylight Time'       abbreviation: 'PDT'.
  }
].

TimeZone class ~> utc
[
"Answer the default time zone - UTC"

  ^ self timeZones detect: [ :tz | tz abbreviation = 'UTC' ]
].


TimeZone class ~> offset: aDuration name: aName abbreviation: anAbbreviation
[
  ^ self new
	offset: aDuration;
	name: aName;
	abbreviation: anAbbreviation;
	yourself
].



"		E O F 		"

