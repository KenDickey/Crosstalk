;;; FILE: "st-date-time.sls"
;;; IMPLEMENTS: PointInTime, DateAndTime, Duration
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017; March 2025

(library (st-date-time)

  (export
   DateAndTime
   PointInTime
   Duration
   )
  
  (import
   (rnrs base)
   (srfi: 19)
   (st-base)
   (st-class-structure)
   (st-metaclass)
   (st-behavior)
   (st-collection)
   (st-sequence-coll)
   )

;; Date+Time has many nuanced representations.
;; Scheme and Smalltalk diverge somewhat in usage.
;; Squeak adds confusion with a TimeOfDay class named #Time.
;; Also Squeak differs from ANSI in use of #Timespan and subclasses.

;; Scheme/SRFI-19's idea of Time is here a PointInTime,
;; which is convertable to DateAndTime.  Smalltalk has no
;; corresponding class, but the PointInTime class is exposed
;; in case it is observed "in the wild" in Smalltalk.


(define DateAndTime
  (newSubclassName:iVars:cVars:
   Magnitude
   'DateAndTime '() '())
)

(define PointInTime
  (newSubclassName:iVars:cVars:
   Magnitude
   'PointInTime '() '())
)

(define Duration
  (newSubclassName:iVars:cVars:
   Magnitude
   'Duration '() '())
)

;;;======================================================
;;; R6RS Libraries: Definitions before Expressions
;;;======================================================


(perform:with: DateAndTime 'methodDict: st-date+time-behavior)

(perform:with: PointInTime 'methodDict: st-time-behavior)

(perform:with: Duration    'methodDict: st-duration-behavior)


(perform:with:
     DateAndTime
     'category:
     'Kernel-Chronology)

(perform:with:
     PointInTime
     'category:
     'Kernel-Chronology)

(perform:with:
     Duration
     'category:
     'Kernel-Chronology)

(perform:with:
     DateAndTime
     'comment:
"I represent a point in UTC time as defined by ISO 8601. I have zero duration."
)

(perform:with:
     PointInTime
     'comment:
"This represents a particular point in time and is convertable to DateAndTime.
 My instances are used internally to calculate durations."
)

(perform:with:
     Duration
     'comment:
     "I represent a duration of time. I have nanosecond precision"
)

(addSelector:withMethod:
     DateAndTime
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'DateAndTime)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     PointInTime
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'PointInTime)
           (superPerform:with: self 'is: symbol))))

(addSelector:withMethod:
     Duration
     'is:
     (lambda (self symbol)
       (or (eq? symbol 'Duration)
           (superPerform:with: self 'is: symbol))))

;;; Duration

(define (time-duration? thing)
  (and (time? thing)
       (eq? 'time-duration (time-type thing))))

(define (duration-check thing)
  (unless (time-duration? thing)
    (error "Expected a Duration:" thing)))

(addSelector:withMethod:
     (class Duration)
     'seconds:nanoSeconds:
     (lambda (self seconds nanos)
       (let* ( (whole-secs (exact (truncate seconds)))
               (frac-secs (- seconds whole-secs))
             )
       (make-time 'time-duration
                  (+ nanos (exact (round (* frac-secs nanos/sec))))
                  whole-secs))))

(addSelector:withMethod:
     (class Duration)
     'seconds:
     (lambda (self seconds)
       (let* ( (whole-secs (exact (truncate seconds)))
               (frac-secs (- seconds whole-secs))
             )
       (make-time 'time-duration
                  (exact (round (* frac-secs nanos/sec)))
                  whole-secs))))

(addSelector:withMethod:
     (class Duration)
     'hours:
     (lambda (self hours)
       (make-time 'time-duration 0 (* hours secs/hour))))

(addSelector:withMethod:
     Integer ;; Nota Bene!
     'hours
     (lambda (self)
       (make-time 'time-duration 0 (* self secs/hour))))

(addSelector:withMethod:
     Duration
     'seconds:
     (lambda (self seconds)
       (set-time-second! self seconds)))

(addSelector:withMethod:
     Duration
     'nanoSeconds:
     (lambda (self nanoSeconds)
       (set-time-nanoSecond! self nanoSeconds)))

(addSelector:withMethod:
     Duration
     'totalSeconds
     (lambda (self)
       (time-second self)))

(addSelector:withMethod:
     Duration
     'nanoSeconds
     (lambda (self)
       (time-nanosecond self)))

(addSelector:withMethod:
     Duration
     'copy
     (lambda (self)
       (copy-time self)))

(addSelector:withMethod:
     Duration
     'zero
     (lambda (self)
       (make-time 'time-duration 0 0)))

(define secs/min 60)
(define secs/hour (* 60 secs/min))
(define secs/day  (* 24 secs/hour))
(define nanos/sec 1000000000)
(define (pad-two n)
  (let ( (s (number->string n)) )
    (if (= 1 (string-length s))
        (string-append "0" s)
        s)))

(addSelector:withMethod:
     Duration
     'printOn:
     (lambda (self port)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ 
                         (if (negative? secs) (- secs) secs)
                         secs/day)) )
           (let-values ( ((hours hours-rem)
                          (truncate/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (truncate/ hours-rem secs/min)) )
               (display
                (string-append
                 (if (negative? secs) "-" "")
                 (number->string days)
                 ":"
                 (pad-two hours)
                 ":"
                 (pad-two minutes)
                 ":"
                 (if (zero? nanos)
                     (pad-two seconds)
                     (let ( (nanos-string (number->string (+ nanos nanos/sec))) )
                       (string-append
                        (pad-two seconds)
                        "."
                        (substring nanos-string 1 (string-length nanos-string))))))
                port)
     ) ) ) ) )
)
;; (make-time 'time-duration 789 273906)
;; (Duration days: 3 hours: 4 minutes: 5 seconds: 6 nanoSeconds: 789) printString
;; -->  '3:04:05:06.000000789'

(addSelector:withMethod:
     Duration
     'daysHoursMinutesSecondsNanosDo:
     (lambda (self aBlockClosure)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (truncate/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (truncate/ hours-rem secs/min)) )
               (aBlockClosure days hours minutes seconds nanos)
     ) ) ) ) )
)

(addSelector:withMethod:
     (class Duration)
     'days:hours:minutes:seconds:nanoSeconds:
     (lambda (self d h m s nanos)
       ($:: Duration
            'seconds:nanoSeconds:
            (+ (* d secs/day)
               (* h secs/hour)
               (* m secs/min)
               s)
            nanos))
)

(addSelector:withMethod:
     Duration
     'days
     (lambda (self)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ secs secs/day)) )
           days
     ) ) )
)

(addSelector:withMethod:
     Duration
     'hours
     (lambda (self)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (truncate/ days-rem secs/hour)) )
             hours
     ) ) ) )
)

(addSelector:withMethod:
     Duration
     'minutes
     (lambda (self)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (truncate/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (truncate/ hours-rem secs/min)) )
               minutes
     ) ) ) ) )
)

(addSelector:withMethod:
     Duration
     'seconds
     (lambda (self)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (truncate/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (truncate/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (truncate/ hours-rem secs/min)) )
               seconds
     ) ) ) ) )
)

(addSelector:withMethod:
     Duration
     'nanoSeconds
     (lambda (self) (time-nanosecond self)))


(addSelector:withMethod:
     Duration
     '-
     (lambda (self other)
       (if (time-duration? other)
           (make-time 'time-duration
                      (- (time-nanosecond self)
                         (time-nanosecond other))
                      (- (time-second self)
                         (time-second other)))
           (error "-: Expected a Duration" other))))


;;; PointInTime

(addSelector:withMethod:
     (class PointInTime)
     'now
     (lambda (self)
       (current-time)))

(addSelector:withMethod:
     PointInTime
     'copy
     (lambda (self)
       (copy-time self)))

(addSelector:withMethod:
     PointInTime
     'nanoSeconds
     (lambda (self)
       (time-nanosecond self)))

(addSelector:withMethod:
     PointInTime
     'seconds
     (lambda (self)
       (floor (/ (time-second self) secs/min))))

(addSelector:withMethod:
     PointInTime
     'totalSeconds
     (lambda (self)
       (time-second self)))

(define (point-in-time? thing)
  (and (time? thing)
       (eq? 'time-utc (time-type thing))))

(define (point-in-time-check thing)
  (unless (point-in-time? thing)
    (error "Expected a PointInTime:" thing)))

(addSelector:withMethod:
     PointInTime
     '<
     (lambda (self other)
       (point-in-time-check other)
       (time<? self other)))

(addSelector:withMethod:
     PointInTime
     '<=
     (lambda (self other)
       (point-in-time-check other)
       (time<=? self other)))

(addSelector:withMethod:
     PointInTime
     '=
     (lambda (self other)
       (point-in-time-check other)
       (time=? self other)))


(addSelector:withMethod:
     PointInTime
     '>
     (lambda (self other)
       (point-in-time-check other)
       (time>? self other)))

(addSelector:withMethod:
     PointInTime
     '>=
     (lambda (self other)
       (point-in-time-check other)
       (time>=? self other)))

(addSelector:withMethod:
     PointInTime
     '+
     (lambda (self other)
       (duration-check other)
       ;; Answers a PointInTime
       (add-duration self other)))

(addSelector:withMethod:
     PointInTime
     '-
     (lambda (self other)
       (cond 
        ((point-in-time? other)
         (time-difference self other)
         ;; Answers a Duration
         )
        ((time-duration? other)
         (time-utc->date (subtract-duration self other))
         ;; Answers a DateAndTime
         )
        ((date? other)
         (time-difference self (date->time-utc other))
         )
        (else
         (error "Expected a Duration or a PointInTime" other)))))

(addSelector:withMethod:
     PointInTime
     'asDateAndTime
     (lambda (self)
       (time-utc->date self)))

(addSelector:withMethod:
     PointInTime
     'printOn:
     (lambda (self port)
       ($: (time-utc->date self) 'printOn: port)))

;;; DateAndTime

;; (define weekday-names-short
;;   (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;; (define weekday-names
;;   (vector "Sunday" "Monday" "Tuesday" "Wednesday"
;;           "Thursday" "Friday" "Saturday"))

;; (define month-names-short
;;   (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
;;           "Aug" "Sep" "Oct" "Nov" "Dec"))

;; (define month-names
;;   (vector "January" "February" "March" "April" "May"
;;           "June" "July" "August" "September" "October"
;;           "November" "December"))

(define (check-date-and-time thing)
  (unless (date? thing)
    (error "Expected a DateAndTime:" thing)))

(addSelector:withMethod:
     (class DateAndTime)
     'now
     (lambda (self)
       (current-date)))

(addSelector:withMethod:
     DateAndTime
     'offset
     (lambda (self) ($: Duration 'seconds: (date-zone-offset self))))

(addSelector:withMethod:
     (class DateAndTime)
     'clockPrecision
     (lambda (self)
       ;; Answer a Duration with nanoSeconds precision
       ($:: Duration
            'seconds:nanoSeconds:
            0
            (time-resolution 'time-utc))))

(addSelector:withMethod:
     (class DateAndTime) ;; NB: a Class method
     'midnight
     (lambda (self)
;"Answer a DateAndTime starting at midnight local time"
       (let ( (dateNow (current-date)) )
         (make-date 0 ;   nanos
                    0 ;   seconds
                    0 ;   minutes
                    0 ;   hours
                    (date-day    dateNow)
                    (date-month  dateNow)
                    (date-year   dateNow)
                    (date-zone-offset dateNow)))))

(addSelector:withMethod:
     (class DateAndTime) ;; NB: a Class method
     'year:month:day:hour:minute:second:nanoSecond:offset:
     (lambda (self year month day hours minutes seconds nanos offset)
;"Answer a DateAndTime starting at midnight local time"
       (make-date nanos
                 seconds
                 minutes
                 hours
                 day
                 month
                 year
                 (cond
                  ((integer? offset) offset) ; srfi-19 is integer seconds
                  ((time? offset) (time-second offset))
                  (else (error "Expected offset to be a Duration" offset))))))

(addSelector:withMethod:
     (class DateAndTime) ;; NB: a Class method
     'year:month:day:hour:minute:second:
     (lambda (self year month day hours minutes seconds)
;"Answer a DateAndTime starting at midnight local time"
       (make-date 0
                 seconds
                 minutes
                 hours
                 day
                 month
                 year
                 (date-zone-offset (current-date)))))



(addSelector:withMethod:
     (class DateAndTime) ;; NB: a Class method
     'year:month:day:hour:minute:second:offset:
     (lambda (self year month day hours minutes seconds offset)
;"Answer a DateAndTime starting at midnight local time"
       (make-date 0
                 seconds
                 minutes
                 hours
                 day
                 month
                 year
                 (cond
                  ((integer? offset) offset) ; srfi-19 is integer seconds
                  ((time? offset) (time-second offset))
                  (else (error "Expected offset to be a Duration" offset))))))

(addSelector:withMethod:
     DateAndTime ;; NB: an Instance method
     'midnight
     (lambda (self)
       (make-date 0 ;   nanos
                  0 ;   seconds
                  0 ;   minutes
                  0 ;   hours
                  (date-day    self)
                  (date-month  self)
                  (date-year   self)
                  (date-zone-offset self))))

(addSelector:withMethod:
     (class DateAndTime) ;; NB: a Class method
     'localOffset
     (lambda (self)
       ($: Duration 'seconds: (date-zone-offset (current-date)))))

(addSelector:withMethod:
     DateAndTime
     'asPointInTime
     (lambda (self)
       (date->time-utc self)))

(addSelector:withMethod:
     DateAndTime
     'printString
     (lambda (self)
       ;; ANSI offset is a Duration
       (let* ( (offset-seconds (date-zone-offset self))
               (abs-offset     (abs offset-seconds))
               (date-string    (date->string self "~5"))
               ;; Above same as (date->string self "~Y-~m-~dT~H:~M:~S")
            )
         (if (zero? offset-seconds)
             date-string
             (let-values ( ((hours minutes)
                            (truncate/ abs-offset secs/hour)) )
               (string-append
                date-string
                (if (negative? offset-seconds) "-" "+")
                (pad-two hours)
                ":"
                (pad-two minutes)))))))

(addSelector:withMethod:
     DateAndTime
     'printOn:
     (lambda (self port)
       (display ($ self 'printString) port)))

(addSelector:withMethod:
     DateAndTime
     '-  ;; DateAndTime - (DateAndTime OR Duration)
     (lambda (self other)
       ($: ($ self 'asPointInTime)
           '-
           other)))

(addSelector:withMethod:
     DateAndTime
     '+  ;; DateAndTime + Duration -> DateAndTime
     (lambda (self other)
       ($ ($: ($ self 'asPointInTime)
              '+
              other)
          'asDateAndTime)))

(addSelector:withMethod:
     DateAndTime
     'nanoSecond
     (lambda (self)
       (date-nanosecond self)))

(addSelector:withMethod:
     DateAndTime
     'second
     (lambda (self)
       (date-second self)))

(addSelector:withMethod:
     DateAndTime
     'seconds
     (lambda (self)
       (date-second self)))

(addSelector:withMethod:
     DateAndTime
     'minute
     (lambda (self)
       (date-minute self)))

(addSelector:withMethod:
     DateAndTime
     'hour
     (lambda (self)
       (date-hour self)))

(addSelector:withMethod:
     DateAndTime
     'dayOfWeek ;; Sunday=1, Monday=2,..
     (lambda (self)
       (+ 1 (date-day self))))

(addSelector:withMethod:
     DateAndTime
     'monthIndex
     (lambda (self)
       (date-month self)))

(addSelector:withMethod:
     DateAndTime
     'yearNumber
     (lambda (self)
       (date-year self)))

;;;;@@@FillMeIn

)

;;;			--- E O F ---			;;;
