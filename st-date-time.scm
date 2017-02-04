;;; FILE: "st-date-time.scm"
;;; IMPLEMENTS: PointInTime, DateAndTime, Duration
;;; AUTHOR: Ken Dickey
;;; DATE: 16 January 2017

;; (requires 'st-core-classes)
;; (requires 'st-number)
;; (import (srfi :19))

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

(set! st-date+time-behavior
      (perform: DateAndTime 'methodDict))

(set! st-time-behavior
      (perform: PointInTime 'methodDict))

(set! st-duration-behavior
      (perform: Duration    'methodDict))

(perform:with:
     DateAndTime
     'category:
     '|Kernel-Chronology|)

(perform:with:
     PointInTime
     'category:
     '|Kernel-Chronology|)

(perform:with:
     Duration
     'category:
     '|Kernel-Chronology|)

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
       (make-time 'time-duration nanos seconds)))

(addSelector:withMethod:
     (class Duration)
     'seconds:
     (lambda (self seconds)
       (make-time 'time-duration 0 seconds)))

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
     'seconds
     (lambda (self)
       (time-second self)))

(addSelector:withMethod:
     Duration
     'nanoSeconds
     (lambda (self)
       (time-nanosecond self)))

(addSelector:withMethod:
     Duration
     'totalSeconds
     (lambda (self)
       (time-second self)))

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
                        (floor/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (floor/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (floor/ hours-rem secs/min)) )
               (display
                (string-append
                 (number->string days)
                 ":"
                 (pad-two hours)
                 ":"
                 (pad-two minutes)
                 ":"
                 (if (zero? nanos)
                     (pad-two seconds)
                     (string-append
                      (if (< seconds 10) "0" "")
                      (number->string (+ 0.0 seconds (/ nanos nanos/sec))))))
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
                        (floor/ secs secs/day)) )
           (let-values ( ((hours hours-rem)
                          (floor/ days-rem secs/hour)) )
             (let-values ( ((minutes seconds)
                            (floor/ hours-rem secs/min)) )
               (aBlockClosure days hours minutes seconds nanos)
     ) ) ) ) )
)

(addSelector:withMethod:
     Duration
     'days
     (lambda (self aBlockClosure)
       (let ( (secs  (time-second     self))
              (nanos (time-nanosecond self))
            )
         (let-values ( ((days days-rem)
                        (floor/ secs secs/day)) )
           days
     ) ) )
)

(addSelector:withMethod:
     (class Duration)
     'days:hours:minutes:seconds:nanoSeconds:
     (lambda (self d h m s nanos)
       (make-time 'time-duration
                  nanos
                  (+ (* d secs/day)
                     (* h secs/hour)
                     (* m secs/min)
                     s)))
)


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
       (time-second self)))

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
     (class DateAndTime)
     'clockPrecision
     (lambda (self)
       ;; Answer a Duration with nanoSeconds precision
       ($:: Duration
            'seconds:nanoSeconds:
            0
            (time-resolution 'time-utc))))

(addSelector:withMethod:
     (class DateAndTime)
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
     DateAndTime
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
     DateAndTime
     'asPointInTime
     (lambda (self)
       (date->time-utc self)))

(addSelector:withMethod:
     DateAndTime
     'printString
     (lambda (self)
       ;; @@FIXME: ANSI offset
;;     (date->string self "~Y-~m-~dT~H:~M:~S")
       (date->string self "~5"))) ;; same result

(addSelector:withMethod:
     DateAndTime
     'printOn:
     (lambda (self port)
       (display (date->string self "~Y-~m-~dT~H:~M:~S") port)))

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

;; (provides st-date-time)

;;;			--- E O F ---			;;;
