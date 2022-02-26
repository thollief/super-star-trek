;;;; Super Star Trek events and time

(in-package sst-events)

(define-constant +number-of-events+ 12)   ; C: NEVENTS

(defvar *stardate* 0.0) ; C: double date
;; TODO - make future-events a list ordered by date, and then define scheduling functions that use
;; the list data structure
;; TODO - can *future-events* not be exported?
;; TODO - possibly a list, possibly an ordered list
(defvar *future-events* (make-array +number-of-events+) "Dates of future events") ; C: future[NEVENTS]

(defun initialize-events ()
  "Prepare the events data structure for use at the start of a game."

  (do ((n 0 (+ n 1)))
      ((>= n (length *future-events*)))
    (setf (aref *future-events* n) nil)))

(defun scheduled-for (event) ; C: scheduled(int evtype)
  "When will this event happen?"

  (aref *future-events* event))

(defun is-scheduled-p (event) ; C: is_scheduled(int evtype)
  "Is an event of the specified type scheduled."

  (aref *future-events* event))

(defun postpone-event (event offset) ; C: postpone(int evtype, double offset)
  "Postpone a scheduled event."

  (when (is-scheduled-p event)
    (setf (aref *future-events* event) (+ (aref *future-events* event) offset))))

(defun schedule-event (event-type offset)
  "Schedule an event of the specific type to occur offset time units in the future. Return the
 event. This isn't a real event queue a la BSD Trek yet -- you can only have one event of each
 type active at any given time.  Mostly these means we can only have one FDISTR/FENSLV/FREPRO
 sequence going at any given time; BSD Trek, from which we swiped the idea, can have up to 5."

  (setf (aref *future-events* event-type) (+ *stardate* offset))
  (return-from schedule-event (aref *future-events* event-type)))

(defun unschedule-event (event) ; C: event *unschedule(int evtype)
  "Remove an event from the schedule."

  (setf (aref *future-events* event) nil))

(defun find-event (event) ; C: #define findevent(evtype)	&game.future[evtype]
  "Find an event in the array of events using event-type as an index."

  (aref *future-events* event))

