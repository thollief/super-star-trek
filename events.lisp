;;;; Super Star Trek events

(in-package sst-events)

;; TODO this is a suspicious constant, can it be removed from the code?
(define-constant +forever+ 1e30)
(define-constant +number-of-events+ 12)   ; C: NEVENTS

;; TODO - make future-events a list ordered by date, and then define scheduling functions that use
;; the list data structure
;; TODO - can *future-events* not be exported?
;; TODO - possibly a list, possibly an ordered list
(defparameter *future-events* (make-array +number-of-events+) "Dates of future events") ; C: future[NEVENTS]

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

(defun unschedule (event) ; C: event *unschedule(int evtype)
  "Remove an event from the schedule."

  (setf (aref *future-events* event) nil))

(defun find-event (event) ; C: #define findevent(evtype)	&game.future[evtype]
  "Find an event in the array of events using event-type as an index."

  (aref *future-events* event))

