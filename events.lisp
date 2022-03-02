;;;; Super Star Trek events and time

(in-package sst-events)

(defvar *stardate* 0.0  ; C: double date
  "A counter for the passage of time. The meaning of stardates is not consistent in canonical
media but presentation is always 4 or 5 digits followed by a decimal point and a single digit.")

(defstruct event
  "A future event. Events occur at specific stardate, are distinguished by event type, and may
include quadrant information. Event types are

spy
supernova
tractor-beam
snapshot-for-time-warp
commander-attacks-base
commander-destroys-base
move-super-commander
super-commander-destroys-base
move-deep-space-probe
distress-call-from-inhabited-world
inhabited-world-is-enslaved
klingons-build-ship-in-enslaved-system"

  date ; stardate when event will occur
  type ; a symbol representing the event type
  quadrant) ; coordinates of the quadrant where the event will occur

(defvar *event-queue* ()
  "A list of event structs ordered by the date when the event will occur. There is only one
instance of each type of event in the queue.")

(defun initialize-events ()
  "Prepare the events data structure for use at the start of a game."

  (setf *event-queue* ()))

(defun scheduled-for (event) ; C: scheduled(int evtype)
  "When will this event happen?"

  (let ((pos (position nil *event-queue* :key #'(lambda (x) (eql event (event-type x))))))
    (when pos
      (event-date (car (subseq *event-queue* pos pos))))))

(defun is-scheduled-p (event) ; C: is_scheduled(int evtype)
  "Is an event of the specified type scheduled."

  (position event *event-queue* :key #'(lambda (x) (eql event (event-type x)))))

(defun postpone-event (event offset) ; C: postpone(int evtype, double offset)
  "Postpone a scheduled event by offset stardates."

  (when (is-scheduled-p event)
    ;; TODO - is this correct? The event returned by find-event is a pointer to the event in the
    ;;        queue, and the event can be updated in place.
    (let ((e (find-event event)))
      (setf (event-date e) (+ (event-date e) offset)))))

(defun schedule-event (event offset &optional (quadrant nil))
  ;; TODO - update this comment
  "Schedule an event of the specific type to occur offset time units in the future. This isn't a
 real event queue a la BSD Trek yet -- you can only have one event of each type active at any
 given time.  Mostly these means we can only have one FDISTR/FENSLV/FREPRO sequence going at any
 given time; BSD Trek, from which we swiped the idea, can have up to 5."

  ;; Create an event struct and insert it into the event queue in date order. If events already in
  ;; the queue have the same date as the event being inserted then insert the new event after the
  ;; existing events.
  ;; TODO - handle the case of a duplicate event type being added, only one event of each type is
  ;;        allowed
  (let ((e (make-event :date (+ *stardate* offset) :type event :quadrant quadrant))
        (start-pos 0)
        insert-pos
        (end-pos (length *event-queue*)))
    (if *event-queue*
        (progn
          (setf insert-pos (position e *event-queue*
                                     :test #'(lambda (x y) (< (event-date x) (event-date y)))))
          (if insert-pos
              (setf *event-queue* (append (subseq *event-queue* start-pos insert-pos)
                                          (list e)
                                          (subseq *event-queue* insert-pos end-pos)))
              (setf *event-queue* (append *event-queue* (list e)))))
        (setf *event-queue* (list e)))))

(defun unschedule-event (event) ; C: event *unschedule(int evtype)
  "Remove an event of the specified type from the schedule."

  (setf *event-queue* (delete event *event-queue*
                              :key #'(lambda (x) (eql event (event-type x))))))

(defun find-event (event) ; C: #define findevent(evtype)	&game.future[evtype]
  "Find an event in the array of events using event-type as an index."

  (let ((pos (position nil *event-queue* :key #'(lambda (x) (eql event (event-type x))))))
    (when pos
      (car (subseq *event-queue* pos pos)))))

(defun get-next-event ()
  "Retmove the next event from the event queue and return it. In other words, pop the queue."

  (pop *event-queue*))

(defun unget-event (event)
  "Put an event back in the queue. Typically called if an event retrieved with get-next-event can't
 be processed."

  (push event *event-queue*))
