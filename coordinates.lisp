;;;; Super Star Trek coordinates

(in-package sst-coordinates)

(define-constant +galaxy-size+ 8) ; C: GALSIZE
(define-constant +quadrant-size+ 10) ; C: QUADSIZE

;; TODO - adjacency is a frequently used property of coordinate pairs. Add support for calculating
;;        it for any pair of coords?
(defstruct coordinate ; C: coord
  "A pair of coordinates in two dimensions. These are used as Lisp array indices, so one less
 than the coordinate values displayed to the player."

  x
  y)

(defstruct (sector-coordinate (:include coordinate))
  "Coordinate pair for a sector.")

(defstruct (quadrant-coordinate (:include coordinate))
  "Coordinate pair for a quadrant.")

(defgeneric valid-p (coord)
  (:documentation "Return true if the x and y slot values of a coordinate are valid, false
otherwise."))

(defmethod valid-p ((coord coordinate))
    (and (numberp (coordinate-x coord))
         (numberp (coordinate-y coord))
         (>= (coordinate-x coord) 0)
         (>= (coordinate-y coord) 0)))

(defmethod valid-p ((coord sector-coordinate))
  (and (call-next-method)
       (< (coordinate-x coord) +quadrant-size+)
       (< (coordinate-y coord) +quadrant-size+)))

(defmethod valid-p ((coord quadrant-coordinate))
  (and (call-next-method)
       (< (coordinate-x coord) +galaxy-size+)
       (< (coordinate-y coord) +galaxy-size+)))

(defmacro coord-ref (array-name coord)
  "Use the coordinate struct to access a 2d array."

  `(aref ,array-name (round (coordinate-x ,coord)) (round (coordinate-y ,coord))))

(defun valid-quadrant-p (x y) ; C: VALID_QUADRANT(x, y)
  "Return true if the quadrant coordinates are valid. These are array indices, not player
coordinates."

  (if (and x y
           (numberp x)
           (numberp y))
      ;; TODO - make-quadrant-coordinate should fail if x and y are not valid
      (valid-p (make-quadrant-coordinate :x x :y y))
      (return-from valid-quadrant-p nil)))

(defun valid-sector-p (x y) ; C: VALID_SECTOR(x, y)
  "Return true if the sector coordinates are valid. These are array indices, not player
coordinates."

  (if (and x y
           (numberp x)
           (numberp y))
      ;; TODO - make-sector-coordinate should fail if x and y are not valid
      (valid-p (make-sector-coordinate :x x :y y))
      (return-from valid-sector-p nil)))

(defun coord-equal (c1 c2) ; C: same(c1, c2)
  "Two coordinates are equal if their corresponding x and y coordinates are equal."

  (and (eql (coordinate-x c1) (coordinate-x c2))
       (eql (coordinate-y c1) (coordinate-y c2))))

;; TODO - this doesn't calculate distance between sectors in different quadrants. Adding that
;; capability could be done by converting to a global coordinate system internally. The need for
;; the calculation came up in the mayday function.
(defun distance (c1 c2) ; C: distance(c1, c2)
  "Calculate the distance between two coordinates."

  (sqrt(+ (expt (- (coordinate-x c1) (coordinate-x c2)) 2)
          (expt (- (coordinate-y c1) (coordinate-y c2)) 2))))

(defun format-coordinates (c) ; C: cramlc
  "Format the supplied location coordinates as a string for printing. The game data structures are
zero-based arrays but the player-visible game coordinate system is one-based. Convert from internal
to external by adding 1."

  (format nil "~A - ~A" (1+ (coordinate-x c)) (1+ (coordinate-y c))))

(defun format-quadrant-coordinates (c) ; C: cramlc
  "Format the supplied quadrant coordinates as a string for printing. No checking is done to ensure
the supplied coordinates are valid for quadrants."

  (format nil "Quadrant ~A" (format-coordinates c)))

(defun format-sector-coordinates (c) ; C: cramlc
  "Format the supplied sector coordinates as a string for printing. No checking is done to ensure
the supplied coordinates are valid for sectors."

  (format nil "Sector ~A" (format-coordinates c)))

(defun galaxy-sector-to-quadrant (sector)
  "Given a sector coordinate within the galaxy, return the corresponding quadrant coordinate."

  (truncate (/ sector +quadrant-size+)))
