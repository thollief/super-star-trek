;;;; Super Star Trek

;; TODO - review all (skip-line) calls to ensure they are going to the correct window
;; TODO - some functions only output to the message window, so ensure that the message window
;;        is selected at the beginning of the function
;; TODO - message window scrolling isn't correct

;; TODO - this sequence of events: tractor beam to q1,1, then supernova in q1,1 correctly triggered
;; emergency override. Warp factor set to 7, which triggered a time warp. Was that ok?
;; Also, commander in q1,1 was not killed but should have been.

(in-package super-star-trek)

(define-constant +galaxy-size+ 8) ; C: GALSIZE
(define-constant +quadrant-size+ 10) ; C: QUADSIZE
(define-constant +habitable-planets+ (/ (* +galaxy-size+ +galaxy-size+) 2)) ; C: NINHAB
(define-constant +max-uninhabitable-planets+ 10) ; C: MAXUNINHAB
(define-constant +min-uninhabitable-planets+ 5)
(define-constant +planet-max+ (+ +habitable-planets+ +max-uninhabitable-planets+)) ; C: PLNETMAX
(define-constant +max-bases+ (floor (/ (* +galaxy-size+ +galaxy-size+) 12))
  "12 looks like a made-up number to get the result close to 5.") ; C: BASEMAX
(define-constant +min-bases+ 2)
(define-constant +max-klingons-per-game+ 127) ; C: MAXKLGAME
(define-constant +max-klingons-per-quadrant+ 9) ; C: MAXKLQUAD
(define-constant +max-commanders-per-game+ 10)
(define-constant +max-stars-per-quadrant+ 9)
(define-constant +phaser-factor+ 2.0) ; C: phasefac
(define-constant +docked-repair-factor+ 0.25 "Repar factor when docked.") ; C: docfac
(define-constant +deathray-failure-chance+ 0.30)
(define-constant +max-safe-phaser-power+ 1500.0 "Amount phasers can fire without overheating.")
(define-constant +checkpoint-file-name+ "sstchkpt.trk"
  "The name of the file used for checkpointing game state.")
(define-constant +algeron-date+ 2311 "Date of the Treaty of Algeron") ; C: #define ALGERON

;; TODO - create a terminalio package?
(defparameter *window-interface-p* t
  "Control whether to display a full screen using (n)curses or the classic line-by-line output.
This variable is not saved and restored because the terminal used could change between sessions.")
(defparameter *current-window* nil) ; C: curwnd
(defparameter *short-range-scan-window* nil) ; C: srscan_window
(defparameter *ship-status-window* nil)
(defparameter *game-status-window* nil)
(defparameter *long-range-scan-window* nil) ; C: lrscan_window
(defparameter *message-window* nil)
(defparameter *message-window-lines* 23 "Total number of lines in the message window. The default
should be suitable for a classic 80x24 terminal.")
(defparameter *message-window-line-count* 0
  "Number of lines that have been output in the message window.")
(defparameter *prompt-window* nil)
(defparameter *starchart-window* nil)
(defparameter *damage-report-window* nil)
(defparameter *planet-report-window* nil)
(defparameter *score-window* nil)

(defparameter *line-tokens* nil "List of input tokens")
(defparameter *input-item* nil "One space-separated token from the keyboard.")

(defun select-window (window)
  "Change window. Do nothing in line-by-line mode."
  (when *window-interface-p*
    (setf *current-window* window)
    ;; make cursor visible if selecting one of these windows, otherwise hide it
    (if (or (equal *message-window* window)
            (equal *prompt-window* window))
        ;; cursor visibility: 0 = invisible, 1 = normal visibility, 2 = very visible
        (curs-set 1)
        (curs-set 0))))

;; TODO - C source also stores the input line in the replay file.
(defun get-input-line () ; C: cgetline(char *line, int max)
  "Get a line of input from the keyboard as a string. Remove leading and trailing spaces, and
lowercase everything."
  (let (line)
    (if *window-interface-p*
        (progn
          ;; wgetstr and related ncurses functions from charms/ll don't seem to work. Simulate with wgetch.
          (do ((input-char 0 (wgetch *prompt-window*)))
              ((= input-char 13))
            ;; wgetch returns numeric character codes, accept the usual printable ASCII characters
            (when (and (>= input-char 32)
                       (<= input-char 126))
              (setf line (concatenate 'string line (string (code-char input-char))))
              (when (= input-char 92) ; escape backslashes with a backslash because (read) is used to parse numbers
                (setf line (concatenate 'string line "\\"))))
            (when (= input-char 127) ; Handle backspace so errors can be corrected
              (let (y x)
                (getyx *prompt-window* y x)
                (setf x (- x 2))
                (mvwaddstr *prompt-window* y x "  ") ; DEL is a ctrl char so "^j" is echoed
                (wmove *prompt-window* y x)
                (when (> (length line) 0)
                  (setf x (- x 1))
                  (mvwaddstr *prompt-window* y x " ")
                  (wmove *prompt-window* y x)
                  (setf line (subseq line 0 (- (length line) 1))))))
            (wrefresh *prompt-window*)))
        (progn ; assume line-by-line
          (setf line (read-line)))) ; C: fgets(line, max, stdin) - input length isn't limited here, use read-char if needed
    (setf line (string-downcase (string-trim " " line)))))

;; TODO - scan should return the value scanned, not set a global, and unscan should accept a parameter
;;       of the item to unscan.
(defun scan-input ()
  "Manage a list of input tokens, returning one token each time this function is called. If the
list is empty then get input from the keyboard and split it on spaces to generate tokens. Return
the first token and save the remainder for the next call. If the keyboard input didn't include
any non-space characters then return nil."

  (when (= (length *line-tokens*) 0)
    (setf *line-tokens* (split-sequence #\SPACE (get-input-line) :remove-empty-subseqs t)))
  (let ((test-number nil))
    (setf *input-item* (pop *line-tokens*))
    (when *input-item*
      ;; Read something, possibly a number. This is reading a Lisp form and if it's malformed then
      ;; we don't want it anyway so ignore the error.
      (setf test-number (ignore-errors (read-from-string *input-item*)))
      (when (numberp test-number) ; If it's a number then keep it
        (setf *input-item* test-number)))))

(defun unscan-input ()
  "Put the current *input-item* back on the list of line tokens."

  ;; *line-tokens* must contains strings, *input-item* could be a number or something.
  (push (format nil "~A" *input-item*) *line-tokens*)
  (setf *input-item* nil))

(defun clear-type-ahead-buffer () ; C: chew()
  "Convenience function to delete any commands the player may have entered in anticipation of
future prompts. This is normally called after an input error or other condition where the future
prompts change."

  (setf *input-item* nil)
  (setf *line-tokens* nil))

(defun match-token (token possible-matches) ; C: isit()
  "Given a string and a list of possible matching strings, return the first item from the list
that matches. A match occurs when the first characters of both strings, up to the length of the
shortest string, are the same."

  (unless (stringp token) ; make sure we are handling a string
    (setf token (format nil "~A" token)))
  (let ((token-length (length token))
        length-to-compare)
    (dolist (candidate possible-matches)
      (setf length-to-compare (min token-length
                                   (length candidate)))
      (when (string= (subseq token 0 length-to-compare)
                     (subseq candidate 0 length-to-compare))
        (return-from match-token candidate)))))

(defun print-out (string-to-print &key (print-slowly nil))
  "Print a string. In curses mode use the current window. If print-slowly is true then print with a
delay between each character to build dramatic tension. Slow printing was not optional on most
paper-based terminals so slow printing helps recreate some of the gameplay experience of prior
versions of Super Star Trek."

  (map nil #'(lambda (c)
               (when print-slowly
                 (sleep 0.030))
               (if *window-interface-p*
                   (progn
                     (wprintw *current-window* (string c))
                     (wrefresh *current-window*))
                   (progn
                     (princ c)
                     (finish-output))))
       string-to-print)
  (when print-slowly
    (sleep 0.300)))

(defun skip-line (&optional (lines-to-skip 1))
  "A convenience function to add blank lines at the current cursor position of the screen or
current window, normally the last line. Since this is just a newline it can also end strings
that did not print their own newline."

  (dotimes (x lines-to-skip)
    (if *window-interface-p*
        (progn
          ;;(format nil "~%")
          ;;(wprintw *current-window* (concatenate 'string (string #\Linefeed) (string #\Return)))
          (wprintw *current-window* (string #\newline))
          (wrefresh *current-window*))
        (progn
          (print-out (format nil "~%"))
          (finish-output)))
    (when (equal *current-window* *message-window*)
      (page-message-window))))

(defun print-message (message-to-print &key (print-slowly nil))
  "Print a string in the message window. If not using the window interface just print it. Page the
window after each newline."

  (when *window-interface-p*
    (select-window *message-window*))

  (let (message-line
        (first-newline-position (position #\newline message-to-print)))
    (if first-newline-position
        (progn
          (setf first-newline-position (1+ first-newline-position))
          (setf message-line (subseq message-to-print 0 first-newline-position)))
        (setf message-line message-to-print))
    (print-out message-line :print-slowly print-slowly)
    (when first-newline-position
      (page-message-window)
      (when (> (length message-to-print) first-newline-position)
        (print-message (subseq message-to-print first-newline-position)
                       :print-slowly print-slowly)))))

(defun page-message-window ()
  "Pause message window output if the window has been filled with the maximum number of lines it
can hold. Don't pause in line-by-line mode - assume player has a scrollbar."

  (when *window-interface-p*
    (setf *message-window-line-count* (1+ *message-window-line-count*))
    ;; Pause at one line less than the total number of lines to account for newlines.
    (when (>= *message-window-line-count* (1- *message-window-lines*))
      (clear-type-ahead-buffer)
      (print-prompt "Press ENTER to continue")
      (scan-input)
      (clear-type-ahead-buffer)
      (restart-message-window-paging))))

(defun restart-message-window-paging ()
  "Assume all previous message window output has been seen by the player."

  (setf *message-window-line-count* 0))

(defun print-stars ()
  "Print a line of stars."

  (print-message (format nil "******************************************************~%")
                 :print-slowly t))

(defun huh () ; C: huh(void)
  "Complain about unparseable input."

  (clear-type-ahead-buffer)
  (skip-line)
  (print-message (format nil "Beg your pardon, Captain?~%")))

(defun get-y-or-n-p () ; C: ja()
  "When a player choice may negatively affect successful completion of the game then require a yes
or no answer. If other answers are given then prompt again. Use this function instead of y-or-n-p
to allow for curses or line-by-line output when the player is reminded of the input options."

  (clear-type-ahead-buffer)
  (do ((char nil))
      (char)
    (scan-input)
    (setf char (match-token *input-item* (list "yes" "no")))
    (cond
      ((string= char "yes")
       (return-from get-y-or-n-p t))

      ((string= char "no")
       (return-from get-y-or-n-p nil))

      (t
       (skip-line)
       (print-out "Please answer with \"y\" or \"n\": ")))))

;; TODO - Can/should coordinate handling be an object or package?
;; TODO - adjacency is a frequently used property of coordinate pairs. Add support for calculating
;;        it for any pair of coords?
(defstruct coordinate ; C: coord
  "Sector or quadrant coordinate pair. These are array indices, so one less that the coordinate
values displayed to the player."

  x
  y)

(defmacro coord-ref (array-name coord)
  "Use the coordinate struct to access a 2d array."

  `(aref ,array-name (coordinate-x ,coord) (coordinate-y ,coord)))

;; TODO - should this be a property of a ship struct?
(defparameter *ship-quadrant* nil) ; C: coord quadrant, where we are
(defparameter *ship-sector* nil) ; C: coord sector, where we are

;; TODO - can/should these take the coordinate structure as their parameter?
(defun valid-quadrant-p (x y) ; C: VALID_QUADRANT(x, y)
  "Return true if the quadrant coordinates are valid. These are array indices, not player
coordinates."

  (and (numberp x)
       (numberp y)
       (>= x 0)
       (< x +galaxy-size+)
       (>= y 0)
       (< y +galaxy-size+)))

;; TODO - should this take a coordinate struct as a parameter? Sometimes we have structs, sometimes we have a pair of scalars
;;        write a version that takes the coordinate struct and unpacks it to use the pair of scalar's version, or
;;        figure out how to write a macro similar to coord-ref
(defun valid-sector-p (x y) ; C: VALID_SECTOR(x, y)
  "Return true if the sector coordinates are valid. These are array indices, not player
coordinates."

  (and (numberp x)
       (numberp y)
       (>= x 0)
       (< x +quadrant-size+)
       (>= y 0)
       (< y +quadrant-size+)))

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

;; TODO - this takes a global (*input-item*) as a parameter...
(defun read-coordinate-number ()
  "Read a player-entered coordinate number from the input buffer. Return the internal coordinate
number (array index) or nil."

  (let ((c-num nil))
    (when (and *input-item* (numberp *input-item*))
      (setf c-num (1- *input-item*)))
    (return-from read-coordinate-number c-num)))

(defun get-quadrant-and-sector ()
  "Get from the player two numbers representing a quadrant, or four numbers representing a quadrant
and sector. If only two numbers are entered then only a quadrant is specified and the sector will
be left empty. Check input for correctness and display an error message if needed. Return the valid
coordinates or nil."

  (scan-input)
  (let (qx qy sx sy)
    (setf qy (read-coordinate-number))
    ;; The player should have entered at least two numbers for the coordinate and the second one is
    ;; still in the input buffer. If the input buffer is empty then the scan will wait for further
    ;; input, which we don't want at this point.
    (when *line-tokens*
      (scan-input)
      (setf qx (read-coordinate-number)))
    ;; If there are two more coordinates then read them, otherwise we don't care what was typed
    (when (>= (length *line-tokens*) 2)
      (scan-input)
      (setf sy (read-coordinate-number))
      (scan-input)
      (setf sx (read-coordinate-number)))
    ;; If player entered only two coordinates, a quadrant, then fix up the quadrant coordinates
    (when (and qx qy (not sx) (not sy))
      (setf sx qx)
      (setf sy qy)
      (setf qx (coordinate-x *ship-quadrant*))
      (setf qy (coordinate-y *ship-quadrant*)))
    ;; Validate coordinate values and return them
    (if (and qx qy (valid-quadrant-p qx qy) sx sy (valid-sector-p sx sy))
        (return-from get-quadrant-and-sector (values sx sy qx qy))
        (huh))))

(defun format-stardate (d)
  "Write a stardate with one decimal point."

  (format nil "~,1,,,F" d))

(define-constant +short-game+ 1)
(define-constant +medium-game+ 2)
(define-constant +long-game+ 4)
(defstruct game-length
  "Player's selection of game length."

  value ; Numeric values 1, 2, or 4. Used to initialize gameplay parameters.
  label) ; The textual representation of the game length: short, medium, long

(define-constant +novice+ 1) ; C: skill, SKILL_NOVICE = 1
(define-constant +fair+ 2) ; C: skill, SKILL_FAIR = 2
(define-constant +good+ 3) ; C: skill, SKILL_GOOD = 3
(define-constant +expert+ 4) ; C: skill, SKILL_EXPERT = 4
(define-constant +emeritus+ 5) ; C: skill, SKILL_EMERITUS = 5
(defstruct skill-level
  "Player's selection of game difficulty. Skill levels are ordered, each higher
than the previous."

  value ; Numeric skill level used to initialize gameplay parameters
  label) ; The textual representaion of the skill level: novice, fair, good, expert, emeritus

(define-constant +class-m+ 1)
(define-constant +class-n+ 2)
(define-constant +class-o+ 3)

(defstruct planet ; C: planet
  "Information about a planet."

  quadrant ; C: w, a coordinate
  ;; TODO - planet class has both a letter and a value. How to represent?
  class ; C: pclass, one of 1, 2, or 3 (M, N, or O) - affects time needed to mine crystals
  name ; Name of the planet. An empty string for uninhabited planets
  destroyedp ; Whether or not this planet has been destroyed
  knownp ; whether or not this planet has been scanned by the player - TODO handle inhabited planets
  inhabitedp ; C: inhabited, Whether or not the planet has inhabitants.
  crystals) ; has crystals: 'absent, 'present, or 'mined

(defun format-planet-class (class)
  "Given a numeric value for the class of a planet, return the single letter equivalent, or an
empty string if the planet class can't be determined."

  (cond
    ((= class +class-m+)
     "M")
    ((= class +class-n+)
     "N")
    ((= class +class-o+)
     "O")
    (t
     "")))

(define-constant +secure+ 0)
(define-constant +distressed+ 1)
(define-constant +enslaved+ 2)

(defparameter *planet-information* nil
  "An alist of planet structs keyed by the quadrant coordinates of the planet")

;; Characters displayed for game entities in short range scans
;; TODO - are probes visible in short range scans? Should they be?
;; C: typedef enum {} feature
(define-constant +romulan+ "R") ; C: IHR
(define-constant +klingon+ "K") ; C: IHK
(define-constant +commander+ "C") ; C: IHC
(define-constant +super-commander+ "S") ; C: IHS
(define-constant +star+ "*") ; C: IHSTAR
(define-constant +planet+ "P") ; C: IHP, an uninhabited planet
(define-constant +world+ "@") ; C: IHW, an inhabited planet
(define-constant +starbase+ "B") ; C: IHB
(define-constant +black-hole+ " ") ; C: IHBLANK
(define-constant +empty-sector+ ".") ; C: IHDOT
(define-constant +thing+ "?") ; C: IHQUEST
(define-constant +enterprise+ "E") ; C: IHE
(define-constant +faerie-queene+ "F") ; C: IHF
(define-constant +no-ship+ "U") ; Any unused letter, only needs to be a string type
(define-constant +tholian+ "T") ; C: IHT
(define-constant +tholian-web+ "#") ; C: IHWEB
(define-constant +materialize-1+ "-") ; C: IHMATER0
(define-constant +materialize-2+ "o") ; C: IHMATER1
(define-constant +materialize-3+ "0") ; C: IHMATER2
(define-constant +reserved+ "X")

(defun letter-to-name (quadrant-entity)
  "Convert a single letter quadrant entity to a string for display."

  ;; TODO these are the strings actually used. Should plan for them all, or use a struct with a
  ;;      symbol and a label, like the game type or game length. Is there an alist in here somewhere?
  (cond
    ((string= quadrant-entity +klingon+)
     "Klingon")
    ((string= quadrant-entity +romulan+)
     "Romulan")
    ((string= quadrant-entity +commander+)
     "Commander")
    ((string= quadrant-entity +super-commander+)
     "Super-commander")
    ((string= quadrant-entity +tholian+)
     "Tholian")
    ((string= quadrant-entity +thing+)
     "Thing")
    ((string= quadrant-entity +black-hole+)
     "Black hole")
    (t
     "unknown")))

;; TODO - define a ship structure. Ships have several properties: a label, a short range scan
;;        symbol, a location, dilithium crystals on board, energy, photon torpedoes, a shuttle
;;        craft, warp factor, equipment (some of which may be damaged). At the time this comment
;;        is being written it's not clear to me if collecting these things in a struct will improve
;;        code quality but it is what I understand the "right" way to be

;; TODO - define a probe structure? It's also an entity that moves around the galaxy, as does the
;;        super-commander and sometimes commanders.

(defstruct quadrant ; C: quadrant
  "Information about a quadrant.

When a quadrant has had a supernova don't zero out the number of stars, planets, and starbases. If
the player caused the supernova these destroyed objects count against the final score and need to
be tracked."

  (stars 0)
  (starbases 0) ; 0 or 1
  ;; TODO - track klingons, commanders and super-commanders separately? If yes, then there may be a
  ;;        need for a function to check if klingons of any type are present. Swings and roundabouts...
  (klingons 0) ; number of klingons of all type: klingons + commanders + super commanders
  (romulans 0)
  (supernovap nil)
  (chartedp nil)
  (status +secure+)) ; C: status, One of 0, 1, 2 for secure, distressed, enslaved - TODO this is a property of the planet

;; TODO - what's the difference between the star chart and the galaxy?
;; Planets and Romulans don't show on the star chart
(defstruct starchart-page ; C: page
  stars
  starbases ; 0 or 1
  klingons)

(defstruct snapshot ; C: snapshot
  (crew 0) ; C: crew, crew complement
  (remaining-klingons 0) ; C: remkl
  (captured-klingons 0)
  (brig-free 0)
  (remaining-commanders 0) ; C: remcom
  (remaining-super-commanders 0) ; C: nscrem
  (remaining-bases 0) ; C: rembase
  (remaining-resources 0.0) ; C: remres
  (remaining-time 0.0) ; C: remtime
  (remaining-romulans 0) ; C: nromrem
  (destroyed-bases 0) ; C: basekl
  (destroyed-stars 0) ; C: starkl
  (destroyed-inhabited-planets 0) ; C: nworldkl
  (destroyed-uninhabited-planets 0) ; C: nplankl
  (planet-information ()) ; C: planets
  (stardate 0.0) ; C: double date
  shuttle-craft-location
  shuttle-craft-quadrant
  base-quadrants
  commander-quadrants
  super-commander-quadrant ; C: kscmdr
  galaxy ; C: galaxy[GALSIZE+1][GALSIZE+1], The Galaxy, array of quadrants
  starchart) ; C: chart[GALSIZE+1][GALSIZE+1], The starchart, array of starchart-pages

;; Devices - indexes into the *devices* and device-damage arrays
(define-constant +short-range-sensors+ 0) ; C: DSRSENS
(define-constant +long-range-sensors+ 1) ; C: DLRSENS
(define-constant +phasers+ 2) ; C: DPHASER
(define-constant +photon-torpedoes+ 3) ; C: DPHOTON
(define-constant +life-support+ 4) ; C: DLIFSUP
(define-constant +warp-engines+ 5) ; C: DWARPEN
(define-constant +impluse-engines+ 6) ; C: DIMPULS
(define-constant +shields+ 7) ; C: DSHIELD
(define-constant +subspace-radio+ 8) ; C: DRADIO
(define-constant +shuttle+ 9) ; C: DSHUTTL
(define-constant +computer+ 10) ; C: DCOMPTR
(define-constant +navigation-system+ 11) ; C: DNAVSYS
(define-constant +transporter+ 12) ; C: DTRANSP
(define-constant +shield-control+ 13) ; C: DSHCTRL
(define-constant +death-ray+ 14) ; C: DDRAY
(define-constant +deep-space-probe-launcher+ 15) ; C: DDSP
(define-constant +cloaking-device+ 16) ; C: DCLOAK

(define-constant +number-of-devices+ 17) ; C: NDEVICES

;; The following global state doesn't need to be saved
;; TODO should devices be a structure instead of an array? Or perhaps each device should be a
;; structure with elements damage, output string? Or some sort of object...
(defparameter *devices* (make-array +number-of-devices+
                                    :initial-contents '("S. R. Sensors" "L. R. Sensors" "Phasers"
                                                        "Photon Tubes" "Life Support"
                                                        "Warp Engines" "Impulse Engines"
                                                        "Shields" "Subspace Radio"
                                                        "Shuttle Craft" "Computer"
                                                        "Navigation System" "Transporter"
                                                        "Shield Control" "Death Ray"
                                                        "D. S. Probe" "Cloaking Device")))


;; Define future events - these are event types.
;; TODO - These are indexes into the future-events array, what is an appropriate Common Lisp data
;; structure? The values are also used in a case statement.
(define-constant +spy+ 0 "Spy event happens always (no future[] entry), can cause SC to tractor beam Enterprise") ; C: FSPY
(define-constant +supernova+ 1) ; C: FSNOVA
(define-constant +tractor-beam+ 2 "Commander tractor beams Enterprise.") ; C: FTBEAM
(define-constant +snapshot-for-time-warp+ 3) ; C: FSNAP
(define-constant +commander-attacks-base+ 4) ; C: FBATTAK
(define-constant +commander-destroys-base+ 5) ; C: FCDBAS
(define-constant +move-super-commander+ 6 "The Super-commander moves, and might attack a base.") ; C: FSCMOVE
(define-constant +super-commander-destroys-base+ 7) ; C: FSCDBAS
(define-constant +move-deep-space-probe+ 8) ; C: FDSPROB
(define-constant +distress-call-from-inhabited-world+ 9) ; C: FDISTR
(define-constant +inhabited-world-is-enslaved+ 10) ; C: FENSLV
(define-constant +klingons-build-ship-in-enslaved-system+ 11) ; C: FREPRO

;; TODO - the C source only used the quadrant in the event struct for planet conquest but most
;;        events do occur in a quadrant. Update event handling to add quadrants to all events?
;;(defstruct event
;;  date ; stardate when event will occur
;;  quadrant) ; coordinates of quadrant where event will occur
(defparameter *conquest-quadrant* nil "Location of planet where the distress/enslave/reproduce events occur.")

;; C: see the enum "condition" in sst.h
;; TODO - must these be integers? Maybe use the word?
;; values for *condition*
(define-constant +green-status+ 0) ; C: IHGREEN
(define-constant +yellow-status+ 1) ; C: IHYELLOW
(define-constant +red-status+ 2) ; C: IHRED
(define-constant +dead+ 3)

(define-constant +sst-version+ "SST 2.0") ; C: SSTMAGIC

;; User-selectable game parameters.
(defparameter *tournament-number* nil "Tournament number, or nil if regular game.") ; C: tourn
(defparameter *game-length* nil "A game-length struct.") ; C: length
(defparameter *skill-level* nil "A skill-level struct") ; C: skill
(defparameter *self-destruct-password* nil) ; C: passwd[10]

;; The Enterprise
;; TODO - the Faerie Queene has no shuttle craft or deathray so it should not be
;; possible/allowed/necessary to check if those devices are damaged
(define-constant +full-crew+ 428) ; C: FULLCREW, BSD Trek was 387, that's wrong
(defparameter *ship* +enterprise+) ; C: ship, 'E' is Enterprise
(defparameter *initial-energy* 5000.0 "Initial and max energy") ; C: inenrg
(defparameter *ship-energy* 5000.0) ; C: energy
(defparameter *initial-shield-energy* 2500.0 "Initial and max shield") ; C: inshld
(defparameter *shield-energy* 2500.0) ; C: shield
(defparameter *shields-are-changing-p* nil) ; C: shldchg, affects efficiency
(defparameter *shields-are-up-p* nil) ; C: shldup
(defparameter *cloakedp* nil "Cloaking is enabled") ; C: iscloaked
(defparameter *cloakingp* nil "In the process of cloaking and can be attacked") ; C: iscloaking
(defparameter *initial-life-support-reserves* 4.0) ; C: inlsr
(defparameter *life-support-reserves* 4.0) ; C: lsupres
(defparameter *initial-torpedos* 10 "Initial and max torpedoes") ; C: intorps
(defparameter *torpedoes* 10) ; C: torps
(defparameter *warp-factor* 5.0) ; C: warpfac, Warp speed
(defparameter *device-damage* (make-array +number-of-devices+ :initial-element 0.0)) ; C: damage[NDEVICES], Damage encountered
(defparameter *crew* +full-crew+) ; C: crew, crew complement
(defparameter *abandoned-crew* 0 "Count of crew abandoned in space") ; C: abandoned
(defparameter *casualties* 0) ; C: casual
(defparameter *brig-capacity* 0 "How many Klingons the brig will hold") ; C: brigcapacity
(defparameter *brig-free* 0 "room in the brig") ; C: brigfree
(defparameter *dilithium-crystals-on-board-p* nil) ; C: icrystl
;; TODO - is the probability crystals will work or that they will fail?
(defparameter *crystal-work-probability* 0.0) ; C: cryprob, probability that crystal will work
;; TODO - dead is captured in *alivep*, should it just be red?
(defparameter *condition* nil "red, yellow, green, dead") ; C: condition,  - TODO - another alist?
;; TODO - decide if orbital cloaking is possible
(defparameter *dockedp* nil) ; a possible flight condition
(defparameter *in-orbit-p* nil) ; C: inorbit, orbiting - a possible flight condition
(defparameter *height-of-orbit* 0) ; C: height, height of orbit around planet

(defparameter *initial-bases* 0) ; C: inbase
(defparameter *destroyed-bases* 0 "Number of bases destroyed by player action.") ; C: basekl
(defparameter *base-quadrants* () "A list of coordinate structs, these are quadrants containing bases.")

(defparameter *initial-stars* 0); C: instar
(defparameter *destroyed-stars* 0 "Number of stars destroyed by player action.") ; C: starkl

(defparameter *initial-planets* 0) ; C: inplan
(defparameter *destroyed-inhabited-planets* 0) ; C: nworldkl
(defparameter *destroyed-uninhabited-planets* 0) ; C: nplankl

(defparameter *initial-klingons* 0) ; C: inkling
(defparameter *remaining-klingons* 0) ; C: remkl
(defparameter *captured-klingons* 0 "number of captured Klingons") ; C: kcaptured

(defparameter *initial-commanders* 0) ; C: incom
(defparameter *commander-quadrants* () "List of coordinate structs, these are quadrants containing commanders.")

(defparameter *initial-super-commanders* 0); C: inscom
(defparameter *remaining-super-commanders* 0) ; C: nscrem
(defparameter *super-commander-quadrant* nil) ; C: kscmdr

(defparameter *initial-romulans* 0) ; C: inrom
(defparameter *remaining-romulans* 0) ; C: nromrem

(defparameter *cloaking-violations* 0 "Number of treaty violations") ; C: ncviol
(defparameter *cloaking-violation-reported-p* nil "Violation reported by Romulan in quadrant") ; C: isviolreported

(defparameter *initial-resources* 0.0) ; C: inresor
(defparameter *remaining-resources* 0.0) ; C: remres
(defparameter *initial-time* 0.0) ; C: intime
(defparameter *remaining-time* 0.0) ; C: remtime
(defparameter *initial-stardate* 0.0) ; C: indate
(defparameter *stardate* 0.0) ; C: double date

(defparameter *damage-factor* 0.0 "Damage modifier based on skill level") ; C: damfac

(defparameter *galaxy* nil "The Galaxy, an array of quadrants") ; C: galaxy[GALSIZE+1][GALSIZE+1]
(defparameter *starchart* nil "The starchart, an array of starchart-pages") ; C: chart[GALSIZE+1][GALSIZE+1]

(defparameter *quadrant-contents* (make-array (list +quadrant-size+ +quadrant-size+))
  "The contents of the current quadrant.") ; C: feature quad[QUADSIZE+1][QUADSIZE+1];
;; TODO the next 4 arrays have information for 1 enemy at a given index. Would a single arrary of structs
;;      do the job as well, perhaps stored in *quadrant-contents*?
;; The array sizes need to accomodate the deathray case of filling the sector.
(defparameter *klingon-energy* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kpower[(QUADSIZE+1)*(QUADSIZE+1)], enemy energy levels
(defparameter *klingon-distance* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kdist[(QUADSIZE+1)*(QUADSIZE+1)], enemy distances
(defparameter *klingon-average-distance* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kavgd[(QUADSIZE+1)*(QUADSIZE+1)], average distances
(defparameter *klingon-sectors* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: ks[(QUADSIZE+1)*(QUADSIZE+1)], enemy sector locations - an array of coordinates
(defparameter *tholian-sector* nil) ; C: coord tholian, coordinates of tholian
(defparameter *base-sector* nil) ; C: base, coordinate position of base in current quadrant

;; Other game parameters
;; TODO - should this be in an event structure?
(defparameter *base-under-attack-quadrant* nil) ; C: battle, Base coordinates being attacked - a coordinate struct, or nil. See also the event +commander-attacks-base+
(defparameter *calls-for-help* 0) ; C: nhelp
(defparameter *energy-barrier-crossings* 0 "Count of energy-barrier crossings") ; C: nkinks
(defparameter *in-shuttle-craft-p* nil "Kirk in Galileo") ; C: icraft
(defparameter *miningp* nil) ; C: imine
(defparameter *restingp* nil) ; C: resting, rest time
(defparameter *super-commander-attack-enterprise-p* nil) ; C: iscate
;; TODO - define some named constants for the status of the super-commander attacking a base, or use symbols
(defparameter *super-commander-attacking-base* 0) ; C: isatb, 0 = not attacking, 1 = attacking, = 2  Super-Commander is in the process of destroying a base
(defparameter *shuttle-craft-location* 'on-ship
  "Location of the shuttle craft. One of 'on-ship, 'off-ship, or 'removed. 'off-ship is effectively
the same as being on a planet.") ; C: iscraft
(defparameter *shuttle-craft-quadrant* nil "The quadrant coordinates of the shuttle craft. They are
the same as the ship if the shuttle craft location is on-ship.")
;; TODO - alivep and condition = dead are two ways of saying the same thing....
(defparameter *alivep* t "The player is alive (not killed)") ; C: alive
(defparameter *action-taken-p* nil "If an action is taken then enemy is allowed to attack.") ; C: ididit

(defparameter *snapshot* nil) ; C: snapshot snapsht;
(defparameter *snapshot-taken-p* nil) ; C:snap
(defun use-snapshot ()
  "Assign the values stored in the snapshot structure to the similarly named global values."

  (setf *crew* (snapshot-crew *snapshot*))
  (setf *remaining-klingons* (snapshot-remaining-klingons *snapshot*))
  (setf *captured-klingons* (snapshot-captured-klingons *snapshot*))
  (setf *brig-free* (snapshot-brig-free *snapshot*))
  (setf *remaining-super-commanders* (snapshot-remaining-super-commanders *snapshot*))
  (setf *remaining-resources* (snapshot-remaining-resources *snapshot*))
  (setf *remaining-time* (snapshot-remaining-time *snapshot*))
  (setf *remaining-romulans* (snapshot-remaining-romulans *snapshot*))
  (setf *destroyed-bases* (snapshot-destroyed-bases *snapshot*))
  (setf *destroyed-stars* (snapshot-destroyed-stars *snapshot*))
  (setf *destroyed-inhabited-planets* (snapshot-destroyed-inhabited-planets *snapshot*))
  (setf *destroyed-uninhabited-planets* (snapshot-destroyed-uninhabited-planets *snapshot*))
  (setf *planet-information* (snapshot-planet-information *snapshot*))
  (setf *stardate* (snapshot-stardate *snapshot*))
  (setf *shuttle-craft-location* (snapshot-shuttle-craft-location *snapshot*))
  (setf *shuttle-craft-quadrant* (snapshot-shuttle-craft-quadrant *snapshot*))
  (setf *base-quadrants* (snapshot-base-quadrants *snapshot*))
  (setf *commander-quadrants* (snapshot-commander-quadrants *snapshot*))
  (setf *super-commander-quadrant* (snapshot-super-commander-quadrant *snapshot*))
  (setf *galaxy* (snapshot-galaxy *snapshot*))
  (setf *starchart* (snapshot-starchart *snapshot*)))

;; The game
(defparameter *game-won-p* nil) ; C: gamewon, Finished!
(defparameter *all-done-p* t "Game is finished. True at the end of a game or if no game is in progress.") ; C: alldone

;; Information about the current quadrant, set each time the ship enters a new quadrant
(defparameter *just-in-p* nil
  "True when the player has entered the quadrant but not yet taken any action.") ; C: justin
(defparameter *klingons-here* nil) ; C: klhere
(defparameter *commanders-here* nil) ; C: comhere - TODO could this be in the quadrant structure like Klingons and Romulans?
(defparameter *super-commanders-here* 0) ; C: ishere - refers to the current quadrant
(defparameter *romulans-here* nil) ; C: irhere	, number of Romulans in quadrant
(defparameter *planet-coord* nil) ; C: iplnet - coordinates of a planet in the current quadrant, if any
(defparameter *enemies-here* nil "Number of enemies in quadrant") ; C: nenhere
(defparameter *romulan-neutral-zone-p* nil) ; C: neutz	, Romulan Neutral Zone
(defparameter *landedp* nil) ; C: landed	, party on planet (true), on ship (false)
(defparameter *attempted-escape-from-super-commander-p* nil) ; C: ientesc
(defparameter *tholians-here* 0) ; C: ithere - Max 1 Tholian in a quadrant but this is an entity count not a boolean
(defparameter *base-attack-report-seen-p* nil) ; C: iseenit
(defparameter *current-planet* nil "Sector coordinate location of a planet in the current quadrant, if any") ; C: plnet

(defparameter *time-taken-by-current-operation* 0.0) ; C: optime
(defparameter *last-chart-update* 0.0); C: lastchart, time starchart was last updated. It starts functional but we've never seen it.

(defparameter *probe-reported-quadrant* (make-coordinate)
  "The last reported location of the probe") ; C: probec, current probe quadrant
;; Probe x and y quadrant+sector
(defparameter *probe-x-coord* 0) ; C: probex	, location of probe
(defparameter *probe-y-coord* 0) ; C: probey
(defparameter *probe-x-increment* nil) ; C: probeinx, Probe x,y increment
(defparameter *probe-y-increment* nil) ; C: probeiny
(defparameter *moves-for-probe* nil) ; C: proben
(defparameter *probes-available* 0) ; C: nprobes
(defparameter *probe-is-armed-p* nil) ; C: isarmed

;; All the ways to finish a game
;; TODO - do these need to be intgers? Can symbols be used instead?
(define-constant +won+ 0) ; C: FWON
(define-constant +deplete+ 1) ; C: FDEPLETE
(define-constant +life-support-consumed+ 2) ; C: FLIFESUP
(define-constant +out-of-energy+ 3) ; C: FNRG
(define-constant +battle+ 4) ; C: FBATTLE
(define-constant +3-negative-energy-barrier-crossings+ 5) ; C: FNEG3
(define-constant +nova+ 6) ; C: FNOVA
(define-constant +destroyed-by-supernova+ 7) ; C: FSNOVAED
(define-constant +abandon+ 8) ; C: FABANDN
(define-constant +dilithium+ 9) ; C: FDILITHIUM
(define-constant +materialize+ 10) ; C: FMATERIALIZE
(define-constant +phaser+ 11) ; C: FPHASER
(define-constant +lost+ 12) ; C: FLOST
(define-constant +mining+ 13) ; C: FMINING
(define-constant +destroyed-planet+ 14) ; C: FDPLANET
(define-constant +mining-party-nova+ 15) ; C: FPNOVA
(define-constant +shuttle-super-nova+ 16) ; C: FSSC
(define-constant +shuttle-tractor-beam+ 17) ; C: FSTRACTOR
(define-constant +death-ray-malfunction+ 18) ; C: FDRAY
(define-constant +tribbles+ 19) ; C: FTRIBBLE
(define-constant +destroyed-by-black-hole+ 20) ; C: FHOLE
(define-constant +all-crew-killed+ 21) ; C: FCREW
(define-constant +cloak+ 22) ; C: FCLOAK

;; C: enum COLORS
(define-constant +default-color+ 0)
(define-constant +black+ 1)
(define-constant +blue+ 2)
(define-constant +green+ 3)
(define-constant +cyan+ 4)
(define-constant +red+ 5)
(define-constant +magenta+ 6)
(define-constant +brown+ 7)
(define-constant +light-gray+ 8)
(define-constant +dark-gray+ 9)
(define-constant +light-blue+ 10)
(define-constant +light-green+ 11)
(define-constant +light-cyan+ 12)
(define-constant +light-red+ 13)
(define-constant +light-magenta+ 14)
(define-constant +yellow+ 15)
(define-constant +white+ 16)

;; The Space Thing's global state should *not* be saved! This prevents players proving they encountered it
;; by examining a saved game.
(defparameter *thing-location* nil); C: thing, location of strange object in galaxy
(defparameter *things-here* 0) ; C: bool iqhere - Normally Max 1 Thing in a quadrant but this an entity count not a boolean.
(defparameter *thing-is-angry-p* nil) ; C: bool iqengry
(defparameter *score* 0) ; C: iscore, Common PLAQ
(defparameter *seed* 0) ; C: int seed, the random-number seed
(defparameter *log-file* 0) ; C: FILE *logfp, TODO - this should be a file, deal with it later
(defparameter *replay-file* 0) ; C: FILE *replayfp, TODO - this should be a file, deal with it later

;; Internal documentation of system names
;;
;; I used <http://www.memory-alpha.org> to find planets
;; with references in ST:TOS.  Eath and the Alpha Centauri
;; Colony have been omitted.
;;
;; Some planets marked Class G and P here will be displayed as class M
;; because of the way planets are generated. This is a known bug. TODO - fix it!
;;
;;      Federation Worlds
;; "Andoria (Fesoan)"),  several episodes
;; "Tellar Prime (Miracht)"),  TOS: "Journey to Babel"
;; "Vulcan (T'Khasi)"),  many episodes
;; "Medusa"),   TOS: "Is There in Truth No Beauty?"
;; "Argelius II (Nelphia)"), TOS: "Wolf in the Fold" ("IV" in BSD)
;; "Ardana"),   TOS: "The Cloud Minders"
;; "Catulla (Cendo-Prae)"),  TOS: "The Way to Eden"
;; "Gideon"),   TOS: "The Mark of Gideon"
;; "Aldebaran III"),  TOS: "The Deadly Years"
;; "Alpha Majoris I"),  TOS: "Wolf in the Fold"
;; "Altair IV"),   TOS: "Amok Time
;; "Ariannus"),   TOS: "Let That Be Your Last Battlefield"
;; "Benecia"),   TOS: "The Conscience of the King"
;; "Beta Niobe I (Sarpeidon)"),  TOS: "All Our Yesterdays"
;; "Alpha Carinae II"),  TOS: "The Ultimate Computer"
;; "Capella IV (Kohath)"),  TOS: "Friday's Child" (Class G)
;; "Daran V"),   TOS: "For the World is Hollow and I Have Touched the Sky"
;; "Deneb II"),   TOS: "Wolf in the Fold" ("IV" in BSD)
;; "Eminiar VII"),   TOS: "A Taste of Armageddon"
;; "Gamma Canaris IV"),  TOS: "Metamorphosis"
;; "Gamma Tranguli VI (Vaalel)"),  TOS: "The Apple"
;; "Ingraham B"),   TOS: "Operation: Annihilate"
;; "Janus IV"),   TOS: "The Devil in the Dark"
;; "Makus III"),   TOS: "The Galileo Seven"
;; "Marcos XII"),   TOS: "And the Children Shall Lead",
;; "Omega IV"),   TOS: "The Omega Glory"
;; "Regulus V"),   TOS: "Amok Time
;; "Deneva"),   TOS: "Operation -- Annihilate!"
;;
;;     Worlds from BSD Trek
;; "Rigel II"),   TOS: "Shore Leave" ("III" in BSD)
;; "Beta III"),   TOS: "The Return of the Archons"
;; "Triacus"),   TOS: "And the Children Shall Lead",
;; "Exo III"),   TOS: "What Are Little Girls Made Of?" (Class P)
;;
;;     Others
;; "Hansen's Planet"),  TOS: "The Galileo Seven"
;; "Taurus IV"),   TOS: "The Galileo Seven" (class G)
;; "Antos IV (Doraphane)"),  TOS: "Whom Gods Destroy", "Who Mourns for Adonais?"
;; "Izar"),    TOS: "Whom Gods Destroy"
;; "Tiburon"),   TOS: "The Way to Eden"
;; "Merak II"),   TOS: "The Cloud Minders"
;; "Coridan (Desotriana)"),  TOS: "Journey to Babel"
;; "Iotia"),   TOS: "A Piece of the Action"
;;
;; No need to save/restore *system-names*, it does not change
(defparameter *system-names* (make-array 40
                                         :initial-contents '("Andoria (Fesoan)"
                                                             "Tellar Prime (Miracht)"
                                                             "Vulcan (T'Khasi)" "Medusa"
                                                             "Argelius II (Nelphia)" "Ardana"
                                                             "Catulla (Cendo-Prae)" "Gideon"
                                                             "Aldebaran III" "Alpha Majoris I"
                                                             "Altair IV" "Ariannus" "Benecia"
                                                             "Beta Niobe I (Sarpeidon)"
                                                             "Alpha Carinae II"
                                                             "Capella IV (Kohath)" "Daran V"
                                                             "Deneb II" "Eminiar VII"
                                                             "Gamma Canaris IV"
                                                             "Gamma Tranguli VI (Vaalel)"
                                                             "Ingraham B" "Janus IV" "Makus III"
                                                             "Marcos XII" "Omega IV"
                                                             "Regulus V" "Deneva" "Rigel II"
                                                             "Beta III" "Triacus" "Exo III"
                                                             "Hansen's Planet" "Taurus IV"
                                                             "Antos IV (Doraphane)" "Izar"
                                                             "Tiburon" "Merak II"
                                                             "Coridan (Desotriana)" "Iotia")))

(defun shuttle-landed-p (p-quad)
  "Return true or false depending on whether or not the planet in the specified quadrant has the
shuttle craft landed on it."

  (and (eql *shuttle-craft-location* 'off-ship)
       *shuttle-craft-quadrant* ; coord-equal requires non-nil inputs
       (coord-equal *shuttle-craft-quadrant* p-quad)))

(defun format-ship-name () ; C: crmshp(void)
  "Return ship name as a string."

  (let (ship-name)
    (cond
      ((string= *ship* +enterprise+)
       (setf ship-name"Enterprise" ))
      ((string= *ship* +faerie-queene+)
       (setf ship-name "Faerie Queene"))
      (t
       (setf ship-name "Ship???")))
    (return-from format-ship-name ship-name)))

;; TODO - every check for subspace radio damage needs to also check if the ship is cloaked because
;;        the radio doesn't work while cloaked (review all calls checking the radio)
(defun damagedp (device) ; C: #define damaged(dev) (game.damage[dev] != 0.0)
  "Evaluate whether or not a device is damaged."

  (/= (aref *device-damage* device) 0.0))

(defun clear-window ()
  "Clear the current window. Do nothing in line-by-line mode."

  (when *window-interface-p*
    ;;(wclrtoeol *current-window*)
    ;;(wmove *current-window* 0 0)
    (wclear *current-window*)
    (wrefresh *current-window*)))

(defun clear-message-window ()
  "Clear the message window."

  (select-window *message-window*)
  (clear-window))

(defun clear-screen ()
  "Clear all windows."

  (setf *current-window* *short-range-scan-window*)
  (clear-window)
  (setf *current-window* *ship-status-window*)
  (clear-window)
  (setf *current-window* *game-status-window*)
  (clear-window)
  (setf *current-window* *long-range-scan-window*)
  (clear-window)
  (clear-message-window)
  (setf *current-window* *prompt-window*)
  (clear-window)
  (setf *current-window* *starchart-window*)
  (clear-window)
  (setf *current-window* *damage-report-window*)
  (clear-window)
  (setf *current-window* *planet-report-window*)
  (clear-window)
  (setf *current-window* *score-window*)
  (clear-window))

(defun textcolor (color) ; C: void textcolor(int color)

  (when *window-interface-p*
    (cond
      ((= color +default-color+)
       (wattrset *current-window* 0))
      ((= color +black+)
       (wattrset *current-window* (color-pair color_black)))
      ((= color +blue+)
       (wattrset *current-window* (color-pair color_blue)))
      ((= color +green+)
       (wattrset *current-window* (color-pair color_green)))
      ((= color +cyan+)
       (wattrset *current-window* (color-pair color_cyan)))
      ((= color +red+)
       (wattrset *current-window* (color-pair color_red)))
      ((= color +magenta+)
       (wattrset *current-window* (color-pair color_magenta)))
      ((= color +brown+)
       (wattrset *current-window* (color-pair color_yellow)))
      ((= color +light-gray+)
       (wattrset *current-window* (color-pair color_white)))
      ((= color +dark-gray+)
       (wattrset *current-window* (color-pair (logior color_black a_bold))))
      ((= color +light-blue+)
       (wattrset *current-window* (color-pair (logior color_blue a_bold))))
      ((= color +light-green+)
       (wattrset *current-window* (color-pair (logior color_green a_bold))))
      ((= color +light-cyan+)
       (wattrset *current-window* (color-pair (logior color_cyan a_bold))))
      ((= color +light-red+)
       (wattrset *current-window* (color-pair (logior color_red a_bold))))
      ((= color +light-magenta+)
       (wattrset *current-window* (color-pair (logior color_magenta a_bold))))
      ((= color +yellow+)
       (wattrset *current-window* (color-pair (logior color_yellow a_bold))))
      ((= color +white+)
       (wattrset *current-window* (color-pair (logior color_white a_bold))))
      (t ; Same as default case
       (wattrset *current-window* 0)))))

(defun highvideo () ; C: highvideo(void)

  (when *window-interface-p*
    (wattron *current-window* a_reverse)))

(defun calculate-warp-movement-time (&key warp-factor distance)
  "Given a movement speed (warp factor) and distance calculate the amount of game time needed to
move that distance."

  (/ (* 10.0 distance) (expt warp-factor 2)))

;; TODO - write a calculate-remaining-time function

(defun attack-report () ; void attackreport(bool curt)
  "Report the status of bases under attack."

  (cond
    ((is-scheduled-p +commander-destroys-base+)
     (print-message (format nil "Starbase in ~A is currently under Commander attack.~%"
                            (format-quadrant-coordinates *base-under-attack-quadrant*)))
     (print-message (format nil "It can hold out until Stardate ~A.~%"
                            (format-stardate (truncate (scheduled-for +commander-destroys-base+))))))

    ((= *super-commander-attacking-base* 1)
     (print-message (format nil "Starbase in ~A is under Super-commander attack.~%"
                            (format-quadrant-coordinates *super-commander-quadrant*)))
     (print-message (format nil "It can hold out until Stardate ~A.~%"
                            (format-stardate (truncate (scheduled-for +super-commander-destroys-base+))))))

    (t
     (print-message (format nil "No Starbase is currently under attack.~%")))))

(defun update-condition () ; C: void newcnd(void)
  "Update our alert status."

  (setf *condition* +green-status+)
  (when (or (< *ship-energy* 1000.0)
            (> (damaged-device-count) 0))
    (setf *condition* +yellow-status+))
  (when (or (> (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0)
            (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0))
    (setf *condition* +red-status+))
  (when (not *alivep*)
    (setf *condition* +dead+)))

(defun sector-scan (good-scan-p i j) ; C: static void sectscan(int goodScan, int i, int j)
  "Light up an individual dot in a sector."

  ;; Always show the sectors immediately adjacent to the ship
  (if (or good-scan-p
          (and (<= (abs (- i (coordinate-x *ship-sector*))) 1)
               (<= (abs (- j (coordinate-y *ship-sector*))) 1)))
      (progn
        (when (or (string= (aref *quadrant-contents* i j) +materialize-1+)
                  (string= (aref *quadrant-contents* i j) +materialize-2+)
                  (string= (aref *quadrant-contents* i j) +materialize-3+)
                  (string= (aref *quadrant-contents* i j) +enterprise+)
                  (string= (aref *quadrant-contents* i j) +faerie-queene+))
          (cond
            ((= *condition* +red-status+)
             (textcolor +red+))
            ((= *condition* +yellow-status+)
             (textcolor +yellow+))
            ((= *condition* +green-status+)
             (textcolor +green+))
            (*dockedp*
             (textcolor +cyan+))
            ((= *condition* +dead+)
             (textcolor +brown+))
            (t
             (textcolor +default-color+)))
          (when (string= (aref *quadrant-contents* i j) *ship*)
            (highvideo)))
        (print-out (format nil "~A" (aref *quadrant-contents* i j)))
        (textcolor +default-color+)
        (print-out " "))
      (print-out "  ")))

(defun update-chart (x y) ; C: rechart_quad(x, y), more or less
  "Update the star chart page at quadrant coordinate x, y using galaxy data."

  (setf (starchart-page-stars (aref *starchart* x y)) (quadrant-stars (aref *galaxy* x y)))
  (setf (starchart-page-starbases (aref *starchart* x y)) (quadrant-starbases (aref *galaxy* x y)))
  (setf (starchart-page-klingons (aref *starchart* x y)) (quadrant-klingons (aref *galaxy* x y)))
  (setf (quadrant-chartedp (aref *galaxy* x y)) t))

(defun convert-offset-to-direction (x-offset y-offset)
  "Given an x and y offset from a coordinate, convert it to a 'clock face' direction. The magnitude
of the offsets are not significant for determining direction, nor is the actual coordinate value.
To determine the direction convert the x and y offsets each to -1, 0, or 1 and then calculate the
predefined array index containing the direction. The following diagram shows the clock times that
are selected and may help visualize the lookup:
            y

    | -1 |  0 | 1  |
  --+--------------+
  -1|10.5|12.0| 1.5|
  --+--------------+
x  0| 9.0| 0.0| 3.0|
  --+--------------+
   1| 7.5| 6.0| 4.5|
  --+--------------+

A lookup of zero means there is no displacement. This calculation is used when the starship is
buffeted by a nova and therefore is less precise than when moving intentionaly."

  (when (/= x-offset 0)
    (setf x-offset (if (< x-offset 0) -1 1)))
  (when (/= y-offset 0)
    (setf y-offset (if (< y-offset 0) -1 1)))
  ;; C: static double course[] = {0.0, 10.5, 12.0, 1.5, 9.0, 0.0, 3.0, 7.5, 6.0, 4.5};
  ;; C: game.direc = course[3*(icx+1)+icy+2];
  (nth (+ (* 3 (+ x-offset 1)) y-offset 1) (list 10.5 12.0 1.5 9.0 0.0 3.0 7.5 6.0 4.5)))

(defun nova (nova-sector) ; C: void nova(coord nov)
  "A nova occurs. It is the result of having a star hit with a photon torpedo. Stars that go nova
cause stars which surround them to undergo the same probabilistic process. Klingons next to them
are destroyed. And if the starship is next to it, it gets zapped. If the zap is too much, it gets
destroyed.

Apply the effects of the nova to the object around the star then move the ship if it was also
affected."

  (if (< (random 1.0) 0.05)
      ;; Wow! We've supernova'ed
      (supernova *ship-quadrant* nova-sector)
      (progn
        ;; handle initial nova
        (setf (coord-ref *quadrant-contents* nova-sector) +empty-sector+)
        (print-message (format nil "Star at sector ~A novas.~%"
                               (format-sector-coordinates nova-sector)))
        (setf (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))
              (1- (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
        (setf *destroyed-stars* (1+ *destroyed-stars*))
        ;; Apply nova effects to adjacent objects
        (do ((nova-stars (list nova-sector)) ; C: hits[QUADSIZE+1][3], now it's a list
             n-sector
             adjacent-coord ; C: coord scratch
             (nova-pushes 0) ; C: kount
             (change-x 0) ; C: icx
             (change-y 0)) ; C: icy
            ((not nova-stars)
             (when (> nova-pushes 0)
               ;; Starship affected by nova -- kick it away.
               (let (movement-distance
                     movement-direction)
                 (setf movement-distance (* nova-pushes 0.1)) ; one sector for each push
                 (setf movement-direction (convert-offset-to-direction change-x change-y))
                 (when (= movement-direction 0)
                   (setf movement-distance 0))
                 (when (> movement-distance 0)
                   (setf *time-taken-by-current-operation* (/ (* 10.0 movement-distance) 16.0)) ; warp 4
                   (skip-line)
                   (print-message (format nil "Force of nova displaces starship.~%"))
                   ;; TODO - check if movement-distance was reduced due to tractor beam while ship was moving
                   (move-ship-within-quadrant :course movement-direction
                                              :distance movement-distance
                                              :nova-push-p t)
                   (setf *time-taken-by-current-operation* (/ (* 10.0 movement-distance) 16.0))))))
          (setf n-sector (pop nova-stars))
          (do ((adjacent-x (1- (coordinate-x n-sector)) (1+ adjacent-x)))
              ((> adjacent-x (1+ (coordinate-x n-sector))))
            (do ((adjacent-y (1- (coordinate-y n-sector)) (1+ adjacent-y)))
                ((> adjacent-y (1+ (coordinate-y n-sector))))
              (setf adjacent-coord (make-coordinate :x adjacent-x :y adjacent-y))
              (when (and (valid-sector-p adjacent-x adjacent-y)
                         (/= adjacent-x (coordinate-x n-sector))
                         (/= adjacent-y (coordinate-y n-sector)))
                (cond
                  ;; Affect another star
                  ((string= (aref *quadrant-contents* adjacent-x adjacent-y) +star+)
                   (if (< (random 1.0) 0.05)
                       (progn
                         ;; This star supernovas
                         (supernova *ship-quadrant* adjacent-coord)
                         (return-from nova nil))
                       (progn
                         (setf (aref *quadrant-contents* adjacent-x adjacent-y) +empty-sector+)
                         (append nova-stars adjacent-coord)
                         (print-message (format nil "Star at ~A novas.~%"
                                                (format-sector-coordinates adjacent-coord)))
                         (setf (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))
                               (1- (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
                         (setf *destroyed-stars* (1+ *destroyed-stars*)))))
                  ;; Destroy planet
                  ((string= (aref *quadrant-contents* adjacent-x adjacent-y) +planet+)
                   (setf *destroyed-uninhabited-planets* (1+ *destroyed-uninhabited-planets*))
                   (print-message (format nil "Planet at ~A destroyed.~%"
                                          (format-sector-coordinates adjacent-coord)))
                   ;; Update the planet struct to show planet is destroyed, then put it back in the alist
                   (let ((p (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
                     (setf (planet-destroyedp p) t)
                     (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) p))
                   (setf *planet-coord* nil)
                   (setf *current-planet* nil)
                   (when *landedp*
                     (finish +mining-party-nova+)
                     (return-from nova nil))
                   (setf (aref *quadrant-contents* adjacent-x adjacent-y) +empty-sector+))
                  ;; Destroy base
                  ((string= (aref *quadrant-contents* adjacent-x adjacent-y) +starbase+)
                   (setf (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*)) 0)
                   (setf *base-quadrants* (remove *ship-quadrant* *base-quadrants* :test #'coord-equal))
                   (setf *base-sector* nil)
                   (setf *destroyed-bases* (1+ *destroyed-bases*))
                   (update-condition)
                   (print-message (format nil "Starbase at ~A destroyed.~%"
                                          (format-sector-coordinates adjacent-coord)))
                   (setf (aref *quadrant-contents* adjacent-x adjacent-y) +empty-sector+))
                  ;; Buffet ship
                  ((or (string= (aref *quadrant-contents* adjacent-x adjacent-y) +enterprise+)
                       (string= (aref *quadrant-contents* adjacent-x adjacent-y) +faerie-queene+))
                   (print-message (format nil "***Starship buffeted by nova.~%"))
                   (if *shields-are-up-p*
                       (if (>= *shield-energy* 2000.0)
                           (setf *shield-energy* (- *shield-energy* 2000.0))
                           (let ((energy-difference (- 2000.0 *shield-energy*)))
                             (setf *ship-energy* (- *ship-energy* energy-difference))
                             (setf *shield-energy* 0.0)
                             (setf *shields-are-up-p* nil)
                             (print-message (format nil "***Shields knocked out.~%"))
                             (setf (aref *device-damage* +shields+)
                                   (+ (aref *device-damage* +shields+) (* 0.005
                                                                          *damage-factor*
                                                                          (random 1.0)
                                                                          energy-difference)))))
                       (setf *ship-energy* (- *ship-energy* 2000.0)))
                   (when (<= *ship-energy* 0)
                     (finish +nova+)
                     (return-from nova nil))
                   ;; add in course nova contributes to kicking starship
                   (setf change-x (+ change-x (- (coordinate-x *ship-sector*) adjacent-x)))
                   (setf change-y (+ change-y (- (coordinate-y *ship-sector*) adjacent-y)))
                   (setf nova-pushes (1+ nova-pushes)))
                  ;; Kill klingon
                  ((string= (aref *quadrant-contents* adjacent-x adjacent-y) +klingon+)
                   (dead-enemy adjacent-coord +klingon+ adjacent-coord))
                  ;; Damage/destroy big enemies
                  ((or (string= (aref *quadrant-contents* adjacent-x adjacent-y) +commander+)
                       (string= (aref *quadrant-contents* adjacent-x adjacent-y) +super-commander+)
                       (string= (aref *quadrant-contents* adjacent-x adjacent-y) +romulan+))
                   (do ((enemy-index 0 (1+ enemy-index)))
                       ((or (>= enemy-index *enemies-here*)
                            (coord-equal (aref *klingon-sectors* enemy-index) adjacent-coord))
                        (setf (aref *klingon-energy* enemy-index) (- (aref *klingon-energy* enemy-index) 800.0))
                        (if (<= (aref *klingon-energy* enemy-index) 0.0) ; If firepower is lost, die
                            (dead-enemy adjacent-coord (aref *quadrant-contents* adjacent-x adjacent-y) adjacent-coord)
                            (let ((new-coord (make-coordinate :x (+ adjacent-x (- adjacent-x (coordinate-x n-sector)))
                                                              :y (+ adjacent-y (- adjacent-y (coordinate-y n-sector))))))
                              (print-message (format nil "~A at ~A damaged"
                                                 (letter-to-name (aref *quadrant-contents* adjacent-x adjacent-y))
                                                 (format-sector-coordinates adjacent-coord)))
                              (cond
                                ;; can't leave quadrant
                                ((not (valid-sector-p (coordinate-x new-coord) (coordinate-y new-coord)))
                                 (print-message (format nil ".~%")))

                                ((string= (coord-ref *quadrant-contents* new-coord) +black-hole+)
                                 (print-message (format nil ", blasted into black hole at ~A.~%"
                                                        (format-sector-coordinates new-coord)))
                                 (dead-enemy adjacent-coord (aref *quadrant-contents* adjacent-x adjacent-y)
                                             adjacent-coord))
                                ;; can't move into something else
                                ((string/= (coord-ref *quadrant-contents* new-coord) +empty-sector+)
                                 (print-message (format nil ".~%")))

                                (t
                                 (print-message (format nil ", buffeted to ~A.~%"
                                                        (format-sector-coordinates new-coord)))
                                 (setf (coord-ref *quadrant-contents* new-coord)
                                       (aref *quadrant-contents* adjacent-x adjacent-y))
                                 (setf (aref *quadrant-contents* adjacent-x adjacent-y) +empty-sector+)
                                 (setf (aref *klingon-sectors* enemy-index) new-coord)
                                 (setf (aref *klingon-average-distance* enemy-index)
                                       (distance *ship-sector* new-coord))
                                 (setf (aref *klingon-distance* enemy-index)
                                       (distance *ship-sector* new-coord)))))))))
                  ;; Empty space, nothing to do
                  (t
                   ;; +empty-sector+
                   ;; +thing+
                   ;; +black-hole+
                   ;; +tholian+
                   ;; +tholian-web+
                   )))))))))

(defun get-random-star ()
  "Get the sector coordinates of a random star in the current quadrant."

  (let ((sector nil)
        nth-star)
    (when (> (quadrant-stars (coord-ref *galaxy* *ship-quadrant*)) 0)
          (setf sector (make-coordinate))
          (setf nth-star (random (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
          (do ((x 0 (1+ x)))
              ((or (>= x +quadrant-size+)
                   (<= nth-star 0)))
            (do ((y 0 (1+ y)))
                ((or (>= y +quadrant-size+)
                     (<= nth-star 0)))
              (when (string= (aref *quadrant-contents* x y) +star+)
                (setf nth-star (1- nth-star))
                (setf (coordinate-x sector) x)
                (setf (coordinate-y sector) y)))))
    (return-from get-random-star sector)))

(defun supernova (nova-quadrant nova-sector) ; C: supernova(bool induced, coord *w)
  "A star goes supernova."

  (if (or (not (coord-equal nova-quadrant *ship-quadrant*))
          *just-in-p*)
      ;; It isn't here, or we just entered (treat as enroute)
      (when (or (not (damagedp +subspace-radio+))
                *dockedp*)
        (skip-line)
        (print-message (format nil "Message from Starfleet Command       Stardate ~A~%"
                               (format-stardate *stardate*)))
        (print-message (format nil "     Supernova in ~A; caution advised.~%"
                               (format-quadrant-coordinates nova-quadrant))))
      ;; we are in the quadrant!
      (progn
        (skip-line)
        (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
        (skip-line)
        (print-message (format nil "***Incipient supernova detected at ~A~%"
                               (format-sector-coordinates nova-sector)))
        ;; Is the player too close to the supernova to survive?
        (when (<= (+ (expt (- (coordinate-x nova-sector) (coordinate-x *ship-sector*)) 2)
                     (expt (- (coordinate-y nova-sector) (coordinate-y *ship-sector*)) 2))
                  2.1)
          (print-message "Emergency override attempts t")
          (print-message (format nil "***************~%") :print-slowly t)
          (skip-line)
          (print-stars)
          (setf *all-done-p* t))))
  (when (and *base-under-attack-quadrant*
             (coord-equal nova-quadrant *base-under-attack-quadrant*))
    (unschedule +super-commander-destroys-base+)
    (unschedule +commander-destroys-base+)
    (setf *base-under-attack-quadrant* nil))
  ;; Destroy any Klingons in supernovaed quadrant
  ;; and count down the number of remaining klingons as they are removed
  (when (and *super-commander-quadrant*
             (coord-equal nova-quadrant *super-commander-quadrant*))
    ;; did in the Super-Commander!
    (setf *remaining-super-commanders* 0)
    (setf *super-commander-quadrant* nil)
    (setf *super-commander-attacking-base* nil)
    (setf *super-commander-attack-enterprise-p* nil)
    (unschedule +move-super-commander+)
    (setf (quadrant-klingons (coord-ref *galaxy* nova-quadrant))
          (1- (quadrant-klingons (coord-ref *galaxy* nova-quadrant))))
    (setf *remaining-klingons* (1- *remaining-klingons*)))
  ;; TODO - can there be two commanders in one quadrant?
  (when (position nova-quadrant *commander-quadrants* :test #'coord-equal)
    ;; Destroyed a Commander
    (setf *commander-quadrants* (remove nova-quadrant *commander-quadrants* :test #'coord-equal))
    (setf (quadrant-klingons (coord-ref *galaxy* nova-quadrant))
          (1- (quadrant-klingons (coord-ref *galaxy* nova-quadrant))))
    (when (= (length *commander-quadrants*) 0)
      (unschedule +tractor-beam+)
      (unschedule +commander-attacks-base+)
      (unschedule +commander-destroys-base+)))
  (setf *remaining-klingons* (- *remaining-klingons* (quadrant-klingons (coord-ref *galaxy* nova-quadrant))))
  (setf (quadrant-klingons (coord-ref *galaxy* nova-quadrant)) 0)
  ;; destroy Romulans and planets in supernovaed quadrant
  (setf *remaining-romulans* (- *remaining-romulans* (quadrant-romulans (coord-ref *galaxy* nova-quadrant))))
  (setf (quadrant-romulans (coord-ref *galaxy* nova-quadrant)) 0)
  ;; Destroy planet if there is one
  (let ((p (rest (assoc nova-quadrant *planet-information* :test #'coord-equal))))
    (when p ; p should be nil if there is no planet in the quadrant
      (setf (planet-destroyedp p) t)
      (rplacd (assoc nova-quadrant *planet-information* :test #'coord-equal) p))
  ;; Destroy any base in supernovaed quadrant
  (setf *base-quadrants* (remove nova-quadrant *base-quadrants* :test #'coord-equal))
  ;; mark supernova in galaxy and in star chart
  (when (or (coord-equal *ship-quadrant* nova-quadrant)
            (not (damagedp +subspace-radio+))
            *dockedp*)
    (setf (quadrant-supernovap (coord-ref *galaxy* nova-quadrant)) t))
  ;; Either case ends the game but neither necessarily occurs. There is no default case.
  (cond
    ((and (not (coord-equal *ship-quadrant* nova-quadrant))
          (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0))
     ;; If supernova destroys last Klingons give special message
     (skip-line 2)
     ;; TODO - the C source only printed "Lucky you" if the supernova was not caused by a deep space
     ;; probe, that is, not induced by the player. Restore this? If yes, then also restore the score
     ;; updates performed in this function when a probe causes as supernova.
     (print-message (format nil "Lucky you!~%"))
     (print-message (format nil "A supernova in ~A has just destroyed the last Klingons.~%"
                            (format-quadrant-coordinates nova-quadrant)))
     (finish +won+))
    (*all-done-p*
     ;; if some Klingons remain, continue or die in supernova.
     (finish +destroyed-by-supernova+)))))

(defun execute-tractor-beam (&key t-quadrant)
  "A commander or super-commander has tractor beamed the player ship.

t-quadrant is the quadrant to which the ship is pulled"

  (setf *time-taken-by-current-operation*
        (calculate-warp-movement-time :distance (distance t-quadrant *ship-quadrant*)
                                      :warp-factor 7.5)) ; 7.5 is tractor beam yank rate
  (skip-line)
  (print-message (format nil "***~A caught in long range tractor beam--~%" (format-ship-name)))
  ;; If Kirk & Co. screwing around on planet, handle
  (quadrant-exit-while-on-planet +mining+)
  (when *all-done-p*
    (return-from execute-tractor-beam nil))
  (when *in-shuttle-craft-p* ; Caught in Galileo?
    (finish +shuttle-tractor-beam+)
    (return-from execute-tractor-beam nil))
  ;; Check to see if shuttle is aboard
  (when (eql *shuttle-craft-location* 'off-ship)
    (skip-line)
    (if (> (random 1.0) 0.5)
        (progn
          (print-message (format nil "Galileo, left on the planet surface, is captured~%"))
          (print-message (format nil "by aliens and made into a flying McDonald's.~%"))
          (setf (aref *device-damage* +shuttle+) -10) ; TODO - stop using special values in the damage array to store device properties
          (setf *shuttle-craft-location* 'removed)
          (setf *shuttle-craft-quadrant* nil))
        (print-message (format nil "Galileo, left on the planet surface, is well hidden.~%"))))
  (setf *ship-quadrant* t-quadrant)
  (setf *ship-sector* (get-random-sector))
  (print-message (format nil "~A is pulled to ~A, ~A~%"
                         (format-ship-name)
                         (format-quadrant-coordinates *ship-quadrant*)
                         (format-sector-coordinates *ship-sector*)))
  (when *restingp*
    (print-message (format nil "(Remainder of rest/repair period cancelled.)~%"))
    (setf *restingp* nil))
  (when (not *shields-are-up-p*)
    (if (and (not (damagedp +shields+))
             (> *shield-energy* 0))
        (progn
          (shield-actions :raise-shields t) ; raise shields
          (setf *shields-are-changing-p* nil))
        (print-message (format nil "(Shields not currently useable.)~%"))))
  (new-quadrant)
  (attack-player :torpedoes-ok-p nil))

(defun cancel-rest-p () ; C: bool cancelrest(void)
  "Rest period is interrupted by event."

  (when *restingp*
    (skip-line)
    (print-prompt "Mr. Spock-  \"Captain, shall we cancel the rest period?\"")
    (when (get-y-or-n-p)
      (setf *restingp* nil)
      (setf *time-taken-by-current-operation* 0.0)
      (return-from cancel-rest-p t)))
  (return-from cancel-rest-p nil))

;; TODO - Should this function also be used when one of our torpedoes destroys a starbase? Messages
;;        to the player are different.
(defun destroy-starbase (base-q)
  "A commander or the super-commander destroys a starbase."

  ;; No default case - if the ship is not in good repair then the base will be destroyed without
  ;; notice to the player including no change to the star chart (the player is in for a nasty surprise)
  ;; TODO - if no notice is given here, should the player be notified if a short- or long-range
  ;;        scan shows no starbase, where previously a starbase existed? Spock would notice, he is
  ;;        able to reconstruct a starchart from memory!
  (cond
    ((coord-equal *ship-quadrant* base-q)
     ;; Handle case where base is in same quadrant as starship
     (setf (coord-ref *quadrant-contents* *base-sector*) +empty-sector+)
     (setf *base-sector* nil)
     (update-condition)
     (skip-line)
     (print-message (format nil "Spock-  \"Captain, I believe the starbase has been destroyed.\"~%"))
     (setf (starchart-page-starbases (coord-ref *starchart* base-q)) 0))
    ;; Get word via subspace radio
    ((and (> (length *base-quadrants*) 0) ; Need an existing base to transmit the message
          (or (not (damagedp +subspace-radio+)) ; and a radio to receive it.
              *dockedp*))
     (skip-line)
     (print-message (format nil "Lt. Uhura-  \"Captain, Starfleet Command reports that~%"))
     (print-message (format nil "   the starbase in ~A has been destroyed by~%"
                            (format-quadrant-coordinates base-q)))
     (if (= *super-commander-attacking-base* 2)
         (print-message (format nil "the Klingon Super-Commander~%"))
         (print-message (format nil "a Klingon Commander~%")))
     (setf (starchart-page-starbases (coord-ref *starchart* base-q)) 0)))
  ;; Remove Starbase from galaxy
  (setf (quadrant-starbases (coord-ref *galaxy* base-q)) 0)
  (setf *base-quadrants* (remove base-q *base-quadrants* :test #'coord-equal)))

(defun move-super-commander-one-quadrant (destination-quadrant ; C: iq
                                          avoidp) ; C: avoid
  "Super-commander movement helper. Move the super-commander into a new quadrant if possible.
Return true on successful move."

  ;; Check for reasons to not perform the move
  (when (or (coord-equal destination-quadrant *ship-quadrant*)
            (not (valid-quadrant-p (coordinate-x destination-quadrant)
                                   (coordinate-y destination-quadrant)))
            (quadrant-supernovap (coord-ref *galaxy* destination-quadrant))
            (> (quadrant-klingons (coord-ref *galaxy* destination-quadrant))
               (- +max-klingons-per-quadrant+ 1))
            (and *just-in-p*
                 (not *super-commander-attack-enterprise-p*)))
    (return-from move-super-commander-one-quadrant nil))
  (when avoidp
    ;; Avoid quadrants with bases if we want to avoid Enterprise
    (dolist (bq *base-quadrants*)
      (when (coord-equal bq destination-quadrant)
        (return-from move-super-commander-one-quadrant nil))))
  ;; Do the move
  (setf (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*))
        (1- (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*))))
  (setf *super-commander-quadrant* destination-quadrant)
  (setf (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*))
        (1+ (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*))))
  (when (> *super-commanders-here* 0)
    ;; SC has scooted, remove him from current quadrant
    (setf *super-commander-attack-enterprise-p* nil)
    (setf *super-commander-attacking-base* 0)
    (setf *super-commanders-here* 0)
    (setf *attempted-escape-from-super-commander-p* nil)
    (unschedule +super-commander-destroys-base+)
    (do ((i 0 (1+ i)))
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant-contents* (aref *klingon-sectors* i)) +super-commander+))
         (setf (coord-ref *quadrant-contents* (aref *klingon-sectors* i)) +empty-sector+)
         (setf (aref *klingon-sectors* i) (aref *klingon-sectors* (1- *enemies-here*)))
         (setf (aref *klingon-distance* i) (aref *klingon-distance* (1- *enemies-here*)))
         (setf (aref *klingon-average-distance* i) (aref *klingon-average-distance* (1- *enemies-here*)))
         (setf (aref *klingon-energy* i) (aref *klingon-energy* (1- *enemies-here*)))))
    (setf *klingons-here* (1- *klingons-here*))
    (setf *enemies-here* (1- *enemies-here*))
    (update-condition)
    (sort-klingons))
  ;; Check for a helpful planet
  (let ((helpful-planet (rest (assoc *super-commander-quadrant* *planet-information* :test #'coord-equal))))
    (when (and helpful-planet
               (eql (planet-crystals helpful-planet) 'present))
      ;; Destroy the planet
      (setf (planet-destroyedp helpful-planet) t)
      (rplacd (assoc *super-commander-quadrant* *planet-information* :test #'coord-equal) helpful-planet)
      (when (or *dockedp*
                (not (damagedp +subspace-radio+)))
        (print-message (format nil "Lt. Uhura-  \"Captain, Starfleet Intelligence reports~%"))
        (print-message (format nil "   a planet in ~A has been destroyed~%"
                               (format-quadrant-coordinates *super-commander-quadrant*)))
        (print-message (format nil "   by the Super-commander.\"~%")))))
  (return-from move-super-commander-one-quadrant t)) ; Looks good!

(defun move-super-commander () ; C: supercommander(void)
  "Move the Super-Commander"

  ;; Decide on being active or passive
  (let ((avoidp (or (< (/ (+ (- *initial-commanders* (length *commander-quadrants*))
                             (- *initial-klingons* *remaining-klingons*))
                          (- (+ *stardate* 0.01) *initial-stardate*))
                       (* 0.1 (skill-level-value *skill-level*)
                          (+ (skill-level-value *skill-level*) 1.0)))
                    (< (- *stardate* *initial-stardate*) 3.0)))
        delta-x delta-y)
    (if (and (not *super-commander-attack-enterprise-p*)
             avoidp)
        ;; Compute move away from Enterprise
        (progn
          (setf delta-x (- (coordinate-x *super-commander-quadrant*) (coordinate-x *ship-quadrant*)))
          (setf delta-y (- (coordinate-y *super-commander-quadrant*) (coordinate-y *ship-quadrant*)))
          (when (> (sqrt (+ (expt delta-x 2) (expt delta-y 2))) 2.0)
            ;; Circulate in space
            (setf delta-x (- (coordinate-y *super-commander-quadrant*) (coordinate-y *ship-quadrant*)))
            (setf delta-y (- (coordinate-x *super-commander-quadrant*) (coordinate-x *ship-quadrant*)))))
        ;; Attack a starbase if possible
        (progn
          (when (= (length *base-quadrants*) 0)
            ;; Nothing left to do
            (unschedule +move-super-commander+)
            (return-from move-super-commander nil))
          (let ((candidate-bases ()) ; Candidates for attack
                (commander-at-closest-p t)
                (closest-base nil))
            ;; Filter bases by no Enterprise, not too many Klingons, and not already under attack
            (dolist (bq *base-quadrants*)
              (unless (or (coord-equal bq *ship-quadrant*)
                          (coord-equal bq *base-under-attack-quadrant*)
                          (> (quadrant-klingons (coord-ref *galaxy* bq)) (1- +max-klingons-per-quadrant+)))
                (setf candidate-bases (append candidate-bases bq))))
            ;; From the available candidates, find the closest one that doesn't have a commander.
            ;; If there is a commander, and no other base is appropriate,
            ;; we will take the one with the commander
            (when (> (length candidate-bases) 0)
              (setf closest-base (first candidate-bases))
              (dolist (cb candidate-bases)
                (when (and (<= (distance cb *super-commander-quadrant*)
                               (distance closest-base *super-commander-quadrant*))
                           (or (not (find cb *commander-quadrants* :test #'coord-equal))
                               commander-at-closest-p))
                  (setf closest-base cb)
                  (setf commander-at-closest-p (if (find cb *commander-quadrants* :test #'coord-equal) t nil)))))
            (if closest-base
                ;; Decide how to move toward base
                (progn
                  (setf delta-x (- (coordinate-x closest-base) (coordinate-x *super-commander-quadrant*)))
                  (setf delta-y (- (coordinate-y closest-base) (coordinate-y *super-commander-quadrant*))))
                ;; Nothing suitable -- wait until next time
                (return-from move-super-commander nil)))))
    ;; Maximum movement is 1 quadrant in either or both axis
    (when (> delta-x 1)
      (setf delta-x 1))
    (when (< delta-x -1)
      (setf delta-x -1))
    (when (> delta-y 1)
      (setf delta-y 1))
    (when (< delta-y -1)
      (setf delta-y -1))
    ;; Try moving in both x and y directions
    (let ((destination-quadrant (make-coordinate :x (+ (coordinate-x *super-commander-quadrant*) delta-x) ; iq
                                                 :y (+ (coordinate-y *super-commander-quadrant*) delta-y))))
      (unless (move-super-commander-one-quadrant destination-quadrant avoidp)
        ;; Failed -- try some other maneuvers
        (if (or (= delta-x 0)
                (= delta-y 0))
            ;; Attempt angle move
            (if (/= delta-x 0)
                (progn
                  (setf (coordinate-y destination-quadrant) (+ (coordinate-y *super-commander-quadrant*) 1))
                  (unless (move-super-commander-one-quadrant destination-quadrant avoidp)
                    (setf (coordinate-y destination-quadrant) (+ (coordinate-y *super-commander-quadrant*) 1))
                    (move-super-commander-one-quadrant destination-quadrant avoidp)))
                (progn
                  (setf (coordinate-x destination-quadrant) (+ (coordinate-x *super-commander-quadrant*) 1))
                  (unless (move-super-commander-one-quadrant destination-quadrant avoidp)
                    (setf (coordinate-x destination-quadrant) (+ (coordinate-x *super-commander-quadrant*) 1))
                    (move-super-commander-one-quadrant destination-quadrant avoidp))))
            (progn
              ;; Try moving just in x or y
              (setf (coordinate-y destination-quadrant) (+ (coordinate-y *super-commander-quadrant*) 1))
              (unless (move-super-commander-one-quadrant destination-quadrant avoidp)
                (setf (coordinate-y destination-quadrant) (+ (coordinate-y *super-commander-quadrant*) delta-y))
                (setf (coordinate-x destination-quadrant) (coordinate-x *super-commander-quadrant*))
                (move-super-commander-one-quadrant destination-quadrant avoidp))))))
    ;; Check for a base
    (if (= (length *base-quadrants*) 0)
        (unschedule +move-super-commander+)
        (dolist (bq *base-quadrants*)
          ;; TODO - if  *base-under-attack-quadrant* refers to commander actions then it would appear that
          ;;        the super-commander only attacks bases already under attack
          ;; TODO - Is this where the SC decides to attack a base or is it somewhere else?
          (when (and (coord-equal bq *super-commander-quadrant*)
                     (coord-equal *base-under-attack-quadrant* *super-commander-quadrant*))
            ;; Attack the base
            (unless avoidp ; no, don't attack base!
              (setf *base-attack-report-seen-p* nil)
              (setf *super-commander-attacking-base* 1)
              (schedule-event +super-commander-destroys-base+ (+ 1.0 (* 2.0 (random 1.0))))
              (when (is-scheduled-p +commander-destroys-base+)
                (postpone-event +super-commander-destroys-base+
                                (- (scheduled-for +commander-destroys-base+) *stardate*)))
              (when (and (damagedp +subspace-radio+)
                         (not *dockedp*))
                (return-from move-super-commander nil)) ; No warning
              (setf *base-attack-report-seen-p* t)
              (print-message (format nil "Lt. Uhura-  \"Captain, the starbase in ~A~%"
                                     (format-quadrant-coordinates *super-commander-quadrant*)))
              (print-message (format nil "   reports that it is under attack from the Klingon Super-commander.~%"))
              (print-message (format nil "   It can survive until stardate ~D.\"~%"
                                     (scheduled-for +super-commander-destroys-base+)))
              (when *restingp*
                (print-prompt "Mr. Spock-  \"Captain, shall we cancel the rest period?\"")
                (when (get-y-or-n-p)
                  (setf *restingp* nil)
                  (setf *time-taken-by-current-operation* 0.0))) ; actually finished
              (return-from move-super-commander nil))))))
  ;; Check for intelligence report
  ;; TODO - If super-commander is in the same quadrant as a base but not attacking then an
  ;;        intelligence report should be sent as long as the ship is docked or the subspace
  ;;        radio is working.
  (when (or (<= (random 1.0) 0.2)
            (not (damagedp +subspace-radio+))
            *dockedp*
            (quadrant-chartedp (coord-ref *galaxy* *super-commander-quadrant*)))
    (print-message (format nil "Lt. Uhura-  \"Captain, Starfleet Intelligence reports~%"))
    (print-message (format nil "   the Super-commander is in ~A.\"~%"
                           (format-quadrant-coordinates *super-commander-quadrant*)))))

(defun schedule-event (event-type offset)
  "Schedule an event of the specific type to occur offset time units in the future. Return the event.
This isn't a real event queue a la BSD Trek yet -- you can only have one event of each type active
at any given time.  Mostly these means we can only have one FDISTR/FENSLV/FREPRO sequence going at
any given time; BSD Trek, from which we swiped the idea, can have up to 5."

  (setf (aref *future-events* event-type) (+ *stardate* offset))
  (return-from schedule-event (aref *future-events* event-type)))

(defun process-events () ; C: events(void)
  "Run through the event queue looking for things to do. This function manages the passing of time.
The time period to manage is the amount of time taken by the last player or event operation. Check
for events that occur naturally as time passes such as running out of life support reserves or
repairing devices, and then trigger any scheduled events that should have occurred during the
operation time. If no operational time has passed and no event should have been fired by the time
of the current stardate then exit this function to return to the command prompt and let the player
enter a command.

Only one tractor beam event can be in progress during an invocation of process-events. Check the
Super-Commander first on every iteration but after either a commander or Super-Commander has
tractor-beamed the ship then the other will not."

  ;; TODO - can/should player input be handled as an event?

  ;; finish-date is the stardate at the time this invocation of process-events is complete
  (do ((finish-date (+ *stardate* *time-taken-by-current-operation*))
       smallest-next-date ; C: datemin, the next stardate at which an event will occur
       execution-time ; C: xtime, time take by an event within the current operation time
       event-code ; C: evcode
       (commander-used-tractor-beam-p nil) ; C: ictbeam
       (super-commander-used-tractor-beam-p nil) ; C: istract
       (allow-player-input nil))
      ((or allow-player-input
           *all-done-p*)) ; events in a previous iteration may have ended the game
    ;; Select earliest extraneous event, evcode==0 if no events
    (setf event-code +spy+)
    (setf smallest-next-date finish-date)
    (do ((l 0 (1+ l)))
        ((>= l (length *future-events*)))
      (when (and (aref *future-events* l) ; The event must be non-nil for the < comparison
                 (< (aref *future-events* l) smallest-next-date))
        (setf event-code l)
        (setf smallest-next-date (aref *future-events* l))))
    (setf execution-time (- smallest-next-date *stardate*))
    (when *cloakedp*
      (setf *ship-energy* (- *ship-energy* (* execution-time 500.0))) ; cloaking uses energy!
      (when (<= *ship-energy* 0)
        (finish +out-of-energy+)
        (return-from process-events nil)))
    (setf *stardate* smallest-next-date) ; Advance game time
    ;; Decrement Federation resources and recompute remaining time
    (setf *remaining-resources* (- *remaining-resources*
                                   (* (+ *remaining-klingons* (* 4 (length *commander-quadrants*))) execution-time)))
    (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
        (setf *remaining-time* (/ *remaining-resources* (+ *remaining-klingons* (* 4 (length *commander-quadrants*)))))
        (setf *remaining-time* 99))
    (when (< *remaining-time* 0)
      (finish +deplete+)
      (return-from process-events nil))
    ;; Any crew left alive?
    (when (<= *crew* 0)
      (finish +all-crew-killed+)
      (return-from process-events nil))
    ;; Is life support adequate?
    (when (and (damagedp +life-support+)
               (not *dockedp*))
      (when (and (< *life-support-reserves* execution-time)
                 (> (aref *device-damage* +life-support+) *life-support-reserves*))
        (finish +life-support-consumed+)
        (return-from process-events nil))
      (setf *life-support-reserves* (- *life-support-reserves* execution-time))
      (when (<= (aref *device-damage* +life-support+) execution-time)
        (setf *life-support-reserves* *initial-life-support-reserves*)))
    ;; Fix devices
    ;; TODO - resting at starbase for the period of time in the damage report doesn't apply the
    ;;        docking factor for device repairs. Tested by ramming an enemy, calling for help,
    ;;        and then resting.
    (let ((repair-time execution-time) ; C: repair
          (repaired-devices (make-array +number-of-devices+ :initial-element nil))) ; C: fixed_dev
      (when *dockedp*
        (setf repair-time (/ repair-time +docked-repair-factor+)))
      ;; Don't fix Deathray here. It's not fixed incrementally, but only after a long continuous time docked.
      (do ((l 0 (1+ l)))
          ((>= l +number-of-devices+))
        (when (and (> (aref *device-damage* l) 0.0)
                   (/= l +death-ray+))
          (if (> (aref *device-damage* l) repair-time)
              (setf (aref *device-damage* l) (- (aref *device-damage* l) repair-time))
              (progn
                (setf (aref *device-damage* l) 0.0)
                (setf (aref repaired-devices l) t)))))
      ;; If radio repaired, update star chart and attack reports
      (when (aref repaired-devices +subspace-radio+)
        (print-message (format nil "Lt. Uhura- \"Captain, the sub-space radio is working and~%"))
        (print-message (format nil "   surveillance reports are coming in.~%"))
        (skip-line)
        (when (not *base-attack-report-seen-p*)
          ;; TODO - rename either attack-report or the boolean tracking if it needs to be called
          (attack-report)
          (setf *base-attack-report-seen-p* t))
        (print-message (format nil "   The star chart is now up to date.\"~%"))
        (skip-line))
      (when (or (aref repaired-devices +subspace-radio+)
                (aref repaired-devices +long-range-sensors+)
                (aref repaired-devices +short-range-sensors+))
        (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*)))
      ;; When in curses mode, if the short range sensors were just repaired and there is a planet
      ;; in the current quadrant that has not been examined (unknown), automatically examine it.
      (when (and *window-interface-p*
                 (aref repaired-devices +short-range-sensors+)
                 *current-planet*
                 (not (planet-knownp (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal)))))
        (sensor)))
    ;; Time was spent so subtract it from the operation time being handled by this invocation
    (setf *time-taken-by-current-operation* (- *time-taken-by-current-operation* execution-time))
    ;; Cause extraneous event event-code to occur (C: EVCODE)
    (cond
      ;; Supernova
      ((= event-code +supernova+)
       ;; Select a quadant for the supernova and blow it up. This algorithm will find a
       ;; quadrant with stars if one exists, unlike get-random-quadrant which could select
       ;; a quadrant already containing a supernova or a quadrant without stars.
       (do (star-to-supernova
            (galaxy-star-count 0)
            (x 0 (1+ x)))
           ((>= x +galaxy-size+)
            (when (> galaxy-star-count 0) ; If something to supernova exists
              ;; Logic changed here so that we won't favor quadrants in top left of universe.
              (setf star-to-supernova (random galaxy-star-count))
              (do ((supernova-q (make-coordinate))
                   (supernova-s nil)
                   (i 0 (1+ i)))
                  ((or (>= i +galaxy-size+)
                       (<= star-to-supernova 0))
                   ;; If the quadrant contains the player then also select the star
                   (when (coord-equal supernova-q *ship-quadrant*)
                     (setf supernova-s (get-random-star)))
                   (supernova supernova-q supernova-s))
                (do ((j 0 (1+ j)))
                    ((or (>= j +galaxy-size+)
                         (<= star-to-supernova 0)))
                  (setf star-to-supernova (- star-to-supernova (quadrant-stars (aref *galaxy* i j))))
                  (setf (coordinate-x supernova-q) i)
                  (setf (coordinate-y supernova-q) j)))))
         (do ((y 0 (1+ y)))
             ((>= y +galaxy-size+))
           (setf galaxy-star-count (+ galaxy-star-count (quadrant-stars (aref *galaxy* x y))))))
       (schedule-event +supernova+ (expran (* 0.5 *initial-time*)))
       (when (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
         (setf allow-player-input t))) ; C: return
      ;; Check with spy to see if S.C. should tractor beam
      ((= event-code +spy+)
       (cond
         ((or (= *remaining-super-commanders* 0)
              commander-used-tractor-beam-p
              super-commander-used-tractor-beam-p
              *dockedp*
              *cloakedp* ; Cannot tractor beam if we can't be seen!
              (= *super-commander-attacking-base* 1)
              *super-commander-attack-enterprise-p*)
          (setf allow-player-input t))

         ((or *attempted-escape-from-super-commander-p*
              (and (< *ship-energy* 2000)
                   (< *torpedoes* 4)
                   (< *shield-energy* 1250))
              (and (damagedp +phaser+)
                   (or (damagedp +photon-torpedoes+)
                       (< *torpedoes* 4)))
              (and (damagedp +shields+)
                   (or (< *ship-energy* 2500)
                       (damagedp +phaser+))
                   (or (< *torpedoes* 5)
                       (damagedp +photon-torpedoes+))))
          ;; The ship is weak, tractor-beam her!
          (setf super-commander-used-tractor-beam-p t)
          (execute-tractor-beam :t-quadrant *super-commander-quadrant*)
          (unless *all-done-p*
            ;; Adjust finish date to date of tractor beaming
            (setf finish-date (+ *stardate* *time-taken-by-current-operation*))))

         (t
          (setf allow-player-input t))))
      ;; Tractor beam
      ((= event-code +tractor-beam+)
       ;; TODO - can/should commander events be unscheduled when the last commander is destroyed?
       ;;        Tentatively, yes, because any other check for a scheduled commander event could
       ;;        incorrectly return true if there are no commanders remaining.
       (if (= (length *commander-quadrants*) 0)
           (progn
             (unschedule +tractor-beam+)
             (unschedule +commander-attacks-base+)
             (unschedule +commander-destroys-base+))
           (let ((commander-index 0)) ; default to the first commander in the list
             ;; Select a random commander if there is a choice of more than one
             (when (> (length *commander-quadrants*) 1)
               (setf commander-index (random (1- (length *commander-quadrants*)))))
             (if (or super-commander-used-tractor-beam-p
                     *dockedp*
                     *cloakedp* ; Cannot tractor beam if we can't be seen!
                     (coord-equal (nth commander-index *commander-quadrants*) *ship-quadrant*))
                 ;; Drats! Have to reschedule
                 (schedule-event +tractor-beam+ (+ *time-taken-by-current-operation*
                                                   (expran (/ (* 1.5 *initial-time*)
                                                              (length *commander-quadrants*)))))
                 (progn
                   (setf commander-used-tractor-beam-p t)
                   ;; TODO - tractor beam did not end game while captain was on shuttle craft
                   (execute-tractor-beam :t-quadrant (nth commander-index *commander-quadrants*))
                   (unless *all-done-p*
                     ;; Adjust finish time to time of tractor beaming
                     (setf finish-date (+ *stardate* *time-taken-by-current-operation*)))
                   (if (= (length *commander-quadrants*) 0)
                       (progn
                         (unschedule +tractor-beam+)
                         (unschedule +commander-attacks-base+)
                         (unschedule +commander-destroys-base+))
                       (schedule-event +tractor-beam+ (+ *time-taken-by-current-operation*
                                                         (expran (/ (* 1.5 *initial-time*)
                                                                    (length *commander-quadrants*)))))))))))
      ;; Snapshot of the universe (for time warp)
      ((= event-code +snapshot-for-time-warp+)
       (setf *snapshot* (make-snapshot :crew *crew*
                                       :remaining-klingons *remaining-klingons*
                                       :remaining-super-commanders *remaining-super-commanders*
                                       :remaining-resources *remaining-resources*
                                       :remaining-time *remaining-time*
                                       :remaining-romulans *remaining-romulans*
                                       :destroyed-bases *destroyed-bases*
                                       :destroyed-stars *destroyed-stars*
                                       :destroyed-inhabited-planets *destroyed-inhabited-planets*
                                       :destroyed-uninhabited-planets *destroyed-uninhabited-planets*
                                       :planet-information *planet-information*
                                       :stardate *stardate*
                                       :shuttle-craft-location *shuttle-craft-location*
                                       :shuttle-craft-quadrant *shuttle-craft-quadrant*
                                       :base-quadrants *base-quadrants*
                                       :commander-quadrants *commander-quadrants*
                                       :super-commander-quadrant *super-commander-quadrant*
                                       :galaxy *galaxy*
                                       :starchart *starchart*))
       (setf *snapshot-taken-p* t)
       (schedule-event +snapshot-for-time-warp+ (expran (* 0.5 *initial-time*))))
      ;; Commander attacks starbase
      ((= event-code +commander-attacks-base+)
       (if (or (= (length *commander-quadrants*) 0)
               (= (length *base-quadrants*) 0))
           (progn
             ;; no can do
             (unschedule +commander-attacks-base+)
             (unschedule +commander-destroys-base+))
           ;; Look for a base quadrant that has a commander in it, as long as the commander is not
           ;; the super-commander and the quadrant is not the current ship quadrant.
           ;; TODO - these loops might be replaceable with find
           (let ((commander-quad nil)) ; coordinates of a commander at a base
             (dolist (bq *base-quadrants*)
               (dolist (cq *commander-quadrants*)
                 (when (and (coord-equal bq cq)
                            (not (coord-equal bq *ship-quadrant*))
                            ;; easy games don't have a supercommander
                            (or (not *super-commander-quadrant*)
                                (not (coord-equal bq *super-commander-quadrant*))))
                   (setf commander-quad cq))))
             (if commander-quad
                 (progn
                   ;; commander + starbase combination found -- launch attack
                   (setf *base-under-attack-quadrant* commander-quad)
                   (schedule-event +commander-destroys-base+ (1+ (random 3.0))) ; C: 1.0+3.0*Rand()
                   ;; TODO - this when clause seems inconsistent with the base search because bases
                   ;;        under attack by the SC are skipped.
                   (when (= *super-commander-attacking-base* 1) ; extra time if SC already attacking
                     (postpone-event +commander-destroys-base+
                                     (- (scheduled-for +super-commander-destroys-base+) *stardate*)))
                   (schedule-event +commander-attacks-base+
                                   (+ (scheduled-for +commander-destroys-base+) (expran (* 0.3 *initial-time*))))
                   (setf *base-attack-report-seen-p* nil)
                   (when (or (not (damagedp +subspace-radio+)) ; No warning :-( if radio not available
                             *dockedp*)
                     (setf *base-attack-report-seen-p* t)
                     (skip-line)
                     (print-message (format nil "Lt. Uhura-  \"Captain, the starbase in ~A"
                                            (format-quadrant-coordinates *base-under-attack-quadrant*)))
                     (print-message (format nil "   reports that it is under attack and that it can~%"))
                     (print-message (format nil "   hold out only until stardate ~A.\"~%"
                                            (format-stardate (scheduled-for +commander-destroys-base+))))
                     (when (cancel-rest-p)
                       (return-from process-events nil)))                               )
                 (progn
                   ;; no match found -- try later
                   (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*)))
                   (unschedule +commander-destroys-base+))))))
      ;; Super-Commander destroys base
      ((= event-code +super-commander-destroys-base+)
       (unschedule +super-commander-destroys-base+)
       ;; TODO - The logic below would seem to leave the super-commander in a "destroying a base"
       ;;        state if, for some reason, there is no base to destroy
       (setf *super-commander-attacking-base* 2) ; TODO - get rid of the magic constant
       (when (> (quadrant-starbases (coord-ref *galaxy* *super-commander-quadrant*)) 0)
         (destroy-starbase *super-commander-quadrant*)
         (setf *super-commander-attacking-base* 0)))
      ;; Commander succeeds in destroying base
      ((= event-code +commander-destroys-base+)
       (unschedule +commander-destroys-base+)
       ;; TODO - do commanders actually destroy bases? Test the game. The original C code seemed
       ;;        to engage in pointless loops and if statements, specifically, determining if there
       ;;        was a commander present to destroy the base, if there were any bases at all, if there
       ;;        was a base in the quadrant where the destruction was scheduled to occur. Those checks
       ;;        seem pointless because if a commander moves away from a base
       ;;        under attack it is the job of the movement function to undschedule the destruction.
       ;;        This event handler has been re-written to just do it.
       (destroy-starbase *base-under-attack-quadrant*)
       (setf *base-under-attack-quadrant* nil))
      ;; Super-Commander moves
      ((= event-code +move-super-commander+)
       (schedule-event +move-super-commander+ 0.2777) ; TODO name the magic constant
       (when (and (not *attempted-escape-from-super-commander-p*)
                  (not super-commander-used-tractor-beam-p)
                  (/= *super-commander-attacking-base* 1)
                  (or (not *super-commander-attack-enterprise-p*)
                      (not *just-in-p*)))
         (move-super-commander)))
      ;; Move deep space probe
      ((= event-code +move-deep-space-probe+)
       (schedule-event +move-deep-space-probe+ 0.01)
       ;; When the probe quadrant changes provide an update to the player
       (setf *probe-x-coord* (+ *probe-x-coord* *probe-x-increment*))
       (setf *probe-y-coord* (+ *probe-y-coord* *probe-y-increment*))
       (let ((i (truncate (+ (/ *probe-x-coord* +quadrant-size+) 0.05)))
             (j (truncate (+ (/ *probe-y-coord* +quadrant-size+) 0.05))))
         (when (or (/= (coordinate-x *probe-reported-quadrant*) i)
                   (/= (coordinate-y *probe-reported-quadrant*) j))
           (setf (coordinate-x *probe-reported-quadrant*) i)
           (setf (coordinate-y *probe-reported-quadrant*) j)
           ;; No update if no working subspace radio
           (when (or (not (damagedp +subspace-radio+))
                     *dockedp*)
             (skip-line)
             (print-message "Lt. Uhura-  \"The deep space probe ")
             (cond ((not (valid-quadrant-p (coordinate-x *probe-reported-quadrant*)
                                           (coordinate-y *probe-reported-quadrant*)))
                    (print-message "has left the galaxy")
                    (unschedule +move-deep-space-probe+))

                   ((quadrant-supernovap (coord-ref *galaxy* *probe-reported-quadrant*))
                    (print-message "is no longer transmitting")
                    (unschedule +move-deep-space-probe+))

                   (t
                    (print-message (format nil "is now in ~A" (format-quadrant-coordinates
                                                               *probe-reported-quadrant*)))))
             (print-message (format nil ".\"~%")))))
       (when (is-scheduled-p +move-deep-space-probe+)
         ;; Update star chart if Radio is working or have access to radio.
         (when (or (not (damagedp +subspace-radio+))
                   *dockedp*)
           (setf (starchart-page-klingons (coord-ref *starchart* *probe-reported-quadrant*))
                 (quadrant-klingons (coord-ref *galaxy* *probe-reported-quadrant*)))
           (setf (starchart-page-starbases (coord-ref *starchart* *probe-reported-quadrant*))
                 (quadrant-starbases (coord-ref *galaxy* *probe-reported-quadrant*)))
           (setf (starchart-page-stars (coord-ref *starchart* *probe-reported-quadrant*))
                 (quadrant-stars (coord-ref *galaxy* *probe-reported-quadrant*)))
           (setf (quadrant-chartedp (coord-ref *galaxy* *probe-reported-quadrant*))
                 t))
         (setf *moves-for-probe* (1- *moves-for-probe*)) ; One less to travel
         (when (<= *moves-for-probe* 0)
           (unschedule +move-deep-space-probe+)
           (when (and *probe-is-armed-p*
                      (> (quadrant-stars (coord-ref *galaxy* *probe-reported-quadrant*)) 0))
             ;; lets blow the sucker!
             (let ((supernova-sector nil))
               (when (coord-equal *probe-reported-quadrant* *ship-quadrant*)
                 ;; TODO Select the star closest to the probe
                 (setf supernova-sector (get-random-star)))
               (supernova *probe-reported-quadrant* supernova-sector))
             ;; If starship caused supernova, tally up destruction.
             ;; This comment is inaccurate because only probes directly cause supernovas,
             ;; although a ship can indirectly cause one if a novaed star goes supernova.
             ;; TODO - hold the player responsible if a star novaed by a torpedo turns into a
             ;;        supernova?
             (setf *destroyed-stars* (+ *destroyed-stars* (quadrant-stars (coord-ref *galaxy* *probe-reported-quadrant*))))
             (setf *destroyed-bases* (+ *destroyed-bases* (quadrant-starbases (coord-ref *galaxy* *probe-reported-quadrant*))))
             ;; TODO inhabited worlds are not counted but should be
             (when (assoc *probe-reported-quadrant* *planet-information* :test #'coord-equal) ; non-nil if planet exists
               (setf *destroyed-uninhabited-planets* (1+ *destroyed-uninhabited-planets*)))
             (setf *probe-reported-quadrant* nil)
             (when (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
               (return-from process-events nil))))))
      ;; Inhabited system issues distress call
      ((= event-code +distress-call-from-inhabited-world+)
       (unschedule +distress-call-from-inhabited-world+)
       ;; Try a whole bunch of times to find something suitable.
       ;; TODO - or just make a list of candidate planets/quadrants, randomly select one, and then
       ;;        randomly decide to attack it, or not? Not sure what the frequency of success in
       ;;        "try a whole bunch of times" is, so it might be difficult to tune the randomness.
       (do ((i 0 (1+ i))
            (candidate-quadrant nil)
            ;;candidate-planet-index ; convenience variable
            candidate-planet)
           ((or (> i 100)
                candidate-quadrant)
            (setf *conquest-quadrant* candidate-quadrant))
         (setf candidate-quadrant (get-random-quadrant))
         (setf candidate-planet (rest (assoc candidate-quadrant *planet-information* :test #'coord-equal)))
         (unless (and (not (coord-equal *ship-quadrant* candidate-quadrant))
                      candidate-planet
                      (not (planet-inhabitedp candidate-planet))
                      (not (quadrant-supernovap (coord-ref *galaxy* candidate-quadrant)))
                      (= (quadrant-status (coord-ref *galaxy* candidate-quadrant)) +secure+)
                      (> (quadrant-klingons (coord-ref *galaxy* candidate-quadrant)) 0))
           (setf candidate-quadrant nil)))
       (when *conquest-quadrant*
         ;; got one!!  Schedule its enslavement
         (schedule-event +inhabited-world-is-enslaved+ (expran *initial-time*))
         (setf (quadrant-status (coord-ref *galaxy* *conquest-quadrant*)) +distressed+)
         ;; tell the captain about it if we can
         (when (or (not (damagedp +subspace-radio+))
                   *dockedp*)
           (skip-line)
           (print-message (format nil "Lt. Uhura- Captain, ~A in ~A reports it is under attack"
                                  (planet-name (rest (assoc *conquest-quadrant* *planet-information*
                                                            :test #'coord-equal)))
                                  (format-quadrant-coordinates *conquest-quadrant*)))
           (print-message (format nil "by a Klingon invasion fleet.~%"))
           (when (cancel-rest-p)
             (return-from process-events nil)))))
      ;; Starsystem is enslaved
      ((= event-code +inhabited-world-is-enslaved+)
       (unschedule +inhabited-world-is-enslaved+)
       ;; TODO should this status change when the last klingon in the quadrant is destroyed?
       ;; see if current distress call still active
       (if (> (quadrant-klingons (coord-ref *galaxy* *conquest-quadrant*))
              0)
           (progn
             (setf (quadrant-status (coord-ref *galaxy* *conquest-quadrant*))
                   +enslaved+)
             ;; play stork and schedule the first baby
             (schedule-event +klingons-build-ship-in-enslaved-system+ (expran (* 2.0 *initial-time*)))
             ;; report the disaster if we can
             (when (or (not (damagedp +subspace-radio+))
                       *dockedp*)
               (print-message (format nil "Lt. Uhura- We've lost contact with starsystem ~A"
                                      (planet-name (rest (assoc *conquest-quadrant*
                                                                *planet-information*
                                                                :test #'coord-equal)))))
               (print-message (format nil "in ~A." (format-quadrant-coordinates *conquest-quadrant*)))))
           (progn
             (setf (quadrant-status (coord-ref *galaxy* *conquest-quadrant*))
                   +secure+)
             (setf *conquest-quadrant* nil))))
      ;; Klingon reproduces
      ((= event-code +klingons-build-ship-in-enslaved-system+)
       ;; TODO should this status change when the last klingon in the quadrant is destroyed?
       ;; see if current distress call still active
       (if (> (quadrant-klingons (coord-ref *galaxy* *conquest-quadrant*))
              0)
           (progn
             (schedule-event +klingons-build-ship-in-enslaved-system+ (* 1.0 *initial-time*))
             (unless (>= *remaining-klingons* +max-klingons-per-game+) ; full right now
               ;; reproduce one Klingon
               (let ((build-quadrant nil))
                 (if (< (quadrant-klingons (coord-ref *galaxy* *conquest-quadrant*))
                        +max-klingons-per-quadrant+)
                     (setf build-quadrant *conquest-quadrant*)
                     (progn ; this quadrant not ok, pick an adjacent one
                       ;; TODO - avoid favoring the top left quadrant by randomly selecting an
                       ;;        adjacent quadrant from a list of eligible quadrants
                       (do ((i (1- (coordinate-x *conquest-quadrant*)) (1+ i)))
                           ((or (> i (1+ (coordinate-x *conquest-quadrant*)))
                                build-quadrant))
                         (do ((j (1- (coordinate-y *conquest-quadrant*)) (1+ j)))
                             ((or (> j (1+ (coordinate-y *conquest-quadrant*)))
                                  build-quadrant))
                           (when (and (valid-quadrant-p i j)
                                      (< (quadrant-klingons (aref *galaxy* i j)) +max-klingons-per-quadrant+)
                                      (not (quadrant-supernovap (aref *galaxy* i j))))
                             (setf build-quadrant (make-coordinate :x i :y j)))))))
                 (when build-quadrant
                   ;; deliver the child
                   (setf *remaining-klingons* (1+ *remaining-klingons*))
                   (setf (quadrant-klingons (coord-ref *galaxy* build-quadrant))
                         (1+ (quadrant-klingons (coord-ref *galaxy* build-quadrant))))
                   (when (coord-equal *ship-quadrant* build-quadrant)
                     ;; TODO move this multiple value bind to a function, it is also called elsewhere
                     (multiple-value-bind (coordinates distance power) (drop-klingon-in-sector)
                       (setf (aref *klingon-sectors* (1+ (length *klingon-sectors*))) coordinates)
                       (setf (aref *klingon-distance* (1+ (length *klingon-distance*))) distance)
                       (setf (aref *klingon-average-distance* (1+ (length *klingon-average-distance*))) distance)
                       (setf (aref *klingon-energy* (1+ (length *klingon-energy*))) power))
                     ;; TODO - call sort-klingons? seems to go along with the array setting
                     (setf *klingons-here* (1+ *klingons-here*))
                     (setf *enemies-here* (1+ *enemies-here*)))
                   ;; recompute time left (ported directly from the C source)
                   (setf *remaining-time* (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
                                              (/ *remaining-resources* (+ *remaining-klingons*
                                                                          (* 4 (length *commander-quadrants*))))
                                              99))
                   ;; report the disaster if we can
                   (cond
                     ((and (coord-equal *ship-quadrant* build-quadrant)
                           (not (damagedp +short-range-sensors+)))
                      ;; TODO - this formatting doesn't look right. Check it, and use newlines or
                      ;;        combine the two statements
                      (print-message (format nil "Spock- sensors indicate the Klingons have"))
                      (print-message (format nil "launched a warship from ~A."
                                  (planet-name (rest (assoc *conquest-quadrant* *planet-information*
                                                            :test #'coord-equal))))))
                     ((or (not (damagedp +subspace-radio+))
                          *dockedp*)
                      (print-message (format nil "Lt. Uhura- Starfleet reports increased Klingon activity"))
                      (print-message (format nil "near ~A in ~A."
                                             (planet-name (rest (assoc *conquest-quadrant*
                                                                       *planet-information*
                                                                       :test #'coord-equal)))
                                             (format-quadrant-coordinates build-quadrant)))))))))
           (progn
             (setf (quadrant-status (coord-ref *galaxy* *conquest-quadrant*))
                   +secure+)
             (setf *conquest-quadrant* nil)))))))

(defun damaged-device-count ()
  "Return the number of damaged devices."

    (do ((c 0 (1+ c))
         (damage-count 0))
        ((>= c +number-of-devices+)
         (return-from damaged-device-count damage-count))
      (when (> (aref *device-damage* c) 0)
        (setf damage-count (1+ damage-count)))))

(defun game-status (&optional (line-to-print nil))
  "Print game statuses next to ship status. The line to print is one of

   1 - Stardate
   2 - Klingons left
   3 - Time left
"

  (when (= line-to-print 1)
    (print-out (format nil "Stardate ~A~%" (format-stardate *stardate*))))
  (when (= line-to-print 2)
    (print-out (format nil "Klingons Left ~A~%" (+ *remaining-klingons*
                                                   (length *commander-quadrants*)
                                                   *remaining-super-commanders*))))
  (when (= line-to-print 3)
    (print-out (format nil "Time Left ~,2,,,F~%" *remaining-time*))))

(defun ship-status (&optional (line-to-print nil)) ; C: void status(int req)
  "Print status reports next to short range scan lines. The classic line to print is one of

   1 - Stardate
   2 - Condition
   3 - Position
   4 - Life Support
   5 - Warp Factor
   6 - Energy
   7 - Torpedoes
   8 - Shields
   9 - Klingons left
  10 - Time left

Also available are

   11 - Planets
   12 - Attack report

With the addition of probes and other ship information the new status lines are

   1 - Condition
   2 - Position
   3 - (Un)Cloaked
   4 - Life Support
   5 - Warp Factor
   6 - Energy
   7 - Torpedoes
   8 - Shields
   9 - Probes

There is no line 10.
"

  (when (= line-to-print 1)
    (print-out "Condition ")
    (when *dockedp*
      ;; TODO - update the player condition every turn, not only when checking status
      (update-condition))
    (cond
      ((= *condition* +red-status+)
       (print-out "RED"))
      ((= *condition* +yellow-status+)
       (print-out "YELLOW"))
      ((= *condition* +green-status+)
       (print-out "GREEN"))
      ((= *condition* +dead+)
       (print-out "DEAD")))
    (when (> (damaged-device-count) 0)
      (print-out (format nil ", ~A DAMAGE~A" (damaged-device-count)
                         (if (> (damaged-device-count) 1) "S" ""))))
    (skip-line))
  (when (= line-to-print 2)
    (print-out (format nil "Position ~A , ~A" (format-coordinates *ship-quadrant*) (format-coordinates *ship-sector*)))
    ;; Print flight status with position
    (cond
      (*dockedp*
       (print-out ", Docked"))
      (*in-orbit-p*
       (print-out ", In Orbit")))
    (skip-line))
  (when (= line-to-print 3)
    (if (damagedp +cloaking-device+)
        (print-out (format nil "Cloaking Device DAMAGED~%"))
        (if *cloakedp*
            (print-out (format nil "Cloaked~%"))
            (print-out (format nil "Not cloaked~%")))))
  (when (= line-to-print 4)
    (print-out "Life Support ")
    (if (damagedp +life-support+)
        (progn
          (print-out "DAMAGED, ")
          (if *dockedp*
              (print-out (format nil "Base provides~%"))
              (print-out (format nil "reserves=~4,2F~%" *life-support-reserves*))))
        (print-out (format nil "Active~%"))))
  (when (= line-to-print 5)
    (print-out (format nil "Warp Factor ~,1F~%" *warp-factor*)))
  (when (= line-to-print 6)
    (print-out (format nil "Energy ~,1F" *ship-energy*))
    (when *dilithium-crystals-on-board-p*
      (print-out " (have crystals)"))
    (skip-line))
  (when (= line-to-print 7)
    (print-out (format nil "~34@<Torpedoes ~A~>" *torpedoes*))
    (if *window-interface-p*
        (skip-line)
        (game-status 1)))
  (when (= line-to-print 8)
    (print-out (format nil "~34@<Shields ~A ~A% ~,1F units~>"
                       (cond
                         ((damagedp +shields+)
                          "DAMAGED,")
                         (*shields-are-up-p*
                          "UP,")
                         ((not *shields-are-up-p*)
                          "DOWN,"))
                       (truncate (+ (/ (* 100.0 *shield-energy*) *initial-shield-energy*) 0.5))
                       *shield-energy*))
    (if *window-interface-p*
        (skip-line)
        (game-status 2)))
  (when (= line-to-print 9)
    (print-out (format nil "~34@<Probe~A~>"
                       (if (is-scheduled-p +move-deep-space-probe+)
                           (format nil " in ~A" (format-quadrant-coordinates *probe-reported-quadrant*))
                           (format nil "s ~A" *probes-available*))))
    (if *window-interface-p*
        (skip-line)
        (game-status 3)))
  (when (= line-to-print 11)
    (let ((p (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
      (if (and p ; p is nil when there is no planet in the quadrant
               (not (planet-inhabitedp p)))
          (print-out (format nil "Major system ~A~%" (planet-name p)))
          (print-out (format nil "Sector is uninhabited~%")))))
  (when (= line-to-print 12)
    (when (and (is-scheduled-p +commander-destroys-base+)
               (not *base-under-attack-quadrant*))
      (print-out (format nil "Base in ~A attacked by C. Alive until ~A~%"
                         (format-coordinates *base-under-attack-quadrant*)
                         (format-stardate (find-event +commander-destroys-base+)))))
    (when (> *super-commander-attacking-base* 0)
      (print-out (format nil "Base in ~A attacked by S. Alive until ~A~%"
                         (format-coordinates *super-commander-quadrant*)
                         (format-stardate (find-event +super-commander-destroys-base+)))))))

(defun all-statuses ()
  "Display all the ship statuses that are displayed next to the short range scan. In
line-by-line mode also display the game statuses that are displayed next to the short
range scan."

    (if *window-interface-p*
      (if *ship-status-window*
          (progn
            (select-window *ship-status-window*)
            (clear-window)
            (wmove *ship-status-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (ship-status 1)
  (ship-status 2)
  (ship-status 3)
  (ship-status 4)
  (ship-status 5)
  (ship-status 6)
  (ship-status 7)
  (ship-status 8)
  (ship-status 9)

  ;; In line-by-line mode game statuses are printed by the ship status function
  (when *window-interface-p*
    (select-window *game-status-window*)
    (clear-window)
    (wmove *game-status-window* 0 0)
    (game-status 1)
    (game-status 2)
    (game-status 3)))

(defun short-range-scan ()

    (if *window-interface-p*
      (if *short-range-scan-window*
          (progn
            (select-window *short-range-scan-window*)
            (clear-window)
            (wmove *short-range-scan-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (let ((good-scan-p t))
    (if (damagedp +short-range-sensors+)
        ;; Allow base's sensors if docked
        (if (not *dockedp*)
            (progn
              (print-out (format nil "  S.R. SENSORS DAMAGED!~%"))
              (setf good-scan-p nil))
            (print-out (format nil " [Using Base's sensors]~%")))
        (print-out (format nil "     SHORT-RANGE SCAN~%")))
    (when good-scan-p
      (setf (quadrant-chartedp (coord-ref *galaxy* *ship-quadrant*)) t)
      (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*)))
    (print-out (format nil "   1 2 3 4 5 6 7 8 9 10~%"))
    (when (not *dockedp*)
      (update-condition))
    (do ((i 0 (+ i 1)))
        ((>= i +quadrant-size+))
      (print-out (format nil "~2@A " (1+ i)))
      (do ((j 0 (1+ j)))
          ((>= j +quadrant-size+))
        (sector-scan good-scan-p i j))
      (if *window-interface-p*
          (skip-line)
          (progn
            (print-out " ")
            (ship-status (1+ i)))))))

(defun long-range-scan ()
  "Scan the galaxy using long-range sensors and update the star chart. Display the results of the scan.
Long-range sensors can scan all adjacent quadrants."

  (if *window-interface-p*
      (if *long-range-scan-window*
          (progn
            (select-window *long-range-scan-window*)
            (clear-window)
            (wmove *long-range-scan-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (print-out (format nil "LONG-RANGE SCAN~%"))
  (if (or (not (damagedp +long-range-sensors+))
           *dockedp*)
      (progn
        (when (damagedp +long-range-sensors+)
            (print-out (format nil "Starbase's sensors~%")))
        (do ((x (- (coordinate-x *ship-quadrant*) 1) (+ x 1)))
            ((> x (+ (coordinate-x *ship-quadrant*) 1)))
          (print-out " ")
          (do ((y (- (coordinate-y *ship-quadrant*) 1) (+ y 1)))
              ((> y (+ (coordinate-y *ship-quadrant*) 1)))
            (if (valid-quadrant-p x y)
                (progn
                  (update-chart x y)
                  (if (quadrant-supernovap (aref *galaxy* x y))
                      (print-out " ***")
                      (print-out (format nil "~4D" (+ (* (starchart-page-klingons (aref *starchart* x y)) 100)
                                                      (* (starchart-page-starbases (aref *starchart* x y)) 10)
                                                      (starchart-page-stars (aref *starchart* x y)))))))
                (print-out (format nil "~4D" -1))))
          (skip-line)))
      (print-out (format nil "SENSORS DAMAGED~%"))))

(defun draw-windows () ; C: void drawmaps(void)
  "Perform the automatic display updates available to curses-enabled terminals."

  (when *window-interface-p*
    (short-range-scan)
    (all-statuses)
    (long-range-scan)
    ;; In curses mode sensors work automatically. Call before updating the display, but only once
    ;; per entry into the quadrant.
    (when *just-in-p*
      (sensor))
    (when *starchart-window*
      (chart))
    (when *damage-report-window*
      (damage-report))
    (when *planet-report-window*
      (survey))
    (when *score-window*
      (score))
    (select-window *message-window*)))

(defun print-prompt (prompt-to-print)
  "Print a string. In curses mode print it in the prompt window, otherwise just print it."

  (if *window-interface-p*
      (if *prompt-window*
          (progn
            (select-window *prompt-window*)
            (clear-window)
            (wmove *prompt-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (print-out prompt-to-print))

;; TODO - move-coordinate is effective when the calling function has displaced the enemy just before
;;        destroying them, e.g. nova buffet or photon torpedo displacedment. Can the move be completed
;;        by the calling function before killing the enemy? Black holes are a problem because they should
;;        not be replaced by an empty sector.
(defun dead-enemy (enemy-coordinate enemy move-coordinate) ; C: deadkl(coord w, feature type, coord mv)
  "Kill a Klingon, Tholian, Romulan, or Thingy. move-coordinate allows enemy to move before dying."

  ;; move-coordinate allows an enemy to "move" before dying
  (print-message (format nil "~A at ~A" (letter-to-name enemy) (format-sector-coordinates move-coordinate)))
  ;; Decide what kind of enemy it is and update appropriately
  (cond
    ;; Chalk up a Romulan
    ((string= enemy +romulan+)
     (setf (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*))
           (1- (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*))))
     (setf *romulans-here* (1- *romulans-here*))
     (setf *remaining-romulans* (1- *remaining-romulans*)))
    ;; Killed a Tholian
    ((string= enemy +tholian+)
     (setf *tholians-here* 0))
    ;; Killed a Thingy
    ((string= enemy +thing+)
     (setf *things-here* 0)
     (setf *thing-is-angry-p* nil)
     (setf *thing-location* nil))
    ;; Some type of a Klingon
    ((string= enemy +commander+)
     (setf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))
           (1- (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))))
     (setf *klingons-here* (1- *klingons-here*))
     (setf *commander-quadrants* (remove *ship-quadrant* *commander-quadrants* :test #'coord-equal))
     (unschedule +tractor-beam+)
     (when (> (length *commander-quadrants*) 0)
       (schedule-event +tractor-beam+ (expran (/ *initial-commanders* (length *commander-quadrants*))))))

    ((string= enemy +klingon+)
     (setf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))
           (1- (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))))
     (setf *klingons-here* (1- *klingons-here*))
     (setf *remaining-klingons* (1- *remaining-klingons*)))

    ((string= enemy +super-commander+)
     (setf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))
           (1- (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))))
     (setf *klingons-here* (1- *klingons-here*))
     (setf *remaining-super-commanders* (1- *remaining-super-commanders*))
     (setf *super-commanders-here* 0)
     (setf *super-commander-quadrant* nil)
     (setf *super-commander-attacking-base* 0)
     (setf *super-commander-attack-enterprise-p* nil)
     (unschedule +move-super-commander+)
     (unschedule +super-commander-destroys-base+)))

  ;; For each kind of enemy, finish message to player
  (print-message (format nil " destroyed.~%"))
  (setf (coord-ref *quadrant-contents* enemy-coordinate) +empty-sector+)
  (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*))
  (when (> (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
    (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
        (setf *remaining-time* (/ *remaining-resources* (+ *remaining-klingons* (* 4 (length *commander-quadrants*)))))
        (setf *remaining-time* 99))
    ;; Remove enemy ship from arrays describing local conditions
    (when (and (is-scheduled-p +commander-destroys-base+)
               (coord-equal *base-under-attack-quadrant* *ship-quadrant*)
               (string= enemy +commander+))
      (unschedule +commander-destroys-base+))
    (do ((i 0 (1+ i))) ; find the enemy sector
        ((or (> i *enemies-here*)
             (coord-equal (aref *klingon-sectors* i) enemy-coordinate))
         (setf *enemies-here* (1- *enemies-here*))
         (when (< i *enemies-here*)
           (do ((j i (1+ j))) ; shift all elements towards the start of the array
               ((>= j *enemies-here*))
             (setf (aref *klingon-sectors* j) (aref *klingon-sectors* (1+ j)))
             (setf (aref *klingon-energy* j) (aref *klingon-energy* (1+ j)))
             (setf (aref *klingon-distance* j) (aref *klingon-distance* (1+ j)))
             (setf (aref *klingon-average-distance* j) (aref *klingon-distance* (1+ j)))
             ))))))

(defun apply-critical-hit (hit) ; C: void fry(double hit)
  "Apply a critical hit."

  ;; a critical hit occured
  (when (>= hit (* (- 275.0 (* 25.0 (skill-level-value *skill-level*)) (+ 1.0 (* 0.5 (random 1.0))))))
    (print-message (format nil "***CRITICAL HIT--~%"))
    ;; Select devices and cause damage
    (do ((hit-count 0 (1+ hit-count)) ; C: loop1 (really?)
         (number-of-hits (truncate (+ 1.0 (/ hit (+ 500.0 (* 100.0 (random 1.0))))))) ; C: ncrit
         (devices-damaged (make-array +number-of-devices+)))
        ((>= hit-count number-of-hits)
         ;; Display damaged devices
         (do ((i 0 (1+ i)))
             ((>= i number-of-hits))
           (print-message (format nil "~A " (aref *devices* (aref devices-damaged i))))
           (when (and (> number-of-hits 1)
                      (= i (- number-of-hits 2)))
             (print-message "and "))
           )
         (print-message (format nil "damaged.~%")))
      ;; Select a random device
      (do ((device-index nil))
          (device-index
           ;; Record which device was damaged so the name can be displayed later
           (setf (aref devices-damaged hit-count) device-index)
           ;; Damage the device
           (setf (aref *device-damage* device-index) (+ (aref *device-damage* device-index)
                                                        (/ (* hit *damage-factor*)
                                                           (* number-of-hits
                                                              (+ 75.0
                                                                 (* 25.0 (random 1.0))))))))
        (setf device-index (get-random-device))
        ;; Cheat to prevent shuttle damage unless on ship
        (when (or (< (aref *device-damage* device-index) 0.0) ; TODO -10 is a special shuttle damage value used when it is not on the ship
                  (and (= device-index +shuttle+)
                       (not (eql *shuttle-craft-location* 'on-ship))))
          (setf device-index nil)))))
    (when (and (damagedp +shields+)
               *shields-are-up-p*)
      (print-message (format nil "***Shields knocked down.~%"))
      (setf *shields-are-up-p* nil))
  (when (and (damagedp +cloaking-device+)
             *cloakedp*)
    (print-message (format nil "***Cloaking device rendered inoperative.~%"))
    (setf *cloakedp* nil))
  (skip-line))

(defun check-for-phasers-overheating (requested-energy) ; C: void overheat(double rpow)
  "Check for phasers overheating"

  (when (> requested-energy +max-safe-phaser-power+)
    (let ((burn (* (- requested-energy +max-safe-phaser-power+) 0.00038))) ; TODO - name the constant
      (when (<= (random 1.0) burn)
        (print-message (format nil "Weapons officer Sulu-  \"Phasers overheated, sir.\"~%"))
        (setf (aref *device-damage* +phaser+) (* *damage-factor* (+ 1 (random 1.0)) (+ 1.0 burn)))))))

(defun energy-to-kill-enemy (i)
  "Calculate the amount of phaser energy needed to kill an enemy in the enemies list. i is the
index of the enemy in the global array. This is the energy needed without any 'fuzz factors'
applied."

  (return-from energy-to-kill-enemy (/ (abs (aref *klingon-energy* i))
                                       (* +phaser-factor+
                                          (expt 0.90 (aref *klingon-distance* i))))))

(defun recommended-energy-for-enemy (i)
  "Calculate the amount of energy recommending for killing one enemy with phaser fire. This
calculation adds a 'fuzz factor' to account for the uncertainy of space battles.

The parameter is the index into the array of enemies of the enemy to kill."

  (return-from recommended-energy-for-enemy
    (+ (* (energy-to-kill-enemy i) (+ 1.01 (* 0.05 (random 1.0)))) 1.0)))

(defun recommended-energy ()
  "Calculate the amount of energy recommended for killing all enemies with phaser fire. This
calculation adds a 'fuzz factor' to account for the uncertainly of space battles."

  (let ((rec 0))
    (do ((i 0 (1+ i)))
        ((>= i *enemies-here*))
      (setf rec (+ rec (recommended-energy-for-enemy i))))
    (return-from recommended-energy rec)))

(defun toggle-high-speed-shield-control (phaser-energy) ; C: bool checkshctrl(double rpow)
  "Change the state of the shields from up to down, or vice-versa depending on the current shield
state, using the high-speed shield control. Check for the unfortunate events that can occur during
high-speed shield operation and apply their effects. The high-speed shield control is used during
phaser firing and phaser-energy is the amount fired.

The caller is required to check if the high-speed shield control is available and to know the state
of the shields before calling this function.

Return t if the shields were successfully raised or lowered, nil if there was a malfunction."

  (if *shields-are-up-p*
      ;; Lower shields
      (progn
        (skip-line)
        (if (< (random 1.0) 0.998)
            (progn
              (print-message (format nil "Shields lowered.~%"))
              (setf *shields-are-up-p* nil)
              (return-from toggle-high-speed-shield-control t))
            (progn
              ;; Something bad has happened
              (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
              (skip-line 2)
              (let* ((energy-hit (/ (* phaser-energy *shield-energy*) *initial-shield-energy*))
                     (casualties (truncate (* energy-hit (random 1) 0.012))))
                (setf *ship-energy* (- *ship-energy* (+ phaser-energy (* energy-hit 0.8))))
                (setf *shield-energy* (- *shield-energy* (* energy-hit 0.2)))
                (when (<= *ship-energy* 0.0)
                  (print-message (format nil "Sulu-  \"Captain! Shield malf***********************\"~%")
                                 :print-slowly t)
                  (skip-line)
                  (print-stars)
                  (finish +phaser+)
                  (return-from toggle-high-speed-shield-control nil))
                (print-message (format nil "Sulu-  \"Captain! Shield malfunction! Phaser fire contained!\"~%")
                               :print-slowly t)
                (skip-line 2)
                (print-message (format nil "Lt. Uhura-  \"Sir, all decks reporting damage.\"~%"))
                (skip-line)
                (apply-critical-hit (* 0.8 energy-hit))
                (when (> casualties 0)
                  (skip-line)
                  (print-message (format nil "McCoy to bridge- \"Severe radiation burns, Jim.~%"))
                  (print-message (format nil "  ~A casualties so far.\"~%" casualties))
                  (setf *casualties* (+ *casualties* casualties))
                  (setf *crew* (- *crew* casualties))))
              (skip-line)
              (print-message (format nil "Phaser energy dispersed by shields.~%"))
              (print-message (format nil "Enemy unaffected.~%"))
              (check-for-phasers-overheating phaser-energy)
              (return-from toggle-high-speed-shield-control nil))))
      ;; Raise shields
      (if (< (random 1.0) 0.99)
          (progn
            (print-message (format nil "Shields raised.~%"))
            (setf *shields-are-up-p* t)
            (return-from toggle-high-speed-shield-control t))
          (progn
            (print-message (format nil "Sulu-  \"Sir, the high-speed shield control has malfunctioned . . .~%"))
            (print-message (format nil "         CLICK   CLICK   POP  . . .~%") :print-slowly t)
            (print-message (format nil " No response, sir!~%"))
            (setf *shields-are-up-p* nil)
            (return-from toggle-high-speed-shield-control nil)))))

(defun apply-phaser-hits (hits) ; C: hittem(double *hits)
  "Apply phaser hits to Klingons and Romulans."

  ;; TODO - Initially, the number of enemies and the number of entries in the hits array are the
  ;;        same. As enemies are killed the enemies array becomes shorter but the hits array does
  ;;        not. Convert this index tracking to some sort of structure that uses lists.
  (do ((hit-index 0 (1+ hit-index))
       (enemy-index 0 (1+ enemy-index))
       (dust-factor (+ 0.9 (* 0.01 (random 1.0))) ; amount by which delivered power is reduced over distance
                    (+ 0.9 (* 0.01 (random 1.0)))) ; different for each enemy
       hit ; The hit amount applied to the current enemy
       initial-enemy-energy
       enemy-energy
       (e-coord (make-coordinate))) ; convenience variable
      ((>= hit-index (length hits)))
    (skip-line)
    (when (> (aref hits hit-index) 0)
      (setf hit (* (aref hits hit-index) (expt dust-factor (aref *klingon-distance* enemy-index))))
      (setf initial-enemy-energy (aref *klingon-energy* enemy-index))
      (setf enemy-energy (abs initial-enemy-energy)) ; apparently, enemy energy can be negative
      (when (< (* +phaser-factor+ hit) enemy-energy)
        (setf enemy-energy (* +phaser-factor+ hit)))
      (if (< (aref *klingon-energy* enemy-index) 0)
          (setf (aref *klingon-energy* enemy-index) (- (aref *klingon-energy* enemy-index) (- enemy-energy)))
          (setf (aref *klingon-energy* enemy-index) (- (aref *klingon-energy* enemy-index) enemy-energy)))
      (setf e-coord (aref *klingon-sectors* enemy-index))
      (if (> hit 0.005)
          (progn
            (when (not (damagedp +short-range-sensors+))
              (boom e-coord))
            (print-message (format nil "~A unit hit on ~A at ~A~%" (truncate hit)
                                   (letter-to-name (coord-ref *quadrant-contents* e-coord))
                                   (format-sector-coordinates e-coord))))
          (print-message (format nil "Very small hit on ~A at ~A~%"
                                 (letter-to-name (coord-ref *quadrant-contents* e-coord))
                                 (format-sector-coordinates e-coord))))
      (when (string= (coord-ref *quadrant-contents* e-coord)
                     +thing+)
        (setf *thing-is-angry-p* t))
      (if (= (aref *klingon-energy* enemy-index) 0)
          (progn
            (dead-enemy e-coord (coord-ref *quadrant-contents* e-coord)
                        e-coord)
            (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
              (finish +won+))
            (when *all-done-p*
              (return-from apply-phaser-hits t))
            (1- enemy-index)) ; don't do the increment, enemy array has one less klingon
          ;; decide whether or not to emasculate klingon
          (when (and (> (aref *klingon-energy* enemy-index) 0)
                     (>= (random 1.0) 0.9)
                     (<= (aref *klingon-energy* enemy-index) (+ 0.4 (* 0.4 (random 1.0) initial-enemy-energy))))
            (print-message (format nil "***Mr. Spock-  \"Captain, the vessel at ~A~%"
                                   (format-sector-coordinates e-coord)))
            (print-message (format nil "   has just lost its firepower.\"~%"))
            (setf (aref *klingon-energy* enemy-index) 0))))))

(defun fire-phasers () ; C: phasers()

  (skip-line)
  (when *dockedp*
    (print-message (format nil "Phasers can't be fired through base shields.~%"))
    (return-from fire-phasers nil))
  (when (damagedp +phaser+)
    (print-message (format nil "Phaser control damaged.~%"))
    (return-from fire-phasers nil))
  (let ((targeting-support-available-p ; C: itarg, SR sensors and Computer are needed for automatic fire
         (not (or (damagedp +short-range-sensors+)
                  (damagedp +computer+))))
        (shield-control-available-p nil) ; C: ifast
        (available-energy 0)
        (requested-energy nil) ; C: rpow, nil when there was no player input
        (raise-shields-p *shields-are-up-p*) ; C: no - Raise shields after firing.
        (fire-mode nil))
    (when *shields-are-up-p*
      (when (damagedp +shield-control+)
        (print-message (format nil "High speed shield control damaged.~%"))
        (return-from fire-phasers nil))
      (when (<= *ship-energy* 200.0)
        (print-message (format nil "Insufficient energy to activate high-speed shield control.~%"))
        (return-from fire-phasers nil))
      (print-message (format nil "Weapons Officer Sulu-  \"High-speed shield control enabled, sir.\"~%"))
      (setf shield-control-available-p t))
    (setf available-energy (if shield-control-available-p (- *ship-energy* 200) *ship-energy*))
    ;; Here are valid forms of the phaser command. n (for "no") is optional, # is a number. "n" can
    ;; appear almost anywhere except the list of amounts to fire in manual mode.
    ;; pha a # n
    ;; pha n a #
    ;; pha a n #
    ;; pha # n
    ;; pha n #
    ;; pha m n # # #...
    ;; pha n m # # #...
    ;; pha m # # # #...
    ;; Read at most three values from the input to obtain the firing mode. A maximum of three
    ;; values are needed for automatic mode so read them here. Amounts to fire will be prompted for
    ;; later if not provided.
    (do ((token-count 0 (1+ token-count)))
        ((> token-count 2)
         (when (not fire-mode)
           (huh)
           (return-from fire-phasers nil)))
      (if (= (length *line-tokens*) 0)
          (when (not fire-mode)
            (print-prompt "Manual or automatic? ")
            (scan-input)
            (cond
              ((or (not *input-item*); no input, player typed enter/return
                   (match-token *input-item* (list "automatic")))
               (setf fire-mode 'automatic))

              ((match-token *input-item* (list "manual"))
               (setf fire-mode 'manual))

              (t
               (huh)
               (return-from fire-phasers nil))))
          (progn
            (scan-input)
            (cond
              ((numberp *input-item*)
               (if (eql fire-mode 'manual)
                   (unscan-input) ; Don't read manual mode energy until the firing loop.
                   (when (not requested-energy) ; If energy was already provided then don't overwrite it.
                     (setf fire-mode 'automatic)
                     (setf requested-energy *input-item*))))

              ((match-token *input-item* (list "automatic"))
               (setf fire-mode 'automatic))

              ((and (not fire-mode)
                    (match-token *input-item* (list "manual")))
               (setf fire-mode 'manual))

              ((match-token *input-item* (list "no"))
               (setf raise-shields-p nil))

              (t
               (huh)
               (return-from fire-phasers nil))))))

    (when (and (eql fire-mode 'automatic)
               (not targeting-support-available-p))
      (setf fire-mode 'force-manual))

    (when (= *enemies-here* 0)
      (when (or (eql fire-mode 'manual)
                (eql fire-mode 'force-manual))
        (print-message (format nil "There is no enemy present to select.~%"))
        (clear-type-ahead-buffer)
        (setf fire-mode 'automatic)) ; In automatic mode a single value is input
      (print-message (format nil "Energy will be expended into space.~%")))

    (if (eql fire-mode 'automatic)
        (progn
          (when (and (not requested-energy)
                     (> *enemies-here* 0))
            (print-message "Phasers locked on target. "))
          (do ()
              ((and requested-energy
                    (< requested-energy available-energy)))
            (print-message (format nil "Energy available= ~,2F~%" available-energy))
            (clear-type-ahead-buffer)
            (setf *input-item* nil)
            (print-prompt (format nil "~D units required. Units to fire: " (ceiling (recommended-energy))))
            (scan-input)
            (when (not (numberp *input-item*))
              (return-from fire-phasers nil))
            (setf requested-energy *input-item*))
          (when (<= requested-energy 0)
            ;; chicken out
            (return-from fire-phasers nil))
          (when (and shield-control-available-p
                     *shields-are-up-p*)
            (setf *ship-energy* (- *ship-energy* 200)) ; Go and do it!
            (when (not (toggle-high-speed-shield-control requested-energy))
              ;; High-speed shield control failed, phaser fire process interrupted
              (return-from fire-phasers nil)))
          ;; Fire!
          (setf *ship-energy* (- *ship-energy* requested-energy))
          (let ((excess-energy requested-energy) ; C: extra
                (remaining-energy requested-energy) ; C: powrem
                (hits (make-array *enemies-here*))) ; C: hits, but sized to fit
            (when (> *enemies-here* 0)
              (setf excess-energy 0.0)
              (do ((i 0 (1+ i))
                   over ; 'fuzz factor' to ensure enemy is destroyed
                   temp)
                  ((>= i *enemies-here*))
                (setf (aref hits i) 0.0)
                (when (> remaining-energy 0)
                  (setf (aref hits i) (energy-to-kill-enemy i))
                  (setf over (* (+ 0.01 (* 0.05 (random 1.0))) (aref hits i)))
                  ;; Is the total amount to fire at this enemy more than the remaining energy?
                  (setf temp remaining-energy)
                  (setf remaining-energy (- remaining-energy (+ (aref hits i) over)))
                  (when (and (<= remaining-energy 0.0)
                             (< temp (aref hits i)))
                    (setf (aref hits i) temp)) ; Use up all remaining energy
                  (when (<= remaining-energy 0.0)
                    (setf over 0.0))
                  (setf excess-energy (+ excess-energy over))))
              (when (> remaining-energy 0.0)
                (setf excess-energy (+ excess-energy remaining-energy)))
              (apply-phaser-hits hits)
              (setf *action-taken-p* t))
            (when (and (> excess-energy 0)
                       (not *all-done-p*))
              (skip-line)
              (if (> *tholians-here* 0)
                  (progn
                    (print-message "*** Tholian web absorbs ")
                    (when (> *enemies-here* 0)
                      (print-message "excess "))
                    (print-message (format nil "phaser energy.~%")))
                  (print-message (format nil "~,2F units expended on empty space.~%" excess-energy))))))
        (progn
          ;; If manual fire was forced then explain why
          (when (eql fire-mode 'force-manual)
            (clear-type-ahead-buffer)
            (if (damagedp +computer+)
                (print-message (format nil "Battle computer damaged, manual fire only.~%"))
                (progn
                  (skip-line)
                  (print-message (format nil "---WORKING---~%") :print-slowly t)
                  (skip-line)
                  (print-message (format nil "Short-range-sensors-damaged~%"))
                  (print-message (format nil "Insufficient-data-for-automatic-phaser-fire~%"))
                  (print-message (format nil "Manual-fire-must-be-used~%"))
                  (skip-line))))
          ;; Allow manual fire even when the short-range sensors are damaged. If the short-range
          ;; sensors are damaged and the enemy is a Commander, Super-commander, or Romulan then
          ;; they can't be fired on unless they are adjacent to the ship.
          ;; TODO - write the enhancements to the visual-scan function and then allow firing on
          ;;        Commanders, Super-commanders, and Romulan) that are detected by the visual scan
          ;; TODO - Consider allowing manual fire only on enemies that are visible, either in the
          ;;        short range scan, adjacent sectors, or visual scan. When an unseen enemy
          ;;        attacks it's location becomes known (check the attack function to confirm this)
          ;;        and it then can be counter-attacked. Display it in the short-range scan with a
          ;;        generic letter such as "A" for "attacker" but not the specific type of enemy.
          ;; TODO - the recommended energy amounts for manual fire can vary if the input loop is
          ;;        repeated. Is this a bug or a feature?
          (setf requested-energy 0.0)
   (do ((current-enemy 0) ; the loop restarts with the first enemy if too much energy is requested
        (display-available-energy-p t)
        enemy
        enemy-coord
               (hits (make-array *enemies-here*))) ; C: hits, but sized to fit
       ((>= current-enemy *enemies-here*)
               (when (= requested-energy 0)
                 ;; zero energy -- abort
                 (return-from fire-phasers nil))
               (setf *ship-energy* (- *ship-energy* requested-energy))
               (clear-type-ahead-buffer)
               (when (and shield-control-available-p
                          *shields-are-up-p*)
                 (setf *ship-energy* (- *ship-energy* 200)) ; TODO - name the constant
                 (when (not (toggle-high-speed-shield-control requested-energy))
                   (return-from fire-phasers nil)))
               (apply-phaser-hits hits)
        (setf *action-taken-p* t))
     (setf enemy-coord (aref *klingon-sectors* current-enemy))
     (setf enemy (coord-ref *quadrant-contents* enemy-coord))
     (when display-available-energy-p
       (print-message (format nil "Energy available= ~,2F~%" (- available-energy 0.006))) ; what is this constant?
       (setf display-available-energy-p nil)
       (setf requested-energy 0.0))
     (if (and (damagedp +short-range-sensors+)
                     (or (string= enemy +super-commander+)
                         (string= enemy +commander+)
                         (string= enemy +romulan+))
                     ;; Check if the enemy is not adjacent to the ship
                     (not (and (< (abs (- (coordinate-x *ship-sector*) (coordinate-x enemy-coord))) 2)
                               (< (abs (- (coordinate-y *ship-sector*) (coordinate-y enemy-coord))) 2))))
                (progn
                  (print-message (format nil "~A can't be located without short range scan.~%" (letter-to-name enemy)))
                  (clear-type-ahead-buffer)
                  (setf (aref hits current-enemy) 0) ; prevent overflow -- thanks to Alexei Voitenko
                  (setf current-enemy (1+ current-enemy)))
                (progn
                  (when (= (length *line-tokens*) 0)
                    (print-prompt (format nil "(~A) units to fire at ~A at ~A: "
                                          (if targeting-support-available-p
                                              (format nil "~D" (truncate (recommended-energy-for-enemy current-enemy)))
                                              "??")
                                          (letter-to-name enemy)
                                          (format-sector-coordinates enemy-coord))))
                  (scan-input)
                  (when (and *input-item*
                             (not (numberp *input-item*)))
                    (huh)
                    (return-from fire-phasers nil))
                  (when (and (numberp *input-item*)
                             (< *input-item* 0))
                    ;; abort out
                    (clear-type-ahead-buffer)
                    (return-from fire-phasers nil))
                  (setf (aref hits current-enemy) *input-item*)
                  (setf requested-energy (+ requested-energy *input-item*))
                  (setf *input-item* nil)
                  ;; If total requested is too much, inform and start over
                  (if (> requested-energy available-energy)
                      (progn
                        (print-message (format nil "Available energy exceeded -- try again.~%"))
                        (clear-type-ahead-buffer)
                        (setf current-enemy 0)
                        (setf display-available-energy-p t))
                      (progn
                        (setf current-enemy (1+ current-enemy)))))))))
    ;; Say shield raised or malfunction, if necessary
    (when (not *all-done-p*)
      (when (and shield-control-available-p
                 (not *shields-are-up-p*))
        (when raise-shields-p
          (toggle-high-speed-shield-control requested-energy))) ; requested energy not important when raising shields
      (check-for-phasers-overheating requested-energy))))

;; This copyright notice applies to the `capture' and `selectKlingon' functions:
;;	$NetBSD: capture.c,v 1.6 2003/08/07 09:37:50 agc Exp $
;;
;; Copyright (c) 1980, 1993
;;	The Regents of the University of California.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the University nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

(defun capture () ; C: void capture(void)
  "Ask a Klingon To Surrender

(Fat chance)

The Subspace Radio is needed to ask a Klingon if he will kindly
surrender.  A random Klingon from the ones in the quadrant is
chosen.

The Klingon is requested to surrender.  The probability of this
is a function of that Klingon's remaining power, our power, etc."

  (setf *action-taken-p* nil) ; Nothing if we fail
  (setf *time-taken-by-current-operation* 0.0)

  (cond
    ;; Make sure there is room in the brig
    ((= *brig-free* 0)
     (print-message (format nil "Security reports the brig is already full.~%"))
     (return-from capture nil))

    ((damagedp +subspace-radio+)
     (print-message (format nil "Uhura- \"We have no subspace radio communication, sir.\"~%"))
     (return-from capture nil))

    ((damagedp +transporter+)
     (print-message (format nil "Scotty- \"Transporter damaged, sir.\"~%"))
     (return-from capture nil))
    ;; find out if there are any at all
    ((< *klingons-here* 1)
     (print-message (format nil "Uhura- \"Getting no response, sir.\"~%"))
     (return-from capture nil))
    (t
     (setf *time-taken-by-current-operation* 0.05) ; This action will take some time
     (setf *action-taken-p* t) ; So any others can strike back
     ;; If there is more than one Klingon, find out which one
     (let ((klingon-index (select-klingon-for-capture)) ; C: k
           k-coord
           x) ; C: x
       ;; Check out that Klingon
       (setf k-coord (aref *klingon-sectors* klingon-index))
       ;; The algorithm isn't that great and could use some more intelligent design
       ;; (setf x (+ 300 (* 25 (skill-level-value *skill-level*)))) - just use ship energy for now
       ;; Multiplier would originally have been equivalent of 1.4, but we want the command to work more often, more humanely
       (setf x (* (/ *ship-energy* (* (aref *klingon-energy* klingon-index) *enemies-here*)) 2.5))
       (if (<= x (* (random 1.0) 100))
           ;; Big surprise, he refuses to surrender
           (progn
             (print-message (format nil "Fat chance, captain~%"))
             (return-from capture nil)))
       ;; Guess what, he surrendered!!!
           (progn
             (print-message (format nil "Klingon captain at ~A surrenders~%" (format-coordinates k-coord)))
             ;; Assume a crew of 200 on the Klingon ship
             (let ((klingons-captured (round (* 200 (random 1.0))))) ; C: i
               (when (> klingons-captured 0)
                 (print-message (format nil "~D Klingons commit suicide rather than be taken captive.~%"
                                        (- 200 klingons-captured))))
               (when (and (> klingons-captured *brig-free*)
                          (not *dockedp*))
                 (print-message (format nil "~D Klingons die because there is no room for them in the brig~%"
                                        (- klingons-captured *brig-free*)))
                 (setf klingons-captured *brig-free*))
               (if *dockedp*
                   (progn
                     (setf *captured-klingons* (+ *captured-klingons* klingons-captured))
                     (print-message (format nil "~D captives taken and transferred to base~%"
                                            klingons-captured)))
                   (progn
                     (setf *brig-free* (- *brig-free* klingons-captured))
                     (print-message (format nil "~D captives taken~%" klingons-captured)))))
             (dead-enemy k-coord (coord-ref *quadrant-contents* k-coord) k-coord)
             (when (= *remaining-klingons* 0)
               (finish +won+)))))))

(defun select-klingon-for-capture () ; C: int selectklingon()
  "Cruddy, just takes one at random.  Should ask the captain.
Nah, just select the weakest one since it is most likely to
surrender (Tom Almy mod)"

  (let ((klingon-index 0)
        (klingon-energy (aref *klingon-energy* 0)))
    ;; Select the weakest one
    (do ((i 1 (1+ i))) ; C: int j
        ((>= i *enemies-here*))
      (unless (string= (coord-ref *quadrant-contents* (aref *klingon-sectors* i)) +romulan+) ; No Romulans surrender
        (when (> (aref *klingon-energy* i) klingon-energy)
          (setf klingon-index i)
          (setf klingon-energy (aref *klingon-energy* i)))))
    (return-from select-klingon-for-capture klingon-index)))

(defun displace-ship (x-coord y-coord hit-angle)
  "When a torpedo hits a ship at position x-coord, y-coord at angle hit-angle, displace the ship
and return the x and y coordinates to which it was displaced."

  ;; It's the movement code again, in a compact form
  (let (angle ; C: ang
        bigger ; C: temp
        delta-x ; C: xx
        delta-y) ; C: yy
    (setf angle (+ hit-angle (* 2.5 (- (random 1.0) 0.5))))
    (setf bigger (abs (sin angle)))
    (when (> (abs (cos angle)) bigger)
      (setf bigger (abs (cos angle))))
    (setf delta-x (- (/ (sin angle) bigger)))
    (setf delta-y (/ (cos angle) bigger))
    (return-from displace-ship (make-coordinate :x (round (+ x-coord delta-x)) :y (round (+ y-coord delta-y))))))

(defun calculate-torpedo-damage (torpedo-origin torpedo-coord hit-angle)
  "Damage done by a torpedo depends on the distance it has travelled and the angle at which it hits."

  (abs (- (+ 700.0 (* 100.0 (random 1.0)))
          (* 1000.0
             (distance torpedo-origin torpedo-coord)
             (abs (sin hit-angle))))))

(defun track-torpedo (torp-x ; floating point x position
                      torp-y ; floating point y position
                      length-of-track
                      torpedo-number
                      number-of-torpedoes-in-salvo
                      sector-contents) ; C: void tracktorpedo(coord w, int l, int i, int n, int iquad)
  "Torpedo-track animation. If displaying coordinates then display fractional amounts since it is
allowed to aim between sectors."

  (if *window-interface-p*
      ;; TODO - write the curses part
      (if (or (not (damagedp +short-range-sensors+))
              *dockedp*)
          (progn
            (setf sector-contents sector-contents) ; never used? hah!
            t) ; TODO - finish this
          (print-out (format nil "~,1F - ~,1F  " (1+ torp-x) (1+ torp-y)))
        )
      (progn
        (if (= length-of-track 1)
            ;; Display track header
            (progn
              (skip-line)
              (if (= number-of-torpedoes-in-salvo 1)
                  (print-out "Torpedo track: ")
                  (print-out (format nil "Track for torpedo number ~A:  " torpedo-number))))
            ;; Line wrap every four sectors
            (when (or (= length-of-track 4)
                      (= length-of-track 9))
              (skip-line)))
        ;; Convert internal torp coords to 1-based player values
        (print-out (format nil "~,1F - ~,1F  " (1+ torp-x) (1+ torp-y))))))

;; TODO - there is a parallel with move-ship-within-quadrant. Is there common code that
;;        be refactored into a general-purpose routine?
;; TODO - displacing an enemy at the edge of the quadrant to an invalid sector produces an error. Seen with space thing.
(defun move-torpedo-within-quadrant (course ; direction in which to fire the torpedo
                                     random-variation ; randomness that affects the torpedo flight
                                     initial-position ; starting position of the torpedo
                                     torpedo-number ; which torpedo out of the total number fired
                                     number-of-torpedoes-in-salvo) ; C: void torpedo(double course, double r, coord in, double *hit, int i, int n)
  "Let a photon torpedo fly. Move one torpedo inside the quadrant, possibly hitting objects, and
handling the result. Return the amount of damage if the player ship was hit."

  (if (or (not (damagedp +short-range-sensors+))
          *dockedp*)
      (select-window *short-range-scan-window*)
      (select-window *message-window*))
  (let (adjusted-course ; C: double ac
        angle ; C: double angle
        bullseye-angle ; C: double bullseye
        delta-x ; C: double deltax
        delta-y ; C: double deltay
        (ship-hit 0)) ; Amount of damage to do to the player ship, calculated here and returned to the caller.
    (setf adjusted-course (+ course (* 0.25 random-variation)))
    (setf angle (* (- 15.0 adjusted-course) 0.5235988))
    (setf bullseye-angle (* (- 15.0 course) 0.5235988))
    (setf delta-x (- (sin angle)))
    (setf delta-y (cos angle))
    (let (bigger) ; C: double bigger
      (if (> (abs delta-x) (abs delta-y))
          (setf bigger (abs delta-x))
          (setf bigger (abs delta-y)))
      (setf delta-x (/ delta-x bigger))
      (setf delta-y (/ delta-y bigger)))
    ;; Loop to move a single torpedo. Movement continues until the torpedo hits something or it
    ;; exits the quadrant.
    (do ((movement-ended-p nil)
         (torp-x (coordinate-x initial-position)) ; C: double x, the x position of the torpedo
         (torp-y (coordinate-y initial-position)) ; C: double y, the y position of the torpedo
         torpedo-sector ; C: coord w, the current position of the torpedo
         (movement-count 0) ; number of times the torpedo has moved
         sector-contents ; C: int iquad, convenience variable - contents of the sector the torpedo moves into
         displaced-to-sector ; C: coord jw, coordinate to which a ship is displaced
         (shovedp nil)) ; C: bool shoved, a ship was moved by a torpedo hit
        (movement-ended-p
         ;; Displaced enemies don't exit the sector
         (when (and shovedp
                    (valid-sector-p (coordinate-x displaced-to-sector) (coordinate-y displaced-to-sector)))
           (setf (coord-ref *quadrant-contents* torpedo-sector) +empty-sector+)
           (setf (coord-ref *quadrant-contents* displaced-to-sector) sector-contents)
           ;; Thing is displaced without notification
           (unless (string= sector-contents +thing+)
             (print-message (format nil " displaced by blast to ~A ~%" (format-sector-coordinates displaced-to-sector))))
           (do ((i 0 (1+ i)))
               ((>= i *enemies-here*))
             (setf (aref *klingon-distance* i) (distance *ship-sector* (aref *klingon-sectors* i)))
             (setf (aref *klingon-average-distance* i) (distance *ship-sector* (aref *klingon-sectors* i)))
             (sort-klingons))))
      (setf torp-x (+ torp-x delta-x))
      (setf torp-y (+ torp-y delta-y))
      (setf torpedo-sector (make-coordinate :x (round torp-x) :y (round torp-y)))
      (setf movement-count (1+ movement-count))
      (if (valid-sector-p (coordinate-x torpedo-sector) (coordinate-y torpedo-sector))
          (progn
            (setf sector-contents (coord-ref *quadrant-contents* torpedo-sector))
            (track-torpedo torp-x torp-y movement-count torpedo-number number-of-torpedoes-in-salvo sector-contents)
            (unless (string= sector-contents +empty-sector+)
              ;; hit something
              (select-window *message-window*)
              (when (or (not *window-interface-p*)
                        (and *window-interface-p*
                             (damagedp +short-range-sensors+)
                             (not *dockedp*)))
                (skip-line)) ; start new line after text track
              (cond
                ;; Hit our ship
                ((or (string= sector-contents +enterprise+)
                     (string= sector-contents +faerie-queene+))
                 (skip-line)
                 (print-message (format nil "Torpedo hits ~A.~%" (format-ship-name)))
                 (setf ship-hit (calculate-torpedo-damage initial-position torpedo-sector bullseye-angle))
                 (setf *dockedp* nil) ; we're blown out of dock
                 ;; We may be displaced.
                 (unless *landedp* ; Cheat if on a planet
                   (setf displaced-to-sector (displace-ship (coordinate-x torpedo-sector)
                                                            (coordinate-y torpedo-sector)
                                                            angle))
                   (when (valid-sector-p (coordinate-x displaced-to-sector) (coordinate-y displaced-to-sector))
                     (when (string= (coord-ref *quadrant-contents* displaced-to-sector) +black-hole+)
                       (finish +destroyed-by-black-hole+)
                       (return-from move-torpedo-within-quadrant ship-hit))
                     ;; can't move into object
                     (when (string= (coord-ref *quadrant-contents* displaced-to-sector) +empty-sector+)
                       (setf *ship-sector* displaced-to-sector)
                       (print-message (format-ship-name))
                       (setf shovedp t)))))
                ;; Hit an enemy
                ((or (string= sector-contents +super-commander+)
                     (string= sector-contents +commander+)
                     (string= sector-contents +klingon+)
                     (string= sector-contents +romulan+))
                 (if (and (or (string= sector-contents +super-commander+) ; If it's a commander
                              (string= sector-contents +commander+))
                          (<= (random 1.0) 0.05)) ; and they successfully neutralize the torpedo
                     (progn
                       (print-message (format nil "***~A at ~A uses anti-photon device;~%"
                                              (letter-to-name sector-contents)
                                              (format-sector-coordinates torpedo-sector)))
                       (print-message (format nil "   torpedo neutralized.~%")))
                     ;; Hit a regular enemy
                     ;; TODO - during testing a torpedo destroyed a klingon but the Klingon was not removed
                     ;;        from the SR scan. The SR scan showed a K but the enemy
                     ;;        was treated as Unknown.
                     (do ((enemy-index 0 (1+ enemy-index)) ; find the enemy
                          enemy-energy
                          enemy-hit)
                         ((coord-equal torpedo-sector (aref *klingon-sectors* enemy-index))
                          (setf enemy-energy (abs (aref *klingon-energy* enemy-index)))
                          (setf enemy-hit (calculate-torpedo-damage initial-position torpedo-sector bullseye-angle))
                          (when (< enemy-energy enemy-hit)
                            (setf enemy-hit enemy-energy))
                          (setf (aref *klingon-energy* enemy-index) (- (aref *klingon-energy* enemy-index)
                                                                       (if (< (aref *klingon-energy* enemy-index) 0)
                                                                           (- enemy-hit)
                                                                           enemy-hit)))
                          (if (= (aref *klingon-energy* enemy-index) 0)
                              (dead-enemy torpedo-sector sector-contents torpedo-sector)
                              ;; If enemy damaged but not destroyed, try to displace
                              (progn
                                (print-message (format nil "***~A at ~A" (letter-to-name sector-contents)
                                                       (format-sector-coordinates torpedo-sector)))
                                (setf displaced-to-sector (displace-ship (coordinate-x torpedo-sector)
                                                                         (coordinate-y torpedo-sector)
                                                                         angle))
                                (cond
                                  ((or (not (valid-sector-p (coordinate-x displaced-to-sector)
                                                            (coordinate-y displaced-to-sector)))
                                       ;; can't move into object
                                       (string/= (coord-ref *quadrant-contents* displaced-to-sector) +empty-sector+))
                                   (print-message (format nil " damaged but not destroyed.~%")))

                                  ((string= (coord-ref *quadrant-contents* displaced-to-sector) +black-hole+)
                                   (print-message (format nil " buffeted into black hole.~%"))
                                   (dead-enemy torpedo-sector sector-contents torpedo-sector))

                                  (t
                                   (print-out " damaged--")
                                   (setf (aref *klingon-sectors* enemy-index) displaced-to-sector)
                                   (setf shovedp t)))))))))
                ;; Hit a base
                ((string= sector-contents +starbase+)
                 (skip-line)
                 (print-message (format nil "***STARBASE DESTROYED..~%"))
                 ;; Remove the base from the list of bases and related data structures
                 (setf *base-quadrants* (remove *ship-quadrant* *base-quadrants* :test #'coord-equal))
                 (setf *base-sector* nil)
                 (setf (coord-ref *quadrant-contents* torpedo-sector) +empty-sector+)
                 (setf (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*))
                       (1- (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*))))
                 (setf (starchart-page-starbases (coord-ref *starchart* *ship-quadrant*))
                       (1- (starchart-page-starbases (coord-ref *starchart* *ship-quadrant*))))
                 (setf *destroyed-bases* (1+ *destroyed-bases*))
                 (setf *dockedp* nil))
                ;; Hit a planet
                ((string= sector-contents +planet+)
                 (print-message (format nil "***~A at ~A destroyed.~%" (letter-to-name sector-contents)
                                        (format-sector-coordinates torpedo-sector)))
                 (setf *destroyed-uninhabited-planets* (1+ *destroyed-uninhabited-planets*))
                 (let ((p (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
                   (setf (planet-destroyedp p) t)
                   (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) p))
                 (setf *planet-coord* nil)
                 (setf *current-planet* nil)
                 (setf (coord-ref *quadrant-contents* torpedo-sector) +empty-sector+)
                 (when *landedp* ; captain perishes on planet
                   (finish +destroyed-planet+)))
                ;; Hit an inhabited world -- very bad!
                ((string= sector-contents +world+)
                 (print-message (format nil "***~A at ~A destroyed.~%" (letter-to-name sector-contents)
                                        (format-sector-coordinates torpedo-sector)))
                 (setf *destroyed-inhabited-planets* (1+ *destroyed-inhabited-planets*))
                 (let ((p (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
                   (setf (planet-destroyedp p) t)
                   (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) p))
                 (setf *planet-coord* nil)
                 (setf *current-planet* nil)
                 (setf (coord-ref *quadrant-contents* torpedo-sector) +empty-sector+)
                 (when *landedp* ; captain perishes on planet
                   (finish +destroyed-planet+))
                 (print-message (format nil "You have just destroyed an inhabited planet.~%"))
                 (print-message (format nil "Celebratory rallies are being held on the Klingon homeworld.~%")))
                ;; Hit a star
                ((string= sector-contents +star+)
                 (if (> (random 1.0) 0.10)
                     (nova torpedo-sector)
                     (print-message (format nil "***~A at ~A unaffected by photon blast.~%"
                                            (letter-to-name sector-contents)
                                            (format-sector-coordinates torpedo-sector)))))
                ;; Hit a thingy
                ((string= sector-contents +thing+)
                 (if (> (random 1.0) 0.7)
                     (progn
                       (skip-line)
                       (print-message
                        (format nil "AAAAIIIIEEEEEEEEAAAAAAAAUUUUUGGGGGHHHHHHHHHHHH!!!~%")
                        :print-slowly t)
                       (skip-line)
                       (print-message
                        (format nil "    HACK!     HACK!    HACK!        *CHOKE!*  ~%")
                        :print-slowly t)
                       (skip-line)
                       (print-message "Mr. Spock-")
                       (print-message (format nil "  \"Fascinating!\"~%") :print-slowly t)
                       (skip-line)
                       (dead-enemy torpedo-sector sector-contents torpedo-sector))
                     ;; Stas Sergeev added the possibility that you can shove the Thingy and
                     ;; piss it off. It then becomes an enemy and may fire at you.
                     (progn
                       (setf *thing-is-angry-p* t)
                       (setf displaced-to-sector (displace-ship (coordinate-x torpedo-sector)
                                                                (coordinate-y torpedo-sector)
                                                                angle))
                       (when (valid-sector-p (coordinate-x displaced-to-sector) (coordinate-y displaced-to-sector))
                         (cond
                           ;; Thing vanishes silently into black hole, very mysterious
                           ((string= (coord-ref *quadrant-contents* displaced-to-sector) +black-hole+)
                            (setf (coord-ref *quadrant-contents* torpedo-sector) +empty-sector+)
                            (setf *thing-is-angry-p* nil)
                            (setf shovedp nil))
                           ;; can't move into object
                           ((string/= (coord-ref *quadrant-contents* displaced-to-sector) +empty-sector+)
                            (setf shovedp nil))
                           (t
                            (setf shovedp t)))))))
                ;; Black hole
                ((string= sector-contents +black-hole+)
                 (skip-line)
                 (print-message (format nil "***~A at ~A swallows torpedo.~%"
                                        (letter-to-name sector-contents)
                                        (format-sector-coordinates torpedo-sector))))
                ;; hit the web
                ((string= sector-contents +tholian-web+)
                 (skip-line)
                 (print-message (format nil "***Torpedo absorbed by Tholian web.~%")))
                ;; Hit a Tholian
                ((string= sector-contents +tholian+)
                 (cond
                   ;; Tholian is destroyed
                   ((>= (calculate-torpedo-damage initial-position torpedo-sector bullseye-angle) 600)
                    (setf (coord-ref *quadrant-contents* torpedo-sector)
                          +empty-sector+)
                    (setf *tholians-here* 0)
                    (dead-enemy torpedo-sector sector-contents torpedo-sector))
                   ;; Tholian survives
                   ((> (random 1.0) 0.05)
                    (skip-line)
                    (print-message (format nil "***~A at ~A survives photon blast.~%"
                                           (letter-to-name sector-contents)
                                           (format-sector-coordinates torpedo-sector))))
                   ;; Tholian vanishes, leaving behind a black hole.
                   (t
                    (skip-line)
                    (print-message (format nil "***~A at ~A disappears.~%"
                                           (letter-to-name sector-contents)
                                           (format-sector-coordinates torpedo-sector)))
                    (setf (coord-ref *quadrant-contents* torpedo-sector)
                          +tholian-web+)
                    (setf *tholians-here* 0)
                    (setf *enemies-here* (1- *enemies-here*))
                    (drop-entity-in-sector +black-hole+))))
                ;; Problem!
                (t
                 (skip-line)
                 (print-message (format nil "***Torpedo hits unknown object ~A at ~A~%" sector-contents
                                        (format-sector-coordinates torpedo-sector)))))
              (setf movement-ended-p t)))
          ;; The torpedo exited the quadrant without hitting anything
          (progn
            (setf movement-ended-p t)
            (skip-line)
            (print-message (format nil "Torpedo missed.~%")))))
    (return-from move-torpedo-within-quadrant ship-hit)))

(defun photon-torpedo-target-check (target-coord) ; C: bool targetcheck(double x, double y, double *course)
  "Verfiy that the parameter is an acceptable target for a photon torpedo. Return the course
direction of the target or nil."

  (when (not (valid-sector-p (coordinate-x target-coord) (coordinate-y target-coord)))
    (huh)
    (return-from photon-torpedo-target-check nil))

  ;; Multiply by 0.1 to scale to qadrant-sized units
  (let ((delta-x (* 0.1 (- (coordinate-y target-coord) (coordinate-y *ship-sector*))))
        (delta-y (* 0.1 (- (coordinate-x *ship-sector*) (coordinate-x target-coord)))))
    ;; When both are zero the player targeted their own sector. Conveniently, values of zero
    ;; aren't valid for atan2.
    (when (and (= delta-x 0)
               (= delta-y 0))
      (skip-line)
      (print-message (format nil "Spock-  \"Bridge to sickbay.  Dr. McCoy,~%"))
      (print-message (format nil "  I recommend an immediate review of~%"))
      (print-message (format nil "  the Captain's psychological profile.\"~%"))
      (skip-line)
      (return-from photon-torpedo-target-check nil))

    (return-from photon-torpedo-target-check (* 1.90985932 (atan delta-x delta-y)))))

(defun get-number-of-torpedoes-to-fire ()
  "Up to three torpedoes can be fired in one salvo. Return a number between 1 and 3 or nil to
cancel."

  (do ()
      (nil)
    ;; Get number of torpedoes to fire
    (when (= (length *line-tokens*) 0)
      (print-message (format nil "~D torpedoes left.~%" *torpedoes*))
      (print-prompt "Number of torpedoes to fire: "))
    (scan-input)
    (cond
      ((numberp *input-item*)
       (cond
         ;; abort command
         ((<= *input-item* 0)
          (return-from get-number-of-torpedoes-to-fire nil))

         ((> *input-item* 3)
          (print-message (format nil "Maximum of 3 torpedoes per burst.~%"))
          (clear-type-ahead-buffer))

         ((<= *input-item* *torpedoes*)
          (return-from get-number-of-torpedoes-to-fire *input-item*))
         ;; less than 4 but more than the available torpedoes
         (t
          (clear-type-ahead-buffer))))
      ;; Not a number and not nil, must be alpha
      (*input-item*
       (huh)
       (return-from get-number-of-torpedoes-to-fire nil))

      (t
       (clear-type-ahead-buffer)))))

(defun get-targets-for-torpedoes (number-of-torpedoes-to-fire)
  "Given a number of torpedoes to fire, get the targets for each torpedo. Possible targets are:
prompt for each one, all torpedoes at one target, and read the coordinates already entered by
the player on the command line. Return an array of courses, one course per torpedo, or nil if
there was an error (including -1 entered by the player to exit the command)."

  (let (target-input-method)
    ;; Get targets for torpedoes
    (cond
      ;; We will try prompting
      ((= (length *line-tokens*) 0)
       (setf target-input-method 'prompt))
      ;; All torpedoes at one target
      ((= (length *line-tokens*) 2)
       (setf target-input-method 'one-target))
      ;; too few coordinates for number of torpedoes
      ((< (* number-of-torpedoes-to-fire 2)
          (length *line-tokens*))
       (huh)
       (return-from get-targets-for-torpedoes nil))
      ;; the coordinates for each torpedo are on the input line, just read them
      (t
       (setf target-input-method 'read-input)))

    (do ((torpedo 0 (1+ torpedo))
         (target (make-coordinate))
         ;; TODO - try it as a list
         (courses (make-array number-of-torpedoes-to-fire))) ; array of directions in which to fire each torpedo
        ((>= torpedo number-of-torpedoes-to-fire)
         (return-from get-targets-for-torpedoes courses))
      (when (eql target-input-method 'prompt) ; Prompt for each one
        (print-prompt (format nil "Target sector for torpedo number ~A: " (1+ torpedo))))
      (if (and (eql target-input-method 'one-target)
               (> torpedo 0))
          (setf (aref courses torpedo) (aref courses 0))
          (progn
            (scan-input)
            (setf (coordinate-x target) (read-coordinate-number))
            (scan-input)
            (setf (coordinate-y target) (read-coordinate-number))
            (setf (aref courses torpedo) (photon-torpedo-target-check target))
            (when (not (aref courses torpedo))
              ;; If the target check returned nil then just return, player was already notified
              (return-from get-targets-for-torpedoes nil)))))))

(defun fire-photon-torpedoes () ; C: torps()
  "Launch photon torpedo salvo."

  (setf *action-taken-p* nil)

  (when (damagedp +photon-torpedoes+)
    (print-message (format nil "Photon tubes damaged.~%"))
    (return-from fire-photon-torpedoes nil))

  (when (= *torpedoes* 0)
    (print-message (format nil "No torpedoes left.~%"))
    (return-from fire-photon-torpedoes nil))

  (let ((number-of-torpedoes-to-fire (get-number-of-torpedoes-to-fire))
        courses)
    (when (> number-of-torpedoes-to-fire 0)
      (setf courses (get-targets-for-torpedoes number-of-torpedoes-to-fire))
      (when courses
        (setf *action-taken-p* t)
        ;; Loop for moving <n> torpedoes
        (do ((i 0 (1+ i))
             (misfire nil)
             random-variation) ; decides torpedo misfires and shield deflection
            ((or misfire
                (>= i number-of-torpedoes-to-fire)))
          (when (not *dockedp*)
            (setf *torpedoes* (1- *torpedoes*)))
          (setf random-variation (- (* (+ (random 1.0) (random 1.0)) 0.5) 0.5))
          (if (>= (abs random-variation) 0.47)
              (progn
                ;; misfire!
                (setf random-variation (* (+ (random 1.0) 1.2) random-variation)) ; unused calculation!
                (if (> number-of-torpedoes-to-fire 1)
                    (print-message (format nil "***TORPEDO NUMBER ~A MISFIRES" i) :print-slowly t)
                    (print-message "***TORPEDO MISFIRES." :print-slowly t))
                (skip-line)
                (when (< (1+ i) number-of-torpedoes-to-fire)
                  (print-message (format nil "  Remainder of burst aborted.~%")))
                (when (<= (random 1.0) 0.2)
                  (print-message (format nil "***Photon tubes damaged by misfire.~%"))
                  (setf (aref *device-damage* +photon-torpedoes+) (* *damage-factor* (+ 1.0 (* 2.0 (random 1.0))))))
                (setf misfire t))
              (progn
                ;; Fire a photon torpedo
                (cond ; Torpedoes are less accurate in several cicrcumstances
                  (*cloakedp*
                   (setf random-variation (* random-variation 1.2)))

                  ((or *shields-are-up-p*
                       *dockedp*)
                   (setf random-variation (* random-variation (+ 1.0 (* 0.0001 *shield-energy*))))))
                (move-torpedo-within-quadrant (aref courses i) random-variation *ship-sector* i number-of-torpedoes-to-fire)
                (when (or *all-done-p*
                          (quadrant-supernovap
                           (coord-ref *galaxy* *ship-quadrant*)))
                  (return-from fire-photon-torpedoes nil))))))
      ;; TODO - is this a common idiom suitable for a function? Yes - (enemies-remaining-p)
      (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
        (finish +won+)))))

(defun cloak () ; C: void cloak(void)

  (when (string= *ship* +faerie-queene+)
    (print-message (format nil "Ye Faerie Queene has no cloaking device.~%"))
    (return-from cloak nil))

  (let ((action 'none))
    (when (> (length *line-tokens*) 0)
      (scan-input))
    (when (numberp *input-item*)
      (return-from cloak nil))
    ;; TODO - what is *input-item* at this point?
    (if *input-item* ; is not nil
        (let ((token (match-token *input-item* (list "on" "off"))))
          (cond
            ((string= token "on")
             (when *cloakedp*
               (print-message (format nil "The cloaking device has already been switched on.~%"))
               (return-from cloak nil))
             (setf action 'turn-cloaking-on))

            ((string= token "off")
             (when (not *cloakedp*)
               (print-message (format nil "The cloaking device has already been switched off.~%"))
               (return-from cloak nil))
             (setf action 'turn-cloaking-off))

            (t
             (huh)
             (return-from cloak nil))))
        (progn
          (when (not *cloakedp*)
            (print-prompt "Switch cloaking device on? ")
            (unless (get-y-or-n-p)
              (return-from cloak nil))
            (setf action 'turn-cloaking-on))
          (when *cloakedp*
            (print-prompt "Switch cloaking device off? ")
            (unless (get-y-or-n-p)
              (return-from cloak nil))
            (setf action 'turn-cloaking-off))
          (when (eql action 'none)
            (return-from cloak nil))))
    (when (eql action 'turn-cloaking-off)
      (when (and (> *romulans-here* 0)
                 (>= *stardate* +algeron-date+)
                 (not *cloaking-violation-reported-p*))
        (print-prompt (format nil "Spock- \"Captain, the Treaty of Algeron is in effect.~%   Are you sure this is wise?\""))
        (unless (get-y-or-n-p)
          (return-from cloak nil)))
      (print-message (format nil "Engineer Scott- \"Aye, Sir.\"~%"))
      (setf *cloakedp* nil)
      ;; The Romulans detect you uncloaking
      (when (and (> *romulans-here* 0)
                 (>= *stardate* +algeron-date+)
                 (not *cloaking-violation-reported-p*))
        (print-message (format nil "The Romulan ship discovers you are breaking the Treaty of Algeron!~%"))
        (setf *cloaking-violations* (1+ *cloaking-violations*))
        (setf *cloaking-violation-reported-p* t))
      (return-from cloak nil))
    ;; turn cloaking on
    (when (damagedp +cloaking-device+)
      (print-message (format nil "Engineer Scott- \"The cloaking device is damaged, Sir.\"~%"))
      (return-from cloak nil))
    (when *dockedp*
      (print-message (format nil "You cannot cloak while docked.~%"))
      (return-from cloak nil))
    (when (and (>= *stardate* +algeron-date+)
               (not *cloaking-violation-reported-p*))
      (print-message (format nil "Spock- \"Captain, using the cloaking device is a violation~%"))
      (print-message (format nil "  of the Treaty of Algeron. Considering the alternatives,~%"))
      (print-prompt "  are you sure this is wise?")
      (unless (get-y-or-n-p)
        (return-from cloak nil)))
    (print-message (format nil "Engineer Scott- \"The cloaking device has been engaged, Sir.\"~%"))
    (setf *cloakedp* t)
    (check-treaty-of-algeron)))

(defun shield-actions (&key (raise-shields nil)) ; C: doshield(bool raise)
  "Change shield status. The optional parameter is used to raise the shields without player
input when a tractor beam event occurs."

  (setf *action-taken-p* nil)
  (let ((action 'none))
    (if raise-shields
        (setf action 'raise)
        (progn
          (when *line-tokens*
            (scan-input))
          (let ((token (match-token *input-item* (list "transfer" "up" "down"))))
            (if (string= token "transfer")
                (setf action 'energy-transfer)
                (cond
                  ((damagedp +shields+)
                   (print-message (format nil "Shields damaged and down.~%"))
                   (return-from shield-actions nil))

                  ((string= token "up")
                   (setf action 'raise))

                  ((string= token "down")
                   (setf action 'lower)))))
            (when (eql action 'none)
            (print-prompt "Do you wish to change shield energy? ")
            (cond
              ((get-y-or-n-p)
               (setf action 'energy-transfer))

              ((damagedp +shields+)
               (print-message (format nil "Shields damaged and down.~%"))
               (return-from shield-actions nil))

              (*shields-are-up-p*
               (print-prompt "Shields are up. Do you want them down? ")
               (if (get-y-or-n-p)
                   (setf action 'lower)
                   (return-from shield-actions nil)))

              (t
               (print-prompt "Shields are down. Do you want them up? ")
               (if (get-y-or-n-p)
                   (setf action 'raise)
                   (return-from shield-actions nil)))))))
    (cond
      ;; raise shields
      ((eql action 'raise)
       (when *shields-are-up-p*
         (print-message (format nil "Shields already up.~%"))
         (return-from shield-actions nil))
       (setf *shields-are-up-p* t)
       (setf *shields-are-changing-p* t)
       (when (not *dockedp*)
         (setf *ship-energy* (- *ship-energy* 50.0)))
       (print-message (format nil "Shields raised.~%"))
       (when (<= *ship-energy* 0)
         (skip-line)
         (print-message (format nil "Shields raising uses up last of energy.~%"))
         (finish +out-of-energy+)
         (return-from shield-actions nil))
       (setf *action-taken-p* t))

      ((eql action 'lower)
       (when (not *shields-are-up-p*)
         (print-message (format nil "Shields already down.~%"))
         (return-from shield-actions nil))
       (setf *shields-are-up-p* nil)
       (setf *shields-are-changing-p* t)
       (print-message (format nil "Shields lowered.~%"))
       (setf *action-taken-p* t))

      ((eql action 'energy-transfer)
       (when *line-tokens*
         (scan-input))
       (do ()
           ((numberp *input-item*))
         (clear-type-ahead-buffer)
         (print-prompt "Energy to transfer to shields: ")
         (scan-input))
       (when (= *input-item* 0)
         (return-from shield-actions nil))
       (when (> *input-item* *ship-energy*)
         (print-message (format nil "Insufficient ship energy.~%"))
         (return-from shield-actions nil))
       (setf *action-taken-p* t)
       (when (>= (+ *shield-energy* *input-item*) *initial-shield-energy*)
         (print-message (format nil "Shield energy maximized.~%"))
         (when (> (+ *shield-energy* *input-item*) *initial-shield-energy*)
           (print-message (format nil "Excess energy requested returned to ship energy~%")))
         (setf *ship-energy* (- *ship-energy* (- *initial-shield-energy* *shield-energy*)))
         (setf *shield-energy* *initial-shield-energy*)
         (return-from shield-actions nil))
       ;; Prevent shield drain loophole
       (when (and (< *input-item* 0.0)
                  (> (- *ship-energy* *input-item*) *initial-energy*))
        (skip-line)
        (print-message (format nil "Engineering to bridge--~%"))
        (print-message (format nil "  \"Scott here. Power circuit problem, Captain.\"~%"))
        (print-message (format nil "  \"I can't drain the shields.\"~%"))
        (setf *action-taken-p* nil)
        (return-from shield-actions nil))

       (when (< (+ *shield-energy* *input-item*) 0)
         (print-message (format nil"All shield energy transferred to ship.~%"))
         (setf *ship-energy* (+ *ship-energy* *shield-energy*))
         (setf *shield-energy* 0.0)
         (return-from shield-actions nil))

       ;; this stanza needs a return-from if there is ever code added after it
       (print-message (format nil "Scotty- ~%"))
       (if (> *input-item* 0)
           (print-message (format nil "\"Transferring energy to shields.\"~%"))
           (print-message (format nil "\"Draining energy from shields.\"~%")))
       (setf *shield-energy* (+ *shield-energy* *input-item*))
       (setf *ship-energy* (- *ship-energy* *input-item*))))))

(defun ram (&key rammed-by-p enemy enemy-coordinates) ; C: void ram(bool ibumpd, feature ienm, coord w)
  "Make our ship ram something. If rammed-by-p is true then an enemy ship is ramming the player.
enemy is the single-letter symbol of the enemy ramming/being rammed. enemy-coordinates are the
coordinates of the enemy being rammed or the original coordinates of ramming enemy."

  (skip-line)
  (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
  (skip-line)
  (print-message (format nil "***COLLISION IMMINENT.~%"))
  (skip-line)
  (print-message (format nil "***~A ~A ~A at ~A~A.~%"
                         (format-ship-name)
                         (if rammed-by-p "rammed by" "rams")
                         (letter-to-name enemy)
                         (format-coordinates enemy-coordinates)
                         (if rammed-by-p " (original position)" "")))
  (skip-line)
  (dead-enemy enemy-coordinates enemy *ship-sector*)
  (skip-line)
  (setf *shields-are-up-p* nil)
  (print-message (format nil "***Shields are down.~%"))
  (let ((number-of-casualties (truncate(+ 10.0 (* 20.0 (random 1.0))))))
    (print-message (format nil "***Sickbay reports ~A casualties~%" number-of-casualties))
    (setf *casualties* (+ *casualties* number-of-casualties))
    (setf *crew* (- *crew* number-of-casualties))) ; TODO - should game end if crew count is too low?
  ;; In the pre-SST2K version, all devices got equiprobably damaged, which was silly. Instead, pick
  ;; up to half the devices at random according to our weighting table,
  (let ((devices-to-damage (truncate (* (random 1.0) (/ +number-of-devices+ 2)))))
    (do ((m 0 (1+ m))
         dev-index)
        ((>= m devices-to-damage))
      (setf dev-index (get-random-device))
      (when (>= (aref *device-damage* dev-index) 0)
        ;; Damage for at least time of travel!
        (setf (aref *device-damage* dev-index)
              (+ (aref *device-damage* dev-index)
                 *time-taken-by-current-operation*
                 (* (1+ (* 10.0 (get-enemy-hardness enemy) (random 1.0))) *damage-factor*))))))
  (print-message (format nil "***~A heavily damaged.~%" (format-ship-name)))
  (if (> (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
      (damage-report)
      (finish +won+)))

(defun get-random-device () ; C: int randdevice(void)
  "Choose a device to damage, at random.

Quoth Eric Allman in the code of BSD-Trek:
Under certain conditions you can get a critical hit.  This
sort of hit damages devices.  The probability that a given
device is damaged depends on the device.  Well protected
devices (such as the computer, which is in the core of the
ship and has considerable redundancy) almost never get
damaged, whereas devices which are exposed (such as the
warp engines) or which are particularly delicate (such as
the transporter) have a much higher probability of being
damaged.

This is one place where OPTION_PLAIN does not restore the
original behavior, which was equiprobable damage across
all devices.  If we wanted that, we'd return NDEVICES*Rand()
and have done with it.  Also, in the original game, DNAVYS
and DCOMPTR were the same device.

Instead, we use a table of weights similar to the one from BSD Trek.
BSD doesn't have the shuttle, shield controller, death ray, or probes.
We don't have a cloaking device.  The shuttle got the allocation
for the cloaking device, then we shaved a half-percent off
everything to have some weight to give DSHCTRL/DDRAY/DDSP."

  ;; TODO - add cloaking device!
  ;; TODO - the Faerie Queene has fewer devices than the Enterprise, should this table still apply?
  (do ((weights (list
                 105  ; DSRSENS: short range scanners	      10.5%
                 105  ; DLRSENS: long range scanners	      10.5%
                 120  ; DPHASER: phasers		      12.0%
                 120  ; DPHOTON: photon torpedoes	      12.0%
                 25   ; DLIFSUP: life support		       2.5%
                 65   ; DWARPEN: warp drive		       6.5%
                 70   ; DIMPULS: impulse engines	       6.5%
                 145  ; DSHIELD: deflector shields	      14.5%
                 30   ; DRADIO:  subspace radio		       3.0%
                 45   ; DSHUTTL: shuttle		       4.5%
                 15   ; DCOMPTR: computer		       1.5%
                 20   ; NAVCOMP: navigation system	       2.0%
                 75   ; DTRANSP: transporter		       7.5%
                 20   ; DSHCTRL: high-speed shield controller  2.0%
                 10   ; DDRAY:   death ray		       1.0%
                 30)) ; DDSP:    deep-space probes	       3.0%
       (random-index (random 1000)) ; weights must sum to 1000
       (i 0 (1+ i))
       (sum 0))
      ((>= i +number-of-devices+))
    (setf sum (+ sum (pop weights)))
    (when (< random-index sum)
      (return-from get-random-device i)))

  ;; Fallback is equiprobable selection of a device
  (return-from get-random-device (random +number-of-devices+)))

(defun get-enemy-hardness (enemy)
  "For a given enemy symbol return a number indicating how \"hard\" it is - essentially a
damage multiplier to determine how much damage is done by ramming this enemy."

  ;; TODO - could/should this be an alist or some other lookup?
  ;; TODO - could/should this be a property of an object?
  (cond
    ((string= enemy +romulan+)
     1.5)
    ((string= enemy +commander+)
     2.0)
    ((string= enemy +super-commander+)
     2.5)
    ((string= enemy +tholian+)
     0.5)
    ((string= enemy +thing+)
     4.0)
    (t
     1.0)))

(defun klingons-per-stardate ()
  "Calculate the number of Klingons killed per stardate."

  (let ((time-used (- *stardate* *initial-stardate*)))
    (when (and (or (= time-used 0)
                   (/= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0))
               (< time-used 5.0))
      (setf time-used 5.0))
    (return-from klingons-per-stardate (/ (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                                             (+ *remaining-klingons*
                                                (length *commander-quadrants*)
                                                *remaining-super-commanders*))
                                          time-used))))

(defun score-multiple (message count score) ; C: score_item(const char *str, int score)
  "Helper function to print a score for multiple items and update the score. The message parameter
is a string suitable for use with the format function."

  (when (> count 0)
    (print-out (format nil message count score))
    (setf *score* (+ *score* score))))

(defun score-single (message score) ; C: score_item(const char *str, int num, int score)
  "Helper function to print a score for a single item and update the score. The message parameter
is a string suitable for use with the format function."

  (print-out (format nil message score))
  (setf *score* (+ *score* score)))

;; TODO - add an option to write the final score to a file, or record it some other way.
;; TODO - can the format be moved to the helper functions? Can the format specification
;;        also put the score in the correct column?
;; TODO - check the grammar of all items, single actions should not use plural nouns
(defun score () ; C: score(void)
  "Compute player's score."

  (if *window-interface-p*
      (if *score-window*
          (progn
            (select-window *score-window*)
            (clear-window)
            (wmove *score-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (setf *score* 0)
  (if *score-window*
      (print-out (format nil "~21@A~%" "SCORE"))
      (print-out (format nil "Your score --~%")))
  (score-multiple "~6@A Romulans destroyed                        ~5@A~%"
                  (- *initial-romulans* *remaining-romulans*)
                  (* 20 (- *initial-romulans* *remaining-romulans*)))
  (when *game-won-p*
    (score-multiple "~6@A Romulans captured                         ~5@A~%"
                    *remaining-romulans* *remaining-romulans*))
  (score-multiple "~6@A ordinary Klingons destroyed               ~5@A~%"
                  (- *initial-klingons* *remaining-klingons*)
                  (* 10 (- *initial-klingons* *remaining-klingons*)))
  (score-multiple "~6@A Klingon commanders destroyed              ~5@A~%"
                  (- *initial-commanders* (length *commander-quadrants*))
                  (* 50 (- *initial-commanders* (length *commander-quadrants*))))
  (score-multiple "~6@A Super-Commander destroyed                 ~5@A~%"
                  (- *initial-super-commanders* *remaining-super-commanders*)
                  (* 200 (- *initial-super-commanders* *remaining-super-commanders*)))
  (score-multiple "~6,2F Klingons per stardate                     ~5@A~%"
                  (klingons-per-stardate) (round (+ (* 500 (klingons-per-stardate)) 0.5)))
  (score-multiple "~6@A Klingons captured               ~5@A~%"
                  *captured-klingons* (* 3 *captured-klingons*))
  (score-multiple "~6@A stars destroyed by your action            ~5@A~%"
                  *destroyed-stars* (* -5 *destroyed-stars*))
  (score-multiple "~6@A uninhabited planets destroyed by your action ~2@A~%"
                  *destroyed-uninhabited-planets* (* -10 *destroyed-uninhabited-planets*))
  (score-multiple "~6@A inhabited planets destroyed by your action   ~2@A~%"
                  *destroyed-inhabited-planets* (* -300 *destroyed-inhabited-planets*))
  (score-multiple "~6@A bases destroyed by your action            ~5@A~%"
                  *destroyed-bases* (* -100 *destroyed-bases*))
  (score-multiple "~6@A calls for help from starbase              ~5@A~%"
                  *calls-for-help* (* -45 *calls-for-help*))
  (score-multiple "~6@A casualties incurred                       ~5@A~%"
                  *casualties* (* -1 *casualties*))
  (score-multiple "~6@A crew abandoned in space                   ~5@A~%"
                  *abandoned-crew* (* -3 *abandoned-crew*))
  (let (ships-destroyed)
    (cond
      ((string= *ship* +enterprise+)
       (setf ships-destroyed 0))
      ((string= *ship* +faerie-queene+)
       (setf ships-destroyed 1))
      ((string= *ship* +no-ship+)
       (setf ships-destroyed 2)))
    (score-multiple "~6@A ship(s) lost or destroyed                 ~5@A~%"
                    ships-destroyed (* -100 ships-destroyed)))
  (score-multiple "~6@A Treaty of Algeron violations                 ~5@A~%"
                    *cloaking-violations* (* -100 *cloaking-violations*))
  (when (not *alivep*)
    (score-single "Penalty for getting yourself killed              ~5@A~%" -200))
  (when *game-won-p*
    (print-out (format nil "       Bonus for winning ~13A           ~5@A~%"
                       (format nil "~A game" (string-capitalize (skill-level-label *skill-level*)))
                       (* 100 (skill-level-value *skill-level*))))
    (setf *score* (+ *score* (* 100 (skill-level-value *skill-level*)))))
  (skip-line)
  (print-out (format nil "       TOTAL SCORE                               ~5@A~%" (round *score*))))

(define-constant +day-names+
    (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define-constant +month-names+
  (list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October"
        "November" "December"))

(defun plaque () ; C: plaque(void)
  "Emit winner's commemmorative plaque."

  ;; TODO - the original Fortran printed on 132 column fanfold, and subsequent versions printed on
  ;;        8.5"x11" or A4 in portrait mode. All versions were nicely centered. One implementation
  ;;        scrolled the ASCII art across the screen.

  (let (file
        winner)
    (clear-type-ahead-buffer)
    (skip-line 2)
    (print-prompt "File or device name for your plaque: ")
    (scan-input)
    (setf file *input-item*)
    (print-prompt "Enter name to go on plaque (up to 30 characters): ")
    (setf winner (read-line))
    (with-open-file (s file :direction :output :if-exists :rename)
      ;; --------DRAW ENTERPRISE PICTURE.
      (format s "                                            EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE~%")
      (format s "                  EEE                      E  : :                                         :  E~%")
      (format s "                EE   EEE                   E  : :                   NCC-1701              :  E~%")
      (format s "EEEEEEEEEEEEEEEE        EEEEEEEEEEEEEE     E  : :                                         : E~%")
      (format s " E                                     E    \EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE~%")
      (format s "   EEEEEEEEE               EEEEEEEEEEEEE                 E  E~%")
      (format s "            EEEEEEE   EEEEE    E          E              E  E~%")
      (format s "                   EEE           E          E            E  E~%")
      (format s "                                   E          E          E  E~%")
      (format s "                                      EEEEEEEEEEEEE      E  E~%")
      (format s "                                   EEE :           EEEEEEE  EEEEEEEE~%")
      (format s "                                 :E    :                 EEEE       E~%")
      (format s "                                .-E   -:-----                       E~%")
      (format s "                                 :E    :                            E~%")
      (format s "                                   EE  :                    EEEEEEEE~%")
      (format s "                                    EEEEEEEEEEEEEEEEEEEEEEE~%")
      (format s "~%~%")
      (format s "~95:@<~A~>~%" "U. S. S. ENTERPRISE")
      (format s "~%~%~%")
      (format s "~95:@<~A~>~%~%" "For demonstrating outstanding ability as a starship captain")
      (format s "~95:@<~A~>~%~%" "Starfleet Command bestows to you")
      (format s "~95:@<~A~>~%~%" winner)
      (format s "~95:@<~A~>~%~%" "the rank of")
      (format s "~95:@<~A~>~%~%" "\"Commodore Emeritus\"")
      (cond
        ((= (skill-level-value *skill-level*) +expert+)
         (format s "~95:@<~A~>~%~%" "Expert level"))

        ((= (skill-level-value *skill-level*) +emeritus+)
         (format s "~95:@<~A~>~%~%" "Emeritus level"))

        (t
         (format s "~95:@<~A~>~%~%" "Cheat level")))
      (multiple-value-bind
            (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
        (setf second second) (setf minute minute) (setf hour hour) (setf day-of-week day-of-week)
        (setf dst-p dst-p) (setf tz tz)
        (format s "~95:@<~A~>~%~%" (format nil "This day of ~A ~A, ~A" date (nth month +month-names+) year)))
      (format s "~95:@<~A~>~%~%" (format nil "Your score:  ~D" *score*))
      (format s "~95:@<~A~>~%~%" (format nil "Klingons per stardate:  ~,2F" (klingons-per-stardate))))))

(defun finish (finish-reason) ; C: finish(FINTYPE ifin)
  "End the game, with appropriate notfications."

  (setf *all-done-p* t)
  (skip-line)
  (print-stars)
  (print-message (format nil "It is stardate ~A.~%" (format-stardate *stardate*)))
  (skip-line)
  (cond
    ;; C: FWON
    ((= finish-reason +won+)
     (setf *game-won-p* t)
     (print-message (format nil "You have smashed the Klingon invasion fleet and saved~%"))
     (print-message (format nil "the Federation.~%"))
     (when (/= *remaining-romulans* 0)
       (print-message (format nil "The remaining ~A Romulans surrender to Starfleet Command.~%" *remaining-romulans*)))
     ;; Captured Klingon crew will get transfered to starbase
     (when (and *alivep*
                (> (- *brig-capacity* *brig-free*) 0))
       (setf *captured-klingons* (+ *captured-klingons* (- *brig-capacity* *brig-free*)))
       (print-message (format nil "The ~D captured Klingons are transferred to Star Fleet Command.~%"
                              (- *brig-capacity* *brig-free*))))
     (when *alivep*
       (let ((bad-points 0.0))
         (setf bad-points (+ (* 5.0 *destroyed-stars*)
                             *casualties*
                             (* 10.0 *destroyed-uninhabited-planets*)
                             (* 300.0 *destroyed-inhabited-planets*)
                             (* 45.0 *calls-for-help*)
                             (* 100.0 *destroyed-bases*)
                             (* 3.0 *abandoned-crew*)))
         (when (string= *ship* +faerie-queene+)
           (setf bad-points (+ bad-points 100.0)))
         (when (string= *ship* +no-ship+) ; TODO - this shouldn't be possible at this point
           (setf bad-points (+ bad-points 300.0)))
         (when (< bad-points 100.0)
           (setf bad-points 0.0)) ; Close enough!
         (when (or (< (- *stardate* *initial-stardate*) 5.0)
                   ;; killsPerDate >= RateMax
                   ;; TODO - for symmetry, could define an (initial-enemies) function, not sure it's needed
                   (>= (/ (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                             (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*))
                          (- *stardate* *initial-stardate*))
                       (+ (* 0.1 (skill-level-value *skill-level*) (+ (skill-level-value *skill-level*) 1.0))
                          0.1
                          (* 0.008 bad-points))))
           (skip-line)
           (print-message (format nil "In fact, you have done so well that Starfleet Command~%"))
           (cond
             ((= (skill-level-value *skill-level*) +novice+)
              (print-message (format nil "promotes you one step in rank from \"Novice\" to \"Fair\".~%")))
             ((= (skill-level-value *skill-level*) +fair+)
              (print-message (format nil "promotes you one step in rank from \"Fair\" to \"Good\".~%")))
             ((= (skill-level-value *skill-level*) +good+)
              (print-message (format nil "promotes you one step in rank from \"Good\" to \"Expert\".~%")))
             ((= (skill-level-value *skill-level*) +expert+)
              (print-message (format nil "promotes you to Commodore Emeritus.~%")))
             ((= (skill-level-value *skill-level*) +emeritus+)
              (print-message "Computer-  ")
              (print-message (format nil "ERROR-ERROR-ERROR-ERROR~%") :print-slowly t)
              (skip-line)
              (print-message
               (format nil "  YOUR-SKILL-HAS-EXCEEDED-THE-CAPACITY-OF-THIS-PROGRAM~%")
               :print-slowly t)
              (skip-line)
              (print-message (format nil "  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (skip-line)
              (print-message (format nil "  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (skip-line)
              (print-message (format nil "  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (skip-line)
              (print-message
               (format nil "  THIS-PROGRAM-MUST?- MUST ? - SUR? ? -?  VI~%")
               :print-slowly t)
              (skip-line)
              (print-message (format nil "Now you can retire and write your own Star Trek game!~%")))))
           (when (>= (skill-level-value *skill-level*) +expert+)
             (print-prompt "Would you like to save your Commodore Emeritus Citation? ")
             (clear-type-ahead-buffer)
             (when (get-y-or-n-p)
               (plaque))))
       ;; Only grant long life if alive (original didn't!)
       (skip-line)
       (print-message (format nil "LIVE LONG AND PROSPER.~%")))
     (score)
     (return-from finish nil))
    ;; FDEPLETE ; Federation Resources Depleted
    ((= finish-reason +deplete+)
     (print-message (format nil "Your time has run out and the Federation has been~%"))
     (print-message (format nil "conquered.  Your starship is now Klingon property,~%"))
     (print-message (format nil "and you are put on trial as a war criminal.  On the~%"))
     (print-message "basis of your record, you are ")
     (if (> (* (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 3.0)
            (+ *initial-klingons* *initial-commanders* *initial-super-commanders*))
         (progn
           (print-message (format nil "acquitted.~%"))
           (skip-line)
           (print-message (format nil "LIVE LONG AND PROSPER.~%")))
         (progn
           (print-message (format nil "found guilty and~%"))
           (print-message (format nil "sentenced to death by slow torture.~%"))
           (setf *alivep* nil)))
     (score)
     (return-from finish nil))
    ;; FLIFESUP
    ((= finish-reason +life-support-consumed+)
     (print-message (format nil "Your life support reserves have run out, and~%"))
     (print-message (format nil "you die of thirst, starvation, and asphyxiation.~%"))
     (skip-line)
     (print-message (format nil "Your starship is a derelict in space.~%")))
    ;; FNRG
    ((= finish-reason +out-of-energy+)
     (print-message (format nil "Your energy supply is exhausted.~%"))
     (skip-line)
     (print-message (format nil "Your starship is a derelict in space.~%")))
    ;; FBATTLE
    ((= finish-reason +battle+)
     (print-message (format nil "The ~A has been destroyed in battle.~%" (format-ship-name)))
     (skip-line)
     (print-message (format nil "Dulce et decorum est pro patria mori.~%")))
    ;; FNEG3
    ((= finish-reason +3-negative-energy-barrier-crossings+)
     (print-message (format nil "You have made three attempts to cross the negative energy~%"))
     (print-message (format nil "barrier which surrounds the galaxy.~%"))
     (skip-line)
     (print-message (format nil "Your navigation is abominable.~%"))
     (score))
    ;; FNOVA
    ((= finish-reason +nova+)
     (print-message (format nil "Your starship has been destroyed by a nova.~%"))
     (skip-line)
     (print-message (format nil "That was a great shot.~%"))
     (skip-line))
    ;; FSNOVAED
    ((= finish-reason +destroyed-by-supernova+)
     (print-message (format nil "The ~A has been fried by a supernova.~%" (format-ship-name)))
     (skip-line)
     (print-message (format nil "...Not even cinders remain...~%")))
    ;; FABANDN
    ((= finish-reason +abandon+)
     (print-message (format nil "You have been captured by the Klingons. If you still~%"))
     (print-message (format nil "had a starbase to be returned to, you would have been~%"))
     (print-message (format nil "repatriated and given another chance. Since you have~%"))
     (print-message (format nil "no starbases, you will be mercilessly tortured to death.~%")))
    ;; FDILITHIUM
    ((= finish-reason +dilithium+)
     (print-message (format nil "Your starship is now an expanding cloud of subatomic particles.~%")))
    ;; FMATERIALIZE
    ((= finish-reason +materialize+)
     (print-message (format nil "Starbase was unable to re-materialize your starship.~%"))
     (skip-line)
     (print-message (format nil "Sic transit gloria mundi~%")))
    ;; FPHASER
    ((= finish-reason +phaser+)
     (print-message (format nil "The ~A has been cremated by its own phasers.~%" (format-ship-name))))
    ;; FLOST
    ((= finish-reason +lost+)
     (print-message (format nil "You and your landing party have been~%"))
     (print-message (format nil "converted to energy, dissipating through space.~%")))
    ;; FMINING
    ((= finish-reason +mining+)
     (print-message (format nil "You are left with your landing party on~%"))
     (print-message (format nil "a wild jungle planet inhabited by primitive cannibals.~%"))
     (print-message (format nil "They are very fond of \"Captain Kirk\" soup.~%"))
     (skip-line)
     (print-message (format nil "Without your leadership, the ~A is destroyed.~%" (format-ship-name))))
    ;; FDPLANET
    ((= finish-reason +destroyed-planet+)
     (print-message (format nil "You and your mining party perish.~%"))
     (skip-line)
     (print-message (format nil "That was a great shot.~%")))
    ;; The Galileo being caught in a supernova is a special case of a mining party being wiped
    ;; out in a nova. Handle them together.
    ;; FSSC, FPNOVA
    ((or (= finish-reason +shuttle-super-nova+)
         (= finish-reason +mining-party-nova+))
     (when (= finish-reason +shuttle-super-nova+)
       (print-message (format nil "The Galileo is instantly annihilated by the supernova.~%")))
     (print-message (format nil "You and your mining party are atomized.~%"))
     (skip-line)
     (print-message (format nil "Mr. Spock takes command of the ~A and~%" (format-ship-name)))
     (print-message (format nil "joins the Romulans, reigning terror on the Federation.~%")))
    ;; FSTRACTOR
    ((= finish-reason +shuttle-tractor-beam+)
     (print-message (format nil "The shuttle craft Galileo is also caught,~%"))
     (print-message (format nil "and breaks up under the strain.~%"))
     (print-message (format nil "Your debris is scattered for millions of miles.~%"))
     (skip-line)
     (print-message (format nil "Without your leadership, the ~A is destroyed.~%" (format-ship-name))))
    ;; FDRAY
    ((= finish-reason +death-ray-malfunction+)
     (print-message (format nil "The mutants attack and kill Spock.~%"))
     (print-message (format nil "Your ship is captured by Klingons, and~%"))
     (print-message (format nil "your crew is put on display in a Klingon zoo.~%")))
    ;; FTRIBBLE
    ((= finish-reason +tribbles+)
     (print-message (format nil "Tribbles consume all remaining water,~%"))
     (print-message (format nil "food, and oxygen on your ship.~%"))
     (print-message (format nil "You die of thirst, starvation, and asphyxiation.~%"))
     (skip-line)
     (print-message (format nil "Your starship is a derelict in space.~%")))
    ;; FHOLE
    ((= finish-reason +destroyed-by-black-hole+)
     (print-message (format nil "Your ship is drawn to the center of the black hole.~%"))
     (print-message (format nil "You are crushed into extremely dense matter.~%")))
    ;; FCREW
    ((= finish-reason +all-crew-killed+)
     (print-message (format nil "Your last crew member has died.~%")))
    ;; FCLOAK
    ((= finish-reason +cloak+)
     (setf *cloaking-violations* (1+ *cloaking-violations*))
     (print-message (format nil "You have violated the Treaty of Algeron.~%"))
     (print-message (format nil "The Romulan Empire can never trust you again.~%")))
    ;; should never reach this, but here we are
    (t
     (print-message (format nil "Game over, man!~%"))
     (skip-line)))
  (when (and (/= finish-reason +won+)
             (/= finish-reason +cloak+)
             *cloakedp*)
    (print-message (format nil "Your ship was cloaked so your subspace radio did not receive anything.~%"))
    (print-message (format nil "You may have missed some warning messages.~%")))
  ;; Win or lose, by this point the player did not survive.
  (setf *alivep* nil)
  ;; Downgrade the ship for score calculation purposes. TODO - this can probably just be based on *alivep*
  (when (string= *ship* +faerie-queene+)
    (setf *ship* +no-ship+))
  (when (string= *ship* +enterprise+)
    (setf *ship* +faerie-queene+))
  (if (/= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
      (let ((for 0)
            (against 0))
        (setf for (/ *remaining-resources* *initial-resources*))
        (setf against (/ (+ *remaining-klingons*
                            (* 2.0 (length *commander-quadrants*)))
                         (+ *initial-klingons*
                            (* 2.0 *initial-commanders*))))
        (if (>= (/ for against)
                (+ 1.0 (* 0.5 (random 1.0))))
            (progn
              (print-message (format nil "As a result of your actions, a treaty with the Klingon~%"))
              (print-message (format nil "Empire has been signed. The terms of the treaty are~%"))
              (if (>= (/ for against)
                      (+ 3.0 (random 1.0)))
                  (progn
                    (print-message (format nil "favorable to the Federation.~%"))
                    (print-message (format nil "Congratulations!~%")))
                  (print-message (format nil "highly unfavorable to the Federation.~%"))))
            (print-message (format nil "The Federation will be destroyed.~%"))))
      (progn
        (print-message (format nil "Since you took the last Klingon with you, you are a~%"))
        (print-message (format nil "martyr and a hero. Someday maybe they'll erect a~%"))
        (print-message (format nil "statue in your memory. Rest in peace, and try not~%"))
        (print-message (format nil "to think about pigeons.~%"))
        (setf *game-won-p* t)))
  (score))

(defun kaboom () ; C: kaboom(void)

  (print-stars)
  (when (string= *ship* +enterprise+)
    (print-message "***") :print-slowly t) ; Extra stars so the lengths of the output lines are the same
  (print-message
   (format nil "********* Entropy of ~A maximized *********~%" (format-ship-name)) :print-slowly t)
  (print-stars)
  (do ((whammo (* 25.0 *ship-energy*))
       (i 0 (+ i 1))) ; TODO - C source starts at 1 and counts up, why?
      ((<= i *enemies-here*))
    (when (<= (* (aref *klingon-energy* i) (aref *klingon-distance* 1)) whammo)
      (dead-enemy (aref *klingon-sectors* i)
                  (coord-ref *quadrant-contents* (aref *klingon-sectors* i))
                  (aref *klingon-sectors* i))))
  (finish +dilithium+))

(defun expran (average)
  "Generate a random number for a time in the future.
This function name and doc string should be more informative."

  ;; TODO - are these numbers reasonably sized?
  ;;(return-from expran (* (- average) (log (+ (expt 1 -7) (random 1.0)))))) ; C: -avrage*log(1e-7 + Rand())
  (return-from expran (* average (log (+ (expt 1 -7) (random 1.0)))))) ; C: -avrage*log(1e-7 + Rand())

(defun get-random-quadrant () ; C: coord randplace(int size)
  "Get a coordinate structure for a random quadrant."

  (make-coordinate :x (random +galaxy-size+) :y (random +galaxy-size+)))

(defun get-random-sector () ; C: coord randplace(int size)
  "Get a coordinate structure for a random sector."

  (make-coordinate :x (random +quadrant-size+) :y (random +quadrant-size+)))

(defun drop-entity-in-sector (entity) ; coord dropin(feature iquad)
  "Drop a game entity in a random sector in the current quadrant. Return the sector coordinates
of the entity."

  (do ((c (get-random-sector) (get-random-sector)))
      ((string= (coord-ref *quadrant-contents* c) +empty-sector+)
       (setf (coord-ref *quadrant-contents* c) entity)
       (return-from drop-entity-in-sector c))))

(defun drop-klingon-in-sector () ; coord newkling(int i)
  "Drop a new Klingon into the current quadrant. Return the sector coordinates, distance from the
ship, and Klingon power."

  (let ((c (drop-entity-in-sector +klingon+)))
  (return-from drop-klingon-in-sector (values c
                                              (distance *ship-sector* c)
                                              (+ (* (random 1.0) 150.0) 300.0 (* 25.0 (skill-level-value *skill-level*))))))) ; Rand()*150.0 +300.0 +25.0*game.skill

(defun drop-commander-in-sector ()
  "Drop a new Commander into the current quadrant. Return the sector coordinates, distance from the
ship, and Commander power."

  (let ((c (drop-entity-in-sector +commander+)))
  (return-from drop-commander-in-sector (values c
                                                (distance *ship-sector* c)
                                                (+ 950.0 (* (random 1.0) 400.0) (* 50.0 (skill-level-value *skill-level*))))))) ; 950.0+400.0*Rand()+50.0*game.skill

(defun drop-super-commander-in-sector ()
  "Drop a new Super-commander into the current quadrant. Return the sector coordinates, distance
from the ship, and Super-commander power."

  (let ((c (drop-entity-in-sector +super-commander+)))
  (return-from drop-super-commander-in-sector (values c
                                                      (distance *ship-sector* c)
                                                      (+ 1175.0 (* (random 1.0) 400.0) (* 125.0 (skill-level-value *skill-level*))))))) ; 1175.0 + 400.0*Rand() + 125.0*game.skill

(defun drop-romulan-in-sector ()
  "Drop a new Romulan into the current quadrant. Return the sector coordinates, distance from the
ship, and Romulan power."

  (let ((c (drop-entity-in-sector +romulan+)))
  (return-from drop-romulan-in-sector (values c
                                              (distance *ship-sector* c)
                                              (+ (* (random 1.0) 400.0) 450.0 (* 50.0 (skill-level-value *skill-level*))))))) ; Rand()*400.0 + 450.0 + 50.0*game.skill

(defun drop-space-thing-in-sector ()
  "Drop a Space Thing into the current quadrant. Return the sector coordinates, distance from the
ship, and Thing power."

  (let ((c (drop-entity-in-sector +thing+)))
  (return-from drop-space-thing-in-sector (values c
                                                  (distance *ship-sector* c)
                                                  (+ (* (random 1.0) 6000.0) 500.0 (* 250.0 (skill-level-value *skill-level*))))))) ; Rand()*6000.0 +500.0 +250.0*game.skill

(defun drop-tholian-in-sector ()
  "Drop a Tholian into the current quadrant. Tholians only occupy the perimeter of a quadrant.
Return the sector coordinates, distance from the ship, and Tholian power."

  (do ((sector-ok-p nil)
       x y c)
      (sector-ok-p
       (setf (aref *quadrant-contents* x y) +tholian+)
       (setf c (make-coordinate :x x :y y))
       (return-from drop-tholian-in-sector (values c
                                                   (distance *ship-sector* c)
                                                   (+ (* (random 1.0) 400.0) 100.0 (* 25.0 (skill-level-value *skill-level*)))))) ; Rand()*400.0 +100.0 +25.0*game.skill
    (if (> (random 1.0) 0.5)
        (setf x (- +quadrant-size+ 1))
        (setf x 0))
    (if (> (random 1.0) 0.5)
        (setf y (- +quadrant-size+ 1))
        (setf y 0))
    (if (string= (aref *quadrant-contents* x y) +empty-sector+)
        (setf sector-ok-p t)
        (setf sector-ok-p nil))))

;; TODO - can Lisp sort function be used?
(defun sort-klingons () ; C: sortklings(void)
  "Sort klingons by distance from us, closest Klingons first in the list."

  (when (> (- *enemies-here* *things-here* *tholians-here*) 1)
    ;; Bubble sort
    (do ((exchanged t))
        ((not exchanged))
      (setf exchanged nil)
      (do ((j 0 (+ j 1))
           temp)
          ((< j *enemies-here*))
        (when (> (aref *klingon-distance* j) (aref *klingon-distance* (+ j 1)))
          (setf exchanged t)
          (setf temp (aref *klingon-distance* j))
          (setf (aref *klingon-distance* j) (aref *klingon-distance* (+ j 1)))
          (setf (aref *klingon-distance* (+ j 1)) temp)
          (setf temp (aref *klingon-average-distance* j))
          (setf (aref *klingon-average-distance* j) (aref *klingon-average-distance* (+ j 1)))
          (setf (aref *klingon-average-distance* (+ j 1)) temp)
          (setf temp (aref *klingon-sectors* j))
          (setf (aref *klingon-sectors* j) (aref *klingon-sectors* (+ j 1)))
          (setf (aref *klingon-sectors* (+ j 1)) temp)
          (setf temp (aref *klingon-energy* j))
          (setf (aref *klingon-energy* j) (aref *klingon-energy* (+ j 1)))
          (setf (aref *klingon-energy* (+ j 1)) temp))))))

(defun new-quadrant (&key (show-thing t))
  "Set up a new quadrant when it is entered or re-entered. The thing should only be shown when the
player has reached a base by abandoning ship or using the SOS command."

  (setf *just-in-p* t)
  (setf *klingons-here* 0) ; TODO - a convenience variable to avoid referencing the galaxy array, keep it?
  (setf *commanders-here* 0)
  (setf *super-commanders-here* 0)
  (setf *romulans-here* 0) ; TODO - a convenience variable to avoid referencing the galaxy array, keep it?
  (setf *romulan-neutral-zone-p* nil)
  (setf *cloaking-violation-reported-p* nil)
  (setf *enemies-here* 0)
  (setf *tholians-here* 0)
  (setf *things-here* 0)
  (setf *thing-is-angry-p* nil)
  (setf *base-sector* nil)
  (setf *base-attack-report-seen-p* nil)
  (setf *current-planet* nil)
  (setf *planet-coord* nil)
  (setf *in-orbit-p* nil)
  (setf *landedp* nil)
  (setf *attempted-escape-from-super-commander-p* nil)
  (when *super-commander-attack-enterprise-p*
    ;; Attempt to escape Super-commander, so tractor beam back!
    (setf *super-commander-attack-enterprise-p* nil)
    (setf *attempted-escape-from-super-commander-p* t))

  ;; TODO - do the above variables need to be initialized if we are leaving the quadrant anyway?
  ;; Cope with supernova
  (unless (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
    ;; Clear/initialize quadrant
    (do ((i 0 (1+ i)))
        ((>= i +quadrant-size+))
      (do ((j 0 (1+ j)))
          ((>= j +quadrant-size+))
        (setf (aref *quadrant-contents* i j) +empty-sector+)))

    (setf *klingons-here*
          (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)))
    (setf *romulans-here*
          (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)))
    (setf *enemies-here* (+ *klingons-here* *romulans-here*))

    ;; Position starship. Do this first, all sectors are still empty.
    (setf (coord-ref *quadrant-contents* *ship-sector*) *ship*)

    (when (or (> (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0)
              (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0))
      (let ((remaining-klingons (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)))
            (next-index 0))
        ;; Put the super-commander in the quadrant if present
        (when (and *super-commander-quadrant*
                   (coord-equal *ship-quadrant* *super-commander-quadrant*))
          (multiple-value-bind (coordinates distance power) (drop-super-commander-in-sector)
            (setf (aref *klingon-sectors* next-index) coordinates)
            (setf (aref *klingon-distance* next-index) distance)
            (setf (aref *klingon-average-distance* next-index) distance)
            (setf (aref *klingon-energy* next-index) power)
            (setf *super-commanders-here* (1+ *super-commanders-here*))
            (setf remaining-klingons (1- remaining-klingons))
            (setf next-index (1+ next-index))))
        ;; Put a Commander in the quadrant if there is one.
        (dolist (cq *commander-quadrants*)
          (when (coord-equal *ship-quadrant* cq)
            (setf *commanders-here* (1+ *commanders-here*))
            (multiple-value-bind (coordinates distance power) (drop-commander-in-sector)
              (setf (aref *klingon-sectors* next-index) coordinates)
              (setf (aref *klingon-distance* next-index) distance)
              (setf (aref *klingon-average-distance* next-index) distance)
              (setf (aref *klingon-energy* next-index) power)
              (setf remaining-klingons (1- remaining-klingons))
              (setf next-index (1+ next-index)))))
        ;; Position ordinary Klingons
        (do ((i 1 (1+ i)))
            ((> i remaining-klingons))
          (multiple-value-bind (coordinates distance power) (drop-klingon-in-sector)
            (setf (aref *klingon-sectors* next-index) coordinates)
            (setf (aref *klingon-distance* next-index) distance)
            (setf (aref *klingon-average-distance* next-index) distance)
            (setf (aref *klingon-energy* next-index) power)
            (setf next-index (1+ next-index))))
        ;; Put in Romulans if needed
        (do ((r 1 (1+ r))) ; This is a count
            ((> r (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*))))
          (multiple-value-bind (coordinates distance power) (drop-romulan-in-sector)
            (setf (aref *klingon-sectors* next-index) coordinates)
            (setf (aref *klingon-distance* next-index) distance)
            (setf (aref *klingon-average-distance* next-index) distance)
            (setf (aref *klingon-energy* next-index) power)
            (setf next-index (1+ next-index))))))

    ;; If quadrant needs a starbase then put it in.
    (when (> (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*)) 0)
      (setf *base-sector* (drop-entity-in-sector +starbase+)))

    ;; If quadrant needs a planet then put it in
    (when (assoc *ship-quadrant* *planet-information* :test #'coord-equal) ; non-nil when planet exists
      (setf *planet-coord* *ship-quadrant*)
      (let ((p (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
        (if (planet-inhabitedp p)
            (setf *current-planet* (drop-entity-in-sector +world+))
            (setf *current-planet* (drop-entity-in-sector +planet+)))))

    ;; Check for condition
    (update-condition)

    ;; And finally the stars
    (do ((i 1 (1+ i))) ; another count
        ((> i (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
      (drop-entity-in-sector +star+))

    ;; Check for Romulan Neutral Zone: Romulans present and no Klingons.
    ;; TODO - does it make sense to have RNZ when a base is present?
    (when (and (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0)
               (= (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0))
      (setf *romulan-neutral-zone-p* t)
      (when (not (damagedp +subspace-radio+))
        (skip-line)
        (print-message (format nil "LT. Uhura- \"Captain, an urgent message.~%"))
        (print-message (format nil "  I'll put it on audio.\"  CLICK~%"))
        (skip-line)
        (print-message (format nil "INTRUDER! YOU HAVE VIOLATED THE ROMULAN NEUTRAL ZONE.~%"))
        (print-message (format nil "LEAVE AT ONCE, OR YOU WILL BE DESTROYED!~%"))))

    ;; Put in THING if needed
    (when (and show-thing
               (coord-equal *thing-location* *ship-quadrant*))
      (setf *thing-location* (get-random-quadrant))
      (1+ *enemies-here*)
      (setf *things-here* 1)
      (multiple-value-bind (coordinates distance power) (drop-space-thing-in-sector)
        (setf (aref *klingon-sectors* *enemies-here*) coordinates)
        (setf (aref *klingon-distance* *enemies-here*) distance)
        (setf (aref *klingon-average-distance* *enemies-here*) distance)
        (setf (aref *klingon-energy* *enemies-here*) power))
      (unless (damagedp +short-range-sensors+)
        (print-message (format nil "Mr. Spock- \"Captain, this is most unusual.~%"))
        (print-message (format nil "    Please examine your short-range scan.\"~%"))))

    ;; Decide if quadrant needs a Tholian
    (when (or (and (< (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.02)) ; Lighten up if skill is low
              (and (= (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.05))
              (and (> (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.08)))
      (setf *tholians-here* 1)
      (1+ *enemies-here*)
      (multiple-value-bind (coordinates distance power) (drop-tholian-in-sector)
        (setf *tholian-sector* coordinates)
        ;; TODO - this is suspicious: the Tholian, if present, must always be the last item in the array.
        ;; Is that relationship maintained, for example when klingons are destroyed?
        (setf (aref *klingon-sectors* *enemies-here*) coordinates)
        (setf (aref *klingon-distance* *enemies-here*) distance)
        (setf (aref *klingon-average-distance* *enemies-here*) distance)
        (setf (aref *klingon-energy* *enemies-here*) power))
      ;; Reserve unoccupied corners
      (when (string= (aref *quadrant-contents* 0 0) +empty-sector+)
        (setf (aref *quadrant-contents* 0 0) +reserved+))
      (when (string= (aref *quadrant-contents* 0 (- +quadrant-size+ 1)) +empty-sector+)
        (setf (aref *quadrant-contents* 0 (- +quadrant-size+ 1)) +reserved+))
      (when (string= (aref *quadrant-contents* (- +quadrant-size+ 1) 0) +empty-sector+)
        (setf (aref *quadrant-contents* (- +quadrant-size+ 1) 0) +reserved+))
      (when (string= (aref *quadrant-contents* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +empty-sector+)
        (setf (aref *quadrant-contents* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +reserved+)))

    ;; TODO - does this need to be done in new-quadrant?
    (sort-klingons)

    ;; Put in a few black holes
    (do ((i 0 (1+ i)))
        ((> i 3))
      (when (> (random 1.0) 0.5)
        (drop-entity-in-sector +black-hole+)))

    ;; Take out X's in corners if Tholian present
    (when (> *tholians-here* 0)
      (when (string= (aref *quadrant-contents* 0 0) +reserved+)
        (setf (aref *quadrant-contents* 0 0) +empty-sector+))
      (when (string= (aref *quadrant-contents* 0 (- +quadrant-size+ 1)) +reserved+)
        (setf (aref *quadrant-contents* 0 (- +quadrant-size+ 1)) +empty-sector+))
      (when (string= (aref *quadrant-contents* (- +quadrant-size+ 1) 0) +reserved+)
        (setf (aref *quadrant-contents* (- +quadrant-size+ 1) 0) +empty-sector+))
      (when (string= (aref *quadrant-contents* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +reserved+)
        (setf (aref *quadrant-contents* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +empty-sector+)))))

(defun abandon-ship () ; C: abandon(void)
  "The ship is abandoned. If your current ship is the Faire Queene, or if your shuttle craft is
dead, you're out of luck. You need the shuttle craft in order for the captain (that's you!!) to
escape.

Your crew can beam to an inhabited starsystem in the quadrant, if there is one and if the
transporter is working. If there is no inhabited starsystem, or if the transporter is out, they
are left to die in outer space.

If there are no starbases left, you are captured by the Klingons, who torture you mercilessly.
However, if there is at least one starbase, you are returned to the Federation in a prisoner of war
exchange. Of course, this can't happen unless you have taken some prisoners."

  (if *dockedp*
      (when (string/= *ship* +enterprise+)
        (print-message (format nil "You cannot abandon Ye Faerie Queene.~%"))
        (return-from abandon-ship nil))
      ;; Must take shuttle craft to exit
      (cond
        ((string= *ship* +faerie-queene+) ; C: game.damage[DSHUTTL]==-1
         (print-message (format nil "Ye Faerie Queene has no shuttle craft.~%"))
         (return-from abandon-ship nil))

        ((< (aref *device-damage* +shuttle+) 0)
         (print-message (format nil "Shuttle craft now serving Big Macs.~%"))
         (return-from abandon-ship nil))

        ((> (aref *device-damage* +shuttle+) 0)
         (print-message (format nil "Shuttle craft damaged.~%"))
         (return-from abandon-ship nil))

        (*landedp*
         (print-message (format nil "You must be aboard the ship.~%"))
         (return-from abandon-ship nil))

        ((not (eql *shuttle-craft-location* 'on-ship))
         (print-message (format nil "Shuttle craft not currently available.~%"))
         (return-from abandon-ship nil))

        (t
         ;; Print abandon ship messages
         (skip-line)
         (print-message (format nil "***ABANDON SHIP!  ABANDON SHIP!~%") :print-slowly t)
         (skip-line)
         (print-message (format nil "***ALL HANDS ABANDON SHIP!~%") :print-slowly t)
         (skip-line 2)
         ;; TODO - this isn't consistent with the "Entire crew.." message below
         (print-message (format nil "Captain and crew escape in shuttle craft.~%"))
         (when (= (length *base-quadrants*) 0)
           ;; Oops! no place to go...
           (finish +abandon+)
           (return-from abandon-ship nil))
         ;; Dispose of crew.
         ;; Before the introduction of inhabited planets the message was
         ;; "Remainder of ship's complement beam down"
         ;; "to nearest habitable planet."
         (let ((p (first (assoc *ship-quadrant* *planet-information* :test #'coord-equal)))) ; non-nil if planet exists
           (if (and p
                    (not (damagedp +transporter+)))
               (print-message (format nil "Remainder of ship's complement beam down to ~A.~%"
                                      (planet-name p)))
               (progn
                 (print-message (format nil "Entire crew of ~A left to die in outer space.~%" *crew*))
                 (setf *casualties* (+ *casualties* *crew*))
                 (setf *abandoned-crew* (+ *abandoned-crew* *crew*)))))
         ;; If at least one base left, give 'em the Faerie Queene
         (setf *dilithium-crystals-on-board-p* nil) ; crystals are lost
         (setf *probes-available* 0) ; No probes
         (skip-line)
         (print-message (format nil "You are captured by Klingons and released to~%"))
         (print-message (format nil "the Federation in a prisoner-of-war exchange.~%"))
         ;; Set up quadrant and position FQ adjacent to base
         ;; Select a random base to be the new start
         (let ((nth-base (truncate (* (length *base-quadrants*) (random 1.0)))))
           (when (not (coord-equal *ship-quadrant* (nth nth-base *base-quadrants*)))
             (setf *ship-quadrant* (nth nth-base *base-quadrants*))
             (setf (coordinate-x *ship-sector*) (/ +quadrant-size+ 2))
             (setf (coordinate-y *ship-sector*) (/ +quadrant-size+ 2))
             (new-quadrant)))
         ;; position next to base by trial and error
         (setf (coord-ref *quadrant-contents* *ship-sector*) +empty-sector+)
         (do ((positionedp nil))
             (positionedp)
           (do ((count 0 (1+ count)))
               ((or (>= count 100) ; previously +quadrant-size+, don't give up so easily
                    positionedp))
             (setf (coordinate-x *ship-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-x *base-sector*))))
             (setf (coordinate-y *ship-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-y *base-sector*))))
             (when (and (valid-sector-p (coordinate-x *ship-sector*) (coordinate-y *ship-sector*))
                        (string= (coord-ref *quadrant-contents* *ship-sector*) +empty-sector+))
               (setf positionedp t))) ; found a spot
           (unless positionedp
             (setf (coordinate-x *ship-sector*) (/ +quadrant-size+ 2))
             (setf (coordinate-y *ship-sector*) (/ +quadrant-size+ 2))
             (new-quadrant))))))
  ;; Get new commission
  (setf (coord-ref *quadrant-contents* *ship-sector*) +faerie-queene+)
  (setf *crew* +full-crew+)
  (print-message (format nil "Starfleet puts you in command of another ship,~%"))
  (print-message (format nil "the Faerie Queene, which is antiquated but,~%"))
  (print-message (format nil "still useable.~%"))
  (when *dilithium-crystals-on-board-p*
    (print-message (format nil "The dilithium crystals have been moved.~%")))
  (setf *miningp* nil)
  (setf *shuttle-craft-location* 'off-ship) ; Galileo disappears
  (setf *shuttle-craft-quadrant* nil) ; FQ has no shuttle, therefore no location
  (setf *brig-capacity* 300) ; Less capacity now
  (setf *brig-free* *brig-capacity*)
  (setf *cloakedp* nil)
  (setf *cloakingp* nil)
  ;; Resupply ship
  (setf *dockedp* t)
  (do ((device-index 0 (1+ device-index)))
      ((>= device-index +number-of-devices+))
    (setf (aref *device-damage* device-index) 0.0))
  (setf (aref *device-damage* +shuttle+) -1) ; TODO - This is a flag to indicate no shuttle exists
  (setf *initial-energy* 3000.0)
  (setf *ship-energy* 3000.0)
  (setf *initial-shield-energy* 1250.0)
  (setf *shield-energy* 1250.0)
  (setf *initial-torpedos* 6)
  (setf *torpedoes* 6)
  (setf *initial-life-support-reserves* 3.0)
  (setf *life-support-reserves* 3.0)
  (setf *shields-are-up-p* nil)
  (setf *warp-factor* 5.0))

(defun dock () ; C: dock(bool verbose)
  "Dock the ship at a starbase."

  (skip-line)
  (cond
    (*dockedp*
     (print-message (format nil "Already docked.~%")))

    (*in-orbit-p*
     (print-message (format nil "You must first leave standard orbit.~%")))

    ((or (not *base-sector*)
         (> (abs (- (coordinate-x *ship-sector*) (coordinate-x *base-sector*))) 1)
         (> (abs (- (coordinate-y *ship-sector*) (coordinate-y *ship-sector*))) 1))
     (print-message (format nil "~A not adjacent to base.~%" (format-ship-name))))

    (*cloakedp*
     (print-message (format nil "You cannot dock while cloaked.~%")))

    (t
     (setf *dockedp* t)
     (print-message (format nil "Docked.~%"))
     (setf *action-taken-p* t)
     (when (< *ship-energy* *initial-energy*) ; Keep energy overload from dilithium crystals
       (setf *ship-energy* *initial-energy*))
     (setf *shield-energy* *initial-shield-energy*)
     (setf *torpedoes* *initial-torpedos*)
     (setf *life-support-reserves* *initial-life-support-reserves*)
     (setf *crew* +full-crew+)
     (when (> (- *brig-capacity* *brig-free*) 0)
       (print-message (format nil "~D captured Klingons transferred to base.~%"
                              (- *brig-capacity* *brig-free*)))
       (setf *captured-klingons* (+ *captured-klingons* (- *brig-capacity* *brig-free*)))
       (setf *brig-free* *brig-capacity*))
     ;; TODO - Possible approach for base attack report seen: make each report an event and only
     ;;        report the event when the radio is functioning or after it has been repaired.
     (when (and (or (/= *super-commander-attacking-base* 0)
                    (is-scheduled-p +commander-destroys-base+))
                (not *base-attack-report-seen-p*))
       ;; Get attack report from base
       (skip-line)
       (print-message (format nil "Lt. Uhura- \"Captain, an important message from the starbase:\"~%"))
       (attack-report)
       (setf *base-attack-report-seen-p* t)))))

(defun put-short-range-scan-symbol (symbol-coord symbol) ; C: void put_srscan_sym(coord w, char sym)
  "In curses mode, place the symbol at symbol-coord in the short range scan."

  ;; TODO - write this
  (setf symbol-coord symbol-coord)
  (setf symbol symbol)
  (print-message (format nil "Debug: displaying a short range scan symbol~%"))
  )

(defun sound (frequency) ; C: not user-defined...
  "Play a sound on the PC speaker at the specified frequency."

  ;; TODO - implement this, probably by calling some OS routine or external library
  (setf frequency frequency)
  )

(defun warble () ; C: void warble(void)
  "Sound and visual effects for teleportation"

  ;; TODO - this function is a placeholder, implement it in a portable way
)

(defun boom (c) ; C: void boom(coord w)
  "enemy fall down, go boom - display a boom effect and make a boom noise"

  ;; TODO - write this
  (setf c c))

(defun time-warp () ; C: timwrp(), /* let's do the time warp again */
  "Travel forward or backward in time."

  (print-message (format nil "***TIME WARP ENTERED.~%"))
  (if (and *snapshot-taken-p*
           (< (random 1.0) 0.5))
      (progn
        ;; Go back in time
        (print-message (format nil "You are traveling backwards in time ~A stardates.~%"
                               (truncate (- *stardate* (snapshot-stardate *snapshot*)))))
        ;; Note the current shuttle location, compare it with the previous historical location, and
        ;; remove or add the shuttle as needed
        (let ((shuttle-craft-location *shuttle-craft-location*))
          ;; (shuttle-craft-quadrant *shuttle-craft-quadrant*)
          (use-snapshot)
          (setf *snapshot-taken-p* nil)
          ;; Reschedule events to occur relative to the new current stardate
          (when (> (length *commander-quadrants*) 0)
            (schedule-event +tractor-beam+ (expran (/ *initial-time* (length *commander-quadrants*))))
            (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*))))
          (schedule-event +supernova+ (expran (* 0.5 *initial-time*)))
          ;; Next snapshot will be sooner
          (schedule-event +snapshot-for-time-warp+ (expran (* 0.25 *remaining-time*)))
          (when (> *remaining-super-commanders* 0)
            (schedule-event +move-super-commander+ 0.2777))
          (setf *super-commander-attacking-base* 0)
          (unschedule +commander-destroys-base+)
          (unschedule +super-commander-destroys-base+)
          (setf *base-under-attack-quadrant* nil)
          (cond
            ;; Make sure Galileo is consistent -- Snapshot may have been taken when on planet,
            ;; which would give us two Galileos!
            ((and (eql *shuttle-craft-location* 'off-ship) ; The shuttle is on the planet
                  *shuttle-craft-quadrant*                 ; in the indicated quadrant,
                  (eql shuttle-craft-location 'on-ship)    ; and before the time warp
                  (string= *ship* +enterprise+))           ; was on the Enterprise
             (print-message (format nil "Checkov-  \"Security reports the Galileo has disappeared, Sir!~%")))
            ;; Likewise, if in the original time the Galileo was abandoned but was on ship in the
            ;; earlier time line, it would have vanished -- let's restore it.
            ((and (not (eql shuttle-craft-location 'on-ship))
                  (eql *shuttle-craft-location* 'on-ship))
             (if (string= *ship* +enterprise+) ; Can't restore to the FQ
                 (print-message (format nil "Checkov-  \"Security reports the Galileo has reappeared in the dock!\"~%"))
                 (progn
                   (setf *shuttle-craft-location* 'off-ship)
                   (setf *shuttle-craft-quadrant* nil))))))
          ;; There used to be code to do the actual reconstruction here,
          ;; but the starchart is now part of the snapshotted galaxy state.
          (print-message (format nil "Spock has reconstructed a correct star chart from memory~%")))
      (progn
        ;; Go forward in time
        (setf *time-taken-by-current-operation* (* -0.5 *initial-time* (log (random 1.0))))
        (print-message (format nil "You are traveling forward in time ~A stardates.~%" *time-taken-by-current-operation*))
        ;; Cheat to make sure no tractor beams occur during time warp
        (postpone-event +tractor-beam+ *time-taken-by-current-operation*)
        (setf (aref *device-damage* +subspace-radio+)
              (+ (aref *device-damage* +subspace-radio+) *time-taken-by-current-operation*))))
  (new-quadrant)
  (process-events)) ; Stas Sergeev added this -- do pending events

;; TODO - Tholian didn't move
(defun move-tholian () ; C: void movetholian(void)
  "Move the Tholian. Tholians always start in a corner and move along the edge of the quadrant. At
each turn the Tholian moves counterclockwise, building the Tholian Web along the next edge. On the
fourth turn the Tholian vanishes, leaving behind a black hole. If the Tholian is obstructed by an
object then it waits, in case the player helpfully removes the blocking object."

  (when (and (> *tholians-here* 1)
              (not *just-in-p*))
    (let ((new-x 0)
          (new-y 0))
      (cond
        ((and (= (coordinate-x *tholian-sector*) 1)
              (= (coordinate-x *tholian-sector*) 1))
         (setf new-x 1)
         (setf new-y +quadrant-size+))

        ((and (= (coordinate-x *tholian-sector*) 1)
              (= (coordinate-x *tholian-sector*) +quadrant-size+))
         (setf new-x +quadrant-size+)
         (setf new-y +quadrant-size+))

        ((and (= (coordinate-x *tholian-sector*) +quadrant-size+)
              (= (coordinate-x *tholian-sector*) +quadrant-size+))
         (setf new-x +quadrant-size+)
         (setf new-y 1))

        ((and (= (coordinate-x *tholian-sector*) +quadrant-size+)
              (= (coordinate-x *tholian-sector*) 1))
         (setf new-x 1)
         (setf new-y 1))

        (t
         ;; Something is wrong!
         (setf *tholians-here* 0)
         (return-from move-tholian nil)))
      ;; Do nothing if we are blocked
      (when (or (string= (aref *quadrant-contents* new-x new-y) +empty-sector+)
                (string= (aref *quadrant-contents* new-x new-y) +tholian-web+))
        (setf (aref *quadrant-contents* new-x new-y) +tholian-web+)
        (cond
          ;; Move in x axis
          ((/= (coordinate-x *tholian-sector*) new-x)
           (do ((incr-x (/ (abs (- new-x (coordinate-x *tholian-sector*)))
                           (- new-x (coordinate-x *tholian-sector*)))))
               ((= (coordinate-x *tholian-sector*) new-x))
             (setf (coordinate-x *tholian-sector*) incr-x)
             (when (string= (coord-ref *quadrant-contents* *tholian-sector*) +empty-sector+)
               (setf (coord-ref *quadrant-contents* *tholian-sector*) +tholian-web+))))
          ;; Move in y axis
          ((/= (coordinate-y *tholian-sector*) new-y)
           (do ((incr-y (/ (abs (- new-y (coordinate-y *tholian-sector*)))
                          (- new-y (coordinate-y *tholian-sector*)))))
               ((= (coordinate-y *tholian-sector*) new-y))
             (setf (coordinate-y *tholian-sector*) incr-y)
             (when (string= (coord-ref *quadrant-contents* *tholian-sector*) +empty-sector+)
               (setf (coord-ref *quadrant-contents* *tholian-sector*) +tholian-web+)))))
        (setf (coord-ref *quadrant-contents* *tholian-sector*) +tholian+)
        ;; TODO - This looks suspicious - why must the Tholian be the last item in the array? The technique for
        ;; deleting an enemy from an array is often to move the last one in the array to the position of the
        ;; deleted enemy. Is that done with *klingon-sectors*?
        (setf (aref *klingon-sectors* *enemies-here*) *tholian-sector*)
        ;; Check to see if all holes plugged
        (do ((i 1 (1+ i))
             (all-holes-plugged-p t))
            ((or (>= i +quadrant-size+)
                 (not all-holes-plugged-p))
             (when all-holes-plugged-p
               ;; All plugged up -- Tholian splits
               (setf (coord-ref *quadrant-contents* *tholian-sector*) +tholian-web+)
               (drop-entity-in-sector +black-hole+)
               (print-message (format nil "***Tholian at ~A completes web.~%" (format-sector-coordinates *tholian-sector*)))
               (setf *tholians-here* 0)
               (setf *enemies-here* (1- *enemies-here*))))
          (when (and (string/= (aref *quadrant-contents* 1 i) +tholian-web+)
                     (string/= (aref *quadrant-contents* 1 i) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant-contents* +quadrant-size+ i) +tholian-web+)
                     (string/= (aref *quadrant-contents* +quadrant-size+ i) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant-contents* i 1) +tholian-web+)
                     (string/= (aref *quadrant-contents* i 1) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant-contents* i +quadrant-size+ ) +tholian-web+)
                     (string/= (aref *quadrant-contents* i +quadrant-size+) +tholian+))
            (setf all-holes-plugged-p nil)))))))

(defun try-exit (look enemy-letter enemy-index running-away-p) ; C: bool tryexit(coord look, int ienm, int loccom, bool irun)
  "A Klingon attempts to leave the current quadrant. Return true if successful."

  (let ((destination-quadrant (make-coordinate))) ; C: iq
    (setf (coordinate-x destination-quadrant) (+ (coordinate-x *ship-quadrant*)
                                                 (/ (+ (coordinate-x look) (1- +quadrant-size+))
                                                    (1- +quadrant-size+))))
    (setf (coordinate-y destination-quadrant) (+ (coordinate-y *ship-quadrant*)
                                                 (/ (+ (coordinate-y look) (1- +quadrant-size+))
                                                    (1- +quadrant-size+))))
    ;; Check for reasons why no can do
    (when (or (not (valid-quadrant-p (coordinate-x destination-quadrant) ; negative energy barrier
                                     (coordinate-y destination-quadrant)))
              (quadrant-supernovap (coord-ref *galaxy* destination-quadrant)) ; supernova
              (> (quadrant-klingons (coord-ref *galaxy* destination-quadrant)) ; no space for more klingons
                 +max-klingons-per-quadrant+)
              (string= enemy-letter +romulan+)) ; Romulans cannot escape!
      (return-from try-exit nil))

    (unless running-away-p
      ;; Avoid intruding on another commander's territory
      (when (string= enemy-letter +commander+)
        (dolist (cq *commander-quadrants*)
          (when (coord-equal cq destination-quadrant)
            (return-from try-exit nil)))
        ;; Refuse to leave if currently attacking starbase
        (when (coord-equal *base-under-attack-quadrant* *ship-quadrant*)
          (return-from try-exit nil)))
      ;; Don't leave if over 1000 units of energy
      (when (> (aref *klingon-energy* enemy-index) 1000.0)
        (return-from try-exit nil)))

    ;; Print escape message and move out of quadrant
    ;; We know this if either short or long range sensors are working
    (when (or (not (damagedp +short-range-sensors+))
              (not (damagedp +long-range-sensors+))
              *dockedp*)
      (print-message (format nil "***~A at ~A escapes to ~A (and regains strength).~%"
                             (letter-to-name enemy-letter)
                             (format-sector-coordinates (aref *klingon-sectors* enemy-index))
                             (format-quadrant-coordinates destination-quadrant))))
    ;; Handle local matters related to escape
    (setf (coord-ref *quadrant-contents* (aref *klingon-sectors* enemy-index)) +empty-sector+)
    (setf (aref *klingon-sectors* enemy-index) (aref *klingon-sectors* (1- *enemies-here*)))
    (setf (aref *klingon-distance* enemy-index) (aref *klingon-distance* (1- *enemies-here*)))
    (setf (aref *klingon-average-distance* enemy-index) (aref *klingon-average-distance* (1- *enemies-here*)))
    (setf (aref *klingon-energy* enemy-index) (aref *klingon-energy* (1- *enemies-here*)))
    (setf *klingons-here* (1- *klingons-here*))
    (setf *enemies-here* (1- *enemies-here*))
    (update-condition)
    ;; Handle global matters related to escape
    (setf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))
          (1- (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*))))
    (setf (quadrant-klingons (coord-ref *galaxy* destination-quadrant))
          (1+ (quadrant-klingons (coord-ref *galaxy* destination-quadrant))))
    (if (string= enemy-letter +super-commander+)
        (progn
          (setf *super-commanders-here* 0)
          (setf *super-commander-attack-enterprise-p* nil)
          (setf *attempted-escape-from-super-commander-p* nil)
          (setf *super-commander-attacking-base* 0)
          (schedule-event +move-super-commander+ 0.2777)
          (unschedule +super-commander-destroys-base+)
          (setf *super-commander-quadrant* destination-quadrant))
        (progn
          (setf *commander-quadrants* (remove *ship-quadrant* *commander-quadrants* :test #'coord-equal))
          (setf *commander-quadrants* (nconc *commander-quadrants* destination-quadrant))
          (setf *commanders-here* (1- *commanders-here*)))))
  (return-from try-exit t))

(defun move-one-enemy (enemy-sector enemy-index enemy-letter) ; C: void movebaddy(coord com, int loccom, feature ienm)
  "Tactical movement for one enemy.

The bad-guy movement algorithm:

1. Enterprise has \"force\" based on condition of phaser and photon torpedoes.
If both are operating full strength, force is 1000. If both are damaged,
force is -1000. Having shields down subtracts an additional 1000.

2. Enemy has forces equal to the energy of the attacker plus
100*(K+R) + 500*(C+S) - 400 for novice through good levels OR
346*K + 400*R + 500*(C+S) - 400 for expert and emeritus.

Attacker Initial energy levels (nominal):
        Klingon    Romulan    Commander   Super-Commander
Novice    400        700        1200
Fair      425        750        1250
Good      450        800        1300        1750
Expert    475        850        1350        1875
Emeritus  500        900        1400        2000
VARIANCE   75        200         200         200

Enemy vessels only move prior to their attack. In Novice - Good games
only commanders move. In Expert games, all enemy vessels move if there
is a commander present. In Emeritus games all enemy vessels move.

3. If Enterprise is not docked, an agressive action is taken if enemy
forces are 1000 greater than Enterprise.

Agressive action on average cuts the distance between the ship and
the enemy to 1/4 the original.

4.  At lower energy advantage, movement units are proportional to the
advantage with a 650 advantage being to hold ground, 800 to move forward
1, 950 for two, 150 for back 4, etc. Variance of 100.

If docked, is reduced by roughly 1.75*game.skill, generally forcing a
retreat, especially at high skill levels.

5.  Motion is limited to skill level, except for SC hi-tailing it out."

  (let (enemy-distance ; C: dist1
        (run-away nil) ; C: irun
        (motion 0) ; C: motion
        next-sector) ; C: next
    (setf enemy-distance (aref *klingon-distance* enemy-index))
    (let (forces) ; C: forces
      ;; If SC, check with spy to see if should hi-tail it
      (if (and (string= enemy-letter +super-commander+)
               (or (<= (aref *klingon-energy* enemy-index) 500.0)
                   (and *dockedp*
                        (not (damagedp +photon-torpedoes+)))))
          (progn
            (setf run-away t)
            (setf motion (- +quadrant-size+)))
          (progn
            ;; Decide whether to advance, retreat, or hold position
            (let (enemy-multiplier) ; C: nbaddys
              ;; This should probably be just game.comhere + game.ishere
              (if (>= (skill-level-value *skill-level*) +expert+)
                  (setf enemy-multiplier (truncate (/ (+ (* *commanders-here* 2)
                                                         (* *super-commanders-here* 2)
                                                         (* *klingons-here* 1.23)
                                                         (* *romulans-here* 1.5))
                                                      2.0)))
                  (setf enemy-multiplier (+ *commanders-here* *super-commanders-here*)))
              (setf forces (+ (aref *klingon-energy* enemy-index)
                              (* *enemies-here* 100.0)
                              (* (1- enemy-multiplier) 400))))
            (unless *shields-are-up-p*
              (setf forces (+ forces 1000))) ; Good for enemy if shield is down!
            (if (or (not (damagedp +phaser+))
                    (not (damagedp +photon-torpedoes+)))
                (progn
                  (if (damagedp +phaser+) ; phasers damaged
                      (setf forces (+ forces 300.0))
                      (setf forces (- forces (* (- *ship-energy* 2500.0) 2.0))))
                  (if (damagedp +photon-torpedoes+) ; photon torpedoes damaged
                      (setf forces (+ forces 300.0))
                      (setf forces (- forces (* *torpedoes* 50.0)))))
                ;; Phasers and photon tubes both out!
                (setf forces (+ forces 1000)))
            (if (and (<= forces 1000.0)
                     (not *dockedp*)) ; Typical situation
                (setf motion (- (/ (+ forces (* 200.0 (random 1.0))) 150.0) 5.0))
                (progn
                  (when (> forces 1000.0) ; Very strong -- move in for kill
                    (setf motion (+ (* (- 1.0 (expt (random 1.0) 2)) enemy-distance) 1.0)))
                  (when *dockedp* ; Protected by base -- back off!
                    (setf motion (- motion (* (skill-level-value *skill-level*)
                                              (- 2.0 (expt (random 1.0) 2))))))))
            ;; Don't move if no motion
            (when (= motion 0)
              (return-from move-one-enemy nil))
            ;; Limit motion according to skill
            (when (> (abs motion) (skill-level-value *skill-level*))
              (if (< motion 0)
                  (setf motion (- (skill-level-value *skill-level*)))
                  (setf motion (skill-level-value *skill-level*)))))))
    ;; Calculate preferred number of steps
    (let (number-of-steps ; C: nsteps
          maximum-distance ; C: mdist, Nearest integer distance
          delta-x ; C: mx
          delta-y) ; C: my
      (setf number-of-steps (if (< motion 0) (- motion) motion))
      (setf maximum-distance (truncate (+ enemy-distance 0.5)))
      (when (and (> motion 0)
                 (> number-of-steps maximum-distance))
        (setf number-of-steps maximum-distance)) ; don't overshoot
      ;;(when (> number-of-steps +quadrant-size+)
      ;;  (setf number-of-steps +quadrant-size+)) ; This shouldn't be necessary, and SBCL thinks it's impossible
      (when (< number-of-steps 1)
        (setf number-of-steps 1)) ; This shouldn't be necessary
      ;; Compute preferred values of delta X and Y
      (setf delta-x (- (coordinate-x *ship-sector*) (coordinate-x enemy-sector)))
      (setf delta-y (- (coordinate-y *ship-sector*) (coordinate-y enemy-sector)))
      (when (< (* (abs delta-x) 2.0) (abs delta-y))
        (setf delta-x 0))
      (when (< (* (abs delta-y) 2.0) (abs (- (coordinate-x *ship-sector*) (coordinate-x enemy-sector))))
        (setf delta-y 0))
      (when (/= delta-x 0)
        (setf delta-x (if (< (* delta-x motion) 0) -1 1)))
      (when (/= delta-y 0)
        (setf delta-y (if (< (* delta-y motion) 0) -1 1)))
      (setf next-sector enemy-sector)
      ;; Main move loop
      (do ((loop-counter 0 (1+ loop-counter)) ; C: ll
           (look (make-coordinate)); C: look
           crawl-x crawl-y ; C: krawlx, krawly
           success ; C: success
           (end-move-loop-p nil))
          ((or end-move-loop-p
               (>= loop-counter number-of-steps)))
        ;; Check if preferred position available
        (setf (coordinate-x look) (+ (coordinate-x next-sector) delta-x))
        (setf (coordinate-y look) (+ (coordinate-y next-sector) delta-y))
        (setf crawl-x (if (< delta-x 0) 1 -1))
        (setf crawl-y (if (< delta-y 0) 1 -1))
        (setf success nil)
        (do ((attempts 0 (1+ attempts)) ; Settle mysterious hang problem
             (end-attempt-p nil))
            ((or (>= attempts 20)
                 success
                 end-attempt-p))
          (cond
            ((or (< (coordinate-x look) 1)
                 (> (coordinate-x look) +quadrant-size+))
             (when (and (< motion 0)
                        (try-exit look enemy-letter enemy-index run-away))
               (return-from move-one-enemy t))
             (if (or (= crawl-x delta-x)
                     (= delta-y 0))
                 (setf end-attempt-p t)
                 (progn
                   (setf (coordinate-x look) (+ (coordinate-x next-sector) crawl-x))
                   (setf crawl-x (- crawl-x)))))

            ((or (< (coordinate-y look) 1)
                 (> (coordinate-y look) +quadrant-size+))
             (when (and (< motion 0)
                        (try-exit look enemy-letter enemy-index run-away))
               (return-from move-one-enemy t))
             (if (or (= crawl-y delta-y)
                     (= delta-x 0))
                 (setf end-attempt-p t)
                 (progn
                   (setf (coordinate-y look) (+ (coordinate-y next-sector) crawl-y))
                   (setf crawl-y (- crawl-y)))))

            ((string/= (coord-ref *quadrant-contents* look) +empty-sector+)
             ;; See if we should ram ship
             (when (and (string= (coord-ref *quadrant-contents* look) *ship*)
                        (or (string= enemy-letter +commander+)
                            (string= enemy-letter +super-commander+)))
               (ram :rammed-by-p t :enemy enemy-letter :enemy-coordinates enemy-sector)
               (return-from move-one-enemy t))
             (cond
               ((and (/= crawl-x delta-x)
                     (/= delta-y 0))
                (setf (coordinate-x look) (+ (coordinate-x next-sector) crawl-x))
                (setf crawl-x (- crawl-x)))

               ((and (/= crawl-y delta-y)
                     (/= delta-x 0))
                (setf (coordinate-y look) (+ (coordinate-y next-sector) crawl-y))
                (setf crawl-y (- crawl-y)))

               (t
                (setf end-attempt-p t)))) ; We have failed

            (t
             (setf success t))))
        (if success
            (setf next-sector look)
            (setf end-move-loop-p t)))) ; Done early
    ;; Put commander in place within same quadrant
    (setf (coord-ref *quadrant-contents* enemy-sector) +empty-sector+)
    (setf (coord-ref *quadrant-contents* next-sector) enemy-letter)
    (unless (coord-equal next-sector enemy-sector)
      ;; It moved
      (setf (aref *klingon-sectors* enemy-index) next-sector)
      (setf (aref *klingon-distance* enemy-index) (distance *ship-sector* next-sector))
      (setf (aref *klingon-average-distance* enemy-index) (distance *ship-sector* next-sector))
      (when (or (not (damagedp +short-range-sensors+))
                *dockedp*)
        (print-message (format nil "***~A from ~A ~A to ~A~%"
                               (letter-to-name enemy-letter)
                               (format-sector-coordinates enemy-sector)
                               (if (< (aref *klingon-distance* enemy-index) enemy-distance) "advances" "retreats")
                               (format-sector-coordinates next-sector)))))))

(defun move-enemies () ; C: void moveklings(void)
  "Klingon and Romulan tactical movement, that is, movement within the current quadrant."

  ;; TODO - could/should these (when) expressions be replaced with a macro?
  ;; Figure out which Klingon is the commander (or Supercommander) and do the move
  ;; TODO - this loop assumes only one commander per quadrant, is that always true?
  (when (> *commanders-here* 0)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant-contents* enemy-sector) +commander+))
         (when (string= (coord-ref *quadrant-contents* enemy-sector) +commander+)
           (move-one-enemy enemy-sector i +commander+)))
      (setf enemy-sector (aref *klingon-sectors* i))))
  (when (> *super-commanders-here* 0)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant-contents* enemy-sector) +super-commander+))
         (when (string= (coord-ref *quadrant-contents* enemy-sector) +super-commander+)
           (move-one-enemy enemy-sector i +super-commander+)))
      (setf enemy-sector (aref *klingon-sectors* i))))
  ;; If skill level is high, move other Klingons and Romulans too!
  ;; Move these last so they can base their actions on what the commander(s) do.
  (when (>= (skill-level-value *skill-level*) +expert+)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (or (string= (coord-ref *quadrant-contents* enemy-sector) +klingon+)
                   (string= (coord-ref *quadrant-contents* enemy-sector) +romulan+)))
         (when (or (string= (coord-ref *quadrant-contents* enemy-sector) +klingon+)
                   (string= (coord-ref *quadrant-contents* enemy-sector) +romulan+))
           (move-one-enemy enemy-sector i (coord-ref *quadrant-contents* enemy-sector))))
      (setf enemy-sector (aref *klingon-sectors* i))))
  (sort-klingons))

(defun attack-player (&key (torpedoes-ok-p nil)) ; C: attack(bool torps_ok)
  "Enemies in quadrant attack the player. If torpedoes are not allowed (the default) then enemies
attack with phasers only. Torpedoes are allowed when it's the regular full turn for enemies after
the player completes their turn."

  ;; Message verbosity: if the skill level is Fair or lower then print the word Sector
  ;; when displaying coordinates, otherwise print only the bare coordinates.

  (when (and *cloakedp*
             (not *cloakingp*))
    (return-from attack-player nil)) ; Nothing happens if we are cloaked

  (unless *all-done-p* ; The game could be over at this point, check
    (when (> *tholians-here* 0) ; Tholian gets to move before attacking
      (move-tholian))
    ;; If you have just entered the RNZ, you'll get a warning
    (if *romulan-neutral-zone-p* ; The one chance not to be attacked
        (setf *romulan-neutral-zone-p* nil)
        (progn
          ;; Commanders get a chance to tac-move towards you
          (when (and (or (and (or (> *commanders-here* 0)
                                  (> *super-commanders-here* 0))
                              (not *just-in-p*))
                         (= (skill-level-value *skill-level*) +emeritus+))
                     torpedoes-ok-p)
            (move-enemies))
          ;; If no enemies remain after movement, we're done
          (unless (or (<= *enemies-here* 0)
                      (and (= *enemies-here* 1)
                           (>= *things-here* 1)
                           (not *thing-is-angry-p*)))
            (skip-line)
            (do ((n 0 (1+ n))
                 (weapon 'torpedo) ; C: usephasers
                 (torpedo-probability (random 1.0)) ; C: r
                 enemy-sector ; C: jay
                 enemy ; C: iquad
                 (hit 0.0) ; C: hit - how much damage is directed at the player ship
                 (attack-attempted-p nil) ; C: attempt
                 (dust-factor (+ 0.8 (* 0.05 (random 1.0))) ; amount by which delivered power is reduced over distance
                              (+ 0.8 (* 0.05 (random 1.0)))) ; different for each enemy
                 course ; C: course
                 random-variation ; C: r, in C, the same storage as r above, used for two purposes
                 (damage-taken-p nil) ; C: ihurt
                 (hit-max 0.0) ; C: hitmax
                 (hit-total 0.0)) ; C: hittot
                ((>= n *enemies-here*)
                 (when (<= *ship-energy* 0)
                   (finish +battle+) ; Returning home upon your shield, not with it...
                   (return-from attack-player nil))
                 (when (not attack-attempted-p)
                   (print-message (format nil "***Enemies decide against attacking your ship.~%")))
                 (when attack-attempted-p
                   (if damage-taken-p
                       ;; Print message if starship suffered hit(s)
                       (progn
                         (skip-line)
                         (print-message (format nil "Energy left ~D    shields " (truncate *ship-energy*)))
                         (cond
                           (*shields-are-up-p*
                            (print-message "up "))
                           ((not (damagedp +shields+))
                            (print-message "down "))
                           (t
                            (print-message "damaged, "))))
                       ;; Shields fully protect ship
                       (print-message "Enemy attack reduces shield strength to "))
                   (print-message (format nil "~D%,   torpedoes left ~D~%"
                                      (* 100.0 (/ *shield-energy* *initial-shield-energy*))
                                      *torpedoes*))
                   ;; Check if anyone was hurt
                   (when (or (>= hit-max 200)
                             (>= hit-total 500))
                     (let ((casualties (truncate(* hit-total (random 1.0) 0.015)))) ; C: icas
                       (when (>= casualties 2)
                         (skip-line)
                         (print-message (format nil "Mc Coy-  \"Sickbay to bridge.  We suffered ~D casualties~%" casualties))
                         (print-message (format nil "   in that last attack.\"~%"))
                         (setf *casualties* (+ *casualties* casualties))
                         (setf *crew* (- *crew* casualties)))))))
              (setf enemy-sector (aref *klingon-sectors* n))
              (setf enemy (coord-ref *quadrant-contents* enemy-sector))
              (unless (or (< (aref *klingon-energy* n) 0) ; too weak to attack -- TODO should be <= ?
                          (string= enemy +tholian+)
                          (and (string= enemy +thing+)
                               (not *thing-is-angry-p*)))
                ;; Compute hit strength and diminish shield power
                ;; Increase chance of photon torpedos if docked or enemy energy low
                (when *dockedp*
                  (setf torpedo-probability (* torpedo-probability 0.25)))
                (when (< (aref *klingon-energy* n) 500)
                  (setf torpedo-probability (* torpedo-probability 0.25)))
                ;; Different enemies have different probabilities of throwing a torp
                (when (or (not torpedoes-ok-p)
                          (and (string= enemy +klingon+)
                               (> torpedo-probability 0.0005))
                          (and (string= enemy +commander+)
                               (> torpedo-probability 0.015))
                          (and (string= enemy +romulan+)
                               (> torpedo-probability 0.3))
                          (and (string= enemy +super-commander+)
                               (> torpedo-probability 0.07))
                          (and (string= enemy +thing+)
                               (> torpedo-probability 0.05)))
                  (setf weapon 'phasers))
                (if (eql weapon 'phasers)
                    (progn  ; Enemy uses phasers
                      (unless *dockedp* ; Don't waste the effort!
                        (setf attack-attempted-p t)
                        (setf hit (* (aref *klingon-energy* n) (expt dust-factor (aref *klingon-average-distance* n))))
                        (setf (aref *klingon-energy* n) (* (aref *klingon-energy* n) 0.75))))
                    (progn ; Enemy uses photon torpedo
                      (setf attack-attempted-p t)
                      (setf course (* 1.90985 (atan (- (coordinate-y *ship-sector*) (coordinate-y enemy-sector))
                                                    (- (coordinate-x enemy-sector) (coordinate-x *ship-sector*)))))
                      (print-message "***TORPEDO INCOMING")
                      (unless (damagedp +short-range-sensors+)
                        (if (<= (skill-level-value *skill-level*) +fair+)
                            (print-message (format nil " From ~A at ~A  ~%"
                                                   (letter-to-name enemy)
                                                   (format-sector-coordinates enemy-sector)))
                            (print-message (format nil " From ~A at ~A  ~%"
                                                   (letter-to-name enemy)
                                                   (format-coordinates enemy-sector)))))
                      (setf random-variation (- (* (+ (random 1.0) (random 1.0)) 0.5) 0.5))
                      (setf random-variation (+ random-variation (* 0.002
                                                                    (aref *klingon-energy* n)
                                                                    random-variation)))
                      (setf hit (move-torpedo-within-quadrant course random-variation enemy-sector 1 1))
                      (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
                        (finish +won+)) ; Klingons did themselves in!
                      (when (or (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
                                *all-done-p*)
                        (return-from attack-player nil))))
                (unless (= hit 0)
                  ;; Incoming phaser or torpedo, shields may dissipate it
                  (when (or *shields-are-up-p*
                            *shields-are-changing-p*
                            *dockedp*)
                    (let (absorb ; C: absorb
                          hit-to-shields ; C: hitsh
                          proportional-strength ; C: propor
                          (shield-change-factor 1.0)) ; C: chgfac
                      ;; Set up partial hits if attack happens during shield status change
                      (when *shields-are-changing-p*
                        (setf shield-change-factor (+ 0.25 (* 0.5 (random 1.0)))))
                      ;; Shields will take hits
                      (setf proportional-strength (* (/ *shield-energy* *initial-shield-energy*)
                                                     (if *dockedp* 2.1 1.0)))
                      (when (< proportional-strength 0.1)
                        (setf proportional-strength 0.1))
                      (setf hit-to-shields (+ (* proportional-strength shield-change-factor hit) 1.0))
                      (setf absorb (* 0.8 hit-to-shields))
                      (when (> absorb *shield-energy*)
                        (setf absorb *shield-energy*))
                      (setf *shield-energy* (- *shield-energy* absorb))
                      (setf hit (- hit hit-to-shields))
                      ;; Tiny hits have no effect if the shields were strong enough
                      (when (and (> proportional-strength 0.1)
                                 (< hit (* 0.005 *ship-energy*)))
                        (setf hit 0))))
                  ;; Hit from this opponent got through shields, so take damage
                  (when (> hit 0)
                    (setf damage-taken-p t)
                    (print-message (format nil "~D unit hit" (truncate hit)))
                    (when (or (and (damagedp +short-range-sensors+)
                                   (eql weapon 'phasers))
                              (<= (skill-level-value *skill-level*) +fair+))
                      (print-message (format nil " on the ~A" (format-ship-name))))
                    (when (and (not (damagedp +short-range-sensors+))
                               (eql weapon 'phasers))
                      (if (<= (skill-level-value *skill-level*) +fair+)
                          (print-message (format nil " from ~A at ~A" (letter-to-name enemy)
                                                 (format-sector-coordinates enemy-sector)))
                          (print-message (format nil " from ~A at ~A~%" (letter-to-name enemy)
                                                 (format-coordinates enemy-sector)))))
                    ;; Decide if hit is critical
                    (when (> hit hit-max)
                      (setf hit-max hit))
                    (setf hit-total (+ hit hit-total))
                    (apply-critical-hit hit)
                    (setf *ship-energy* (- *ship-energy* hit))))))
            ;; After attack, reset average distance to enemies
            (do ((n 0 (1+ n)))
                ((>= n *enemies-here*))
              (setf (aref *klingon-average-distance* n) (aref *klingon-distance* n)))
            (sort-klingons))))))

;; TODO - Error: the ship doesn't always leave the quadrant, and no E appears in the short range scan
;; seems to occur when moving at warp 10 over a distance of 3 or so quadrants. The error was seen when
;; testing the time-warp code, by moving a few quadrants at warp 10.
(defun move-ship-within-quadrant (&key course distance (nova-push-p nil)) ; C: imove(bool novapush)
  "In-sector movement actions for warp and impulse drives. Supernova and tractor beam events
can occur."

  (when *in-orbit-p*
    (print-message (format nil "Helmsman Sulu- \"Leaving standard orbit.\"~%"))
    (setf *in-orbit-p* nil))

  (setf *dockedp* nil)

  ;; TODO - n probably isn't needed: stop movement when full movement distance has been covered
  (let ((tractor-beam-scheduled-p nil)
        angle delta-x delta-y bigger
        n
        (s-coord (make-coordinate :x 0 :y 0))) ; C: w

    (when (and *cloakedp*
               (is-scheduled-p +tractor-beam+)
               (>= (+ *stardate* *time-taken-by-current-operation*) (scheduled-for +tractor-beam+)))
      ;; We can't be tractor beamed if cloaked, so move the event into the future
      (schedule-event +tractor-beam+ (+ *time-taken-by-current-operation*
                                        (expran (* 1.5 (/ *initial-stardate* (length *commander-quadrants*)))))))

    ;; If tractor beam is to occur, don't move full distance
    (when (and (is-scheduled-p +tractor-beam+)
               (>= (+ *stardate* *time-taken-by-current-operation*) (scheduled-for +tractor-beam+)))
      (setf tractor-beam-scheduled-p t)
      (setf *condition* +red-status+)
      (setf distance (+ (/ (* distance (- (scheduled-for +tractor-beam+) *stardate*))
                           *time-taken-by-current-operation*)
                        0.1))
      (setf *time-taken-by-current-operation* (+ (- (scheduled-for +tractor-beam+) *stardate*) (expt 1 -5))))

    ;; 30 degrees is 0.5235988 radians, 1/12 of a circle
    ;; Convert the clock direction to radians.
    ;; Shift the clock by 3 hours (12 + 3) to turn the angle 90 degrees
    (setf angle (* (- 15.0 course) 0.5235988))
    ;; The x direction of change is flipped by using -1
    (setf delta-x (- (sin angle)))
    (setf delta-y (cos angle))
    ;; Set the larger of delta-x or delta-y to 1 and proportionally adjust the size of the other value
    (if (> (abs delta-x) (abs delta-y))
        (setf bigger (abs delta-x))
        (setf bigger (abs delta-y)))
    (setf delta-x (/ delta-x bigger))
    (setf delta-y (/ delta-y bigger))
    (setf n (truncate (+ (* 10.0 distance bigger) 0.5))) ; Simulate C assignment to int
    ;; Move within the quadrant
    (setf (coord-ref *quadrant-contents* *ship-sector*) +empty-sector+)
    (do ((m 0 (1+ m))
         (movement-stopped-p nil)
         (prev-x (coordinate-x *ship-sector*))
         (prev-y (coordinate-y *ship-sector*)))
        ((or movement-stopped-p
             (>= m n)))
      (setf prev-x (+ prev-x delta-x))
      (setf prev-y (+ prev-y delta-y))
      (setf (coordinate-x s-coord) (round prev-x))
      (setf (coordinate-y s-coord) (round prev-y))
      ;; Leaving the quadrant (and this function)
      (when (not (valid-sector-p (coordinate-x s-coord) (coordinate-y s-coord)))
        ;; Allow a final enemy attack unless being pushed by a nova.
        (when (and (not nova-push-p)
                   (/= *enemies-here* 0)
                   (not *cloakedp*))
          (update-condition)
          ;; *enemies-here* is a count, start at zero to use as an array reference
          (do ((m 0 (1+ m))) ; TODO - can compute average distance be a function? Needs a sector coord input
              ((>= m *enemies-here*))
            (setf (aref *klingon-average-distance* m) (/ (+ (distance s-coord (aref *klingon-sectors* m))
                                                            (aref *klingon-distance* m))
                                                         2.0)))
          ;; Stas Sergeev added the condition that attacks only happen
          ;; if Klingons are present and your skill is good.
          (when (and (> (skill-level-value *skill-level*) +good+)
                     (> *klingons-here* 0) ; Romulans don't get another attack
                     (not (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))))
            (attack-player))
          (when *all-done-p*
            (return-from move-ship-within-quadrant t)))
        ;; Compute final position -- new quadrant and sector
        (setf prev-x (+ (* +quadrant-size+ (coordinate-x *ship-quadrant*)) (coordinate-x *ship-sector*)))
        (setf prev-y (+ (* +quadrant-size+ (coordinate-y *ship-quadrant*)) (coordinate-y *ship-sector*)))
        ;; position in units of sectors
        (setf (coordinate-x s-coord) (truncate (+ prev-x (* 10.0 distance bigger delta-x) 0.5)))
        (setf (coordinate-y s-coord) (truncate (+ prev-y (* 10.0 distance bigger delta-y) 0.5)))
        ;; Check for edge of galaxy
        (do ((energy-barrier-crossed-p nil)
             (coordinate-adjusted-p t))
            ((not coordinate-adjusted-p)
             (when energy-barrier-crossed-p
               (1+ *energy-barrier-crossings*)
               (when (>= *energy-barrier-crossings* 3) ; Three strikes -- you're out!
                 (finish +3-negative-energy-barrier-crossings+)
                 (return-from move-ship-within-quadrant t))
               (skip-line)
               (print-message (format nil "YOU HAVE ATTEMPTED TO CROSS THE NEGATIVE ENERGY BARRIER~%"))
               (print-message (format nil "AT THE EDGE OF THE GALAXY.  THE THIRD TIME YOU TRY THIS,~%"))
               (print-message (format nil "YOU WILL BE DESTROYED.~%"))))
          (setf coordinate-adjusted-p nil)
          (when (< (coordinate-x s-coord) 0)
            (setf (coordinate-x s-coord) (1+ (- (coordinate-x s-coord))))
            (setf coordinate-adjusted-p t))
          (when (< (coordinate-y s-coord) 0)
            (setf (coordinate-y s-coord) (1+ (- (coordinate-y s-coord))))
            (setf coordinate-adjusted-p t))
          (when (> (coordinate-x s-coord) (1- (* +galaxy-size+ +quadrant-size+)))
            (setf (coordinate-x s-coord) (- (1+ (* +galaxy-size+ +quadrant-size+ 2)) (coordinate-x s-coord)))
            (setf coordinate-adjusted-p t))
          (when (> (coordinate-y s-coord) (1- (* +galaxy-size+ +quadrant-size+)))
            (setf (coordinate-y s-coord) (- (1+ (* +galaxy-size+ +quadrant-size+ 2)) (coordinate-y s-coord)))
            (setf coordinate-adjusted-p t))
          (when coordinate-adjusted-p
            (setf energy-barrier-crossed-p t)))
        ;; Compute final position in new quadrant
        (when (not tractor-beam-scheduled-p) ; Tractor beam will change the quadrant
          (setf (coordinate-x *ship-quadrant*)
                (truncate (1- (/ (+ (coordinate-x s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-y *ship-quadrant*)
                (truncate (1- (/ (+ (coordinate-y s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-x *ship-sector*) (- (coordinate-x s-coord) (* +quadrant-size+ (coordinate-x *ship-quadrant*))))
          (setf (coordinate-y *ship-sector*) (- (coordinate-y s-coord) (* +quadrant-size+ (coordinate-y *ship-quadrant*))))
          (skip-line)
          (print-message (format nil "Entering ~A.~%" (format-quadrant-coordinates *ship-quadrant*)))
          (new-quadrant :show-thing nil)
          (when (> (skill-level-value *skill-level*) +novice+)
            (attack-player)))
        (return-from move-ship-within-quadrant t))
      ;; Object encountered in flight path
      (when (string/= (coord-ref *quadrant-contents* s-coord) +empty-sector+)
        (setf distance (/ (distance *ship-sector* s-coord) (* +quadrant-size+ 1.0)))
        (cond
          ((or (string= (coord-ref *quadrant-contents* s-coord) +tholian+) ; Ram a Tholian
               ;; Ram enemy ship
               (string= (coord-ref *quadrant-contents* s-coord) +klingon+)
               (string= (coord-ref *quadrant-contents* s-coord) +commander+)
               (string= (coord-ref *quadrant-contents* s-coord) +super-commander+)
               (string= (coord-ref *quadrant-contents* s-coord) +romulan+)
               (string= (coord-ref *quadrant-contents* s-coord) +thing+))
           (setf (coordinate-x *ship-sector*) (coordinate-x s-coord))
           (setf (coordinate-y *ship-sector*) (coordinate-y s-coord))
           (ram :rammed-by-p nil :enemy (coord-ref *quadrant-contents* s-coord)
                :enemy-coordinates *ship-sector*))
          ((string= (coord-ref *quadrant-contents* s-coord) +black-hole+)
           (skip-line)
           (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
           (skip-line)
           (print-message (format nil "*** ~A pulled into black hole at ~A~%"
                                  (format-ship-name) (format-sector-coordinates s-coord)))
           ;; Getting pulled into a black hole was certain death in Almy's original.
           ;; Stas Sergeev added a possibility that you'll get timewarped instead.
           (do ((damaged-devices 0)
                (index 0 (1+ index))
                probability-factor)
               ((>= index +number-of-devices+)
                (setf probability-factor (* (expt 1.4 (1- (/ (+ *ship-energy* *shield-energy*) 5000.0)))
                                            (expt 1.3 (1- (/ 1.0 (1+ damaged-devices))))))
                (if (> (random 1.0) probability-factor)
                    (time-warp)
                    (finish +destroyed-by-black-hole+))
                (return-from move-ship-within-quadrant t))
             (when (> (aref *device-damage* index) 0) ; TODO - define a "count damaged devices" function?
               (1+ damaged-devices))))
          ;; Something else
          (t
           (let ((stop-energy (/ (* 50.0 distance) *time-taken-by-current-operation*)))
             (skip-line)
             (if (string= (coord-ref *quadrant-contents* s-coord) +tholian-web+)
                 (print-message (format nil "~A encounters Tholian web at ~A;~%"
                                        (format-ship-name) (format-sector-coordinates s-coord)))
                 (print-message (format nil "~A blocked by object at ~A;~%"
                                        (format-ship-name) (format-sector-coordinates s-coord))))
             (print-message (format nil "Emergency stop required ~,2F units of energy.~%" stop-energy))
             (setf (coordinate-x s-coord) (truncate (- prev-x delta-x))) ; simulate C float to int assignment
             (setf (coordinate-y s-coord) (truncate (- prev-y delta-y)))
             (setf *ship-energy* (- *ship-energy* stop-energy))
             (when (<= *ship-energy* 0)
               (finish +out-of-energy+)
               (return-from move-ship-within-quadrant t)))))
        (setf movement-stopped-p t)))
    (setf (coordinate-x *ship-sector*) (coordinate-x s-coord))
    (setf (coordinate-y *ship-sector*) (coordinate-y s-coord))
    ;; Movement completed and no quadrant change -- compute new average enemy distances
    (setf (coord-ref *quadrant-contents* *ship-sector*) *ship*)
    (when (> *enemies-here* 0)
      (do ((m 0 (1+ m))
           (final-distance 0))
          ((>= m *enemies-here*))
        (setf final-distance (distance *ship-sector* (aref *klingon-sectors* m)))
        (setf (aref *klingon-average-distance* m) (/ (+ final-distance (aref *klingon-distance* m)) 2.0))
        (setf (aref *klingon-distance* m) final-distance))
      (sort-klingons)
      (when (not (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*)))
        (attack-player))
      (do ((m 0 (1+ m)))
          ((>= m *enemies-here*))
        (setf (aref *klingon-average-distance* m) (aref *klingon-distance* m))))
    (update-condition)
    (draw-windows)))

(defun get-probe-course-and-distance () ; C: oid getcd(bool isprobe, int akey), for the probe
  "Get course direction and distance for moving the probe.

This program originally required input in terms of a (clock) direction and distance. Somewhere in
history, it was changed to cartesian coordinates. So we need to convert.  Probably \"manual\"
input should still be done this way -- it's a real pain if the computer isn't working! Manual mode
is still confusing because it involves giving x and y motions, yet the coordinates are always
displayed y - x, where +y is downward!"

  (when *window-interface-p*
    (select-window *message-window*))

  (let ((navigation-mode nil)
        delta-y delta-x
        direction
        distance)

    (when (damagedp +navigation-system+)
      (print-message (format nil "Computer damaged; manual navigation only~%"))
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token)
        (navigation-mode)
      (when (not *line-tokens*) ; *input-item* is "move" on first loop, empty thereafter
        (print-prompt "Manual or automatic: "))
      (scan-input)
      (if (numberp *input-item*) ; No input (<enter> key only) will loop
          (progn
            ;; TODO - short form of the command doesn't assume quadrant destination
            (print-message (format nil "Manual navigation assumed.~%")) ; Per the original docs
            (setf navigation-mode 'manual))
          (when *input-item*
            (setf token (match-token *input-item* (list "manual" "automatic")))
            (if token
                (progn
                  (if (string= token "manual")
                      (setf navigation-mode 'manual)
                      (setf navigation-mode 'automatic))
                  (if *line-tokens*
                      (scan-input)
                      (setf *input-item* nil)))
                (progn
                  (huh)
                  (clear-type-ahead-buffer)
                  (return-from get-probe-course-and-distance (values nil nil)))))))

    (if (eql navigation-mode 'automatic)
        (do (sx sy qx qy)
            (*input-item*
             (setf qx (read-coordinate-number))
             (setf *input-item* nil)
             (when *line-tokens*
               (scan-input)
               (setf qy (read-coordinate-number)))
             (when (>= (length *line-tokens*) 2) ; both quadrant and sector specified
               (scan-input)
               (setf sx (read-coordinate-number))
               (scan-input)
               (setf sy (read-coordinate-number)))
             (when (and (not sx) (not sy))
               ;; Only quadrant specified -- go to center of destination quadrant
               (setf sx (+ qx 0.5))
               (setf sy (+ qy 0.5)))
             (if (and (valid-quadrant-p qx qy) (valid-sector-p sx sy))
                 (progn
                   (skip-line)
                   ;; Multiply sectors by 0.1 to scale them to the size of a quadrant
                   (setf delta-x (+ (- qy (coordinate-y *ship-quadrant*))
                                    (* 0.1 (- sy (coordinate-y *ship-sector*)))))
                   (setf delta-y (+ (- (coordinate-x *ship-quadrant*) qx)
                                    (* 0.1 (- (coordinate-x *ship-sector*) sx)))))
                 (progn
                   (huh)
                   (return-from get-probe-course-and-distance (values nil nil)))))
          (print-prompt "Target quadrant or quadrant&sector: ")
          (clear-type-ahead-buffer)
          (scan-input))
        (progn ; manual
          (do (nothing)
              (*input-item*
               (when (numberp *input-item*)
                 (setf delta-x *input-item*))
               (when *line-tokens*
                 (scan-input)
                 (when (numberp *input-item*)
                   (setf delta-y *input-item*)))
               (when (or (not delta-x) (not delta-y))
                 (huh)
                 (return-from get-probe-course-and-distance (values nil nil))))
            (setf nothing nothing)
            (print-prompt "X and Y displacements: ")
            (clear-type-ahead-buffer)
            (scan-input))))
    ;; Check for zero movement
    (when (and (= delta-x 0) (= delta-y 0))
      (clear-type-ahead-buffer)
      (return-from get-probe-course-and-distance (values nil nil)))
    (setf distance (sqrt (+ (expt delta-x 2) (expt delta-y 2))))
    (setf direction (* (atan delta-x delta-y) 1.90985932))
    (when (< direction 0.0)
      (setf direction (+ direction 12.0)))
    (clear-type-ahead-buffer)
    (return-from get-probe-course-and-distance (values direction distance))))

;; TODO - the outputs of this function are movement direction and distance, either with values or
;;        nil. Direction of -1 also means "bad value entered" or "cancel"
;; TODO - use only course and distance method for the Faerie Queene, it's an older ship with older technology
(defun get-ship-course-and-distance () ; C: oid getcd(bool isprobe, int akey), but only for the ship
  "Get course direction and distance for moving the ship.

This program originally required input in terms of a (clock) direction and distance. Somewhere in
history, it was changed to cartesian coordinates. So we need to convert.  Probably \"manual\"
input should still be done this way -- it's a real pain if the computer isn't working! Manual mode
is still confusing because it involves giving x and y motions, yet the coordinates are always
displayed y - x, where +y is downward!"

  ;; If user types bad values, return with course = -1.0.
  (when *window-interface-p*
    (select-window *message-window*))

  (when *landedp*
    (print-message (format nil "Captain! You can't leave standard orbit until you~%"))
    (print-message (format nil "are back aboard the ship.~%"))
    (return-from get-ship-course-and-distance (values -1.0 0)))

  (let ((navigation-mode nil)
        (need-prompt-p nil)
        (feedback-from 'chekov) ; Who acknowledges, and how, depends on movement mode and whether or not the player needed a prompt. It seems confusing.
        delta-y delta-x)

    (when (damagedp +navigation-system+)
      (print-message (format nil "Computer damaged; manual movement only~%"))
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token)
        (navigation-mode)
      (when (not *line-tokens*) ; *input-item* is "move" on first loop, empty thereafter
        (print-prompt "Manual or automatic: ")
        (setf need-prompt-p t))
      (scan-input)
      (if (numberp *input-item*) ; No input (<enter> key only) will loop
          (progn
            (print-message (format nil "Manual movement assumed.~%")) ; Per the original docs
            (setf navigation-mode 'manual))
          (when *input-item*
            (setf token (match-token *input-item* (list "manual" "automatic")))
            (if token
                (progn
                  (if (string= token "manual")
                      (setf navigation-mode 'manual)
                      (setf navigation-mode 'automatic))
                  (if *line-tokens*
                      (scan-input)
                      (setf *input-item* nil)))
                (progn
                  (huh)
                  (return-from get-ship-course-and-distance (values -1.0 0)))))))

    (if (eql navigation-mode 'automatic)
        (do (sx sy qx qy)
            (*input-item*
             (setf sx (read-coordinate-number))
             (setf *input-item* nil)
             (when *line-tokens*
               (scan-input)
               (setf sy (read-coordinate-number)))
             (when (>= (length *line-tokens*) 2) ; both quadrant and sector specified
               (setf qx sx)
               (setf qy sy)
               (scan-input)
               (setf sx (read-coordinate-number))
               (scan-input)
               (setf sy (read-coordinate-number)))
             (when (and (not qy) (not qx))
               (setf qx (coordinate-x *ship-quadrant*))
               (setf qy (coordinate-y *ship-quadrant*))
               (setf feedback-from 'sulu))
             (if (and (valid-quadrant-p qx qy) (valid-sector-p sx sy))
                 (progn
                   (if (eql feedback-from 'chekov)
                       (print-message (format nil "Ensign Chekov- \"Course laid in, Captain.\"~%"))
                       (when need-prompt-p
                         ;; Displayed coordinates are integers, use truncate
                         (print-message (format nil "Helmsman Sulu- \"Course locked in for ~A.\"~%"
                                                (format-sector-coordinates (make-coordinate :y (truncate sx)
                                                                                            :x (truncate sy)))))
                         (setf feedback-from 'nobody)))
                   ;; Multiply sectors by 0.1 to scale them to the size of a quadrant
                   (setf delta-x (+ (- qy (coordinate-y *ship-quadrant*))
                                    (* 0.1 (- sy (coordinate-y *ship-sector*)))))
                   (setf delta-y (+ (- (coordinate-x *ship-quadrant*) qx)
                                    (* 0.1 (- (coordinate-x *ship-sector*) sx)))))
                 (progn
                   (huh)
                   (return-from get-ship-course-and-distance (values -1.0 0)))))
          (print-prompt "Destination sector or quadrant&sector: ")
          (clear-type-ahead-buffer)
          (setf need-prompt-p t)
          (scan-input))
        (progn ; manual
          (do ()
              (*input-item*
               (when (numberp *input-item*)
                 (setf delta-x *input-item*))
               (when *line-tokens*
                 (scan-input)
                 (when (numberp *input-item*)
                   (setf delta-y *input-item*)))
               (when (or (not delta-x) (not delta-y))
                 (huh)
                 (return-from get-ship-course-and-distance (values -1.0 0)))
               (setf feedback-from 'sulu))
            (print-prompt "X and Y displacements: ")
            (clear-type-ahead-buffer)
            (scan-input))))
    ;; Check for zero movement
    (when (and (= delta-x 0) (= delta-y 0))
      (return-from get-ship-course-and-distance (values -1.0 0)))
    (when (eql feedback-from 'sulu)
      (print-message (format nil "Helmsman Sulu- \"Aye, Sir.\"~%")))
    (let ((course (* (atan delta-x delta-y) 1.90985932))
          (distance (sqrt (+ (expt delta-x 2) (expt delta-y 2)))))
      (when (< course 0.0)
        (setf course (+ course 12.0)))
    (return-from get-ship-course-and-distance (values course distance)))))

(defun move-under-impulse-power () ; C: impulse(void)

  (when (damagedp +impluse-engines+)
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message (format nil "Engineer Scott- \"The impulse engines are damaged, Sir.\"~%"))
    (return-from move-under-impulse-power nil))

  (multiple-value-bind (course distance) (get-ship-course-and-distance)
    (when (= course -1.0) ; TODO test this
      (return-from move-under-impulse-power nil))

    (when (or (>= (+ 20.0 (* 100.0 distance)) *ship-energy*)
              (<= *ship-energy* 30.0))
      ;; Insufficient power for trip
      (skip-line)
      (print-message (format nil "First Officer Spock- \"Captain, the impulse engines~%"))
      (print-message (format nil "require 20.0 units to engage, plus 100.0 units per~%"))
      (if (> *ship-energy* 30.0)
          (print-message (format nil "quadrant.  We can go, therefore, a maximum of ~A quadrants.\"~%"
                                 (truncate (- (* 0.01 (- *ship-energy* 20.0)) 0.05))))
            (print-message (format nil "quadrant.  They are, therefore, useless.\"~%")))
      (clear-type-ahead-buffer)
      (return-from move-under-impulse-power nil))

    ;; Make sure enough time is left for the trip
    (when (>= (/ distance 0.095) *remaining-time*)
      (print-message (format nil "First Officer Spock- \"Captain, our speed under impulse~%"))
      (print-message (format nil "power is only 0.95 sectors per stardate.~%"))
      (print-prompt "Are you sure we dare spend the time?\" ")
      (unless (get-y-or-n-p)
        (return-from move-under-impulse-power nil)))

    ;; Activate impulse engines and pay the cost
    ;; TODO - distance should be returned, the move-ship function changes it if the ship is tractor-beamed
    (move-ship-within-quadrant :course course :distance distance)
    (setf *action-taken-p* t)
    (when *all-done-p*
      (return-from move-under-impulse-power nil))
    (setf *ship-energy* (- *ship-energy* (+ 20.0 (* 100.0 distance))))
    (setf *time-taken-by-current-operation* (/ distance 0.095)))

  (when (<= *ship-energy* 0)
    (finish +out-of-energy+)))

(defun execute-warp-move (course distance) ; C: part of warp(bool timewarp)
  "Carry out warp movement that was set up by player command or emergency override (fast exit from
quadrant experiencing a supernova)."

  (let ((time-warp-p nil)
        (engine-damage-p nil))
    (when (> *warp-factor* 6.0)
      ;; Decide if engine damage will occur
      (when (> (/ (* distance (expt (- 6.0 *warp-factor*) 2)) 66.666666666)
               (random 1.0))
        (setf engine-damage-p t)
        (setf distance (* distance (random 1.0))))
      ;; Decide if time warp will occur
      (when (> (* 0.5 distance (expt 7.0 (- *warp-factor* 10.0)) (random 1.0)))
        (setf time-warp-p t))
      ;; If time warp or engine damage, check path. If it is obstructed, don't do warp or damage.
      (when (or engine-damage-p time-warp-p)
        (let ((angle (* (- 15.0 course) 0.5235998))
              delta-x delta-y bigger)
          (setf delta-x (- (sin angle)))
          (setf delta-y (cos angle))
          (if (> (abs delta-x) (abs delta-y))
              (setf bigger (abs delta-x))
              (setf bigger (abs delta-y)))
          (setf delta-x (/ delta-x bigger))
          (setf delta-y (/ delta-y bigger))
          (do ((move-again-p t)
               (prev-x (coordinate-x *ship-sector*))
               (prev-y (coordinate-y *ship-sector*))
               curr-x
               curr-y)
              ((not move-again-p))
            (setf prev-x (+ prev-x delta-x))
            (setf prev-y (+ prev-y delta-y))
            (setf curr-x (round prev-x))
            (setf curr-y (round prev-y))
            (when (and (valid-sector-p curr-x curr-y)
                       (string/= (aref *quadrant-contents* curr-x curr-y) +empty-sector+))
              (setf engine-damage-p nil)
              (setf time-warp-p nil)
              (setf move-again-p nil))
            (when (or (not (valid-sector-p curr-x curr-y)) ; path is outside the quadrant, no obstruction encountered
                      (>= (sqrt (+ (expt (- curr-x (coordinate-x *ship-sector*)) 2)
                                   (expt (- curr-y (coordinate-y *ship-sector*)) 2)))
                          distance)) ; path covered the full movement distance without obstruction
              (setf move-again-p nil))))))
    ;; Activate Warp Engines and pay the cost
    (move-ship-within-quadrant :course course :distance distance)
    (when *all-done-p*
      (return-from execute-warp-move nil))
    (setf *ship-energy* (- *ship-energy* (* distance
                                            (expt *warp-factor* 3)
                                            (if *shields-are-up-p* 2 1))))
    (when (<= *ship-energy* 0)
      (finish +out-of-energy+))
    (setf *time-taken-by-current-operation* (calculate-warp-movement-time :distance distance
                                                                          :warp-factor *warp-factor*))
    (when time-warp-p
      (time-warp))
    (when engine-damage-p
      (setf (aref *device-damage* +warp-engines+) (* *damage-factor* (+ (* 3.0 (random 1.0)) 1.0)))
      (skip-line)
      (print-message (format nil "Engineering to bridge--~%"))
      (print-message (format nil "  Scott here.  The warp engines are damaged.~%"))
      (print-message (format nil "  We'll have to reduce speed to warp 4.~%")))
    (setf *action-taken-p* t)))

(defun move-under-warp-drive () ;  C: warp(bool timewarp)

  (when *cloakedp*
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message (format nil "Engineer Scott- \"The warp engines cannot be used while cloaked, Sir.\"~%"))
    (return-from move-under-warp-drive nil))

  (when (> (aref *device-damage* +warp-engines+) 10.0)
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message (format nil "Engineer Scott- \"The warp engines are damaged, Sir.\"~%"))
    (return-from move-under-warp-drive nil))

  (when (and (damagedp +warp-engines+)
             (> *warp-factor* 4.0))
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message (format nil "Engineer Scott- \"Sorry, Captain. Until this damage~%"))
    (print-message (format nil "  is repaired, I can only give you warp 4.\"~%"))
    (return-from move-under-warp-drive nil))

  ;; Read in course and distance
  (multiple-value-bind (course distance) (get-ship-course-and-distance)
    (when (= course -1.0) ; TODO test this
      (return-from move-under-warp-drive nil))

    (when *window-interface-p*
      (select-window *message-window*))
    ;; TODO - put warp power calculations into a function?
    ;; Make sure starship has enough energy for the trip
    (let ((power (* (+ distance 0.05) (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))
          (iwarp (truncate (expt (/ *ship-energy* (+ distance 0.05)) 0.333333333))))
      (when (>= power *ship-energy*)
        ;; Insufficient power for trip
        (skip-line)
        (print-message "Engineering to bridge--~%")
        (if (or (not *shields-are-up-p*)
                (> (* 0.5 power) *ship-energy*))
            (if (<= iwarp 0)
                (print-message (format nil "We can't do it, Captain. We don't have enough energy.~%"))
                (progn
                  (print-message (format nil "We don't have enough energy, but we could do it at warp ~A" iwarp))
                  (if *shields-are-up-p*
                      (print-message (format nil ", if you'll lower the shields.~%"))
                      (print-message (format nil ".~%")))))
            (print-message (format nil "We haven't the energy to go that far with the shields up.~%")))
        (return-from move-under-warp-drive nil)))

    ;;Make sure enough time is left for the trip
    (setf *time-taken-by-current-operation* (calculate-warp-movement-time :distance distance
                                                                          :warp-factor *warp-factor*))
    (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
      (skip-line)
      (print-message (format nil "First Officer Spock- \"Captain, I compute that such~%"))
      (print-message (format nil "  a trip would require approximately %~,2F~%"
                         (/ (* 100.0 *time-taken-by-current-operation*) *remaining-time*)))
      (print-message (format nil " percent of our remaining time.\"~%"))
      (skip-line)
      (print-prompt "\"Are you sure this is wise?\" ")
      (unless (get-y-or-n-p)
        (setf *time-taken-by-current-operation* 0)
        (return-from move-under-warp-drive nil)))

    (execute-warp-move course distance)))

;; TODO - make the prompted interface for this function more similar to the typed command
(defun launch-probe () ; C: probe(void)
  "Launch deep-space probe."

  ;; New code to launch a deep space probe
  (cond
    ((= *probes-available* 0)
     (clear-type-ahead-buffer)
     (skip-line)
     (if (string= *ship* +enterprise+)
         (print-message (format nil "Engineer Scott- \"We have no more deep space probes, Sir.\"~%"))
         (print-message (format nil "Ye Faerie Queene has no deep space probes.~%"))))

    ((damagedp +deep-space-probe-launcher+)
     (clear-type-ahead-buffer)
     (skip-line)
     (print-message (format nil "Engineer Scott- \"The probe launcher is damaged, Sir.\"~%")))

    ((is-scheduled-p +move-deep-space-probe+)
     (clear-type-ahead-buffer)
     (skip-line)
     (if (and (damagedp +subspace-radio+)
              (not *dockedp*))
         (progn
           (print-message (format nil "Spock-  \"Records show the previous probe has not yet~%"))
           (print-message (format nil "   reached its destination.\"~%")))
         (print-message (format nil "Uhura- \"The previous probe is still reporting data, Sir.\"~%"))))

    (t
     (when (= (length *line-tokens*) 0)
       ;; Slow mode, so let Kirk know how many probes there are left
       (print-message (format nil "~A probe~A left.~%" *probes-available* (if (= *probes-available* 1) "" "s")))       (print-prompt "Are you sure you want to fire a probe? ")
       (unless (get-y-or-n-p)
         (return-from launch-probe nil)))
     (setf *probe-is-armed-p* nil)
     (if (= (length *line-tokens*) 0)
         (progn
           (print-prompt "Arm NOVAMAX warhead? ")
           (when (get-y-or-n-p)
             (setf *probe-is-armed-p* t)))
         (progn
           (scan-input)
           (cond ((numberp *input-item*)
                  (unscan-input)) ; probably a quadrant coordinate, put it back

                 ((match-token *input-item* (list "yes"))
                  (setf *probe-is-armed-p* t))

                 ((match-token *input-item* (list "no"))
                  (setf *probe-is-armed-p* nil))

                 (t
                  (huh)
                  (return-from launch-probe nil)))))
     (multiple-value-bind (direction distance) (get-probe-course-and-distance)
       (when (and (not direction)
                  (not distance))
         (return-from launch-probe nil))
       (setf *probes-available* (1- *probes-available*))
       (let ((angle (* (- 15.0 direction) 0.5235988))
             bigger)
         (setf *probe-x-increment* (- (sin angle))) ; C: game.probeinx = -sin(angle);
         (setf *probe-y-increment* (cos angle))
         (if (> (abs *probe-x-increment*)
                (abs *probe-y-increment*))
             (setf bigger (abs *probe-x-increment*))
             (setf bigger (abs *probe-y-increment*)))
         (setf *probe-x-increment* (/ *probe-x-increment* bigger))
         (setf *probe-y-increment* (/ *probe-y-increment* bigger))
         (setf *moves-for-probe* (+ (* 10.0 distance bigger) 0.5)) ; TODO - is the half-sector needed?
         ;; We will use better packing than original
         (setf *probe-x-coord* (+ (* (coordinate-x *ship-quadrant*) +quadrant-size+)
                                  (coordinate-x *ship-sector*)))
         (setf *probe-y-coord* (+ (* (coordinate-y *ship-quadrant*) +quadrant-size+)
                                  (coordinate-y *ship-sector*)))
         (setf (coordinate-x *probe-reported-quadrant*) (coordinate-x *ship-quadrant*))
         (setf (coordinate-y *probe-reported-quadrant*) (coordinate-y *ship-quadrant*)))
       (schedule-event +move-deep-space-probe+ 0.01)) ; Time to move one sector
     (print-message (format nil "Ensign Chekov-  \"The deep space probe is launched, Captain.\"~%"))
     (setf *action-taken-p* t))))

(defun set-warp-factor () ; C: setwarp(void)
  "Change the warp factor."

  (if (> (length *line-tokens*) 0)
      (scan-input)
      (setf *input-item* nil))
  (do ()
      (*input-item*)
    (print-prompt "Warp factor: ")
    (scan-input))
  (if (numberp *input-item*)
      (progn
        (skip-line)
        (cond
          ((> (aref *device-damage* +warp-engines+) 10.0)
           (print-message (format nil "Warp engines inoperative.~%")))

          ((and (damagedp +warp-engines+) (> *input-item* 4.0))
           (print-message (format nil "Engineer Scott- \"I'm doing my best, Captain,~%"))
           (print-message (format nil "  but right now we can only go warp 4.\"~%")))

          ((> *input-item* 10.0)
           (print-message (format nil "Helmsman Sulu- \"Our top speed is warp 10, Captain.\"~%")))

          ((< *input-item* 1.0)
           (print-message (format nil "Helmsman Sulu- \"We can't go below warp 1, Captain.\"~%")))

          (t
           (cond
             ((or (<= *input-item* *warp-factor*)
                  (<= *input-item* 6.0))
              (print-message (format nil "Helmsman Sulu- \"Warp factor ~A, Captain.\"~%" (truncate *input-item*))))

             ((< *input-item* 8.00)
              (print-message (format nil "Engineer Scott- \"Aye, but our maximum safe speed is warp 6.\"~%")))

             ((= *input-item* 10.0)
              (print-message (format nil "Engineer Scott- \"Aye, Captain, we'll try it.\"~%")))

             (t
              (print-message (format nil "Engineer Scott- \"Aye, Captain, but our engines may not take it.\"~%"))))
           (setf *warp-factor* *input-item*))))
      (huh)))

(defun wait () ; C: wait(void)
  "Wait on events."

  (let (original-time
        time-to-wait)
    (setf *action-taken-p* nil)
    (unless (> (length *line-tokens*) 0)
      (print-prompt "How long? "))
    (scan-input)
    (when (not (numberp *input-item*))
      (huh)
      (return-from wait nil))
    (when (<= *input-item* 0.0)
      (return-from wait nil))
    (setf original-time *input-item*)
    (setf time-to-wait *input-item*)
    (when (or (>= *input-item* *remaining-time*)
              (> *enemies-here* 0))
      ;; TODO - have Spock calculate remaining time and ask "Are you sure this is wise?"
      ;; TODO - for number of enemies just prompt "Are you sure?", player will figure it out eventually
      (print-prompt "Are you sure? ")
      (when (string= (get-y-or-n-p) "n")
        (return-from wait nil)))
    ;; Alternate resting periods (events) with attacks
    (setf *restingp* t)
    (do (temp
         random-time)
        ;; leave if quadrant supernovas
        ((quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*)))
      (when (<= time-to-wait 0)
        (setf *restingp* nil))
      (when (not *restingp*)
        (print-message (format nil "~A stardates left.~%" (truncate *remaining-time*)))
        (return-from wait nil))
      (setf temp time-to-wait)
      (setf *time-taken-by-current-operation* time-to-wait)
      (when (> *enemies-here* 0)
        (setf random-time (+ 1.0 (random 1.0)))
        (when (< random-time temp)
          (setf temp random-time))
        (setf *time-taken-by-current-operation* temp))
      (when (< *time-taken-by-current-operation* time-to-wait)
        (attack-player))
      (when *all-done-p*
        (return-from wait nil))
      (process-events)
      (setf *action-taken-p* t)
      (when *all-done-p*
        (return-from wait nil))
      (setf time-to-wait (- time-to-wait temp))
      ;; Repair Deathray if long rest at starbase
      (when (and (>= (- original-time time-to-wait) 9.99)
                 *dockedp*)
        (setf (aref *device-damage* +death-ray+) 0.0))))
  (setf *restingp* nil)
  (setf *time-taken-by-current-operation* 0.0))

(defun self-destruct () ; C: selfdestruct(void)
  "Self-destruct maneuver."

  ;; Finish with a BANG!
  (clear-type-ahead-buffer)
  (skip-line)
  (if (damagedp +computer+)
      (print-message (format nil "Computer damaged; cannot execute destruct sequence.~%"))
      (progn
        (print-message (format nil "---WORKING---~%") :print-slowly t)
        (print-message (format nil "SELF-DESTRUCT-SEQUENCE-ACTIVATED~%") :print-slowly t)
        (print-message (format nil "   10~%") :print-slowly t)
        (print-message (format nil "       9~%") :print-slowly t)
        (print-message (format nil "          8~%") :print-slowly t)
        (print-message (format nil "             7~%") :print-slowly t)
        (print-message (format nil "                6~%") :print-slowly t)
        (skip-line)
        (print-message (format nil "ENTER-CORRECT-PASSWORD-TO-CONTINUE-~%"))
        (skip-line 2)
        (print-message (format nil "SELF-DESTRUCT-SEQUENCE-OTHERWISE-~%"))
        (skip-line 2)
        (print-message (format nil "SELF-DESTRUCT-SEQUENCE-WILL-BE-ABORTED~%"))
        (skip-line 2)
        (when *window-interface-p*
          (select-window *prompt-window*)
          (clear-window))
        (clear-type-ahead-buffer)
        (scan-input)
        (when (numberp *input-item*)
          (setf *input-item* (write-to-string *input-item*))
        (if (string= *self-destruct-password* *input-item*)
            (progn
              (print-message (format nil "PASSWORD-ACCEPTED~%") :print-slowly t)
              (print-message (format nil "                   5~%") :print-slowly t)
              (print-message (format nil "                      4~%")) :print-slowly t
              (print-message (format nil "                         3~%") :print-slowly t)
              (print-message (format nil "                            2~%") :print-slowly t)
              (print-message (format nil "                              1~%") :print-slowly t)
              (when (< (random 1.0) 0.15)
                (print-message (format nil "GOODBYE-CRUEL-WORLD~%") :print-slowly t))
              (kaboom))
            (progn
              (print-message (format nil "PASSWORD-REJECTED;~%") :print-slowly t)
              (print-message (format nil "CONTINUITY-EFFECTED~%") :print-slowly t)))))))

(defun calculate-eta () ; C: eta(void)
  "Use computer to get estimated time of arrival for a warp jump."

  (skip-line)

  (when (damagedp +computer+)
    (print-message (format nil "COMPUTER DAMAGED, USE A POCKET CALCULATOR.~%")) ;  TODO -rude!
    (return-from calculate-eta nil))

  (let (need-prompt wfl ttime twarp tpower trip-distance destination-quadrant)
    (when (< (length *line-tokens*) 2)
      (setf need-prompt t)
      (clear-type-ahead-buffer)
      (print-prompt "Destination quadrant or quadrant&sector? "))
    (multiple-value-bind (sx sy qx qy) (get-quadrant-and-sector)
      (if (and qx qy)
          (progn
            ;; The C source adds .5, why?
            (setf qx (+ qx 0.5))
            (setf qy (+ qy 0.5))
            ;; If not provided, calculate sector to be the nearest sector in the destination quadrant
            (if (and sx sy)
                (progn
                  (setf sx (+ sx 0.5))
                  (setf sy (+ sy 0.5)))
                (progn
                  (setf sx (if (> (coordinate-y *ship-quadrant*) qx) 1 +quadrant-size+))
                  (setf sy (if (> (coordinate-x *ship-quadrant*) qy) 1 +quadrant-size+))))
            ;; The C source adds 0.1, why?
            (setf trip-distance
                  (sqrt (+ (expt (* (+ (- qy (coordinate-y *ship-quadrant*)) 0.1)
                                    (- sy (coordinate-y *ship-sector*)))
                                 2)
                           (expt (* (+ (- qx (coordinate-x *ship-quadrant*)) 0.1)
                                    (- sx (coordinate-x *ship-sector*)))
                                 2))))
            (setf destination-quadrant (make-coordinate :x qx :y qy)))
          (return-from calculate-eta nil)))

    (when need-prompt
      (print-message (format nil "Answer \"no\" if you don't know the value:~%")))
    (do ()
        ((or ttime twarp))
      (clear-type-ahead-buffer)
      (print-prompt "Time or arrival date? ")
      (scan-input)
      (when (numberp *input-item*)
        (setf ttime *input-item*)
        (when (> ttime *stardate*)
          (setf ttime (- ttime *stardate*))) ; Actually a star date
        (setf twarp (/ (+ (floor (* (sqrt (/ (* 10.0 trip-distance) ttime)) 10.0)) 1.0) 10.0))
        (when (or (<= ttime 1e-10) ; TODO - name the constant
                  (> twarp 10))
          (print-message (format nil "We'll never make it, sir.~%"))
          (clear-type-ahead-buffer)
          (return-from calculate-eta nil))
        (when (< twarp 1.0)
          (setf twarp 1.0)))
      (unless ttime
        (clear-type-ahead-buffer)
        (print-prompt "Warp factor? ")
        (scan-input)
        (when (numberp *input-item*)
          (setf wfl t)
          (setf twarp *input-item*)
          (when (or (< twarp 1.0) (> twarp 10.0))
            (huh)
            (return-from calculate-eta nil))))
      (unless (or ttime twarp)
        (print-message (format nil "Captain, certainly you can give me one of these.~%"))))

    (skip-line)

    (do ((try-another-warp-factor-p t))
        ((not try-another-warp-factor-p))
      (setf try-another-warp-factor-p nil)
      (clear-type-ahead-buffer)
      (setf ttime (/ (* 10.0 trip-distance) (expt twarp 2)))
      (setf tpower (* trip-distance (expt twarp 3) (if *shields-are-up-p* 2 1)))
      (if (>= tpower *ship-energy*)
          (progn
            (print-message (format nil "Insufficient energy, sir."))
            (when (or (not *shields-are-up-p*)
                      (> tpower (* *ship-energy* 2.0)))
              (unless wfl
                (return-from calculate-eta nil))
              (skip-line)
              (print-prompt "New warp factor to try? ")
              (scan-input)
              (if (numberp *input-item*)
                  (if (and (>= twarp 1.0)
                           (<= twarp 10.0))
                      (progn
                        (setf wfl t)
                        (setf try-another-warp-factor-p t)
                        (setf twarp *input-item*))
                      (progn
                        (huh)
                        (return-from calculate-eta nil)))
                  (progn
                    (clear-type-ahead-buffer)
                    (return-from calculate-eta nil))))
            (unless try-another-warp-factor-p
              (print-message (format nil "But if you lower your shields,~%"))
              (print-message "remaining")
              (setf tpower (/ tpower 2))))
          (progn
            (when *window-interface-p*
              (select-window *message-window*))
            (print-message "Remaining")))
      (unless try-another-warp-factor-p
        (print-message (format nil " energy will be ~,2F.~%" (- *ship-energy* tpower)))
        (cond
          (wfl
           (print-message (format nil "And we will arrive at stardate ~A.~%" (format-stardate (+ *stardate* ttime)))))
          ((= twarp 1.0)
           (print-message (format nil "Any warp speed is adequate.~%")))
          (t
           (print-message (format nil "Minimum warp needed is ~,2F,~%" twarp))
           (print-message (format nil "and we will arrive at stardate ~A.~%" (format-stardate (+ *stardate* ttime))))))
        (when (< *remaining-time* ttime)
          (print-message (format nil "Unfortunately, the Federation will be destroyed by then.~%")))
        (when (> twarp 6.0)
          (print-message (format nil "You'll be taking risks at that speed, Captain.~%")))
        (when (or (and (= *super-commander-attacking-base* 1)
                       (coord-equal *super-commander-quadrant* destination-quadrant)
                       (< (find-event +super-commander-destroys-base+) (+ *stardate* ttime)))
                  (and (< (find-event +commander-destroys-base+) (+ *stardate* ttime))
                       (coord-equal *base-under-attack-quadrant* destination-quadrant)))
          (print-message (format nil "The starbase there will be destroyed by then.~%")))
        (skip-line)
        (print-prompt "New warp factor to try? ")
        (scan-input)
        (if (numberp *input-item*)
          (if (and (>= twarp 1.0)
                   (<= twarp 10.0))
              (progn
                (setf wfl t)
                (setf try-another-warp-factor-p t)
                (setf twarp *input-item*))
              (progn
                (huh)
                (return-from calculate-eta nil)))
          (progn
            (clear-type-ahead-buffer)
            (return-from calculate-eta nil)))))))

;; TODO - see the rechart() function in the C source and the next three comments
;; TODO - The chart command can (should) do an implicit call to srscan or lrscan
;; TODO - The chart command can (should) update from starbase records when docked
;; TODO - The chart command can (should) do an implicit update from dsradio messages
(defun chart () ; C: chart(void), and TODO - bring in the contents of makechart()
  "Display the start chart."

  (if *window-interface-p*
      (if *starchart-window*
          (progn
            (select-window *starchart-window*)
            (clear-window)
            (wmove *starchart-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (print-out (format nil "       STAR CHART FOR THE KNOWN GALAXY~%"))
  (when (> *stardate* *last-chart-update*)
    (print-out (format nil "(Last surveillance update ~A stardates ago).~%"
                       (truncate (- *stardate* *last-chart-update*)))))
  (print-out (format nil "      1    2    3    4    5    6    7    8~%"))
  (do ((x 0 (+ x 1)))
      ((>= x +galaxy-size+))
    (print-out (format nil "~A |" (+ x 1)))
    (do ((y 0 (+ y 1)))
        ((>= y +galaxy-size+))
      (if (and (= x (coordinate-x *ship-quadrant*))
               (= y (coordinate-y *ship-quadrant*)))
          (print-out "<")
          (print-out " "))
      (cond
        ((quadrant-supernovap (aref *galaxy* x y))
         (print-out "***"))

        ((and (not (quadrant-chartedp (aref *galaxy* x y)))
              (> (quadrant-starbases (aref *galaxy* x y)) 0))
         (print-out ".1."))

        ((quadrant-chartedp (aref *galaxy* x y))
         (print-out (format nil "~3D" (+ (* (starchart-page-klingons (aref *starchart* x y)) 100)
                                         (* (starchart-page-starbases (aref *starchart* x y)) 10)
                                         (starchart-page-stars (aref *starchart* x y))))))

        (t
         (print-out "...")))
      (if (and (= x (coordinate-x *ship-quadrant*))
               (= y (coordinate-y *ship-quadrant*)))
          (print-out ">")
          (print-out " ")))
    (print-out "|")
    (when (< x +galaxy-size+)
      (skip-line))))

;; TODO - make this a device status report. Show repair time of 0 when a device is not damaged.
;;        The goal is to have a continuously updated console showing device status.
(defun damage-report () ; C: damagereport(void)
  "List damaged devices and repair times for each."

  (if *window-interface-p*
      (if *damage-report-window*
          (progn
            (select-window *damage-report-window*)
            (clear-window)
            (wmove *damage-report-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (do ((i 0 (+ i 1))
       (header-printed-p nil))
      ((>= i +number-of-devices+)
       (when (not header-printed-p)
         (print-out (format nil "All devices functional.~%"))))
    (when (damagedp i)
      (when (not header-printed-p)
        (print-out (format nil "~12@A~24@A~%" "DEVICE" "-REPAIR TIMES-"))
        (print-out (format nil "~21A~8@A~8@A~%" " " "IN FLIGHT" "DOCKED"))
        (setf header-printed-p t))
      (print-out (format nil "  ~17A~9,2F~9,2F~%"
                         (aref *devices* i)
                         (+ (aref *device-damage* i) 0.05)
                         (+ (* (aref *device-damage* i) +docked-repair-factor+) 0.005))))))

(defun report ()
  "Report on general game status."

  (skip-line)
  (print-message (format nil "You are playing a ~A ~A game.~%"
                         (game-length-label *game-length*)
                         (skill-level-label *skill-level*)))
  (when *tournament-number*
    (print-message (format nil "This is tournament game ~A.~%" *tournament-number*)))
  (print-message (format nil "Your secret password is \"~A\"~%" *self-destruct-password*))
  (print-out (format nil "~A of ~A Klingons have been killed"
                     (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                        (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*))
                     (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
  (cond
    ((> (- *initial-commanders* (length *commander-quadrants*)) 0)
     (print-message (format nil ", including ~A Commander~A.~%"
                            (- *initial-commanders* (length *commander-quadrants*))
                            (if (= (- *initial-commanders* (length *commander-quadrants*)) 1) "" "s"))))

    ((> (+ (- *initial-klingons* *remaining-klingons*)
           (- *initial-super-commanders* *remaining-super-commanders*))
        0)
     (print-message (format nil ", but no Commanders.~%")))

    (t
     (print-message (format nil ".~%"))))
  (when (> (skill-level-value *skill-level*) +fair+)
    (print-message (format nil "The Super Commander has ~Abeen destroyed.~%"
                           (if (> *remaining-super-commanders* 0) "not " ""))))
  (if (/= *initial-bases* (length *base-quadrants*))
      (progn
        (print-out "There ")
        (if (= (- *initial-bases* (length *base-quadrants*)) 1)
            (print-out "has been 1 base")
            (print-out (format nil "have been ~A bases" (- *initial-bases* (length *base-quadrants*)))))
        (print-message (format nil " destroyed, ~A remaining.~%" (length *base-quadrants*))))
      (print-message (format nil "There are ~A bases.~%" *initial-bases*)))
  ;; Don't report this if not seen and either the radio is damaged or not at base!
  (when (or (not (damagedp +subspace-radio+))
            *dockedp*
            *base-attack-report-seen-p*)
    (attack-report))
  (when (> *casualties* 0)
    (print-message (format nil "~A casualt~A suffered so far.~%" *casualties* (if (= *casualties* 1) "y" "ies"))))
  (when (/= *brig-capacity* *brig-free*)
    (print-message (format nil "~D Klingon~A in brig.~%"
                           (- *brig-capacity* *brig-free*)
                           (when (> (- *brig-capacity* *brig-free*) 1) "s"))))
  (when (> *captured-klingons* 0)
    (print-message (format nil "~D captured Klingon~A turned in to Star Fleet.~%"
                           *captured-klingons* (when (> *captured-klingons* 1) "s"))))
  (when (> *calls-for-help* 0)
    (print-message (format nil "There ~A ~A call~A for help.~%"
                           (if (= *calls-for-help* 1) "was" "were")
                           *calls-for-help*
                           (if (= *calls-for-help* 1) "" "s"))))
  (when (string= *ship* +enterprise+)
    (print-message (format nil "You have ~A deep space probe~A.~%"
                           (if (> *probes-available* 0) *probes-available* "no")
                           (if (/= *probes-available* 1) "s" ""))))
  (when (and (or (not (damagedp +subspace-radio+))
                 *dockedp*)
             (is-scheduled-p +move-deep-space-probe+))
    (if *probe-is-armed-p*
        (print-out "An armed deep space probe is in ")
        (print-out "A deep space probe is in "))
    (print-message (format nil "~A.~%" (format-quadrant-coordinates *probe-reported-quadrant*))))
  (when *dilithium-crystals-on-board-p*
    (if (<= *crystal-work-probability* 0.05)
        (print-message (format nil "Dilithium crystals aboard ship... not yet used.~%"))
        ;; Calculate number of times crystals have been used, and display it.
        (do ((i 0 (+ i 1))
             (ai 0.05 (* ai 2.0)))
            ((< *crystal-work-probability* ai)
             (print-message (format nil "Dilithium crystals have been used ~A time~A.~%" i (if (= i 1) "" "s"))))))))

(defun request () ; C: request()
  "Request a single item of status information."

  (let (req-item)
    (unless *line-tokens*
      (print-prompt "Information desired? "))
    (scan-input)
    (setf req-item (match-token *input-item* (list "date" "condition" "position" "lsupport" "warpfactor"
                                                   "energy" "torpedoes" "shields" "klingons" "time")))
    (unless req-item
      (setf req-item "?")) ; blank input still results in a help message
    (when *window-interface-p*
      (select-window *message-window*))
    (cond
      ((string= req-item "date")
       (ship-status 1))
      ((string= req-item "condition")
       (ship-status 2))
      ((string= req-item "position")
       (ship-status 3))
      ((string= req-item "lsupport")
       (ship-status 4))
      ((string= req-item "warpfactor")
       (ship-status 5))
      ((string= req-item "energy")
       (ship-status 6))
      ((string= req-item "torpedoes")
       (ship-status 7))
      ((string= req-item "shields")
       (ship-status 8))
      ((string= req-item "klingons")
       (ship-status 9))
      ((string= req-item "time")
       (ship-status 10))
      (t
       (skip-line)
       (print-message (format nil "UNRECOGNIZED REQUEST. Valid requests are:~%"))
       (print-message (format nil "  date, condition, position, lsupport, warpfactor,~%"))
       (print-message (format nil "  energy, torpedoes, shields, klingons, time.~%"))
       (clear-type-ahead-buffer)))))

;; Possible enhancements:
;; 1. Display the visual scan in the short range scan grid. Per the function documentation
;;    string, this doesn't offer anything we don't already have but it looks better.
;; 2. Like #1, and display more than one adjacent sector, say 3 adjacent sectors, in a 90 degree
;;    cone but only in the specified direction. This gives more information than the default
;;    adjacent sectors at the cost of giving enemies another turn to attack.
;; 3. Like #1 and #2, but display only 2 adjacent sectors in a 90 degree cone in the specified
;;    direction and all adjacent sectors.
(defun visual-scan () ; C: visual(void)
  "A visual scan is made in a particular direction of three sectors in the general direction
specified. This takes time, and Klingons can attack you, so it should be done only when sensors
are out.

Code swiped from BSD-Trek. Not necesssarily useful, as we automatically display all adjacent
sectors on the short-range scan even when short-range sensors are out."

  ;; The C source uses an array of coordinate structs. Just use pairs of numbers here.
  (let (delta-index
        (visual-delta (make-array '(11 2) :initial-contents '((-1 -1)
                                                              (-1  0)
                                                              (-1  1)
                                                              ( 0  1)
                                                              ( 1  1)
                                                              ( 1  0)
                                                              ( 1 -1)
                                                              ( 0 -1)
                                                              (-1 -1)
                                                              (-1  0)
                                                              (-1  1))))
        ix iy sector-contents)
    (skip-line)
    (when (not (numberp *input-item*))
      (clear-type-ahead-buffer)
      (print-prompt "Direction? ")
      (scan-input)
      (when (not (numberp *input-item*))
        (huh)
        (return-from visual-scan nil)))
    (when (or (< *input-item* 0.0)
              (> *input-item* 360.0))
      (return-from visual-scan nil))
    (when *window-interface-p*
      (select-window *message-window*))
    (setf delta-index (floor (/ (+ *input-item* 22) 45)))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A,~A ~A " (+ ix 1) (+ iy 1) sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A " sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A ~A,~A~%" sector-contents (+ ix 1) (+ iy 1)))
    (setf *time-taken-by-current-operation* 0.5)
    (setf *action-taken-p* t)))

(defun clean-up ()
  "Cleanup function to run at program exit."

  (when *window-interface-p*
    (endwin))

  ;; Remove this cleanup function from the exit hooks
  (delete '(clean-up) sb-ext:*exit-hooks*))

(defun set-text-color (color)
  "Set the current window to the specified color."

  (when (and *current-window*
             (has-colors))
    (cond
      ((equal color +default-color+)
       (wattrset *current-window* 0))
      ((equal color +black+)
       (wattron *current-window* (color-pair color_black)))
      ((equal color +blue+)
       (wattron *current-window* (color-pair color_blue)))
      ((equal color +green+)
       (wattron *current-window* (color-pair color_green)))
      ((equal color +cyan+)
       (wattron *current-window* (color-pair color_cyan)))
      ((equal color +red+)
       (wattron *current-window* (color-pair color_red)))
      ((equal color +magenta+)
       (wattron *current-window* (color-pair color_magenta)))
      ((equal color +brown+)
       (wattron *current-window* (color-pair color_yellow)))
      ((equal color +light-gray+)
       (wattron *current-window* (color-pair color_white)))
      ((equal color +dark-gray+)
       (wattron *current-window* (logior (color-pair color_black) a_bold)))
      ((equal color +light-blue+)
       (wattron *current-window* (logior (color-pair color_blue) a_bold)))
      ((equal color +light-green+)
       (wattron *current-window* (logior (color-pair color_green) a_bold)))
      ((equal color +light-cyan+)
       (wattron *current-window* (logior (color-pair color_cyan) a_bold)))
      ((equal color +light-red+)
       (wattron *current-window* (logior (color-pair color_red) a_bold)))
      ((equal color +light-magenta+)
       (wattron *current-window* (logior (color-pair color_magenta) a_bold)))
      ((equal color +yellow+)
       (wattron *current-window* (logior (color-pair color_yellow) a_bold)))
      ((equal color +white+)
       (wattron *current-window* (logior (color-pair color_white) a_bold))))))

(defun initialize ()
  "One-time start up actions. Initialize I/O, curses, data values, etc."

  ;; Register a cleanup function to run on program exit.
  (setf sb-ext:*exit-hooks* (append sb-ext:*exit-hooks* '(clean-up)))

  (setf *random-state* (make-random-state t)) ; Seed the random number generator

  (setf *print-right-margin* 80) ; should be good for both windowed and line-by-line

  ;; Win32 environments get line-by-line mode until I have the patience for making pdcurses work
  (if (string= (software-type) "Win32")
      (setf *window-interface-p* nil)
      (progn
        (initscr)
	(setf *window-interface-p* t)
        ;; Ensure screen is large enough (24x80) for windows, otherwise use line-by-line mode.
        (unless (and (>= *lines* 24)
                     (>= *cols* 80))
	  (endwin)
          (setf *window-interface-p* nil))))

  ;; DEBUG - force line-by-line mode
  ;;(endwin)
  ;;(setf *window-interface-p* nil)

  (when *window-interface-p*
     (keypad *stdscr* true) ; Assume the terminal has a keypad and function keys
     (nonl)
     (cbreak)
     (when (has-colors)
       (start-color)
       (init-pair color_black color_black color_black)
       (init-pair color_green color_green color_black)
       (init-pair color_red color_red color_black)
       (init-pair color_cyan color_cyan color_black)
       (init-pair color_white color_white color_black)
       (init-pair color_magenta color_magenta color_black)
       (init-pair color_blue color_blue color_black)
       (init-pair color_yellow color_yellow color_black))
     (setf *current-window* *stdscr*)
     (clear-window) ; After this point use curses window functions, not *stdscr* functions.

     ;; In curses/window mode these windows are always created because they fit the minimum
     ;; space required for windowed mode: short range scan, status, long range scan, game status,
     ;; message, prompt. If more rows/columns are available then more windows are created.
     ;;
     ;; All output that doesn't go to a specific window is displayed in the message window.
     ;; In line-by-line mode the full screen is effectively the message window.
     ;;
     ;; TODO - name all these constants that define window position and size?
     (setf *short-range-scan-window* (newwin 12 24 0 0))
     (setf *ship-status-window* (newwin 10 36 2 24))
     (setf *long-range-scan-window* (newwin 5 19 0 60))
     (setf *game-status-window* (newwin 3 19 8 60))
     ;; The message window is allocated all space between the short range scan window and the
     ;; prompt window. The message window must be at least 12 lines long to display a star chart.
     (setf *message-window-lines* (- *lines* 1 11))
     ;; Game narrative and general output
     (setf *message-window* (newwin *message-window-lines* 80 12 0))
     (scrollok *message-window* true)
     (setf *prompt-window* (newwin 1 0 (- *lines* 1) 0))
     (when (>= *cols* 125)
       (setf *starchart-window* (newwin 10 45 0 81)))
     (when (and (>= *cols* 119)
                (>= *lines* 25))
       (setf *damage-report-window* (newwin 15 38 11 81)))
     (when (and (>= *cols* 185)
                (>= *lines* 44))
       (setf *planet-report-window* (newwin 44 59 0 126)))
     (when (and (>= *cols* 182)
                (>= *lines* 65))
       (setf *score-window* (newwin 20 55 45 126)))
     (set-text-color +default-color+)
     (select-window *message-window*)))

;; TODO - every operation that uses time should call this, e.g. visual-scan.
;; TODO - can this function be merged into the process-events function?
(defun consume-time () ; C: bool consumeTime(void)
  "Abort a lengthy operation if an event interrupts it. Return value of True indicates that there
was an event that requires aborting the operation carried out by the calling function."

  (setf *action-taken-p* t)
  (process-events)
  (when (or *all-done-p*
            (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
            *just-in-p*)
    (return-from consume-time t))
  (return-from consume-time nil))

(defun shuttle () ; C: shuttle(void)
  "Use shuttle craft for planetary jaunt."

  (skip-line)
  (cond
    ((damagedp +shuttle+)
     ;; TODO - The -1.0 value is a bizarre way of saying the current ship is the Faerie Queene
     (cond
       ((= (aref *device-damage* +shuttle+) -1.0)
        (if (and *in-orbit-p*
                 (shuttle-landed-p *ship-quadrant*))
            (print-message (format nil "Ye Faerie Queene has no shuttle craft bay to dock it at.~%"))
            (print-message (format nil "Ye Faerie Queene had no shuttle craft.~%"))))

       ((> (aref *device-damage* +shuttle+) 0)
        (print-message (format nil "The Galileo is damaged.~%")))

       (t ; C: game.damage[DSHUTTL] < 0, or (< (aref *device-damage* +shuttle+) 0)
        (print-message (format nil "Shuttle craft is now serving Big Macs.~%")))))

    ((not *in-orbit-p*)
     (print-message (format nil "~A not in standard orbit.~%" (format-ship-name))))

    ((and (not (shuttle-landed-p *ship-quadrant*))
          (not (eql *shuttle-craft-location* 'on-ship)))
     (print-message (format nil "Shuttle craft not currently available.~%")))

    ((and (shuttle-landed-p *ship-quadrant*)
          (not *landedp*))
     (print-message (format nil "You will have to beam down to retrieve the shuttle craft.~%")))

    ((or *shields-are-up-p*
         *dockedp*)
     (print-message (format nil "Shuttle craft cannot pass through shields.~%")))

    ((not (planet-knownp (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
     (print-message (format nil "Spock-  \"Captain, we have no information on this planet~%"))
     (print-message (format nil "  and Starfleet Regulations clearly state that in this situation~%"))
     (print-message (format nil "  you may not fly down.\"~%")))

    (t
     (setf *time-taken-by-current-operation* (* (expt 3.0 -5) *height-of-orbit*))
     (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
       (print-message (format nil "First Officer Spock-  \"Captain, I compute that such~%"))
       (print-message (format nil "  a maneuver would require approximately ~,2F% of our remaining time.~%"
                              (truncate (/ (* 100 *time-taken-by-current-operation*) *remaining-time*))))
       (print-prompt"Are you sure this is wise?\" " )
       (unless (get-y-or-n-p)
         (setf *time-taken-by-current-operation* 0.0)
         (return-from shuttle nil)))
     (if *landedp*
         ;; Kirk on planet
         (if (eql *shuttle-craft-location* 'on-ship)
             ;; Galileo on ship!
             (progn
               (if (not (damagedp +transporter+))
                   (progn
                     (print-prompt "Spock-  \"Would you rather use the transporter?\" ")
                     (when (get-y-or-n-p)
                       (beam)
                       (return-from shuttle nil))
                     (print-message "Shuttle crew"))
                   (print-message "Rescue party"))
               (print-message (format nil " boards Galileo and swoops toward planet surface.~%"))
               (setf *shuttle-craft-location* 'off-ship)
               (setf *shuttle-craft-quadrant* *ship-quadrant*)
               (skip-line)
               (when (consume-time)
                 (return-from shuttle nil))
               (print-message (format nil "Trip complete.~%")))
             (progn
               ;; Ready to go back to ship
               (print-message (format nil "You and your mining party board the~%"))
               (print-message (format nil "shuttle craft for the trip back to the Enterprise.~%"))
               (skip-line)
               (print-message (format nil "The short hop begins . . .~%"))
               (skip-line 2)
               (let ((pl (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
                 (setf (planet-knownp pl) t)
                 (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) pl))
               (setf *in-shuttle-craft-p* t)
               (setf *landedp* nil)
               (when (consume-time)
                 (return-from shuttle nil))
               (setf *in-shuttle-craft-p* nil)
               (setf *shuttle-craft-location* 'on-ship)
               (setf *shuttle-craft-quadrant* nil)
               (when *miningp*
                 (setf *dilithium-crystals-on-board-p* t)
                 (setf *crystal-work-probability* 0.05))
               (setf *miningp* nil)
               (print-message (format nil "Trip complete.~%"))))
         (progn
           ;; Kirk on ship
           ;; and so is Galileo
           (print-message (format nil "Mining party assembles in the hangar deck,~%"))
           (print-message (format nil "ready to board the shuttle craft \"Galileo\".~%"))
           (skip-line)
           (print-message (format nil "The hangar doors open; the trip begins.~%"))
           (skip-line)
           (setf *in-shuttle-craft-p* t)
           (setf *shuttle-craft-location* 'off-ship)
           (setf *shuttle-craft-quadrant* *ship-quadrant*)
           (when (consume-time)
             (return-from shuttle nil))
           (setf *in-shuttle-craft-p* nil)
           (setf *landedp* t)
           (print-message (format nil "Trip complete.~%")))))))

(defun beam () ; C: beam(void)
  "Use the transporter."

  (let ((energy-needed (+ (* 50 (skill-level-value *skill-level*)) (/ *height-of-orbit* 100.0))))
    (skip-line)
    (when (damagedp +transporter+)
      (print-message (format nil "Transporter damaged.~%"))
      ;; The shuttle is an option if it is not damaged and on the planet or on the ship
      (when (and (not (damagedp +shuttle+))
                 (or (shuttle-landed-p *ship-quadrant*)
                     (eql *shuttle-craft-location* 'on-ship)))
        (skip-line)
        (print-prompt "Spock-  \"May I suggest the shuttle craft, Sir?\" ")
        (when (get-y-or-n-p)
          (shuttle)))
      (return-from beam nil))
    (when (not *in-orbit-p*)
      (print-message (format nil "~A not in standard orbit.~%" (format-ship-name)))
      (return-from beam nil))
    (when *shields-are-up-p*
      (print-message (format nil "Impossible to transport through shields.~%"))
      (return-from beam nil))
    (when (not (planet-knownp (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
      (print-message (format nil "Spock-  \"Captain, we have no information on this planet~%"))
      (print-message (format nil "  and Starfleet Regulations clearly state that in this situation~%"))
      (print-message (format nil "  you may not go down.\"~%"))
      (return-from beam nil))
    (when (and (not *landedp*)
               (eql (planet-crystals (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal)))
                    'absent))
      (print-message (format nil "Spock-  \"Captain, I fail to see the logic in~%"))
      (print-message (format nil "  exploring a planet with no dilithium crystals.~%"))
      (print-prompt "  Are you sure this is wise?\" ")
      (unless (get-y-or-n-p)
        (return-from beam nil)))
    (when (> energy-needed *ship-energy*)
      (print-message (format nil "Engineering to bridge--~%"))
      (print-message (format nil "  Captain, we don't have enough energy for transportation.~%"))
      (return-from beam nil))
    (when (and (not *landedp*)
               (> (* 2 energy-needed) *ship-energy*))
      (print-message (format nil "Engineering to bridge--~%"))
      (print-message (format nil "  Captain, we have enough energy only to transport you down to~%"))
      (print-message (format nil "  the planet, but there wouldn't be any energy for the trip back.~%"))
      (when (shuttle-landed-p *ship-quadrant*)
        (print-message (format nil "  Although the Galileo shuttle craft may still be on the surface.~%")))
      (print-prompt "  Are you sure this is wise?\" ")
      (unless (get-y-or-n-p)
        (return-from beam nil)))
    (if *landedp*
        ;; Coming from planet
        (progn
          (when (shuttle-landed-p *ship-quadrant*)
            (print-prompt "Spock-  \"Wouldn't you rather take the Galileo?\" ")
            (when (get-y-or-n-p)
              (return-from beam nil))
            (print-message (format nil "Your crew hides the Galileo to prevent capture by aliens.~%")))
          (print-message (format nil "Landing party assembled, ready to beam up.~%"))
          (skip-line)
          (print-message (format nil "Kirk whips out communicator...~%"))
          (print-message (format nil "BEEP  BEEP  BEEP~%") :print-slowly t)
          (skip-line 2)
          (print-message (format nil "\"Kirk to enterprise-  Lock on coordinates...energize.\"~%")))
        ;; Going to planet
        (progn
          (print-message (format nil "Scotty-  \"Transporter room ready, Sir.\"~%"))
          (skip-line)
          (print-message (format nil "Kirk and landing party prepare to beam down to planet surface.~%"))
          (skip-line)
          (print-message (format nil "Kirk-  \"Energize.\"~%"))))
    (setf *action-taken-p* t)
    (skip-line)
    (print-message (format nil "WWHOOOIIIIIRRRRREEEE.E.E.  .  .  .  .   .    .~%") :print-slowly t)
    (skip-line)
    (when (> (random 1.0) 0.98)
      (print-message (format nil "BOOOIIIOOOIIOOOOIIIOIING . . .~%") :print-slowly t)
      (skip-line)
      (print-message (format nil "Scotty-  \"Oh my God!  I've lost them.\"~%"))
      (finish +lost+)
      (return-from beam nil))
    (print-message (format nil ".    .   .  .  .  .  .E.E.EEEERRRRRIIIIIOOOHWW~%") :print-slowly t)
    (setf *landedp* (not *landedp*))
    (setf *ship-energy* (- *ship-energy* energy-needed))
    (skip-line 2)
    (print-message (format nil "Transport complete.~%"))
    (when (and *landedp*
               (shuttle-landed-p *ship-quadrant*))
      (print-message (format nil "The shuttle craft Galileo is here!~%")))
    (when (and (not *landedp*)
               *miningp*)
      (setf *dilithium-crystals-on-board-p* t)
      (setf *crystal-work-probability* 0.05)) ; TODO - 0.05 is also a flag value indicating mining has been completed
    (setf *miningp* nil)))

(defun deathray () ; C: deathray(void)
  "Use the big zapper."

  (setf *action-taken-p* nil)
  (skip-line)
  (cond
    ((string= *ship* +faerie-queene+)
     (print-message (format nil "Ye Faerie Queene has no death ray.~%")))

    ((= *enemies-here* 0)
     (print-message (format nil "Sulu-  \"But Sir, there are no enemies in this quadrant.\"~%")))

    ((damagedp +death-ray+)
     (print-message (format nil "Death Ray is damaged.~%")))

    (t
     (print-message (format nil "Spock-  \"Captain, the 'Experimental Death Ray'~%"))
     (print-message (format nil "  is highly unpredictible.  Considering the alternatives,~%"))
     (print-prompt "  are you sure this is wise?\" ")
     (unless (get-y-or-n-p)
       (return-from deathray nil))
     (print-message (format nil "Spock-  \"Acknowledged.\"~%"))
     (skip-line)
     (setf *action-taken-p* t)
     (print-message (format nil "WHOOEE ... WHOOEE ... WHOOEE ... WHOOEE~%") :print-slowly t)
     (print-message (format nil "Crew scrambles in emergency preparation.~%"))
     (print-message (format nil "Spock and Scotty ready the death ray and~%"))
     (print-message (format nil "prepare to channel all ship's power to the device.~%"))
     (skip-line)
     (print-message (format nil "Spock-  \"Preparations complete, sir.\"~%"))
     (print-message (format nil "Kirk-  \"Engage!\"~%"))
     (skip-line)
     (print-message (format nil "WHIRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR~%") :print-slowly t)
     (if (> (random 1.0) +deathray-failure-chance+)
         (progn
           (print-message (format nil "Sulu- \"Captain!  It's working!\"~%") :print-slowly t)
           (skip-line)
           (when (> *enemies-here* 0)
             (dead-enemy (aref *klingon-sectors* 1)
                         (coord-ref *quadrant-contents* (aref *klingon-sectors* 1))
                         (aref *klingon-sectors* 1)))
           (print-message (format nil "Ensign Chekov-  \"Congratulations, Captain!\"~%"))
           (skip-line)
           (print-message (format nil "Spock-  \"Captain, I believe the `Experimental Death Ray'~%"))
           (if (<= (random 1.0) 0.05)
               (print-message (format nil "   is still operational.\"~%"))
               (progn
                 (print-message (format nil "   has been rendered nonfunctional.\"~%"))
                 (setf (aref *device-damage* +death-ray+) 39.95)))
           ;; In the C source the victory check was done before reporting on the status of the
           ;; deathray. That seems backwards - the deathray status report is irrelevant and out
           ;; of place after the text explaining that the game has been won.
           (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
             (finish +won+)))
         (let ((r (random 1.0)))
           ;; Pick failure method
           (cond
             ((<= r 0.30)
              (print-message (format nil "Sulu- \"Captain!  It's working!\"~%") :print-slowly t)
              (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
              (print-message (format nil "***MATTER-ANTIMATTER IMPLOSION IMMINENT!~%"))
              (skip-line)
              (print-message
               (format nil "***RED ALERT!  RED A*L********************************~%")
               :print-slowly t)
              (print-stars)
              (print-message
               (format nil "******************   KA-BOOM!!!!   *******************~%")
               :print-slowly t)
              (kaboom))

             ((<= r 0.55)
              (print-message
               (format nil "Sulu- \"Captain!  Yagabandaghangrapl, brachriigringlanbla!\"~%")
               :print-slowly t)
              (print-message (format nil "Lt. Uhura-  \"Graaeek!  Graaeek!\"~%"))
              (print-message (format nil "Spock-  \"Fascinating!  . . . All humans aboard~%"))
              (print-message (format nil "  have apparently been transformed into strange mutations.~%"))
              (print-message (format nil"  Vulcans do not seem to be affected.~%"))
              (skip-line)
              (print-message (format nil "Kirk-  \"Raauch!  Raauch!\"~%"))
              (finish +death-ray+))

             ((<= r 0.75)
              (print-message (format nil "Sulu- \"Captain!  It's   --WHAT?!?!\"~%") :print-slowly t)
              (skip-line)
              (print-message "Spock-  \"I believe the word is")
              (print-message " *ASTONISHING*"  :print-slowly t)
              (print-message (format nil " Mr. Sulu.~%"))
              (do ((i 0 (1+ i)))
                  ((>= i +quadrant-size+))
                (do ((j 0 (1+ j)))
                    ((>= j +quadrant-size+))
                  (when (string= (aref *quadrant-contents* i j) +empty-sector+)
                    (setf (aref *quadrant-contents* i j) +thing+))))
              (print-message (format nil "  Captain, our quadrant is now infested with~%"))
              (print-message (format nil " - - - - - -  *THINGS*.~%") :print-slowly t)
              (print-message (format nil "  I have no logical explanation.\"~%")))

             (t
              (print-message
               (format nil "Sulu- \"Captain!  The Death Ray is creating tribbles!\"~%")
               :print-slowly t)
              (print-message (format nil "Scotty-  \"There are so many tribbles down here~%"))
              (print-message (format nil "  in Engineering, we can't move for 'em, Captain.\"~%"))
              (finish +tribbles+))))))))

(defun mine () ; C: mine(void)
  "Mine dilithium from a planet."

  (skip-line)
  (cond
    ((not *landedp*)
     (print-message (format nil "Mining party not on planet.~%")))

    ((eql (planet-crystals (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))) 'mined)
     (print-message (format nil "This planet has already been mined for dilithium.~%")))

    ((eql (planet-crystals (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))) 'absent)
     (print-message (format nil "No dilithium crystals on this planet.~%")))

    (*miningp*
     (print-message (format nil "You've already mined enough crystals for this trip.~%")))

    ((and *dilithium-crystals-on-board-p*
          (= *crystal-work-probability* 0.05))
     (print-message
      (format nil "With all those fresh crystals aboard the ~A~%there's no reason to mine more at this time.~%"
              (format-ship-name))))

    (t
     (setf *time-taken-by-current-operation*
           (* (+ 0.1 (* 0.2 (random 1.0)))
              (planet-class (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal)))))
     (unless (consume-time)
       (skip-line)
       (print-message (format nil "Mining operation complete.~%"))
       (let ((pl (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
         (setf (planet-crystals pl) 'mined)
         (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) pl))
       (setf *miningp* t)
       (setf *action-taken-p* t)))))

(defun quadrant-exit-while-on-planet (finish-reason) ; C: atover()
  "Handle the case of the captain being on a planet when the ship suddenly exits the quadrant.
These exits occur when there is a supernova in the quadrant or a tractor beam grabs the ship.
The finish-reason determines the ending text if the captain is not successfully retrieved from
the planet."

  ;; TODO - set the output window when in curses mode?
  (clear-type-ahead-buffer)
  (when *landedp*
    (when (damagedp +transporter+)
      (finish finish-reason)
      (return-from quadrant-exit-while-on-planet nil))
    (print-message (format nil "Scotty rushes to the transporter controls.~%"))
    (when *shields-are-up-p*
      (print-message (format nil "But with the shields up it's hopeless.~%"))
      (finish finish-reason)
      (return-from quadrant-exit-while-on-planet nil))
    (print-message (format nil "His desperate attempt to rescue you . . .") :print-slowly t)
    (if (<= (random 1.0) 0.05)
        (progn
          (print-message (format nil "fails.~%"))
          (finish finish-reason))
        (progn
          (print-message (format nil "SUCCEEDS!~%"))
          (when *miningp*
            (setf *miningp* nil)
            (print-message "The crystals mined were ")
            (if (<= (random 1.0) 0.25)
                (print-message (format nil "lost.~%"))
                (progn
                  (print-message (format nil "saved.~%"))
                  (setf *dilithium-crystals-on-board-p* t))))))))

(defun emergency-supernova-exit () ; C: atover()
  "Handle emergency exit from the quadrant due to a supernova."

  (quadrant-exit-while-on-planet +mining-party-nova+)
  ;; TODO - is this the correct place for the shuttle craft check?
  ;; Check to see if captain in shuttle craft
  (when *in-shuttle-craft-p*
    (finish +shuttle-super-nova+)) ;; C used Shuttle Tractor Beam but that code couldn't be reached
  (unless *all-done-p*
    ;; Inform captain of attempt to reach safety
    (skip-line)
    (do ((power (* 0.75 *ship-energy*))
         (required-distance (+ 1.4142 (random 1.0))))
        ;; Repeat if another supernova
        ((not (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))))
      (when *just-in-p*
        (print-message (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
        (print-message (format nil "The ~A has stopped in a quadrant containing~%" (format-ship-name)))
        (print-message (format nil "   a supernova.~%") :print-slowly t)
        (skip-line))
      (print-message (format nil "***Emergency automatic override attempts to hurl ~A~%" (format-ship-name)))
      (print-message (format nil "safely out of quadrant.~%"))
      (setf (quadrant-chartedp (coord-ref *galaxy* *ship-quadrant*)) t)
      ;; Try to use warp engines
      (when (damagedp +warp-engines+)
        (skip-line)
        (print-message (format nil "Warp engines damaged.~%"))
        (finish +destroyed-by-supernova+)
        (return-from emergency-supernova-exit nil))
      (setf *warp-factor* (+ 6.0 (* 2.0 (random 1.0))))
      (print-message (format nil "Warp factor set to ~A~%" (truncate *warp-factor*)))
      (let ((course (* 12.0 (random 1.0))) ; How dumb! (really?)
            (distance (/ power (* (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))))
        (when (< required-distance distance)
          (setf distance required-distance))
        (setf *time-taken-by-current-operation* (calculate-warp-movement-time :distance distance
                                                                              :warp-factor *warp-factor*))
        (setf *just-in-p* nil)
        (setf *in-orbit-p* nil)
        (execute-warp-move course distance))
      (when (not *just-in-p*)
        ;; This is bad news, we didn't leave the quadrant.
        (when *all-done-p*
          (return-from emergency-supernova-exit nil))
        (skip-line)
        ;; Not true in the case where the ship was blocked on the way out of the quadrant.
        (print-message (format nil "Insufficient energy to leave quadrant.~%"))
        (finish +destroyed-by-supernova+)
        (return-from emergency-supernova-exit nil)))
    (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
      (finish +won+)))) ; Supernova killed remaining enemies.

(defun orbit () ; C: orbit(void)
  "Enter standard orbit."

  (skip-line)
  (cond
    (*in-orbit-p*
     (print-message (format nil "Already in standard orbit.~%")))

    ((and (damagedp +warp-engines+)
          (damagedp +impluse-engines+))
     (print-message (format nil "Both warp and impulse engines damaged.~%")))

    ((or (not *current-planet*)
         (> (abs (- (coordinate-x *ship-sector*) (coordinate-x *current-planet*))) 1)
         (> (abs (- (coordinate-y *ship-sector*) (coordinate-y *current-planet*))) 1))
     (print-message (format nil "~A not adjacent to planet.~%" (format-ship-name)))
     (skip-line))

    (t
     (setf *time-taken-by-current-operation* (+ 0.02 (* 0.03 (random 1.0))))
     (print-message (format nil "Helmsman Sulu-  \"Entering standard orbit, Sir.\"~%"))
     (update-condition)
     (unless (consume-time)
       (setf *height-of-orbit* (+ 1400.0 (* 7200.0 (random 1.0))))
       (print-message (format nil "Sulu-  \"Entered orbit at altitude ~,2F kilometers.\"~%" *height-of-orbit*))
       (setf *in-orbit-p* t)
       (setf *action-taken-p* t)))))

;; TODO - respond in a reasonable way if there is an inhabited planet in the quadrant. There will
;;        never be dilithium crystals (that's how the initialization code works - presumably if
;;        the planet is inhabited the crystals have already been mined, or are not available for
;;        the player to mine) but at least print the planet name.
(defun sensor () ; C: sensor(void)
  "Examine planets in this quadrant."

  (when *window-interface-p*
    (select-window *message-window*))

  (skip-line)
  (cond
    ((and (damagedp +short-range-sensors+)
          (not *dockedp*))
     (print-message (format nil "Short range sensors damaged.~%")))

    ((not *current-planet*) ; sector coordinates or nil
     (print-message (format nil "Spock- \"No planet in this quadrant, Captain.\"~%")))

    (t ; Go ahead and scan even if the planet has been scanned before
     (let ((pl (rest (assoc *ship-quadrant* *planet-information* :test #'coord-equal))))
       (print-message (format nil "Spock-  \"Sensor scan for ~A -~%"
                              (format-quadrant-coordinates *ship-quadrant*)))
       (skip-line)
       ;; TODO - use pretty print justification
       (print-message (format nil "         Planet at ~A is of class ~A.~%"
                              (format-sector-coordinates *current-planet*)
                              (format-planet-class (planet-class pl))))
       (when (shuttle-landed-p *ship-quadrant*)
         ;; TODO - use pretty print justification
         (print-message (format nil "         Sensors show Galileo still on surface.~%")))
       ;; TODO - use pretty print justification
       (print-message "         Readings indicate")
       (when (not (eql (planet-crystals pl) 'present))
         (print-message " no"))
       (print-message (format nil " dilithium crystals present.\"~%"))
       (setf (planet-knownp pl) t)
       (rplacd (assoc *ship-quadrant* *planet-information* :test #'coord-equal) pl)))))

(defun mayday () ; C: mayday(void)
  "Yell for help from nearest starbase. There's more than one way to move in this game!

Here's how the mayday code works:

First, the closest starbase is selected.  If there is a a starbase in your own quadrant, you
are in good shape. This distance takes quadrant distances into account only.

A magic number is computed based on the distance which acts as the probability that you will be
rematerialized. You get three tries.

When it is determined that you should be able to be rematerialized (i.e., when the probability
thing mentioned above comes up positive), you are put into that quadrant (anywhere). Then, we try
to see if there is a spot adjacent to the starbase.  If not, you can't be rematerialized!!!
Otherwise, it drops you there.  It only tries five times to find a spot to drop you. After that,
it's your problem."

  ;; Test for conditions which prevent calling for help.
  (cond
    (*dockedp*
     (print-message (format nil "Lt. Uhura-  \"But Captain, we're already docked.\"~%")))

    ((damagedp +subspace-radio+)
     (print-message (format nil "Subspace radio damaged.~%")))

    ((= (length *base-quadrants*) 0)
     (print-message (format nil "Lt. Uhura-  \"Captain, I'm not getting any response from Starbase.\"~%")))

    (*landedp*
     (print-message (format nil "You must be aboard the ~A.~%" (format-ship-name))))

    (t ; OK -- call for help from nearest starbase
     (1+ *calls-for-help*)
     (let (dest-dist probf)
       (if (coordinate-p *base-sector*)
           ;; There's one in this quadrant
           (setf dest-dist (distance *base-sector* *ship-sector*))
           ;; Find the nearest starbase in another quadrant
           (let (dest-quadrant)
             ;; Find the distance to the first base in the list of bases
             (setf dest-dist (* +quadrant-size+ (distance (first *base-quadrants*) *ship-quadrant*)))
             (setf dest-quadrant (first *base-quadrants*))
             ;; Then try to find a base that's closer
             (dolist (bq *base-quadrants*)
               (when (< (* +quadrant-size+ (distance bq *ship-quadrant*)) dest-dist)
                 (setf dest-dist (* +quadrant-size+ (distance bq *ship-quadrant*)))
                 (setf dest-quadrant bq)))
             ;; The destination is not in the current quadrant so set up a new quadrant.
             (setf *ship-quadrant* dest-quadrant)
             (new-quadrant)))
       ;; dematerialize starship
       (setf (coord-ref *quadrant-contents* *ship-sector*) +empty-sector+)
       (skip-line)
       (print-message (format nil "Starbase in ~A responds-- ~A dematerializes.~%"
                              (format-quadrant-coordinates *ship-quadrant*)
                              (format-ship-name)))
       (setf *ship-sector* nil)
       (do ((five-tries 0 (1+ five-tries))
            (found-empty-sector-p nil)
            x y)
           ((or found-empty-sector-p
                (>= five-tries 5)))
         (setf x (truncate (+ (coordinate-x *base-sector*) (* 3.0 (random 1.0)) -1))) ; C: game.base.x+3.0*Rand()-1;
         (setf y (truncate (+ (coordinate-y *base-sector*) (* 3.0 (random 1.0)) -1)))
         (when (and (valid-sector-p x y)
                    (string= (aref *quadrant-contents* x y) +empty-sector+))
           ;; found one -- finish up
           (setf *ship-sector* (make-coordinate :x x :y y))
           (setf found-empty-sector-p t)))
       (when (not (coordinate-p *ship-sector*))
         (skip-line)
         ;; TODO - move this message to the finish function
         (print-message (format nil "You have been lost in space...~%"))
         (finish +materialize+)
         (return-from mayday nil))
       ;; Give starbase three chances to rematerialize starship
       (setf probf (expt (- 1.0 (expt 0.98 dest-dist)) 0.33333333))
       (do ((three-tries 0 (1+ three-tries))
            (succeeds-p nil))
           ((or succeeds-p
                (>= three-tries 3 ))
            (when (not succeeds-p)
              (setf (coord-ref *quadrant-contents* *ship-sector*) +thing+) ; question mark
              (setf *alivep* nil)
              ;; Don't call the sensor scan even if in curses mode. The ship has been
              ;; lost/destroyed and Spock isn't doing anything now. Fill the windows so the player
              ;; isn't left with a blank screen.
              (draw-windows)
              (finish +materialize+)
              (return-from mayday nil)))
         (print-message (format nil "~A attempt to re-materialize ~A "
                                (cond
                                  ((= three-tries 0) "1st")
                                  ((= three-tries 1) "2nd")
                                  ((= three-tries 2) "3rd"))
                                (format-ship-name)))
         (warble)
         (if (> (random 1.0) probf)
             (setf succeeds-p t)
             (progn
               (textcolor +red+)
               (print-message (format nil "fails.~%"))
               (textcolor +default-color+)))
         (sleep 0.500)) ; half of a second?
       (setf (coord-ref *quadrant-contents* *ship-sector*) *ship*)
       (textcolor +green+)
       (print-message (format nil "succeeds.~%"))
       (textcolor +default-color+)
       (dock))
     (skip-line)
     (print-message (format nil "Lt. Uhura-  \"Captain, we made it!\"~%")))))

;; TODO - there is no reason to limit the report to uninhabited planets. Sure, the only tactical
;;        reason for the report is to remember where the crystals were located, but go ahead and
;;        display inhabited worlds too. This is a "planet report" and the player gets a better
;;        sense of having a history in the game if all the planets that were scanned are reported.
(defun survey () ; C: survey(void)
  "Report on (uninhabited) planets in the galaxy."

  (if *window-interface-p*
      (if *planet-report-window*
          (progn
            (select-window *planet-report-window*)
            (clear-window)
            (wmove *planet-report-window* 0 0))
          (progn
            (select-window *message-window*)
            (skip-line)))
      (skip-line))

  (if *planet-report-window*
      (print-out (format nil "~36@A~%" "KNOWN PLANETS"))
      (progn
        (print-out (format nil "Spock-  \"Planet report follows, Captain.\"~%"))
        (skip-line)))
  (let ((one-planet-knownp-p nil))
    (dolist (p-cons *planet-information*)
      (unless (planet-class (rest p-cons))
        (when (and (planet-knownp (rest p-cons))
                   (not (planet-inhabitedp (rest p-cons))))
          (setf one-planet-knownp-p t)
          (print-out (format nil "~A  class ~A  "
                             (format-quadrant-coordinates (first p-cons))
                             (format-planet-class (planet-class (rest p-cons)))))
          (unless (eql (planet-crystals (rest p-cons)) 'present)
            (print-out "no "))
          (print-out (format nil "dilithium crystals present.~%"))
          (when (shuttle-landed-p (first p-cons))
            (print-out (format nil "~58@A~%" "Shuttle Craft Galileo on surface."))))))
    (unless one-planet-knownp-p
      (print-out (format nil "No information available.~%")))))

(defun use-crystals () ; C: usecrystals(void)
  "Use dilithium crystals."

  (setf *action-taken-p* nil)
  (skip-line)
  (when (not *dilithium-crystals-on-board-p*)
    (print-message (format nil "No dilithium crystals available.~%"))
    (return-from use-crystals nil))
  (when (>= *ship-energy* 1000.0)
    (print-message (format nil "Spock-  \"Captain, Starfleet Regulations prohibit such an operation~%"))
    (print-message (format nil "  except when Condition Yellow exists.~%"))
    (return-from use-crystals nil))
  (print-message (format nil "Spock- \"Captain, I must warn you that loading~%"))
  (print-message (format nil "  raw dilithium crystals into the ship's power~%"))
  (print-message (format nil "  system may risk a severe explosion.~%"))
  (print-prompt "  Are you sure this is wise?\" ")
  (unless (get-y-or-n-p)
    (return-from use-crystals nil))
  (skip-line)
  (print-message (format nil "Engineering Officer Scott-  \"(GULP) Aye Sir.~%"))
  (print-message (format nil "  Mr. Spock and I will try it.\"~%"))
  (skip-line)
  (print-message (format nil "Spock-  \"Crystals in place, Sir.~%"))
  (print-message (format nil "  Ready to activate circuit.\"~%"))
  (skip-line)
  (print-message (format nil "Scotty-  \"Keep your fingers crossed, Sir!\"~%") :print-slowly t)
  (when (<= (random 1.0) *crystal-work-probability*)
    (print-message (format nil "  \"Activating now! - - No good!  It's***~%") :print-slowly t)
    (skip-line)
    (print-message
     (format nil "***RED ALERT!  RED A*L********************************~%") :print-slowly t)
    (print-stars)
    (print-message (format nil "******************   KA-BOOM!!!!   *******************~%")
                   :print-slowly t)
    (kaboom)
    (return-from use-crystals nil))
  (setf *ship-energy* (+ *ship-energy* (* 5000.0 (+ 1.0 (* 0.9 (random 1.0))))))
  (print-message (format nil "  \"Activating now! - - ~%") :print-slowly t)
  (print-message (format nil "   The instruments~%"))
  (print-message (format nil "   are going crazy, but I think it's~%"))
  (print-message (format nil "   going to work!!  Congratulations, Sir!\"~%"))
  (setf *crystal-work-probability* (* *crystal-work-probability* 2.0))
  (setf *action-taken-p* t))

(defun restore-game-from-checkpoint (&optional (file-name +checkpoint-file-name+)) ; C: bool thaw(void)
  "Restore a saved game. Return true or false for restore success or failure."

  (with-open-file (s file-name :direction :input :if-does-not-exist nil)
    (setf events::*future-events* (read s))
    (setf *random-state* (read s))
    (setf *line-tokens* (read s))
    (setf *input-item* (read s))
    (setf *ship-quadrant* (read s))
    (setf *ship-sector* (read s))
    (setf *conquest-quadrant* (read s))
    (setf *tournament-number* (read s))
    (setf *game-length* (read s))
    (setf *skill-level* (read s))
    (setf *self-destruct-password* (read s))
    (setf *initial-energy* (read s))
    (setf *initial-shield-energy* (read s))
    (setf *initial-life-support-reserves* (read s))
    (setf *initial-torpedos* (read s))
    (setf *initial-klingons* (read s))
    (setf *initial-bases* (read s))
    (setf *initial-commanders* (read s))
    (setf *initial-super-commanders* (read s))
    (setf *initial-romulans* (read s))
    (setf *initial-stars* (read s))
    (setf *initial-planets* (read s))
    (setf *initial-resources* (read s))
    (setf *initial-time* (read s))
    (setf *initial-stardate* (read s))
    (setf *damage-factor* (read s))
    (setf *snapshot-taken-p* (read s))
    (setf *crew* (read s))
    (setf *captured-klingons* (read s))
    (setf *brig-capacity* (read s))
    (setf *brig-free* (read s))
    (setf *cloakedp* (read s))
    (setf *cloakingp* (read s))
    (setf *cloaking-violations* (read s))
    (setf *cloaking-violation-reported-p* (read s))
    (setf *remaining-klingons* (read s))
    (setf *remaining-super-commanders* (read s))
    (setf *remaining-resources* (read s))
    (setf *remaining-time* (read s))
    (setf *remaining-romulans* (read s))
    (setf *destroyed-bases* (read s))
    (setf *destroyed-stars* (read s))
    (setf *destroyed-inhabited-planets* (read s))
    (setf *destroyed-uninhabited-planets* (read s))
    (setf *planet-information* (read s))
    (setf *stardate* (read s))
    (setf *base-quadrants* (read s))
    (setf *commander-quadrants* (read s))
    (setf *super-commander-quadrant* (read s))
    (setf *galaxy* (read s))
    (setf *starchart* (read s))
    (setf *snapshot* (read s))
    (setf *quadrant-contents* (read s))
    (setf *klingon-energy* (read s))
    (setf *klingon-distance* (read s))
    (setf *klingon-average-distance* (read s))
    (setf *klingon-sectors* (read s))
    (setf *tholian-sector* (read s))
    (setf *base-sector* (read s))
    (setf *base-under-attack-quadrant* (read s))
    (setf *abandoned-crew* (read s))
    (setf *casualties* (read s))
    (setf *calls-for-help* (read s))
    (setf *energy-barrier-crossings* (read s))
    (setf *in-shuttle-craft-p* (read s))
    (setf *dilithium-crystals-on-board-p* (read s))
    (setf *miningp* (read s))
    (setf *restingp* (read s))
    (setf *super-commander-attack-enterprise-p* (read s))
    (setf *super-commander-attacking-base* (read s))
    (setf *shuttle-craft-location* (read s))
    (setf *shuttle-craft-quadrant* (read s))
    (setf *alivep* (read s))
    (setf *action-taken-p* (read s))
    (setf *game-won-p* (read s))
    (setf *all-done-p* (read s))
    (setf *ship* (read s))
    (setf *ship-energy* (read s))
    (setf *shield-energy* (read s))
    (setf *shields-are-changing-p* (read s))
    (setf *shields-are-up-p* (read s))
    (setf *life-support-reserves* (read s))
    (setf *torpedoes* (read s))
    (setf *warp-factor* (read s))
    (setf *device-damage* (read s))
    (setf *just-in-p* (read s))
    (setf *klingons-here* (read s))
    (setf *commanders-here* (read s))
    (setf *super-commanders-here* (read s))
    (setf *romulans-here* (read s))
    (setf *planet-coord* (read s))
    (setf *enemies-here* (read s))
    (setf *romulan-neutral-zone-p* (read s))
    (setf *dockedp* (read s))
    (setf *in-orbit-p* (read s))
    (setf *landedp* (read s))
    (setf *attempted-escape-from-super-commander-p* (read  s))
    (setf *tholians-here* (read s))
    (setf *base-attack-report-seen-p* (read s))
    (setf *current-planet* (read s))
    (setf *condition* (read s))
    (setf *time-taken-by-current-operation* (read s))
    (setf *last-chart-update* (read s))
    (setf *crystal-work-probability* (read s))
    (setf *probe-reported-quadrant* (read s))
    (setf *probe-x-coord* (read s))
    (setf *probe-y-coord* (read s))
    (setf *probe-x-increment* (read s))
    (setf *probe-y-increment* (read s))
    (setf *moves-for-probe* (read s))
    (setf *probes-available* (read s))
    (setf *probe-is-armed-p* (read s))
    (setf *height-of-orbit* (read s))
    (setf *thing-location* (read s))
    (setf *things-here* (read s))
    (setf *thing-is-angry-p* (read s))
    (setf *score* (read s))
    (setf *seed* (read s))))

;; TODO - add a file format version number to check when the file is read back in.
;; TODO - put the parameters to be saved in a list and iterate over the list.
(defun checkpoint-game (&optional (file-name +checkpoint-file-name+)) ; C: freeze(bool boss)
  "Save the game. This function is called to save the game after every iteration of the command
loop, in effect continuously saving the current state of the game."

  (with-open-file (s file-name :direction :output :if-exists :supersede)
    (print events::*future-events* s)
    (print *random-state* s)
    (print *line-tokens* s)
    (print *input-item* s)
    (print *ship-quadrant* s)
    (print *ship-sector* s)
    (print *conquest-quadrant* s)
    (print *tournament-number* s)
    (print *game-length* s)
    (print *skill-level* s)
    (print *self-destruct-password* s)
    (print *initial-energy* s)
    (print *initial-shield-energy* s)
    (print *initial-life-support-reserves* s)
    (print *initial-torpedos* s)
    (print *initial-klingons* s)
    (print *initial-bases* s)
    (print *initial-commanders* s)
    (print *initial-super-commanders* s)
    (print *initial-romulans* s)
    (print *initial-stars* s)
    (print *initial-planets* s)
    (print *initial-resources* s)
    (print *initial-time* s)
    (print *initial-stardate* s)
    (print *damage-factor* s)
    (print *snapshot-taken-p* s)
    (print *crew* s)
    (print *captured-klingons* s)
    (print *brig-capacity* s)
    (print *brig-free* s)
    (print *cloakedp* s)
    (print *cloakingp* s)
    (print *cloaking-violations* s)
    (print *cloaking-violation-reported-p* s)
    (print *remaining-klingons* s)
    (print *remaining-super-commanders* s)
    (print *remaining-resources* s)
    (print *remaining-time* s)
    (print *remaining-romulans* s)
    (print *destroyed-bases* s)
    (print *destroyed-stars* s)
    (print *destroyed-inhabited-planets* s)
    (print *destroyed-uninhabited-planets* s)
    (print *planet-information* s)
    (print *stardate* s)
    (print *base-quadrants* s)
    (print *commander-quadrants* s)
    (print *super-commander-quadrant* s)
    (print *galaxy* s)
    (print *starchart* s)
    (print *snapshot* s)
    (print *quadrant-contents* s)
    (print *klingon-energy* s)
    (print *klingon-distance* s)
    (print *klingon-average-distance* s)
    (print *klingon-sectors* s)
    (print *tholian-sector* s)
    (print *base-sector* s)
    (print *base-under-attack-quadrant* s)
    (print *abandoned-crew* s)
    (print *casualties* s)
    (print *calls-for-help* s)
    (print *energy-barrier-crossings* s)
    (print *in-shuttle-craft-p* s)
    (print *dilithium-crystals-on-board-p* s)
    (print *miningp* s)
    (print *restingp* s)
    (print *super-commander-attack-enterprise-p* s)
    (print *super-commander-attacking-base* s)
    (print *shuttle-craft-location* s)
    (print *shuttle-craft-quadrant* s)
    (print *alivep* s)
    (print *action-taken-p* s)
    (print *game-won-p* s)
    (print *all-done-p* s)
    (print *ship* s)
    (print *ship-energy* s)
    (print *shield-energy* s)
    (print *shields-are-changing-p* s)
    (print *shields-are-up-p* s)
    (print *life-support-reserves* s)
    (print *torpedoes* s)
    (print *warp-factor* s)
    (print *device-damage* s)
    (print *just-in-p* s)
    (print *klingons-here* s)
    (print *commanders-here* s)
    (print *super-commanders-here* s)
    (print *romulans-here* s)
    (print *planet-coord* s)
    (print *enemies-here* s)
    (print *romulan-neutral-zone-p* s)
    (print *dockedp* s)
    (print *in-orbit-p* s)
    (print *landedp* s)
    (print *attempted-escape-from-super-commander-p* s)
    (print *tholians-here* s)
    (print *base-attack-report-seen-p* s)
    (print *current-planet* s)
    (print *condition* s)
    (print *time-taken-by-current-operation* s)
    (print *last-chart-update* s)
    (print *crystal-work-probability* s)
    (print *probe-reported-quadrant* s)
    (print *probe-x-coord* s)
    (print *probe-y-coord* s)
    (print *probe-x-increment* s)
    (print *probe-y-increment* s)
    (print *moves-for-probe* s)
    (print *probes-available* s)
    (print *probe-is-armed-p* s)
    (print *height-of-orbit* s)
    (print *thing-location* s)
    (print *things-here* s)
    (print *thing-is-angry-p* s)
    (print *score* s)
    (print *seed* s)))

(defun get-game-type () ; C: choose()
  "Prompt for type of game and perform setup based on selected game type.
Return game type, tournament number, and whether or not this is a restored game."

  (do ((game-type nil)
       (tournament-number nil))
      (game-type
       (return-from get-game-type (values game-type tournament-number)))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Would you like a Regular or Tournament game? "))
    (scan-input)
    (setf game-type (match-token *input-item* (list "regular" "tournament")))
    (cond
      ((string= game-type "tournament")
       (unless *line-tokens*
         (print-prompt "Type in name or number of tournament: "))
       (scan-input)
       (if *input-item*
           (setf tournament-number *input-item*)
           (setf game-type nil))) ; no tournament name or number input so start over

      ((string= game-type "regular")
       t) ; Acknowledge the selection because any other input is an error.

      (t
       (when *input-item*
         (print-message (format nil "What is \"~A\"?~%" *input-item*)))
       (clear-type-ahead-buffer)))))

(defun get-game-length () ; C: choose()

  (do ((g-length nil))
      (g-length
       (setf *game-length* (make-game-length :label g-length))
       (cond
         ((string= g-length "short")
          (setf (game-length-value *game-length*) +short-game+))
         ((string= g-length "medium")
          (setf (game-length-value *game-length*) +medium-game+))
         ((string= g-length "long")
          (setf (game-length-value *game-length*) +long-game+))))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Would you like a Short, Medium, or Long game? "))
    (scan-input)
    (setf g-length (match-token *input-item* (list "short" "medium" "long")))
    (unless g-length
      (when *input-item*
        (print-message (format nil "What is \"~A\"?~%" *input-item*)))
      (clear-type-ahead-buffer))))

(defun get-skill-level () ; C: choose()

  (do ((game-skill nil))
      (game-skill
       (setf *skill-level* (make-skill-level :label game-skill))
       (cond
         ((string= game-skill "novice")
          (setf (skill-level-value *skill-level*) +novice+))
         ((string= game-skill "fair")
          (setf (skill-level-value *skill-level*) +fair+))
         ((string= game-skill "good")
          (setf (skill-level-value *skill-level*) +good+))
         ((string= game-skill "expert")
          (setf (skill-level-value *skill-level*) +expert+))
         ((string= game-skill "emeritus")
          (setf (skill-level-value *skill-level*) +emeritus+))))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Are you a Novice, Fair, Good, Expert, or Emeritus player? "))
    (scan-input)
    (setf game-skill (match-token *input-item* (list "novice" "fair" "good" "expert" "emeritus")))
    (unless game-skill
      (when *input-item*
        (print-message (format nil "What is \"~A\"?~%" *input-item*)))
      (clear-type-ahead-buffer))))

;; TODO - add an option to generate a password if the player can't be bothered to make one up
(defun get-game-password () ; C: setpassword()
  "Set the self-destruct password."

  (do ((game-password nil))
      (game-password
       (return-from get-game-password game-password))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Please type in a secret password: "))
    (scan-input)
    (when *input-item*
      (setf game-password *input-item*)
      (when (numberp game-password)
        (setf game-password (write-to-string game-password))))))

(defun set-up-new-game ()
  "Prepare to play - set up the universe. Use input parameters to generate initial core game
values, expecially number of entities in the game."

  (skip-line)
  ;; Choose game type, length, skill level, and password
  (multiple-value-bind (game-type tournament-number) (get-game-type)
    (get-game-length)
    (get-skill-level)
    (let ((game-password (get-game-password)))
      ;; Games restored from save files already have these.
      (setf *tournament-number* tournament-number)
      (when *tournament-number*
        (setf *random-state* (sb-ext:seed-random-state *tournament-number*)))
      (setf *self-destruct-password* game-password))
    (setf *damage-factor* (* 0.5 (skill-level-value *skill-level*)))
    (setf *initial-bases* (max (+ (random +max-bases+) 1) +min-bases+))
    (setf *initial-planets* (+ (random (- +max-uninhabitable-planets+ +min-uninhabitable-planets+))
                               +min-uninhabitable-planets+
                               +habitable-planets+))
    (setf *initial-romulans* (* (+ (random 1) 2) (skill-level-value *skill-level*)))
    (setf *remaining-romulans* *initial-romulans*)
    (setf *cloaking-violations* 0)
    (setf *initial-time* (* 7.0 (game-length-value *game-length*)))
    (setf *remaining-time* *initial-time*)
    (setf *initial-klingons*  (truncate (+ (* 2.0 *initial-time*
                                              (- (+ (skill-level-value *skill-level*) 1)  (random 1))
                                              (skill-level-value *skill-level*)
                                              0.1)
                                           0.15)))
    (setf *remaining-klingons* *initial-klingons*)
    (setf *captured-klingons* 0)
    (when (> *initial-klingons* 50) ; That's a lot of klingons, give the player another base
      (setf *initial-bases* (1+ *initial-bases*)))
    (setf *initial-commanders* (min +max-commanders-per-game+
                                    (+ (skill-level-value *skill-level*)
                                       (truncate (* 0.0626 *initial-klingons* (random 1.0))))))
    (if (> (skill-level-value *skill-level*) +fair+) ; higher skill levels get a super-commander
        (setf *initial-super-commanders* 1)
        (setf *initial-super-commanders* 0))
    (setf *remaining-super-commanders* *initial-super-commanders*)
    (setf *initial-resources* (* (+ *initial-klingons*
                                    (* 4 *initial-commanders*))
                                 *initial-time*))
    (setf *remaining-resources* *initial-resources*)

    ;; Prepare the Enterprise
    (setf *all-done-p* nil)
    (setf *game-won-p* nil)
    (setf *ship-quadrant* (get-random-quadrant))
    (setf *ship-sector* (get-random-sector))
    (setf *ship* +enterprise+)
    (setf *crew* +full-crew+)
    (setf *initial-energy* 5000.0)
    (setf *ship-energy* 5000.0)
    (setf *initial-shield-energy* 2500.0)
    (setf *shield-energy* 2500.0)
    (setf *shields-are-changing-p* nil)
    (setf *shields-are-up-p* nil)
    (setf *initial-life-support-reserves* 4.0)
    (setf *life-support-reserves* 4.0)
    (setf *initial-torpedos* 10)
    (setf *torpedoes* 10)
    ;; Give the player 2-4 of these wonders
    (setf *probes-available* (+ (random 2) 2))
    (setf *warp-factor* 5.0)
    (do ((i 0 (+ i 1)))
        ((>= i +number-of-devices+))
      (setf (aref *device-damage* i) 0.0))
    (setf *brig-capacity* 400)
    (setf *brig-free* *brig-capacity*)
    (setf *cloakedp* nil)
    (setf *cloakingp* nil)

    ;; Set up assorted game parameters
    ;; TODO - should this be before or after the algeron date?
    (setf *initial-stardate* (* 100.0 (+ (* 31.0 (random 1.0)) 20.0))) ; C: 100.0*(int)(31.0*Rand()+20.0)
    (setf *stardate* *initial-stardate*)
    (setf *abandoned-crew* 0)
    (setf *casualties* 0)
    (setf *calls-for-help* 0)
    (setf *energy-barrier-crossings* 0)
    (setf *in-shuttle-craft-p* nil)
    (setf *dilithium-crystals-on-board-p* nil)
    (setf *crystal-work-probability* 0.0)
    (setf *miningp* nil)
    (setf *restingp* nil)
    (setf *super-commander-attack-enterprise-p* nil)
    (setf *super-commander-attacking-base* 0)
    (setf *destroyed-inhabited-planets* 0)
    (setf *destroyed-uninhabited-planets* 0)
    (setf *destroyed-bases* 0)
    (setf *destroyed-stars* 0)
    (setf *shuttle-craft-location* 'on-ship)
    (setf *shuttle-craft-quadrant* nil)
    (setf *landedp* nil)
    (setf *alivep* t)
    (setf *galaxy* (make-array (list +galaxy-size+ +galaxy-size+)))
    (do ((i 0 (+ i 1)))
        ((>= i +galaxy-size+))
      (do ((j 0 (+ j 1)))
          ((>= j +galaxy-size+))
        (setf (aref *galaxy* i j) (make-quadrant :chartedp nil
                                                 :romulans 0
                                                 :klingons 0
                                                 :starbases 0
                                                 :supernovap nil
                                                 :status +secure+))))

    (initialize-events)
    ;; Initialize times for extraneous events
    (schedule-event +supernova+ (expran (* 0.5 *initial-time*))) ; C: expran(0.5 * game.intime)
    (schedule-event +tractor-beam+
                    (expran (* 1.5 (/ *initial-time* *initial-commanders*)))) ; C: expran(1.5 * (game.intime / game.state.remcom))
    (schedule-event +snapshot-for-time-warp+ (+ 1 (random 1))); Force an early snapshot, 1.0 + Rand()
    (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*))) ; C: expran(0.3*game.intime)
    (when (> *remaining-super-commanders* 0)
      (schedule-event +move-super-commander+ 0.2777))
    (when (>= (skill-level-value *skill-level*) +good+)
      (schedule-event +distress-call-from-inhabited-world+ (expran (+ 1.0 *initial-time*)))) ; C: expran(1.0 + game.intime)

    ;; Initialize the starchart
    (setf *starchart* (make-array (list +galaxy-size+ +galaxy-size+)))
    (do ((i 0 (+ i 1)))
        ((>= i +galaxy-size+))
      (do ((j 0 (+ j 1)))
          ((>= j +galaxy-size+))
        (setf (aref *starchart* i j) (make-starchart-page :stars 0
                                                          :starbases 0
                                                          :klingons 0))))

    ;; Starchart is functional but we've never seen it
    (setf *last-chart-update* +forever+)

    ;; Put stars in the galaxy
    (setf *initial-stars* 0)
    (let (sector-stars)
      (do ((x 0 (+ x 1)))
          ((>= x +galaxy-size+))
        (do ((y 0 (+ y 1)))
            ((>= y +galaxy-size+))
          (setf sector-stars (truncate (+ (* (random 1.0) +max-stars-per-quadrant+) 1))) ; C: Rand()*9.0 + 1.0
          (setf *initial-stars* (+ *initial-stars* sector-stars))
          (setf (quadrant-stars (aref *galaxy* x y)) sector-stars))))

    ;; Put starbases in the galaxy
    ;; Use an improved placement algorithm to spread out the bases.
    ;; For the total number of starbases, for each base to place pick a random quadrant and test
    ;; it for suitability. Tests are:
    ;;   The first base is always far enough from all others
    ;;   The quadrant doesn't already have a starbase
    ;;   The distance between the candidate quadrant and all other quadrants containing bases
    ;;   is below a calculated threshold. The threshold goes lower as the total number of bases
    ;;   goes higher.
    ;;   Randomly accept a base location even if it doesn't meet the distance threshold, and
    ;;   progressivly lower the random threshold for base acceptance if nothing suitable is found.
    ;; Also update the starchart - base locations are known at the start of a game.
    (setf *base-quadrants* ())
    (do ((i 0 (1+ i))
         ;; Original distance-threshold formula seems to produce values that are too large
         ;;(distance-threshold (* 6.0 (- (1+ +max-bases+) *initial-bases*)))
         (distance-threshold (- 8 *initial-bases*)))
        ((>= i *initial-bases*))
      (do (candidate-quadrant
           (candidate-ok-p nil)
           random-threshold)
          (candidate-ok-p
           (setf *base-quadrants* (append *base-quadrants* (list candidate-quadrant)))
           (setf (quadrant-starbases (coord-ref *galaxy* candidate-quadrant)) 1)
           (setf (starchart-page-starbases (coord-ref *starchart* candidate-quadrant)) 1))
        (setf candidate-quadrant (get-random-quadrant))
        (setf random-threshold 0.80)
        (setf candidate-ok-p t) ; Assume ok, then try to falsify the assumption
        (when (> i 0) ; The first base always succeeds
          (if (= (quadrant-starbases (coord-ref *galaxy* candidate-quadrant)) 0)
              (dolist (bq *base-quadrants*)
                ;; The original C did the following, which seems pointless:
                ;; (and (< (distance candidate-quadrant bq) distance-threshold) (<= 0.75 (random 1.0)))
                (when (or (< (distance candidate-quadrant bq) distance-threshold)
                          (> (random 1.0) random-threshold))
                  (setf candidate-ok-p nil)
                  ;; TODO - this doesn't seem to lower the threshold enough, some loops never finish
                  (setf random-threshold (- random-threshold 0.01))))
              (setf candidate-ok-p nil)))))

    ;; Put ordinary Klingon Battle Cruisers in the galaxy
    (let ((klingons-remaining *initial-klingons*)
          (klumper (+ (* 0.25 (skill-level-value *skill-level*) (- 9.0 (game-length-value *game-length*))) 1.0)))
      (when (> klumper +max-klingons-per-quadrant+)
        (setf klumper +max-klingons-per-quadrant+))
      (do (random-number klump)
          ((<= klingons-remaining 0))
        (setf random-number (random 1.0))
        (setf klump (truncate (* (- 1.0 (* random-number random-number)) klumper)))
        (when (> klump klingons-remaining)
          (setf klump klingons-remaining))
        (setf klingons-remaining (- klingons-remaining klump))
        (do ((q-coord (get-random-quadrant) (get-random-quadrant))
             (klingons-placed-p nil))
            (klingons-placed-p)
          (when (and (not (quadrant-supernovap (coord-ref *galaxy* q-coord)))
                     (<= (+ (quadrant-klingons (coord-ref *galaxy* q-coord))
                            klump)
                         +max-klingons-per-quadrant+))
            (setf (quadrant-klingons (coord-ref *galaxy* q-coord))
                  (+ (quadrant-klingons (coord-ref *galaxy* q-coord)) klump))
            (setf klingons-placed-p t)))))

    ;; Put Klingon Commanders in the galaxy
    ;; Select a random location for each commander to be placed based on these filters:
    ;; - If there is a supernova in the quadrant then nothing can be placed there
    ;; - If there are 9 klingons (max klingons per quadrant) there already then don't add another
    ;; - If there are less than 9 klingons then there is a 75% chance of adding the commander
    ;; - If there is already a commander in the quadrant then don't put another one there
    (setf *commander-quadrants* ())
    (do ((i 0 (+ i 1))
         c-coord)
        ((>= i *initial-commanders*))
      (setf c-coord (get-random-quadrant))
      (do (candidate-ok-p)
          (candidate-ok-p)
        (setf candidate-ok-p t)
        ;; Use short-circuit evaluation and a nested "and" to decide at what frequency the
        ;; commander will be placed in an empty quadrant
        (when (and (and (< (quadrant-klingons (coord-ref *galaxy* c-coord)) +max-klingons-per-quadrant+)
                        (< (random 1.0) 0.75))
                   (not (quadrant-supernovap (coord-ref *galaxy* c-coord)))
                   (< (quadrant-klingons (coord-ref *galaxy* c-coord)) +max-klingons-per-quadrant+))
          ;; check if a commander is already there - not nil means commander is there
          (when (position c-coord *commander-quadrants*)
            (setf candidate-ok-p nil))))
      (setf *commander-quadrants* (append *commander-quadrants* (list c-coord)))
      (setf (quadrant-klingons (coord-ref *galaxy* c-coord))
            (+ (quadrant-klingons (coord-ref *galaxy* c-coord)) 1)))

    ;; Put planets in the galaxy, one planet per quadrant whether inhabited or uninhabited
    (setf *planet-information* ())
    (do ((i 0 (1+ i))
         pl
         q)
        ((>= i *initial-planets*))
      (setf pl (make-planet))
      (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
          ((not (assoc q-coord *planet-information* :test #'coord-equal)) ; nil when planet does not exist
           (setf (planet-quadrant pl) q-coord)
           (setf q q-coord))) ; dumb code alert!
      (if (< i +habitable-planets+)
          (progn
            (setf (planet-class pl) +class-m+) ; All inhabited planets are class M
            (setf (planet-crystals pl) 'absent)
            ;; ; TODO - "knownp" should only change when planets are scanned, we don't know them in advance
            (setf (planet-knownp pl) t)
            (setf (planet-destroyedp pl) nil)
            (setf (planet-inhabitedp pl) t)
            (setf (planet-name pl) (aref *system-names* i)))
          (progn
            (setf (planet-class pl) (+ (random 2) 1)) ; Planet class M, N, or O
            ;;(setf (planet-crystals pl) (* (random 1.0) 1.5)) ; 1 in 3 chance of crystals
            (if (= (random 2) 0) ; 1 in 3 chance of crystals
                (setf (planet-crystals pl) 'present)
                (setf (planet-crystals pl) 'absent))
            (setf (planet-knownp pl) nil)
            (setf (planet-destroyedp pl) nil)
            (setf (planet-inhabitedp pl) nil)
            (setf (planet-name pl) "")))
      (setf *planet-information* (acons q pl *planet-information*)))

    ;; Put Romulans in the galaxy
    (do ((i 0 (+ i 1))
         (q-coord (get-random-quadrant) (get-random-quadrant)))
        ((>= i *remaining-romulans*)
         (setf (quadrant-romulans (coord-ref *galaxy* q-coord))
               (+ (quadrant-romulans (coord-ref *galaxy* q-coord)) 1))))

    ;; Put the Super Commander in the galaxy
    (when (> *remaining-super-commanders* 0)
      (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
          ((and (not (quadrant-supernovap (coord-ref *galaxy* q-coord)))
                (<= (quadrant-klingons (coord-ref *galaxy* q-coord)) 8))
           (setf *super-commander-quadrant* q-coord)
           (setf (quadrant-klingons (coord-ref *galaxy* q-coord))
                 (+ (quadrant-klingons (coord-ref *galaxy* q-coord)) 1)))))

    ;; Put the thing in the galaxy unless this is a tournament game.
    (setf *thing-location* nil)
    (when (string= game-type "regular")
      (setf *thing-location* (get-random-quadrant)))

    (setf *snapshot-taken-p* nil)

    ;; Introduce the player to the current situation.
    (skip-line 2)
    (if (= (skill-level-value *skill-level*) +novice+)
        (progn
          (print-message (format nil "It is stardate ~A. The Federation is being attacked by~%"
                                 (format-stardate *stardate*)))
          (print-message (format nil "a deadly Klingon invasion force. As captain of the United~%"))
          (print-message (format nil "Starship U.S.S. Enterprise, it is your mission to seek out~%"))
          (print-message (format nil "and destroy this invasion force of ~A battle cruisers.~%"
                                 (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
          (print-message (format nil "You have an initial allotment of ~A stardates to complete~%"
                                 (truncate *initial-time*)))
          (print-message (format nil "your mission. As you proceed you may be given more time.~%"))
          (skip-line)
          (print-message (format nil "You will have ~A supporting starbases.~%" *initial-bases*))
          (print-message "Starbase locations-  "))
        (progn
          (print-message (format nil "Stardate ~A.~%" (truncate *stardate*)))
          (skip-line)
          (print-message (format nil "~A klingons.~%"
                                 (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
          (print-message (format nil "An unknown number of Romulans.~%"))
          (when (> *remaining-super-commanders* 0)
            (print-message (format nil "And one (GULP) Super-Commander.~%")))
          (print-message (format nil "~A stardates.~%" (truncate *initial-time*)))
          (print-message (format nil "~A starbases in " *initial-bases*))))
    (dolist (bq *base-quadrants*)
      (print-message (format nil "~A  " (format-coordinates bq))))
    (skip-line 2)
    (print-message (format nil "The Enterprise is currently in ~A ~A~%"
                           (format-quadrant-coordinates *ship-quadrant*)
                           (format-sector-coordinates *ship-sector*)))
    (skip-line)
    (print-message "Good Luck!")
    (when (> *remaining-super-commanders* 0)
      (print-message " YOU'LL NEED IT."))
    (skip-line 2)
    (new-quadrant :show-thing nil)
    (when (> (- *enemies-here* *things-here* *tholians-here*) 0)
      (setf *shields-are-up-p* t))
    (when *romulan-neutral-zone-p*
      (attack-player))))

(defun check-treaty-of-algeron ()
  "Check if the player has violated the treaty of Algeron, and if so update the player on the
consequences."

  (when (and (> *romulans-here* 0)
             (> *stardate* +algeron-date+)
             (not *cloaking-violation-reported-p*)
             *cloakedp*)
    (print-message (format nil "The Romulan ship discovers you are breaking the Treaty of Algeron!~%"))
    (setf *cloaking-violations* (1+ *cloaking-violations*))
    (setf *cloaking-violation-reported-p* t)))

(defun print-help-topics (help-topics)
  "Print a list of help topics."

  (restart-message-window-paging)
  (skip-line)
  (print-message (format nil "Available help topics are:~%"))
  (skip-line)
  (let ((longest-topic-title 0))
    (dolist (topic help-topics)
      (when (> (length topic) longest-topic-title)
	(setf longest-topic-title (length topic))))
    ;; TODO - how to eliminate the hard-coded 17?
    ;; Arrange the help topics into columns and the columns into lines, then display one line at a
    ;; time.
    (dolist (topic-line (split-sequence #\newline (format nil "~{~17@<~%~:;~A ~>~}" help-topics)))
      (print-message (format nil "~A~%" topic-line))))
  (skip-line))

(defun display-online-help () ; C: helpme(void)
  "Browse on-line help."

  (let (help-topics
        topic)
    (setf help-topics (mapcar #'first *help-database*))
    (when (> (length *line-tokens*) 0)
      (scan-input)
      (setf topic (match-token *input-item* help-topics)))
    ;; Prompt if no topic supplied, or a second try if the supplied topic didn't exist
    (when (= (length topic) 0)
      (print-help-topics help-topics)
      (print-prompt "Help on what command or topic? ")
      (clear-type-ahead-buffer)
      (scan-input)
      (setf topic (match-token *input-item* help-topics)))
    (let (contents)
      (setf contents (rest (assoc topic *help-database* :test #'string=)))
      (skip-line)
      (when (> (length contents) 0)
        (restart-message-window-paging)
        (print-message (format nil "Spock- \"Captain, I've found the following information:\"~%"))
        (skip-line)
        (dolist (content-line (split-sequence #\newline contents))
          (print-message (format nil "~A~%" content-line))))
      (when (and (= (length contents) 0)
                 (> (length topic) 0))
        (print-message (format nil "Spock- \"Captain, there is no information on that command.\"~%"))))))

(defun display-commands (commands) ; C: listCommands(void)
  "Print a list of valid commands."

  (print-message (format nil "Valid commands are:~%"))
  (skip-line)
  (dolist (command-line (split-sequence #\newline (format nil "~{~12@<~%~:;~A ~>~}" commands)))
    (print-message (format nil "~A~%" command-line))))

(defun make-moves ()
  "Command interpretation loop. This is the heart of the game: read commands and carry them out.
The loop ends when the player wins by killing all Klingons, is killed, or decides to exit or quit."

  ;; Commands that should match the shortest possible string come first in the list.
  (do ((exit-game-p nil)
       (commands (list "abandon" "chart" "capture" "cloak" "commands" "computer" "crystals" "dock"
                       "damages" "deathray" "destruct" "emexit" "exit" "help"
                       "impulse" "lrscan" "move" "mayday" "mine" "orbit" "phasers" "photons"
                       "planets" "probe" "quit" "rest" "report" "request" "srscan" "status" "score"
                       "sensors" "shields" "shuttle" "transport" "torpedoes" "visual" "warp"))
       command
       hit-me-p) ;; When true, player has taken an action which consumes game time or a turn,
                 ;; after which enemies take their action turn, usually an attack.
      ((or *all-done-p*
           exit-game-p))
    (when *window-interface-p*
      (draw-windows))
    (skip-line)
    (restart-message-window-paging)
    (setf hit-me-p nil)
    (setf *just-in-p* nil)
    (setf *time-taken-by-current-operation* 0.0)
    (setf *action-taken-p* nil)
    (print-prompt "COMMAND: ")
    (clear-type-ahead-buffer)
    (scan-input)
    (setf command (match-token *input-item* commands)) ; TODO - fix match-token to ignore all previous input on -1
    ;; TODO - check that commands that must be typed in full were: abandon destruct quit deathray cloak
    (cond
      ((string= command "abandon")
       (abandon-ship))
      ((string= command "capture") ; Attempt to get Klingon ship to surrender
       (capture)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "chart")
       (chart))
      ((string= command "cloak") ; Turn on/off cloaking
       (cloak)
       ;; TODO - can this when form be moved to the cloak function?
       (when *cloakingp*
         (attack-player :torpedoes-ok-p t) ; We will be seen while we cloak
         (setf *cloakingp* nil)
         (when (not (damagedp +cloaking-device+)) ; Don't cloak if we got damaged while cloaking!
           (setf *cloakedp* t))))
      ((string= command "commands")
       (display-commands commands))
      ((string= command "computer")
       (calculate-eta))
      ((string= command "crystals")
       (use-crystals)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "damages")
       (damage-report))
      ((string= command "deathray") ; Try a desperation measure
       (deathray)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "destruct")
       (self-destruct))
      ((string= command "dock")
       (dock)
       (when *action-taken-p*
         (attack-player)))
      ((string= command "emexit") ; Emergency exit, first in the command list so it's quick to enter.
       (clear-screen)             ; Hide the screen
       (sb-ext:exit))             ; A quick exit to the command line
      ((string= command "exit") ; Ordinary game exit. Like emexit but there's nothing to hide.
       (setf exit-game-p t))
      ((string= command "help") ; Get help and information
       (display-online-help))
      ((string= command "impulse")
       (move-under-impulse-power))
      ;; TODO - add load command
      ((string= command "lrscan")
       (long-range-scan))
      ((string= command "mayday") ; Call for help
       (mayday)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "mine")
       (mine)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "move")
       (move-under-warp-drive))
      ((string= command "orbit")
       (orbit)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "phasers")
       (fire-phasers)
       (when *action-taken-p*
         (check-treaty-of-algeron) ; TODO - can this be done in fire-phasers
         (setf hit-me-p t)))
      ((string= command "planets")
       (survey))
      ((string= command "probe")
       (launch-probe)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "quit") ; Quit the game currently in progress
       (setf *all-done-p* t))
      ((string= command"report" )
       (report))
      ((string= command "request")
       (request))
      ((string= command "rest")
       (wait)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ;; TODO - add save command
      ((string= command "score")
       (score))
      ((string= command "sensors")
       (sensor))
      ((string= command "shields")
       (shield-actions)
       (when *action-taken-p*
         (setf hit-me-p t)
         (setf *shields-are-changing-p* nil)))
      ((string= command "shuttle")
       (shuttle)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "srscan")
       (short-range-scan))
      ((string= command "status")
       (all-statuses))
      ((string= command "photons")
       (fire-photon-torpedoes)
       (when *action-taken-p*
         (check-treaty-of-algeron) ; TODO - can this be done in fire-photon-torpedoes?
         (setf hit-me-p t)))
      ((string= command "transport")
       (beam))
      ((string= command "visual")
       (visual-scan)
       (when *action-taken-p*
         (setf hit-me-p t)))
      ((string= command "warp")
       (set-warp-factor))
      (t ; Unsuable input, print a list of valid commands.
       (display-commands commands)))

    ;; Give the game a turn after the player completes theirs: invoke scheduled events and give
    ;; enemies their turn to attack.
    ;; There can be a chain of reactions, for example an attacking enemy causes a star to go
    ;; supernova, which causes an emergency exit of the quadrant, and the quadrant escaped to also
    ;; has a supernova, and when the second emergency exit is in progress a scheduled tractor beam
    ;; event occurs.
    (do ((turn-ended-p nil))
        ((or *all-done-p*
             turn-ended-p
             exit-game-p)) ; Game has ended or the turn is over.
      (when (/= *time-taken-by-current-operation* 0.0)
        (process-events))
      (unless *all-done-p* ; Scheduled events may have resulted in the game ending.
        (cond
          ((quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
           (emergency-supernova-exit)) ; Supernova event scheduled or player landed in a supernova quadrant

          ((and hit-me-p
                (not *just-in-p*))
           (attack-player)
           (unless *all-done-p*
             (if (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
                 (emergency-supernova-exit) ; Attacker caused a supernova
                 (setf turn-ended-p t))))

          (t
           (setf turn-ended-p t)))))
    (checkpoint-game)))

(defun print-banner () ; C: prelim()
  "Print an introductory banner."

  (clear-message-window)
  (print-message (format nil "-SUPER- STAR TREK~%")))

;; TODO - record and play back games.
(defun sst () ; C: main()
  "Super Star Trek starting function."

  (initialize) ; C: iostart()
  (clear-screen)
  (print-banner)

  (when (probe-file +checkpoint-file-name+)
    (restore-game-from-checkpoint)
    (unless *all-done-p* ; If there's a game in progress tell the player about it.
      (report)))

  (do ((play-again-p t))
      ((not play-again-p))

    (when *all-done-p*
      (set-up-new-game)
      ;; If there is no checkpoint file then this might be the first time this player has
      ;; played. Give them a little help.
      (unless (probe-file +checkpoint-file-name+)
        (skip-line)
        (print-message (format nil "Enter `help' for help, `commands' for commands.~%"))
        (skip-line)))
    (make-moves)
    (setf play-again-p nil)

    (skip-line)
    (print-stars)
    (when *all-done-p*
      (skip-line)
      (print-prompt "Do you want to play again? ")
      (setf play-again-p (get-y-or-n-p))))

  (clean-up)
  (format t "~%May the Great Bird of the Galaxy roost upon your home planet.~%")
  (sb-ext:exit))
