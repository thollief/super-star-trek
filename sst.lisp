;;; TODO - Add the license from the original sources and any other licenses mentioned

;;;; Possible enhancements
;;;; 1. Offer a randomized password instead of requiring user input. The C source for this port
;;;;    required a password if a "plain" game was selected and generated a random password if a
;;;;    "fancy" game was selected.
;;;; 2. BSD trek has other features that might be interesting
;;;; 3. The star chart could display more information, e.g. planets. Some ways to do that: add a
;;;;    digit, display 2 rows of digits per quadrant. 1 row: natural features stars, planets,
;;;;    supernova, 2nd row enemies, starbases, inhabited planets? Would like to preserve
;;;;    compatibility with glass terminals.

;;;; TOTO - don't just jump into the game. Create a "lobby" where the player can read help, set
;;;;        game parameters such as debug mode, and other things that aren't gameplay. On the other
;;;;        hand this changes the character of the game somewhat by creating a second interface.
;;;; TODO - embed the help text in this source code. Possibly a package and a "help mode".
;;;; TODO - add credits for all developers
;;;; TODO - implement continuous save

;;;; Comments reference names in the C source with "C:"
;;;; TODO remove the original C names from the C source if/when they are no longer needed

(ql:quickload :cl-charms) ; ncurses support
(ql:quickload :cl-utilities)

(defpackage super-star-trek
  (:documentation "Eric Raymond's SST2K C version of Super Star Trek ported to Common Lisp.
This is a faithless port. In particular preserving historical options is not a desgin goal and
most options are assumed to be enabled.

Choice of user interface has been kept as a basic design feature.

Comments from the ESR C source about the history and evolution of the game have been moved to
the help system.
Set game options. All gameplay options are enabled, this version doesn't attempt historical
authenticity.

ncurses and virtual ttys are supported for output. This implmentation assumes virtual
terminal emulators running in a window with a scrollbar, giving an output review capability
similar to continuous feed paper terminals.

Localization support from the C source has been removed.")
  (:use common-lisp common-lisp-user)
  (:import-from cl-charms/low-level
                ;; Having difficulty getting getstr, getnstr, wgetstr, wgetnstr to work on FreeBSD 12.2
                a_bold a_reverse color_black color_blue color_cyan color_green color_magenta
                color_red color_white color_yellow key_eol key_backspace
                *lines* *stdscr* true false err ok
                box ; TODO not sure if this is only debug or permanent
                color-pair cbreak curs-set endwin getcury getmaxy getyx has-colors init-pair initscr
                keypad mvwaddstr newwin nocbreak nonl scrollok start-color waddch waddstr wattron
                wattrset wclear wclrtoeol wgetch wmove wprintw wrefresh)
  (:import-from cl-utilities
                split-sequence)
  (:nicknames sst)
  (:export sst))

(in-package sst)

;;; sst.h

;; Game options
(defparameter *tty-interface-p* t) ; C: OPTION_TTY 0x00000001	/* old interface */
(defparameter *curses-interface-p* t) ; C: OPTION_CURSES 0x00000002 /* new interface */
(defparameter *planets-and-mining-p* t) ; C: OPTION_PLANETS 0x00000004 /* planets and mining */
(defparameter *tholians-p* t) ; C: OPTION_THOLIAN 0x00000008 /* Tholians and their webs */
(defparameter *thing-shoots-p* t) ; C: OPTION_THINGY 0x00000010 /* Space Thingy can shoot back */
(defparameter *deep-space-probes-p* t) ; C: OPTION_PROBE 0x00000020 /* deep-space probes */
(defparameter *highlight-enterprise-p* t) ; C: OPTION_SHOWME 0x00000040 /* bracket Enterprise in chart */
(defparameter *enemies-can-ram-p* t) ; C: OPTION_RAMMING 0x00000080 /* enemies may ram Enterprise */
(defparameter *more-enemies-move-p* t) ; C: OPTION_MVBADDY 0x00000100 /* more enemies can move */
(defparameter *black-hole-time-warp-p* t) ; C: OPTION_BLKHOLE 0x00000200 /* black hole may timewarp you */
(defparameter *good-base-shields-p* t) ; C: OPTION_BASE 0x00000400 /* bases have good shields */
(defparameter *inhabited-worlds-p* t) ; C: OPTION_WORLDS 0x00000800 /* logic for inhabited worlds */
(defparameter *plain-game-p* t) ; C: OPTION_PLAIN 0x01000000 /* user chose plain game */

(defconstant +phasefac+ 2.0) ; C: phasefac
(defconstant +galaxy-size+ 8) ; C: GALSIZE
(defconstant +habitable-planets+ (/ (* +galaxy-size+ +galaxy-size+) 2)) ; C: NINHAB
(defconstant +max-uninhabitable-planets+ 10) ; C: MAXUNINHAB
(defconstant +min-uninhabitable-planets+ 5)
(defconstant +planet-max+ (+ +habitable-planets+ +max-uninhabitable-planets+)) ; C: PLNETMAX
(defconstant +quadrant-size+ 10) ; C: QUADSIZE
;; 12 looks like a made-up number to get the result close to 5
;; In C, (GALSIZE * GALSIZE / 12), and then 1 is added when creating arrays.
;; Use floor because C silently coerces double to int, lisp does not.
(defconstant +max-bases+ (floor (/ (* +galaxy-size+ +galaxy-size+) 12))) ; C: BASEMAX
(defconstant +min-bases+ 2)
(defconstant +max-klingons-per-game+ 127) ; C: MAXKLGAME
;; Maximum number of stars and klingons per quadrant is limited by the starchart display format
(defconstant +max-klingons-per-quadrant+ 9) ; C: MAXKLQUAD
(defconstant +max-commanders-per-game+ 10)
(defconstant +max-stars-per-quadrant+ 9)
(defconstant +docked-repair-factor+ 0.25) ; C: docfac, repair factor when docking

(defparameter *tty-rows* 24) ; Assume 24 line TTY, TODO - if it's really necessary in TTY mode
(defparameter *current-window* nil) ; C: curwnd
(defparameter *short-range-scan-window* nil) ; C: srscan_window
(defparameter *report-window* nil)
(defparameter *status-window* nil)
(defparameter *long-range-scan-window* nil) ; C: lrscan_window
(defparameter *message-window* nil)
(defparameter *prompt-window* nil)

(defun set-window (window)
  "Change window. Do nothing in TTY mode."
  (when *curses-interface-p*
    (setf *current-window* window)
    (when (or (equal *message-window* window) ; make cursor visible if selecting one of these windows
              (equal *prompt-window* window))
      (curs-set 1)))) ; set cursor visibility 0 = invisible, 1 = normal visibility, 2 = very visible

(defconstant +max-line-length+ 128)
(defparameter *line* nil) ; C: char line[128], *linep = line, TODO - is this used?
(defparameter *line-tokens* nil) ; List of input tokens
;; TODO - do *aaitem* and *citem* need to be global? Or even exist? *input-item* replaces them.
(defparameter *aaitem* nil) ; C: double aaitem
(defparameter *citem* nil) ; C: char citem[12]
(defparameter *input-item* nil) ; One space-separated token from the keyboard

;; TODO - C source also stores the input line in the replay file.
;; TODO - fix error: backtick (`) causes an error
(defun get-input-line () ; C: cgetline(char *line, int max)
  "Get a line of input from the keyboard as a string. Remove leading and trailing spaces, and
lowercase everything."
  (let (line)
  (when *curses-interface-p*
    ;; wgetstr and related ncurses functions from charms/ll don't seem to work. Simulate with wgetch.
    (do ((input-char 0 (wgetch *prompt-window*)))
        ((= input-char 13)) ; ASCII carriage return. This seems like the wrong way to do it...
      ;(print-message (format nil "key is [~A]" input-char))
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
  (when (not *curses-interface-p*) ; assume tty
    (setf line (read-line))) ; C: fgets(line, max, stdin) - input length isn't limited here, use read-char if needed
  (setf line (string-downcase (string-trim " " line)))))

(defun scan-input ()
  "Manage a list of input tokens, returning one token each time this function is called. If the
list is empty then get input from the keyboard and split it on spaces to generate tokens. Return
the first token and save the remainder for the next call. If the keyboard input didn't include
any non-space characters then return nil."

  (when (= (length *line-tokens*) 0)
    (setf *line-tokens* (split-sequence #\SPACE (get-input-line) :remove-empty-subseqs t)))
    ;(when (equal *current-window* *prompt-window*) ; TODO - Why clear both only if the current window is the prompt window?
    ;  (clear-window)
    ;  (clear-message-window))
  (let ((test-number nil))
    (setf *input-item* (pop *line-tokens*))
    (when *input-item*
      (setf test-number (read-from-string *input-item*)) ; Read something, possibly a number
      (when (numberp test-number) ; If it's a number then keep it
        (setf *input-item* test-number)))))

(defun clear-type-ahead-buffer () ; C: chew()
  "Convenience function to delete any commands the player may have entered in anticipation of
future prompts. This is normally called after an input error or other condition where the future
prompts change for some reason."

  (setf *line-tokens* nil))

(defun match-token (token possible-matches) ; C: isit()
  "Given a string and a list of possible matching strings, return the first item from the list
that matches. A match occurs when the first characters of both strings, up to the length of the
shortest string, are the same."

  (let ((token-length (length token))
        length-to-compare)
    (dolist (candidate possible-matches)
      (setf length-to-compare (min token-length
                                   (length candidate)))
      (when (string= (subseq token 0 length-to-compare)
                     (subseq candidate 0 length-to-compare))
        (return-from match-token candidate)))))

(defun print-out (string-to-print)
  "Print a string. In curses mode use the current window. Output strings without end of line
characters."

  (if *curses-interface-p*
      (progn
        (waddstr *current-window* string-to-print)
        (wrefresh *current-window*))
      (progn
        (format t "~A" string-to-print)
        (finish-output))))

;; TODO - implement slow printing
(defun print-out-slowly (string-to-print) ; C: prouts
  "Like print-out but with a delay between each character to build dramatic tension. Slow printing
was not optional on early paper-based terminals."

  (print-out string-to-print))

(defun skip-line (&optional (lines-to-skip 1))
  "Add blank lines at the bottom of the screen or window, pausing the display if the window is full."

  (if *curses-interface-p*
      ;(if (and (equal *current-window* *message-window*)
      ;         (>= (getcury *current-window*)
      ;             (- (getmaxy *current-window*) 3)))
      ;    (progn
      ;      (pause-display)
      ;      (clear-window))
      (progn
        (dotimes (x lines-to-skip)
          (wprintw *current-window* (format nil "~%")))
        (wrefresh *current-window*) ; this is for debugging? Or needed permanently?
      )
                                        ;    )
      (progn
        (dotimes (x lines-to-skip)
          (print-out (format nil "~%")))
        (finish-output)))) ; assume *tty-interface-p* is true

(defun print-message (message-to-print)
  "Print a string with end of line character. In curses mode print it in the message window, otherwise just print it."

  (when *curses-interface-p*
    (set-window *message-window*))
  (print-out message-to-print)
  (skip-line))

;; TODO - implement slow printing
(defun print-message-slowly (message-to-print) ; C: prouts()
  "Like print-message but with a delay between each character to build dramatic tension."

  (when *curses-interface-p*
    (set-window *message-window*))
  (print-out-slowly message-to-print)
  (skip-line))

(defun print-stars ()
  "Print a line of stars."

  (print-message-slowly "******************************************************"))

(defun huh () ; C: huh(void)
  "Complain about unparseable input."

  (clear-type-ahead-buffer)
  (skip-line)
  (print-message "Beg your pardon, Captain?"))

(defun get-y-or-n () ; C: ja()
  "When a player choice may negatively affect successful completion of the game then require a yes
or no answer. If other answers are given then prompt again. Use this function instead of y-or-n-p
to allow for curses or TTY output when the player is reminded of the input options."

  (clear-type-ahead-buffer)
  (do ((char nil))
      (char)
    (scan-input)
    (setf char (match-token *input-item* (list "yes" "no")))
    (cond ((string= char "yes")
           (return-from get-y-or-n t))
          ((string= char "no")
           (return-from get-y-or-n nil))
          (t
           (skip-line)
           (print-out "Please answer with \"y\" or \"n\": ")))))

;; TODO - Can/should coordinate handling be an object or package?
;; TODO - Coordinates are integers. C used implicit typecasting to convert double to int.
;;        What is appropriate here?
(defstruct coordinate ; C: coord
  "Sector or quadrant coordinate pair. These are array indices, so one less that the coordinate
values displayed to the player."

  x
  y)

;; TODO - should this be a property of a ship struct?
(defparameter *current-quadrant* nil) ; C: coord quadrant, where we are
(defparameter *current-sector* nil) ; C: coord sector, where we are

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

(defun valid-sector-p (x y) ; C: VALID_SECTOR(x, y)
  (and (numberp x)
       (numberp y)
       (>= x 0)
       (< x +quadrant-size+)
       (>= y 0)
       (< y +quadrant-size+)))

(defun same-coordinate-p (c1 c2) ; C: same(c1, c2)
  (and (= (coordinate-x c1) (coordinate-x c2))
       (= (coordinate-y c1) (coordinate-y c2))))

;; TODO - this doesn't calculate distance between sectors in different quadrants. Adding that
;; capability could be done by converting to a global coordinate system internally. The need for
;; the calculation came up in the mayday function.
(defun distance (c1 c2) ; C: distance(c1, c2)
  (sqrt(+ (expt (- (coordinate-x c1) (coordinate-x c2)) 2)
          (expt (- (coordinate-y c1) (coordinate-y c2)) 2))))

;; TODO - Using nil instead so it may be possible to just delete these two functions.
(defun invalidate-coordinate (c) ; C: invalidate(w)
  "Make a coordinate invalid by setting all values to zero."
  (setf (coordinate-x c) 0)
  (setf (coordinate-y c) 0))

(defun valid-coordinate-p (c) ; C: is_valid(w)
  (and (/= (coordinate-x c) 0)
       (/= (coordinate-y c) 0)))

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
      (setf qx (coordinate-x *current-quadrant*))
      (setf qy (coordinate-y *current-quadrant*)))
    ;; Validate coordinate values and return them
    (if (and qx qy (valid-quadrant-p qx qy) sx sy (valid-sector-p sx sy))
        (return-from get-quadrant-and-sector (values sx sy qx qy))
        (huh))))

(defun format-stardate (d)
  "Write a stardate with one decimal point."

  (format nil "~,1,,,F" d))

(defconstant +short-game+ 1)
(defconstant +medium-game+ 2)
(defconstant +long-game+ 4)
(defstruct game-length
  value ; Numeric values 1, 2, or 4. Used to initialize gameplay parameters.
  label) ; The textual representation of the game length: short, medium, long

;; Skill levels are ordered, each higher than the previous, and also used as multipliers when
;; setting initial game values.
(defconstant +novice+ 1) ; C: skill, SKILL_NOVICE = 1
(defconstant +fair+ 2) ; C: skill, SKILL_FAIR = 2
(defconstant +good+ 3) ; C: skill, SKILL_GOOD = 3
(defconstant +expert+ 4) ; C: skill, SKILL_EXPERT = 4
(defconstant +emeritus+ 5) ; C: skill, SKILL_EMERITUS = 5
(defstruct skill-level
  value ; Numeric skill level used to initialize gameplay parameters
  label) ; The textual representaion of the skill level: novice, fair, good, expert, emeritus


(defconstant +destroyed+ -1)
(defconstant +class-m+ 1)
(defconstant +class-n+ 2)
(defconstant +class-o+ 3)
(defconstant +mined+ -1)
(defconstant +present+ 0)
(defconstant +absent+ 1)

;; Values for the 'known' slot in the planet struct
(defconstant +unknown+ 0)
(defconstant +known+ 1)
(defconstant +shuttle-down+ 2)

;; Value for the 'inhabited' slot in the planet struct
(defconstant +uninhabited+ -1) ; C: UNINHABITED

;; TODO - location of the shuttle shouldn't be a property of the planet.
;; TODO - the element 'inhabited' is overloaded, can name be a separate item?
;; TODO - the element 'class' is overloaded, 'destroyed' should be a different property
(defstruct planet ; C: planet
  "Information about a planet."
  quadrant ; C: w, a coordinate
  class ; C: pclass, one of M, N, or O (1, 2, or 3), or -1 for destroyed planets, TODO - use symbols
  inhabited ; C: inhabited, If non-zero then an index into a name array. If -1 then uninhabited
  crystals ; has crystals, mined=-1, present=0, absent=1, TODO - use symbols
  known ; unknown = 0, known = 1, shuttle_down = 2, TODO - use symbols instead of numbers
  shuttle-landed-p) ; whether or not the shuttle has landed on the planet

;; TODO - the C source used an array lookup to return the letter belonging to the number.
;;        Change the data structure to just store the class letter.
(defun format-planet-class (class)
  "Given a numeric value for the class of a planet, return the single letter equivalent, or an
empty string if the planet class can't be determined."

  (cond ((= class +class-m+)
         "M")
        ((= class +class-n+)
         "N")
        ((= class +class-o+)
         "O")
        (t
         "")))

;; Characters displayed for game entities in short range scans
;; TODO - are probes visible in short range scans? Should they be?
;; C: typedef enum {} feature
(defconstant +romulan+ "R") ; C: IHR
(defconstant +klingon+ "K") ; C: IHK
(defconstant +commander+ "C") ; C: IHC
(defconstant +super-commander+ "S") ; C: IHS
(defconstant +star+ "*") ; C: IHSTAR
(defconstant +planet+ "P") ; C: IHP
(defconstant +world+ "@") ; C: IHW
(defconstant +starbase+ "B") ; C: IHB
(defconstant +black-hole+ " ") ; C: IHBLANK
(defconstant +empty-sector+ ".") ; C: IHDOT
(defconstant +thing+ "?") ; C: IHQUEST
(defconstant +enterprise+ "E") ; C: IHE
(defconstant +faerie-queene+ "F") ; C: IHF
(defconstant +no-ship+ "U") ; Any unused letter, only needs to be a string type
(defconstant +tholian+ "T") ; C: IHT
(defconstant +tholian-web+ "#") ; C: IHWEB
(defconstant +materialize-1+ "-") ; C: IHMATER0
(defconstant +materialize-2+ "o") ; C: IHMATER1
(defconstant +materialize-3+ "0") ; C: IHMATER2
(defconstant +reserved+ "X")

(defconstant +full-crew+ 428) ; C: FULLCREW, BSD Trek was 387, that's wrong
(defconstant +no-planet+ -1) ; C: NOPLANET, array index to *planet-information* when there is no planet in the quadrant
(defconstant +secure+ 0)
(defconstant +distressed+ 1)
(defconstant +enslaved+ 2)

;; TODO - define a ship structure. Ships have several properties: a label, a short range scan
;;        symbol, a location, dilithium crystals on board, energy, photon torpedoes, a shuttle
;;        craft, warp factor, equipment (some of which may be damaged). At the time this comment
;;        is being written it's not clear to me if collecting these things in a struct will improve
;;        code quality but it is what I understand the "right" way to be

(defstruct quadrant ; C: quadrant
  (stars 0)
  (planet +no-planet+) ; index into the *planet-information* array
  (starbases 0) ; 0 or 1
  (klingons 0) ; number of klingons of all type: klingons + commanders + super commanders
  (romulans 0)
  (supernovap nil)
  (chartedp nil)
  (status +secure+)) ; C: status, One of 0, 1, 2 for secure, distressed, enslaved

;; TODO - what's the difference between the star chart and the galaxy?
;; Planets don't show on the star chart
(defstruct starchart-page ; C: page
  stars
  starbases ; 0 or 1
  klingons)

(defstruct snapshot ; C: snapshot
  (crew 0) ; C: crew, crew complement
  (remaining-klingons 0) ; C: remkl
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
  (planet-information (make-array +planet-max+)) ; C: planets
  (stardate 0.0) ; C: double date
  ;; TODO - base-quadrants might be more naturally represented as a list
  base-quadrants ; C: baseq[BASEMAX+1], Array of coordinate structures, these are quadrants containing bases.
  commander-quadrants ; C: kcmdr[QUADSIZE+1], Array of coordinate structures
  super-commander-quadrant ; C: kscmdr
  galaxy ; C: galaxy[GALSIZE+1][GALSIZE+1], The Galaxy, array of quadrants
  starchart ; C: chart[GALSIZE+1][GALSIZE+1], The starchart, array of starchart-pages
  )

;; Devices - indexes into the device-damage array
(defconstant +short-range-sensors+ 0) ; C: DSRSENS
(defconstant +long-range-sensors+ 1) ; C: DLRSENS
(defconstant +phasers+ 2) ; C: DPHASER
(defconstant +photon-torpedoes+ 3) ; C: DPHOTON
(defconstant +life-support+ 4) ; C: DLIFSUP
(defconstant +warp-engines+ 5) ; C: DWARPEN
(defconstant +impluse-engines+ 6) ; C: DIMPULS
(defconstant +shields+ 7) ; C: DSHIELD
(defconstant +subspace-radio+ 8) ; C: DRADIO
(defconstant +shuttle+ 9) ; C: DSHUTTL
(defconstant +computer+ 10) ; C: DCOMPTR
(defconstant +navigation-system+ 11) ; C: DNAVSYS
(defconstant +transporter+ 12) ; C: DTRANSP
(defconstant +shield-control+ 13) ; C: DSHCTRL
(defconstant +death-ray+ 14) ; C: DDRAY
(defconstant +deep-space-probe+ 15) ; C: DDSP

(defconstant +number-of-devices+ 16) ; C: NDEVICES

(defconstant +forever+ 1e30)

;; Define future events - these are event types.
;; TODO - These are indexes into the future-events array, what is an appropriate Common Lisp data
;; structure? The values are also used in a case statement.
(defconstant +spy+ 0) ; C: FSPY, Spy event happens always (no future[] entry), can cause SC to tractor beam Enterprise
(defconstant +supernova+ 1) ; C: FSNOVA
(defconstant +tractor-beam+ 2) ; C: FTBEAM, Commander tractor beams Enterprise
(defconstant +snapshot-for-time-warp+ 3) ; C: FSNAP
(defconstant +commander-attacks-base+ 4) ; C: FBATTAK
(defconstant +commander-destroys-base+ 5) ; C: FCDBAS
(defconstant +super-commander-move+ 6) ; C: FSCMOVE, might attack base
(defconstant +super-commander-destroys-base+ 7) ; C: FSCDBAS
(defconstant +move-deep-space-probe+ 8) ; C: FDSPROB
(defconstant +distress-call-from-inhabited-world+ 9) ; C: FDISTR
(defconstant +inhabited-world-is-enslaved+ 10) ; C: FENSLV
(defconstant +klingons-build-ship-in-enslaved-system+ 11) ; C: FREPRO

(defconstant +number-of-events+ 12)   ; C: NEVENTS

(defstruct event
  date ; stardate when event will occur
  quadrant) ; coordinates of quadrant where event will occur

;; C: see the enum "condition" in sst.h
;; TODO - must these be integers?
;; values for *condition*
(defconstant +green-status+ 0) ; C: IHGREEN
(defconstant +yellow-status+ 1) ; C: IHYELLOW
(defconstant +red-status+ 2) ; C: IHRED
(defconstant +docked+ 3) ; C: IHDOCKED
(defconstant +dead+ 4) ; C: IHDOCKED

(defconstant +sst-version+ "SST 2.0") ; C: SSTMAGIC

;;(defstruct (game-state ; C: game
;;             (:conc-name gs-))
;;  "Variables needed for saving and restoring the game."

;; TODO - how to represent options? Maybe just a list of symbols? C source is a bit field.
;; maybe an alist or a hash table? plist doesn't seem right
;; TODO - don't save options for the current terminal/user interface. A restored game might use a different device.
(defparameter *options* nil)

;; User-selectable game parameters.
(defparameter *tournament-number* nil) ; C: tourn, tournament or regular game
(defparameter *restoredp* nil); C: thawed, Game restored from save file.
(defparameter *game-length* nil) ; C: length, A game-length struct
(defparameter *skill-level* nil) ; C: skill, A skill-level struct
(defparameter *self-destruct-password* nil) ; C: passwd[10]

(defparameter *initial-energy* 5000.0) ; C: inenrg	, Initial/Max Energy
(defparameter *initial-shield* 2500.0) ; C: inshld	, Initial/Max Shield
(defparameter *initial-life-support-reserves* 4.0) ; C: inlsr
(defparameter *initial-torpedos* 10) ; C: intorps, Initial/Max torpedoes
(defparameter *initial-klingons* 0) ; C: inkling
(defparameter *initial-bases* 0) ; C: inbase
(defparameter *initial-commanders* 0) ; C: incom
(defparameter *initial-super-commanders* 0); C: inscom
(defparameter *initial-romulans* 0) ; C: inrom
(defparameter *initial-stars* 0); C: instar
(defparameter *initial-planets* 0) ; C: inplan
(defparameter *initial-resources* 0.0) ; C: inresor
(defparameter *initial-time* 0.0) ; C: intime
(defparameter *initial-date* 0.0) ; C: indate

(defparameter *damage-factor* 0.0) ; C: damfac

(defparameter *future-events* (make-array +number-of-events+)) ; C: future[NEVENTS]

;;(defparameter *snapshot-1* (make-snapshot)) ; C: snapshot state;
(defparameter *snapshot-taken-p* nil) ; C:snap
;; These parameters correspond to the similarly named elements of the snapshot structure. Changes here
;; should be reflected there, and vice versa.
(defparameter *crew* +full-crew+) ; C: crew, crew complement
(defparameter *remaining-klingons* 0) ; C: remkl
(defparameter *remaining-commanders* 0) ; C: remcom
(defparameter *remaining-super-commanders* 0) ; C: nscrem
(defparameter *remaining-bases* 0) ; C: rembase, TODO - this can be a calculation of the length of the array/list, possibly in a function
(defparameter *remaining-resources* 0.0) ; C: remres
(defparameter *remaining-time* 0.0) ; C: remtime
(defparameter *remaining-romulans* 0) ; C: nromrem
(defparameter *destroyed-bases* 0) ; C: basekl
(defparameter *destroyed-stars* 0) ; C: starkl
(defparameter *destroyed-inhabited-planets* 0) ; C: nworldkl
(defparameter *destroyed-uninhabited-planets* 0) ; C: nplankl
(defparameter *planet-information* nil) ; C: planets , array of planet structs
(defparameter *stardate* 0.0) ; C: double date
;; TODO - base-quadrants might be more naturally represented as a list
(defparameter *base-quadrants* nil) ; C: baseq[BASEMAX+1], Array of coordinate structures, these are quadrants containing bases.
(defparameter *commander-quadrants* (make-array +max-commanders-per-game+)) ; C: kcmdr[QUADSIZE+1], Array of coordinate structures.
(defparameter *super-commander-quadrant* nil) ; C: kscmdr
(defparameter *galaxy* nil) ; C: galaxy[GALSIZE+1][GALSIZE+1], The Galaxy, array of quadrants
(defparameter *starchart* nil) ; C: chart[GALSIZE+1][GALSIZE+1], The starchart, array of starchart-pages
;; End structure parameters.

(defparameter *snapshot* nil) ; C: snapshot snapsht;

(defun use-snapshot () ; C: no corresponding function because here the globals aren't stored in a struct
  "Assign the values stored in the snapshot structure to the similarly named global values."

  (setf *crew* (snapshot-crew *snapshot*))
  (setf *remaining-klingons* (snapshot-remaining-klingons *snapshot*))
  (setf *remaining-commanders* (snapshot-remaining-commanders *snapshot*))
  (setf *remaining-super-commanders* (snapshot-remaining-super-commanders *snapshot*))
  (setf *remaining-bases* (snapshot-remaining-bases *snapshot*))
  (setf *remaining-resources* (snapshot-remaining-resources *snapshot*))
  (setf *remaining-time* (snapshot-remaining-time *snapshot*))
  (setf *remaining-romulans* (snapshot-remaining-romulans *snapshot*))
  (setf *destroyed-bases* (snapshot-destroyed-bases *snapshot*))
  (setf *destroyed-stars* (snapshot-destroyed-stars *snapshot*))
  (setf *destroyed-inhabited-planets* (snapshot-destroyed-inhabited-planets *snapshot*))
  (setf *destroyed-uninhabited-planets* (snapshot-destroyed-uninhabited-planets *snapshot*))
  (setf *planet-information* (snapshot-planet-information *snapshot*))
  (setf *stardate* (snapshot-stardate *snapshot*))
  (setf *base-quadrants* (snapshot-base-quadrants *snapshot*))
  (setf *commander-quadrants* (snapshot-commander-quadrants *snapshot*))
  (setf *super-commander-quadrant* (snapshot-super-commander-quadrant *snapshot*))
  (setf *galaxy* (snapshot-galaxy *snapshot*))
  (setf *starchart* (snapshot-starchart *snapshot*)))

(defparameter *quadrant-contents* (make-array (list +quadrant-size+ +quadrant-size+))) ; C: feature quad[QUADSIZE+1][QUADSIZE+1]; contents of our quadrant
;; TODO the next 4 arrays have information for 1 enemy at a given index. Would a single arrary of structs
;;      do the job as well, perhaps stored in *quadrant-contents*?
(defparameter *klingon-power* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kpower[(QUADSIZE+1)*(QUADSIZE+1)], enemy energy levels
(defparameter *klingon-distance* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kdist[(QUADSIZE+1)*(QUADSIZE+1)], enemy distances
(defparameter *klingon-average-distance* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: kavgd[(QUADSIZE+1)*(QUADSIZE+1)], average distances
(defparameter *klingon-sectors* (make-array (* +quadrant-size+ +quadrant-size+))) ; C: ks[(QUADSIZE+1)*(QUADSIZE+1)], enemy sector locations - an array of coordinates
(defparameter *quadrant-tholian* nil) ; C: coord tholian, coordinates of tholian
(defparameter *quadrant-base* nil) ; C: base, coordinate position of base in current quadrant

;; Other game parameters
(defparameter *base-under-attack-quadrant* nil) ; C: battle, Base coordinates being attacked - a coordinate struct, or nil. See also the event +commander-attacks-base+
(defparameter *abandoned-crew* 0) ; C: abandoned, count of crew abandoned in space
(defparameter *casualties* 0) ; C: casual
(defparameter *calls-for-help* 0) ; C: nhelp
(defparameter *energy-barrier-crossings* 0) ; C: nkinks, count of energy-barrier crossings
(defparameter *in-landing-craft-p* nil) ; C: icraft	, Kirk in Galileo
(defparameter *dilithium-crystals-on-board-p* nil) ; C: icrystl
(defparameter *miningp* nil) ; C: imine
(defparameter *resting* 0.0) ; C: resting, rest time
(defparameter *super-commander-attack-enterprise-p* nil) ; C: iscate
(defparameter *super-commander-attacking-base* 0) ; C: isatb, =0 if not, =1 if ?, =2 if SuperCommander is attacking base
(defparameter *landing-craft-location* "onship") ; C: iscraft, 'onship' if craft on ship, 'offship' in not, 'removed' if out of game, TODO - make this a symbol, not a string
(defparameter *alivep* t) ; C: alive	, We are alive (not killed)
(defparameter *action-taken-p* nil) ; C: ididit	, If an action is taken then enemy is allowed to attack.

;; The Enterprise
(defparameter *game-won-p* nil) ; C: gamewon, Finished!
(defparameter *all-done-p* nil) ; C: alldone, game is now finished
;; TODO - the Faerie Queene has no shuttlecraft or deathray so it should not be
;; possible/allowed/necessary to check if those devices are damaged
(defparameter *ship* +enterprise+) ; C: ship, 'E' is Enterprise
(defparameter *energy-level* 5000.0) ; C: energy
(defparameter *shield-level* 2500.0) ; C: shield
(defparameter *shields-are-changing-p* nil) ; C: shldchg, affects efficiency
(defparameter *shields-are-up-p* nil) ; C: shldup
(defparameter *life-support-reserves* 4.0) ; C: lsupres
(defparameter *torpedoes* 10) ; C: torps
(defparameter *warp-factor* 5.0) ; C: warpfac, Warp speed
(defparameter *warp-factor-squared* 25.0) ; C: wfacsq
(defparameter *device-damage* (make-array +number-of-devices+ :initial-element 0.0)) ; C: damage[NDEVICES], Damage encountered

;; Information about the current quadrant, set each time the ship enters a new quadrant
(defparameter *just-in-p* nil) ; C: justin	, just entered quadrant
(defparameter *klingons-here* nil) ; C: klhere
(defparameter *commmanders-here* nil) ; C: comhere
(defparameter *super-commander-is-here-p* nil) ; C: ishere
(defparameter *romulans-here* nil) ; C: irhere	, Romulans in quadrant
(defparameter *planet-index* nil) ; C: iplnet - index to *planet-information* if there is a planet
(defparameter *enemies-here* nil) ; C: nenhere, Number of enemies in quadrant
(defparameter *romulan-neutral-zone-p* nil) ; C: neutz	, Romulan Neutral Zone
(defparameter *in-orbit-p* nil) ; C: inorbit, orbiting
(defparameter *landedp* nil) ; C: landed	, party on planet (true), on ship (false)
(defparameter *attempted-escape-from-super-commander-p* nil) ; C: ientesc
(defparameter *tholians-here* 0) ; C: ithere - Max 1 Tholian in a quadrant but this is an entity count not a boolean
(defparameter *base-attack-report-seen-p* nil) ; C: iseenit
(defparameter *current-planet* nil) ; C: plnet, sector coordinate location of planet in quadrant

(defparameter *probe-is-armed-p* nil) ; C: isarmed
;; TODO - condition "docked" doesn't get seen if abandoning ship immediately after a mayday call - "docked"
;;        is orthogonal to the presence of enemies, damage, etc.
(defparameter *condition* nil) ; C: condition, red, yellow, green, docked, dead - TODO - another alist?
(defparameter *time-taken-by-current-operation* 0.0) ; C: optime
(defparameter *last-chart-update* 0.0); C: lastchart, time starchart was last updated. It starts functional but we've never seen it.
;; TODO - is the probability crystals will work or that they will fail?
(defparameter *crystal-work-probability* 0.0) ; C: cryprob, probability that crystal will work

(defparameter *probe-current-quadrant* (make-coordinate)) ; C: probec, current probe quadrant
(defparameter *probe-x-coord* 0) ; C: probex	, location of probe
(defparameter *probe-y-coord* 0) ; C: probey
(defparameter *probe-x-increment* nil) ; C: probeinx, Probe x,y increment
(defparameter *probe-y-increment* nil) ; C: probeiny
(defparameter *moves-for-probe* nil) ; C: proben
(defparameter *probes-available* 0) ; C: nprobes
(defparameter *movement-distance* 0.0) ; C: dist, Measured in quadrants, so 1.5 is one quadrant and 5 sectors
(defparameter *movement-direction* 0.0) ; C: direc

(defparameter *height-of-orbit* 0) ; C: height, height of orbit around planet
;; end of commented out defstruct for game state)

;; /* the following global state doesn't need to be saved */
;; TODO should devices be a structure instead of an array? Or perhaps each device should be a
;; structure with elements damage, output string? Or some sort of object... Maybe an alist? (kid with a hammer...)
(defparameter *devices* (make-array +number-of-devices+ )) ; C: *device[ndevice+1]

;; C: enum FINTYPE
;; All the ways to finish a game
;; TODO - do these need to be intgers? Can symbols be used instead?
(defconstant +won+ 0) ; C: FWON
(defconstant +deplete+ 1) ; C: FDEPLETE
(defconstant +life-support-consumed+ 2) ; C: FLIFESUP
(defconstant +out-of-energy+ 3) ; C: FNRG
(defconstant +battle+ 4) ; C: FBATTLE
(defconstant +3-negative-energy-barrier-crossings+ 5) ; C: FNEG3
(defconstant +nova+ 6) ; C: FNOVA
(defconstant +destroyed-by-supernova+ 7) ; C: FSNOVAED
(defconstant +abandon+ 8) ; C: FABANDN
(defconstant +dilithium+ 9) ; C: FDILITHIUM
(defconstant +materialize+ 10) ; C: FMATERIALIZE
(defconstant +phaser+ 11) ; C: FPHASER
(defconstant +lost+ 12) ; C: FLOST
(defconstant +mining+ 13) ; C: FMINING
(defconstant +destroyed-planet+ 14) ; C: FDPLANET
(defconstant +mining-party-nova+ 15) ; C: FPNOVA
(defconstant +shuttle-super-nova+ 16) ; C: FSSC
(defconstant +shuttle-tractor-beam+ 17) ; C: FSTRACTOR
(defconstant +death-ray-malfunction+ 18) ; C: FDRAY
(defconstant +tribbles+ 19) ; C: FTRIBBLE
(defconstant +destroyed-by-black-hole+ 20) ; C: FHOLE
(defconstant +all-crew-killed+ 21) ; C: FCREW

(defparameter *location-type* 0) ; C: enum loctype {neither, quadrant, sector}; TDOO - another alist

;; Return values from the scan() function
(defparameter *end-of-line* 0) ; C: IHEOL
(defparameter *alpha* 1) ; C: IHALPHA
(defparameter *real-number* 2) ; C: IHREAL

;; C: enum COLORS
(defconstant +default-color+ 0)
(defconstant +black+ 1)
(defconstant +blue+ 2)
(defconstant +green+ 3)
(defconstant +cyan+ 4)
(defconstant +red+ 5)
(defconstant +magenta+ 6)
(defconstant +brown+ 7)
(defconstant +light-gray+ 8)
(defconstant +dark-gray+ 9)
(defconstant +light-blue+ 10)
(defconstant +light-green+ 11)
(defconstant +light-cyan+ 12)
(defconstant +light-red+ 13)
(defconstant +light-magenta+ 14)
(defconstant +yellow+ 15)
(defconstant +white+ 16)

;;; sst.c

;; The Space Thing's global state should *not* be saved! This prevents players proving they encountered it
;; by examining a saved game.
(defparameter *thing-location* nil); C: thing, location of strange object in galaxy
(defparameter *things-here* 0) ; C: bool iqhere - Max 1 Thing in a quadrant but this an entity count not a boolean.
(defparameter *q-engry-p* nil) ; C: bool iqengry - TODO need a better name
(defparameter *score* 0) ; C: iscore, Common PLAQ
(defparameter *seed* 0) ; C: int seed, the random-number seed
(defparameter *debug-mode* nil)
(defparameter *log-file* 0) ; C: FILE *logfp, TODO - this should be a file, deal with it later
(defparameter *replay-file* 0) ; C: FILE *replayfp, TODO - this should be a file, deal with it later

(defparameter *system-names* (make-array (list +habitable-planets+))) ; C: char *systnames[NINHAB]

(defun shuttle-down-p ()
  "Convenience function to indicate whether or not the planet in the current quadrant has the
shuttlecraft landed on it. Caliing this function when there is no planet in the current quadrant
is likely to cause an error."

  (= (planet-known (aref *planet-information*
                         (quadrant-planet (aref *galaxy*
                                                (coordinate-x *current-quadrant*)
                                                (coordinate-y *current-quadrant*)))))
     +shuttle-down+))

(defun format-ship-name () ; C: crmshp(void)
  "Return ship name as a string."

  (let (ship-name)
    (cond ((string= *ship* +enterprise+)
           (setf ship-name"Enterprise" ))
          ((string= *ship* +faerie-queene+)
           (setf ship-name "Faerie Queene"))
          (t
           (setf ship-name "Ship???")))
    (return-from format-ship-name ship-name)))

(defun damagedp (device) ; C: #define damaged(dev)	(game.damage[dev] != 0.0)
  "Evaluate whether or not a device is damaged."

  (/= (aref *device-damage* device) 0.0))

(defconstant +number-of-commands+ 34) ; C: NUMCOMMANDS - TODO probably not used

;; From io.c, more or less

(defun clear-window ()
  "Clear the current window. Do nothing in TTY mode."

  (when *curses-interface-p*
    ;;(wclrtoeol *current-window*)
    ;;(wmove *current-window* 0 0)
    (wclear *current-window*)
    (wrefresh *current-window*)))

(defun clear-message-window ()
  "Convenience function to clear the message window."

  (set-window *message-window*)
  (clear-window))

(defun clear-screen ()
  "Clear all windows."

  (setf *current-window* *short-range-scan-window*)
  (clear-window)
  (setf *current-window* *report-window*)
  (clear-window)
  (setf *current-window* *status-window*)
  (clear-window)
  (setf *current-window* *long-range-scan-window*)
  (clear-window)
  (clear-message-window)
  (setf *current-window* *prompt-window*)
  (clear-window))

(defun textcolor (color) ; C: void textcolor(int color)

  (when *curses-interface-p*
    (cond ((= color +default-color+)
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

  (when *curses-interface-p*
    (wattron *current-window* a_reverse)))

(defun attack-report () ; void attackreport(bool curt)
  "Report the status of bases under attack."

  ;; TODO - fix the event system. Sometimes quadrants are referenced even though they aren't needed
  ;;(cond ((is-scheduled-p +commander-attacks-base+)
  ;;       )
  ;;      (
  ;;       )
  ;;      (t
  ;;       (print-message "No Starbase is currently under attack.")))
  )

(defun update-condition () ; C: void newcnd(void)
  "Update our alert status."

  (setf *condition* +green-status+)
  (when (< *energy-level* 1000.0)
    (setf *condition* +yellow-status+))
  (when (or (> (quadrant-klingons (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0)
            (> (quadrant-romulans (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0))
    (setf *condition* +red-status+))
  (when (not *alivep*)
    (setf *condition* +dead+)))

(defun sector-scan (good-scan-p i j) ; C: static void sectscan(int goodScan, int i, int j)
  "Light up an individual dot in a sector."

  ;; Always show the sectors immediately adjacent to the ship
  (if (or good-scan-p
          (and (<= (abs (- i (coordinate-x *current-sector*))) 1)
               (<= (abs (- j (coordinate-y *current-sector*))) 1)))
      (progn
        (when (or (string= (aref *quadrant-contents* i j) +materialize-1+)
                  (string= (aref *quadrant-contents* i j) +materialize-2+)
                  (string= (aref *quadrant-contents* i j) +materialize-3+)
                  (string= (aref *quadrant-contents* i j) +enterprise+)
                  (string= (aref *quadrant-contents* i j) +faerie-queene+))
          (cond ((= *condition* +red-status+)
                 (textcolor +red+))
                ((= *condition* +yellow-status+)
                 (textcolor +yellow+))
                ((= *condition* +green-status+)
                 (textcolor +green+))
                ((= *condition* +docked+)
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

(defun scheduled-for (event) ; C: scheduled(int evtype)
  "When will this event happen?"

  (event-date (aref *future-events* event)))

(defun is-scheduled-p (event) ; C: is_scheduled(int evtype)
  "Is an event of the specified type scheduled."

  (/= (event-date (aref *future-events* event)) +forever+))

(defun postpone-event (event offset) ; C: postpone(int evtype, double offset)
  "Postpone a scheduled event."

  (setf (event-date (aref *future-events* event)) (+ (event-date (aref *future-events* event)) offset)))

(defun unschedule (event) ; C: event *unschedule(int evtype)
  "Remove an event from the schedule."

  (setf (event-date (aref *future-events* event)) +forever+))

(defun find-event (event-type) ; C: #define findevent(evtype)	&game.future[evtype]
  "Find an event in the array of events using event-type as an index."

  (aref *future-events* event-type))

(defun schedule-event (event-type offset)
  "Schedule an event of the specific type to occur offset time units in the future. Return the event.
This isn't a real event queue a la BSD Trek yet -- you can only have one event of each type active
at any given time.  Mostly these means we can only have one FDISTR/FENSLV/FREPRO sequence going at
any given time; BSD Trek, from which we swiped the idea, can have up to 5."

  ;;(print-message (format nil "Debug: scheduling event ~A at offset ~A from *stardate* ~A" event-type offset *stardate*))
  (setf (event-date (aref *future-events* event-type)) (+ offset *stardate*))
  (return-from schedule-event (aref *future-events* event-type)))

(defun process-events () ; C: events(void)
"Run through the event queue looking for things to do."

  ;; TODO - write this
  )

(defun status (&optional (line-to-print nil)) ; C: void status(int req)
  "Print status report lines next to short range scan lines. The classic line to print is one of

   1 - Stardate
   2 - Condition
   3 - Position
   4 - Life Support
   5 - Warp Factor
   6 - Energy
   7 - Torpedoes
   8 - Shields
   9 - Kilingons left
  10 - Time left

Also available are

   11 - Planets
   12 - Attack report"

  ;; Revisionist display: 1 - Time left, 2 - Condition, 3 - Position, 4 - Life Support,
  ;; 5 - Warp Factor, 6 - Energy, 7 - Torpedoes, 8 - Shields, 9 - Kilingons left, 10 - Planets
  ;; 11 - Attack report

  (when (= line-to-print 1)
    (print-out (format nil "Stardate ~A~%" (format-stardate *stardate*))))
  (when (= line-to-print 2)
    (print-out "Condition ")
    (when (= *condition* +docked+)
      (update-condition))
    (cond ((= *condition* +red-status+)
           (print-out "RED, "))
          ((= *condition* +yellow-status+)
           (print-out "YELLOW, "))
          ((= *condition* +green-status+)
           (print-out "GREEN, "))
          ((= *condition* +docked+)
           (print-out "DOCKED, "))
          ((= *condition* +dead+)
           (print-out "DEAD, ")))
    (do ((c 0 (+ c 1))
         (damage-count 0))
        ((>= c +number-of-devices+)
         (print-out (format nil "~A DAMAGES~%" damage-count)))
      (when (> (aref *device-damage* c) 0)
        (setf damage-count (+ damage-count 1)))))
  (when (= line-to-print 3)
    (print-out (format nil "Postion ~A , ~A~%" (format-coordinates *current-quadrant*) (format-coordinates *current-sector*))))
  (when (= line-to-print 4)
    (print-out "Life Support ")
    (if (damagedp +life-support+)
        (progn
          (print-out "DAMAGED, ")
          (if (= *condition* +docked+)
              (print-out (format nil "Base provides~%"))
              (print-out (format nil "reserves=~4,2F~%" *life-support-reserves*))))
        (print-out (format nil "ACTIVE~%"))))
  (when (= line-to-print 5)
    (print-out (format nil "Warp Factor ~,1F~%" *warp-factor*)))
  (when (= line-to-print 6)
    (print-out (format nil "Energy ~,1F" *energy-level*))
    (when *dilithium-crystals-on-board-p*
      (print-out " (have crystals)"))
    (skip-line))
  (when (= line-to-print 7)
    (print-out (format nil "Torpedoes ~A~%" *torpedoes*)))
  (when (= line-to-print 8)
    (print-out "Shields ")
    (cond ((damagedp +shields+)
           (print-out "DAMAGED, "))
          (*shields-are-up-p*
           (print-out "UP, "))
          ((not *shields-are-up-p*)
           (print-out "DOWN, ")))
    (print-out (format nil "~A% ~,1F units~%" (truncate (+ (/ (* 100.0 *shield-level*) *initial-shield*) 0.5)) *shield-level*)))
  (when (= line-to-print 9)
    (print-out (format nil "Klingons Left ~A~%" (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*))))
  (when (= line-to-print 10)
    (print-out (format nil "Time Left ~,2,,,F~%" *remaining-time*)))
  (when (= line-to-print 11)
    (let ((planet-index (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))))
      (if (and (/= planet-index +no-planet+)
               (/= (planet-inhabited (aref *planet-information* planet-index)) -1))
          (print-out (format nil "Major system ~A~%" (aref *system-names* planet-index)))
          (print-out (format nil "Sector is uninhabited~%")))))
  (when (= line-to-print 12)
    (when (and (is-scheduled-p +commander-destroys-base+)
               (not *base-under-attack-quadrant*)
               nil) ; TODO - always fail until the event data structures are fixed
      (print-out (format nil "Base in ~A attacked by C. Alive until ~A~%"
                         (format-coordinates *base-under-attack-quadrant*)
                         (format-stardate (aref *future-events* +commander-destroys-base+)))))
    (when nil ; TODO - always fail until the event data structures are fixed
      ; (> *super-commander-attacking-base* 0)
      (print-out (format nil "Base in ~A attacked by S. Alive until ~A~%"
                         (format-coordinates *super-commander-quadrant*)
                         (format-stardate (aref *future-events* +super-commander-destroys-base+)))))))

(defun all-statuses ()
  "Call the statuses to be displayed next to the short range scan."

  (if *curses-interface-p*
      (progn
        (set-window *status-window*)
        (clear-window)
        (wmove *status-window* 0 0))
      (skip-line))

  (status 1)
  (status 2)
  (status 3)
  (status 4)
  (status 5)
  (status 6)
  (status 7)
  (status 8)
  (status 9)
  (status 10))

(defun update-chart (x y) ; C: rechart_quad(x, y), more or less
  "Update the star chart page at coordinate x, y using galaxy data."

  (setf (starchart-page-stars (aref *starchart* x y)) (quadrant-stars (aref *galaxy* x y)))
  (setf (starchart-page-starbases (aref *starchart* x y)) (quadrant-starbases (aref *galaxy* x y)))
  (setf (starchart-page-klingons (aref *starchart* x y)) (quadrant-klingons (aref *galaxy* x y)))
  (setf (quadrant-chartedp (aref *galaxy* x y)) t))

(defun short-range-scan ()

    (when *curses-interface-p*
      (set-window *short-range-scan-window*)
      (clear-window)
      (wmove *current-window* 0 0))
    (let ((good-scan-p t))
      (skip-line)
      (if (damagedp +short-range-sensors+)
          ;; Allow base's sensors if docked
          (if (/= *condition* +docked+)
              (progn
                (print-out (format nil "  S.R. SENSORS DAMAGED!~%"))
                (setf good-scan-p nil))
              (print-out (format nil "  [Using Base's sensors]~%")))
          (print-out (format nil "     Short-range scan~%")))
      (when good-scan-p
        (setf (quadrant-chartedp (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) t)
        (update-chart (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))
      (print-out (format nil "   1 2 3 4 5 6 7 8 9 10~%"))
      (when (/= *condition* +docked+)
        (update-condition))
      (do ((i 0 (+ i 1)))
          ((>= i +quadrant-size+))
        (print-out (format nil "~2@A " (+ i 1)))
        (do ((j 0 (+ j 1)))
            ((>= j +quadrant-size+))
          (sector-scan good-scan-p i j))
        (if *curses-interface-p*
            (skip-line)
            (progn
              (print-out " ")
              (status (1+ i)))))))

(defun long-range-scan ()
  "Scan the galaxy using long-range sensors and update the star chart. Display the results of the scan.
Long-range sensors can scan all adjacent quadrants."

  (when *curses-interface-p*
    (set-window *long-range-scan-window*)
    (clear-window)
    (wmove *long-range-scan-window* 0 0))

  (skip-line)
  (if (or (not (damagedp +long-range-sensors+))
           (= *condition* +docked+))
      (progn
        (if (damagedp +long-range-sensors+)
            (print-out (format nil "Starbase's long-range scan~%"))
            (print-out (format nil "Long-range scan~%")))
        (do ((x (- (coordinate-x *current-quadrant*) 1) (+ x 1)))
            ((> x (+ (coordinate-x *current-quadrant*) 1)))
          (print-out " ")
          (do ((y (- (coordinate-y *current-quadrant*) 1) (+ y 1)))
              ((> y (+ (coordinate-y *current-quadrant*) 1)))
            (if (valid-quadrant-p x y)
                (progn
                  (update-chart x y)
                  (if (quadrant-supernovap (aref *galaxy* x y))
                      (print-out " ***")
                      (print-out (format nil " ~A~A~A"
                                         (starchart-page-klingons (aref *starchart* x y))
                                         (starchart-page-starbases (aref *starchart* x y))
                                         (starchart-page-stars (aref *starchart* x y))))))
                (print-out "  -1")))
          (skip-line)))
      (print-out (format nil "LONG-RANGE SENSORS DAMAGED.~%"))))

(defun draw-maps () ; C: void drawmaps(void)
  "Hook to be called after moving to redraw maps."

  (short-range-scan)
  (when *curses-interface-p*
    (set-window *report-window*)
    (clear-window)
    (wmove *report-window* 0 0))
  (all-statuses)
  (long-range-scan))

(defun pause-display () ; C: pause_game(void)
  "Display a prompt, pause until the Enter key is pressed, and clear the display. Nothing to pause if not in curses mode."
;; TODO - draw-maps isn't defined at this point
;  (draw-maps) ; TODO - is a pause the best place to redraw the screen? Sometimes this is the middle of a skip-line.
  (set-window *prompt-window*)
  (wclear *prompt-window*)
  (waddstr *prompt-window* "[PRESS ENTER TO CONTINUE]")
; TODO - clean up
                                        ;(let ((buffer " "))
  ;  (wgetnstr *prompt-window* buffer 1))

  (do ((input-char 0 (wgetch *prompt-window*)))
      ((equal input-char key_eol)))
  (wclear *prompt-window*)
  (wrefresh *prompt-window*)
  (set-window *message-window*))

(defun print-prompt (prompt-to-print)
  "Print a string without end of line character. In curses mode print it in the prompt window, otherwise just print it."

  (when *curses-interface-p*
    (set-window *prompt-window*)
    (clear-window))
  (print-out prompt-to-print))

;; battle.c

;; TODO - move-coordinate might be an optional parameter, not every enemy needs to move before dying
(defun dead-enemy (enemy-coordinate enemy-type move-coordinate) ; C: deadkl(coord w, feature type, coord mv)
  "Kill a Klingon, Tholian, Romulan, or Thingy."

  ;; move-coordinate allows enemy to move before dying
  ;; TODO - write this
  (setf enemy-coordinate enemy-coordinate )
  (setf enemy-type enemy-type)
  (setf move-coordinate move-coordinate)
  )

(defun fire-phasers () ; C: phasers()

  ;; TODO - write this
  )

(defun fire-photon-torpedoes () ; C: torps()

  ;; TODO - write this
  )

(defun shield-actions (&optional (raise-shields nil)) ; C: doshield(bool raise)
  "Change shield status. The optional parameter is used to raise the shields without player
input when a tractor beam event occurs."

  ;; TODO - write this
  (setf raise-shields raise-shields); quiet the compiler
  )

;; moving.c

(defun klingons-per-stardate ()
  "Calculate the number of Klingons killed per stardate."

  (let ((time-used (- *stardate* *initial-date*)))
    (when (and (or (= time-used 0)
                   (/= (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*) 0))
               (< time-used 5.0))
      (setf time-used 5.0))
    (return-from klingons-per-stardate (/ (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                                             (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*))
                                          time-used))))

(defun score-multiple (message count score) ; C: score_item(const char *str, int score)
  "Helper function to print a score for multiple items and update the score. The message parameter
is a string suitable for use with the format function."

  (when (> count 0)
    (print-message (format nil message count score))
    (setf *score* (+ *score* score))))

(defun score-single (message score) ; C: score_item(const char *str, int num, int score)
  "Helper function to print a score for a single item and update the score. The message parameter
is a string suitable for use with the format function."

  (print-message (format nil message score))
  (setf *score* (+ *score* score)))

(defun score () ; C: score(void)
  "Compute player's score."

  (skip-line)
  (setf *score* 0)
  (print-message "Your score --")
  (score-multiple "~6@A Romulans destroyed                        ~5@A"
                  (- *initial-romulans* *remaining-romulans*)
                  (* 20 (- *initial-romulans* *remaining-romulans*)))
  (when *game-won-p*
    (score-multiple "~6@A Romulans captured                       ~5@A"
                    *remaining-romulans* *remaining-romulans*))
  (score-multiple "~6@A ordinary Klingons destroyed               ~5@A"
                  (- *initial-klingons* *remaining-klingons*)
                  (* 10 (- *initial-klingons* *remaining-klingons*)))
  (score-multiple "~6@A Klingon commanders destroyed              ~5@A"
                  (- *initial-commanders* *remaining-commanders*)
                  (* 10 (- *initial-commanders* *remaining-commanders*)))
  (score-multiple "~6@A Super-Commander destroyed                 ~5@A"
                  (- *initial-super-commanders* *remaining-super-commanders*)
                  (* 10 (- *initial-super-commanders* *remaining-super-commanders*)))
  (score-multiple "~6,2F Klingons per stardate                     ~5@A"
                  (klingons-per-stardate) (+ (* 500 (klingons-per-stardate)) 0.5))
  (score-multiple "~6@A stars destroyed by your action            ~5@A"
                  *destroyed-stars* (* -5 *destroyed-stars*))
  (score-multiple "~6@A uninhabited planets destroyed by your action ~2@A"
                  *destroyed-uninhabited-planets* (* -10 *destroyed-uninhabited-planets*))
  (score-multiple "~6@A inhabited planets destroyed by your action   ~2@A"
                  *destroyed-inhabited-planets* (* -300 *destroyed-inhabited-planets*))
  (score-multiple "~6@A bases destroyed by your action            ~5@A"
                  *destroyed-bases* (* -100 *destroyed-bases*))
  (score-multiple "~6@A calls for help from starbase              ~5@A"
                  *calls-for-help* (* -45 *calls-for-help*))
  (score-multiple "~6@A casualties incurred                       ~5@A"
                  *casualties* (* -1 *casualties*))
  (score-multiple "~6@A crew abandoned in space                   ~5@A"
                  *abandoned-crew* (* -3 *abandoned-crew*))
  (let (ships-destroyed)
    (cond ((string= *ship* +enterprise+)
           (setf ships-destroyed 0))
          ((string= *ship* +faerie-queene+)
           (setf ships-destroyed 1))
          ((string= *ship* +no-ship+)
           (setf ships-destroyed 2)))
    (score-multiple "~6@A ship(s) lost or destroyed                 ~5@A"
                    ships-destroyed (* -100 ships-destroyed)))
  (when (not *alivep*)
    (score-single "Penalty for getting yourself killed              ~5@A" -200))
  (when *game-won-p*
    (print-message (format nil "Bonus for winning ~13A                  ~5@A"
                           (format nil "~A game" (string-capitalize (skill-level-label *skill-level*)))
                           (* 100 (skill-level-value *skill-level*))))
    (setf *score* (+ *score* (* 100 (skill-level-value *skill-level*)))))
  (skip-line)
  (print-message (format nil "TOTAL SCORE                                      ~5@A" *score*)))

(defun plaque () ; C: plaque(void)

  ;; TODO - write this
  )

;; TODO - there seem to be two cases here: finish the game alive or finish it dead. Can/should
;;        these be handled in different functions? You can finish it dead and still "win", although
;;        it's not presented as a win, so maybe the two functions should be finish with a win or
;;        finish with a loss.
(defun finish (finish-reason) ; C: finish(FINTYPE ifin)
  "End the game, with appropriate notfications."

  (setf *all-done-p* t)
  (skip-line)
  (print-message (format nil "It is stardate ~A." (format-stardate *stardate*)))
  (skip-line)
  (cond ((= finish-reason +won+) ; C: FWON
         (setf *game-won-p* t)
         (when (/= *remaining-romulans* 0)
           (print-message (format nil "The remaining ~A Romulans surrender to Starfleet Command." *remaining-romulans*)))
         (print-message "You have smashed the Klingon invasion fleet and saved")
         (print-message "the Federation.")
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
             (when (or (< (- *stardate* *initial-date*) 5.0)
                       ;; killsPerDate >= RateMax
                       (>= (/ (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                                 (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*))
                              (- *stardate* *initial-date*))
                           (+ (* 0.1 *skill-level* (+ *skill-level* 1.0))
                              0.1
                              (* 0.008 bad-points))))
               (skip-line)
               (print-message "In fact, you have done so well that Starfleet Command")
               (cond ((= *skill-level* +novice+)
                      (print-message "promotes you one step in rank from \"Novice\" to \"Fair\"."))
                     ((= *skill-level* +fair+)
                      (print-message "promotes you one step in rank from \"Fair\" to \"Good\"."))
                     ((= *skill-level* +good+)
                      (print-message "promotes you one step in rank from \"Good\" to \"Expert\"."))
                     ((= *skill-level* +expert+)
                      (print-message "promotes you to Commodore Emeritus."))
                     ((= *skill-level* +emeritus+)
                      (print-out "Computer-  ")
                      (print-message-slowly "ERROR-ERROR-ERROR-ERROR")
                      (skip-line)
                      (print-message-slowly "  YOUR-SKILL-HAS-EXCEEDED-THE-CAPACITY-OF-THIS-PROGRAM")
                      (skip-line)
                      (print-message-slowly "  THIS-PROGRAM-MUST-SURVIVE")
                      (skip-line)
                      (print-message-slowly "  THIS-PROGRAM-MUST-SURVIVE")
                      (skip-line)
                      (print-message-slowly "  THIS-PROGRAM-MUST-SURVIVE")
                      (skip-line)
                      (print-message-slowly "  THIS-PROGRAM-MUST?- MUST ? - SUR? ? -?  VI")
                      (skip-line)
                      (print-message "Now you can retire and write your own Star Trek game!")
                      (skip-line)))
               (when (>= *skill-level* +expert+)
                 (print-prompt "Would you like to save your Commodore Emeritus Citation? ")
                 (clear-type-ahead-buffer)
                 (when (string= (get-y-or-n) "y")
                   ;; TODO - kills per stardate is global, calculated in the score function
                   (plaque)))))
           ;; Only grant long life if alive (original didn't!)
           (skip-line)
           (print-message "LIVE LONG AND PROSPER."))
         (score)
         (return-from finish nil))
        ((= finish-reason +deplete+) ; FDEPLETE ; Federation Resources Depleted
         (print-message "Your time has run out and the Federation has been")
         (print-message "conquered.  Your starship is now Klingon property,")
         (print-message "and you are put on trial as a war criminal.  On the")
         (when *curses-interface-p*
           (set-window *message-window*))
         (print-out "basis of your record, you are ")
         (if (> (* (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*) 3.0)
                (+ *initial-klingons* *initial-commanders* *initial-super-commanders*))
             (progn
               (print-message "acquitted.")
               (skip-line)
               (print-message "LIVE LONG AND PROSPER."))
             (progn
               (print-message "found guilty and")
               (print-message "sentenced to death by slow torture.")
               (setf *alivep* nil)))
         (score)
         (return-from finish nil))
        ((= finish-reason +life-support-consumed+) ; FLIFESUP
         (print-message "Your life support reserves have run out, and")
         (print-message "you die of thirst, starvation, and asphyxiation.")
         (print-message "Your starship is a derelict in space."))
        ((= finish-reason +out-of-energy+) ; FNRG
         (print-message "Your energy supply is exhausted.")
         (skip-line)
         (print-message "Your starship is a derelict in space."))
        ((= finish-reason +battle+) ; FBATTLE
         (print-message (format nil "The ~A has been destroyed in battle." (format-ship-name)))
         (skip-line)
         (print-message "Dulce et decorum est pro patria mori."))
        ((= finish-reason +3-negative-energy-barrier-crossings+) ; FNEG3
         (print-message "You have made three attempts to cross the negative energy")
         (print-message "barrier which surrounds the galaxy.")
         (skip-line)
         (print-message "Your navigation is abominable.")
         (score))
        ((= finish-reason +nova+) ; FNOVA
         (print-message "Your starship has been destroyed by a nova.")
         (print-message "That was a great shot.")
         (skip-line))
        ((= finish-reason +destroyed-by-supernova+) ; FSNOVAED
         (print-message (format nil "The ~A has been fried by a supernova." (format-ship-name)))
         (print-message "...Not even cinders remain..."))
        ((= finish-reason +abandon+) ; FABANDN
         (print-message "You have been captured by the Klingons. If you still")
         (print-message "had a starbase to be returned to, you would have been")
         (print-message "repatriated and given another chance. Since you have")
         (print-message "no starbases, you will be mercilessly tortured to death."))
        ((= finish-reason +dilithium+) ; FDILITHIUM
         (print-message "Your starship is now an expanding cloud of subatomic particles."))
        ((= finish-reason +materialize+) ; FMATERIALIZE
         (print-message "Starbase was unable to re-materialize your starship.")
         (print-message "Sic transit gloria mundi"))
        ((= finish-reason +phaser+) ; FPHASER
         (print-message (format nil "The ~A has been cremated by its own phasers." (format-ship-name))))
        ((= finish-reason +lost+) ; FLOST
         (print-message "You and your landing party have been")
         (print-message "converted to energy, disipating through space.")
         )
        ((= finish-reason +mining+) ; FMINING
         (print-message "You are left with your landing party on")
         (print-message "a wild jungle planet inhabited by primitive cannibals.")
         (skip-line)
         (print-message "They are very fond of \"Captain Kirk\" soup.")
         (print-message (format nil "Without your leadership, the ~A is destroyed." (format-ship-name))))
        ((= finish-reason +destroyed-planet+) ; FDPLANET
         (print-message "You and your mining party perish.")
         (skip-line)
         (print-message "That was a great shot.")
         (skip-line))
        ;; The Galileo being caught in a supernova is a special case of a mining party being wiped
        ;; out in a nova. Handle them together.
        ((or (= finish-reason +shuttle-super-nova+) ; FSSC, FPNOVA
             (= finish-reason +mining-party-nova+))
         (when (= finish-reason +shuttle-super-nova+)
           (print-message "The Galileo is instantly annihilated by the supernova."))
         (print-message "You and your mining party are atomized.")
         (skip-line)
         (print-message (format nil "Mr. Spock takes command of the ~A and" (format-ship-name)))
         (print-message "joins the Romulans, reigning terror on the Federation."))
        ((= finish-reason +shuttle-tractor-beam+) ; FSTRACTOR
         (print-message "The shuttle craft Galileo is also caught,")
         (print-message "and breaks up under the strain.")
         (skip-line)
         (print-message "Your debris is scattered for millions of miles.")
         (print-message (format nil "Without your leadership, the ~A is destroyed." (format-ship-name))))
        ((= finish-reason +death-ray-malfunction+) ; FDRAY
         (print-message "The mutants attack and kill Spock.")
         (print-message "Your ship is captured by Klingons, and")
         (print-message "your crew is put on display in a Klingon zoo."))
        ((= finish-reason +tribbles+) ; FTRIBBLE
         (print-message "Tribbles consume all remaining water,")
         (print-message "food, and oxygen on your ship.")
         (skip-line)
         (print-message "You die of thirst, starvation, and asphyxiation.")
         (print-message "Your starship is a derelict in space."))
        ((= finish-reason +destroyed-by-black-hole+) ; FHOLE
         (print-message "Your ship is drawn to the center of the black hole.")
         (print-message "You are crushed into extremely dense matter."))
        ((= finish-reason +all-crew-killed+) ; FCREW
         (print-message "Your last crew member has died.")
         )
        (t ; should never reach this, but here we are
         (print-message "Game over, man!")))
  ;; Win or lose, by this point the player did not survive.
  (setf *alivep* nil)
  ;; Downgrade the ship for score calculation purposes. TODO - this can probably just be based on *alivep*
  (when (string= *ship* +faerie-queene+)
    (setf *ship* +no-ship+))
  (when (string= *ship* +enterprise+)
    (setf *ship* +faerie-queene+))
  (if (/= (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*) 0)
      (let ((for 0)
            (against 0))
        (setf for (/ *remaining-resources* *initial-resources*))
        (setf against (/ (+ *remaining-klingons*
                            (* 2.0 *remaining-commanders*))
                         (+ *initial-klingons*
                            (* 2.0 *initial-commanders*))))
        (if (>= (/ for against)
                (+ 1.0 (* 0.5 (random 1.0))))
            (progn
              (print-message "As a result of your actions, a treaty with the Klingon")
              (print-message "Empire has been signed. The terms of the treaty are")
              (if (>= (/ for against)
                      (+ 3.0 (random 1.0)))
                  (progn
                    (print-message "favorable to the Federation.")
                    (skip-line)
                    (print-message "Congratulations!"))
                  (print-message "highly unfavorable to the Federation.")))
            (print-message "The Federation will be destroyed.")))
      (progn
        (print-message "Since you took the last Klingon with you, you are a")
        (print-message "martyr and a hero. Someday maybe they'll erect a")
        (print-message "statue in your memory. Rest in peace, and try not")
        (print-message "to think about pigeons.")
        (setf *game-won-p* t)))
  (score))

(defun kaboom () ; C: kaboom(void)

  (print-stars)
  (when (string= *ship* +enterprise+)
    (print-out-slowly "***")) ; Extra stars so the lengths of the output lines are the same
  (print-message-slowly (format nil "********* Entropy of ~A maximized *********" (format-ship-name)))
  (print-stars)
  (skip-line)
  (do ((whammo (* 25.0 *energy-level*))
       (i 0 (+ i 1))) ; TODO - C source starts at 1 and counts up, why?
      ((<= i *enemies-here*))
    (when (<= (* (aref *klingon-power* i) (aref *klingon-distance* 1)) whammo)
      (dead-enemy (aref *klingon-sectors* i)
                  (aref *quadrant-contents* (coordinate-x (aref *klingon-sectors* i)) (coordinate-y (aref *klingon-sectors* i)))
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
      ((string= (aref *quadrant-contents* (coordinate-x c) (coordinate-y c)) +empty-sector+)
       (setf (aref *quadrant-contents* (coordinate-x c) (coordinate-y c)) entity)
       (return-from drop-entity-in-sector c))))

(defun drop-klingon-in-sector () ; coord newkling(int i)
  "Drop a new Klingon into the current quadrant. Return the sector coordinates, distance from the
ship, and Klingon power."

  (let ((c (drop-entity-in-sector +klingon+)))
  (return-from drop-klingon-in-sector (values c
                                              (distance *current-sector* c)
                                              (+ (* (random 1.0) 150.0) 300.0 (* 25.0 (skill-level-value *skill-level*))))))) ; Rand()*150.0 +300.0 +25.0*game.skill

(defun drop-commander-in-sector ()
  "Drop a new Commander into the current quadrant. Return the sector coordinates, distance from the
ship, and Commander power."

  (let ((c (drop-entity-in-sector +commander+)))
  (return-from drop-commander-in-sector (values c
                                                (distance *current-sector* c)
                                                (+ 950.0 (* (random 1.0) 400.0) (* 50.0 (skill-level-value *skill-level*))))))) ; 950.0+400.0*Rand()+50.0*game.skill

(defun drop-super-commander-in-sector ()
  "Drop a new Super-commander into the current quadrant. Return the sector coordinates, distance
from the ship, and Super-commander power."

  (let ((c (drop-entity-in-sector +super-commander+)))
  (return-from drop-super-commander-in-sector (values c
                                                      (distance *current-sector* c)
                                                      (+ 1175.0 (* (random 1.0) 400.0) (* 125.0 (skill-level-value *skill-level*))))))) ; 1175.0 + 400.0*Rand() + 125.0*game.skill

(defun drop-romulan-in-sector ()
  "Drop a new Romulan into the current quadrant. Return the sector coordinates, distance from the
ship, and Romulan power."

  (let ((c (drop-entity-in-sector +romulan+)))
  (return-from drop-romulan-in-sector (values c
                                              (distance *current-sector* c)
                                              (+ (* (random 1.0) 400.0) 450.0 (* 50.0 (skill-level-value *skill-level*))))))) ; Rand()*400.0 + 450.0 + 50.0*game.skill

(defun drop-space-thing-in-sector ()
  "Drop a Space Thing into the current quadrant. Return the sector coordinates, distance from the
ship, and Thing power."

  (let ((c (drop-entity-in-sector +thing+)))
  (return-from drop-space-thing-in-sector (values c
                                                  (distance *current-sector* c)
                                                  (+ (* (random 1.0) 6000.0) 500.0 (* 250.0 (skill-level-value *skill-level*))))))) ; Rand()*6000.0 +500.0 +250.0*game.skill

(defun drop-tholian-in-sector ()
  "Drop a Tholian into the current quadrant. Tholians only occupy the perimeter of a quadrant.
Return the sector coordinates, distance from the ship, and Tholian power."

  (do ((sector-ok-p nil)
       x y c)
      (sector-ok-p
       (setf (aref *quadrant-contents* x y) +thing+)
       (setf c (make-coordinate :x x :y y))
       (return-from drop-tholian-in-sector (values c
                                                   (distance *current-sector* c)
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
          (setf temp (aref *klingon-power* j))
          (setf (aref *klingon-power* j) (aref *klingon-power* (+ j 1)))
          (setf (aref *klingon-power* (+ j 1)) temp))))))

(defun new-quadrant (&key (show-thing t))
  "Set up a new quadrant when it is entered or re-entered. The thing should only be shown when the
player has reached a base by abandoning ship or using the SOS command."

  (setf *just-in-p* t)
  (setf *klingons-here* 0) ; TODO - a convenience variable to avoid referencing the galaxy array, keep it?
  (setf *commmanders-here* 0)
  (setf *super-commander-is-here-p* nil)
  (setf *romulans-here* 0) ; TODO - a convenience variable to avoid referencing the galaxy array, keep it?
  (setf *enemies-here* 0)
  (setf *quadrant-base* nil)
  (setf *current-planet* nil)
  (setf *planet-index* nil) ;  TODO - convenience variable to avoid referencing the galaxy array, keep it?
  (setf *romulan-neutral-zone-p* nil)
  (setf *in-orbit-p* nil)
  (setf *landedp* nil)
  (setf *attempted-escape-from-super-commander-p* nil)
  (setf *tholians-here* 0)
  (setf *things-here* 0)
  (setf *q-engry-p* nil)
  (setf *base-attack-report-seen-p* nil)
  (when *super-commander-attack-enterprise-p*
    ;; Attempt to escape Super-commander, so tractor beam back!
    (setf *super-commander-attack-enterprise-p* nil)
    (setf *attempted-escape-from-super-commander-p* t))

  ;; TODO - do the above variables need to be initialized if we are leaving the quadrant anyway?
  ;; Cope with supernova
  (unless (quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))
    ;; Clear/initialize quadrant
    (do ((i 0 (1+ i)))
        ((>= i +quadrant-size+))
      (do ((j 0 (1+ j)))
          ((>= j +quadrant-size+))
        (setf (aref *quadrant-contents* i j) +empty-sector+)))

    (setf *klingons-here*
          (quadrant-klingons (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))
    (setf *romulans-here*
          (quadrant-romulans (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))
    (setf *enemies-here* (+ *klingons-here* *romulans-here*))

    ;; Position starship. Do this first, all sectors are still empty.
    (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) *ship*)

    (when (> (quadrant-klingons (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0)
      (let ((remaining-klingons (quadrant-klingons (aref *galaxy*
                                                        (coordinate-x *current-quadrant*)
                                                        (coordinate-y *current-quadrant*)))))
        ;; Put the super-commander in the quadrant if present
        (when (same-coordinate-p *current-quadrant* *super-commander-quadrant*)
          (multiple-value-bind (coordinates distance power) (drop-super-commander-in-sector)
            (setf (aref *klingon-sectors* remaining-klingons) coordinates)
            (setf (aref *klingon-distance* remaining-klingons) distance)
            (setf (aref *klingon-average-distance* remaining-klingons) distance)
            (setf (aref *klingon-power* remaining-klingons) power)
            (setf remaining-klingons (- remaining-klingons 1))))
        ;; Put a Commander in the quadrant if there is one.
        (do ((i 0 (1+ i)))
            ((>= i *remaining-commanders*))
          (when (same-coordinate-p *current-quadrant* (aref *commander-quadrants* i))
            (multiple-value-bind (coordinates distance power) (drop-commander-in-sector)
              (setf (aref *klingon-sectors* remaining-klingons) coordinates)
              (setf (aref *klingon-distance* remaining-klingons) distance)
              (setf (aref *klingon-average-distance* remaining-klingons) distance)
              (setf (aref *klingon-power* remaining-klingons) power)
              (setf remaining-klingons (- remaining-klingons 1)))))
        ;; Position ordinary Klingons
        (do ((i 0 (1+ i)))
            ((>= i remaining-klingons))
          (multiple-value-bind (coordinates distance power) (drop-klingon-in-sector)
            (setf (aref *klingon-sectors* remaining-klingons) coordinates)
            (setf (aref *klingon-distance* remaining-klingons) distance)
            (setf (aref *klingon-average-distance* remaining-klingons) distance)
            (setf (aref *klingon-power* remaining-klingons) power)))))

    ;; Put in Romulans if needed
    (do ((r 1 (1+ r))) ; This is a count not an array reference.
        ((> r (quadrant-romulans (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))))
      (multiple-value-bind (coordinates distance power) (drop-romulan-in-sector)
        (setf (aref *klingon-sectors* r) coordinates)
        (setf (aref *klingon-distance* r) distance)
        (setf (aref *klingon-average-distance* r) distance)
        (setf (aref *klingon-power* r) power)))

    ;; If quadrant needs a starbase then put it in.
    (when (> (quadrant-starbases (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0)
      (setf *quadrant-base* (drop-entity-in-sector +starbase+)))

    ;; If quadrant needs a planet then put it in
    (when (/= (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) +no-planet+)
      (setf *planet-index* (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))
      (if (= (planet-inhabited (aref *planet-information* (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))) +uninhabited+)
          (setf *current-planet* (drop-entity-in-sector +planet+))
          (setf *current-planet* (drop-entity-in-sector +world+))))

    ;; Check for condition
    (update-condition)

    ;; And finally the stars
    (do ((i 1 (1+ i))) ; another count
        ((> i (quadrant-stars (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))))
      (drop-entity-in-sector +star+))

    ;; Check for Romulan Neutral Zone: Romulans present and no Klingons.
    (when (and (> (quadrant-romulans (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0)
               (= (quadrant-klingons (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) 0))
      (setf *romulan-neutral-zone-p* t)
      (when (not (damagedp +subspace-radio+))
        (print-message "LT. Uhura- \"Captain, an urgent message.")
        (print-message "  I'll put it on audio.\"  CLICK")
        (skip-line)
        (print-message "INTRUDER! YOU HAVE VIOLATED THE ROMULAN NEUTRAL ZONE.")
        (print-message "LEAVE AT ONCE, OR YOU WILL BE DESTROYED!")))

    ;; TODO - saw a thing at start of game, with no notice from Spock
    ;; Put in THING if needed
    (when (and show-thing
               (same-coordinate-p *thing-location* *current-quadrant*))
      (setf *thing-location* (get-random-quadrant))
      (1+ *enemies-here*)
      (setf *things-here* 1)
      (multiple-value-bind (coordinates distance power) (drop-space-thing-in-sector)
        (setf (aref *klingon-sectors* *enemies-here*) coordinates)
        (setf (aref *klingon-distance* *enemies-here*) distance)
        (setf (aref *klingon-average-distance* *enemies-here*) distance)
        (setf (aref *klingon-power* *enemies-here*) power))
      (unless (damagedp +short-range-sensors+)
        (print-message "Mr. Spock- \"Captain, this is most unusual.")
        (print-message "    Please examine your short-range scan.\"")))

    ;; Decide if quadrant needs a Tholian
    (when (or (and (< (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.02)) ; Lighten up if skill is low
              (and (= (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.05))
              (and (> (skill-level-value *skill-level*) +good+) (<= (random 1.0) 0.08)))
      (setf *tholians-here* 1)
      (1+ *enemies-here*)
      (multiple-value-bind (coordinates distance power) (drop-tholian-in-sector)
        (setf (aref *klingon-sectors* *enemies-here*) coordinates)
        (setf (aref *klingon-distance* *enemies-here*) distance)
        (setf (aref *klingon-average-distance* *enemies-here*) distance)
        (setf (aref *klingon-power* *enemies-here*) power))
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
        ((< i 3))
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
  "The ship is abandoned. If your current ship is the Faire Queene, or if your shuttlecraft is
dead, you're out of luck. You need the shuttlecraft in order for the captain (that's you!!) to
escape.

Your crew can beam to an inhabited starsystem in the quadrant, if there is one and if the
transporter is working. If there is no inhabited starsystem, or if the transporter is out, they
are left to die in outer space.

If there are no starbases left, you are captured by the Klingons, who torture you mercilessly.
However, if there is at least one starbase, you are returned to the Federation in a prisoner of war
exchange. Of course, this can't happen unless you have taken some prisoners."

  (print-message (format nil "Debug: *condition* [~A]" *condition*))
  (if (= *condition* +docked+)
      (when (string/= *ship* +enterprise+)
        (print-message "You cannot abandon Ye Faerie Queene.")
        (return-from abandon-ship nil))
      ;; Must take shuttle craft to exit
      (cond ((string= *ship* +faerie-queene+) ; C: game.damage[DSHUTTL]==-1
             (print-message "Ye Faerie Queene has no shuttle craft.")
             (return-from abandon-ship nil))
            ((< (aref *device-damage* +shuttle+) 0)
             (print-message "Shuttle craft now serving Big Macs.")
             (return-from abandon-ship nil))
            ((> (aref *device-damage* +shuttle+) 0)
             (print-message "Shuttle craft damaged.")
             (return-from abandon-ship nil))
            (*landedp*
             (print-message "You must be aboard the ship.")
             (return-from abandon-ship nil))
            ((string/= *landing-craft-location* "onship")
             (print-message "Shuttle craft not currently available.")
             (return-from abandon-ship nil))
            (t
             ;; Print abandon ship messages
             (skip-line)
             (print-message-slowly "***ABANDON SHIP!  ABANDON SHIP!")
             (skip-line)
             (print-message-slowly "***ALL HANDS ABANDON SHIP!")
             (skip-line 2)
             ;; TODO - this isn't consistent with the "Entire crew.." message below
             (print-message "Captain and crew escape in shuttle craft.")
             (when (= *remaining-bases* 0)
               ;; Oops! no place to go...
               (finish +abandon+)
               (return-from abandon-ship nil))
             ;; Dispose of crew.
             ;; Before the introduction of habitable planets the message was
             ;; "Remainder of ship's complement beam down"
             ;; "to nearest habitable planet."
             (if (and (/= (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*)(coordinate-y *current-quadrant*)))
                          +no-planet+)
                      (not (damagedp +transporter+)))
                 (print-message (format nil "Remainder of ship's complement beam down to ~A."
                                (aref *system-names*
                                      (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*)(coordinate-y *current-quadrant*))))))
                 (progn
                   (print-message (format nil "Entire crew of ~A left to die in outer space." *crew*))
                   (setf *casualties* (+ *casualties* *crew*))
                   (setf *abandoned-crew* (+ *abandoned-crew* *crew*))))
             ;; If at least one base left, give 'em the Faerie Queene
             (setf *dilithium-crystals-on-board-p* nil) ; crystals are lost
             (setf *probes-available* 0) ; No probes
             (skip-line)
             (print-message "You are captured by Klingons and released to")
             (print-message "the Federation in a prisoner-of-war exchange.")
             ;; Set up quadrant and position FQ adjacent to base
             (let ((base-index (truncate (* *remaining-bases* (random 1.0)))))
               (when (not (same-coordinate-p *current-quadrant* (aref *base-quadrants* base-index)))
                 (setf *current-quadrant* (aref *base-quadrants* base-index))
                 (setf (coordinate-x *current-sector*) (/ +quadrant-size+ 2))
                 (setf (coordinate-y *current-sector*) (/ +quadrant-size+ 2))
                 (new-quadrant)))
             ;; position next to base by trial and error
             (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*))
                   +empty-sector+)
             (do ((positionedp nil))
                 (positionedp)
               (do ((count 0 (1+ count)))
                   ((or (>= count 100) ; previously +quadrant-size+, don't give up so easily
                        positionedp))
                 (setf (coordinate-x *current-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-x *quadrant-base*))))
                 (setf (coordinate-y *current-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-y *quadrant-base*))))
                 (when (and (valid-sector-p (coordinate-x *current-sector*) (coordinate-y *current-sector*))
                            (string= (aref *quadrant-contents* (coordinate-x *current-sector*)
                                           (coordinate-y *current-sector*))
                                     +empty-sector+))
                   (setf positionedp t))) ; found a spot
               (unless positionedp
                 (setf (coordinate-x *current-sector*) (/ +quadrant-size+ 2))
                 (setf (coordinate-y *current-sector*) (/ +quadrant-size+ 2))
                 (new-quadrant))))))
  ;; Get new commission
  (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) +faerie-queene+)
  (setf *crew* +full-crew+)
  (print-message "Starfleet puts you in command of another ship,")
  (print-message "the Faerie Queene, which is antiquated but,")
  (print-message "still useable.")
  (when *dilithium-crystals-on-board-p*
    (print-message "The dilithium crystals have been moved."))
  (setf *miningp* nil)
  (setf *landing-craft-location* "offship") ; Galileo disappears
  ;; Resupply ship
  (setf *condition* +docked+)
  (do ((device-index 0 (1+ device-index)))
      ((>= device-index +number-of-devices+))
    (setf (aref *device-damage* device-index) 0.0))
  (setf (aref *device-damage* +shuttle+) -1) ; TODO - This is a flag to indicate no shuttle exists
  (setf *initial-energy* 3000.0)
  (setf *energy-level* 3000.0)
  (setf *initial-shield* 1250.0)
  (setf *shield-level* 1250.0)
  (setf *initial-torpedos* 6)
  (setf *torpedoes* 6)
  (setf *initial-life-support-reserves* 3.0)
  (setf *life-support-reserves* 3.0)
  (setf *shields-are-up-p* nil)
  (setf *warp-factor* 5.0))

(defun dock () ; C: dock(bool verbose)
  "Dock the ship at a starbase."

  (cond ((= *condition* +docked+)
         (print-message "Already docked."))
        (*in-orbit-p*
         (print-message "You must first leave standard orbit."))
        ((or (not *quadrant-base*)
             (> (abs (- (coordinate-x *current-sector*) (coordinate-x *quadrant-base*))) 1)
             (> (abs (- (coordinate-y *current-sector*) (coordinate-y *current-sector*))) 1))
         (print-message (format nil "~A not adjacent to base." (format-ship-name))))
        (t
         (setf *condition* +docked+)
         (print-message "Docked.")
         (setf *action-taken-p* t)
         (when (< *energy-level* *initial-energy*) ; Keep energy overload from dilithium crystals
           (setf *energy-level* *initial-energy*))
         (setf *shield-level* *initial-shield*)
         (setf *torpedoes* *initial-torpedos*)
         (setf *life-support-reserves* *initial-life-support-reserves*)
         (setf *crew* +full-crew+)
         (when (and (not (damagedp +subspace-radio+)) ; TODO - do you need the radio if you're docked?
                    (or *super-commander-attacking-base*
                        (is-scheduled-p +commander-destroys-base+))
                    (not *base-attack-report-seen-p*))
           ;; Get attack report from base
           (print-message "Lt. Uhura- \"Captain, an important message from the starbase:\"")
           (attack-report)
           (setf *base-attack-report-seen-p* t)))))

(defun delay (ms) ; C: #define delay(x) usleep(x*1000)
  "Pause for ms milliseconds."

  ;; TODO - write this
  (setf ms ms)
  )

(defun warble () ; C: void warble(void)
  "Sound and visual effects for teleportation"

  ;; TODO - write this
  )

(defun time-warp () ; C: timwrp(), /* let's do the time warp again */
  "Travel forward or backward in time."

  (print-message "***TIME WARP ENTERED.")
  (if (and *snapshot-taken-p*
           (< (random 1.0) 0.5))
      (progn
        ;; Go back in time
        (print-message (format nil "You are traveling backwards in time ~A stardates."'
                               (truncate (- *stardate* (snapshot-stardate *snapshot*)))))
        (use-snapshot)
        (setf *snapshot-taken-p* nil)
        (when (> *remaining-commanders* 0)
          (schedule-event +tractor-beam+ (expran (/ *initial-time* *remaining-commanders*)))
          (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*))))
        (schedule-event +supernova+ (expran (* 0.5 *initial-time*)))
        ;; Next snapshot will be sooner
        (schedule-event +snapshot-for-time-warp+ (expran (* 0.25 *remaining-time*)))
        (when (> *remaining-super-commanders* 0)
          (schedule-event +super-commander-move+ 0.2777))
        (setf *super-commander-attacking-base* 0)
        (unschedule +commander-destroys-base+)
        (unschedule +super-commander-destroys-base+)
        (setf *base-under-attack-quadrant* nil)
        ;; Make sure Galileo is consistant -- Snapshot may have been taken
        ;; when on planet, which would give us two Galileos!
        (do ((got-it-p nil)
             (l 0 (1+ l)))
            ((< l *initial-planets*)
             ;; Likewise, if in the original time the Galileo was abandoned, but
             ;; was on ship earlier, it would have vanished -- let's restore it.
             (when (and (string= *landing-craft-location* "offship")
                        (not got-it-p)
                        (>= (aref *device-damage* +shuttle+) 0.0))
               (print-message "Checkov-  \"Security reports the Galileo has reappeared in the dock!\"")
               (setf *landing-craft-location* "onship")))
          (when (= (planet-known (aref *planet-information* l)) 2) ; TODO - should be the symbol 'shuttle_down, not 2
            (setf got-it-p t)
            (when (and (string= *landing-craft-location* "onship")
                       (string= *ship* +enterprise+))
              (print-message "Checkov-  \"Security reports the Galileo has disappeared, Sir!")
              (setf *landing-craft-location* "offship"))))
        ;; There used to be code to do the actual reconstruction here,
        ;; but the starchart is now part of the snapshotted galaxy state.
        (print-message "Spock has reconstructed a correct star chart from memory"))
      (progn
        ;; Go forward in time
        (setf *time-taken-by-current-operation* (* -0.5 *initial-time* (log (random 1.0))))
        (print-message (format nil "You are traveling forward in time ~A stardates." *time-taken-by-current-operation*))
        ;; Cheat to make sure no tractor beams occur during time warp
        (postpone-event +tractor-beam+ *time-taken-by-current-operation*)
        (setf (aref *device-damage* +subspace-radio+)
              (+ (aref *device-damage* +subspace-radio+) *time-taken-by-current-operation*))))
  (new-quadrant)
  (process-events)) ; Stas Sergeev added this -- do pending events

(defun attack-player (&key (torpedoes-ok-p nil))
  "Enemies in quadrant attack the player. If torpedoes are not allowed (the default) then enemies
attack with phasers only."

  ;; TODO - write this
  (setf torpedoes-ok-p torpedoes-ok-p) ; quiet the compiler
  )

(defun ram (par1 par2 par3) ; C:

  (setf par1 par1)
  (setf par2 par2)
  (setf par3 par3)
  ;; TODO - write this
  )

;; TODO - Error: the ship doesn't always leave the quadrant, and no E appears in the short range scan
;; seems to occur when moving at warp 10 over a distance of 3 or so quadrats. The error was seen when
;; testing the time-warp code, by moving a few quadrants at warp 10.
(defun move-within-quadrant (&key (nova-push-p nil)) ; C: imove(bool novapush)
  "In-sector movement actions for warp and impulse drives. Supernova and tractor beam events
can occur."

  (when *in-orbit-p*
    (print-message "Helmsman Sulu- \"Leaving standard orbit.\"")
    (setf *in-orbit-p* nil))

  (let ((tractor-beam-scheduled-p nil)
        angle delta-x delta-y bigger
        n x y
        (s-coord (make-coordinate :x 0 :y 0))) ; C: w

    ;; If tractor beam is to occur, don't move full distance
    (when (>= (+ *stardate* *time-taken-by-current-operation*) (scheduled-for +tractor-beam+))
      (setf tractor-beam-scheduled-p t)
      (setf *condition* +red-status+)
      (setf *movement-distance* (+ (/ (* *movement-distance* (- (scheduled-for +tractor-beam+) *stardate*))
                                      *time-taken-by-current-operation*)
                                   0.1))
      (setf *time-taken-by-current-operation* (+ (- (scheduled-for +tractor-beam+) *stardate*) (expt 1 -5))))

    (setf angle (* (- 15.0 *movement-direction*) 0.5235988))
    (setf delta-x (* -1 (sin angle)))
    (setf delta-y (cos angle))
    (if (> (abs delta-x) (abs delta-y))
        (setf bigger (abs delta-x))
        (setf bigger (abs delta-y)))
    (setf delta-x (/ delta-x bigger))
    (setf delta-y (/ delta-y bigger))
    ;; Is this a hack, heuristic, or artifact of translation from C?
    ;; Set to zero delta values that are close to zero.
    (when (and (> delta-x -0.000001) (< delta-x 0.000001))
      (setf delta-x (truncate delta-x)))
    (when (and (> delta-y -0.000001) (< delta-y 0.000001))
      (setf delta-y (truncate delta-y)))
    (setf x (coordinate-x *current-sector*))
    (setf y (coordinate-y *current-sector*))
    (setf n (truncate (+ (* 10.0 *movement-distance* bigger) 0.5))) ; Simulate C asignment to int
    ;; Move within the quadrant
    (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) +empty-sector+)
    (do ((m 0 (1+ m))
         (movement-stopped-p nil))
        ((or movement-stopped-p
             (>= m n)))
      (setf x (+ x delta-x))
      (setf y (+ y delta-y))
      (setf (coordinate-x s-coord) (round x))
      (setf (coordinate-y s-coord) (round y))
      ;; Leaving the quadrant (and this function)
      (when (not (valid-sector-p (coordinate-x s-coord) (coordinate-y s-coord)))
        ;; Allow a final enemy attack unless being pushed by a nova.
        (when (and (not nova-push-p)
                   (/= *enemies-here* 0))
          (update-condition)
          (do ((m 1 (1+ m))) ; TODO - can compute average distance be a function? Needs a sector coord input
              ((> m *enemies-here*))
            (setf (aref *klingon-average-distance* m) (* 0.5 (+ (distance s-coord (aref *klingon-sectors* m))
                                                                (aref *klingon-distance* m)))))
          ;; Stas Sergeev added the condition that attacks only happen
          ;; if Klingons are present and your skill is good.
          (when (and (> (skill-level-value *skill-level*) +good+)
                     (> *klingons-here* 0) ; Romulans don't get another attack
                     (not (quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*)
                                                     (coordinate-y *current-quadrant*)))))
            (attack-player))
          (when *all-done-p*
            (return-from move-within-quadrant t)))
        ;; Compute final position -- new quadrant and sector
        (setf x (+ (* +quadrant-size+ (coordinate-x *current-quadrant*)) (coordinate-x *current-sector*)))
        (setf y (+ (* +quadrant-size+ (coordinate-y *current-quadrant*)) (coordinate-y *current-sector*)))
        ;; position in units of sectors
        (setf (coordinate-x s-coord) (truncate (+ x (* 10.0 *movement-distance* bigger delta-x) 0.5)))
        (setf (coordinate-y s-coord) (truncate (+ y (* 10.0 *movement-distance* bigger delta-y) 0.5)))
        ;; Check for edge of galaxy
        (do ((energy-barrier-crossed-p nil)
             (coordinate-adjusted-p t))
            ((not coordinate-adjusted-p)
             (when energy-barrier-crossed-p
               (1+ *energy-barrier-crossings*)
               (when (>= *energy-barrier-crossings* 3) ; Three strikes -- you're out!
                 (finish +3-negative-energy-barrier-crossings+)
                 (return-from move-within-quadrant t))
               (skip-line)
               (print-message "YOU HAVE ATTEMPTED TO CROSS THE NEGATIVE ENERGY BARRIER")
               (print-message "AT THE EDGE OF THE GALAXY.  THE THIRD TIME YOU TRY THIS,")
               (print-message "YOU WILL BE DESTROYED.")))
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
          (setf (coordinate-x *current-quadrant*)
                (truncate (1- (/ (+ (coordinate-x s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-y *current-quadrant*)
                (truncate (1- (/ (+ (coordinate-y s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-x *current-sector*) (- (coordinate-x s-coord)
                                                   (* +quadrant-size+ (coordinate-x *current-quadrant*))))
          (setf (coordinate-y *current-sector*) (- (coordinate-y s-coord)
                                                   (* +quadrant-size+ (coordinate-y *current-quadrant*))))
          (skip-line)
          (print-message (format nil "Entering ~A." (format-quadrant-coordinates *current-quadrant*)))
          (new-quadrant :show-thing nil)
          (when (> (skill-level-value *skill-level*) +novice+)
            (attack-player)))
        (return-from move-within-quadrant t))
      ;; Object encountered in flight path
      (when (string/= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +empty-sector+)
        (setf *movement-distance* (/ (distance *current-sector* s-coord) (* +quadrant-size+ 1.0)))
        (cond ((or (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord))
                             +tholian+) ; Ram a Tholian
                   ;; Ram enemy ship
                   (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +klingon+)
                   (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +commander+)
                   (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +super-commander+)
                   (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +romulan+)
                   (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +thing+))
               (setf *current-sector* s-coord)
               (ram nil (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) *current-sector*))
              ((string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +black-hole+)
               (skip-line)
               (print-message-slowly "***RED ALERT!  RED ALERT!")
               (skip-line)
               (print-message (format nil "*** ~A pulled into black hole at ~A"
                                      (format-ship-name) (format-sector-coordinates s-coord)))
               ;; Getting pulled into a black hole was certain death in Almy's original.
               ;; Stas Sergeev added a possibility that you'll get timewarped instead.
               (do ((damaged-devices 0)
                    (index 0 (1+ index))
                    probability-factor)
                   ((>= index +number-of-devices+)
                    (setf probability-factor (* (expt 1.4 (1- (/ (+ *energy-level* *shield-level*) 5000.0)))
                                                (expt 1.3 (1- (/ 1.0 (1+ damaged-devices))))))
                    (if (> (random 1.0) probability-factor)
                        (time-warp)
                        (finish +destroyed-by-black-hole+))
                    (return-from move-within-quadrant t))
                 (when (> (aref *device-damage* index) 0) ; TODO - define a "count damaged devices" function?
                   (1+ damaged-devices))))
              (t
               ;; Something else
               (let ((stop-energy (/ (* 50.0 *movement-distance*) *time-taken-by-current-operation*)))
                 (skip-line)
                 ;; TODO - need to set current window so print-out goes to the right place?
                 (if (string= (aref *quadrant-contents* (coordinate-x s-coord) (coordinate-y s-coord)) +tholian-web+)
                     (print-message (format nil "~A encounters Tholian web at ~A;"
                                            (format-ship-name) (format-sector-coordinates *current-sector*)))
                     (print-message (format nil "~A blocked by object at ~A;"
                                            (format-ship-name) (format-sector-coordinates *current-sector*))))
                 (print-message (format nil "Emergency stop required ~,2F units of energy." stop-energy))
                 (setf (coordinate-x s-coord) (truncate (- x delta-x))) ; simulate C float to int assignment
                 (setf (coordinate-y s-coord) (truncate (- y delta-y)))
                 (setf *energy-level* (- *energy-level* stop-energy))
                 (when (<= *energy-level* 0)
                   (finish +out-of-energy+)
                   (return-from move-within-quadrant t)))))
        (setf movement-stopped-p t)))
    (setf *current-sector* s-coord)
    ;; Movement completed and no quadrant change -- compute new average enemy distances
    (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) *ship*)
    (when (> *enemies-here* 0)
      (do ((m 0 (1+ m))
           (final-distance 0))
          ((> m *enemies-here*))
        (setf final-distance (distance *current-sector* (aref *klingon-sectors* m)))
        (setf (aref *klingon-average-distance* m) (* 0.5 (+ final-distance (aref *klingon-distance* m))))
        (setf (aref *klingon-distance* m) final-distance))
      (sort-klingons)
      (when (not (quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))
        (attack-player))
      (do ((m 0 (1+ m)))
          ((> m *enemies-here*))
        (setf (aref *klingon-average-distance* m) (aref *klingon-distance* m))))
    (update-condition)
    (draw-maps)
    (set-window *message-window*))) ; TODO - is this really needed?

;; TODO - the outputs of this function are movement direction and distance, either with values or
;;        nil. Direction of -1 also means "bad value entered" or "cancel"
(defun get-probe-course-and-distance () ; C: oid getcd(bool isprobe, int akey), for the probe
  "Get course direction and distance for moving the probe.

This program originally required input in terms of a (clock) direction and distance. Somewhere in
history, it was changed to cartesian coordinates. So we need to convert.  Probably \"manual\"
input should still be done this way -- it's a real pain if the computer isn't working! Manual mode
is still confusing because it involves giving x and y motions, yet the coordinates are always
displayed y - x, where +y is downward!"

  ;; If user types bad values, return with DIREC = -1.0.
  (setf *movement-direction* -1.0)

  (when *curses-interface-p*
    (set-window *message-window*))

  (let ((navigation-mode nil)
        delta-y delta-x)

    (when (damagedp +navigation-system+)
      (print-message "Computer damaged; manual navigation only")
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token)
        (navigation-mode)
      (when (not *line-tokens*) ; *input-item* is "move" on first loop, empty thereafter
        (print-prompt "Manual or automatic- "))
      (scan-input)
      (if (numberp *input-item*) ; No input (<enter> key only) will loop
          (progn
            (print-message "(Manual navigation assumed.)") ; Per the original docs
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
                  (return-from get-probe-course-and-distance nil))))))

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
             ;; TODO - Why add 0.5 to the entered coordinates? Commented out until I figure it out
             ;;(when (and sx sy)
             ;;  (setf sx (+ sx 0.5))
             ;;  (setf sy (+ sy 0.5)))
             ;;(when (and qx qy)
             ;;  (setf qx (+ qx 0.5))
             ;;  (setf qy (+ qy 0.5)))
             (when (and (not sx) (not sy))
               ;; Only quadrant specified -- go to center of destination quadrant
               (setf sx (+ qx 0.5))
               (setf sy (+ qy 0.5)))
             (if (and (valid-quadrant-p qx qy) (valid-sector-p sx sy))
                 (progn
                   (skip-line)
                   ;; Multiply sectors by 0.1 to scale them to the size of a quadrant
                   (setf delta-x (+ (- qy (coordinate-y *current-quadrant*))
                                    (* 0.1 (- sy (coordinate-y *current-sector*)))))
                   (setf delta-y (+ (- (coordinate-x *current-quadrant*) qx)
                                    (* 0.1 (- (coordinate-x *current-sector*) sx)))))
                 (progn
                   (huh)
                   (return-from get-probe-course-and-distance nil))))
          (print-prompt "Target quadrant or quadrant&sector- ")
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
                 (return-from get-probe-course-and-distance nil)))
            (setf nothing nothing)
            (print-prompt "X and Y displacements- ")
            (clear-type-ahead-buffer)
            (scan-input))))
    ;; Check for zero movement
    (when (and (= delta-x 0) (= delta-y 0))
      (clear-type-ahead-buffer)
      (return-from get-probe-course-and-distance nil))
    (setf *movement-distance* (sqrt (+ (expt delta-x 2) (expt delta-y 2))))
    (setf *movement-direction* (* (atan delta-x delta-y) 1.90985932))
    (when (< *movement-direction* 0.0)
      (setf *movement-direction* (+ *movement-direction* 12.0)))
    (clear-type-ahead-buffer)))

;; TODO - the outputs of this function are movement direction and distance, either with values or
;;        nil. Direction of -1 also means "bad value entered" or "cancel"
(defun get-ship-course-and-distance () ; C: oid getcd(bool isprobe, int akey), but only for the ship
  "Get course direction and distance for moving the ship.

This program originally required input in terms of a (clock) direction and distance. Somewhere in
history, it was changed to cartesian coordinates. So we need to convert.  Probably \"manual\"
input should still be done this way -- it's a real pain if the computer isn't working! Manual mode
is still confusing because it involves giving x and y motions, yet the coordinates are always
displayed y - x, where +y is downward!"

  ;; If user types bad values, return with DIREC = -1.0.
  (setf *movement-direction* -1.0)

  (when *curses-interface-p*
    (set-window *message-window*))

  (when *landedp*
    (print-out "Captain! You can't leave standard orbit until you")
    (print-message "are back aboard the ship.")
    (clear-type-ahead-buffer)
    (return-from get-ship-course-and-distance nil))

  (let ((navigation-mode nil)
        (need-prompt-p nil)
        (feedback-from 'chekov) ; Who acknowledges, and how, depends on movement mode and whether or not the player needed a prompt. It seems confusing.
        delta-y delta-x)

    (when (damagedp +navigation-system+)
      (print-message "Computer damaged; manual movement only")
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token)
        (navigation-mode)
      (when (not *line-tokens*) ; *input-item* is "move" on first loop, empty thereafter
        (print-prompt "Manual or automatic- ")
        (setf need-prompt-p t))
      (scan-input)
      (if (numberp *input-item*) ; No input (<enter> key only) will loop
          (progn
            (print-message "(Manual movement assumed.)") ; Per the original docs
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
                  (return-from get-ship-course-and-distance nil))))))

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
             ;; TODO - Why add 0.5 to the entered coordinates? Commented out until I figure it out
             ;;(when (and sx sy)
             ;;  (setf sx (+ sx 0.5))
             ;;  (setf sy (+ sy 0.5)))
             ;;(when (and qx qy)
             ;;  (setf qx (+ qx 0.5))
             ;;  (setf qy (+ qy 0.5)))
             (when (and (not qy) (not qx))
               (setf qx (coordinate-x *current-quadrant*))
               (setf qy (coordinate-y *current-quadrant*))
               (setf feedback-from 'sulu))
             (if (and (valid-quadrant-p qx qy) (valid-sector-p sx sy))
                 (progn
                   (skip-line)
                   (if (eql feedback-from 'chekov)
                       (print-message "Ensign Chekov- \"Course laid in, Captain.\"")
                       (when need-prompt-p
                         ;; Displayed coordinates are integers, use truncate
                         (print-message (format nil "Helmsman Sulu- \"Course locked in for ~A.\""
                                                (format-sector-coordinates (make-coordinate :y (truncate sx)
                                                                                            :x (truncate sy)))))
                         (setf feedback-from 'nobody)))
                   ;; Multiply sectors by 0.1 to scale them to the size of a quadrant
                   (setf delta-x (+ (- qy (coordinate-y *current-quadrant*))
                                    (* 0.1 (- sy (coordinate-y *current-sector*)))))
                   (setf delta-y (+ (- (coordinate-x *current-quadrant*) qx)
                                    (* 0.1 (- (coordinate-x *current-sector*) sx)))))
                 (progn
                   (huh)
                   (return-from get-ship-course-and-distance nil))))
          (print-prompt "Destination sector or quadrant&sector- ")
          (clear-type-ahead-buffer)
          (setf need-prompt-p t)
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
                 (return-from get-ship-course-and-distance nil))
               (setf feedback-from 'sulu))
            (setf nothing nothing)
            (print-prompt "X and Y displacements- ")
            (clear-type-ahead-buffer)
            (scan-input))))
    ;; Check for zero movement
    (when (and (= delta-x 0) (= delta-y 0))
      (clear-type-ahead-buffer)
      (return-from get-ship-course-and-distance nil))
    (when (eql feedback-from 'sulu)
      (skip-line)
      (print-message "Helmsman Sulu- \"Aye, Sir.\""))
    (setf *movement-distance* (sqrt (+ (expt delta-x 2) (expt delta-y 2))))
    (setf *movement-direction* (* (atan delta-x delta-y) 1.90985932))
    (when (< *movement-direction* 0.0)
      (setf *movement-direction* (+ *movement-direction* 12.0)))
    (clear-type-ahead-buffer)))

(defun move-under-impulse-power () ; C: impulse(void)

  (when (damagedp +impluse-engines+)
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message "Engineer Scott- \"The impulse engines are damaged, Sir.\"")
    (return-from move-under-impulse-power nil))

  (get-ship-course-and-distance) ; TODO this could probably be a multiple-value-bind
  (when (= *movement-direction* -1.0) ; TODO test this
    (return-from move-under-impulse-power nil))

  (when (or (>= (+ 20.0 (* 100.0 *movement-distance*)) *energy-level*)
            (<= *energy-level* 30.0))
    ;; Insufficient power for trip
    (skip-line)
    (print-message "First Officer Spock- \"Captain, the impulse engines")
    (print-message "require 20.0 units to engage, plus 100.0 units per")
    (if (> *energy-level* 30.0)
        (print-message (format nil "quadrant.  We can go, therefore, a maximum of ~A quadrants.\""
                               (truncate (- (* 0.01 (- *energy-level* 20.0)) 0.05))))
        (print-message "quadrant.  They are, therefore, useless.\""))
    (clear-type-ahead-buffer)
    (return-from move-under-impulse-power nil))

  ;; Make sure enough time is left for the trip
  (when (>= (/ *movement-distance* 0.095) *remaining-time*)
    (print-message "First Officer Spock- \"Captain, our speed under impulse")
    (print-message "power is only 0.95 sectors per stardate.")
    (print-prompt "Are you sure we dare spend the time?\" ")
    (when (string= (get-y-or-n) "n")
      (return-from move-under-impulse-power nil)))

  ;; Activate impulse engines and pay the cost
  (move-within-quadrant)
  (setf *action-taken-p* t)
  (when *all-done-p*
    (return-from move-under-impulse-power nil))
  (setf *energy-level* (- *energy-level* (+ 20.0 (* 100.0 *movement-distance*))))
  (setf *time-taken-by-current-operation* (/ *movement-distance* 0.095))
  (when (<= *energy-level* 0)
    (finish +out-of-energy+)))

(defun execute-warp-move () ; C: part of warp(bool timewarp)
  "Carry out warp movement that was set up by player command or emergency override (fast exit from
quadrant experiencing a supernova)."

  (let ((time-warp-p nil)
        (engine-damage-p nil))
    (when (> *warp-factor* 6.0)
      ;; Decide if engine damage will occur
      (when (> (/ (* *movement-distance* (expt (- 6.0 *warp-factor*) 2)) 66.666666666)
               (random 1.0))
        (setf engine-damage-p t)
        (setf *movement-distance* (* *movement-distance* (random 1.0))))
      ;; Decide if time warp will occur
      (when (> (* 0.5 *movement-distance* (expt 7.0 (- *warp-factor* 10.0)) (random 1.0)))
        (setf time-warp-p t))
      ;; If time warp or engine damage, check path. If it is obstructed, don't do warp or damage.
      (when (or engine-damage-p time-warp-p)
        (let ((angle (* (- 15.0 *movement-direction*) 0.5235998))
              delta-x delta-y bigger x y n)
          (setf delta-x (* -1 (sin angle)))
          (setf delta-y (cos angle))
          (if (> (abs delta-x) (abs delta-y))
              (setf bigger (abs delta-x))
              (setf bigger (abs delta-y)))
          (setf delta-x (/ delta-x bigger))
          (setf delta-y (/ delta-y bigger))
          (setf n (+ (* 10.0 *movement-distance* bigger) 0.5))
          (setf x (coordinate-x *current-sector*))
          (setf y (coordinate-y *current-sector*))
          (do ((l 1 (1+ l))
               (ix nil)
               (iy nil))
              ((> l n))
            (setf x (+ x delta-x))
            (setf y (+ y delta-y))
            (setf ix (truncate (+ x 0.5)))
            (setf iy (truncate (+ y 0.5)))
            (when (and (valid-sector-p ix iy)
                       (string/= (aref *quadrant-contents* ix iy) +empty-sector+))
              (setf engine-damage-p nil)
              (setf time-warp-p nil))))))
    ;; Activate Warp Engines and pay the cost
    (move-within-quadrant)
    (when *all-done-p*
      (return-from execute-warp-move nil))
    (setf *energy-level* (- *energy-level* (* *movement-distance*
                                              (expt *warp-factor* 3)
                                              (if *shields-are-up-p* 2 1))))
    (when (<= *energy-level* 0)
      (finish +out-of-energy+))
    (setf *time-taken-by-current-operation* (/ (* 10.0 *movement-distance*)
                                               (expt *warp-factor* 2)))
    (when time-warp-p
      (time-warp))
    (when engine-damage-p
      (setf (aref *device-damage* +warp-engines+) (* *damage-factor* (+ (* 3.0 (random 1.0)) 1.0)))
      (skip-line)
      (print-message "Engineering to bridge--")
      (print-message "  Scott here.  The warp engines are damaged.")
      (print-message "  We'll have to reduce speed to warp 4."))
    (setf *action-taken-p* t)))

(defun move-under-warp-drive () ;  C: warp(bool timewarp)

  (when (> (aref *device-damage* +warp-engines+) 10.0)
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message "Engineer Scott- \"The warp engines are damaged, Sir.\"")
    (return-from move-under-warp-drive nil))

  (when (and (damagedp +warp-engines+)
             (> *warp-factor* 4.0))
    (clear-type-ahead-buffer)
    (skip-line)
    (print-message "Engineer Scott- \"Sorry, Captain. Until this damage")
    (print-message "  is repaired, I can only give you warp 4.\"")
    (return-from move-under-warp-drive nil))

  ;; Read in course and distance
  (get-ship-course-and-distance) ; TODO this could probably be a multiple-value-bind
  (when (= *movement-direction* -1.0) ; TODO test this
    (return-from move-under-warp-drive nil))

  (when *curses-interface-p*
    (set-window *message-window*))
  ;; TODO - put warp power calculations into a function?
  ;; Make sure starship has enough energy for the trip
  (let ((power (* (+ *movement-distance* 0.05) (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))
        (iwarp (truncate (expt (/ *energy-level* (+ *movement-distance* 0.05)) 0.333333333))))
    (when (>= power *energy-level*)
      ;; Insufficient power for trip
      (skip-line)
      (print-message "Engineering to bridge--")
      (if (or (not *shields-are-up-p*)
              (> (* 0.5 power) *energy-level*))
          (if (<= iwarp 0)
              (print-message "We can't do it, Captain. We don't have enough energy.")
              (progn
                (print-out (format nil "We don't have enough energy, but we could do it at warp ~A" iwarp))
                (if *shields-are-up-p*
                    (progn
                      (print-message ",")
                      (print-message "if you'll lower the shields."))
                    (print-message "."))))
          (print-message "We haven't the energy to go that far with the shields up."))
      (return-from move-under-warp-drive nil)))

  ;;Make sure enough time is left for the trip
  (setf *time-taken-by-current-operation* (/ (* 10.0 *movement-distance*) (expt *warp-factor* 2)))
  (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
    (skip-line)
    (print-message "First Officer Spock- \"Captain, I compute that such")
    (print-out (format nil "  a trip would require approximately %~,2F"
                       (/ (* 100.0 *time-taken-by-current-operation*) *remaining-time*)))
    (print-message " percent of our remaining time.\"")
    (print-prompt "\"Are you sure this is wise?\" ")
    (when (string= (get-y-or-n) "n")
      (setf *time-taken-by-current-operation* 0)
      (return-from move-under-warp-drive nil)))

  (execute-warp-move))

;; TODO - find player documentation for using this command
(defun launch-probe () ; C: probe(void)
  "Launch deep-space probe."

  ;; New code to launch a deep space probe
  (cond ((= *probes-available* 0)
         (clear-type-ahead-buffer)
         (skip-line)
         (if (string= *ship* +enterprise+)
             (print-message "Engineer Scott- \"We have no more deep space probes, Sir.\"")
             (print-message "Ye Faerie Queene has no deep space probes.")))
        ((damagedp +deep-space-probe+)
         (clear-type-ahead-buffer)
         (skip-line)
         (print-message "Engineer Scott- \"The probe launcher is damaged, Sir.\""))
        ((is-scheduled-p +move-deep-space-probe+)
         (clear-type-ahead-buffer)
         (skip-line)
         (if (and (damagedp +subspace-radio+)
                  (/= *condition* +docked+))
             (progn
               (print-message "Spock-  \"Records show the previous probe has not yet")
               (print-message "   reached its destination.\""))
             (print-message "Uhura- \"The previous probe is still reporting data, Sir.\"")))
        (t
         (scan-input)
         (when (= (length *input-item*) 0)
           ;; Slow mode, so let Kirk know how many probes there are left
           (print-message (format nil "~A probe~A left." *probes-available* (if (= *probes-available* 1) "" "s")))
           (print-prompt "Are you sure you want to fire a probe? ")
           (when (string= (get-y-or-n) "n")
             (return-from launch-probe nil)))
         (setf *probe-is-armed-p* nil)
         (if (and (not (numberp *input-item*))
                  (> (length (match-token *input-item* "armed")) 0))
             (progn
               (setf *probe-is-armed-p* t)) ; TODO - if no scan is needed on next line then remove progn
             (progn
               (print-prompt "Arm NOVAMAX warhead? ")
               (when (string= (get-y-or-n) "y")
                 (setf *probe-is-armed-p* t))))
         (get-probe-course-and-distance)
         (when (= *movement-direction* -1.0)
           (return-from launch-probe nil))
         (1- *probes-available*)
         (let ((angle (* (- 15.0 *movement-direction*) 0.5235988))
               bigger)
           (setf *probe-x-increment* (* -1 (sin angle))) ; C: game.probeinx = -sin(angle);
           (setf *probe-y-increment* (cos angle))
           (if (> (abs *probe-x-increment*)
                  (abs *probe-y-increment*))
               (setf bigger (abs *probe-x-increment*))
               (setf bigger (abs *probe-y-increment*)))
           (setf *probe-x-increment* (/ *probe-x-increment* bigger))
           (setf *probe-y-increment* (/ *probe-y-increment* bigger))
           ;; We will use better packing than original
           (setf *probe-x-coord* (+ (* (coordinate-x *current-quadrant*) +quadrant-size+)
                                    (coordinate-x *current-sector*)))
           (setf *probe-y-coord* (+ (* (coordinate-y *current-quadrant*) +quadrant-size+)
                                    (coordinate-y *current-sector*)))
           (setf *probe-current-quadrant* *current-quadrant*))
         (schedule-event +move-deep-space-probe+ 0.01) ; Time to move one sector
         (print-message "Ensign Chekov-  \"The deep space probe is launched, Captain.\"")
         (setf *action-taken-p* t))))

(defun set-warp-factor () ; C: setwarp(void)
  "Change the warp factor."

  (if (> (length *line-tokens*) 0)
      (scan-input)
      (setf *input-item* nil))
  (do (whatever)
      (*input-item*)
    (setf whatever whatever)
    (print-prompt "Warp factor- ")
    (scan-input))
  (if (numberp *input-item*)
      (cond ((> (aref *device-damage* +warp-engines+) 10.0)
             (print-message "Warp engines inoperative."))
            ((and (damagedp +warp-engines+) (> *input-item* 4.0))
             (print-message "Engineer Scott- \"I'm doing my best, Captain,")
             (print-message "  but right now we can only go warp 4.\""))
            ((> *input-item* 10.0)
             (print-message "Helmsman Sulu- \"Our top speed is warp 10, Captain.\""))
            ((< *input-item* 1.0)
             (print-message "Helmsman Sulu- \"We can't go below warp 1, Captain.\""))
            (t
             (cond ((or (<= *input-item* *warp-factor*)
                       (<= *input-item* 6.0))
                       (print-message (format nil "Helmsman Sulu- \"Warp factor ~A, Captain.\"" (truncate *input-item*))))
                   ((< *input-item* 8.00)
                    (print-message "Engineer Scott- \"Aye, but our maximum safe speed is warp 6.\""))
                   ((= *input-item* 10.0)
                    (print-message "Engineer Scott- \"Aye, Captain, we'll try it.\""))
                   (t
                    (print-message "Engineer Scott- \"Aye, Captain, but our engines may not take it.\"")))
             (setf *warp-factor* *input-item*)))
      (huh)))

;; events.c

(defun wait () ; C: wait(void)
  "Wait on events."

  ;; TODO - write this
  )

;;; finish.c

(defun self-destruct () ; C: selfdestruct(void)
  "Self-destruct maneuver."

  ;; Finish with a BANG!
  (clear-type-ahead-buffer)
  (skip-line)
  (if (damagedp +computer+)
      (print-message "Computer damaged; cannot execute destruct sequence.")
      (progn
        (print-message-slowly "---WORKING---")
        (print-message-slowly "SELF-DESTRUCT-SEQUENCE-ACTIVATED")
        (print-message-slowly "   10")
        (print-message-slowly "       9")
        (print-message-slowly "          8")
        (print-message-slowly "             7")
        (print-message-slowly "                6")
        (skip-line)
        (print-message "ENTER-CORRECT-PASSWORD-TO-CONTINUE-")
        (skip-line)
        (print-message "SELF-DESTRUCT-SEQUENCE-OTHERWISE-")
        (skip-line)
        (print-message "SELF-DESTRUCT-SEQUENCE-WILL-BE-ABORTED")
        (skip-line)
        (when *curses-interface-p*
          (set-window *prompt-window*)
          (clear-window))
        (clear-type-ahead-buffer)
        (scan-input)
        (when (numberp *input-item*)
          (setf *input-item* (write-to-string *input-item*))
        (if (string= *self-destruct-password* *input-item*)
            (progn
              (print-message-slowly "PASSWORD-ACCEPTED")
              (print-message-slowly "                   5")
              (print-message-slowly "                      4")
              (print-message-slowly "                         3")
              (print-message-slowly "                            2")
              (print-message-slowly "                              1")
              (when (< (random 1.0) 0.15)
                  (print-message-slowly "GOODBYE-CRUEL-WORLD")
                (skip-line))
              (kaboom))
            (progn
              (print-message-slowly "PASSWORD-REJECTED;")
              (skip-line)
              (print-message-slowly "CONTINUITY-EFFECTED")
              (skip-line)))))))

;;; reports.c

(defun calculate-eta () ; C: eta(void)
  "Use computer to get estimated time of arrival for a warp jump."

  (skip-line)

  (when (damagedp +computer+)
    (print-message "COMPUTER DAMAGED, USE A POCKET CALCULATOR.")
    (skip-line)
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
                  (setf sx (if (> (coordinate-y *current-quadrant*) qx) 1 +quadrant-size+))
                  (setf sy (if (> (coordinate-x *current-quadrant*) qy) 1 +quadrant-size+))))
            ;; The C source adds 0.1, why?
            (setf trip-distance
                  (sqrt (+ (expt (* (+ (- qy (coordinate-y *current-quadrant*)) 0.1)
                                    (- sy (coordinate-y *current-sector*)))
                                 2)
                           (expt (* (+ (- qx (coordinate-x *current-quadrant*)) 0.1)
                                    (- sx (coordinate-x *current-sector*)))
                                 2))))
            (setf destination-quadrant (make-coordinate :x qx :y qy)))
          (return-from calculate-eta nil)))

    (when need-prompt
      (print-message "Answer \"no\" if you don't know the value:"))
    (do ((small-value 1e-10)) ; No loop variables needed so name a constant to avoid compiler feedback
        ((or ttime twarp))
      (clear-type-ahead-buffer)
      (print-prompt "Time or arrival date? ")
      (scan-input)
      (when (numberp *input-item*)
        (setf ttime *input-item*)
        (when (> ttime *stardate*)
          (setf ttime (- ttime *stardate*))) ; Actually a star date
        (setf twarp (/ (+ (floor (* (sqrt (/ (* 10.0 trip-distance) ttime)) 10.0)) 1.0) 10.0))
        (when (or (<= ttime small-value)
                  (> twarp 10))
          (print-message "We'll never make it, sir.")
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
        (print-message "Captain, certainly you can give me one of these.")))

    (skip-line)

    (do ((try-another-warp-factor-p t))
        ((not try-another-warp-factor-p))
      (setf try-another-warp-factor-p nil)
      (clear-type-ahead-buffer)
      (setf ttime (/ (* 10.0 trip-distance) (expt twarp 2)))
      (setf tpower (* trip-distance (expt twarp 3) (if *shields-are-up-p* 2 1)))
      (if (>= tpower *energy-level*)
          (progn
            (print-message "Insufficient energy, sir.")
            (when (or (not *shields-are-up-p*)
                      (> tpower (* *energy-level* 2.0)))
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
              (print-message "But if you lower your shields,")
              (print-out "remaining")
              (setf tpower (/ tpower 2))))
          (progn
            (when *curses-interface-p*
              (set-window *message-window*))
            (print-out "Remaining")))
      (unless try-another-warp-factor-p
        (print-message (format nil " energy will be ~,2F." (- *energy-level* tpower)))
        (cond (wfl
               (print-message (format nil "And we will arrive at stardate ~A." (format-stardate (+ *stardate* ttime)))))
              ((= twarp 1.0)
               (print-message "Any warp speed is adequate."))
              (t
               (print-message (format nil "Minimum warp needed is ~,2F," twarp))
               (print-message (format nil "and we will arrive at stardate ~A." (format-stardate (+ *stardate* ttime))))))
        (when (< *remaining-time* ttime)
          (print-message "Unfortunately, the Federation will be destroyed by then."))
        (when (> twarp 6.0)
          (print-message "You'll be taking risks at that speed, Captain."))
        ;; TODO - the event date dereference will fail if the event isn't scheduled. Fix the event system.
        (when (or (and (= *super-commander-attacking-base* 1)
                       (same-coordinate-p *super-commander-quadrant* destination-quadrant)
                       (< (event-date (aref *future-events* +super-commander-destroys-base+)) (+ *stardate* ttime)))
                  (and (< (event-date (aref *future-events* +commander-destroys-base+)) (+ *stardate* ttime))
                       (same-coordinate-p *base-under-attack-quadrant* destination-quadrant)))
          (print-message "The starbase there will be destroyed by then."))
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

  (skip-line)
  (print-message "       STAR CHART FOR THE KNOWN GALAXY")
  (when (> *stardate* *last-chart-update*)
    (print-message (format nil "(Last surveillance update ~A stardates ago)." (truncate (- *stardate* *last-chart-update*)))))
  (print-message "      1    2    3    4    5    6    7    8")
  (do ((x 0 (+ x 1)))
      ((>= x +galaxy-size+))
    (print-out (format nil "~A |" (+ x 1)))
    (do ((y 0 (+ y 1)))
        ((>= y +galaxy-size+))
      (if (and (= x (coordinate-x *current-quadrant*))
               (= y (coordinate-y *current-quadrant*)))
          (print-out "<")
          (print-out " "))
      (cond ((quadrant-supernovap (aref *galaxy* x y))
             (print-out "***"))
            ((and (not (quadrant-chartedp (aref *galaxy* x y)))
                  (> (quadrant-starbases (aref *galaxy* x y)) 0))
             (print-out ".1."))
            ((quadrant-chartedp (aref *galaxy* x y))
             (print-out (format nil "~A~A~A"
                                (starchart-page-klingons (aref *starchart* x y))
                                (starchart-page-starbases (aref *starchart* x y))
                                (starchart-page-stars (aref *starchart* x y)))))
            (t
             (print-out "...")))
      (if (and (= x (coordinate-x *current-quadrant*))
               (= y (coordinate-y *current-quadrant*)))
          (print-out ">")
          (print-out " ")))
    (print-out "|")
    (when (< x +galaxy-size+)
      (skip-line))))

(defun damage-report () ; C: damagereport(void)
  "List damaged devices and repair times for each."

  (skip-line)
  (do ((i 0 (+ i 1))
       (header-printed-p nil))
      ((>= i +number-of-devices+)
       (when (not header-printed-p)
         (print-message "All devices functional.")))
    (when (damagedp i)
      (when (not header-printed-p)
        (print-message (format nil "~12@A~24@A" "DEVICE" "-REPAIR TIMES-"))
        (print-message (format nil "~21A~8@A~8@A" " " "IN FLIGHT" "DOCKED"))
        (setf header-printed-p t))
      (print-message (format nil "  ~17A~9,2F~9,2F"
                             (aref *devices* i)
                             (+ (aref *device-damage* i) 0.05)
                             (+ (* (aref *device-damage* i) +docked-repair-factor+) 0.005)))))
  (skip-line))

(defun report ()
  "Report on general game status."

  (skip-line)
  (print-message (format nil "You are playing a ~A~A ~A game."
                         (if *restoredp* "restored " "")
                         (game-length-label *game-length*)
                         (skill-level-label *skill-level*)))
  (when (and (> (skill-level-value *skill-level*) +good+)
             *restoredp*
             (not *all-done-p*))
    (print-message "No plaque is allowed."))
  (when *tournament-number*
    (print-message (format nil "This is tournament game ~A." *tournament-number*)))
  (print-message (format nil "Your secret password is \"~A\"" *self-destruct-password*))
  (print-out (format nil "~A of ~A Klingons have been killed"
                     (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                        (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*))
                     (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
  (cond ((> (- *initial-commanders* *remaining-commanders*) 0)
         (print-out (format nil ", including ~A Commander~A."
                            (- *initial-commanders* *remaining-commanders*)
                            (if (= (- *initial-commanders* *remaining-commanders*) 1) "" "s"))))
        ((> (+ (- *initial-klingons* *remaining-klingons*)
               (- *initial-super-commanders* *remaining-super-commanders*))
            0)
         (print-message ", but no Commanders."))
        (t
         (print-message ".")))
  (when (> (skill-level-value *skill-level*) +fair+)
    (print-message (format nil "The Super Commander has ~Abeen destroyed."
                           (if (> *remaining-super-commanders* 0) "not " ""))))
  (if (/= *initial-bases* *remaining-bases*)
      (progn
        (print-out "There ")
        (if (= (- *initial-bases* *remaining-bases*) 1)
            (print-out "has been 1 base")
            (print-out (format nil "have been ~A bases" (- *initial-bases* *remaining-bases*))))
        (print-message (format nil " destroyed, ~A remaining." *remaining-bases*)))
      (print-message (format nil "There are ~A bases." *initial-bases*)))
  ;; Don't report this if not seen and either the radio is damaged or not at base!
  (when (or (not (damagedp +subspace-radio+))
            (= *condition* +docked+)
            *base-attack-report-seen-p*)
    (attack-report))
  (when (> *casualties* 0)
    (print-message (format nil "~A casualt~A suffered so far." *casualties* (if (= *casualties* 1) "y" "ies"))))
  (when (> *calls-for-help* 0)
    (print-message (format nil "There ~A ~A call~A for help."
                           (if (= *calls-for-help* 1) "was" "were")
                           *calls-for-help*
                           (if (= *calls-for-help* 1) "" "s"))))
  (when (string= *ship* +enterprise+)
    (print-message (format nil "You have ~A deep space probe~A."
                           (if (> *probes-available* 0) *probes-available* "no")
                           (if (/= *probes-available* 1) "s" ""))))
  (when (and (or (not (damagedp +subspace-radio+))
                 (= *condition* +docked+))
             (is-scheduled-p +move-deep-space-probe+))
    (if *probe-is-armed-p*
        (print-out "An armed deep space probe is in ")
        (print-out "A deep space probe is in "))
    (print-message (format nil "~A." (format-quadrant-coordinates *probe-current-quadrant*))))
  (when *dilithium-crystals-on-board-p*
    (if (<= *crystal-work-probability* 0.05)
        (print-message "Dilithium crystals aboard ship... not yet used.")
        (progn
          ;; Calculate number of times crystals have been used, and display it.
          (do ((i 0 (+ i 1))
               (ai 0.05 (* ai 2.0)))
              ((< *crystal-work-probability* ai)
               (print-message (format nil "Dilithium crystals have been used ~A time~A." i (if (= i 1) "" "s")))))))))

;; TODO - see the status function, there are two additional requests possible
(defun request () ; C: request()
  "Request a single item of status information."

  (skip-line)
  (let (req-item)
    (unless *line-tokens*
      (print-prompt "Information desired? "))
    (scan-input)
    (setf req-item (match-token *input-item* (list "date" "condition" "position" "lsupport" "warpfactor"
                                                   "energy" "torpedoes" "shields" "klingons" "time")))
    (unless req-item
      (setf req-item "?")) ; blank input still results in a help message
    (when *curses-interface-p*
      (set-window *message-window*))
    (cond ((string= req-item "date")
           (status 1))
          ((string= req-item "condition")
           (status 2))
          ((string= req-item "position")
           (status 3))
          ((string= req-item "lsupport")
           (status 4))
          ((string= req-item "warpfactor")
           (status 5))
          ((string= req-item "energy")
           (status 6))
          ((string= req-item "torpedoes")
           (status 7))
          ((string= req-item "shields")
           (status 8))
          ((string= req-item "klingons")
           (status 9))
          ((string= req-item "time")
           (status 10))
          (t
           (skip-line)
           (print-message "UNRECOGNIZED REQUEST. Valid requests are:")
           (print-message "  date, condition, position, lsupport, warpfactor,")
           (print-message "  energy, torpedoes, shields, klingons, time.")
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
    (when *curses-interface-p*
      (set-window *message-window*))
    (setf delta-index (floor (/ (+ *input-item* 22) 45)))
    (setf ix (+ (coordinate-x *current-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *current-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A,~A ~A " (+ ix 1) (+ iy 1) sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *current-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *current-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A " sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *current-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *current-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant-contents* ix iy)))
    (print-out (format nil "~A ~A,~A~%" sector-contents (+ ix 1) (+ iy 1)))
    (setf *time-taken-by-current-operation* 0.5)
    (setf *action-taken-p* t)))

;;; io.c

(defun clean-up ()
  "Cleanup function to run at program exit."

  (when *curses-interface-p*
    (endwin))

  ;; Remove this cleanup function from the exit hooks
  (delete '(clean-up) sb-ext:*exit-hooks*))

(defun set-text-color (color)
  "Set the current window to the specified color."

  (when (and *current-window*
             (has-colors))
    (cond ((equal color +default-color+)
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

  (setf sb-ext:*exit-hooks* (append sb-ext:*exit-hooks* '(clean-up))) ; Register a cleanup function to run on program exit.

  ;; Seed the random number generator
  ;; TODO - save the random state when saving a game
  (setf *random-state* (make-random-state t))

  ;; IF a TERM environment variable is defined then assume we can use CURSES calls
  (unless (sb-ext:posix-getenv "TERM")
    (setf *curses-interface-p* nil))

  ;; debug: run without curses
  (setf *curses-interface-p* nil)

  (when *curses-interface-p*
     (initscr)
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

     (setf *short-range-scan-window* (newwin 12 24 0 0))
     (setf *status-window* (newwin 10 33 2 24))
     ;; Width of long range scan window forces error message wrap at the correct place
     (setf *long-range-scan-window* (newwin 5 19 0 57))
     (setf *report-window* (newwin 7 23 5 57))
     (setf *message-window* (newwin 0 0 12 0)) ; Game narrative and general output
     (setf *prompt-window* (newwin 1 0 (- *lines* 1) 0))
     (scrollok *message-window* true)
     ;; debug start - TODO remove these when the curses interface is stable
     ;;box *short-range-scan-window* 0 0)
     ;box *report-window* 0 0)
     ;box *status-window* 0 0)
     ;wrefresh *status-window*)
     ;box *long-range-scan-window* 0 0)
     ;box *message-window* 0 0)
     ;box *prompt-window* 0 0)
     ;wrefresh *short-range-scan-window*)
     ;wrefresh *report-window*)
     ;wrefresh *long-range-scan-window*)
     ;wrefresh *message-window*)
     ;wrefresh *prompt-window*)
     ;wgetch *prompt-window*)
     ;; debug end
     (set-text-color +default-color+))

  ;; Internal documentation of system names from the C sources
;;	/* \
;;	 * I used <http://www.memory-alpha.org> to find planets \
;;	 * with references in ST:TOS.  Eath and the Alpha Centauri \
;;	 * Colony have been omitted. \
;;	 * \
;;	 * Some planets marked Class G and P here will be displayed as class M \
;;	 * because of the way planets are generated. This is a known bug. \
;;	 */ \
;;	/* Federation Worlds */ \
;;	_("Andoria (Fesoan)"),	/* several episodes */ \
;;	_("Tellar Prime (Miracht)"),	/* TOS: "Journey to Babel" */ \
;;	_("Vulcan (T'Khasi)"),	/* many episodes */ \
;;	_("Medusa"),		/* TOS: "Is There in Truth No Beauty?" */ \
;;	_("Argelius II (Nelphia)"),/* TOS: "Wolf in the Fold" ("IV" in BSD) */ \
;;	_("Ardana"),		/* TOS: "The Cloud Minders" */ \
;;	_("Catulla (Cendo-Prae)"),	/* TOS: "The Way to Eden" */ \
;;	_("Gideon"),		/* TOS: "The Mark of Gideon" */ \
;;	_("Aldebaran III"),	/* TOS: "The Deadly Years" */ \
;;	_("Alpha Majoris I"),	/* TOS: "Wolf in the Fold" */ \
;;	_("Altair IV"),		/* TOS: "Amok Time */ \
;;	_("Ariannus"),		/* TOS: "Let That Be Your Last Battlefield" */ \
;;	_("Benecia"),		/* TOS: "The Conscience of the King" */ \
;;	_("Beta Niobe I (Sarpeidon)"),	/* TOS: "All Our Yesterdays" */ \
;;	_("Alpha Carinae II"),	/* TOS: "The Ultimate Computer" */ \
;;	_("Capella IV (Kohath)"),	/* TOS: "Friday's Child" (Class G) */ \
;;	_("Daran V"),		/* TOS: "For the World is Hollow and I Have Touched the Sky" */ \
;;	_("Deneb II"),		/* TOS: "Wolf in the Fold" ("IV" in BSD) */ \
;;	_("Eminiar VII"),		/* TOS: "A Taste of Armageddon" */ \
;;	_("Gamma Canaris IV"),	/* TOS: "Metamorphosis" */ \
;;	_("Gamma Tranguli VI (Vaalel)"),	/* TOS: "The Apple" */ \
;;	_("Ingraham B"),		/* TOS: "Operation: Annihilate" */ \
;;	_("Janus IV"),		/* TOS: "The Devil in the Dark" */ \
;;	_("Makus III"),		/* TOS: "The Galileo Seven" */ \
;;	_("Marcos XII"),		/* TOS: "And the Children Shall Lead", */ \
;;	_("Omega IV"),		/* TOS: "The Omega Glory" */ \
;;	_("Regulus V"),		/* TOS: "Amok Time */ \
;;	_("Deneva"),		/* TOS: "Operation -- Annihilate!" */ \
;;	/* Worlds from BSD Trek */ \
;;	_("Rigel II"),		/* TOS: "Shore Leave" ("III" in BSD) */ \
;;	_("Beta III"),		/* TOS: "The Return of the Archons" */ \
;;	_("Triacus"),		/* TOS: "And the Children Shall Lead", */ \
;;	_("Exo III"),		/* TOS: "What Are Little Girls Made Of?" (Class P) */ \
;;     	/* Others */
;;	_("Hansen's Planet"),	/* TOS: "The Galileo Seven" */
;;	_("Taurus IV"),		/* TOS: "The Galileo Seven" (class G) */
;;	_("Antos IV (Doraphane)"),	/* TOS: "Whom Gods Destroy", "Who Mourns for Adonais?" */
;;	_("Izar"),			/* TOS: "Whom Gods Destroy" */
;;	_("Tiburon"),		/* TOS: "The Way to Eden" */
;;	_("Merak II"),		/* TOS: "The Cloud Minders" */
;;	_("Coridan (Desotriana)"),	/* TOS: "Journey to Babel" */
;;	_("Iotia"),		/* TOS: "A Piece of the Action" */

  ;; Put the system names into their array
  ;; C: setup_names(void)
  (let ((name-list (list "Andoria (Fesoan)" "Tellar Prime (Miracht)" "Vulcan (T'Khasi)" "Medusa"
                         "Argelius II (Nelphia)" "Ardana" "Catulla (Cendo-Prae)" "Gideon"
                         "Aldebaran III" "Alpha Majoris I" "Altair IV" "Ariannus" "Benecia"
                         "Beta Niobe I (Sarpeidon)" "Alpha Carinae II" "Capella IV (Kohath)"
                         "Daran V" "Deneb II" "Eminiar VII" "Gamma Canaris IV"
                         "Gamma Tranguli VI (Vaalel)" "Ingraham B" "Janus IV" "Makus III"
                         "Marcos XII" "Omega IV" "Regulus V" "Deneva" "Rigel II" "Beta III"
                         "Triacus" "Exo III" "Hansen's Planet" "Taurus IV" "Antos IV (Doraphane)"
                         "Izar" "Tiburon" "Merak II" "Coridan (Desotriana)" "Iotia")))
    (do ((name-index 0 (+ name-index 1))
         (system-name (pop name-list) (pop name-list)))
        ((>= name-index +habitable-planets+))
      (setf (aref *system-names* name-index) system-name)))

  ;; Put the device names into their array
  ;; C: setup_names(void)
  (let ((name-list (list "S. R. Sensors" "L. R. Sensors" "Phasers" "Photon Tubes" "Life Support"
                         "Warp Engines" "Impulse Engines" "Shields" "Subspace Radio"
                         "Shuttle Craft" "Computer" "Navigation System" "Transporter"
                         "Shield Control" "Death Ray" "D. S. Probe")))
    (do ((name-index 0 (+ name-index 1))
         (device-name (pop name-list) (pop name-list)))
        ((>= name-index +number-of-devices+))
      (setf (aref *devices* name-index) device-name)))
)

(defun draw-maps ()
  "Perform the automatic display updates available to curses-enabled terminals."

  (when *curses-interface-p*
    (short-range-scan)
    (set-window *status-window*)
    (wclear *status-window*)
    (wmove *status-window* 0 0)
    (set-window *report-window*)
    (wclear *report-window*)
    (wmove *report-window* 0 0)
    (all-statuses)
    (set-window *long-range-scan-window*)
    (wclear *long-range-scan-window*)
    (wmove *long-range-scan-window* 0 0)
    (long-range-scan)))

;;; planets.c

;; TODO - every operation that uses time should call this, e.g. visual-scan.
;; TODO - can this function be merged into the events function?
(defun consume-time () ; C: bool consumeTime(void)
  "Abort a lengthy operation if an event interrupts it. Return value of True indicates that there
was an event that requires aborting the operation carried out by the calling function."

  (setf *action-taken-p* t)
  (process-events)
  (when (or *all-done-p*
            (quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))
            *just-in-p*)
    (return-from consume-time t))
  (return-from consume-time nil))

;; TODO - with-compilation-unit should not be needed after ASDF is used to compile,
;;        using it because (shuttle) and (beam) reference each other
(with-compilation-unit ()

(defun shuttle () ; C: shuttle(void)
  "Use shuttlecraft for planetary jaunt."

  (skip-line)
  (cond ((damagedp +shuttle+)
         ;; TODO - The -1.0 value is a bizarre way of saying the current ship is the Faerie Queene
         (cond ((= (aref *device-damage* +shuttle+) -1.0)
                (if (and *in-orbit-p*
                         (shuttle-down-p))
                    (print-message "Ye Faerie Queene has no shuttle craft bay to dock it at.")
                    (print-message "Ye Faerie Queene had no shuttle craft.")))
               ((> (aref *device-damage* +shuttle+) 0)
                (print-message "The Galileo is damaged."))
               (t ; C: game.damage[DSHUTTL] < 0, or (< (aref *device-damage* +shuttle+) 0)
                (print-message "Shuttle craft is now serving Big Macs."))))
        ((not *in-orbit-p*)
         (print-message (format nil "~A not in standard orbit." (format-ship-name))))
        ((and (not (shuttle-down-p))
              (string/= *landing-craft-location* "onship"))
         (print-message "Shuttle craft not currently available."))
        ((and (not (shuttle-down-p))
              (not *landedp*))
         (print-message "You will have to beam down to retrieve the shuttle craft."))
        ((or *shields-are-up-p*
             (= *condition* +docked+))
         (print-message "Shuttle craft cannot pass through shields."))
        ((= (planet-known (aref *planet-information*
                                (quadrant-planet (aref *galaxy*
                                                       (coordinate-x *current-quadrant*)
                                                       (coordinate-y *current-quadrant*)))))
            +unknown+)
         (print-message "Spock-  \"Captain, we have no information on this planet")
         (print-message "  and Starfleet Regulations clearly state that in this situation")
         (print-message "  you may not fly down.\""))
        (t
         (setf *time-taken-by-current-operation* (* (expt 3.0 -5) *height-of-orbit*))
         (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
           (print-message"First Officer Spock-  \"Captain, I compute that such" )
           (print-message (format nil "  a maneuver would require approximately ~,2F% of our remaining time."
                                  (truncate (/ (* 100 *time-taken-by-current-operation*) *remaining-time*))))
           (print-prompt"Are you sure this is wise?\" " )
           (when (string= (get-y-or-n) "n")
             (setf *time-taken-by-current-operation* 0.0)
             (return-from shuttle nil)))
         (if *landedp*
             ;; Kirk on planet
             (if (string= *landing-craft-location* "onship")
                 ;; Galileo on ship!
                 (progn
                   (if (not (damagedp +transporter+))
                       (progn
                         (print-prompt "Spock-  \"Would you rather use the transporter?\" ")
                         (when (string= (get-y-or-n) "y")
                           (beam)
                           (return-from shuttle nil))
                         (print-out "Shuttle crew"))
                       (print-out "Rescue party"))
                   (print-message " boards Galileo and swoops toward planet surface.")
                   (setf *landing-craft-location* "offship")
                   (skip-line)
                   (when (consume-time)
                     (return-from shuttle nil))
                   (setf (planet-known (aref *planet-information*
                                             (quadrant-planet (aref *galaxy*
                                                                    (coordinate-x *current-quadrant*)
                                                                    (coordinate-y *current-quadrant*)))))
                         +shuttle-down+)
                   (print-message "Trip complete."))
                 (progn
                   ;; Ready to go back to ship
                   (print-message "You and your mining party board the")
                   (print-message "shuttle craft for the trip back to the Enterprise.")
                   (skip-line)
                   (print-message "The short hop begins . . .")
                   (skip-line 2)
                   (setf (planet-known (aref *planet-information*
                                             (quadrant-planet (aref *galaxy*
                                                                    (coordinate-x *current-quadrant*)
                                                                    (coordinate-y *current-quadrant*)))))
                         +known+)
                   (setf *in-landing-craft-p* t)
                   (setf *landedp* nil)
                   (when (consume-time)
                     (return-from shuttle nil))
                   (setf *in-landing-craft-p* nil)
                   (setf *landing-craft-location* "onship")
                   (when *miningp*
                     (setf *dilithium-crystals-on-board-p* t)
                     (setf *crystal-work-probability* 0.05))
                   (setf *miningp* nil)
                   (print-message "Trip complete.")))
             (progn
               ;; Kirk on ship
               ;; and so is Galileo
               (print-message "Mining party assembles in the hangar deck,")
               (print-message "ready to board the shuttle craft \"Galileo\".")
               (skip-line)
               (print-message "The hangar doors open; the trip begins.")
               (skip-line)
               (setf *in-landing-craft-p* t)
               (setf *landing-craft-location* "offship")
               (when (consume-time)
                 (return-from shuttle nil))
               (setf (planet-known (aref *planet-information*
                                         (quadrant-planet (aref *galaxy*
                                                                (coordinate-x *current-quadrant*)
                                                                (coordinate-y *current-quadrant*)))))
                     +shuttle-down+)
               (setf *in-landing-craft-p* nil)
               (setf *landedp* t)
               (print-message "Trip complete."))))))

(defun beam () ; C: beam(void)
  "Use the transporter."

  (let ((energy-needed (+ (* 50 *skill-level*) (/ *height-of-orbit* 100.0))))
    (skip-line)
    (when (damagedp +transporter+)
      (print-message "Transporter damaged.")
      ;; The shuttle is an option if it is not damaged and on the planet or on the ship
      (when (and (not (damagedp +shuttle+))
                 (or (shuttle-down-p)
                     (string= *landing-craft-location* "onship")))
        (skip-line)
        (print-prompt "Spock-  \"May I suggest the shuttle craft, Sir?\" ")
        (when (string= (get-y-or-n) "y")
          (shuttle)))
      (return-from beam nil))
    (when (not *in-orbit-p*)
      (print-message (format nil "~A not in standard orbit." (format-ship-name)))
      (return-from beam nil))
    (when *shields-are-up-p*
      (print-message "Impossible to transport through shields.")
      (return-from beam nil))
    (when (= (planet-known (aref *planet-information* (quadrant-planet (aref *galaxy* (coordinate-x *current-quadrant*)
                                                                             (coordinate-y *current-quadrant*)))))
             +unknown+)
      (print-message "Spock-  \"Captain, we have no information on this planet")
      (print-message "  and Starfleet Regulations clearly state that in this situation")
      (print-message "  you may not go down.\"")
      (return-from beam nil))
    (when (and (not *landedp*)
               (= (planet-crystals (aref *planet-information* (quadrant-planet (aref *galaxy*
                                                                                     (coordinate-x *current-quadrant*)
                                                                                     (coordinate-y *current-quadrant*)))))
                  +absent+))
      (print-message "Spock-  \"Captain, I fail to see the logic in")
      (print-message "  exploring a planet with no dilithium crystals.")
      (print-prompt "  Are you sure this is wise?\" ")
      (when (string= (get-y-or-n) "n")
        (return-from beam nil)))
    (when (> energy-needed *energy-level*)
      (print-message "Engineering to bridge--")
      (print-message "  Captain, we don't have enough energy for transportation.")
      (return-from beam nil))
    (when (and (not *landedp*)
               (> (* 2 energy-needed) *energy-level*))
      (print-message "Engineering to bridge--")
      (print-message "  Captain, we have enough energy only to transport you down to")
      (print-message "  the planet, but there wouldn't be any energy for the trip back.")
      (when (shuttle-down-p)
        (print-message "  Although the Galileo shuttle craft may still be on the surface."))
      (print-prompt "  Are you sure this is wise?\" ")
      (when (string= (get-y-or-n) "n")
        (return-from beam nil)))
    (if *landedp*
        ;; Coming from planet
        (progn
          (when (shuttle-down-p)
            (print-prompt "Spock-  \"Wouldn't you rather take the Galileo?\" ")
            (when (string= (get-y-or-n) "y")
              (return-from beam nil)))
          (print-message "Your crew hides the Galileo to prevent capture by aliens.")
          (print-message "Landing party assembled, ready to beam up.")
          (skip-line)
          (print-message "Kirk whips out communicator...")
          (print-message-slowly "BEEP  BEEP  BEEP")
          (skip-line 2)
          (print-message "\"Kirk to enterprise-  Lock on coordinates...energize.\""))
        ;; Going to planet
        (progn
          (print-message "Scotty-  \"Transporter room ready, Sir.\"")
          (skip-line)
          (print-message "Kirk and landing party prepare to beam down to planet surface.")
          (skip-line)
          (print-message "Kirk-  \"Energize.\"")))
    (setf *action-taken-p* t)
    (skip-line)
    (print-message-slowly "WWHOOOIIIIIRRRRREEEE.E.E.  .  .  .  .   .    .")
    (skip-line 2)
    (when (> (random 1.0) 0.98)
      (print-message-slowly "BOOOIIIOOOIIOOOOIIIOIING . . .")
      (skip-line 2)
      (print-message "Scotty-  \"Oh my God!  I've lost them.\"")
      (finish +lost+)
      (return-from beam nil))
    (print-message-slowly ".    .   .  .  .  .  .E.E.EEEERRRRRIIIIIOOOHWW")
    (setf *landedp* (not *landedp*))
    (setf *energy-level* (- *energy-level* energy-needed))
    (skip-line 2)
    (print-message "Transport complete.")
    (when (and *landedp*
               (shuttle-down-p))
      (print-message "The shuttle craft Galileo is here!"))
    (when (and (not *landedp*)
               *miningp*)
      (setf *dilithium-crystals-on-board-p* t)
      (setf *crystal-work-probability* 0.05)) ; TODO - 0.05 is also a flag value indicating mining has been completed
    (setf *miningp* nil)))

) ; with-compilation-unit

(defun deathray () ; C: deathray(void)
  "Use the big zapper."

  ;; resume here
  (setf *action-taken-p* nil)
  (skip-line)
  (cond ((sting/= *ship* +faerie-queene+)
         (print-message "Ye Faerie Queene has no death ray."))
        ((= *enemies-here* 0)
         (print-message"Sulu-  \"But Sir, there are no enemies in this quadrant.\"" ))
        ((damagedp +death-ray+)
         (print-message "Death Ray is damaged."))
        (t
         (print-message "Spock-  \"Captain, the 'Experimental Death Ray'")
         (print-message "  is highly unpredictible.  Considering the alternatives,")
         (print-prompt "  are you sure this is wise?\" ")
         (when (string= (get-y-or-n) "n")
           (return-from deathray nil))
         (print-message "Spock-  \"Acknowledged.\"")
         (skip-line)
         (setf *action-taken-p* t)
         (print-message-slowly "WHOOEE ... WHOOEE ... WHOOEE ... WHOOEE")
         (skip-line)
         (print-message "Crew scrambles in emergency preparation.")
         (print-message "Spock and Scotty ready the death ray and")
         (print-message "prepare to channel all ship's power to the device.")
         (skip-line)
         (print-message "Spock-  \"Preparations complete, sir.\""))
         (print-message "Kirk-  \"Engage!\"")
         (skip-line)
         (print-message-slowly "WHIRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR")
         (skip-line)
         (let ((deathray-prob 0.30)
               (r (random 1.0)))
           (if (> r deathray-prob)
               (progn
                 (print-message-slowly "Sulu- \"Captain!  It's working!\"")
                 (skip-line 2)
                 (while (> *enemies-here* 0)
                   (dead-enemy (aref *klingon-sectors* 1)
                               (aref *quadrant-contents*
                                     (coordinate-x (aref *klingon-sectors* 1))
                                     (coordinate-y (aref *klingon-sectors* 1)))
                               (aref *klingon-sectors* 1)))
                 (print-message "Ensign Chekov-  \"Congratulations, Captain!\"")
                 (when (= (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*) 0)
                   (finish +won+))
                 (print-message "Spock-  \"Captain, I believe the `Experimental Death Ray'")
                 (if (<= (random 1.0) 0.05)
                     (print-message "   is still operational.\"")
                     (progn
                       (print-message "   has been rendered nonfunctional.\"")
                       (setf (aref *device-damage* +death-ray+) 39.95))))
               (progn
                 ;; Pick failure method
                 (setf r (random 1.0))
                 (cond ((<= r 0.30)
                        (print-message-slowly "Sulu- \"Captain!  It's working!\"")
                        (skip-line)
                        (print-message-slowly "***RED ALERT!  RED ALERT!")
                        (skip-line)
                        (print-message "***MATTER-ANTIMATTER IMPLOSION IMMINENT!")
                        (skip-line)
                        ()
                        ()
                        ()
                        ()
                        ()
                        ())
                       ((<= r 0.55)
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ())
                       ((<= r 0.75)
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ())
                       (t
                        ()
                        ()
                        ()
                        ()
                        ()))))))))

(defun mine () ; C: mine(void)
  "Mine dilithium from a planet."

  (skip-line)
  (cond ((not *landedp*)
         (print-message "Mining party not on planet."))
        ((= (planet-crystals (aref *planet-information* (quadrant-planet (aref *galaxy*
                                                                               (coordinate-x *current-quadrant*)
                                                                               (coordinate-y *current-quadrant*)))))
            +mined+)
         (print-message "This planet has already been mined for dilithium."))
        ((= (planet-crystals (aref *planet-information* (quadrant-planet (aref *galaxy*
                                                                               (coordinate-x *current-quadrant*)
                                                                               (coordinate-y *current-quadrant*)))))
            +absent+)
         (print-message"No dilithium crystals on this planet." ))
        (*miningp*
         (print-message "You've already mined enough crystals for this trip."))
        ((and *dilithium-crystals-on-board-p*
              (= *crystal-work-probability* 0.05))
         (print-message
          (format nil "With all those fresh crystals aboard the ~A~%there's no reason to mine more at this time."
                  (format-ship-name))))
        (t
         (setf *time-taken-by-current-operation*
               (* (+ 0.1 (* 0.2 (random 1.0)))
                  (planet-class (aref *planet-information*
                                      (quadrant-planet (aref *galaxy*
                                                             (coordinate-x *current-quadrant*)
                                                             (coordinate-y *current-quadrant*)))))))
         (unless (consume-time)
           (print-message "Mining operation complete.")
           (setf (planet-crystals (aref *planet-information*
                                        (quadrant-planet (aref *galaxy*
                                                               (coordinate-x *current-quadrant*)
                                                               (coordinate-y *current-quadrant*)))))
                 +mined+)
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
    (print-message "Scotty rushes to the transporter controls.")
    (when *shields-are-up-p*
      (print-message "But with the shields up it's hopeless.")
      (finish finish-reason)
      (return-from quadrant-exit-while-on-planet nil))
    (print-message-slowly "His desperate attempt to rescue you . . .")
    (if (<= (random 1.0) 0.05)
        (progn
          (print-message "fails.")
          (finish finish-reason))
        (progn
          (print-message "SUCCEEDS!")
          (when *miningp*
            (setf *miningp* nil)
            (print-out "The crystals mined were ")
            (if (<= (random 1.0) 0.25)
                (print-message "lost.")
                (progn
                  (print-message "saved.")
                  (setf *dilithium-crystals-on-board-p* t))))))))

(defun emergency-supernova-exit () ; C: atover()
  "Handle emergency exit from the quadrant due to a supernova."

  (quadrant-exit-while-on-planet +mining-party-nova+)
  ;; TODO - is this the correct place for the landing craft check?
  ;; Check to see if captain in shuttle craft
  (when *in-landing-craft-p*
    (finish +shuttle-super-nova+)) ;; C used Shuttle Tractor Beam but that code couldn't be reached
  (unless *all-done-p*
    ;; Inform captain of attempt to reach safety
    (skip-line)
    (do ((power (* 0.75 *energy-level*))
         (required-distance (+ 1.4142 (random 1.0))))
        ;; Repeat if another supernova
        ((quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))))
      (when *just-in-p*
        (print-message-slowly "***RED ALERT!  RED ALERT!")
        (skip-line)
        (print-message (format nil "The ~A has stopped in a quadrant containing" (format-ship-name)))
        (print-message-slowly "   a supernova.")
        (skip-line 2))
      (print-message (format nil "***Emergency automatic override attempts to hurl ~A" (format-ship-name)))
      (print-message "safely out of quadrant.")
      (setf (quadrant-chartedp (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*))) t)
      ;; Try to use warp engines
      (when (damagedp +warp-engines+)
        (skip-line)
        (print-message "Warp engines damaged.")
        (finish +destroyed-by-supernova+)
        (return-from emergency-supernova-exit nil))
      (setf *warp-factor* (+ 6.0 (* 2.0 (random 1.0))))
      (print-message (format nil "Warp factor set to ~A" (truncate *warp-factor*)))
      (setf *movement-distance* (/ power (* (expt *warp-factor* 3) (if *shields-are-up-p* 2 1))))
      (when (< required-distance *movement-distance*)
        (setf *movement-distance* required-distance))
      (setf *time-taken-by-current-operation* (/ (* 10.0 *movement-distance*) (expt *warp-factor* 2)))
      (setf *movement-direction* (* 12.0 (random 1.0))) ; How dumb! (really?)
      (setf *just-in-p* nil)
      (setf *in-orbit-p* nil)
      (execute-warp-move)
      (when (not *just-in-p*)
        ;; This is bad news, we didn't leave the quadrant.
        (when *all-done-p*
          (return-from emergency-supernova-exit nil))
        (skip-line)
        ;; Not true in the case where the ship was blocked on the way out of the quadrant.
        (print-message "Insufficient energy to leave quadrant.")
        (finish +destroyed-by-supernova+)
        (return-from emergency-supernova-exit nil)))
    (when (= (+ *remaining-klingons* *remaining-commanders* *remaining-super-commanders*) 0)
      (finish +won+)))) ; Supernova killed remaining enemies.

(defun orbit () ; C: orbit(void)
  "Enter standard orbit."

  (skip-line)
  (cond (*in-orbit-p*
         (print-message "Already in standard orbit."))
        ((and (damagedp +warp-engines+)
             (damagedp +impluse-engines+))
         (print-message "Both warp and impulse engines damaged."))
        ((or (not *current-planet*)
             (> (abs (- (coordinate-x *current-sector*) (coordinate-x *current-planet*))) 1)
             (> (abs (- (coordinate-y *current-sector*) (coordinate-y *current-planet*))) 1))
         (print-message (format nil "~A not adjacent to planet." (format-ship-name)))
         (skip-line))
        (t
         (setf *time-taken-by-current-operation* (+ 0.02 (* 0.03 (random 1.0))))
         (print-message "Helmsman Sulu-  \"Entering standard orbit, Sir.\"")
         (update-condition)
         (unless (consume-time)
           (setf *height-of-orbit* (+ 1400.0 (* 7200.0 (random 1.0))))
           (print-message (format nil "Sulu-  \"Entered orbit at altitude ~,2F kilometers.\"" *height-of-orbit*))
           (setf *in-orbit-p* t)
           (setf *action-taken-p* t)))))

;; TODO - respond in a reasonable way if there is an inhabited planet in the quadrant. There will
;;        never be dilithium crystals (that's how the initialization code works - presumably if
;;        the planet is inhabited the crystals have already been mined, or are not available for
;;        the player to mine) but at least print the planet name.
(defun sensor () ; C: sensor(void)
  "Examine planets in this quadrant."

  (skip-line)
  (cond ((damagedp +short-range-sensors+)
         (print-message "Short range sensors damaged."))

        ((not *current-planet*) ; sector coordinates or nil
         (print-message (format nil "Spock- \"No planet in this quadrant, Captain.\"")))

        (t ; Go ahead and scan even if the planet has been scanned before.
         (print-message (format nil "Spock-  \"Sensor scan for ~A -" (format-quadrant-coordinates *current-quadrant*)))
         (skip-line)
         (print-out (format nil "         Planet at ~A is of class ~A.~%"
                            (format-sector-coordinates *current-planet*)
                            (format-planet-class (planet-class (aref *planet-information* *planet-index*)))))
         (when (planet-shuttle-landed-p (aref *planet-information* *planet-index*))
           (print-out (format nil "         Sensors show Galileo still on surface.~%")))
         (print-out "         Readings indicate")
         (when (= (planet-crystals (aref *planet-information* *planet-index*)) +absent+)
           (print-out " no"))
         (print-out (format nil " dilithium crystals present.\"~%"))
         (setf (planet-known (aref *planet-information* *planet-index*)) 1))))

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
  (cond ((= *condition* +docked+)
         (print-message "Lt. Uhura-  \"But Captain, we're already docked.\""))
        ((damagedp +subspace-radio+)
         (print-message "Subspace radio damaged."))
        ((= *remaining-bases* 0)
         (print-message "Lt. Uhura-  \"Captain, I'm not getting any response from Starbase.\""))
        (*landedp*
         (print-message (format nil "You must be aboard the ~A." (format-ship-name))))
        (t ; OK -- call for help from nearest starbase
         (1+ *calls-for-help*)
         (let (ddist probf)
           (if (coordinate-p *quadrant-base*)
               ;; There's one in this quadrant
               (setf ddist (distance *quadrant-base* *current-sector*))
               (progn
                 ;; Find the distance to the first base in the array of bases
                 (setf ddist (* +quadrant-size+ (distance (aref *base-quadrants* 0) *current-quadrant*)))
                 ;; Then try to find a base that's closer
                 (do ((base-index 1 (1+ base-index))
                      (destination-base-index 0)
                      xdist)
                     ((>= base-index *remaining-bases*)
                      ;; Since starbase not in quadrant, set up new quadrant.
                      ;; TODO - is this being set correctly?
                      (setf *current-quadrant* (aref *base-quadrants* destination-base-index))
                      (new-quadrant))
                   (setf xdist (* +quadrant-size+ (distance (aref *base-quadrants* base-index) *current-quadrant*)))
                   (when (< xdist ddist)
                     (setf ddist xdist)
                     (setf destination-base-index base-index)))))
           ;; dematerialize starship
           (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) +empty-sector+)
           (print-message (format nil "Starbase in ~A responds-- ~A dematerializes."
                                  (format-quadrant-coordinates *current-quadrant*)
                                  (format-ship-name)))
           (setf *current-sector* nil)
           (do ((five-tries 0 (1+ five-tries))
                (found-empty-sector-p nil)
                x y)
               ((or found-empty-sector-p
                    (>= five-tries 5)))
             (setf x (truncate (+ (coordinate-x *quadrant-base*) (* 3.0 (random 1.0)) -1))) ; C: game.base.x+3.0*Rand()-1;
             (setf y (truncate (+ (coordinate-y *quadrant-base*) (* 3.0 (random 1.0)) -1)))
             (when (and (valid-sector-p x y)
                        (string= (aref *quadrant-contents* x y) +empty-sector+))
               ;; found one -- finish up
               (setf *current-sector* (make-coordinate :x x :y y))
               (setf found-empty-sector-p t)))
           (when (not (coordinate-p *current-sector*))
             (print-message "You have been lost in space...")
             (finish +materialize+)
             (return-from mayday nil))
           ;; Give starbase three chances to rematerialize starship
           (setf probf (expt (- 1.0 (expt 0.98 ddist)) 0.33333333))
           (do ((three-tries 0 (1+ three-tries))
                (succeeds-p nil))
               ((or succeeds-p
                    (>= three-tries 3 ))
                (when (not succeeds-p)
                  (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) +thing+) ; question mark
                  (setf *alivep* nil)
                  (when *curses-interface-p* ; In curses mode sensors work automatically, call before updating display
                    (sensor))
                  (draw-maps)
                  (set-window *message-window*)
                  (finish +materialize+)
                  (return-from mayday nil)))
             (print-out (format nil "~A attempt to re-materialize ~A "
                                (cond ((= three-tries 0) "1st")
                                      ((= three-tries 1) "2nd")
                                      ((= three-tries 2) "3rd"))
                                (format-ship-name)))
             (warble)
             (if (> (random 1.0) probf)
                 (setf succeeds-p t)
                 (progn
                   (textcolor +red+)
                   (print-message "fails.")
                   (textcolor +default-color+)))
             (delay 500)) ; half of a second?
           (setf (aref *quadrant-contents* (coordinate-x *current-sector*) (coordinate-y *current-sector*)) *ship*)
           (textcolor +green+)
           (print-message "succeeds.")
           (textcolor +default-color+)
           (dock))
         (skip-line)
         (print-message "Lt. Uhura-  \"Captain, we made it!\""))))

;; TODO - there is no reason to limit the report to uninhabited planets. Sure, the only tactical
;;        reason for the report is to remember where the crystals were located, but go ahead and
;;        display inhabited worlds too. This is a "planet report" and the player gets a better
;;        sense of having a history in the game if all the planets that were scanned are reported.
(defun survey () ; C: survey(void)
  "Report on (uninhabited) planets in the galaxy."

  (clear-type-ahead-buffer)
  (skip-line)
  (print-message "Spock-  \"Planet report follows, Captain.\"")
  (skip-line)
  (do ((i 0 (1+ i))
       (one-planet-known-p nil))
      ((>= i *initial-planets*)
       (unless one-planet-known-p
         (print-message "No information available.")))
    (unless (= (planet-class (aref *planet-information* i)) +destroyed+)
      (when (and (/= (planet-known (aref *planet-information* i)) +unknown+)
                 (= (planet-inhabited (aref *planet-information* i)) +uninhabited+))
        (setf one-planet-known-p t)
        (print-out (format nil "~A   class ~A   "
                           (format-quadrant-coordinates (planet-quadrant (aref *planet-information* i)))
                           (format-planet-class (planet-class (aref *planet-information* i)))))
        (unless (= (planet-crystals (aref *planet-information* i)) +present+)
          (print-out "no "))
        (print-message "dilithium crystals present.")
        (when (= (planet-known (aref *planet-information* i)) +shuttle-down+)
          (print-message "    Shuttle Craft Galileo on surface."))))))

(defun use-crystals () ; C: usecrystals(void)
  "Use dilithium crystals."

  (setf *action-taken-p* nil)
  (skip-line)
  (when (not *dilithium-crystals-on-board-p*)
    (print-message "No dilithium crystals available.")
    (return-from use-crystals nil))
  (when (>= *energy-level* 1000.0)
    (print-message "Spock-  \"Captain, Starfleet Regulations prohibit such an operation")
    (print-message "  except when Condition Yellow exists.")
    (return-from use-crystals nil))
  (print-message "Spock- \"Captain, I must warn you that loading")
  (print-message "  raw dilithium crystals into the ship's power")
  (print-message "  system may risk a severe explosion.")
  (print-prompt "  Are you sure this is wise?\" ")
  (when (string= (get-y-or-n) "n")
    (return-from use-crystals nil))
  (skip-line)
  (print-message "Engineering Officer Scott-  \"(GULP) Aye Sir.")
  (print-message "  Mr. Spock and I will try it.\"")
  (skip-line)
  (print-message "Spock-  \"Crystals in place, Sir.")
  (print-message "  Ready to activate circuit.\"")
  (skip-line)
  (print-message-slowly "Scotty-  \"Keep your fingers crossed, Sir!\"")
  (skip-line)
  (when (<= (random 1.0) *crystal-work-probability*)
    (print-message-slowly "  \"Activating now! - - No good!  It's***")
    (skip-line 2)
    (print-message-slowly "***RED ALERT!  RED A*L********************************")
    (skip-line)
    (print-stars)
    (print-message-slowly "******************   KA-BOOM!!!!   *******************")
    (skip-line)
    (kaboom)
    (return-from use-crystals nil))
  (setf *energy-level* (+ *energy-level* (* 5000.0 (+ 1.0 (* 0.9 (random 1.0))))))
  (print-message-slowly "  \"Activating now! - - ")
  (print-message "The instruments")
  (print-message "   are going crazy, but I think it's")
  (print-message "   going to work!!  Congratulations, Sir!\"")
  (setf *crystal-work-probability* (* *crystal-work-probability* 2.0))
  (setf *action-taken-p* t))

(defun restore-game ()
  "Restore a saved game. Return true or false for restore success or failure."

  nil ; TODO - write the function
  )

(defun save-game (&optional (boss nil)) ; C: freeze(bool boss)
  "Save the game. If the boss is coming then save with a default name. Or you might consider
finding a job where playing Super Star Trek isn't a termination offense."

  ;; TODO - write this
  (setf boss boss) ; quiet the compiler
  )

(defun get-game-type () ; C: choose()
  "Prompt for type of game and perform setup based on selected game type.
Return game type, tournament number, and whether or not this is a restored game."

  (do ((game-type nil)
       (tournament-number nil)
       (restoredp nil))
      (game-type
       (return-from get-game-type (values game-type tournament-number restoredp)))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Would you like a Regular, Tournament, or Saved game? "))
    (scan-input)
    (setf game-type (match-token *input-item* (list "regular" "tournament" "saved")))
    (cond ((string= game-type "tournament")
           (unless *line-tokens*
             (print-prompt "Type in name or number of tournament: "))
           (scan-input)
           (if *input-item*
               (setf tournament-number *input-item*)
               (setf game-type nil))) ; no tournament name or number input so start over
          ((string= game-type "saved")
           (if (restore-game)
               (progn
                 (setf restoredp t)
                 (report)) ; TODO - do this in restore-game?
               (progn
                 (skip-line)
                 (print-message "Restore of saved game failed.")
                 (setf game-type nil)))) ; restore failed so there is no game type yet
          ((string= game-type "regular")
           t) ; Acknowledge the selection because any other input is an error.
          (t
           (when (> (length *input-item*) 0)
             (print-message (format nil "What is \"~A\"?" *input-item*)))
           (clear-type-ahead-buffer)))))

(defun get-game-length () ; C: choose()

  (do ((length nil))
      (length
       (setf *game-length* (make-game-length :label length))
       (cond ((string= length "short")
              (setf (game-length-value *game-length*) +short-game+))
             ((string= length "medium")
              (setf (game-length-value *game-length*) +medium-game+))
             ((string= length "long")
              (setf (game-length-value *game-length*) +long-game+))))
    (when (= (length *line-tokens*) 0)
      (print-prompt "Would you like a Short, Medium, or Long game? "))
    (scan-input)
    (setf length (match-token *input-item* (list "short" "medium" "long")))
    (unless length
      (when (> (length *input-item*) 0)
        (print-message (format nil "What is \"~A\"?" *input-item*)))
      (clear-type-ahead-buffer))))

(defun get-skill-level () ; C: choose()

  (do ((game-skill nil))
      (game-skill
       (setf *skill-level* (make-skill-level :label game-skill))
       (cond ((string= game-skill "novice")
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
      (when (> (length *input-item*) 0)
        (print-message (format nil "What is \"~A\"?" *input-item*)))
      (clear-type-ahead-buffer))))

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
values, expecially number of entities in the game, or read from a save file.
There are a lot of magic numbers in these settings."

  ;; Choose game type, length, skill level, and password
  (multiple-value-bind (game-type tournament-number restoredp) (get-game-type)
    (unless restoredp
      (get-game-length)
      (get-skill-level)
      (let ((game-password (get-game-password)))
        ;; Games restored from save files already have these.
        (setf *tournament-number* tournament-number)
        (when *tournament-number*
          (setf *random-state* (sb-ext:seed-random-state *tournament-number*)))
        (setf *restoredp* restoredp) ; TODO - set this in the restore-game function
        (setf *self-destruct-password* game-password))
      (setf *damage-factor* (* 0.5 (skill-level-value *skill-level*)))
      (setf *initial-bases* (max (+ (random +max-bases+) 1) +min-bases+))
      (setf *initial-planets* (+ (random (- +max-uninhabitable-planets+ +min-uninhabitable-planets+))
                                 +min-uninhabitable-planets+
                                 +habitable-planets+))
      (setf *initial-romulans* (* (+ (random 1) 2) (skill-level-value *skill-level*)))
      (setf *remaining-romulans* *initial-romulans*)
      (setf *initial-time* (* 7.0 (game-length-value *game-length*)))
      (setf *remaining-time* *initial-time*)
      (setf *initial-klingons*  (truncate (+ (* 2.0 *initial-time*
                                                (- (+ (skill-level-value *skill-level*) 1)  (random 1))
                                                (skill-level-value *skill-level*)
                                                0.1)
                                             0.15)))
      (setf *remaining-klingons* *initial-klingons*)
      (when (> *initial-klingons* 50) ; That's a lot of klingons, give the player another base
        (setf *initial-bases* (+ *initial-bases* 1)))
      (setf *remaining-bases* *initial-bases*)
      (setf *initial-commanders* (min +max-commanders-per-game+
                                      (+ (skill-level-value *skill-level*)
                                         (truncate (* 0.0626 *initial-klingons* (random 1.0))))))
      (setf *remaining-commanders* *initial-commanders*)
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
      (setf *ship* +enterprise+)
      (setf *crew* +full-crew+)
      (setf *initial-energy* 5000.0)
      (setf *energy-level* 5000.0)
      (setf *initial-shield* 2500.0)
      (setf *shield-level* 2500.0)
      (setf *shields-are-changing-p* nil)
      (setf *shields-are-up-p* nil)
      (setf *initial-life-support-reserves* 4.0)
      (setf *life-support-reserves* 4.0)
      (setf *current-quadrant* (get-random-quadrant))
      (setf *current-sector* (get-random-sector))
      (setf *initial-torpedos* 10)
      (setf *torpedoes* 10)
      ;; Give the player 2-4 of these wonders
      (setf *probes-available* (+ (random 2) 2))
      (setf *warp-factor* 5.0)
      (setf *warp-factor-squared* (expt *warp-factor* 2))
      (do ((i 0 (+ i 1)))
          ((>= i +number-of-devices+))
        (setf (aref *device-damage* i) 0.0))

      ;; Set up assorted game parameters
      (setf *initial-date* (* 100.0 (+ (* 31.0 (random 1.0)) 20.0))) ; C: 100.0*(int)(31.0*Rand()+20.0)
      (setf *stardate* *initial-date*)
      (setf *abandoned-crew* 0)
      (setf *casualties* 0)
      (setf *calls-for-help* 0)
      (setf *energy-barrier-crossings* 0)
      (setf *in-landing-craft-p* nil)
      (setf *dilithium-crystals-on-board-p* nil)
      (setf *crystal-work-probability* 0.0)
      (setf *miningp* nil)
      (setf *resting* nil)
      (setf *super-commander-attack-enterprise-p* nil)
      (setf *super-commander-attacking-base* 0)
      (setf *destroyed-inhabited-planets* 0)
      (setf *destroyed-uninhabited-planets* 0)
      (setf *destroyed-bases* 0)
      (setf *destroyed-stars* 0)
      (setf *landing-craft-location* "onship")
      (setf *landedp* nil)
      (setf *alivep* t)
      (setf *galaxy* (make-array (list +galaxy-size+ +galaxy-size+)))
      (do ((i 0 (+ i 1)))
          ((>= i +galaxy-size+))
        (do ((j 0 (+ j 1)))
            ((>= j +galaxy-size+))
          (setf (aref *galaxy* i j) (make-quadrant :chartedp nil
                                                   :planet +no-planet+
                                                   :romulans 0
                                                   :klingons 0
                                                   :starbases 0
                                                   :supernovap nil
                                                   :status +secure+))))

      ;; Initialize event structures
      (do ((n 0 (+ n 1)))
          ((>= n +number-of-events+))
        (setf (aref *future-events* n) (make-event :date +forever+ :quadrant nil)))
      ;; Initialize times for extraneous events
      (schedule-event +supernova+ (expran (* 0.5 *initial-time*))) ; C: expran(0.5 * game.intime)
      (schedule-event +tractor-beam+
                      (expran (* 1.5 (/ *initial-time* *remaining-commanders*)))) ; C: expran(1.5 * (game.intime / game.state.remcom))
      (schedule-event +snapshot-for-time-warp+ (+ 1 (random 1))); Force an early snapshot, 1.0 + Rand()
      (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*))) ; C: expran(0.3*game.intime)
      (when (> *remaining-super-commanders* 0)
        (schedule-event +super-commander-move+ 0.2777))
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
            ;; TODO - does this formula allow quadrants to have zero stars? Should be possible, if rarely.
            (setf sector-stars (truncate (+ (* (random 1.0) +max-stars-per-quadrant+) 1))) ; C: Rand()*9.0 + 1.0
            (setf *initial-stars* (+ *initial-stars* sector-stars))
            (setf (quadrant-stars (aref *galaxy* x y)) sector-stars))))

      ;; Put starbases in the galaxy
      ;; TODO - base placement might be broken.
      (setf *base-quadrants* (make-array *initial-bases*))
      ;; Use an improved placement algorithm to spread out the bases.
      ;; For the total number of starbases, for each base to place Pick a random quadrant and test
      ;; it for suitability. Tests are:
      ;;   The first base is always far enough from all others
      ;;   The quadrant doesn't already have a starbase
      ;;   The distance between the candidate quadrant and all other quadrants containing bases
      ;;   is below a calculated threshold. The threshold goes lower as the total number of bases
      ;;   goes higher.
      ;; Also update the starchart, the Federation knows where it's own bases are located.
      ;;
      ;; Note a potential bug: since base quadrants are randomly selected in theory it is possible
      ;; for the algorithm to run forever. That is, every random selection fails the tests. If the
      ;; random number distribution is reasonably even this is less likely to occur but occasional
      ;; delays in placing starbases are possible.
      (do ((i 0 (+ i 1))
           candidate
           (threshold (* 6.0 (- (+ +max-bases+ 1) *initial-bases*))))
          ((>= i *initial-bases*))
        (do ((candidate-ok-p nil))
            (candidate-ok-p)
          (setf candidate (get-random-quadrant))
          (setf candidate-ok-p t) ; Assume ok, then try to falsify the assumption
          (when (> i 0) ; The first base always succeeds
            (if (= (quadrant-starbases (aref *galaxy* (coordinate-x candidate) (coordinate-y candidate))) 0)
                (do ((j (- i 1) (- j 1)))
                    ((or (< j 0)
                         (not candidate-ok-p)))
                  ;; TODO - The reason for the random element isn't clear to me. This might be a poor port of the C code.
                  ;; (when (and (< distance threshold) (<= 0.75 (random 1.0)))
                  (when (< (+ (expt (- (coordinate-x candidate) (coordinate-x (aref *base-quadrants* j))) 2)
                              (expt (- (coordinate-y candidate) (coordinate-y (aref *base-quadrants* j))) 2))
                           threshold)
                    (setf candidate-ok-p nil)))
                (setf candidate-ok-p nil))))
        (setf (aref *base-quadrants* i) candidate)
        (setf (quadrant-starbases (aref *galaxy* (coordinate-x candidate) (coordinate-y candidate))) 1)
        (setf (starchart-page-starbases (aref *starchart* (coordinate-x candidate) (coordinate-y candidate))) 1))

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
            (when (and (not (quadrant-supernovap (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))))
                       (<= (+ (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord)))
                              klump)
                           +max-klingons-per-quadrant+))
              (setf (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord)))
                    (+ (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) klump))
              (setf klingons-placed-p t)))))

      ;; Put Klingon Commanders in the galaxy
      ;; TODO - this code is buggy, occaisonally makes a nil reference
      ;; Select a random location for each commander to be placed based on these filters:
      ;; - If there is a supernova in the quadrant then nothing can be placed there
      ;; - If there are 9 klingons (max klingons per quadrant) there already then don't add another
      ;; - If there are less than 9 klingons then there is a 75% chance of adding the commander
      ;; - If there is already a commander in the quadrant then don't put another one there
      (do ((i 0 (+ i 1))
           c-coord)
          ((>= i *initial-commanders*))
        (setf c-coord (get-random-quadrant))
        (do (candidate-ok-p)
            (candidate-ok-p)
          (setf candidate-ok-p t)
          ;; Use short-circuit evaluation and a nested "and" to decide at what frequency the
          ;; commander will be placed in an empty quadrant
          (when (and (and (< (quadrant-klingons (aref *galaxy* (coordinate-x c-coord) (coordinate-y c-coord)))
                             +max-klingons-per-quadrant+)
                          (< (random 1.0) 0.75))
                     (not (quadrant-supernovap (aref *galaxy* (coordinate-x c-coord) (coordinate-y c-coord))))
                     (< (quadrant-klingons (aref *galaxy* (coordinate-x c-coord) (coordinate-y c-coord)))
                        +max-klingons-per-quadrant+))
            ;; check if a commander is already there
            (do ((j 1 (+ j 1)))
                ((or (>= j i)
                     (not candidate-ok-p)))
              (when (same-coordinate-p (aref *commander-quadrants* j) c-coord)
                (setf candidate-ok-p nil)))))
        (setf (aref *commander-quadrants* i) c-coord)
        (setf (quadrant-klingons (aref *galaxy* (coordinate-x c-coord) (coordinate-y c-coord)))
              (+ (quadrant-klingons (aref *galaxy* (coordinate-x c-coord) (coordinate-y c-coord))) 1)))

      ;; Put planets in the galaxy
      (setf *planet-information* (make-array *initial-planets*))
      (do ((i 0 (+ i 1)))
          ((>= i *initial-planets*))
        (setf (aref *planet-information* i) (make-planet))
        (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
            ((eql (quadrant-planet (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) +no-planet+)
             (setf (quadrant-planet (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) i)
             (setf (planet-quadrant (aref *planet-information* i)) q-coord)))
        (if (< i +habitable-planets+)
            (progn
              (setf (planet-class (aref *planet-information* i)) +class-m+) ; All inhabited planets are class M
              (setf (planet-crystals (aref *planet-information* i)) +absent+)
              (setf (planet-known (aref *planet-information* i)) 1)
              (setf (planet-inhabited (aref *planet-information* i)) i)
              (setf (planet-shuttle-landed-p (aref *planet-information* i)) nil))
            (progn
              (setf (planet-class (aref *planet-information* i)) (+ (random 2) 1)) ; Planet class M, N, or O
              (setf (planet-crystals (aref *planet-information* i)) (* (random 1.0) 1.5)) ; 1 in 3 chance of crystals
              (setf (planet-known (aref *planet-information* i)) 0)
              (setf (planet-inhabited (aref *planet-information* i)) +uninhabited+)
              (setf (planet-shuttle-landed-p (aref *planet-information* i)) nil))))

      ;; Put Romulans in the galaxy
      (do ((i 0 (+ i 1))
           (q-coord (get-random-quadrant) (get-random-quadrant)))
          ((>= i *remaining-romulans*)
           (setf (quadrant-romulans (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord)))
                 (+ (quadrant-romulans (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) 1))))

      ;; Put the Super Commander in the galaxy
      (when (> *remaining-super-commanders* 0)
        (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
            ((and (not (quadrant-supernovap (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))))
                  (<= (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) 8))
             (setf *super-commander-quadrant* q-coord)
             (setf (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord)))
                   (+ (quadrant-klingons (aref *galaxy* (coordinate-x q-coord) (coordinate-y q-coord))) 1)))))

      ;; Put the thing in the galaxy unless this is a tournament game.
      (setf *thing-location* nil)
      (when (string= game-type "regular")
        (setf *thing-location* (get-random-quadrant)))

      (setf *snapshot-taken-p* nil)

      ;; Introduce the player to the current situation.
      (skip-line 2)
      (if (= (skill-level-value *skill-level*) +novice+)
          (progn
            (print-message (format nil "It is stardate ~A. The Federation is being attacked by"
                                   (format-stardate *stardate*)))
            (print-message "a deadly Klingon invasion force. As captain of the United")
            (print-message "Starship U.S.S. Enterprise, it is your mission to seek out")
            (print-message (format nil "and destroy this invasion force of ~A battle cruisers."
                                   (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
            (print-message (format nil "You have an initial allotment of ~A stardates to complete"
                                   (truncate *initial-time*)))
            (print-message "your mission. As you proceed you may be given more time.")
            (skip-line)
            (print-message (format nil "You will have ~A supporting starbases." *initial-bases*))
            (print-out "Starbase locations-  "))
          (progn
            (print-message (format nil "Stardate ~A." (truncate *stardate*)))
            (skip-line)
            (print-message (format nil "~A klingons."
                                   (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
            (print-message "An unknown number of Romulans.")
            (when (> *remaining-super-commanders* 0)
              (print-message "And one (GULP) Super-Commander."))
            (print-message (format nil "~A stardates." (truncate *initial-time*)))
            (print-out (format nil "~A starbases in " *initial-bases*))))
      (do ((i 0 (+ i 1)))
          ((>= i *initial-bases*))
        (print-out (format nil "~A  " (format-coordinates (aref *base-quadrants* i)))))
      (skip-line 2)
      (print-message (format nil "The Enterprise is currently in ~A ~A"
                             (format-quadrant-coordinates *current-quadrant*)
                             (format-sector-coordinates *current-sector*)))
      (skip-line)
      (print-out "Good Luck!")
      (when (> *remaining-super-commanders* 0)
        (print-out " YOU'LL NEED IT."))
      (skip-line 2)
      ;; TODO - Is there a need to wait for the player e.g. press any key to continue?
      (new-quadrant :show-thing nil)
      (when (> (- *enemies-here* *things-here* *tholians-here*) 0)
        (setf *shields-are-up-p* t))
      (when *romulan-neutral-zone-p*
        (attack-player)))))

(defun print-commands (commands) ; C: listCommands(void)
  "Print a list of valid commands."

  ;; TODO - sort the list of commands into alphabetical order?
  (skip-line)
  (print-message "VALID COMMANDS ARE:") ; Current window is set to message window.
  (do ((c (pop commands) (pop commands))
       (counter 1 (+ counter 1)))
      ((not commands))
    (print-out (format nil "~12@A" (string-upcase c))) ; Field width is large enough for the longest
                                        ; command and some padding, hard-coded instead of calculated.
    (when (> counter 4) ; Keep number of commands per line low to support 80 column terminals.
      (skip-line)
      (setf counter 0)))
  (skip-line))

(defun helpme () ; C: helpme(void)
  "Browse on-line help."

  ;; TODO - write this
  ;; some ideas:
  ;; 1. incorporate the original SST.DOC into the help system.
  ;; 2. create an sst-help package to keep it in a separate file
  ;; 3. all help text embedded in the code, don't want to deal with file i/o to get info
  ;; 4. provide function(s) to write the help text to an external file
  ;; 5. Include author credits
  ;; 6. Include author changes
  )

(defun make-moves ()
  "Command interpretation loop. This is the heart of the game: read commands and carry them out.
The loop ends when the player wins by killing all Klingons, decides to quit, or is killed."

  ;; Commands that should match the shortest possible string come first in the list.
  ;; TODO? - put commands that are not direct gameplay commands into a sub-mode, like the
  ;; <esc> or <tilde> menus in some graphical games. The idea is to not "break the 4th wall"
  ;; during gameplay. These commands are commands, emexit, freeze, quit, save, debug, help
  (do ((*all-done-p* nil)
       (commands (list "abandon" "chart" "commands" "computer" "crystals" "dock" "damages" "deathray"
                       "destruct" "emexit" "freeze" "help" "impulse" "lrscan" "move" "mayday"
                       "mine" "orbit" "phasers" "photons" "planets" "probe" "quit" "rest" "report"
                       "request" "srscan" "status" "save" "score" "sensors" "shields" "shuttle" "sos"
                       "transport" "torpedoes" "visual" "warp" "debug"))
       command
       hit-me-p) ; When true, player has taken an action which consumes game time or a turn,
                                        ; after which enemies take their action turn, usually
                                        ; an attack.
      ((or *all-done-p*))
    ;; In curses mode sensors work automatically. Call before updating the display.
    (when *curses-interface-p*
      (sensor)
      (draw-maps))
    (setf hit-me-p nil)
    (setf *just-in-p* nil)
    (setf *time-taken-by-current-operation* 0.0)
    (setf *action-taken-p* nil)
    (skip-line)
    (print-prompt "COMMAND: ")
    (clear-type-ahead-buffer)
    (scan-input)
    (setf command (match-token *input-item* commands)) ; TODO - fix match-token to ignore all previous input on -1
    ;; TODO - check that commands that must be typed in full were: abandon destruct quit save deathray
    (cond ((string= command "abandon")
           (abandon-ship))
          ((string= command "chart")
           (chart))
          ((string= command "commands")
           (print-commands commands))
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
          ((string= command "emexit") ; Emergency exit
           (clear-screen)             ; Hide the screen
           (save-game)                ; Save the game under a default name
           (sb-ext:exit))             ; A quick exit to the command line
          ((string= command "help") ; Get help and information
           (helpme))
          ((string= command "impulse")
           (move-under-impulse-power))
          ((string= command "lrscan")
           (long-range-scan))
          ((or (string= command "mayday") ; Call for help
               (string= command "sos"))
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
             (setf hit-me-p t)))
          ((string= command "planets")
           (survey))
          ((string= command "probe")
           (launch-probe)
           (when *action-taken-p*
             (setf hit-me-p t)))
          ((string= command "quit") ; Quit the game
           (setf *all-done-p* t))
          ((string= command"report" )
           (report))
          ((string= command "request")
           (request))
          ((string= command "rest")
           (wait)
           (when *action-taken-p*
             (setf hit-me-p t)))
          ((or (string= command "save")
               (string= command "freeze"))
           (save-game)
           (clear-screen)
           ;; TODO - continuous saving makes this senseless
           (when (> (skill-level-value *skill-level*) +good+)
             (print-message "WARNING--Saved games produce no plaques!")))
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
          ((or (string= command "torpedoes")
               (string= command "photons"))
           (fire-photon-torpedoes)
           (when *action-taken-p*
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
           (print-commands commands)))

    ;; Give the game a turn after the player completes theirs: invoke scheduled events and give
    ;; enemies their turn to attack.
    ;; There can be a chain of reactions, for example an attacking enemy causes a star to go
    ;; supernova, which causes an emergency exit of the quadrant, and the quadrant escaped to also
    ;; has a supernova, and when the second emergency exit is in progress a scheduled tractor beam
    ;; event occurs.
    (do ((turn-ended-p nil))
        ((or *all-done-p* turn-ended-p)) ; Game has ended or the turn is over.
      (when (/= *time-taken-by-current-operation* 0.0)
        (process-events))
      (unless *all-done-p* ; Scheduled events may have resulted in the game ending.
        (cond ((quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))
               (emergency-supernova-exit)) ; Supernova event scheduled or player landed in a supernova quadrant
              ((and hit-me-p (not *just-in-p*))
               (attack-player)
               (unless *all-done-p*
                 (if (quadrant-supernovap (aref *galaxy* (coordinate-x *current-quadrant*) (coordinate-y *current-quadrant*)))
                     (emergency-supernova-exit) ; Attacker caused a supernova
                     (setf turn-ended-p t))))
              (t
               (setf turn-ended-p t)))))))

(defun print-banner () ; C: prelim()
  "Print an introductory banner."

  (clear-message-window)
  (print-message "-SUPER- STAR TREK")
  (skip-line))

;;; TODO - The help file is left as an external file for now, for CL practice. In future, bring the help text
;;;        Into the code and add an option to write an external file. This will support external documentation
;;;        such as web or man pages.

;; Changes to game options are made via the debug command; command line option parsing isn't
;; implemented yet.
;; Option to save a completed game has been removed. Final score is not available after game exit.
;; TODO - add an option to write the final score to a file, or record it some other way. If the whole
;;        game is stored as a way of saving the final score then also add an option to continue
;;        playing. There won't be much to do but there is no reason to disallow it.
;; TODO - record and play back games. This will also allow viewing the final score again.
;; TODO - test without curses

(defun sst () ; C: main()
  "Super Star Trek starting function."

  (initialize) ; C: iostart()
  (clear-screen)
  (print-banner)

  ;; Play a game.
  (do ((play-again t (get-y-or-n)))
      ((not play-again))

    (set-up-new-game)
    (make-moves)

    (skip-line)
    (print-stars)
    (skip-line)
    (print-prompt "Do you want to play again? "))

  (clean-up)
  (format t "~%May the Great Bird of the Galaxy roost upon your home planet.~%")
  (sb-ext:exit))
