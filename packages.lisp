;;;; Super Star Trek package definitions

;;;; Possible enhancementsl
;;;; 1. Offer a randomized password instead of requiring user input. The C source for this port
;;;;    required a password if a "plain" game was selected and generated a random password if a
;;;;    "fancy" game was selected.
;;;; 2. BSD trek has other features that might be interesting
;;;; 3. The star chart could display more information, e.g. planets. Some ways to do that: add a
;;;;    digit, display 2 rows of digits per quadrant. 1 row: natural features stars, planets,
;;;;    supernova, 2nd row enemies, starbases, inhabited planets? Would like to preserve
;;;;    compatibility with glass terminals.
;;;; 3. Star chart could show quadrants having a planet under attack
;;;; 3. Star chart could show quadrants having a base under attack
;;;; 3. Star chart could show qaudrants in color, ship quadrant in green, supernova quadrants in
;;;;    red, quadrants under some kind of attack in yellow, quadrants containing a planet with
;;;;    dilithim crystals in blue. Same colors as SR scan but with different meanings
;;;; 4. The todo list from the sst2k github repository (grabbed in June 2021):
;;;;    Short-term items:
;;;;    * Add the attacked inhabited planets to the status report
;;;;    Long-term items:
;;;;    * Wrap a GUI around it.
;;;;    * Wandering planet-killer as in "The Doomsday Machine";
;;;;      see <http://www.ericweisstein.com/fun/startrek/TheDoomsdayMachine.html>.
;;;;    BSD-Trek features we haven't swiped yet:
;;;;    * Smarter computer, with multiple requests.
;;;;    * There is a small probility that a nova event will leave a black hole.
;;;;    * Multiple laser banks and beam spreading.
;;;; 5. Black holes can warp you to another location in the galaxy. This should have low probability.
;;;; 7. Add an undock command

;;;; TODO - add quadrants to events - every event occurs in a quadrant
;;;; TODO - bsdtrek mentioned boarding parties to capture a klingon, klingons could try it too
;;;; TODO - cloaking device. Capture/board a Romulan to acquire one?
;;;; TODO - can the player have a tractor beam too? Long-range variant? Capture a long-range variant?

;;;; Comments reference names in the C source with "C:"
;;;; TODO remove the original C names from the C source if/when they are no longer needed

;;;; TODO - Does the transporter work while cloaked? Can the ship be cloaked while in orbit?
;;;; TODO - Can the capture mechanic be enhanced by carefully damaging opponents? "Thrash them to
;;;;        within an inch of their lives", that is use enough phaser energy to weaken them to
;;;;        the point of not being able to fight back.
;;;; TODO - are enemies that run away limited by their current energy value? That is, do they use
;;;;        energy to move? If yes, then getting hit hard enough could prevent them from running
;;;;        away to another quadrant and make them easier to capture. This could also be added to
;;;;        the enemy AI - run away if there is just enough energy to do so, because enemies
;;;;        automatically regain full power when not in the player quadrant.
;;;; TODO - Do bases have long-range sensors? Why wouldn't they? If yes, then surely they would
;;;;        share their information with the ship, assuming the subspace radio is working. This
;;;;        would change game play a lot - less need to run around the galaxy finding klingons.
;;;; TODO - However, per various websites, the subspace radio is not instantaneous. For play
;;;;        purposes some information arrives instantly, but additional information from starbase
;;;;        could arrive over the course of time. Use an enhanced event system to do this?

;; TODO - delete these lines. But, write a shell script or something to make sure quicklisp has the
;;        latest versions of packages? Add to sst.asd?
;;(ql:quickload :cl-charms) ; ncurses support
;;(ql:quickload :cl-utilities)

(defpackage sbcl-support
  (:documentation "Support for SBCL-specific features.")
  (:use common-lisp common-lisp-user)
  (:export define-constant))

(defpackage sst-coordinates
  (:documentation "Coordinate definitions for Super Star Trek")
  (:use common-lisp common-lisp-user)
  (:import-from sbcl-support
                define-constant)
  (:export +galaxy-size+
           +quadrant-size+
           coordinate
           coordinate-x
           coordinate-y
           sector-coordinate
           sector-coordinate-p
           make-sector-coordinate
           quadrant-coordinate
           make-quadrant-coordinate
           valid-p
           coord-ref
           valid-quadrant-p
           valid-sector-p
           coord-equal
           distance
           format-coordinates
           format-sector-coordinates
           format-quadrant-coordinates
           galaxy-sector-to-quadrant
           galaxy-sector-to-local-sector))

(defpackage sst-terminal-io
  (:documentation "Read and write from terminals.")
  (:use common-lisp common-lisp-user)
  (:import-from cl-utilities
                split-sequence)
  (:import-from sbcl-support
                define-constant)
  (:import-from cl-charms/low-level
                a_bold a_reverse color_black color_blue color_cyan color_green color_magenta
                color_red color_white color_yellow
                true false err ok *lines* *cols* *stdscr*
                cbreak color-pair curs-set endwin getcury getmaxy getmaxyx getyx has-colors
                init-pair initscr keypad mvwaddstr newwin nocbreak nonl scrollok start-color
                wattron wattrset wclear wgetch wmove wprintw wrefresh)
  (:export +default-color+
           +green+
           +cyan+
           +red+
           +brown+
           +yellow+
           screen
           window
           curses-window
           *message-window*
           *short-range-scan-window*
           *ship-status-window*
           *game-status-window*
           *long-range-scan-window*
           *prompt-window*
           *starchart-window*
           *damage-report-window*
           *planet-report-window*
           *score-window*
           initialize-windows
           clean-up-windows
           toggle-windows
           clear-window
           get-input-line
           scan-input
           unscan-input
           input-available-p
           two-input-items-available
           number-of-input-items
           format-input-items
           clear-type-ahead-buffer
           match-token
           print-out
           skip-line
           print-message
           print-prompt
           page-window
           restart-paging
           set-text-color
           toggle-reverse-video
           track-torpedo
           put-short-range-scan-symbol
           boom
           turn-sound-on
           turn-sound-off
           warble))

(defpackage sst-events
  (:documentation "Event handling for Super Star Trek")
  (:use common-lisp common-lisp-user)
  (:import-from sbcl-support
                define-constant)
  (:export *future-events*
           initialize-events
           scheduled-for
           is-scheduled-p
           postpone-event
           unschedule-event
           find-event))

(defpackage sst-help
  (:documentation "Help information for Super Star Trek")
  (:use common-lisp common-lisp-user)
  (:export display-online-help)
  (:import-from cl-utilities
                split-sequence)
  (:import-from sst-terminal-io
                *message-window*
                skip-line
                restart-paging
                clear-type-ahead-buffer
                input-available-p
                match-token
                print-message
                print-prompt
                scan-input))

(defpackage super-star-trek
  (:documentation "Super Star Trek ported to Common Lisp.")
  (:use common-lisp common-lisp-user)
  (:import-from sbcl-support
                define-constant)
  (:import-from cl-utilities
                split-sequence)
  (:import-from sst-coordinates
                +galaxy-size+
                +quadrant-size+
                coordinate
                coordinate-x
                coordinate-y
                sector-coordinate
                sector-coordinate-p
                make-sector-coordinate
                quadrant-coordinate
                make-quadrant-coordinate
                valid-p
                coord-ref
                valid-quadrant-p
                valid-sector-p
                coord-equal
                distance
                format-coordinates
                format-sector-coordinates
                format-quadrant-coordinates
                galaxy-sector-to-quadrant
                galaxy-sector-to-local-sector)
  (:import-from sst-events
                *future-events*
                initialize-events
                scheduled-for
                is-scheduled-p
                postpone-event
                unschedule-event
                find-event)
  (:import-from sst-help
                display-online-help)
  (:import-from sst-terminal-io
                +default-color+
                +green+
                +cyan+
                +red+
                +brown+
                +yellow+
                screen
                window
                curses-window
                *message-window*
                *short-range-scan-window*
                *ship-status-window*
                *game-status-window*
                *long-range-scan-window*
                *prompt-window*
                *starchart-window*
                *damage-report-window*
                *planet-report-window*
                *score-window*
                initialize-windows
                clean-up-windows
                toggle-windows
                clear-window
                get-input-line
                scan-input
                unscan-input
                input-available-p
                two-input-items-available
                number-of-input-items
                format-input-items
                clear-type-ahead-buffer
                match-token
                print-out
                skip-line
                print-message
                print-prompt
                page-window
                restart-paging
                set-text-color
                toggle-reverse-video
                track-torpedo
                put-short-range-scan-symbol
                boom
                turn-sound-on
                turn-sound-off
                warble)
  (:nicknames sst)
  (:export sst))
