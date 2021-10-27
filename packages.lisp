;;;; Super Star Trek package definitions

;;;; Possible enhancements
;;;; 1. Offer a randomized password instead of requiring user input. The C source for this port
;;;;    required a password if a "plain" game was selected and generated a random password if a
;;;;    "fancy" game was selected.
;;;; 2. BSD trek has other features that might be interesting
;;;; 3. The star chart could display more information, e.g. planets. Some ways to do that: add a
;;;;    digit, display 2 rows of digits per quadrant. 1 row: natural features stars, planets,
;;;;    supernova, 2nd row enemies, starbases, inhabited planets? Would like to preserve
;;;;    compatibility with glass terminals.
;;;; 4. The todo list from the sst2k github repository (grabbed in June 2021):
;;;;    Short-term items:
;;;;    * Adjust the sst.spec wrt the build system changes
;;;;    * Complete, polish, improve, complete, polish, improve the
;;;;      gettextization and the translation! Right now it just sucks.
;;;;    * Add the attacked inhabited planets to the status report
;;;;    * Status display should update after kills so Klingons Left is correct.
;;;;    Long-term items:
;;;;    * Wrap a GUI around it.
;;;;    * Wandering planet-killer as in "The Doomsday Machine";
;;;;      see <http://www.ericweisstein.com/fun/startrek/TheDoomsdayMachine.html>.
;;;;    BSD-Trek features we haven't swiped yet:
;;;;    * The cloaking device.
;;;;    * Summoning Klingons to surrender and taking captives.
;;;;    * Smarter computer, with multiple requests.
;;;;    * There is a small probility that a nova event will leave a black hole.
;;;;    * Multiple laser banks and beam spreading.
;;;; 5. Black holes can warp you to another location in the galaxy
;;;; 5. S-lang instead of ncurses?

;;;; TODO - add quadrants to events - every event occurs in a quadrant
;;;; TODO - bsdtrek mentioned boarding parties to capture a klingon, klingons could try it too
;;;; TODO - cloaking device. Capture/board a Romulan to acquire one?
;;;; TODO - can the player have a tractor beam too? Long-range variant? Capture a long-range variant?

;;;; Comments reference names in the C source with "C:"
;;;; TODO remove the original C names from the C source if/when they are no longer needed

;;;; TODO - Does the transporter work while cloaked? Can the ship be cloaked while in orbit?
;;;; TODO - Can the capture mechanic be enhanced by carefully damaging opponents? "Thrash them to
;;;;        within an inch of their lives", that is use enough phaser energy to weaken them to the point
;;;;        of not being able to fight back.
;;;; TODO - are enemies that run away limited by their current energy value? That is, do they use energy
;;;;        to move? If yes, then getting hit hard enough could prevent them from running away to another
;;;;        quadrant and make them easier to capture. This could also be added to the enemy AI - run away
;;;;        if there is just enough energy to do so, because enemies automatically regain full power when
;;;;        not in the current quadrant.

;; TODO - delete these lines. But, write a shell script or something to make sure quicklisp has the
;;        latest versions of packages? Add to sst.asd?
;;(ql:quickload :cl-charms) ; ncurses support
;;(ql:quickload :cl-utilities)

(defpackage sbcl-support
  (:documentation "Support for SBCL-specific features.")
  (:use common-lisp common-lisp-user)
  (:export define-constant))

(defpackage events
  (:documentation "Event handling for Super Star Trek")
  (:use common-lisp common-lisp-user)
  (:import-from sbcl-support
                define-constant)
  (:export +forever+
           *future-events*
           initialize-events
           scheduled-for
           is-scheduled-p
           postpone-event
           unschedule
           find-event))

(defpackage help
  (:documentation "Help information for Super Star Trek")
  (:use common-lisp common-lisp-user)
  (:export *help-database*))

(defpackage super-star-trek
  (:documentation "Super Star Trek ported to Common Lisp.")
  (:use common-lisp common-lisp-user)
  (:import-from sbcl-support
                define-constant)
  (:import-from cl-charms/low-level
                ;; Having difficulty getting getstr, getnstr, wgetstr, wgetnstr to work on FreeBSD
                a_bold a_reverse color_black color_blue color_cyan color_green color_magenta
                color_red color_white color_yellow key_eol key_backspace
                *lines* *stdscr* true false err ok
                box ; TODO not sure if this is only debug or permanent
                color-pair cbreak curs-set endwin getcury getmaxy getmaxyx getyx has-colors
                init-pair initscr keypad mvwaddstr newwin nocbreak nonl scrollok start-color
                waddch waddstr wattron wattrset wclear wclrtoeol wgetch wmove wprintw wrefresh)
  (:import-from cl-utilities
                split-sequence)
  (:import-from events
                +forever+
                *future-events*
                initialize-events
                scheduled-for
                is-scheduled-p
                postpone-event
                unschedule
                find-event)
  (:import-from help
                *help-database*)
  (:export print-prompt
           print-message
           print-out)
  (:nicknames sst)
  (:export sst))
