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
;;;; 5. Black holes can warp you to another location in the galaxy
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

;; TODO - delete these lines. But, write a shell script or something to make sure quicklisp has the
;;        latest versions of packages? Add to sst.asd?
;;(ql:quickload :cl-charms) ; ncurses support
;;(ql:quickload :cl-utilities)

(defpackage sbcl-support
  (:documentation "Support for SBCL-specific features.")
  (:use common-lisp common-lisp-user)
  (:export define-constant))

(defpackage sst-events
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

(defpackage sst-help
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
                *lines* *cols* *stdscr* true false err ok
                wborder box ; TODO not sure if this is only debug or permanent
                ;; line/box drawing characters
                acs_hline acs_vline acs_llcorner acs_lrcorner acs_ulcorner acs_urcorner
                color-pair cbreak curs-set endwin getcury getmaxy getmaxyx getyx has-colors
                init-pair initscr keypad mvwaddstr newwin nocbreak nonl scrollok start-color
                waddch waddstr wattron wattrset wclear wclrtoeol wgetch wmove wprintw wrefresh)
  (:import-from cl-utilities
                split-sequence)
  (:import-from sst-events
                +forever+
                *future-events*
                initialize-events
                scheduled-for
                is-scheduled-p
                postpone-event
                unschedule
                find-event)
  (:import-from sst-help
                *help-database*)
  (:export print-prompt
           print-message
           print-out)
  (:nicknames sst)
  (:export sst))
