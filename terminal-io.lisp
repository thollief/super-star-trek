;;;; Super Star Trek terminal I/O

(in-package sst-terminal-io)

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

(defvar *message-window* nil
  "Window into which game narrative is printed. This window is the default
output location if no function-specific window is available.")
(defvar *short-range-scan-window* nil) ; C: srscan_window
(defvar *ship-status-window* nil)
(defvar *game-status-window* nil)
(defvar *long-range-scan-window* nil) ; C: lrscan_window
(defvar *prompt-window* nil)
(defvar *starchart-window* nil)
(defvar *damage-report-window* nil)
(defvar *planet-report-window* nil)
(defvar *score-window* nil)

(defvar *line-tokens* nil "List of input tokens")

(defun clean-up-windows ()
  "Window cleanup function to run at program exit."

  (ignore-errors ; sloppy - we might not be using curses, but endwin needs to be called if we are
    (endwin))

  ;; Remove this cleanup function from the exit hooks
  (delete '(clean-up-windows) sb-ext:*exit-hooks*))

(defun initialize-windows ()
  "Set up the window variables for the current terminal, with or without curses as appropriate."

  ;; Register a curses cleanup function to run on program exit.
  (setf sb-ext:*exit-hooks* (append sb-ext:*exit-hooks* '(clean-up-windows)))

  (setf *print-right-margin* 80) ; should be good for both windowed and line-by-line

  (let ((use-curses-p nil))
    ;; Win32 environments get line-by-line mode, pdcurses has too many issues
    (if (string= (software-type) "Win32")
        (setf *message-window* (make-instance 'screen :height 24 :width 80 :pagingp t))
        (progn
	  (setf use-curses-p t)
          (initscr)
          ;; Ensure terminal screen is large enough for windows, otherwise use line-by-line mode.
          (unless (and (>= *lines* 24)
                       (>= *cols* 80))
            (setf *message-window* (make-instance 'screen :height *lines* :width *cols*
                                                          :pagingp t))
	    (endwin)
            (setf use-curses-p nil))))

    ;; DEBUG - force line-by-line mode so debugger output is readable
    ;;(when use-curses-p
    ;;  (setf *message-window* (make-instance 'screen :height *lines* :width *cols* :pagingp t))
    ;;  (endwin)
    ;;  (setf use-curses-p nil))

    (if use-curses-p
        (progn
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
          (wclear *stdscr*)
          (wmove *stdscr* 0 0)
          (wrefresh *stdscr*)
          ;; After this point use curses window functions, not *stdscr* functions.

          ;; In curses/window mode these windows are always created because they fit the minimum
          ;; space required for windowed mode: short range scan, status, long range scan, game
          ;; status, message, prompt. If more rows/columns are available then more windows are
          ;; created. All output that doesn't go to a specific window is displayed in the message
          ;;window.
          ;;
          ;; TODO - name all these constants that define window position and size?
          (setf *short-range-scan-window*
                (make-instance 'window :height 12 :width 24 :curses-window (newwin 12 24 0 0)))
          (setf *ship-status-window*
                (make-instance 'window :height 10 :width 36 :curses-window (newwin 10 36 2 24)))
          (setf *long-range-scan-window*
                (make-instance 'window :height 5 :width 19 :curses-window (newwin 5 19 0 60)))
          (setf *game-status-window*
                (make-instance 'window :height 3 :width 19 :curses-window (newwin 3 19 8 60)))
          (setf *prompt-window*
                (make-instance 'window :height 1 :width *cols*
                                       :curses-window (newwin 1 *cols* (- *lines* 1) 0)))
          ;; The message window is allocated all space between the short range scan window and the
          ;; prompt window.
          (setf *message-window*
                (make-instance 'window :height (- *lines* 1 11) :width 80
                                       :curses-window (newwin(- *lines* 1 11)  80 12 0)
                                       :pagingp t))
          (scrollok (curses-window *message-window*) true)
          (if (>= *cols* 125)
              (setf *starchart-window*
                    (make-instance 'window :height 10 :width 45
                                           :curses-window (newwin 10 45 0 81)))
              (setf *starchart-window* *message-window*))
          (if (and (>= *cols* 119)
                   (>= *lines* 25))
              (setf *damage-report-window*
                    (make-instance 'window :height 15 :width 38
                                           :curses-window (newwin 15 38 11 81)))
              (setf *damage-report-window* *message-window*))
          (if (and (>= *cols* 185)
                   (>= *lines* 44))
              (setf *planet-report-window*
                    (make-instance 'window :height 44 :width 59
                                           :curses-window (newwin 44 59 0 126)))
              (setf *planet-report-window* *message-window*))
          (if (and (>= *cols* 187)
                   (>= *lines* 65))
              (setf *score-window*
                    (make-instance 'window :height 20 :width 61
                                           :curses-window (newwin 20 61 45 126)))
              (setf *score-window* *message-window*)))
        (progn
          ;; Line by line mode - all output goes to the *message-windw*
          (setf *short-range-scan-window* *message-window*)
          (setf *ship-status-window* *message-window*)
          (setf *long-range-scan-window* *message-window*)
          (setf *game-status-window* *message-window*)
          (setf *prompt-window* *message-window*)
          (setf *starchart-window* *message-window*)
          (setf *damage-report-window* *message-window*)
          (setf *planet-report-window* *message-window*)
          (setf *score-window* *message-window*)))))

(defclass screen ()
  ((height
    :initarg :height
    :initform nil
    :accessor height)
   (width
    :initarg :width
    :initform nil
    :accessor width)
   (output-line-count
    :initarg :output-line-count
    :initform 0
    :accessor output-line-count
    :documentation "The number of lines that have been output since the last time paging was
 restarted.")
   (pagingp
    :initarg :pagingp
    :initform nil
    :accessor pagingp
    :documentation "When true, page output to the window. That is, count the number of lines that
have been output, pause when the window is full, and prompt the user to press Enter to continue."))
  (:documentation
  "The screen area in which game content is displayed. The screen is divided into cells, each
of which can display one character. Cells are addressed with X and Y coordinates with the top left
cell desginated 0, 0. X coordinates increase from top to bottom, Y coordinates increase from left
to right. This arrangement corresponds to that typically used by serial terminals, virtual terminal
emulators, etc. Output is done using standard Lisp functions."))

(defclass window (screen)
  ((curses-window
    :initarg :curses-window
    :initform nil
    :accessor curses-window
    :documentation "A pointer to a window defined by the curses newwin function."))
  (:documentation
  "A rectangular display area on the physical screen. Output is done using curses functions."))

(defgeneric clear-window (output-window)
  (:documentation "Erase all text in the output window."))

(defmethod clear-window ((output-window screen))
  "Use a safe, ugly, brute-force method to clear the screen."

  (let ((p (pagingp output-window)))
    (setf (pagingp output-window) nil)
    (skip-line output-window (height output-window))
    (setf (pagingp output-window) p)
    (restart-paging output-window)))

(defmethod clear-window ((output-window window))
  "Clear the window using curses functions."

  (wclear (curses-window output-window))
  (wmove (curses-window output-window) 0 0)
  (wrefresh (curses-window output-window)))

;; TODO - should the curs-set function be called elsewhere, and if so where?
;;        the prompt window can show a cursor, and maybe the message window
;;        (curs-set 1)
;;        (curs-set 0))))

;; TODO - C source also stores the input line in the replay file.
;; TODO - input of special keys, for example arrow keys, echo as multiple symbols. Need to
;;        backspace and write over those, too.
(defgeneric get-input-line (input-window)
  (:documentation "Read input from the keyboard. The type of input window determines which
keyboard input functions will be used, either standard Lisp or curses."))

(defmethod get-input-line ((input-window screen))
  "Read input from the keyboard using Lisp functions,"

  ;; input length isn't limited here, use read-char if needed
  (read-line)) ; C: fgets(line, max, stdin)

(defmethod get-input-line ((input-window window))
  "Read input from the keyboard using curses functions,"

  (let (line)
    ;; wgetstr and related ncurses functions from charms/ll don't seem to work. Simulate
    ;; with wgetch.
    (do ((input-char 0 (wgetch (curses-window *prompt-window*))))
        ((= input-char 13)
         line) ; return value
      ;; wgetch returns numeric character codes, accept the usual printable ASCII characters
      (when (and (>= input-char 32)
                 (<= input-char 126))
        (setf line (concatenate 'string line (string (code-char input-char))))
        ;; escape backslashes with a backslash because (read) is used to parse numbers
        (when (= input-char 92)
          (setf line (concatenate 'string line "\\"))))
      (when (= input-char 127) ; Handle backspace so errors can be corrected
        (let (y x)
          (getyx (curses-window *prompt-window*) y x)
          (decf x 2)
          ;; DEL is a ctrl char so "^j" is echoed, need to overwrite 2 chars
          (mvwaddstr (curses-window *prompt-window*) y x "  ")
          (wmove (curses-window *prompt-window*) y x)
          (when (> (length line) 0)
            (decf x 1)
            (mvwaddstr (curses-window *prompt-window*) y x " ")
            (wmove (curses-window *prompt-window*) y x)
            (setf line (subseq line 0 (- (length line) 1))))))
      (wrefresh (curses-window *prompt-window*)))))

(defun scan-input ()
  "Manage a list of input tokens, returning one token each time this function is called. If the
list is empty then get input from the keyboard, remove leading and trailing spaces, downcase
everything, and then split it on spaces to generate tokens. Return the first token and save the
remainder for the next call, in effect providing command type-ahead. If the keyboard input didn't
 include any non-space characters then return nil."

  (unless (input-available-p)
    (setf *line-tokens*
          (split-sequence #\SPACE
                          (string-downcase (string-trim " " (get-input-line *prompt-window*)))
                          :remove-empty-subseqs t)))
  (let ((test-number nil)
        input-item
        temp-read-eval) ; used to limit malicious actions, per the Lisp FAQ section 3-11
    (setf input-item (pop *line-tokens*))
    ;; input-item could be nil if player just presses the enter key
    (when input-item
      (setf temp-read-eval *read-eval*)
      (setf *read-eval* nil)
      ;; Read something, possibly a number. This is reading a Lisp form and if it's malformed then
      ;; we don't want it anyway so ignore the error.
      (setf test-number (ignore-errors (read-from-string input-item)))
      (setf *read-eval* temp-read-eval)
      (when (numberp test-number) ; If it's a number then keep it, as a number
        (setf input-item test-number)))
    (return-from scan-input input-item)))

(defun unscan-input (token)
  "Push the parameter on to the list of line tokens."

  ;; *line-tokens* must contains strings, token could be a number or something.
  (push (format nil "~A" token) *line-tokens*))

(defun input-available-p ()
  "Return true when there are elements in the *line-tokens* list, false otherwise."

  (> (length *line-tokens*) 0))

(defun two-input-items-available ()
  "Return true when there are at least two input tokens waiting to be processed. The specific
number 'two' is used because some commands expect a pair of coordinate numbers. If there are
two input items available then they could be a coordinate pair, which will be determined by
reading and evaluating the input items."

  (>= (length *line-tokens*) 2))

(defun number-of-input-items ()
  "Return the number of tokens in the *line-tokens* list. This can be used by commands that
requie a specific number of tokens to be syntactically correct."

  (length *line-tokens*))

(defun format-input-items ()
  "Convert the contents of the input buffer to a string."

  (format nil "~{ ~A~}" *line-tokens*))

(defun clear-type-ahead-buffer () ; C: chew()
  "Convenience function to delete any commands the player may have entered in anticipation of
future prompts. This is normally called after an input error or other condition where the future
prompts change."

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

(defgeneric print-out (output-window string-to-print &key print-slowly)
  (:documentation "Print a string in the specified window. If print-slowly is true then print with
 a delay between each character to build dramatic tension. Slow printing was not optional on most
 paper-based terminals so slow printing helps recreate some of the gameplay experience of prior
 versions of Super Star Trek."))

(defmethod print-out ((output-window window) string-to-print &key (print-slowly nil))
  "Print a string in the specified window using curses output functions."

  (map nil #'(lambda (c)
               (when print-slowly
                 (sleep 0.030))
               ;; The curses wprintw function uses % to indicate format control characters for
               ;; C printf, so double them to print literal % symbols
               (let ((string-to-print (string c)))
                 (when (string= string-to-print "%")
                   (setf string-to-print "%%"))
                 (wprintw (curses-window output-window) string-to-print)
                 (wrefresh (curses-window output-window))))
       string-to-print)
  (when print-slowly
    (sleep 0.300)))

(defmethod print-out ((output-window screen) string-to-print &key (print-slowly nil))
  "Print a string using Lisp print functions."

  (map nil #'(lambda (c)
               (when print-slowly
                 (sleep 0.030))
               (princ c)
               (finish-output))
       string-to-print)
  (when print-slowly
    (sleep 0.300)))

(defgeneric skip-line (output-window &optional lines-to-skip)
  (:documentation "A convenience function to add blank lines at the current cursor position of the
 screen or current window, normally the last line. Since this is just a newline it can also end
 strings that did not print their own newline."))

(defmethod skip-line ((output-window screen) &optional (lines-to-skip 1))

  (dotimes (x lines-to-skip)
    (print-out output-window (format nil "~%"))
    (finish-output)
    (page-window output-window)))

(defmethod skip-line ((output-window window) &optional (lines-to-skip 1))

  (dotimes (x lines-to-skip)
    (wprintw (curses-window output-window) (string #\newline))
    (wrefresh (curses-window output-window))
    (page-window output-window)))

;; TODO - rename this to something like print-paged; more descriptive
(defgeneric print-message (output-window message-to-print &key print-slowly)
  (:documentation "Print a string in the specified window. Page the window after each newline."))

(defmethod print-message ((output-window screen) message-to-print &key (print-slowly nil))

  (let (message-line
        (first-newline-position (position #\newline message-to-print)))
    (if first-newline-position
        (progn
          (setf first-newline-position (1+ first-newline-position))
          (setf message-line (subseq message-to-print 0 first-newline-position)))
        (setf message-line message-to-print))
    (print-out output-window message-line :print-slowly print-slowly)
    (when first-newline-position
      (page-window output-window)
      (when (> (length message-to-print) first-newline-position)
        (print-message output-window (subseq message-to-print first-newline-position)
                       :print-slowly print-slowly)))))

(defgeneric page-window (output-window)
  (:documentation "Pause output in the specified window if it has been filled with the maximum
number of lines it can hold (except the `continue' prompt). Prompt for the user to press `Enter'
to continue output."))

(defmethod page-window ((output-window screen))

  (when (pagingp output-window)
    (incf (output-line-count output-window) 1)
    ;; Pause at one line less than the total number of lines to account for newlines.
    (when (>= (output-line-count output-window) (1- (height output-window)))
      (print-prompt "Press ENTER to continue")
      (scan-input) ; We don't care about the input, it's the enter key that matters
      (clear-type-ahead-buffer)
      (restart-paging output-window))))

(defgeneric restart-paging (output-window)
  (:documentation "Assume all window output has been seen by the player, so that paging for the
next full screen of output is ready to be displayed."))

(defmethod restart-paging ((output-window screen))

  (setf (output-line-count output-window) 0))

(defgeneric set-text-color (output-window color)
  (:documentation "Use curses calls to specify the color text will have next time it is output to
the specified window."))

;; TODO - consider using ANSI escape sequences to set colors in full screen mode
(defmethod set-text-color ((output-window screen) color)
  "Do nothing - full screen doesn't use color.")

(defmethod set-text-color ((output-window window) color)

  (cond
    ((= color +default-color+)
     (wattrset (curses-window output-window) 0))
    ((= color +black+)
     (wattrset (curses-window output-window) (color-pair color_black)))
    ((= color +blue+)
     (wattrset (curses-window output-window) (color-pair color_blue)))
    ((= color +green+)
     (wattrset (curses-window output-window) (color-pair color_green)))
    ((= color +cyan+)
     (wattrset (curses-window output-window) (color-pair color_cyan)))
    ((= color +red+)
     (wattrset (curses-window output-window) (color-pair color_red)))
    ((= color +magenta+)
     (wattrset (curses-window output-window) (color-pair color_magenta)))
    ((= color +brown+)
     (wattrset (curses-window output-window) (color-pair color_yellow)))
    ((= color +light-gray+)
     (wattrset (curses-window output-window) (color-pair color_white)))
    ((= color +dark-gray+)
     (wattrset (curses-window output-window) (color-pair (logior color_black a_bold))))
    ((= color +light-blue+)
     (wattrset (curses-window output-window) (color-pair (logior color_blue a_bold))))
    ((= color +light-green+)
     (wattrset (curses-window output-window) (color-pair (logior color_green a_bold))))
    ((= color +light-cyan+)
     (wattrset (curses-window output-window) (color-pair (logior color_cyan a_bold))))
    ((= color +light-red+)
     (wattrset (curses-window output-window) (color-pair (logior color_red a_bold))))
    ((= color +light-magenta+)
     (wattrset (curses-window output-window) (color-pair (logior color_magenta a_bold))))
    ((= color +yellow+)
     (wattrset (curses-window output-window) (color-pair (logior color_yellow a_bold))))
    ((= color +white+)
     (wattrset (curses-window output-window) (color-pair (logior color_white a_bold))))
    (t ; Same as default case
     (wattrset (curses-window output-window) 0))))

(defgeneric toggle-reverse-video (output-window)
  (:documentation "Toggle the state of the reverse video attribute that will be applied to text
output after this call."))

;; TODO - consider using ANSI escape sequences to set reverse video
(defmethod toggle-reverse-video ((output-window screen))
  "Do nothing - reverse video is not used in full-screen mode."

  )

(defmethod toggle-reverse-video ((output-window window))
  "Use curses calls to toggle the reverse video attribute for future text output."

  (wattron (curses-window output-window) a_reverse))

(defgeneric put-short-range-scan-symbol (output-window symbol x y)
  (:documentation "Output a letter at the x, y coordinates of the specified window. This is assumes
x,y coordinate addressing is available so methods implementing this interface must be capable of
that specific form of coordinate addressing."))

;; TODO - should this be named "put-symbol"? If yes, then the adjustments for the short range scan
;;        header and row prefix would need to be removed.
;; C: void put_srscan_sym(coord w, char sym)
(defmethod put-short-range-scan-symbol ((output-window window) symbol x y)
  "Use curses calls to place the symbol at the x,y coordinates of the specified window."

  ;; TODO - give these constants a name, they are the number of header lines or left-side
  ;;        columns to skip
  (mvwaddstr (curses-window output-window) (+ x 2) (+ (* y 2) 3) symbol)
  (wrefresh (curses-window output-window)))

(defgeneric boom (output-window symbol x y) ; C: void boom(coord w)
  (:documentation "Output visuals and sound effects when an enemy is destroyed."))

(defmethod boom ((output-window screen) symbol x y)
  "Output sound effects and visuals for a destroyd enemy. For the default scren there are no visuals
or sound."

  )

(defmethod boom ((output-window window) symbol x y)
  "Use curses functions to output effects when an enemy is destroyed."

  (turn-sound-on 500)
  (wattron (curses-window output-window) a_reverse)
  (put-short-range-scan-symbol output-window symbol x y)
  (sleep .5)
  (set-text-color output-window +default-color+)
  (put-short-range-scan-symbol output-window symbol x y)
  (sleep .3)
  (wattron (curses-window output-window) a_reverse)
  (put-short-range-scan-symbol output-window symbol x y)
  (sleep .3)
  (set-text-color output-window +default-color+)
  (put-short-range-scan-symbol output-window symbol x y)
  (sleep .3)
  (turn-sound-off))

(defun turn-sound-on (frequency) ; C: not user-defined...
  "Play a sound on the PC speaker at the specified frequency."

  ;; TODO - implement this, probably by calling some OS routine or external library
  (setf frequency frequency))

(defun turn-sound-off ()
  "Stop sound output."

  ;; TODO - implement this, how does SDL do it?
  )

(defun warble () ; C: void warble(void)
  "Sound and visual effects for mayday - ship teleportation"

  ;; (short-range-scan) ; this was in the C source, is it needed here?
  (turn-sound-on 50)
  (print-message *message-window* "     . . . . .     " :print-slowly t)
  (sleep 1)
  (turn-sound-off))
