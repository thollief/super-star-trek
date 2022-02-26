;;;; Super Star Trek

;;Actions that allow the enemy to respond but don't take time:
;;- fire phasers
;;- fire photon torpedoes
;;- raise and lower shields
;;- dock at starbase
;;- launch probe
;;- use the transporter
;;- use deathray
;;- use dilithium crystals
;; Enemy response is attack-player

;; Actions that consume time:
;;- moving when buffeted by a nova
;;- movement when being pulled by a tractor beam
;;- capturing klingons
;;- ramming a ship
;;- time warp
;;- moving under impulse or warp drive, including supernova emergency exit
;;- resting
;;- visual scan
;;- using the shuttle craft
;;- mining dilithium crystals
;;- entering planetary orbit

;; enemy actions do not consume time

(in-package super-star-trek)

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

(defun print-stars ()
  "Print a line of stars."

  (print-message *message-window*
                 (format nil "************************************************************~%")
                 :print-slowly t))

(defun huh () ; C: huh(void)
  "Complain about unparseable input."

  (clear-type-ahead-buffer)
  (print-message *message-window* (format nil "~%Beg your pardon, Captain?~%")))

(defun get-y-or-n-p () ; C: ja()
  "When a player choice may negatively affect successful completion of the game then require a yes
or no answer. If other answers are given then prompt again. Use this function instead of y-or-n-p
to allow for curses or line-by-line output when the player is reminded of the input options."

  (do ((char nil))
      (char)
    (clear-type-ahead-buffer)
    (setf char (match-token (scan-input) (list "yes" "no")))
    (cond
      ((string= char "yes")
       (return-from get-y-or-n-p t))

      ((string= char "no")
       (return-from get-y-or-n-p nil))

      (t
       (print-message *message-window* (format nil "~%Please answer with \"y\" or \"n\": "))))))

;; TODO - should this be a property of a ship struct?
(defvar *ship-quadrant* nil) ; C: coord quadrant, where we are
(defvar *ship-sector* nil) ; C: coord sector, where we are

;; TODO - players may input fractional numbers for coordinates for moving, firing photon torpedoes,
;;        and calculating distances, and those numbers should be used. Check this.
(defun scan-coordinate-number ()
  "Read a player-entered coordinate number from the input buffer. Return the internal coordinate
number (array index) or nil."

  (let ((c-num nil)
        input-item)
    (setf input-item (scan-input))
    (when (and input-item
               (numberp input-item))
      (setf c-num (1- input-item)))
    (return-from scan-coordinate-number c-num)))

(defun scan-coordinate-pair ()
  "Read two numbers from the input line and return them. The player is expected to enter two values
but might enter only one, or none."

  (let (x y)
    (setf x (scan-coordinate-number))
    ;; The player should have entered at least two numbers for the coordinate and the second one is
    ;; still in the input buffer. If the input buffer is empty then the scan would wait for further
    ;; input, which we don't want at this point because there is no prompt for what is needed.
    (when (input-available-p)
      (setf y (scan-coordinate-number)))
    (return-from scan-coordinate-pair (values x y))))

(defun get-quadrant-and-sector ()
  "Get from the player two numbers representing a quadrant, or four numbers representing a quadrant
and sector. If only two numbers are entered then only a quadrant is specified and the sector will
be left empty. Check input for correctness and display an error message if needed. Return the valid
coordinates or nil."

  (let (qx qy sx sy)
    (multiple-value-setq (qx qy) (scan-coordinate-pair))
    ;; If there are two more coordinates then read them, otherwise we don't care what was typed
    (when (two-input-items-available)
      (multiple-value-setq (sx sy) (scan-coordinate-pair)))
    ;; TODO - the code doesn't match the comment, retest the function that calls this
    ;; If player entered only two coordinates, a quadrant, then fix up the quadrant coordinates
    (when (and qx qy (not sx) (not sy))
      (setf sx qx)
      (setf sy qy)
      (setf qx (coordinate-x *ship-quadrant*))
      (setf qy (coordinate-y *ship-quadrant*)))
    ;; Validate coordinate values and return them
    (if (and qx qy (valid-quadrant-p qx qy) sx sy (valid-sector-p sx sy))
        (return-from get-quadrant-and-sector (values sx sy qx qy))
        (huh)))
  )

(defun format-stardate (d)
  "Write a stardate with one decimal point."

  (format nil "~,1,,,F" d))

;; Characters displayed for game entities in short range scans
;; TODO - Make probes visible in short range scans. When you launch a probe and then move to the
;;        qaudrant where it is reported to be then it should be visible.
;;        This means giving the probe a sector coordinate, too, and handling it when displaying
;;        a short range scan. Being small, probes can be in the same sector as another object and
;;        the other object will be displayed on the SR scan.
;; TODO - show enterprise or faerie queene movement in the short range scan, until exiting the
;;        current quadrant, and show the same movement when entering the destination quadrant.
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
(define-constant +probe+ "^")
(define-constant +torpedo+ "+")

(defun letter-to-name (sector-entity)
  "Convert a single letter sector entity to a string for display."

  ;; TODO these are the strings actually used. Should plan for them all, or use a struct with a
  ;;      symbol and a label, like the game type or game length. Is there an alist in here
  ;;      somewhere, or an object?
  (cond
    ((string= sector-entity +klingon+)
     "Klingon")
    ((string= sector-entity +romulan+)
     "Romulan")
    ((string= sector-entity +commander+)
     "Commander")
    ((string= sector-entity +super-commander+)
     "Super-commander")
    ((string= sector-entity +tholian+)
     "Tholian")
    ((string= sector-entity +thing+)
     "Thing")
    ((string= sector-entity +black-hole+)
     "Black hole")
    ((string= sector-entity +planet+)
     "Planet")
    ((string= sector-entity +star+)
     "Star")
    (t
     "unknown")))

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
  knownp ; whether or not this planet has been scanned by the player
  inhabitedp ; C: inhabited, Whether or not the planet has inhabitants.
  crystals ; has crystals: 'absent, 'present, or 'mined
  (status 'secure)) ; C: status, One of secure, distressed, enslaved

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

(defvar *planets* nil
  "An alist of planet structs keyed by the quadrant coordinates of the planet")

;; TODO - define a ship structure. Ships have several properties: a label, a short range scan
;;        symbol, a location, dilithium crystals on board, energy, photon torpedoes, a shuttle
;;        craft, warp factor, equipment (some of which may be damaged). At the time this comment
;;        is being written it's not clear to me if collecting these things in a struct will improve
;;        code quality but it is what I understand the "right" way to be

;; TODO - define a probe structure? It's also an entity that moves around the galaxy, as does the
;;        super-commander and sometimes commanders.

(defstruct quadrant ; C: quadrant
  "Information about a quadrant. Some entities in a quadrant are transient and don't need to be
tracked, including Tholians, black holes, and space things.

When a quadrant has had a supernova don't zero out the number of stars, planets, and starbases. If
the player caused the supernova these destroyed objects count against the final score and need to
be tracked."

  (stars 0)
  (starbases 0) ; 0 or 1
  (klingons 0) ; number of klingons of all type: klingons + commanders + super commanders
  (romulans 0)
  (supernovap nil)
  (chartedp nil))

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
  (planets ()) ; C: planets
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
;; structure with elements damage, output string? A list of structs? Or some sort of object...
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
(define-constant +spy+ 0
  "Spy event happens always (no future[] entry), can cause SC to tractor beam Enterprise") ; C: FSPY
(define-constant +supernova+ 1) ; C: FSNOVA
(define-constant +tractor-beam+ 2 "Commander tractor beams Enterprise.") ; C: FTBEAM
(define-constant +snapshot-for-time-warp+ 3) ; C: FSNAP
(define-constant +commander-attacks-base+ 4) ; C: FBATTAK
(define-constant +commander-destroys-base+ 5) ; C: FCDBAS
(define-constant +move-super-commander+ 6
  "The Super-commander moves, and might attack a base.") ; C: FSCMOVE
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
(defvar *conquest-quadrant* nil "Location of planet where the distress/enslave/reproduce events occur.")

(define-constant +sst-version+ "SST 2.0") ; C: SSTMAGIC

;; User-selectable game parameters.
(defvar *tournament-number* nil "Tournament number, or nil if regular game.") ; C: tourn
(defparameter *game-length-values* (list
                                    (cons "short" 1)
                                    (cons "medium" 2)
                                    (cons "long" 4))
  "Numeric values for game lengths used to intialize parameters in new games.")
(defvar *game-length* nil
  "A string containing player's selection of game length.") ; C: length
(define-constant +novice+ 1) ; C: skill, SKILL_NOVICE = 1
(define-constant +fair+ 2) ; C: skill, SKILL_FAIR = 2
(define-constant +good+ 3) ; C: skill, SKILL_GOOD = 3
(define-constant +expert+ 4) ; C: skill, SKILL_EXPERT = 4
(define-constant +emeritus+ 5) ; C: skill, SKILL_EMERITUS = 5
(defparameter *skill-level-labels* (list
                                    (cons +novice+ "novice")
                                    (cons +fair+ "fair")
                                    (cons +good+ "good")
                                    (cons +expert+ "expert")
                                    (cons +emeritus+ "emeritus"))
  "Labels for skill levels.")
(defvar *skill-level* nil
  "A number representing the player's selection of skill level.") ; C: skill
(defvar *self-destruct-password* nil) ; C: passwd[10]

(defstruct mover
  "An entity in the game that can move. This includes the player ship, enemy ships, torpedoes,
and probes. Entities in motion have a current x,y location, an increment by which they will next
change location, a speed (warp factor), and a destination. When a destination is not specified
the entity may continue moving indefinitely, until it exits the current sector (torpedoes) or exits
 the galaxy (probes)."

  x ; cuurent sector x coordinate within the galaxy
  y ; current sector y coordinate within the galaxy
  warp-factor ; speed at which the entity moves
  inc-x ; amount of change in the x direction next time the entity moves
  inc-y ; amount of change in the y direction next time the entity moves
  d-x ; destination sector x coordinate within the galaxy
  d-y ; destination sector y coordinate within the galaxy
  moves-remaining) ; number of moves remaining to cover the desired distance

;; The Enterprise
;; TODO - the Faerie Queene has no shuttle craft or deathray so it should not be
;; possible/allowed/necessary to check if those devices are damaged
(define-constant +full-crew+ 428) ; C: FULLCREW, BSD Trek was 387, that's wrong
(defvar *ship* +enterprise+) ; C: ship, 'E' is Enterprise
(defvar *initial-energy* 5000.0 "Initial and max energy") ; C: inenrg
(defvar *ship-energy* 5000.0) ; C: energy
(defvar *initial-shield-energy* 2500.0 "Initial and max shield") ; C: inshld
(defvar *shield-energy* 2500.0) ; C: shield
(defvar *shields-are-changing-p* nil) ; C: shldchg, affects efficiency
(defvar *shields-are-up-p* nil) ; C: shldup
(defvar *cloakedp* nil "Cloaking is enabled") ; C: iscloaked
(defvar *cloakingp* nil "In the process of cloaking and can be attacked") ; C: iscloaking
(defvar *initial-life-support-reserves* 4.0) ; C: inlsr
(defvar *life-support-reserves* 4.0) ; C: lsupres
(defvar *initial-torpedos* 10 "Initial and max torpedoes") ; C: intorps
(defvar *torpedoes* 10) ; C: torps
(defvar *warp-factor* 5.0) ; C: warpfac, Warp speed
(defvar *device-damage* (make-array +number-of-devices+ :initial-element 0.0)) ; C: damage[NDEVICES], Damage encountered
(defvar *crew* +full-crew+) ; C: crew, crew complement
(defvar *abandoned-crew* 0 "Count of crew abandoned in space") ; C: abandoned
(defvar *casualties* 0) ; C: casual
;; TODO - show brig free/capacity as part of the ship status
(defvar *brig-capacity* 0 "How many Klingons the brig will hold") ; C: brigcapacity
(defvar *brig-free* 0 "room in the brig") ; C: brigfree
(defvar *dilithium-crystals-on-board-p* nil) ; C: icrystl
(defvar *crystal-work-probability* 0.0) ; C: cryprob, probability that crystal will work
(defvar *probes-available* 0) ; C: nprobes

(defvar *condition* nil "red, yellow, green") ; C: condition
;; Values for *condition*
(define-constant +green-status+ "GREEN") ; C: IHGREEN
(define-constant +yellow-status+ "YELLOW") ; C: IHYELLOW
(define-constant +red-status+ "RED") ; C: IHRED

;; TODO - decide if orbital cloaking is possible
(defvar *dockedp* nil) ; a possible flight condition
(defvar *in-orbit-p* nil) ; C: inorbit, orbiting - a possible flight condition
(defvar *height-of-orbit* 0) ; C: height, height of orbit around planet

(defvar *initial-bases* 0) ; C: inbase
(defvar *destroyed-bases* 0 "Number of bases destroyed by player action.") ; C: basekl
(defvar *base-quadrants* ()
  "A list of coordinate structs, these are quadrants containing bases.")

(defvar *initial-stars* 0); C: instar
(defvar *destroyed-stars* 0 "Number of stars destroyed by player action.") ; C: starkl

(defvar *initial-planets* 0) ; C: inplan
(defvar *destroyed-inhabited-planets* 0) ; C: nworldkl
(defvar *destroyed-uninhabited-planets* 0) ; C: nplankl

(defvar *initial-klingons* 0) ; C: inkling
(defvar *remaining-klingons* 0) ; C: remkl
(defvar *captured-klingons* 0 "number of captured Klingons") ; C: kcaptured

(defvar *initial-commanders* 0) ; C: incom
(defvar *commander-quadrants* () "List of coordinate structs, these are quadrants containing commanders.")

(defvar *initial-super-commanders* 0); C: inscom
(defvar *remaining-super-commanders* 0) ; C: nscrem
(defvar *super-commander-quadrant* nil) ; C: kscmdr

(defvar *initial-romulans* 0) ; C: inrom
(defvar *remaining-romulans* 0) ; C: nromrem

(defvar *cloaking-violations* 0 "Number of treaty violations") ; C: ncviol
(defvar *cloaking-violation-reported-p* nil "Violation reported by Romulan in quadrant") ; C: isviolreported

(defvar *initial-resources* 0.0) ; C: inresor
(defvar *remaining-resources* 0.0) ; C: remres
(defvar *initial-time* 0.0) ; C: intime
(defvar *remaining-time* 0.0) ; C: remtime
(defvar *initial-stardate* 0.0) ; C: indate
(defvar *stardate* 0.0) ; C: double date

(defvar *damage-factor* 0.0 "Damage modifier based on skill level") ; C: damfac

(defvar *galaxy* nil "The Galaxy, an array of quadrants") ; C: galaxy[GALSIZE+1][GALSIZE+1]
(defvar *starchart* nil "The starchart, an array of starchart-pages") ; C: chart[GALSIZE+1][GALSIZE+1]

(defstruct enemy
  "Information about an enemy in a sector.

Average distance is the average of the distances between the player ship and an enemy before the
 ship moved and after the ship moved. The average distance is used to calculate the strength of
 the enemy's attack on the player, not the previous or current distances, either of which could be
greater or lesser."

  energy ; C: kpower
  distance ; C: kdist
  average-distance ; C: kavgd
  sector-coordinates) ; C: ks

;; The array size needs to accommodate the deathray case of filling the sector.
;; TODO - there seems to be two arrays for similar items
(defvar *quadrant-enemies* (make-array (* +quadrant-size+ +quadrant-size+))
  "Information about enemies in the quadrant, represented as an enemy struct.")

(defvar *quadrant* (make-array (list +quadrant-size+ +quadrant-size+))
  "The contents of the current quadrant as seen on a short range scan. That is, an array of
 strings, each containing a single character.") ; C: feature quad[QUADSIZE+1][QUADSIZE+1];

(defvar *tholian-sector* nil) ; C: coord tholian, coordinates of tholian
(defvar *base-sector* nil) ; C: base, coordinate position of base in current quadrant

;; Other game parameters
;; TODO - should this be in an event structure?
(defvar *base-under-attack-quadrant* nil) ; C: battle, Base coordinates being attacked - a coordinate struct, or nil. See also the event +commander-attacks-base+
(defvar *calls-for-help* 0) ; C: nhelp
(defvar *energy-barrier-crossings* 0 "Count of energy-barrier crossings") ; C: nkinks
(defvar *in-shuttle-craft-p* nil "Kirk in Galileo") ; C: icraft
(defvar *miningp* nil) ; C: imine
(defvar *restingp* nil) ; C: resting, rest time
(defvar *super-commander-attack-enterprise-p* nil) ; C: iscate
(defvar *super-commander-attacking-base* 'not-atacking
  "Possible states are attacking, not attacking, in the process of destroying.") ; C: isatb
(defvar *shuttle-craft-location* 'on-ship
  "Location of the shuttle craft. One of 'on-ship, 'off-ship, or 'removed. 'off-ship is effectively
the same as being on a planet.") ; C: iscraft
(defvar *shuttle-craft-quadrant* nil "The quadrant coordinates of the shuttle craft. They are
the same as the ship if the shuttle craft location is on-ship.")
(defvar *alivep* t "The player is alive (not killed)") ; C: alive
(defvar *action-taken-p* nil "If an action is taken then enemy is allowed to attack.") ; C: ididit

(defvar *snapshot* nil) ; C: snapshot snapsht;
(defvar *snapshot-taken-p* nil) ; C:snap
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
  (setf *planets* (snapshot-planets *snapshot*))
  (setf *stardate* (snapshot-stardate *snapshot*))
  (setf *shuttle-craft-location* (snapshot-shuttle-craft-location *snapshot*))
  (setf *shuttle-craft-quadrant* (snapshot-shuttle-craft-quadrant *snapshot*))
  (setf *base-quadrants* (snapshot-base-quadrants *snapshot*))
  (setf *commander-quadrants* (snapshot-commander-quadrants *snapshot*))
  (setf *super-commander-quadrant* (snapshot-super-commander-quadrant *snapshot*))
  (setf *galaxy* (snapshot-galaxy *snapshot*))
  (setf *starchart* (snapshot-starchart *snapshot*)))

;; The game
(defvar *game-won-p* nil) ; C: gamewon, Finished!
(defvar *all-done-p* t "Game is finished. True at the end of a game or if no game is in progress.") ; C: alldone

;; Information about the current quadrant, set each time the ship enters a new quadrant
(defvar *just-in-p* nil
  "True when the player has entered the quadrant but not yet taken any action.") ; C: justin
(defvar *klingons-here* nil) ; C: klhere
(defvar *commanders-here* nil) ; C: comhere - TODO could this be in the quadrant structure like Klingons and Romulans?
(defvar *super-commanders-here* 0) ; C: ishere - refers to the current quadrant
(defvar *romulans-here* nil) ; C: irhere	, number of Romulans in quadrant
(defvar *planet-coord* nil) ; C: iplnet - coordinates of a planet in the current quadrant, if any
(defvar *enemies-here* nil "Number of enemies in quadrant") ; C: nenhere
(defvar *romulan-neutral-zone-p* nil) ; C: neutz	, Romulan Neutral Zone
(defvar *landedp* nil) ; C: landed	, party on planet (true), on ship (false)
(defvar *attempted-escape-from-super-commander-p* nil) ; C: ientesc
(defvar *tholians-here* 0) ; C: ithere - Max 1 Tholian in a quadrant but this is an entity count not a boolean
(defvar *base-attack-report-seen-p* nil) ; C: iseenit
(defvar *current-planet* nil "Sector coordinate location of a planet in the current quadrant, if any") ; C: plnet

(defvar *time-taken-by-current-operation* 0.0) ; C: optime

(defstruct (probe (:include mover))
  "A deep space probe launched by the Enterprise."

  is-armed-p)  ; C: isarmed, When true, the probe's NOVAMAX warhead is armed and will detonate at the destination

(defvar *probe* nil
  "The probe currently in flight.")

;; The Space Thing's global state should *not* be saved! This prevents players proving they
;; encountered it by examining a saved game.
(defvar *thing-location* nil "Location of strange object in galaxy"); C: thing
(defvar *things-here* 0) ; C: bool iqhere - Normally Max 1 Thing in a quadrant but this an entity count not a boolean.
(defvar *thing-is-angry-p* nil) ; C: bool iqengry
(defvar *score* 0) ; C: iscore, Common PLAQ
(defvar *seed* 0) ; C: int seed, the random-number seed
(defvar *log-file* 0) ; C: FILE *logfp, TODO - this should be a file, deal with it later
(defvar *replay-file* 0) ; C: FILE *replayfp, TODO - this should be a file, deal with it later

;; Internal documentation of system names
;;
;; I used <http://www.memory-alpha.org> to find planets
;; with references in ST:TOS.  Eath and the Alpha Centauri
;; Colony have been omitted.
;;
;; Some planets marked Class G and P here will be displayed as class M
;; because of the way planets are generated. This is a known bug.
;;
;;      Federation Worlds
;; "Andoria (Fesoan)" class M,  several episodes
;; "Tellar Prime (Miracht)" class M,  TOS: "Journey to Babel"
;; "Vulcan (T'Khasi)" class M,  many episodes
;; "Medusa" class ?,   TOS: "Is There in Truth No Beauty?"
;; "Argelius II (Nelphia)" class M, TOS: "Wolf in the Fold" ("IV" in BSD)
;; "Ardana" class M,   TOS: "The Cloud Minders"
;; "Catulla (Cendo-Prae)" class ?,  TOS: "The Way to Eden"
;; "Gideon" class M,   TOS: "The Mark of Gideon"
;; "Aldebaran III" class ?,  TOS: "The Deadly Years"
;; "Alpha Majoris I" class ?,  TOS: "Wolf in the Fold"
;; "Altair IV" class ?,   TOS: "Amok Time
;; "Ariannus" class ?,   TOS: "Let That Be Your Last Battlefield"
;; "Benecia" class ?,   TOS: "The Conscience of the King"
;; "Beta Niobe I (Sarpeidon)" class ?,  TOS: "All Our Yesterdays"
;; "Alpha Carinae II" class ?,  TOS: "The Ultimate Computer"
;; "Capella IV (Kohath)" class M,  TOS: "Friday's Child" (Class G)
;; "Daran V" class ?,   TOS: "For the World is Hollow and I Have Touched the Sky"
;; "Deneb II" class ?,   TOS: "Wolf in the Fold" ("IV" in BSD)
;; "Eminiar VII" class ?,   TOS: "A Taste of Armageddon"
;; "Gamma Canaris IV" class ?,  TOS: "Metamorphosis"
;; "Gamma Tranguli VI (Vaalel)" class ?,  TOS: "The Apple"
;; "Ingraham B" class M,   TOS: "Operation: Annihilate"
;; "Janus IV" class ?,   TOS: "The Devil in the Dark"
;; "Makus III" class ?,   TOS: "The Galileo Seven"
;; "Marcos XII" class ?,   TOS: "And the Children Shall Lead",
;; "Omega IV" class M,   TOS: "The Omega Glory"
;; "Regulus V" class M,   TOS: "Amok Time
;; "Deneva" class ?,   TOS: "Operation -- Annihilate!"
;;
;;     Worlds from BSD Trek
;; "Rigel II" class ?,   TOS: "Shore Leave" ("III" in BSD)
;; "Beta III" class M,   TOS: "The Return of the Archons"
;; "Triacus" class ?,   TOS: "And the Children Shall Lead",
;; "Exo III" class ?,   TOS: "What Are Little Girls Made Of?" (Class P)
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

(defun damagedp (device) ; C: #define damaged(dev) (game.damage[dev] != 0.0)
  "Evaluate whether or not a device is damaged."

  (/= (aref *device-damage* device) 0.0))

(defun clear-screen ()
  "Clear all windows."

  (when *message-window*
    (clear-window *message-window*))
  (when *short-range-scan-window*
    (clear-window *short-range-scan-window*))
  (when *ship-status-window*
    (clear-window *ship-status-window*))
  (when *game-status-window*
    (clear-window *game-status-window*))
  (when *long-range-scan-window*
    (clear-window *long-range-scan-window*))
  (when *message-window*
    (clear-window *message-window*))
  (when *prompt-window*
    (clear-window *prompt-window*))
  (when *starchart-window*
    (clear-window *starchart-window*))
  (when *damage-report-window*
    (clear-window *damage-report-window*))
  (when *planet-report-window*
    (clear-window *planet-report-window*))
  (when *score-window*
    (clear-window *score-window*)))

(defun calculate-warp-movement-time (&key warp-factor distance)
  "Given a movement speed (warp factor) and distance (in quadrants) calculate the amount of game
 time needed to move that distance."

  (/ (* 10.0 distance) (expt warp-factor 2)))

;; TODO - write a calculate-remaining-time function

(defun attack-report () ; void attackreport(bool curt)
  "Report the status of bases under attack."

  (cond
    ((is-scheduled-p +commander-destroys-base+)
     (print-message *message-window* (format nil "Starbase in ~A is currently under Commander attack.~%"
                            (format-quadrant-coordinates *base-under-attack-quadrant*)))
     (print-message *message-window* (format nil "It can hold out until Stardate ~A.~%"
                            (format-stardate (truncate (scheduled-for +commander-destroys-base+))))))

    ((eql *super-commander-attacking-base* 'attacking)
     (print-message *message-window* (format nil "Starbase in ~A is under Super-commander attack.~%"
                            (format-quadrant-coordinates *super-commander-quadrant*)))
     (print-message *message-window* (format nil "It can hold out until Stardate ~A.~%"
                            (format-stardate (truncate (scheduled-for +super-commander-destroys-base+))))))

    (t
     (print-message *message-window* (format nil "No Starbase is currently under attack.~%")))))

(defun update-condition () ; C: void newcnd(void)
  "Update our alert status to one of Green, Yellow, or Red. Yellow supercedes Green and Red
supercedes Yellow."

  (cond
    ;; Enemies present - always condition Red regardless of other factors
    ((or (> (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0)
         (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0))
     (setf *condition* +red-status+))
    ;; Energy low - always condition Yellow
    ((< *ship-energy* 1000.0)
     (setf *condition* +yellow-status+))
    ;; The Experimental Death Ray isn't an essential ship system
    ((and (= (damaged-device-count) 1)
          (damagedp +death-ray+))
     (setf *condition* +green-status+))

    ((> (damaged-device-count) 0)
     (setf *condition* +yellow-status+))

    (t
     (setf *condition* +green-status+))))

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
        (setf (coord-ref *quadrant* nova-sector) +empty-sector+)
        (print-message *message-window* (format nil "Star at ~A novas.~%"
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
                   (print-message *message-window* (format nil "~%Force of nova displaces starship.~%"))
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
              (setf adjacent-coord (make-sector-coordinate :x adjacent-x :y adjacent-y))
              (when (and (valid-sector-p adjacent-x adjacent-y)
                         (not (coord-equal adjacent-coord n-sector)))
                (cond
                  ;; Affect another star
                  ((string= (aref *quadrant* adjacent-x adjacent-y) +star+)
                   (if (< (random 1.0) 0.05)
                       (progn
                         ;; This star supernovas
                         (supernova *ship-quadrant* adjacent-coord)
                         (return-from nova nil))
                       (progn
                         (setf (aref *quadrant* adjacent-x adjacent-y) +empty-sector+)
                         (append nova-stars adjacent-coord)
                         (print-message *message-window* (format nil "Star at ~A novas.~%"
                                                (format-sector-coordinates adjacent-coord)))
                         (setf (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))
                               (1- (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
                         (setf *destroyed-stars* (1+ *destroyed-stars*)))))
                  ;; Destroy planet
                  ((string= (aref *quadrant* adjacent-x adjacent-y) +planet+)
                   (setf *destroyed-uninhabited-planets* (1+ *destroyed-uninhabited-planets*))
                   (print-message *message-window* (format nil "Planet at ~A destroyed.~%"
                                          (format-sector-coordinates adjacent-coord)))
                   ;; Update the planet struct to show planet is destroyed, then put it back in the alist
                   (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
                     (setf (planet-destroyedp p) t)
                     (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) p))
                   (setf *planet-coord* nil)
                   (setf *current-planet* nil)
                   (when *landedp*
                     (finish 'nova-destroys-planet-while-landed)
                     (return-from nova nil))
                   (setf (aref *quadrant* adjacent-x adjacent-y) +empty-sector+))
                  ;; Destroy base
                  ((string= (aref *quadrant* adjacent-x adjacent-y) +starbase+)
                   (setf (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*)) 0)
                   (setf *base-quadrants* (remove *ship-quadrant* *base-quadrants* :test #'coord-equal))
                   (setf *base-sector* nil)
                   (incf *destroyed-bases* 1)
                   (setf *dockedp* nil) ; No need to test if previously docked
                   (print-message *message-window* (format nil "Starbase at ~A destroyed.~%"
                                          (format-sector-coordinates adjacent-coord)))
                   (setf (aref *quadrant* adjacent-x adjacent-y) +empty-sector+))
                  ;; Buffet ship
                  ((or (string= (aref *quadrant* adjacent-x adjacent-y) +enterprise+)
                       (string= (aref *quadrant* adjacent-x adjacent-y) +faerie-queene+))
                   (print-message *message-window* (format nil "***Starship buffeted by nova.~%"))
                   (if *shields-are-up-p*
                       (if (>= *shield-energy* 2000.0)
                           (decf *shield-energy* 2000.0)
                           (let ((energy-difference (- 2000.0 *shield-energy*)))
                             (decf *ship-energy* energy-difference)
                             (setf *shield-energy* 0.0)
                             (setf *shields-are-up-p* nil)
                             (print-message *message-window* (format nil "***Shields knocked out.~%"))
                             (incf (aref *device-damage* +shields+) (* 0.005
                                                                       *damage-factor*
                                                                       (random 1.0)
                                                                       energy-difference))))
                       (decf *ship-energy* 2000.0))
                   (when (<= *ship-energy* 0)
                     (finish 'ship-destroyed-by-nova)
                     (return-from nova nil))
                   ;; add in course nova contributes to kicking starship
                   (incf change-x (- (coordinate-x *ship-sector*) (coordinate-x n-sector)))
                   (incf change-y (- (coordinate-y *ship-sector*) (coordinate-y n-sector)))
                   (incf nova-pushes 1))
                  ;; Kill klingon
                  ((string= (aref *quadrant* adjacent-x adjacent-y) +klingon+)
                   (dead-enemy adjacent-coord +klingon+ adjacent-coord))
                  ;; Damage/destroy big enemies
                  ((or (string= (aref *quadrant* adjacent-x adjacent-y) +commander+)
                       (string= (aref *quadrant* adjacent-x adjacent-y) +super-commander+)
                       (string= (aref *quadrant* adjacent-x adjacent-y) +romulan+))
                   (do ((enemy-index 0 (1+ enemy-index)))
                       ((or (>= enemy-index *enemies-here*)
                            (coord-equal (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index))
                                         adjacent-coord))
                        (decf (enemy-energy (aref *quadrant-enemies* enemy-index)) 800.0)
                        (if (<= (enemy-energy (aref *quadrant-enemies* enemy-index)) 0.0)
                            ;; If firepower is lost, die
                            (dead-enemy adjacent-coord
                                        (aref *quadrant* adjacent-x adjacent-y)
                                        adjacent-coord)
                            (let ((new-coord (make-sector-coordinate
                                              :x (+ adjacent-x (- adjacent-x (coordinate-x n-sector)))
                                              :y (+ adjacent-y (- adjacent-y (coordinate-y n-sector))))))
                              (print-message *message-window* (format nil "~A at ~A damaged"
                                                 (letter-to-name (aref *quadrant* adjacent-x adjacent-y))
                                                 (format-sector-coordinates adjacent-coord)))
                              (cond
                                ;; can't leave quadrant
                                ((not (valid-p new-coord))
                                 (print-message *message-window* (format nil ".~%")))

                                ((string= (coord-ref *quadrant* new-coord) +black-hole+)
                                 (print-message *message-window* (format nil ", blasted into black hole at ~A.~%"
                                                        (format-sector-coordinates new-coord)))
                                 (dead-enemy adjacent-coord (aref *quadrant* adjacent-x adjacent-y)
                                             adjacent-coord))
                                ;; can't move into something else
                                ((string/= (coord-ref *quadrant* new-coord) +empty-sector+)
                                 (print-message *message-window* (format nil ".~%")))

                                (t
                                 (print-message *message-window* (format nil ", buffeted to ~A.~%"
                                                        (format-sector-coordinates new-coord)))
                                 (setf (coord-ref *quadrant* new-coord)
                                       (aref *quadrant* adjacent-x adjacent-y))
                                 (setf (aref *quadrant* adjacent-x adjacent-y) +empty-sector+)
                                 (setf (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index))
                                       new-coord)
                                 (setf (enemy-average-distance (aref *quadrant-enemies* enemy-index))
                                       (distance *ship-sector* new-coord))
                                 (setf (enemy-distance (aref *quadrant-enemies* enemy-index))
                                       (distance *ship-sector* new-coord)))))))))
                  ;; Empty space, nothing to do
                  (t
                   ;; +empty-sector+
                   ;; +thing+
                   ;; +black-hole+
                   ;; +tholian+
                   ;; +tholian-web+
                   nil)))))))))

(defun get-random-star ()
  "Get the sector coordinates of a random star in the current quadrant."

  (let ((sector nil)
        nth-star)
    (when (> (quadrant-stars (coord-ref *galaxy* *ship-quadrant*)) 0)
          (setf sector (make-sector-coordinate))
          (setf nth-star (random (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
          (do ((x 0 (1+ x)))
              ((or (>= x +quadrant-size+)
                   (<= nth-star 0)))
            (do ((y 0 (1+ y)))
                ((or (>= y +quadrant-size+)
                     (<= nth-star 0)))
              (when (string= (aref *quadrant* x y) +star+)
                (setf nth-star (1- nth-star))
                (setf (coordinate-x sector) x)
                (setf (coordinate-y sector) y)))))
    (return-from get-random-star sector)))

(defun supernova (nova-quadrant nova-sector) ; C: supernova(bool induced, coord *w)
  "A star goes supernova."

  (if (or (not (coord-equal nova-quadrant *ship-quadrant*))
          *just-in-p*)
      ;; It isn't here, or we just entered (treat as enroute)
      (when (subspace-radio-available-p)
        (print-message *message-window* (format nil "~%Message from Starfleet Command       Stardate ~A~%"
                               (format-stardate *stardate*)))
        (print-message *message-window* (format nil "     Supernova in ~A; caution advised.~%"
                               (format-quadrant-coordinates nova-quadrant))))
      ;; we are in the quadrant!
      (progn
        (print-message *message-window* (format nil "~%***RED ALERT!  RED ALERT!~%") :print-slowly t)
        (print-message *message-window* (format nil "~%***Incipient supernova detected at ~A~%"
                               (format-sector-coordinates nova-sector)))
        ;; Is the player too close to the supernova to survive?
        (when (<= (+ (expt (- (coordinate-x nova-sector) (coordinate-x *ship-sector*)) 2)
                     (expt (- (coordinate-y nova-sector) (coordinate-y *ship-sector*)) 2))
                  2.1)
          (print-message *message-window* "Emergency override attempts t")
          (print-message *message-window* (format nil "***************~%~%") :print-slowly t)
          (print-stars)
          (setf *all-done-p* t))))
  (when (and *base-under-attack-quadrant*
             (coord-equal nova-quadrant *base-under-attack-quadrant*))
    (unschedule-event +super-commander-destroys-base+)
    (unschedule-event +commander-destroys-base+)
    (setf *base-under-attack-quadrant* nil))
  ;; Destroy any Klingons in supernovaed quadrant
  ;; and count down the number of remaining klingons as they are removed
  (when (and *super-commander-quadrant*
             (coord-equal nova-quadrant *super-commander-quadrant*))
    ;; did in the Super-Commander!
    (setf *remaining-super-commanders* 0)
    (setf *super-commander-quadrant* nil)
    (setf *super-commander-attacking-base* 'not-attacking)
    (setf *super-commander-attack-enterprise-p* nil)
    (unschedule-event +move-super-commander+)
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
      (unschedule-event +tractor-beam+)
      (unschedule-event +commander-attacks-base+)
      (unschedule-event +commander-destroys-base+)))
  (decf *remaining-klingons* (quadrant-klingons (coord-ref *galaxy* nova-quadrant)))
  (setf (quadrant-klingons (coord-ref *galaxy* nova-quadrant)) 0)
  ;; destroy Romulans and planets in supernovaed quadrant
  (decf *remaining-romulans* (quadrant-romulans (coord-ref *galaxy* nova-quadrant)))
  (setf (quadrant-romulans (coord-ref *galaxy* nova-quadrant)) 0)
  ;; Destroy planet if there is one
  (let ((p (rest (assoc nova-quadrant *planets* :test #'coord-equal))))
    (when p ; p should be nil if there is no planet in the quadrant
      (setf (planet-destroyedp p) t)
      (rplacd (assoc nova-quadrant *planets* :test #'coord-equal) p)))
  ;; Destroy any base in supernovaed quadrant
  (setf *base-quadrants* (remove nova-quadrant *base-quadrants* :test #'coord-equal))
  ;; mark supernova in galaxy and in star chart
  (when (or (coord-equal *ship-quadrant* nova-quadrant)
            (subspace-radio-available-p))
    (setf (quadrant-supernovap (coord-ref *galaxy* nova-quadrant)) t))
  ;; Either case ends the game but neither necessarily occurs. There is no default case.
  (cond
    ((and (not (coord-equal *ship-quadrant* nova-quadrant))
          (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0))
     ;; If supernova destroys last Klingons give special message
     (skip-line *message-window* 2)
     ;; TODO - the C source only printed "Lucky you" if the supernova was not caused by a deep space
     ;; probe, that is, not induced by the player. Restore this? If yes, then also restore the score
     ;; updates performed in this function when a probe causes a supernova.
     (print-message *message-window* (format nil "Lucky you!~%"))
     (print-message *message-window*
                    (format nil "A supernova in ~A has just destroyed the last Klingons.~%"
                            (format-quadrant-coordinates nova-quadrant)))
     (finish 'won))
    (*all-done-p*
     ;; if some Klingons remain, continue or die in supernova.
     (finish 'destroyed-by-supernova))))

(defun execute-tractor-beam (&key t-quadrant)
  "A commander or super-commander has tractor beamed the player ship.

t-quadrant is the quadrant to which the ship is pulled"

  (setf *time-taken-by-current-operation*
        (calculate-warp-movement-time :distance (distance t-quadrant *ship-quadrant*)
                                      :warp-factor 7.5)) ; 7.5 is tractor beam yank rate
  (print-message *message-window*
                 (format nil "~%***~A caught in long range tractor beam--~%" (format-ship-name)))
  ;; If Kirk & Co. screwing around on planet, handle
  (quadrant-exit-while-on-planet 'tractor-beam-while-mining)
  (when *all-done-p*
    (return-from execute-tractor-beam nil))
  (when *in-shuttle-craft-p* ; Caught in Galileo?
    (finish 'tractor-beam-destroys-shuttle)
    (return-from execute-tractor-beam nil))
  ;; Check to see if shuttle is aboard
  (when (eql *shuttle-craft-location* 'off-ship)
    (if (> (random 1.0) 0.5)
        (progn
          (print-message *message-window*
                         (format nil "~%Galileo, left on the planet surface, is captured~%"))
          (print-message *message-window*
                         (format nil "by aliens and made into a flying McDonald's.~%"))
          (setf (aref *device-damage* +shuttle+) -10) ; TODO - stop using special values in the damage array to store device properties
          (setf *shuttle-craft-location* 'removed)
          (setf *shuttle-craft-quadrant* nil))
        (print-message *message-window*
                       (format nil "~%Galileo, left on the planet surface, is well hidden.~%"))))
  (setf *ship-quadrant* t-quadrant)
  (setf *ship-sector* (get-random-sector))
  (print-message *message-window* (format nil "~A is pulled to ~A, ~A~%"
                         (format-ship-name)
                         (format-quadrant-coordinates *ship-quadrant*)
                         (format-sector-coordinates *ship-sector*)))
  (when *restingp*
    (print-message *message-window* (format nil "(Remainder of rest/repair period cancelled.)~%"))
    (setf *restingp* nil))
  (unless *shields-are-up-p*
    (if (and (not (damagedp +shields+))
             (> *shield-energy* 0))
        (progn
          (shield-actions :raise-shields t) ; raise shields
          (setf *shields-are-changing-p* nil))
        (print-message *message-window* (format nil "(Shields not currently useable.)~%"))))
  (new-quadrant)
  (attack-player :torpedoes-ok-p nil))

(defun cancel-rest-p () ; C: bool cancelrest(void)
  "Rest period is interrupted by event."

  (when *restingp*
    (print-prompt "Mr. Spock-  \"Captain, shall we cancel the rest period?\"")
    (when (get-y-or-n-p)
      (setf *restingp* nil)
      (setf *time-taken-by-current-operation* 0.0)
      (return-from cancel-rest-p t)))
  (return-from cancel-rest-p nil))

(defun destroy-starbase (base-q)
  "A commander or the super-commander destroys a starbase."

  ;; No default case - if the ship is not in good repair then the base will be destroyed without
  ;; notice to the player including no change to the star chart (the player is in for a nasty
  ;; surprise)
  ;; TODO - if no notice is given here, should the player be notified if a short- or long-range
  ;;        scan shows no starbase, where previously a starbase existed? Spock would notice, he is
  ;;        able to reconstruct a starchart from memory!
  (cond
    ((coord-equal *ship-quadrant* base-q)
     ;; Handle case where base is in same quadrant as starship
     (setf (coord-ref *quadrant* *base-sector*) +empty-sector+)
     (setf *base-sector* nil)
     (setf *dockedp* nil)
     (print-message *message-window* (format nil "~%Spock-  \"Captain, I believe the starbase has been destroyed.\"~%"))
     (setf (starchart-page-starbases (coord-ref *starchart* base-q)) 0))
    ;; Get word via subspace radio
    ((and (> (length *base-quadrants*) 0) ; Need an existing base to transmit the message
          (subspace-radio-available-p)) ; and a radio to receive it.
     (print-message *message-window* (format nil "~%Lt. Uhura-  \"Captain, Starfleet Command reports that~%"))
     (print-message *message-window* (format nil "   the starbase in ~A has been destroyed by~%"
                            (format-quadrant-coordinates base-q)))
     (if (eql *super-commander-attacking-base* 'destroying)
         (print-message *message-window* (format nil "the Klingon Super-Commander~%"))
         (print-message *message-window* (format nil "a Klingon Commander~%")))
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
            (not (valid-p destination-quadrant))
            (quadrant-supernovap (coord-ref *galaxy* destination-quadrant))
            (> (quadrant-klingons (coord-ref *galaxy* destination-quadrant))
               (1- +max-klingons-per-quadrant+))
            (and *just-in-p*
                 (not *super-commander-attack-enterprise-p*)))
    (return-from move-super-commander-one-quadrant nil))
  (when avoidp
    ;; Avoid quadrants with bases if we want to avoid Enterprise
    (dolist (bq *base-quadrants*)
      (when (coord-equal bq destination-quadrant)
        (return-from move-super-commander-one-quadrant nil))))
  ;; Do the move
  (decf (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*)) 1)
  (setf *super-commander-quadrant* destination-quadrant)
  (incf (quadrant-klingons (coord-ref *galaxy* *super-commander-quadrant*)) 1)
  (when (> *super-commanders-here* 0)
    ;; SC has scooted, remove him from current quadrant
    (setf *super-commander-attack-enterprise-p* nil)
    (setf *super-commander-attacking-base* 'not-attacking)
    (setf *super-commanders-here* 0)
    (setf *attempted-escape-from-super-commander-p* nil)
    (unschedule-event +super-commander-destroys-base+)
    (do ((i 0 (1+ i)))
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant* (enemy-sector-coordinates (aref *quadrant-enemies* i)))
                      +super-commander+))
         (setf (coord-ref *quadrant* (enemy-sector-coordinates (aref *quadrant-enemies* i)))
               +empty-sector+)
         ;; TODO - this is wrong if a Tholian is the last enemy in the array, should shift
         ;;        elements instead of moving the last one
         (setf (aref *quadrant-enemies* i) (aref *quadrant-enemies* (1- *enemies-here*)))))
    (decf *klingons-here* 1)
    (decf *enemies-here* 1)
    (update-condition)
    (sort-klingons))
  ;; Check for a helpful planet
  (let ((helpful-planet (rest (assoc *super-commander-quadrant* *planets* :test #'coord-equal))))
    (when (and helpful-planet
               (eql (planet-crystals helpful-planet) 'present))
      ;; Destroy the planet
      (setf (planet-destroyedp helpful-planet) t)
      (rplacd (assoc *super-commander-quadrant* *planets* :test #'coord-equal) helpful-planet)
      (when (subspace-radio-available-p)
        (print-message *message-window* (format nil "Lt. Uhura-  \"Captain, Starfleet Intelligence reports~%"))
        (print-message *message-window* (format nil "   a planet in ~A has been destroyed~%"
                               (format-quadrant-coordinates *super-commander-quadrant*)))
        (print-message *message-window* (format nil "   by the Super-commander.\"~%")))))
  (return-from move-super-commander-one-quadrant t)) ; Looks good!

(defun move-super-commander () ; C: supercommander(void)
  "Move the Super-Commander"

  ;; Decide on being active or passive
  (let ((avoidp (or (< (/ (+ (- *initial-commanders* (length *commander-quadrants*))
                             (- *initial-klingons* *remaining-klingons*))
                          (- (+ *stardate* 0.01) *initial-stardate*))
                       (* 0.1 *skill-level* (+ *skill-level* 1.0)))
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
            (unschedule-event +move-super-commander+)
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
    (let ((destination-quadrant (make-quadrant-coordinate
                                 :x (+ (coordinate-x *super-commander-quadrant*) delta-x) ; iq
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
        (unschedule-event +move-super-commander+)
        (dolist (bq *base-quadrants*)
          ;; TODO - if  *base-under-attack-quadrant* refers to commander actions then it would
          ;;        appear that the super-commander only attacks bases already under attack
          ;; TODO - Is this where the SC decides to attack a base or is it somewhere else?
          (when (and (coord-equal bq *super-commander-quadrant*)
                     (coord-equal *base-under-attack-quadrant* *super-commander-quadrant*))
            ;; Attack the base
            (unless avoidp ; no, don't attack base!
              (setf *base-attack-report-seen-p* nil)
              (setf *super-commander-attacking-base* 'attacking)
              (schedule-event +super-commander-destroys-base+ (+ 1.0 (* 2.0 (random 1.0))))
              (when (is-scheduled-p +commander-destroys-base+)
                (postpone-event +super-commander-destroys-base+
                                (- (scheduled-for +commander-destroys-base+) *stardate*)))
              (when (subspace-radio-available-p)
                (return-from move-super-commander nil)) ; No warning
              (setf *base-attack-report-seen-p* t)
              (print-message *message-window* (format nil "Lt. Uhura-  \"Captain, the starbase in ~A~%"
                                     (format-quadrant-coordinates *super-commander-quadrant*)))
              (print-message *message-window* (format nil "   reports that it is under attack from the Klingon Super-commander.~%"))
              (print-message *message-window* (format nil "   It can survive until stardate ~D.\"~%"
                                     (scheduled-for +super-commander-destroys-base+)))
              (when *restingp*
                (print-prompt "Mr. Spock-  \"Captain, shall we cancel the rest period?\"")
                (when (get-y-or-n-p)
                  (setf *restingp* nil)
                  (setf *time-taken-by-current-operation* 0.0))) ; actually finished
              (return-from move-super-commander nil))))))
  ;; Check for intelligence report
  (when (and (or (<= (random 1.0) 0.2)
                 (> (quadrant-starbases (coord-ref *galaxy* *super-commander-quadrant*)) 0))
             (subspace-radio-available-p)
             (quadrant-chartedp (coord-ref *galaxy* *super-commander-quadrant*)))
    (print-message *message-window* (format nil "Lt. Uhura-  \"Captain, Starfleet Intelligence reports~%"))
    (print-message *message-window* (format nil "   the Super-commander is in ~A.\"~%"
                                            (format-quadrant-coordinates *super-commander-quadrant*)))))

(defun move-deep-space-probe ()
  "Move the Deep Space Probe one sector and handle the results."

  (let ((probe-quadrant
          (make-quadrant-coordinate :x (galaxy-sector-to-quadrant (probe-x *probe*))
                                    :y (galaxy-sector-to-quadrant (probe-y *probe*)))))
    (incf (probe-x *probe*) (probe-inc-x *probe*))
    (incf (probe-y *probe*) (probe-inc-y *probe*))
    (decf (probe-moves-remaining *probe*) 1) ; One less to travel
    ;; When the probe quadrant changes provide an update to the player
    (let ((i (galaxy-sector-to-quadrant (probe-x *probe*)))
          (j (galaxy-sector-to-quadrant (probe-y *probe*))))
      (when (or (/= (coordinate-x probe-quadrant) i)
                (/= (coordinate-y probe-quadrant) j))
        (setf (coordinate-x probe-quadrant) i)
        (setf (coordinate-y probe-quadrant) j)
        ;; No update if no working subspace radio
        (when (subspace-radio-available-p)
          (print-message *message-window* (format nil "~%Lt. Uhura-  \"The deep space probe "))
          (cond ((not (valid-p probe-quadrant))
                 (print-message *message-window* (format nil "has left the galaxy.\"~%"))
                 (setf *probe* nil))

                ((quadrant-supernovap (coord-ref *galaxy* probe-quadrant))
                 (print-message *message-window* "is no longer transmitting.\"~%")
                 (setf *probe* nil))

                (t
                 (print-message *message-window*
                                (format nil "is now in ~A.\"~%" (format-quadrant-coordinates
                                                                 probe-quadrant)))
                 (setf (starchart-page-klingons (coord-ref *starchart* probe-quadrant))
                       (quadrant-klingons (coord-ref *galaxy* probe-quadrant)))
                 (setf (starchart-page-starbases (coord-ref *starchart* probe-quadrant))
                       (quadrant-starbases (coord-ref *galaxy* probe-quadrant)))
                 (setf (starchart-page-stars (coord-ref *starchart* probe-quadrant))
                       (quadrant-stars (coord-ref *galaxy* probe-quadrant)))
                 (setf (quadrant-chartedp (coord-ref *galaxy* probe-quadrant)) t))))))
    ;; When an armed probe reaches the target location, cause a supernova. Even if there is no star
    ;; to blow up the probe is still destroyed. Unarmed probes keep travelling and reporting until
    ;; they exit the galaxy.
    (when (and *probe*
               (<= (probe-moves-remaining *probe*) 0)
               (probe-is-armed-p *probe*))
      (when (> (quadrant-stars (coord-ref *galaxy* probe-quadrant)) 0)
        ;; lets blow the sucker!
        (let ((supernova-sector nil))
          (when (coord-equal probe-quadrant *ship-quadrant*)
            ;; TODO - Select the star closest to the probe
            (setf supernova-sector (get-random-star)))
          (supernova probe-quadrant supernova-sector))
        ;; If player caused supernova, tally up destruction.
        (incf *destroyed-stars* (quadrant-stars (coord-ref *galaxy* probe-quadrant)))
        (incf *destroyed-bases* (quadrant-starbases (coord-ref *galaxy* probe-quadrant)))
        ;; TODO inhabited worlds are not counted but should be
        (when (assoc probe-quadrant *planets* :test #'coord-equal) ; non-nil if planet exists
          (incf *destroyed-uninhabited-planets* 1)))
      (setf *probe* nil))
    (if *probe*
        (schedule-event +move-deep-space-probe+ 0.01)
        (unschedule-event +move-deep-space-probe+))))

(defun schedule-event (event-type offset)
  "Schedule an event of the specific type to occur offset time units in the future. Return the
 event. This isn't a real event queue a la BSD Trek yet -- you can only have one event of each
 type active at any given time.  Mostly these means we can only have one FDISTR/FENSLV/FREPRO
 sequence going at any given time; BSD Trek, from which we swiped the idea, can have up to 5."

  (setf (aref *future-events* event-type) (+ *stardate* offset))
  (return-from schedule-event (aref *future-events* event-type)))

;; TODO - implement an ordered event queue and get-next-event to remove an item from the queue for
;;        processing. schedule-event is the enqueue function and unschedule-event is the dequeue
;;        function.
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
      (decf *ship-energy* (* execution-time 500.0)) ; cloaking uses energy!
      (when (<= *ship-energy* 0)
        (finish 'out-of-energy)
        (return-from process-events nil)))
    (setf *stardate* smallest-next-date) ; Advance game time
    ;; Decrement Federation resources and recompute remaining time
    (decf *remaining-resources* (* (+ *remaining-klingons*
                                      (* 4 (length *commander-quadrants*)))
                                   execution-time))
    (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
        (setf *remaining-time* (/ *remaining-resources*
                                  (+ *remaining-klingons* (* 4 (length *commander-quadrants*)))))
        (setf *remaining-time* 99))
    (when (< *remaining-time* 0)
      (finish 'federation-resources-depleted)
      (return-from process-events nil))
    ;; Any crew left alive?
    (when (<= *crew* 0)
      (finish 'all-crew-killed)
      (return-from process-events nil))
    ;; Is life support adequate?
    (when (and (damagedp +life-support+)
               (not *dockedp*))
      (when (and (< *life-support-reserves* execution-time)
                 (> (aref *device-damage* +life-support+) *life-support-reserves*))
        (finish 'life-support-consumed)
        (return-from process-events nil))
      (decf *life-support-reserves* execution-time)
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
              (decf (aref *device-damage* l) repair-time)
              (progn
                (setf (aref *device-damage* l) 0.0)
                (setf (aref repaired-devices l) t)))))
      ;; If radio repaired, update star chart and attack reports
      (when (aref repaired-devices +subspace-radio+)
        (print-message *message-window* (format nil "Lt. Uhura- \"Captain, the sub-space radio is working and~%"))
        (print-message *message-window* (format nil "   surveillance reports are coming in.~%~%"))
        (unless *base-attack-report-seen-p*
          (attack-report)
          (setf *base-attack-report-seen-p* t))
        (print-message *message-window* (format nil "   The star chart is now up to date.\"~%~%")))
      (when (or (aref repaired-devices +subspace-radio+)
                (aref repaired-devices +long-range-sensors+)
                (aref repaired-devices +short-range-sensors+))
        (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*)))
      ;; When there is a separate short range scan window, if the short range sensors were just
      ;; repaired and there is a planet in the current quadrant that has not been examined
      ;; (unknown), then automatically examine it.
      (when (and (not (eql *short-range-scan-window* *message-window*))
                 (aref repaired-devices +short-range-sensors+)
                 *current-planet*
                 (not (planet-knownp (rest (assoc *ship-quadrant* *planets* :test #'coord-equal)))))
        (sensor)))
    ;; Time was spent so subtract it from the operation time being handled by this invocation
    (decf *time-taken-by-current-operation* execution-time)
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
              (setf star-to-supernova (1+ (random (1- galaxy-star-count)))) ; Random can be 0
              (do ((supernova-q (make-quadrant-coordinate))
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
                  (decf star-to-supernova (quadrant-stars (aref *galaxy* i j)))
                  (setf (coordinate-x supernova-q) i)
                  (setf (coordinate-y supernova-q) j)))))
         (do ((y 0 (1+ y)))
             ((>= y +galaxy-size+))
           (incf galaxy-star-count (quadrant-stars (aref *galaxy* x y)))))
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
              (eql *super-commander-attacking-base* 'attacking)
              *super-commander-attack-enterprise-p*)
          (setf allow-player-input t))

         ((or *attempted-escape-from-super-commander-p*
              (and (< *ship-energy* 2000)
                   (< *torpedoes* 4)
                   (< *shield-energy* 1250))
              (and (damagedp +phasers+)
                   (or (damagedp +photon-torpedoes+)
                       (< *torpedoes* 4)))
              (and (damagedp +shields+)
                   (or (< *ship-energy* 2500)
                       (damagedp +phasers+))
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
             (unschedule-event +tractor-beam+)
             (unschedule-event +commander-attacks-base+)
             (unschedule-event +commander-destroys-base+))
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
                         (unschedule-event +tractor-beam+)
                         (unschedule-event +commander-attacks-base+)
                         (unschedule-event +commander-destroys-base+))
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
                                       :planets *planets*
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
             (unschedule-event +commander-attacks-base+)
             (unschedule-event +commander-destroys-base+))
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
                   (when (eql *super-commander-attacking-base* 'attacking) ; extra time if SC already attacking
                     (postpone-event +commander-destroys-base+
                                     (- (scheduled-for +super-commander-destroys-base+) *stardate*)))
                   (schedule-event +commander-attacks-base+
                                   (+ (scheduled-for +commander-destroys-base+) (expran (* 0.3 *initial-time*))))
                   (setf *base-attack-report-seen-p* nil)
                   ;; No warning if radio not available
                   (when (subspace-radio-available-p)
                     (setf *base-attack-report-seen-p* t)
                     (print-message *message-window* (format nil "~%Lt. Uhura-  \"Captain, the starbase in ~A"
                                            (format-quadrant-coordinates *base-under-attack-quadrant*)))
                     (print-message *message-window* (format nil "   reports that it is under attack and that it can~%"))
                     (print-message *message-window* (format nil "   hold out only until stardate ~A.\"~%"
                                            (format-stardate (scheduled-for +commander-destroys-base+))))
                     (when (cancel-rest-p)
                       (return-from process-events nil)))                               )
                 (progn
                   ;; no match found -- try later
                   (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*)))
                   (unschedule-event +commander-destroys-base+))))))
      ;; Super-Commander destroys base
      ((= event-code +super-commander-destroys-base+)
       (unschedule-event +super-commander-destroys-base+)
       ;; TODO - The logic below would seem to leave the super-commander in a "destroying a base"
       ;;        state if, for some reason, there is no base to destroy
       (setf *super-commander-attacking-base* 'destroying)
       (when (> (quadrant-starbases (coord-ref *galaxy* *super-commander-quadrant*)) 0)
         (destroy-starbase *super-commander-quadrant*)
         (setf *super-commander-attacking-base* 'not-attacking)))
      ;; Commander succeeds in destroying base
      ((= event-code +commander-destroys-base+)
       (unschedule-event +commander-destroys-base+)
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
                  (not (eql *super-commander-attacking-base* 'attacking))
                  (or (not *super-commander-attack-enterprise-p*)
                      (not *just-in-p*)))
         (move-super-commander)))
      ;; Move deep space probe
      ((= event-code +move-deep-space-probe+)
       (move-deep-space-probe)
       ;; The player targeted their own quadrant with an armed probe
       (when (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
         (return-from process-events nil)))
      ;; Inhabited system issues distress call
      ((= event-code +distress-call-from-inhabited-world+)
       (unschedule-event +distress-call-from-inhabited-world+)
       (let (candidate-planet)
         ;; Try a whole bunch of times to find something suitable.
         ;; TODO - or just make a list of candidate planets/quadrants, randomly select one, and then
         ;;        randomly decide to attack it, or not? Not sure what the frequency of success in
         ;;        "try a whole bunch of times" is, so it might be difficult to tune the randomness.
         (do ((i 0 (1+ i))
              (candidate-quadrant nil))
             ((or (> i 100)
                  candidate-quadrant)
              (setf *conquest-quadrant* candidate-quadrant))
           (setf candidate-quadrant (get-random-quadrant))
           (setf candidate-planet
                 (rest (assoc candidate-quadrant *planets* :test #'coord-equal)))
           (unless (and (not (coord-equal *ship-quadrant* candidate-quadrant))
                        candidate-planet
                        (not (planet-inhabitedp candidate-planet))
                        (eql (planet-status candidate-planet) 'secure)
                        (not (quadrant-supernovap (coord-ref *galaxy* candidate-quadrant)))
                        (> (quadrant-klingons (coord-ref *galaxy* candidate-quadrant)) 0))
             (setf candidate-quadrant nil)))
         (when *conquest-quadrant*
           ;; got one!!  Schedule its enslavement
           (schedule-event +inhabited-world-is-enslaved+ (expran *initial-time*))
           (setf (planet-status candidate-planet) 'distressed)
           (rplacd (assoc *conquest-quadrant* *planets* :test #'coord-equal)
                   candidate-planet)
           ;; tell the captain about it if we can
           (when (subspace-radio-available-p)
             (print-message *message-window* (format nil "~%Lt. Uhura- \"Captain, ~A in ~A reports it is under attack~%"
                                    (planet-name candidate-planet)
                                    (format-quadrant-coordinates *conquest-quadrant*)))
             (print-message *message-window* (format nil "            by a Klingon invasion fleet.\"~%"))
             (when (cancel-rest-p)
               (return-from process-events nil))))))
      ;; Starsystem is enslaved
      ((= event-code +inhabited-world-is-enslaved+)
       (unschedule-event +inhabited-world-is-enslaved+)
       (let ((conquest-planet
               (rest (assoc *conquest-quadrant* *planets* :test #'coord-equal))))
         ;; TODO should this status change when the last klingon in the quadrant is destroyed?
         ;; see if current distress call still active
         (if (> (quadrant-klingons (coord-ref *galaxy* *conquest-quadrant*)) 0)
             (progn
               (setf (planet-status conquest-planet) 'enslaved)
               (rplacd (assoc *conquest-quadrant* *planets* :test #'coord-equal)
                       conquest-planet)
               ;; play stork and schedule the first baby
               (schedule-event +klingons-build-ship-in-enslaved-system+
                               (expran (* 2.0 *initial-time*)))
               ;; report the disaster if we can
               (when (subspace-radio-available-p)
                 (print-message *message-window*
                  (format nil "Lt. Uhura- \"We've lost contact with starsystem ~A in ~A.\""
                          (planet-name conquest-planet)
                          (format-quadrant-coordinates *conquest-quadrant*)))))
             (progn
               (setf (planet-status *conquest-quadrant*) 'secure)
               (rplacd (assoc *conquest-quadrant* *planets* :test #'coord-equal)
                       conquest-planet)
               (setf *conquest-quadrant* nil)))))
      ;; Klingon reproduces
      ((= event-code +klingons-build-ship-in-enslaved-system+)
       (let ((conquest-planet
               (rest (assoc *conquest-quadrant* *planets* :test #'coord-equal)))
             (build-quadrant nil))
         ;; TODO should this status change when the last klingon in the quadrant is destroyed?
         ;; see if current distress call still active
         (if (> (quadrant-klingons (coord-ref *galaxy* *conquest-quadrant*)) 0)
             (progn
               (schedule-event +klingons-build-ship-in-enslaved-system+ (* 1.0 *initial-time*))
               (unless (>= *remaining-klingons* +max-klingons-per-game+) ; full right now
                 ;; reproduce one Klingon
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
                             (setf build-quadrant (make-quadrant-coordinate :x i :y j)))))))
                 (when build-quadrant
                   ;; deliver the child
                   (setf *remaining-klingons* (1+ *remaining-klingons*))
                   (setf (quadrant-klingons (coord-ref *galaxy* build-quadrant))
                         (1+ (quadrant-klingons (coord-ref *galaxy* build-quadrant))))
                   (when (coord-equal *ship-quadrant* build-quadrant)
                     (incf *enemies-here* 1)
                     (incf *klingons-here* 1)
                     (multiple-value-bind (coordinates distance power) (drop-klingon-in-sector)
                       ;; TODO - this is wrong if a Tholian is the last item in the array
                       (setf (aref *quadrant-enemies* *enemies-here*)
                             (make-enemy :energy power
                                         :distance distance
                                         :average-distance distance
                                         :sector-coordinates coordinates)))
                     (sort-klingons))
                   ;; recompute time left (ported directly from the C source)
                   (setf *remaining-time* (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
                                              (/ *remaining-resources* (+ *remaining-klingons*
                                                                          (* 4 (length *commander-quadrants*))))
                                              99))
                   ;; report the disaster if we can
                   (cond
                     ((and (coord-equal *ship-quadrant* build-quadrant)
                           (not (damagedp +short-range-sensors+)))
                      (print-message *message-window*
                       (format nil "Spock- sensors indicate the Klingons have launched a warship from ~A."
                               (planet-name conquest-planet))))
                     ((subspace-radio-available-p)
                      (print-message *message-window*
                       (format nil "Lt. Uhura- Starfleet reports increased Klingon activity near ~A in ~A."
                               (planet-name conquest-planet)
                               (format-quadrant-coordinates build-quadrant))))))))
           (progn
             (setf (planet-status conquest-planet) 'secure)
             (rplacd (assoc *conquest-quadrant* *planets* :test #'coord-equal)
                     conquest-planet)
             (setf *conquest-quadrant* nil))))))))

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
    (print-out *game-status-window* (format nil "Stardate ~A~%" (format-stardate *stardate*))))
  (when (= line-to-print 2)
    (print-out *game-status-window*
               (format nil "Klingons Left ~A~%" (+ *remaining-klingons*
                                                   (length *commander-quadrants*)
                                                   *remaining-super-commanders*))))
  (when (= line-to-print 3)
    (print-out *game-status-window* (format nil "Time Left ~,2,,,F~%" *remaining-time*))))

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
    ;; TODO - update the player condition every turn, not only when checking status
    (update-condition)
    (print-out *ship-status-window* "Condition ")
    (print-out *ship-status-window* *condition*)
    (when (> (damaged-device-count) 0)
      (print-out *ship-status-window* (format nil ", ~A DAMAGE~:@(~:P~)" (damaged-device-count))))
    (skip-line *ship-status-window*))
  (when (= line-to-print 2)
    (print-out *ship-status-window*
               (format nil "Position ~A , ~A"
                       (format-coordinates *ship-quadrant*) (format-coordinates *ship-sector*)))
    ;; Print flight status with position
    (cond
      (*dockedp*
       (print-out *ship-status-window* ", Docked"))
      (*in-orbit-p*
       (print-out *ship-status-window* ", In Orbit")))
    (skip-line *ship-status-window*))
  (when (= line-to-print 3)
    (if (damagedp +cloaking-device+)
        (print-out *ship-status-window* (format nil "Cloaking Device DAMAGED~%"))
        (if *cloakedp*
            (print-out *ship-status-window* (format nil "Cloaked~%"))
            (print-out *ship-status-window* (format nil "Not cloaked~%")))))
  (when (= line-to-print 4)
    (print-out *ship-status-window* "Life Support ")
    (if (damagedp +life-support+)
        (progn
          (print-out *ship-status-window* "DAMAGED, ")
          (if *dockedp*
              (print-out *ship-status-window* (format nil "Base provides~%"))
              (print-out *ship-status-window*
                         (format nil "reserves=~4,2F~%" *life-support-reserves*))))
        (print-out *ship-status-window* (format nil "Active~%"))))
  (when (= line-to-print 5)
    (print-out *ship-status-window* (format nil "Warp Factor ~,1F~%" *warp-factor*)))
  (when (= line-to-print 6)
    (print-out *ship-status-window* (format nil "Energy ~,1F" *ship-energy*))
    (when *dilithium-crystals-on-board-p*
      (print-out *ship-status-window* " (have crystals)"))
    (skip-line *ship-status-window*))
  (when (= line-to-print 7)
    (print-out *ship-status-window* (format nil "~34@<Torpedoes ~A~>" *torpedoes*))
    (if (eql *ship-status-window* *message-window*)
        (game-status 1)
        (skip-line *ship-status-window*)))
  (when (= line-to-print 8)
    (print-out *ship-status-window* (format nil "~34@<Shields ~A, ~D%, ~,1F units~;~>"
                       (cond
                         ((damagedp +shields+)
                          "DAMAGED")
                         (*shields-are-up-p*
                          "UP")
                         ((not *shields-are-up-p*)
                          "DOWN"))
                       (round (/ (* 100.0 *shield-energy*) *initial-shield-energy*))
                       *shield-energy*))
    (if (eql *ship-status-window* *message-window*)
        (game-status 2)
        (skip-line *ship-status-window*)))
  (when (= line-to-print 9)
    (print-out *ship-status-window*
               (format nil "~34@<Probe~A~>"
                       (if (is-scheduled-p +move-deep-space-probe+)
                           (format nil " in ~A"
                                   (format-quadrant-coordinates
                                    (make-quadrant-coordinate
                                     :x (galaxy-sector-to-quadrant (probe-x *probe*))
                                     :y (galaxy-sector-to-quadrant (probe-y *probe*)))))
                           (format nil "s ~A" *probes-available*))))
    (if (eql *ship-status-window* *message-window*)
        (game-status 3)
        (skip-line *ship-status-window*)))
  (when (= line-to-print 11)
    (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
      (if (and p ; p is nil when there is no planet in the quadrant
               (not (planet-inhabitedp p)))
          (print-out *ship-status-window* (format nil "Major system ~A~%" (planet-name p)))
          (print-out *ship-status-window* (format nil "Sector is uninhabited~%")))))
  (when (= line-to-print 12)
    (when (and (is-scheduled-p +commander-destroys-base+)
               (not *base-under-attack-quadrant*))
      (print-out *ship-status-window* (format nil "Base in ~A attacked by C. Alive until ~A~%"
                         (format-coordinates *base-under-attack-quadrant*)
                         (format-stardate (find-event +commander-destroys-base+)))))
    (when (or (eql *super-commander-attacking-base* 'attacking)
              (eql *super-commander-attacking-base* 'destroying))
      (print-out *ship-status-window* (format nil "Base in ~A attacked by S. Alive until ~A~%"
                         (format-coordinates *super-commander-quadrant*)
                         (format-stardate (find-event +super-commander-destroys-base+)))))))

(defun all-statuses ()
  "Display all the ship statuses that are displayed next to the short range scan. In
line-by-line mode also display the game statuses that are displayed next to the short
range scan."

    (if (eql *ship-status-window* *message-window*)
	(skip-line *ship-status-window*)
	(clear-window *ship-status-window*))

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
  (unless (eql *game-status-window* *message-window*)
    (clear-window *game-status-window*)
    (game-status 1)
    (game-status 2)
    (game-status 3)))

(defun sector-scan (i j) ; C: static void sectscan(int goodScan, int i, int j)
  "Light up an individual dot in a sector."

  ;; Always show the sectors immediately adjacent to the ship
  (if (or (not (damagedp +short-range-sensors+))
          (and (<= (abs (- i (coordinate-x *ship-sector*))) 1)
               (<= (abs (- j (coordinate-y *ship-sector*))) 1)))
      (progn
        (when (or (string= (aref *quadrant* i j) +materialize-1+)
                  (string= (aref *quadrant* i j) +materialize-2+)
                  (string= (aref *quadrant* i j) +materialize-3+)
                  (string= (aref *quadrant* i j) +enterprise+)
                  (string= (aref *quadrant* i j) +faerie-queene+))
          (cond
            ((string= *condition* +red-status+)
             (set-text-color *short-range-scan-window* +red+))
            ;; If docked then Condition Yellow is less serious than when not docked
            (*dockedp*
             (set-text-color *short-range-scan-window*  +cyan+))
            ((string= *condition* +yellow-status+)
             (set-text-color *short-range-scan-window*  +yellow+))
            ((string= *condition* +green-status+)
             (set-text-color *short-range-scan-window*  +green+))
            ((not *alivep*)
             (set-text-color *short-range-scan-window*  +brown+))
            (t
             (set-text-color *short-range-scan-window*  +default-color+)))
          (when (string= (aref *quadrant* i j) *ship*)
            (toggle-reverse-video *short-range-scan-window*)))
        ;; Determine which symbol to display and then display it
        (let ((symbol (aref *quadrant* i j)))
          (when *probe*
            ;; If the probe is in the current quadrant and in the sector to be displayed, and the
            ;; sector it occupies is empty (including no black hole - probes get to avoid black
            ;; holes) then display the probe symbol.
            (when (and (coord-equal *ship-quadrant* (make-quadrant-coordinate
                                                     :x (galaxy-sector-to-quadrant (probe-x *probe*))
                                                     :y (galaxy-sector-to-quadrant (probe-y *probe*))))
                       (= i (galaxy-sector-to-local-sector (probe-x *probe*)))
                       (= j (galaxy-sector-to-local-sector (probe-y *probe*)))
                       (string= symbol +empty-sector+))
              (setf symbol +probe+)))
          (print-out *short-range-scan-window* (format nil "~A" symbol)))
        (set-text-color *short-range-scan-window*  +default-color+)
        (print-out *short-range-scan-window* " "))
      (print-out *short-range-scan-window* "  ")))

(defgeneric short-range-scan (output-window)
  (:documentation "Display a short range scan in the specified window."))

(defmethod short-range-scan ((output-window screen))
  "Display a short range scan in a line by line format."

  (skip-line output-window))

(defmethod short-range-scan ((output-window window))
  "Display a short range scan using curses commands."

  (clear-window output-window))

;; TODO - this uses lisp syntax to get the desired result but it seems like the wrong thing to
;;        have the most important part of the scan done in an :after function.
(defmethod short-range-scan :after ((output-window screen))
  "Short range scan output for all window types. This is the informational part of the scan."

  (when (or *dockedp*
            (not (damagedp +short-range-sensors+)))
    (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*)))

  ;; Print a short range scan header
  (if (damagedp +short-range-sensors+)
      (if *dockedp*
          (print-out output-window (format nil " [Using Base's sensors]~%"))
          (print-out output-window (format nil "  S.R. SENSORS DAMAGED!~%")))
      (print-out output-window (format nil "     SHORT-RANGE SCAN~%")))
  (print-out output-window (format nil "   1 2 3 4 5 6 7 8 9 10~%"))
  (do ((i 0 (1+ i)))
      ((>= i +quadrant-size+))
    (print-out output-window (format nil "~2@A " (1+ i)))
    (do ((j 0 (1+ j)))
        ((>= j +quadrant-size+))
      (sector-scan i j))
    ;; When there is a separate short range scan window then just end the line. Otherwise, the
    ;; next line segment of the current line is the ship status, (which will in turn print the
    ;; appropriate game status on the current line).
    (if (eql *short-range-scan-window* *message-window*)
        (progn
          (print-out output-window " ")
          (ship-status (1+ i)))
        (skip-line output-window))))

(defun long-range-scan ()
  "Scan the galaxy using long-range sensors and update the star chart. Display the results of the
scan. Long-range sensors can scan all adjacent quadrants."

  (if (eql *long-range-scan-window* *message-window*)
      (skip-line *long-range-scan-window*)
      (clear-window *long-range-scan-window*))

  (print-out *long-range-scan-window* (format nil "LONG-RANGE SCAN~%"))
  (if (or (not (damagedp +long-range-sensors+))
           *dockedp*)
      (progn
        (when (damagedp +long-range-sensors+)
            (print-out *long-range-scan-window* (format nil "Starbase's sensors~%")))
        (do ((x (- (coordinate-x *ship-quadrant*) 1) (1+ x)))
            ((> x (+ (coordinate-x *ship-quadrant*) 1)))
          (print-out *long-range-scan-window* " ")
          (do ((y (- (coordinate-y *ship-quadrant*) 1) (1+ y)))
              ((> y (+ (coordinate-y *ship-quadrant*) 1)))
            (if (valid-quadrant-p x y)
                (progn
                  (update-chart x y)
                  (if (quadrant-supernovap (aref *galaxy* x y))
                      (print-out *long-range-scan-window* " ***")
                      (print-out *long-range-scan-window*
                                 (format nil "~4D" (+ (* (starchart-page-klingons (aref *starchart* x y)) 100)
                                                      (* (starchart-page-starbases (aref *starchart* x y)) 10)
                                                      (starchart-page-stars (aref *starchart* x y)))))))
                (print-out *long-range-scan-window* (format nil "~4D" -1))))
          (skip-line *long-range-scan-window*)))
      (print-out *long-range-scan-window* (format nil "SENSORS DAMAGED~%"))))

(defun update-windows () ; C: void drawmaps(void)
  "Perform the automatic display updates available to curses-enabled terminals."

  ;; If there is a short range scan window then update all the default windows
  (unless (eql *short-range-scan-window* *message-window*)
    (short-range-scan *short-range-scan-window*)
    (all-statuses)
    (long-range-scan)
    ;; In curses mode sensors work automatically. Call before updating the display, but only once
    ;; per entry into the quadrant.
    (when *just-in-p*
      (sensor)))
  (unless (eql *starchart-window* *message-window*)
    (chart))
  (unless (eql *damage-report-window* *message-window*)
      (damage-report))
  (unless (eql *planet-report-window* *message-window*)
      (survey))
  (unless (eql *score-window* *message-window*)
      (score)))

(defun print-prompt (prompt-to-print)
  "Print a string. In curses mode print it in the prompt window, otherwise just print it."

  (if (eql *prompt-window* *message-window*)
      (skip-line *prompt-window*)
      (clear-window *prompt-window*))

  (print-out *prompt-window* prompt-to-print))

;; TODO - move-coordinate is effective when the calling function has displaced the enemy just
;;        before destroying them, e.g. nova buffet or photon torpedo displacedment. Can the move
;;        be completed by the calling function before killing the enemy? Black holes are a problem
;;        because they should not be replaced by an empty sector.
(defun dead-enemy (enemy-coord enemy move-coordinate) ; C: deadkl(coord w, feature type, coord mv)
  "Kill a Klingon, Tholian, Romulan, or Thingy. move-coordinate allows enemy to move before dying."

  ;; move-coordinate allows an enemy to "move" before dying
  (print-message *message-window* (format nil "~A at ~A" (letter-to-name enemy) (format-sector-coordinates move-coordinate)))
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
     (decf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 1)
     (decf *klingons-here* 1)
     (decf *commanders-here* 1)
     (setf *commander-quadrants* (remove *ship-quadrant* *commander-quadrants* :test #'coord-equal))
     (unschedule-event +tractor-beam+)
     (when (> (length *commander-quadrants*) 0)
       (schedule-event +tractor-beam+ (expran (/ *initial-commanders* (length *commander-quadrants*))))))

    ((string= enemy +klingon+)
     (decf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 1)
     (decf *klingons-here* 1)
     (decf *remaining-klingons* 1))

    ((string= enemy +super-commander+)
     (decf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 1)
     (decf *klingons-here* 1)
     (decf *remaining-super-commanders* 1)
     (decf *super-commanders-here* 1)
     (setf *super-commander-quadrant* nil)
     (setf *super-commander-attacking-base* 'not-attacking)
     (setf *super-commander-attack-enterprise-p* nil)
     (unschedule-event +move-super-commander+)
     (unschedule-event +super-commander-destroys-base+)))

  ;; For each kind of enemy, finish message to player
  (print-message *message-window* (format nil " destroyed.~%"))
  (setf (coord-ref *quadrant* enemy-coord) +empty-sector+)
  (update-chart (coordinate-x *ship-quadrant*) (coordinate-y *ship-quadrant*))
  (when (> (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
    (if (> (+ *remaining-klingons* (length *commander-quadrants*)) 0)
        (setf *remaining-time* (/ *remaining-resources* (+ *remaining-klingons* (* 4 (length *commander-quadrants*)))))
        (setf *remaining-time* 99))
    ;; Show the new remaining time immediately
    (unless (eql *game-status-window* *message-window*)
      (clear-window *game-status-window*)
      (game-status 1)
      (game-status 2)
      (game-status 3))
    ;; Remove enemy ship from arrays describing local conditions
    (when (and (is-scheduled-p +commander-destroys-base+)
               (coord-equal *base-under-attack-quadrant* *ship-quadrant*)
               (string= enemy +commander+))
      (unschedule-event +commander-destroys-base+))
    (do ((i 0 (1+ i))) ; find the enemy sector
        ((or (> i *enemies-here*)
             (coord-equal (enemy-sector-coordinates (aref *quadrant-enemies* i)) enemy-coord))
         (decf *enemies-here* 1)
         (when (< i *enemies-here*)
           ;; TODO - apply this shift to all code that removes an enemy from the array, it
           ;;        preserves the special location (last item in the array) of the Tholian
           (do ((j i (1+ j))) ; shift all elements towards the start of the array
               ((>= j *enemies-here*))
             (setf (aref *quadrant-enemies* j) (aref *quadrant-enemies* (1+ j))))))))
  (update-condition))

(defun apply-critical-hit (hit) ; C: void fry(double hit)
  "Apply a critical hit."

  ;; a critical hit occured
  (when (>= hit (* (- 275.0 (* 25.0 *skill-level*) (+ 1.0 (* 0.5 (random 1.0))))))
    (print-message *message-window* (format nil "***CRITICAL HIT--~%"))
    ;; Select devices and cause damage
    (do ((hit-count 0 (1+ hit-count)) ; C: loop1 (really?)
         (number-of-hits (truncate (+ 1.0 (/ hit (+ 500.0 (* 100.0 (random 1.0))))))) ; C: ncrit
         (devices-damaged (make-array +number-of-devices+)))
        ((>= hit-count number-of-hits)
         ;; Display damaged devices
         (do ((i 0 (1+ i)))
             ((>= i number-of-hits))
           (print-message *message-window* (format nil "~A " (aref *devices* (aref devices-damaged i))))
           (when (and (> number-of-hits 1)
                      (= i (- number-of-hits 2)))
             (print-message *message-window* "and "))
           )
         (print-message *message-window* (format nil "damaged.~%")))
      ;; Select a random device
      (do ((device-index nil))
          (device-index
           ;; Record which device was damaged so the name can be displayed later
           (setf (aref devices-damaged hit-count) device-index)
           ;; Damage the device
           (incf (aref *device-damage* device-index) (/ (* hit *damage-factor*)
                                                        (* number-of-hits
                                                           (+ 75.0
                                                              (* 25.0 (random 1.0)))))))
        (setf device-index (get-random-device))
        ;; Cheat to prevent shuttle damage unless on ship
        (when (or (< (aref *device-damage* device-index) 0.0) ; TODO -10 is a special shuttle damage value used when it is not on the ship
                  (and (= device-index +shuttle+)
                       (not (eql *shuttle-craft-location* 'on-ship))))
          (setf device-index nil)))))
    (when (and (damagedp +shields+)
               *shields-are-up-p*)
      (print-message *message-window* (format nil "***Shields knocked down.~%"))
      (setf *shields-are-up-p* nil))
  (when (and (damagedp +cloaking-device+)
             *cloakedp*)
    (print-message *message-window* (format nil "***Cloaking device rendered inoperative.~%"))
    (setf *cloakedp* nil))
  (skip-line *message-window*))

(defun check-for-phasers-overheating (requested-energy) ; C: void overheat(double rpow)
  "Check for phasers overheating"

  (when (> requested-energy +max-safe-phaser-power+)
    (let ((burn (* (- requested-energy +max-safe-phaser-power+) 0.00038))) ; TODO - name the constant
      (when (<= (random 1.0) burn)
        (print-message *message-window* (format nil "Weapons officer Sulu-  \"Phasers overheated, sir.\"~%"))
        (setf (aref *device-damage* +phasers+) (* *damage-factor* (+ 1 (random 1.0)) (+ 1.0 burn)))))))

(defun energy-to-kill-enemy (i)
  "Calculate the amount of phaser energy needed to kill an enemy in the enemies list. i is the
index of the enemy in the global array. This is the energy needed without any 'fuzz factors'
applied."

  (return-from energy-to-kill-enemy (/ (abs (enemy-energy (aref *quadrant-enemies* i)))
                                       (* +phaser-factor+
                                          (expt 0.90
                                                (enemy-distance (aref *quadrant-enemies* i)))))))

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
      (incf rec (recommended-energy-for-enemy i)))
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
        (skip-line *message-window*)
        (if (< (random 1.0) 0.998)
            (progn
              (print-message *message-window* (format nil "Shields lowered.~%"))
              (setf *shields-are-up-p* nil)
              (return-from toggle-high-speed-shield-control t))
            (progn
              ;; Something bad has happened
              (print-message *message-window*
                             (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
              (skip-line *message-window* 2)
              (let* ((energy-hit (/ (* phaser-energy *shield-energy*) *initial-shield-energy*))
                     (casualties (truncate (* energy-hit (random 1) 0.012))))
                (decf *ship-energy* (+ phaser-energy (* energy-hit 0.8)))
                (decf *shield-energy* (* energy-hit 0.2))
                (when (<= *ship-energy* 0.0)
                  (print-message
                   *message-window*
                   (format nil "Sulu-  \"Captain! Shield malf***********************\"~%")
                                 :print-slowly t)
                  (skip-line *message-window*)
                  (print-stars)
                  (finish 'phaser-fire-inside-shields)
                  (return-from toggle-high-speed-shield-control nil))
                (print-message
                 *message-window*
                 (format nil "Sulu-  \"Captain! Shield malfunction! Phaser fire contained!\"~%")
                 :print-slowly t)
                (skip-line *message-window* 2)
                (print-message
                 *message-window*
                 (format nil "Lt. Uhura-  \"Sir, all decks reporting damage.\"~%"))
                (skip-line *message-window*)
                (apply-critical-hit (* 0.8 energy-hit))
                (when (> casualties 0)
                  (print-message
                   *message-window*
                   (format nil "~%McCoy to bridge- \"Severe radiation burns, Jim.~%"))
                  (print-message
                   *message-window* (format nil "  ~A casualties so far.\"~%" casualties))
                  (incf *casualties* casualties)
                  (decf *crew* casualties)))
              (print-message *message-window*
                             (format nil "~%Phaser energy dispersed by shields.~%"))
              (print-message *message-window* (format nil "Enemy unaffected.~%"))
              (check-for-phasers-overheating phaser-energy)
              (return-from toggle-high-speed-shield-control nil))))
      ;; Raise shields
      (if (< (random 1.0) 0.99)
          (progn
            (print-message *message-window* (format nil "Shields raised.~%"))
            (setf *shields-are-up-p* t)
            (return-from toggle-high-speed-shield-control t))
          (progn
            (print-message
             *message-window*
             (format nil "Sulu-  \"Sir, the high-speed shield control has malfunctioned . . .~%"))
            (print-message *message-window*
                           (format nil "         CLICK   CLICK   POP  . . .~%") :print-slowly t)
            (print-message *message-window* (format nil " No response, sir!~%"))
            (setf *shields-are-up-p* nil)
            (return-from toggle-high-speed-shield-control nil)))))

(defun apply-phaser-hits (hits) ; C: hittem(double *hits)
  "Apply phaser hits to Klingons and Romulans."

  (do ((hit-index 0 (1+ hit-index))
       (enemy-index 0 (1+ enemy-index))
       (dust-factor (+ 0.9 (* 0.01 (random 1.0))) ; amount by which delivered power is reduced over distance
                    (+ 0.9 (* 0.01 (random 1.0)))) ; different for each enemy
       hit ; The hit amount applied to the current enemy
       initial-enemy-energy
       enemy-energy
       (e-coord (make-sector-coordinate))) ; convenience variable
      ((>= hit-index (length hits)))
    (skip-line *message-window*)
    (when (> (aref hits hit-index) 0)
      (setf hit (* (aref hits hit-index)
                   (expt dust-factor (enemy-distance (aref *quadrant-enemies* enemy-index)))))
      (setf initial-enemy-energy (enemy-energy (aref *quadrant-enemies* enemy-index)))
      (setf enemy-energy (abs initial-enemy-energy)) ; apparently, enemy energy can be negative
      (when (< (* +phaser-factor+ hit) enemy-energy)
        (setf enemy-energy (* +phaser-factor+ hit)))
      (if (< (enemy-energy (aref *quadrant-enemies* enemy-index)) 0)
          (decf (enemy-energy (aref *quadrant-enemies* enemy-index)) (- enemy-energy))
          (decf (enemy-energy (aref *quadrant-enemies* enemy-index)) enemy-energy))
      (setf e-coord (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index)))
      (if (> hit 0.005)
          (progn
            (unless (damagedp +short-range-sensors+)
              (boom *short-range-scan-window*
                    (coord-ref *quadrant* e-coord) (coordinate-x e-coord) (coordinate-y e-coord)))
            (print-message *message-window* (format nil "~A unit hit on ~A at ~A~%" (truncate hit)
                                   (letter-to-name (coord-ref *quadrant* e-coord))
                                   (format-sector-coordinates e-coord))))
          (print-message *message-window* (format nil "Very small hit on ~A at ~A~%"
                                 (letter-to-name (coord-ref *quadrant* e-coord))
                                 (format-sector-coordinates e-coord))))
      (when (string= (coord-ref *quadrant* e-coord)
                     +thing+)
        (setf *thing-is-angry-p* t))
      (if (= (enemy-energy (aref *quadrant-enemies* enemy-index)) 0)
          (progn
            (dead-enemy e-coord (coord-ref *quadrant* e-coord)
                        e-coord)
            (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
              (finish 'won))
            (when *all-done-p*
              (return-from apply-phaser-hits t))
            (decf enemy-index 1)) ; don't do the increment, enemy array has one less klingon
          ;; decide whether or not to emasculate klingon
          (when (and (> (enemy-energy (aref *quadrant-enemies* enemy-index)) 0)
                     (>= (random 1.0) 0.9)
                     (<= (enemy-energy (aref *quadrant-enemies* enemy-index))
                         (+ 0.4 (* 0.4 (random 1.0) initial-enemy-energy))))
            (print-message *message-window* (format nil "***Mr. Spock-  \"Captain, the vessel at ~A~%"
                                   (format-sector-coordinates e-coord)))
            (print-message *message-window* (format nil "   has just lost its firepower.\"~%"))
            (setf (enemy-energy (aref *quadrant-enemies* enemy-index)) 0))))))

(defun fire-phasers () ; C: phasers()

  (skip-line *message-window*)
  (when *dockedp*
    (print-message *message-window* (format nil "Phasers can't be fired through base shields.~%"))
    (return-from fire-phasers nil))
  (when (damagedp +phasers+)
    (print-message *message-window* (format nil "Phaser control damaged.~%"))
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
        (print-message *message-window* (format nil "High speed shield control damaged.~%"))
        (return-from fire-phasers nil))
      (when (<= *ship-energy* 200.0)
        (print-message *message-window* (format nil "Insufficient energy to activate high-speed shield control.~%"))
        (return-from fire-phasers nil))
      (print-message *message-window*
       (format nil "Weapons Officer Sulu-  \"High-speed shield control enabled, sir.\"~%"))
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
    (do ((token-count 0 (1+ token-count))
         input-item)
        ((> token-count 2)
         (unless fire-mode
           (huh)
           (return-from fire-phasers nil)))
      (if (input-available-p)
          (progn
            (setf input-item (scan-input))
            (cond
              ((numberp input-item)
               (if (eql fire-mode 'manual)
                   (unscan-input input-item) ; Don't read manual mode energy until the firing loop.
                   (unless requested-energy ; If energy was already provided then don't overwrite it.
                     (setf fire-mode 'automatic)
                     (setf requested-energy input-item))))

              ((match-token input-item (list "automatic"))
               (setf fire-mode 'automatic))

              ((and (not fire-mode)
                    (match-token input-item (list "manual")))
               (setf fire-mode 'manual))

              ((match-token input-item (list "no"))
               (setf raise-shields-p nil))

              (t
               (huh)
               (return-from fire-phasers nil))))
          (unless fire-mode
            (print-prompt "Manual or automatic? ")
            (setf input-item (scan-input))
            (cond
              ((or (not input-item); no input, player typed enter/return
                   (match-token input-item (list "automatic")))
               (setf fire-mode 'automatic))

              ((match-token input-item (list "manual"))
               (setf fire-mode 'manual))

              (t
               (huh)
               (return-from fire-phasers nil))))))

    (when (and (eql fire-mode 'automatic)
               (not targeting-support-available-p))
      (setf fire-mode 'force-manual))

    (when (= *enemies-here* 0)
      (when (or (eql fire-mode 'manual)
                (eql fire-mode 'force-manual))
        (print-message *message-window* (format nil "There is no enemy present to select.~%"))
        (clear-type-ahead-buffer)
        (setf fire-mode 'automatic)) ; In automatic mode a single value is input
      (print-message *message-window* (format nil "Energy will be expended into space.~%")))

    (if (eql fire-mode 'automatic)
        (progn
          (when (and (not requested-energy)
                     (> *enemies-here* 0))
            (print-message *message-window* "Phasers locked on target. "))
          (do (input-item)
              ((and requested-energy
                    (< requested-energy available-energy)))
            (print-message *message-window* (format nil "Energy available= ~,2F~%" available-energy))
            (clear-type-ahead-buffer)
            (print-prompt (format nil "~D units required. Units to fire: " (ceiling (recommended-energy))))
            (setf input-item (scan-input))
            (unless (numberp input-item)
              (return-from fire-phasers nil))
            (setf requested-energy input-item))
          (when (<= requested-energy 0)
            ;; chicken out
            (return-from fire-phasers nil))
          (when (and shield-control-available-p
                     *shields-are-up-p*)
            (decf *ship-energy* 200) ; Go and do it!
            (unless (toggle-high-speed-shield-control requested-energy)
              ;; High-speed shield control failed, phaser fire process interrupted
              (return-from fire-phasers nil)))
          ;; Fire!
          (decf *ship-energy* requested-energy)
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
                  (decf remaining-energy (+ (aref hits i) over))
                  (when (and (<= remaining-energy 0.0)
                             (< temp (aref hits i)))
                    (setf (aref hits i) temp)) ; Use up all remaining energy
                  (when (<= remaining-energy 0.0)
                    (setf over 0.0))
                  (incf excess-energy over)))
              (when (> remaining-energy 0.0)
                (incf excess-energy remaining-energy))
              (apply-phaser-hits hits)
              (setf *action-taken-p* t))
            (when (and (> excess-energy 0)
                       (not *all-done-p*))
              (skip-line *message-window*)
              (if (> *tholians-here* 0)
                  (progn
                    (print-message *message-window* "*** Tholian web absorbs ")
                    (when (> *enemies-here* 0)
                      (print-message *message-window* "excess "))
                    (print-message *message-window* (format nil "phaser energy.~%")))
                  (print-message *message-window* (format nil "~,2F units expended on empty space.~%" excess-energy))))))
        (progn
          ;; If manual fire was forced then explain why
          (when (eql fire-mode 'force-manual)
            (clear-type-ahead-buffer)
            (if (damagedp +computer+)
                (print-message *message-window* (format nil "Battle computer damaged, manual fire only.~%"))
                (progn
                  (print-message *message-window* (format nil "~%---WORKING---~%") :print-slowly t)
                  (print-message *message-window* (format nil "~%Short-range-sensors-damaged~%"))
                  (print-message *message-window* (format nil "Insufficient-data-for-automatic-phaser-fire~%"))
                  (print-message *message-window* (format nil "Manual-fire-must-be-used~%~%")))))
          ;; Allow manual fire even when the short-range sensors are damaged. If the short-range
          ;; sensors are damaged and the enemy is a Commander, Super-commander, or Romulan then
          ;; they can't be fired on unless they are adjacent to the ship.
          ;; TODO - write the enhancements to the visual-scan function and then allow firing on
          ;;        Commanders, Super-commanders, and Romulan) that are detected by the visual scan
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
               (decf *ship-energy* requested-energy)
               (clear-type-ahead-buffer)
               (when (and shield-control-available-p
                          *shields-are-up-p*)
                 (decf *ship-energy* 200) ; TODO - name the constant
                 (unless (toggle-high-speed-shield-control requested-energy)
                   (return-from fire-phasers nil)))
               (apply-phaser-hits hits)
               (setf *action-taken-p* t))
            (setf enemy-coord (enemy-sector-coordinates (aref *quadrant-enemies* current-enemy)))
            (setf enemy (coord-ref *quadrant* enemy-coord))
            (when display-available-energy-p
              (print-message *message-window* (format nil "Energy available= ~,2F~%" (- available-energy 0.006))) ; what is this constant?
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
                  (print-message *message-window*
                                 (format nil "~A can't be located without short range scan.~%"
                                         (letter-to-name enemy)))
                  (clear-type-ahead-buffer)
                  (setf (aref hits current-enemy) 0) ; prevent overflow -- thanks to Alexei Voitenko
                  (incf current-enemy 1))
                (progn
                  (unless (input-available-p)
                    (print-prompt
                     (format nil "(~A) units to fire at ~A at ~A: "
                             (if targeting-support-available-p
                                 (format nil "~D"
                                         (truncate (recommended-energy-for-enemy current-enemy)))
                                 "??")
                             (letter-to-name enemy)
                             (format-sector-coordinates enemy-coord))))
                  (let ((input-item (scan-input)))
                    (unless (numberp input-item)
                      (huh)
                      (return-from fire-phasers nil))
                    (when (< input-item 0)
                      (clear-type-ahead-buffer)
                      (return-from fire-phasers nil))
                    (setf (aref hits current-enemy) input-item)
                    (incf requested-energy input-item))
                  ;; If total requested is too much, inform and start over
                  (if (> requested-energy available-energy)
                      (progn
                        (print-message *message-window*
                                       (format nil "Available energy exceeded -- try again.~%"))
                        (clear-type-ahead-buffer)
                        (setf current-enemy 0)
                        (setf display-available-energy-p t))
                      (progn
                        (incf current-enemy 1))))))))
    ;; Say shield raised or malfunction, if necessary
    (unless *all-done-p*
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

  (setf *time-taken-by-current-operation* 0.0)

  (cond
    ;; Make sure there is room in the brig
    ((= *brig-free* 0)
     (print-message *message-window* (format nil "Security reports the brig is already full.~%"))
     (return-from capture nil))

    ((or *cloakedp*
         (damagedp +subspace-radio+))
     (print-message *message-window* (format nil "Uhura- \"We have no subspace radio communication, sir.\"~%"))
     (return-from capture nil))

    ((damagedp +transporter+)
     (print-message *message-window* (format nil "Scotty- \"Transporter damaged, sir.\"~%"))
     (return-from capture nil))
    ;; find out if there are any at all
    ((< *klingons-here* 1)
     (print-message *message-window* (format nil "Uhura- \"Getting no response, sir.\"~%"))
     (return-from capture nil))
    (t
     (setf *time-taken-by-current-operation* 0.05) ; This action will take some time
     (setf *action-taken-p* t) ; So any others can strike back
     ;; If there is more than one Klingon, find out which one
     (let ((klingon-index (select-klingon-for-capture)) ; C: k
           k-coord
           x) ; C: x
       ;; Check out that Klingon
       (setf k-coord (enemy-sector-coordinates (aref *quadrant-enemies* klingon-index)))
       ;; The algorithm isn't that great and could use some more intelligent design
       ;; (setf x (+ 300 (* 25 *skill-level*))) - just use ship energy for now
       ;; Multiplier would originally have been equivalent of 1.4, but we want the command to work more often, more humanely
       (setf x (* (/ *ship-energy* (* (enemy-energy (aref *quadrant-enemies* klingon-index))
                                      *enemies-here*))
                  2.5))
       (if (<= x (* (random 1.0) 100))
           ;; Big surprise, he refuses to surrender
           (progn
             (print-message *message-window* (format nil "Fat chance, captain~%"))
             (return-from capture nil)))
       ;; Guess what, he surrendered!!!
           (progn
             (print-message *message-window* (format nil "Klingon captain at ~A surrenders~%" (format-coordinates k-coord)))
             ;; Assume a crew of 200 on the Klingon ship
             (let ((klingons-captured (round (* 200 (random 1.0))))) ; C: i
               (when (> klingons-captured 0)
                 (print-message *message-window* (format nil "~D Klingons commit suicide rather than be taken captive.~%"
                                        (- 200 klingons-captured))))
               (when (and (> klingons-captured *brig-free*)
                          (not *dockedp*))
                 (print-message *message-window* (format nil "~D Klingons die because there is no room for them in the brig~%"
                                        (- klingons-captured *brig-free*)))
                 (setf klingons-captured *brig-free*))
               (if *dockedp*
                   (progn
                     (incf *captured-klingons* klingons-captured)
                     (print-message *message-window* (format nil "~D captives taken and transferred to base~%"
                                            klingons-captured)))
                   (progn
                     (decf *brig-free* klingons-captured)
                     (print-message *message-window* (format nil "~D captives taken~%" klingons-captured)))))
             (dead-enemy k-coord (coord-ref *quadrant* k-coord) k-coord)
             (when (= *remaining-klingons* 0)
               (finish 'won)))))))

(defun select-klingon-for-capture () ; C: int selectklingon()
  "Cruddy, just takes one at random.  Should ask the captain.
Nah, just select the weakest one since it is most likely to
surrender (Tom Almy mod)"

  (let ((klingon-index 0)
        (klingon-energy (enemy-energy (aref *quadrant-enemies* 0))))
    ;; Select the weakest one
    (do ((i 1 (1+ i))) ; C: int j
        ((>= i *enemies-here*))
      (unless (string= (coord-ref *quadrant*
                                  (enemy-sector-coordinates (aref *quadrant-enemies* i)))
                       +romulan+) ; No Romulans surrender
        (when (> (enemy-energy (aref *quadrant-enemies* i)) klingon-energy)
          (setf klingon-index i)
          (setf klingon-energy (enemy-energy (aref *quadrant-enemies* i))))))
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
    (return-from displace-ship (make-sector-coordinate :x (round (+ x-coord delta-x))
                                                       :y (round (+ y-coord delta-y))))))

(defun calculate-torpedo-damage (torpedo-origin torpedo-coord hit-angle)
  "Damage done by a torpedo depends on the distance it has travelled and the angle at which it hits."

  (abs (- (+ 700.0 (* 100.0 (random 1.0)))
          (* 1000.0
             (distance torpedo-origin torpedo-coord)
             (abs (sin hit-angle))))))

;; C: void tracktorpedo(coord w, int l, int i, int n, int iquad)
(defgeneric track-torpedo (output-window
                           torp-x ; floating point x position
                           torp-y ; floating point y position
                           length-of-track
                           torpedo-number
                           number-of-torpedoes-in-salvo
                           sector-contents)
  (:documentation "Display torpedo track information."))

(defmethod track-torpedo ((output-window screen)
                          torp-x ; floating point x position
                          torp-y ; floating point y position
                          length-of-track
                          torpedo-number
                          number-of-torpedoes-in-salvo
                          sector-contents)
  "Display torpedo track information as a series of coordinates. Display fractional amounts
 because aiming between sectors is allowed."

  (if (= length-of-track 1)
      ;; Display track header
      (progn
        (skip-line output-window)
        (if (= number-of-torpedoes-in-salvo 1)
            (print-out output-window "Torpedo track: ")
            (print-out output-window
                       (format nil "Track for torpedo number ~A:  " torpedo-number))))
      ;; Line wrap every four sectors
      (when (or (= length-of-track 4)
                (= length-of-track 9))
        (skip-line output-window)))
  ;; Convert internal torp coords to 1-based player values
  (print-out output-window (format nil "~,1F - ~,1F  " (1+ torp-x) (1+ torp-y))))

(defmethod track-torpedo ((output-window window)
                          torp-x ; floating point x position
                          torp-y ; floating point y position
                          length-of-track
                          torpedo-number
                          number-of-torpedoes-in-salvo
                          sector-contents)
  "Display torpedo track information as updates to the short range scan window."

  (when (or (not (damagedp +short-range-sensors+))
            *dockedp*)
    (when (= length-of-track 1)
      (short-range-scan output-window))
    (if (or (string= sector-contents +empty-sector+)
            (string= sector-contents +black-hole+))
        ;; Pass over an empty sector. Black hole collisions are handled without effects.
        (progn
          (put-short-range-scan-symbol output-window +torpedo+
                                       (round torp-x) (round torp-y))
          (turn-sound-on (* length-of-track 10))
          (sleep 1)
          (turn-sound-off)
          (put-short-range-scan-symbol output-window
                                       sector-contents (round torp-x) (round torp-y)))
        ;; Highlight an impact sector
        (boom output-window sector-contents (round torp-x) (round torp-y))))
  ;; Also print torpedo track information even if short range sensors are damaged
  (call-next-method *message-window*
                    torp-x
                    torp-y
                    length-of-track
                    torpedo-number
                    number-of-torpedoes-in-salvo
                    sector-contents))

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
                    (valid-p displaced-to-sector))
           (setf (coord-ref *quadrant* torpedo-sector) +empty-sector+)
           (setf (coord-ref *quadrant* displaced-to-sector) sector-contents)
           ;; Thing is displaced without notification
           (unless (string= sector-contents +thing+)
             (print-message *message-window* (format nil " displaced by blast to ~A ~%"
                                    (format-sector-coordinates displaced-to-sector))))
           ;; A ship was displaced, reset all distances for next attack on player.
           (do ((i 0 (1+ i)))
               ((>= i *enemies-here*))
             (setf (enemy-distance (aref *quadrant-enemies* i))
                   (distance *ship-sector* (enemy-sector-coordinates (aref *quadrant-enemies* i))))
             (setf (enemy-average-distance (aref *quadrant-enemies* i))
                   (distance *ship-sector* (enemy-sector-coordinates (aref *quadrant-enemies* i))))
             (sort-klingons))))
      (incf torp-x delta-x)
      (incf torp-y delta-y)
      (setf torpedo-sector (make-sector-coordinate :x (round torp-x) :y (round torp-y)))
      (setf movement-count (1+ movement-count))
      (if (valid-p torpedo-sector)
          (progn
            (setf sector-contents (coord-ref *quadrant* torpedo-sector))
            (track-torpedo *short-range-scan-window*
                           torp-x torp-y
                           movement-count torpedo-number number-of-torpedoes-in-salvo
                           sector-contents)
            (unless (string= sector-contents +empty-sector+)
              ;; hit something
              (skip-line *message-window*) ; start new line after text track
              (cond
                ;; Hit our ship
                ((or (string= sector-contents +enterprise+)
                     (string= sector-contents +faerie-queene+))
                 (print-message *message-window*
                                (format nil "~%Torpedo hits ~A.~%" (format-ship-name)))
                 (setf ship-hit
                       (calculate-torpedo-damage initial-position torpedo-sector bullseye-angle))
                 (setf *dockedp* nil) ; we're blown out of dock
                 ;; We may be displaced.
                 (unless *landedp* ; Cheat if on a planet
                   (setf displaced-to-sector (displace-ship (coordinate-x torpedo-sector)
                                                            (coordinate-y torpedo-sector)
                                                            angle))
                   (when (valid-p displaced-to-sector)
                     (when (string= (coord-ref *quadrant* displaced-to-sector) +black-hole+)
                       (finish 'destroyed-by-black-hole)
                       (return-from move-torpedo-within-quadrant ship-hit))
                     ;; can't move into object
                     (when (string= (coord-ref *quadrant* displaced-to-sector) +empty-sector+)
                       (setf *ship-sector* displaced-to-sector)
                       (print-message *message-window* (format-ship-name))
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
                       (print-message *message-window*
                                      (format nil "***~A at ~A uses anti-photon device;~%"
                                              (letter-to-name sector-contents)
                                              (format-sector-coordinates torpedo-sector)))
                       (print-message *message-window* (format nil "   torpedo neutralized.~%")))
                     ;; Hit a regular enemy
                     (do ((enemy-index 0 (1+ enemy-index)) ; find the enemy
                          e-energy
                          enemy-hit)
                         ((coord-equal torpedo-sector
                                       (enemy-sector-coordinates (aref *quadrant-enemies*
                                                                       enemy-index)))
                          (setf e-energy
                                (abs (enemy-energy (aref *quadrant-enemies* enemy-index))))
                          (setf enemy-hit (calculate-torpedo-damage initial-position
                                                                    torpedo-sector
                                                                    bullseye-angle))
                          (when (< e-energy enemy-hit)
                            (setf enemy-hit e-energy))
                          (decf (enemy-energy (aref *quadrant-enemies* enemy-index))
                                (if (< (enemy-energy (aref *quadrant-enemies* enemy-index)) 0)
                                    (- enemy-hit)
                                    enemy-hit))
                          (if (= (enemy-energy (aref *quadrant-enemies* enemy-index)) 0)
                              (dead-enemy torpedo-sector sector-contents torpedo-sector)
                              ;; If enemy damaged but not destroyed, try to displace
                              (progn
                                (print-message *message-window* (format nil "***~A at ~A"
                                                       (letter-to-name sector-contents)
                                                       (format-sector-coordinates torpedo-sector)))
                                (setf displaced-to-sector
                                      (displace-ship (coordinate-x torpedo-sector)
                                                     (coordinate-y torpedo-sector)
                                                     angle))
                                (cond
                                  ((or (not (valid-p displaced-to-sector))
                                       ;; can't move into object
                                       (string/= (coord-ref *quadrant* displaced-to-sector)
                                                 +empty-sector+))
                                   (print-message *message-window*
                                                  (format nil " damaged but not destroyed.~%")))

                                  ((string= (coord-ref *quadrant* displaced-to-sector)
                                            +black-hole+)
                                   (print-message *message-window*
                                                  (format nil " buffeted into black hole.~%"))
                                   (dead-enemy torpedo-sector sector-contents torpedo-sector))

                                  (t
                                   (print-out *message-window* " damaged --")
                                   (setf (enemy-sector-coordinates
                                          (aref *quadrant-enemies* enemy-index))
                                         displaced-to-sector)
                                   (setf shovedp t)))))))))
                ;; Hit a base
                ((string= sector-contents +starbase+)
                 (print-message *message-window* (format nil "~%***STARBASE DESTROYED..~%"))
                 ;; Remove the base from the list of bases and related data structures
                 (setf *base-quadrants*
                       (remove *ship-quadrant* *base-quadrants* :test #'coord-equal))
                 (setf *base-sector* nil)
                 (setf (coord-ref *quadrant* torpedo-sector) +empty-sector+)
                 (setf (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*))
                       (1- (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*))))
                 (setf (starchart-page-starbases (coord-ref *starchart* *ship-quadrant*))
                       (1- (starchart-page-starbases (coord-ref *starchart* *ship-quadrant*))))
                 (setf *destroyed-bases* (1+ *destroyed-bases*))
                 (setf *dockedp* nil))
                ;; Hit a planet
                ((string= sector-contents +planet+)
                 (print-message
                  *message-window*
                  (format nil "***~A at ~A destroyed.~%" (letter-to-name sector-contents)
                          (format-sector-coordinates torpedo-sector)))
                 (setf *destroyed-uninhabited-planets* (1+ *destroyed-uninhabited-planets*))
                 (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
                   (setf (planet-destroyedp p) t)
                   (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) p))
                 (setf *planet-coord* nil)
                 (setf *current-planet* nil)
                 (setf (coord-ref *quadrant* torpedo-sector) +empty-sector+)
                 (when *landedp* ; captain perishes on planet
                   (finish 'planet-destroyed-while-landed)))
                ;; Hit an inhabited world -- very bad!
                ((string= sector-contents +world+)
                 (print-message
                  *message-window*
                  (format nil "***~A at ~A destroyed.~%" (letter-to-name sector-contents)
                          (format-sector-coordinates torpedo-sector)))
                 (setf *destroyed-inhabited-planets* (1+ *destroyed-inhabited-planets*))
                 (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
                   (setf (planet-destroyedp p) t)
                   (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) p))
                 (setf *planet-coord* nil)
                 (setf *current-planet* nil)
                 (setf (coord-ref *quadrant* torpedo-sector) +empty-sector+)
                 (when *landedp* ; captain perishes on planet
                   (finish 'planet-destroyed-while-landed))
                 (print-message
                  *message-window*
                  (format nil "You have just destroyed an inhabited planet.~%"))
                 (print-message
                  *message-window*
                  (format nil "Celebratory rallies are being held on the Klingon homeworld.~%")))
                ;; Hit a star
                ;; TODO - hold the player responsible if a star novaed by a torpedo turns into a
                ;;        supernova
                ((string= sector-contents +star+)
                 (if (> (random 1.0) 0.10)
                     (nova torpedo-sector)
                     (print-message *message-window*
                                    (format nil "***~A at ~A unaffected by photon blast.~%"
                                            (letter-to-name sector-contents)
                                            (format-sector-coordinates torpedo-sector)))))
                ;; Hit a thingy
                ((string= sector-contents +thing+)
                 (if (> (random 1.0) 0.7)
                     (progn
                       (print-message *message-window*
                        (format nil "~%AAAAIIIIEEEEEEEEAAAAAAAAUUUUUGGGGGHHHHHHHHHHHH!!!~%")
                        :print-slowly t)
                       (print-message *message-window*
                        (format nil "~%    HACK!     HACK!    HACK!        *CHOKE!*  ~%")
                        :print-slowly t)
                       (print-message *message-window* "~%Mr. Spock-")
                       (print-message *message-window*
                                      (format nil "  \"Fascinating!\"~%~%") :print-slowly t)
                       (dead-enemy torpedo-sector sector-contents torpedo-sector))
                     ;; Stas Sergeev added the possibility that you can shove the Thingy and
                     ;; piss it off. It then becomes an enemy and may fire at you.
                     (progn
                       (setf *thing-is-angry-p* t)
                       (setf displaced-to-sector (displace-ship (coordinate-x torpedo-sector)
                                                                (coordinate-y torpedo-sector)
                                                                angle))
                       (when (valid-p displaced-to-sector)
                         (cond
                           ;; Thing vanishes silently into black hole, very mysterious
                           ((string= (coord-ref *quadrant* displaced-to-sector) +black-hole+)
                            (setf (coord-ref *quadrant* torpedo-sector) +empty-sector+)
                            (setf *thing-is-angry-p* nil)
                            (setf shovedp nil))
                           ;; can't move into object
                           ((string/= (coord-ref *quadrant* displaced-to-sector) +empty-sector+)
                            (setf shovedp nil))
                           (t
                            (setf shovedp t)))))))
                ;; Black hole
                ((string= sector-contents +black-hole+)
                 (print-message *message-window* (format nil "~%***~A at ~A swallows torpedo.~%"
                                        (letter-to-name sector-contents)
                                        (format-sector-coordinates torpedo-sector))))
                ;; hit the web
                ((string= sector-contents +tholian-web+)
                 (print-message *message-window*
                                (format nil "~%***Torpedo absorbed by Tholian web.~%")))
                ;; Hit a Tholian
                ((string= sector-contents +tholian+)
                 (cond
                   ;; Tholian is destroyed
                   ((>= (calculate-torpedo-damage initial-position torpedo-sector bullseye-angle)
                        600)
                    (setf (coord-ref *quadrant* torpedo-sector)
                          +empty-sector+)
                    (setf *tholians-here* 0)
                    (dead-enemy torpedo-sector sector-contents torpedo-sector))
                   ;; Tholian survives
                   ((> (random 1.0) 0.05)
                    (print-message *message-window*
                                   (format nil "~%***~A at ~A survives photon blast.~%"
                                           (letter-to-name sector-contents)
                                           (format-sector-coordinates torpedo-sector))))
                   ;; Tholian vanishes, leaving behind a black hole.
                   (t
                    (print-message *message-window* (format nil "~%***~A at ~A disappears.~%"
                                           (letter-to-name sector-contents)
                                           (format-sector-coordinates torpedo-sector)))
                    (setf (coord-ref *quadrant* torpedo-sector)
                          +tholian-web+)
                    (setf *tholians-here* 0)
                    (setf *enemies-here* (1- *enemies-here*))
                    (drop-entity-in-sector +black-hole+))))
                ;; Problem!
                (t
                 (print-message *message-window*
                                (format nil "~%***Torpedo hits unknown object ~A at ~A~%"
                                        sector-contents
                                        (format-sector-coordinates torpedo-sector)))))
              (setf movement-ended-p t)))
          ;; The torpedo exited the quadrant without hitting anything
          (progn
            (setf movement-ended-p t)
            (print-message *message-window* (format nil "~%Torpedo missed.~%")))))
    (return-from move-torpedo-within-quadrant ship-hit)))

(defun photon-torpedo-target-check (x y) ; C: bool targetcheck(double x, double y, double *course)
  "Verfiy that the parameter x and y coordinates are an acceptable target for a photon torpedo.
 Return the course direction of the target or nil."

  ;; TODO - fractional sector coordinates are allowed, valid-p checks may need to be relaxed to
  ;;        allow fractional coords, or find a different way to represent coordinates. The
  ;;        place to check might be when a coordinate is used as an array index. That is,
  ;;        coordinates are used as array indices but they could equally be an offset into an
  ;;        array. The storage format of sector information should not restrict the valid values
  ;;        of coordinates.
  (unless (valid-sector-p x y)
    (huh)
    (return-from photon-torpedo-target-check nil))

  ;; Multiply by 0.1 to scale to qadrant-sized units
  (let ((delta-x (* 0.1 (- y (coordinate-y *ship-sector*))))
        (delta-y (* 0.1 (- (coordinate-x *ship-sector*) x))))
    ;; When both are zero the player targeted their own sector. Conveniently, values of zero
    ;; aren't valid for atan2.
    (when (and (= delta-x 0)
               (= delta-y 0))
      (print-message *message-window* (format nil "~%Spock-  \"Bridge to sickbay.  Dr. McCoy,~%"))
      (print-message *message-window* (format nil "  I recommend an immediate review of~%"))
      (print-message *message-window* (format nil "  the Captain's psychological profile.\"~%~%"))
      (return-from photon-torpedo-target-check nil))

    (return-from photon-torpedo-target-check (* 1.90985932 (atan delta-x delta-y)))))

(defun get-number-of-torpedoes-to-fire ()
  "Up to three torpedoes can be fired in one salvo. Return a number between 1 and 3 or 0 to
cancel."

  (do (input-item)
      (nil)
    ;; Get number of torpedoes to fire
    (unless (input-available-p)
      (print-message *message-window* (format nil "~D torpedoes left.~%" *torpedoes*))
      (print-prompt "Number of torpedoes to fire: "))
    (setf input-item (scan-input))
    (cond
      ((numberp input-item)
       (cond
         ;; abort command
         ((<= input-item 0)
          (return-from get-number-of-torpedoes-to-fire 0))

         ((> input-item 3)
          (print-message *message-window* (format nil "Maximum of 3 torpedoes per burst.~%"))
          (clear-type-ahead-buffer))

         ((<= input-item *torpedoes*)
          (return-from get-number-of-torpedoes-to-fire input-item))
         ;; less than 4 but more than the available torpedoes
         (t
          (clear-type-ahead-buffer))))
      ;; Not a number and not nil, must be alpha
      (input-item
       (huh)
       (return-from get-number-of-torpedoes-to-fire 0))

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
      ((not (input-available-p))
       (setf target-input-method 'prompt))
      ;; All torpedoes at one target
      ((= (number-of-input-items) 2)
       (setf target-input-method 'one-target))
      ;; too few coordinates for number of torpedoes
      ((< (* number-of-torpedoes-to-fire 2) (number-of-input-items))
       (huh)
       (return-from get-targets-for-torpedoes nil))
      ;; the coordinates for each torpedo are on the input line, just read them
      (t
       (setf target-input-method 'read-input)))

    (do ((torpedo 0 (1+ torpedo))
         ;; TODO - try it as a list?
         (courses (make-array number-of-torpedoes-to-fire))) ; array of directions in which to fire each torpedo
        ((>= torpedo number-of-torpedoes-to-fire)
         (return-from get-targets-for-torpedoes courses))
      (when (eql target-input-method 'prompt) ; Prompt for each one
        (print-prompt (format nil "Target sector for torpedo number ~A: " (1+ torpedo))))
      (if (and (eql target-input-method 'one-target)
               (> torpedo 0))
          (setf (aref courses torpedo) (aref courses 0))
          (progn
            (multiple-value-bind (x y) (scan-coordinate-pair)
              (setf (aref courses torpedo) (photon-torpedo-target-check x y)))
            (unless (aref courses torpedo)
              ;; If the target check returned nil then just return, player was already notified
              (return-from get-targets-for-torpedoes nil)))))))

(defun fire-photon-torpedoes () ; C: torps()
  "Launch photon torpedo salvo."

  (when (damagedp +photon-torpedoes+)
    (print-message *message-window* (format nil "Photon tubes damaged.~%"))
    (return-from fire-photon-torpedoes nil))

  (when (= *torpedoes* 0)
    (print-message *message-window* (format nil "No torpedoes left.~%"))
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
          (unless *dockedp*
            (setf *torpedoes* (1- *torpedoes*)))
          (setf random-variation (- (* (+ (random 1.0) (random 1.0)) 0.5) 0.5))
          (if (>= (abs random-variation) 0.47)
              (progn
                ;; misfire!
                (setf random-variation (* (+ (random 1.0) 1.2) random-variation)) ; unused calculation!
                (if (> number-of-torpedoes-to-fire 1)
                    (print-message *message-window*
                                   (format nil "***TORPEDO NUMBER ~A MISFIRES~%" i)
                                   :print-slowly t)
                    (print-message *message-window* "***TORPEDO MISFIRES.~%" :print-slowly t))
                (when (< (1+ i) number-of-torpedoes-to-fire)
                  (print-message *message-window* (format nil "  Remainder of burst aborted.~%")))
                (when (<= (random 1.0) 0.2)
                  (print-message *message-window*
                                 (format nil "***Photon tubes damaged by misfire.~%"))
                  (setf (aref *device-damage* +photon-torpedoes+)
                        (* *damage-factor* (+ 1.0 (* 2.0 (random 1.0))))))
                (setf misfire t))
              (progn
                ;; Fire a photon torpedo
                (cond ; Torpedoes are less accurate in several cicrcumstances
                  (*cloakedp*
                   (setf random-variation (* random-variation 1.2)))

                  ((or *shields-are-up-p*
                       *dockedp*)
                   (setf random-variation (* random-variation (+ 1.0 (* 0.0001 *shield-energy*))))))
                (move-torpedo-within-quadrant (aref courses i) random-variation *ship-sector*
                                              i number-of-torpedoes-to-fire)
                (when (or *all-done-p*
                          (quadrant-supernovap
                           (coord-ref *galaxy* *ship-quadrant*)))
                  (return-from fire-photon-torpedoes nil))))))
      ;; TODO - is this a common idiom suitable for a function? Yes - (enemies-remaining-p)
      (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
        (finish 'won)))))

(defun subspace-radio-available-p ()
  "Check if a subspace radio is available. A subspace radio is available if the onboard device is
not damaged or if the ship is docked, and the ship is not cloaked."

  (and (or (not (damagedp +subspace-radio+))
           *dockedp*)
       (not *cloakedp*)))

(defun cloak () ; C: void cloak(void)

  (when (string= *ship* +faerie-queene+)
    (print-message *message-window* (format nil "Ye Faerie Queene has no cloaking device.~%"))
    (return-from cloak nil))

  (let ((action 'none)
        input-item)
    (unless (input-available-p)
      (setf input-item (scan-input)))
    (when (numberp input-item)
      (return-from cloak nil))
    (if input-item ; is not nil
        (let ((token (match-token input-item (list "on" "off"))))
          (cond
            ((string= token "on")
             (when *cloakedp*
               (print-message *message-window*
                              (format nil "The cloaking device has already been switched on.~%"))
               (return-from cloak nil))
             (setf action 'turn-cloaking-on))

            ((string= token "off")
             (unless *cloakedp*
               (print-message *message-window*
                              (format nil "The cloaking device has already been switched off.~%"))
               (return-from cloak nil))
             (setf action 'turn-cloaking-off))

            (t
             (huh)
             (return-from cloak nil))))
        (progn
          (unless *cloakedp*
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
      (print-message *message-window* (format nil "Engineer Scott- \"Aye, Sir.\"~%"))
      (setf *cloakedp* nil)
      ;; The Romulans detect you uncloaking
      (when (and (> *romulans-here* 0)
                 (>= *stardate* +algeron-date+)
                 (not *cloaking-violation-reported-p*))
        (print-message
         *message-window*
         (format nil "The Romulan ship discovers you are breaking the Treaty of Algeron!~%"))
        (incf *cloaking-violations* 1)
        (setf *cloaking-violation-reported-p* t))
      ;; TODO - breaking the Treaty of Algeron while in the Romulan Neutral Zone ends the game
      ;;        should check for both cloaking and uncloaking
      ;;(when (and neutz
      ;;           (>= stardate +algeron-date+)
      ;;  (finish 'cloaking-while-in-neutral-zone))
      (return-from cloak nil))
    ;; turn cloaking on
    (when (damagedp +cloaking-device+)
      (print-message *message-window*
                     (format nil "Engineer Scott- \"The cloaking device is damaged, Sir.\"~%"))
      (return-from cloak nil))
    (when *dockedp*
      (print-message *message-window* (format nil "You cannot cloak while docked.~%"))
      (return-from cloak nil))
    (when (and (>= *stardate* +algeron-date+)
               (not *cloaking-violation-reported-p*))
      (print-message *message-window*
                     (format nil "Spock- \"Captain, using the cloaking device is a violation~%"))
      (print-message *message-window*
                     (format nil "  of the Treaty of Algeron. Considering the alternatives,~%"))
      (print-prompt "  are you sure this is wise?")
      (unless (get-y-or-n-p)
        (return-from cloak nil)))
    (print-message *message-window*
                   (format nil "Engineer Scott- \"The cloaking device has been engaged, Sir.\"~%"))
    (setf *cloakedp* t)
    (check-treaty-of-algeron)))

(defun shield-actions (&key (raise-shields nil)) ; C: doshield(bool raise)
  "Change shield status. The optional parameter is used to raise the shields without player
input when a tractor beam event occurs."

  (let ((action 'none)
        input-item)
    (if raise-shields
        (setf action 'raise)
        (progn
          (when (input-available-p)
            (setf input-item (scan-input)))
          (let ((token (match-token input-item (list "transfer" "up" "down"))))
            (if (string= token "transfer")
                (setf action 'energy-transfer)
                (cond
                  ((damagedp +shields+)
                   (print-message *message-window* (format nil "Shields damaged and down.~%"))
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
               (print-message *message-window* (format nil "Shields damaged and down.~%"))
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
         (print-message *message-window* (format nil "Shields already up.~%"))
         (return-from shield-actions nil))
       (setf *shields-are-up-p* t)
       (setf *shields-are-changing-p* t)
       (unless *dockedp*
         (decf *ship-energy* 50.0))
       (print-message *message-window* (format nil "Shields raised.~%"))
       (when (<= *ship-energy* 0)
         (print-message *message-window*
                        (format nil "~%Shields raising uses up last of energy.~%"))
         (finish 'out-of-energy)
         (return-from shield-actions nil))
       (setf *action-taken-p* t))

      ((eql action 'lower)
       (unless *shields-are-up-p*
         (print-message *message-window* (format nil "Shields already down.~%"))
         (return-from shield-actions nil))
       (setf *shields-are-up-p* nil)
       (setf *shields-are-changing-p* t)
       (print-message *message-window* (format nil "Shields lowered.~%"))
       (setf *action-taken-p* t))

      ((eql action 'energy-transfer)
       (when (input-available-p)
         (setf input-item (scan-input)))
       (do ()
           ((numberp input-item))
         (clear-type-ahead-buffer)
         (print-prompt "Energy to transfer to shields: ")
         (setf input-item (scan-input)))
       (when (= input-item 0)
         (return-from shield-actions nil))
       (when (> input-item *ship-energy*)
         (print-message *message-window* (format nil "Insufficient ship energy.~%"))
         (return-from shield-actions nil))
       (setf *action-taken-p* t)
       (when (>= (+ *shield-energy* input-item) *initial-shield-energy*)
         (print-message *message-window* (format nil "Shield energy maximized.~%"))
         (when (> (+ *shield-energy* input-item) *initial-shield-energy*)
           (print-message *message-window*
                          (format nil "Excess energy requested returned to ship energy~%")))
         (decf *ship-energy* (- *initial-shield-energy* *shield-energy*))
         (setf *shield-energy* *initial-shield-energy*)
         (return-from shield-actions nil))
       ;; Prevent shield drain loophole
       (when (and (< input-item 0.0)
                  (> (- *ship-energy* input-item) *initial-energy*))
        (print-message *message-window* (format nil "~%Engineering to bridge--~%"))
        (print-message *message-window*
                       (format nil "  \"Scott here. Power circuit problem, Captain.\"~%"))
        (print-message *message-window* (format nil "  \"I can't drain the shields.\"~%"))
        (setf *action-taken-p* nil)
        (return-from shield-actions nil))

       (when (< (+ *shield-energy* input-item) 0)
         (print-message *message-window* (format nil"All shield energy transferred to ship.~%"))
         (incf *ship-energy* *shield-energy*)
         (setf *shield-energy* 0.0)
         (return-from shield-actions nil))

       ;; this stanza needs a return-from if there is ever code added after it
       (print-message *message-window* (format nil "Scotty- ~%"))
       (if (> input-item 0)
           (print-message *message-window* (format nil "\"Transferring energy to shields.\"~%"))
           (print-message *message-window* (format nil "\"Draining energy from shields.\"~%")))
       (incf *shield-energy* input-item)
       (decf *ship-energy* input-item)))))

(defun ram (&key rammed-by-p enemy enemy-coordinates) ; C: void ram(bool ibumpd, feature ienm, coord w)
  "Make our ship ram something. If rammed-by-p is true then an enemy ship is ramming the player.
enemy is the single-letter symbol of the enemy ramming/being rammed. enemy-coordinates are the
coordinates of the enemy being rammed or the original coordinates of ramming enemy."

  (print-message *message-window* (format nil "~%***RED ALERT!  RED ALERT!~%") :print-slowly t)
  (print-message *message-window* (format nil "~%***COLLISION IMMINENT.~%"))
  (print-message *message-window* (format nil "~%***~A ~A ~A at ~A~A.~%~%"
                         (format-ship-name)
                         (if rammed-by-p "rammed by" "rams")
                         (letter-to-name enemy)
                         (format-coordinates enemy-coordinates)
                         (if rammed-by-p " (original position)" "")))
  (dead-enemy enemy-coordinates enemy *ship-sector*)
  (setf *shields-are-up-p* nil)
  (print-message *message-window* (format nil "***Shields are down.~%"))
  (let ((number-of-casualties (truncate(+ 10.0 (* 20.0 (random 1.0))))))
    (print-message *message-window*
                   (format nil "***Sickbay reports ~A casualties~%" number-of-casualties))
    (incf *casualties* number-of-casualties)
    (decf *crew* number-of-casualties)) ; TODO - should game end if crew count is too low?
  ;; In the pre-SST2K version, all devices got equiprobably damaged, which was silly. Instead, pick
  ;; up to half the devices at random according to our weighting table,
  (let ((devices-to-damage (truncate (* (random 1.0) (/ +number-of-devices+ 2)))))
    (do ((m 0 (1+ m))
         dev-index)
        ((>= m devices-to-damage))
      (setf dev-index (get-random-device))
      (when (>= (aref *device-damage* dev-index) 0)
        ;; Damage for at least time of travel!
        (incf (aref *device-damage* dev-index)
              (+ *time-taken-by-current-operation* (* (1+ (* 10.0 (get-enemy-hardness enemy)
                                                             (random 1.0)))
                                                     *damage-factor*))))))
  (print-message *message-window* (format nil "***~A heavily damaged.~%" (format-ship-name)))
  (if (> (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
      (damage-report)
      (finish 'won)))

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
    (incf sum (pop weights))
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

(defun score-multiple (format-string count score) ; C: score_item(const char *str, int score)
  "Helper function to print a score for multiple items and update the score. The format-string
is a string suitable for use with the format function."

  (when (> count 0)
    (print-out *score-window* (format nil format-string count (* count score)))
    (incf *score* (* count score))))

(defun score-single (format-string score) ; C: score_item(const char *str, int num, int score)
  "Helper function to print a score for a single item and update the score. The format-string
is a string suitable for use with the format function."

  (print-out *score-window* (format nil format-string score))
  (incf *score* score))

;; TODO - add an option to write the final score to a file, or record it some other way.
(defun score () ; C: score(void)
  "Compute player's score."

  ;; TODO - use a generic function (prepare-window-for-output)?
  (if (eql *score-window* *message-window*)
      (skip-line *score-window*)
      (clear-window *score-window*))

  (setf *score* 0)
  (if *score-window*
      (print-out *score-window* (format nil "~60:@<SCORE~>~%"))
      (print-out *score-window* (format nil "Your score --~%")))
  (score-multiple "~7@A~45< Romulan~:P destroyed~;~>~8@A~%"
                  (- *initial-romulans* *remaining-romulans*) 20)
  (when *game-won-p*
    (score-multiple "~7@A~45< Romulan~:P captured~;~>~8@A~%"
                    *remaining-romulans* *remaining-romulans*))
  (score-multiple "~7@A~45< ordinary Klingon~:P destroyed~;~>~8@A~%"
                  (- *initial-klingons* *remaining-klingons*) 10)
  (score-multiple "~7@A~45< Klingon commander~:P destroyed~;~>~8@A~%"
                  (- *initial-commanders* (length *commander-quadrants*)) 50)
  (score-multiple "~7@A~45< Super-Commander~:P destroyed~;~>~8@A~%"
                  (- *initial-super-commanders* *remaining-super-commanders*) 200)
  (score-multiple "~7,2F~45< Klingon~:P per stardate~;~>~8,2F~%"
                  (klingons-per-stardate) (round (+ (* 500 (klingons-per-stardate)) 0.5)))
  (score-multiple "~7@A~45< Klingon~:P captured~;~>~8@A~%"
                  *captured-klingons* 3)
  (score-multiple "~7@A~45< star~:P destroyed by your action~;~>~8@A~%"
                  *destroyed-stars* -5)
  (score-multiple "~7@A~45< uninhabited planet~:P destroyed by your action~;~>~8@A~%"
                  *destroyed-uninhabited-planets* -10)
  (score-multiple "~7@A~45< inhabited planet~:P destroyed by your action~;~>~8@A~%"
                  *destroyed-inhabited-planets* -300)
  (score-multiple "~7@A~45< base~:P destroyed by your action~;~>~8@A~%"
                  *destroyed-bases* -100)
  (score-multiple "~7@A~45< call~:P for help from starbase~;~>~8@A~%"
                  *calls-for-help* -45)
  (score-multiple "~7@A~45< casualt~:@P incurred~;~>~8@A~%"
                  *casualties* -1)
  (score-multiple "~7@A~45< crew abandoned in space~;~>~8@A~%"
                  *abandoned-crew* -3)
  (let (ships-destroyed)
    (cond
      ((string= *ship* +enterprise+)
       (setf ships-destroyed 0))
      ((string= *ship* +faerie-queene+)
       (setf ships-destroyed 1))
      ((string= *ship* +no-ship+)
       (setf ships-destroyed 2)))
    (score-multiple "~7@A~45< ship~:P lost or destroyed~;~>~8@A~%"
                    ships-destroyed -100))
  (score-multiple "~7@A~45< Treaty of Algeron violation~:P~;~>~8@A~%"
                    *cloaking-violations* -100)
  (unless *alivep*
    (score-single "~52<        Penalty for getting yourself killed~;~>~8@A~%" -200))
  (when *game-won-p*
    (print-out *score-window* (format nil "~52<        Bonus for winning ~A game~;~>~8@A~%"
                       (string-capitalize (rest (assoc *skill-level* *skill-level-labels*)))
                       (* 100 *skill-level*)))
    (incf *score* (* 100 *skill-level*)))
  (print-out *score-window* (format nil "~%~52<TOTAL SCORE~>~8@A~%" (round *score*))))

(define-constant +day-names+
    (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun plaque () ; C: plaque(void)
  "Emit winner's commemmorative plaque.

The original Fortran printed on 132 column fanfold, and subsequent versions printed on
8.5\"x11\" or A4 in portrait mode. All versions were nicely centered. One implementation
scrolled the ASCII art across the screen. Just write it to a file and let the player decide
what to do with it."

  (let (file
        winner)
    (clear-type-ahead-buffer)
    (skip-line *message-window* 2)
    (print-prompt "File or device name for your plaque: ")
    (setf file (get-input-line *prompt-window*))
    (print-prompt "Enter name to go on plaque (up to 30 characters): ")
    (setf winner (get-input-line *prompt-window*))
    (setf winner (subseq winner 0 (min 30 (length winner))))
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
      (format s "~95:@<U. S. S. ENTERPRISE~>~%")
      (format s "~%~%~%")
      (format s "~95:@<For demonstrating outstanding ability as a starship captain~>~%~%")
      (format s "~95:@<Starfleet Command bestows to you~>~%~%")
      (format s "~95:@<~A~>~%~%" winner)
      (format s "~95:@<the rank of~>~%~%")
      (format s "~95:@<\"Commodore Emeritus\"~>~%~%")
      (cond
        ((= *skill-level* +expert+)
         (format s "~95:@<Expert level~>~%~%"))

        ((=  *skill-level* +emeritus+)
         (format s "~95:@<Emeritus level~>~%~%"))

        (t
         (format s "~95:@<Cheat level~>~%~%")))
      (multiple-value-bind
            (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
        (setf second second) (setf minute minute) (setf hour hour) (setf day-of-week day-of-week)
        (setf dst-p dst-p) (setf tz tz)
        (format s "~95:@<This day of ~A ~A, ~A~>~%~%"
                date
                (nth (1- month) (list "January" "February" "March" "April" "May" "June" "July"
                                      "August" "September" "October" "November" "December"))
                year))
      (format s "~95:@<Your score:  ~D~>~%~%" *score*)
      (format s "~95:@<Klingons per stardate:  ~,2F~>~%~%" (klingons-per-stardate)))))

(defun finish (finish-reason) ; C: finish(FINTYPE ifin)
  "End the game, with appropriate notfications. Here are the ways the game can end:

won - All klingons have been destroyed before time runs out. The player wins.
federation-resources-depleted - Time ran out with klingons remaining.
life-support-consumed - Ship has no life support
out-of-energy - Ship has no energy
destroyed-in-battle - Ship destroyed by an enemy
3-negative-energy-barrier-crossings - Attempted navigation outside the galaxy
ship-destroyed-by-nova - Ship destroyed by ordinary nova
destroyed-by-supernova - Ship did not escape from a quadrant containing a supernova
abandon-ship-with-no-starbases - No place to go
dilithium-crystals-failed - Use of raw dilithium crystals failed, or player self-destructed
failed-to-rematerialize - Emergency transport of ship by a base failed
phaser-fire-inside-shields - High-speed shield control failed, frying the ship
transporter-failure - Transporter fails while beaming up to or down from a planet
tractor-beam-while-mining - The ship is tractor-beamed while the mining party is on a planet
planet-destroyed-while-landed - Planet is destroyed while the mining party is on it
nova-destroys-planet-while-landed - Planet destroyed by nova while the mining party is on it
super-nova-destroys-shuttle - Supernova destroys shuttle while the mining party is using it
tractor-beam-destroys-shuttle - Tractor beam destroys shuttle while the mining part is using it
death-ray-malfunction - Death ray malfunctions by mutating humans on the ship
death-ray-creates-tribbles - Death ray malfunction creates tribbles that consume all life support
destroyed-by-black-hole - Ship falls into a black hole and is destroyed
all-crew-killed - All crew have been killed
cloaking-while-in-neutral-zone - Cloaking when Romulans are present, but no other enemy
"

  (update-windows)

  (setf *all-done-p* t)
  (skip-line *message-window*)
  (print-stars)
  (print-message *message-window*
                 (format nil "It is stardate ~A.~%~%" (format-stardate *stardate*)))
  (cond
    ;; C: FWON
    ((eql finish-reason 'won)
     (setf *game-won-p* t)
     (print-message *message-window*
                    (format nil "You have smashed the Klingon invasion fleet and saved~%"))
     (print-message *message-window* (format nil "the Federation.~%"))
     (when (/= *remaining-romulans* 0)
       (print-message *message-window*
                      (format nil "The remaining ~A Romulans surrender to Starfleet Command.~%"
                              *remaining-romulans*)))
     ;; Captured Klingon crew will get transfered to starbase
     (when (and *alivep*
                (> (- *brig-capacity* *brig-free*) 0))
       (incf *captured-klingons* (- *brig-capacity* *brig-free*))
       (print-message *message-window* (format nil "The ~D captured Klingons are transferred to Star Fleet Command.~%"
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
           (incf bad-points 100.0))
         (when (string= *ship* +no-ship+) ; TODO - this shouldn't be possible at this point
           (incf bad-points 300.0))
         (when (< bad-points 100.0)
           (setf bad-points 0.0)) ; Close enough!
         (when (or (< (- *stardate* *initial-stardate*) 5.0)
                   ;; killsPerDate >= RateMax
                   ;; TODO - for symmetry, could define an (initial-enemies) function, not sure it's needed
                   (>= (/ (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                             (+ *remaining-klingons*
                                (length *commander-quadrants*) *remaining-super-commanders*))
                          (- *stardate* *initial-stardate*))
                       (+ (* 0.1 *skill-level* (+ *skill-level* 1.0))
                          0.1
                          (* 0.008 bad-points))))
           (print-message *message-window*
                          (format nil "~%In fact, you have done so well that Starfleet Command~%"))
           (cond
             ((= *skill-level* +novice+)
              (print-message
               *message-window*
               (format nil "promotes you one step in rank from \"Novice\" to \"Fair\".~%")))
             ((= *skill-level* +fair+)
              (print-message
               *message-window*
               (format nil "promotes you one step in rank from \"Fair\" to \"Good\".~%")))
             ((= *skill-level* +good+)
              (print-message
               *message-window*
               (format nil "promotes you one step in rank from \"Good\" to \"Expert\".~%")))
             ((= *skill-level* +expert+)
              (print-message *message-window*
                             (format nil "promotes you to Commodore Emeritus.~%")))
             ((= *skill-level* +emeritus+)
              (print-message *message-window* "Computer-  ")
              (print-message *message-window*
                             (format nil "ERROR-ERROR-ERROR-ERROR~%") :print-slowly t)
              (print-message *message-window*
               (format nil "~%  YOUR-SKILL-HAS-EXCEEDED-THE-CAPACITY-OF-THIS-PROGRAM~%")
               :print-slowly t)
              (print-message *message-window*
                             (format nil "~%  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (print-message *message-window*
                             (format nil "~%  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (print-message *message-window*
                             (format nil "~%  THIS-PROGRAM-MUST-SURVIVE~%") :print-slowly t)
              (print-message *message-window*
               (format nil "~%  THIS-PROGRAM-MUST?- MUST ? - SUR? ? -?  VI~%") :print-slowly t)
              (print-message *message-window*
               (format nil "~%Now you can retire and write your own Star Trek game!~%")))))
           (when (>= *skill-level* +expert+)
             (print-prompt "Would you like to save your Commodore Emeritus Citation? ")
             (clear-type-ahead-buffer)
             (when (get-y-or-n-p)
               (plaque))))
       ;; Only grant long life if alive (original didn't!)
       (print-message *message-window* (format nil "~%LIVE LONG AND PROSPER.~%")))
     (score)
     (return-from finish nil))
    ;; FDEPLETE ; time ran out - Federation Resources Depleted
    ((eql finish-reason 'federation-resources-depleted)
     (print-message *message-window*
                    (format nil "Your time has run out and the Federation has been~%"))
     (print-message *message-window*
                    (format nil "conquered.  Your starship is now Klingon property,~%"))
     (print-message *message-window*
                    (format nil "and you are put on trial as a war criminal.  On the~%"))
     (print-message *message-window* "basis of your record, you are ")
     (if (> (* (+ *remaining-klingons*
                  (length *commander-quadrants*) *remaining-super-commanders*) 3.0)
            (+ *initial-klingons* *initial-commanders* *initial-super-commanders*))
         (progn
           (print-message *message-window* (format nil "acquitted.~%"))
           (print-message *message-window* (format nil "~%LIVE LONG AND PROSPER.~%")))
         (progn
           (print-message *message-window* (format nil "found guilty and~%"))
           (print-message *message-window* (format nil "sentenced to death by slow torture.~%"))
           (setf *alivep* nil)))
     (score)
     (return-from finish nil))
    ;; FLIFESUP
    ((eql finish-reason 'life-support-consumed)
     (print-message *message-window* (format nil "Your life support reserves have run out, and~%"))
     (print-message *message-window* (format nil "you die of thirst, starvation, and asphyxiation.~%"))
     (print-message *message-window* (format nil "~%Your starship is a derelict in space.~%")))
    ;; FNRG
    ((eql finish-reason 'out-of-energy)
     (print-message *message-window* (format nil "Your energy supply is exhausted.~%"))
     (print-message *message-window* (format nil "~%Your starship is a derelict in space.~%")))
    ;; FBATTLE
    ((eql finish-reason 'destroyed-in-battle)
     (print-message *message-window* (format nil "The ~A has been destroyed in battle.~%" (format-ship-name)))
     (print-message *message-window* (format nil "~%Dulce et decorum est pro patria mori.~%")))
    ;; FNEG3
    ((eql finish-reason '3-negative-energy-barrier-crossings)
     (print-message *message-window* (format nil "You have made three attempts to cross the negative energy~%"))
     (print-message *message-window* (format nil "barrier which surrounds the galaxy.~%"))
     (print-message *message-window* (format nil "~%Your navigation is abominable.~%"))
     (score))
    ;; FNOVA
    ((eql finish-reason 'ship-destroyed-by-nova)
     (print-message *message-window* (format nil "Your starship has been destroyed by a nova.~%"))
     (print-message *message-window* (format nil "~%That was a great shot.~%~%")))
    ;; FSNOVAED
    ((eql finish-reason 'destroyed-by-supernova)
     (print-message *message-window* (format nil "The ~A has been fried by a supernova.~%" (format-ship-name)))
     (print-message *message-window* (format nil "~%...Not even cinders remain...~%")))
    ;; FABANDN
    ((eql finish-reason 'abandon-ship-with-no-starbases)
     (print-message *message-window* (format nil "You have been captured by the Klingons. If you still~%"))
     (print-message *message-window* (format nil "had a starbase to be returned to, you would have been~%"))
     (print-message *message-window* (format nil "repatriated and given another chance. Since you have~%"))
     (print-message *message-window* (format nil "no starbases, you will be mercilessly tortured to death.~%")))
    ;; FDILITHIUM
    ;; This is the same finish as the self-destruct command
    ((eql finish-reason 'dilithium-crystals-failed)
     (print-message *message-window* (format nil "Your starship is now an expanding cloud of subatomic particles.~%")))
    ;; FMATERIALIZE
    ((eql finish-reason 'failed-to-rematerialize)
     (print-message *message-window* (format nil "Starbase was unable to re-materialize your starship.~%"))
     (print-message *message-window* (format nil "~%Sic transit gloria mundi~%")))
    ;; FPHASER
    ((eql finish-reason 'phaser-fire-inside-shields)
     (print-message *message-window* (format nil "The ~A has been cremated by its own phasers.~%" (format-ship-name))))
    ;; FLOST
    ((eql finish-reason 'transporter-failure)
     (print-message *message-window* (format nil "You and your landing party have been~%"))
     (print-message *message-window* (format nil "converted to energy, dissipating through space.~%")))
    ;; FMINING
    ((eql finish-reason 'tractor-beam-while-mining)
     (print-message *message-window* (format nil "You are left with your landing party on~%"))
     (print-message *message-window* (format nil "a wild jungle planet inhabited by primitive cannibals.~%"))
     (print-message *message-window* (format nil "They are very fond of \"Captain Kirk\" soup.~%"))
     (print-message *message-window* (format nil "~%Without your leadership, the ~A is destroyed.~%" (format-ship-name))))
    ;; FDPLANET
    ((eql finish-reason 'planet-destroyed-while-landed)
     (print-message *message-window* (format nil "You and your mining party perish.~%"))
     (print-message *message-window* (format nil "~%That was a great shot.~%")))
    ;; The Galileo being caught in a supernova is a special case of a mining party being wiped
    ;; out in a nova. Handle them together.
    ;; FSSC, FPNOVA
    ((or (eql finish-reason 'super-nova-destroys-shuttle)
         (eql finish-reason 'nova-destroys-planet-while-landed))
     (when (eql finish-reason 'super-nova-destroys-shuttle)
       (print-message *message-window* (format nil "The Galileo is instantly annihilated by the supernova.~%")))
     (print-message *message-window* (format nil "You and your mining party are atomized.~%"))
     (print-message *message-window* (format nil "~%Mr. Spock takes command of the ~A and~%" (format-ship-name)))
     (print-message *message-window* (format nil "joins the Romulans, reigning terror on the Federation.~%")))
    ;; FSTRACTOR
    ((eql finish-reason 'tractor-beam-destroys-shuttle)
     (print-message *message-window* (format nil "The shuttle craft Galileo is also caught,~%"))
     (print-message *message-window* (format nil "and breaks up under the strain.~%"))
     (print-message *message-window* (format nil "Your debris is scattered for millions of miles.~%"))
     (print-message *message-window* (format nil "~%Without your leadership, the ~A is destroyed.~%" (format-ship-name))))
    ;; FDRAY
    ((eql finish-reason 'death-ray-malfunction)
     (print-message *message-window* (format nil "The mutants attack and kill Spock.~%"))
     (print-message *message-window* (format nil "Your ship is captured by Klingons, and~%"))
     (print-message *message-window* (format nil "your crew is put on display in a Klingon zoo.~%")))
    ;; FTRIBBLE
    ((eql finish-reason 'death-ray-creates-tribbles)
     (print-message *message-window* (format nil "Tribbles consume all remaining water,~%"))
     (print-message *message-window* (format nil "food, and oxygen on your ship.~%"))
     (print-message *message-window* (format nil "You die of thirst, starvation, and asphyxiation.~%"))
     (print-message *message-window* (format nil "~%Your starship is a derelict in space.~%")))
    ;; FHOLE
    ((eql finish-reason 'destroyed-by-black-hole)
     (print-message *message-window* (format nil "Your ship is drawn to the center of the black hole.~%"))
     (print-message *message-window* (format nil "You are crushed into extremely dense matter.~%")))
    ;; FCREW
    ((eql finish-reason 'all-crew-killed)
     (print-message *message-window* (format nil "Your last crew member has died.~%")))
    ;; TODO - implement this
    ;;;; FCLOAK
    ;;((eql finish-reason 'cloaking-while-in-neutral-zone)
    ;; (setf *cloaking-violations* (1+ *cloaking-violations*))
    ;; (print-message *message-window* (format nil "You have violated the Treaty of Algeron.~%"))
    ;; (print-message *message-window* (format nil "The Romulan Empire can never trust you again.~%")))
    ;; should never reach this, but here we are
    (t
     (print-message *message-window* (format nil "Game over, man!~%~%"))))
  (when (and (not (eql finish-reason 'won))
             ;;(not (eql finish-reason 'cloaking-while-in-neutral-zone))
             *cloakedp*)
    (print-message *message-window* (format nil "Your ship was cloaked so your subspace radio did not receive anything.~%"))
    (print-message *message-window* (format nil "You may have missed some warning messages.~%")))
  ;; Win or lose, by this point the player did not survive.
  (setf *alivep* nil)
  ;; Downgrade the ship for score calculation purposes.
  ;; TODO - this is a poor hack, should count the number of ships destroyed
  ;; TODO - this can probably just be based on *alivep*
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
              (print-message *message-window* (format nil "~%As a result of your actions, a treaty with the Klingon~%"))
              (print-message *message-window* (format nil "Empire has been signed. The terms of the treaty are~%"))
              (if (>= (/ for against)
                      (+ 3.0 (random 1.0)))
                  (progn
                    (print-message *message-window* (format nil "favorable to the Federation.~%"))
                    (print-message *message-window* (format nil "Congratulations!~%")))
                  (print-message *message-window* (format nil "highly unfavorable to the Federation.~%"))))
            (print-message *message-window* (format nil "The Federation will be destroyed.~%"))))
      (progn
        (print-message *message-window* (format nil "Since you took the last Klingon with you, you are a~%"))
        (print-message *message-window* (format nil "martyr and a hero. Someday maybe they'll erect a~%"))
        (print-message *message-window* (format nil "statue in your memory. Rest in peace, and try not~%"))
        (print-message *message-window* (format nil "to think about pigeons.~%"))
        (setf *game-won-p* t)))
  (score))

(defun kaboom () ; C: kaboom(void)

  (print-stars)
  (when (string= *ship* +enterprise+)
    ;; Extra stars so the lengths of the output lines are the same
    (print-message *message-window* "***") :print-slowly t)
  (print-message *message-window*
   (format nil "********* Entropy of ~A maximized *********~%"
           (format-ship-name)) :print-slowly t)
  (print-stars)
  (do ((whammo (* 25.0 *ship-energy*))
       (i 0 (1+ i)))
      ((<= i *enemies-here*))
    (when (<= (* (enemy-energy (aref *quadrant-enemies* i))
                 (enemy-distance(aref *quadrant-enemies* 1)))
              whammo)
      (dead-enemy (enemy-sector-coordinates (aref *quadrant-enemies* i))
                  (coord-ref *quadrant* (enemy-sector-coordinates (aref *quadrant-enemies* i)))
                  (enemy-sector-coordinates (aref *quadrant-enemies* i)))))
  (finish 'dilithium-crystals-failed))

(defun expran (average)
  "Generate a random number for a time in the future.
This function name and doc string should be more informative."

  ;; TODO - are these numbers reasonably sized?
  ;;(return-from expran (* (- average) (log (+ (expt 1 -7) (random 1.0)))))) ; C: -avrage*log(1e-7 + Rand())
  (return-from expran (* average (log (+ (expt 1 -7) (random 1.0)))))) ; C: -avrage*log(1e-7 + Rand())

(defun get-random-quadrant () ; C: coord randplace(int size)
  "Get a coordinate structure for a random quadrant."

  (make-quadrant-coordinate :x (random +galaxy-size+) :y (random +galaxy-size+)))

(defun get-random-sector () ; C: coord randplace(int size)
  "Get a coordinate structure for a random sector."

  (make-sector-coordinate :x (random +quadrant-size+) :y (random +quadrant-size+)))

(defun drop-entity-in-sector (entity) ; coord dropin(feature iquad)
  "Drop a game entity in a random sector in the current quadrant. Return the sector coordinates
of the entity."

  (do ((c (get-random-sector) (get-random-sector)))
      ((string= (coord-ref *quadrant* c) +empty-sector+)
       (setf (coord-ref *quadrant* c) entity)
       (return-from drop-entity-in-sector c))))

(defun drop-klingon-in-sector () ; coord newkling(int i)
  "Drop a new Klingon into the current quadrant. Return the sector coordinates, distance from the
ship, and Klingon power."

  (let ((c (drop-entity-in-sector +klingon+)))
    (return-from drop-klingon-in-sector
      (values c
              (distance *ship-sector* c)
              (+ (* (random 1.0) 150.0) 300.0
                 (* 25.0 *skill-level*)))))) ; Rand()*150.0 +300.0 +25.0*game.skill

(defun drop-commander-in-sector ()
  "Drop a new Commander into the current quadrant. Return the sector coordinates, distance from the
ship, and Commander power."

  (let ((c (drop-entity-in-sector +commander+)))
    (return-from drop-commander-in-sector
      (values c
              (distance *ship-sector* c)
              (+ 950.0 (* (random 1.0) 400.0)
                 (* 50.0 *skill-level*)))))) ; 950.0+400.0*Rand()+50.0*game.skill

(defun drop-super-commander-in-sector ()
  "Drop a new Super-commander into the current quadrant. Return the sector coordinates, distance
from the ship, and Super-commander power."

  (let ((c (drop-entity-in-sector +super-commander+)))
    (return-from drop-super-commander-in-sector
      (values c
              (distance *ship-sector* c)
              (+ 1175.0 (* (random 1.0) 400.0)
                 (* 125.0 *skill-level*)))))) ; 1175.0 + 400.0*Rand() + 125.0*game.skill

(defun drop-romulan-in-sector ()
  "Drop a new Romulan into the current quadrant. Return the sector coordinates, distance from the
ship, and Romulan power."

  (let ((c (drop-entity-in-sector +romulan+)))
    (return-from drop-romulan-in-sector
      (values c
              (distance *ship-sector* c)
              (+ (* (random 1.0) 400.0) 450.0
                 (* 50.0 *skill-level*)))))) ; Rand()*400.0 + 450.0 + 50.0*game.skill

(defun drop-space-thing-in-sector ()
  "Drop a Space Thing into the current quadrant. Return the sector coordinates, distance from the
ship, and Thing power."

  (let ((c (drop-entity-in-sector +thing+)))
    (return-from drop-space-thing-in-sector
      (values c
              (distance *ship-sector* c)
              (+ (* (random 1.0) 6000.0) 500.0
                 *skill-level*))))) ; Rand()*6000.0 +500.0 +250.0*game.skill

(defun drop-tholian-in-sector ()
  "Drop a Tholian into the current quadrant. Tholians only occupy the perimeter of a quadrant.
Return the sector coordinates, distance from the ship, and Tholian power."

  (do ((sector-ok-p nil)
       x y c)
      (sector-ok-p
       (setf (aref *quadrant* x y) +tholian+)
       (setf c (make-sector-coordinate :x x :y y))
       (return-from drop-tholian-in-sector
         (values c
                 (distance *ship-sector* c)
                 (+ (* (random 1.0) 400.0) 100.0
                    (* 25.0 *skill-level*))))) ; Rand()*400.0 +100.0 +25.0*game.skill
    (if (> (random 1.0) 0.5)
        (setf x (- +quadrant-size+ 1))
        (setf x 0))
    (if (> (random 1.0) 0.5)
        (setf y (- +quadrant-size+ 1))
        (setf y 0))
    (if (string= (aref *quadrant* x y) +empty-sector+)
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
      (do ((j 0 (1+ j))
           temp)
          ((< j *enemies-here*))
        (when (> (enemy-distance (aref *quadrant-enemies* j))
                 (enemy-distance (aref *quadrant-enemies* (1+ j))))
          (setf exchanged t)
          (setf temp (aref *quadrant-enemies* j))
          (setf (aref *quadrant-enemies* j) (aref *quadrant-enemies* (1+ j)))
          (setf (aref *quadrant-enemies* (1+ j)) temp))))))

(defun new-quadrant (&key (show-thing t))
  "Set up a new quadrant when it is entered or re-entered. The thing should only be shown when the
player has reached a base by abandoning ship or using the SOS command."

  (setf *just-in-p* t)
  (setf *klingons-here* 0)
  (setf *commanders-here* 0)
  (setf *super-commanders-here* 0)
  (setf *romulans-here* 0)
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

  ;; Cope with supernova
  (unless (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
    ;; Clear/initialize quadrant
    (do ((i 0 (1+ i)))
        ((>= i +quadrant-size+))
      (do ((j 0 (1+ j)))
          ((>= j +quadrant-size+))
        (setf (aref *quadrant* i j) +empty-sector+)))

    (setf *klingons-here*
          (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)))
    (setf *romulans-here*
          (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)))
    (setf *enemies-here* (+ *klingons-here* *romulans-here*))

    ;; Position starship. Do this first, all sectors are still empty.
    (setf (coord-ref *quadrant* *ship-sector*) *ship*)

    (when (or (> (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0)
              (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0))
      (let ((remaining-klingons (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)))
            (next-index 0))
        ;; Put the super-commander in the quadrant if present
        (when (and *super-commander-quadrant*
                   (coord-equal *ship-quadrant* *super-commander-quadrant*))
          (multiple-value-bind (coordinates distance power) (drop-super-commander-in-sector)
            (setf (aref *quadrant-enemies* next-index) (make-enemy :energy power
                                                                   :distance distance
                                                                   :average-distance distance
                                                                   :sector-coordinates coordinates)))
          (incf next-index 1)
          (incf *super-commanders-here* 1)
          (decf remaining-klingons 1))
        ;; Put a Commander in the quadrant if there is one.
        (dolist (cq *commander-quadrants*)
          (when (coord-equal *ship-quadrant* cq)
            (multiple-value-bind (coordinates distance power) (drop-commander-in-sector)
              (setf (aref *quadrant-enemies* next-index) (make-enemy :energy power
                                                                     :distance distance
                                                                     :average-distance distance
                                                                     :sector-coordinates coordinates)))
            (incf next-index 1)
            (incf *commanders-here* 1)
            (decf remaining-klingons 1)))
        ;; Position ordinary Klingons
        (do ((i 1 (1+ i)))
            ((> i remaining-klingons))
          (multiple-value-bind (coordinates distance power) (drop-klingon-in-sector)
            (setf (aref *quadrant-enemies* next-index) (make-enemy :energy power
                                                                   :distance distance
                                                                   :average-distance distance
                                                                   :sector-coordinates coordinates)))
          (incf next-index 1))
        ;; Put in Romulans if needed
        (do ((r 1 (1+ r))) ; This is a count
            ((> r (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*))))
          (multiple-value-bind (coordinates distance power) (drop-romulan-in-sector)
            (setf (aref *quadrant-enemies* next-index) (make-enemy :energy power
                                                                   :distance distance
                                                                   :average-distance distance
                                                                   :sector-coordinates coordinates)))
          (incf next-index 1))))

    ;; If quadrant needs a starbase then put it in.
    (when (> (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*)) 0)
      (setf *base-sector* (drop-entity-in-sector +starbase+)))

    ;; If quadrant needs a planet then put it in
    (when (assoc *ship-quadrant* *planets* :test #'coord-equal) ; non-nil when planet exists
      (setf *planet-coord* *ship-quadrant*)
      (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
        (if (planet-inhabitedp p)
            (setf *current-planet* (drop-entity-in-sector +world+))
            (setf *current-planet* (drop-entity-in-sector +planet+)))))

    ;; Check for condition
    (update-condition)

    ;; And finally the stars
    (do ((i 1 (1+ i))) ; another count
        ((> i (quadrant-stars (coord-ref *galaxy* *ship-quadrant*))))
      (drop-entity-in-sector +star+))

    ;; Check for Romulan Neutral Zone: Romulans present, no Klingons, and no starbases.
    (when (and (> (quadrant-romulans (coord-ref *galaxy* *ship-quadrant*)) 0)
               (= (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 0)
               (= (quadrant-starbases (coord-ref *galaxy* *ship-quadrant*)) 0))
      (setf *romulan-neutral-zone-p* t)
      (when (subspace-radio-available-p)
        (print-message *message-window* (format nil "~%LT. Uhura- \"Captain, an urgent message.~%"))
        (print-message *message-window* (format nil "  I'll put it on audio.\"  CLICK~%"))
        (print-message *message-window* (format nil "~%INTRUDER! YOU HAVE VIOLATED THE ROMULAN NEUTRAL ZONE.~%"))
        (print-message *message-window* (format nil "LEAVE AT ONCE, OR YOU WILL BE DESTROYED!~%"))))

    ;; Put in THING if needed
    (when (and show-thing
               (coord-equal *thing-location* *ship-quadrant*))
      (setf *thing-location* (get-random-quadrant))
      (setf *things-here* 1)
      (multiple-value-bind (coordinates distance power) (drop-space-thing-in-sector)
        (setf (aref *quadrant-enemies* *enemies-here*) (make-enemy :energy power
                                                                   :distance distance
                                                                   :average-distance distance
                                                                   :sector-coordinates coordinates))
        (incf *enemies-here* 1)) ; TODO - should be next-index, not *enemies-here*
      (unless (damagedp +short-range-sensors+)
        (print-message *message-window* (format nil "Mr. Spock- \"Captain, this is most unusual.~%"))
        (print-message *message-window* (format nil "    Please examine your short-range scan.\"~%"))))

    ;; Decide if quadrant needs a Tholian
    (when (or (and (< *skill-level* +good+)
                   (<= (random 1.0) 0.02)) ; Lighten up if skill is low
              (and (= *skill-level* +good+)
                   (<= (random 1.0) 0.05))
              (and (> *skill-level* +good+)
                   (<= (random 1.0) 0.08)))
      (setf *tholians-here* 1)
      (multiple-value-bind (coordinates distance power) (drop-tholian-in-sector)
        (setf *tholian-sector* coordinates)
        ;; If present, the Tholian is always the last item in the array of enemies.
        ;; TODO - don't use a special case ("last item in the array") to identify the Tholian
        (setf (aref *quadrant-enemies* *enemies-here*) (make-enemy :energy power
                                                                   :distance distance
                                                                   :average-distance distance
                                                                   :sector-coordinates coordinates)) ; TODO - should be next-index, not *enemies-here*
        (incf *enemies-here* 1))
      ;; Reserve unoccupied corners
      (when (string= (aref *quadrant* 0 0) +empty-sector+)
        (setf (aref *quadrant* 0 0) +reserved+))
      (when (string= (aref *quadrant* 0 (- +quadrant-size+ 1)) +empty-sector+)
        (setf (aref *quadrant* 0 (- +quadrant-size+ 1)) +reserved+))
      (when (string= (aref *quadrant* (- +quadrant-size+ 1) 0) +empty-sector+)
        (setf (aref *quadrant* (- +quadrant-size+ 1) 0) +reserved+))
      (when (string= (aref *quadrant* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +empty-sector+)
        (setf (aref *quadrant* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +reserved+)))

    (sort-klingons)

    ;; Put in a few black holes
    (do ((i 0 (1+ i)))
        ((> i 3))
      (when (> (random 1.0) 0.5)
        (drop-entity-in-sector +black-hole+)))

    ;; Take out X's in corners if Tholian present
    (when (> *tholians-here* 0)
      (when (string= (aref *quadrant* 0 0) +reserved+)
        (setf (aref *quadrant* 0 0) +empty-sector+))
      (when (string= (aref *quadrant* 0 (- +quadrant-size+ 1)) +reserved+)
        (setf (aref *quadrant* 0 (- +quadrant-size+ 1)) +empty-sector+))
      (when (string= (aref *quadrant* (- +quadrant-size+ 1) 0) +reserved+)
        (setf (aref *quadrant* (- +quadrant-size+ 1) 0) +empty-sector+))
      (when (string= (aref *quadrant* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +reserved+)
        (setf (aref *quadrant* (- +quadrant-size+ 1) (- +quadrant-size+ 1)) +empty-sector+)))))

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
        (print-message *message-window* (format nil "You cannot abandon Ye Faerie Queene.~%"))
        (return-from abandon-ship nil))
      ;; Must take shuttle craft to exit
      (cond
        ((string= *ship* +faerie-queene+) ; C: game.damage[DSHUTTL]==-1
         (print-message *message-window* (format nil "Ye Faerie Queene has no shuttle craft.~%"))
         (return-from abandon-ship nil))

        ((< (aref *device-damage* +shuttle+) 0)
         (print-message *message-window* (format nil "Shuttle craft now serving Big Macs.~%"))
         (return-from abandon-ship nil))

        ((> (aref *device-damage* +shuttle+) 0)
         (print-message *message-window* (format nil "Shuttle craft damaged.~%"))
         (return-from abandon-ship nil))

        (*landedp*
         (print-message *message-window* (format nil "You must be aboard the ship.~%"))
         (return-from abandon-ship nil))

        ((not (eql *shuttle-craft-location* 'on-ship))
         (print-message *message-window* (format nil "Shuttle craft not currently available.~%"))
         (return-from abandon-ship nil))))

  ;; Print abandon ship messages
  (print-message *message-window* (format nil "~%***ABANDON SHIP!  ABANDON SHIP!~%") :print-slowly t)
  (print-message *message-window* (format nil "~%***ALL HANDS ABANDON SHIP!~%~%~%") :print-slowly t)
  ;; TODO - this isn't consistent with the "Entire crew.." message below
  (print-message *message-window* (format nil "Captain and crew escape in shuttle craft.~%"))
  (when (= (length *base-quadrants*) 0)
    ;; Oops! no place to go...
    (finish 'abandon-ship-with-no-starbases)
    (return-from abandon-ship nil))
  ;; Dispose of crew.
  ;; Before the introduction of inhabited planets the message was
  ;; "Remainder of ship's complement beam down"
  ;; "to nearest habitable planet."
  (let ((p (rest (assoc *ship-quadrant* *planets* :test #'coord-equal)))) ; non-nil if planet exists
    (if (and p
             (not (damagedp +transporter+)))
        (print-message *message-window* (format nil "Remainder of ship's complement beam down to ~A.~%"
                               (planet-name p)))
        (progn
          (print-message *message-window* (format nil "Entire crew of ~A left to die in outer space.~%" *crew*))
          (incf *casualties* *crew*)
          (incf *abandoned-crew* *crew*))))
  ;; If at least one base left, give 'em the Faerie Queene
  (setf *dilithium-crystals-on-board-p* nil) ; crystals are lost
  (setf *probes-available* 0) ; No probes
  (print-message *message-window* (format nil "~%You are captured by Klingons and released to~%"))
  (print-message *message-window* (format nil "the Federation in a prisoner-of-war exchange.~%"))
  ;; Set up quadrant and position FQ adjacent to base
  ;; Select a random base to be the new start
  (let ((nth-base (truncate (* (length *base-quadrants*) (random 1.0)))))
    (unless (coord-equal *ship-quadrant* (nth nth-base *base-quadrants*))
      (setf *ship-quadrant* (nth nth-base *base-quadrants*))
      (setf (coordinate-x *ship-sector*) (/ +quadrant-size+ 2))
      (setf (coordinate-y *ship-sector*) (/ +quadrant-size+ 2))
      (new-quadrant)))
  ;; position next to base by trial and error
  ;; TODO - in theory this could fail if there is no empty sector next to the base, or if the RNG
  ;;        doesn't find an empty sector. Instead, select a random empty sector (not "randomly
  ;;        select a sector and check if it's empty"), and run new-quadrant again if none exists
  (setf (coord-ref *quadrant* *ship-sector*) +empty-sector+)
  (do ((positionedp nil))
      (positionedp)
    (do ((count 0 (1+ count)))
        ((or (>= count 100) ; previously +quadrant-size+, don't give up so easily
             positionedp))
      (setf (coordinate-x *ship-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-x *base-sector*))))
      (setf (coordinate-y *ship-sector*) (truncate (+ (* 3.0 (random 1.0)) -1 (coordinate-y *base-sector*))))
      (when (and (valid-p *ship-sector*)
                 (string= (coord-ref *quadrant* *ship-sector*) +empty-sector+))
        (setf positionedp t))) ; found a spot
    (unless positionedp
      (setf (coordinate-x *ship-sector*) (/ +quadrant-size+ 2))
      (setf (coordinate-y *ship-sector*) (/ +quadrant-size+ 2))
      (new-quadrant)))
  ;; Get new commission
  (setf *ship* +faerie-queene+)
  (setf (coord-ref *quadrant* *ship-sector*) *ship*)
  (setf *crew* +full-crew+)
  (print-message *message-window* (format nil "Starfleet puts you in command of another ship,~%"))
  (print-message *message-window* (format nil "the Faerie Queene, which is antiquated but,~%"))
  (print-message *message-window* (format nil "still useable.~%"))
  (when *dilithium-crystals-on-board-p*
    (print-message *message-window* (format nil "The dilithium crystals have been moved.~%")))
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

;; There is no undock operation, other than moving the ship. This is by intent, for gameplay balance.
;; Since docking costs no time, a player could undock, fire weapons, and then dock again to resupply
;; and repair in relative safety. Requiring movement to undock gives enemies a chance to attack.
(defun dock () ; C: dock(bool verbose)
  "Dock the ship at a starbase."

  (skip-line *message-window*)
  (cond
    (*dockedp*

     (print-message *message-window* (format nil "Already docked.~%")))

    (*in-orbit-p*
     (print-message *message-window* (format nil "You must first leave standard orbit.~%")))

    ((or (not *base-sector*)
         (> (abs (- (coordinate-x *ship-sector*) (coordinate-x *base-sector*))) 1)
         (> (abs (- (coordinate-y *ship-sector*) (coordinate-y *base-sector*))) 1))
     (print-message *message-window* (format nil "~A not adjacent to base.~%" (format-ship-name))))

    (*cloakedp*
     (print-message *message-window* (format nil "You cannot dock while cloaked.~%")))

    (t
     (setf *dockedp* t)
     (print-message *message-window* (format nil "Docked.~%"))
     (setf *action-taken-p* t)
     (when (< *ship-energy* *initial-energy*) ; Keep energy overload from dilithium crystals
       (setf *ship-energy* *initial-energy*))
     (setf *shield-energy* *initial-shield-energy*)
     (setf *torpedoes* *initial-torpedos*)
     (setf *life-support-reserves* *initial-life-support-reserves*)
     (setf *crew* +full-crew+)
     (when (> (- *brig-capacity* *brig-free*) 0)
       (print-message *message-window* (format nil "~D captured Klingons transferred to base.~%"
                              (- *brig-capacity* *brig-free*)))
       (incf *captured-klingons* (- *brig-capacity* *brig-free*))
       (setf *brig-free* *brig-capacity*))
     ;; TODO - Possible approach for base attack report seen: make each report an event and only
     ;;        report the event when the radio is functioning or after it has been repaired.
     (when (and (or (eql *super-commander-attacking-base* 'attacking)
                    (eql *super-commander-attacking-base* 'destroying)
                    (is-scheduled-p +commander-destroys-base+))
                (not *base-attack-report-seen-p*))
       ;; Get attack report from base
       (print-message *message-window*
                      (format nil "~%Lt. Uhura- \"Captain, an important message from the starbase:\"~%"))
       (attack-report)
       (setf *base-attack-report-seen-p* t)))))

(defun time-warp () ; C: timwrp(), /* let's do the time warp again */
  "Travel forward or backward in time."

  (print-message *message-window* (format nil "~%***TIME WARP ENTERED.~%"))
  (if (and *snapshot-taken-p*
           (< (random 1.0) 0.5))
      (progn
        ;; Go back in time
        (print-message *message-window* (format nil "You are traveling backwards in time ~A stardates.~%"
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
          (setf *super-commander-attacking-base* 'not-attacking)
          (unschedule-event +commander-destroys-base+)
          (unschedule-event +super-commander-destroys-base+)
          (setf *base-under-attack-quadrant* nil)
          (cond
            ;; Make sure Galileo is consistent -- Snapshot may have been taken when on planet,
            ;; which would give us two Galileos!
            ((and (eql *shuttle-craft-location* 'off-ship) ; The shuttle is on the planet
                  *shuttle-craft-quadrant*                 ; in the indicated quadrant,
                  (eql shuttle-craft-location 'on-ship)    ; and before the time warp
                  (string= *ship* +enterprise+))           ; was on the Enterprise
             (print-message *message-window* (format nil "Checkov-  \"Security reports the Galileo has disappeared, Sir!~%")))
            ;; Likewise, if in the original time the Galileo was abandoned but was on ship in the
            ;; earlier time line, it would have vanished -- let's restore it.
            ((and (not (eql shuttle-craft-location 'on-ship))
                  (eql *shuttle-craft-location* 'on-ship))
             (if (string= *ship* +enterprise+) ; Can't restore to the FQ
                 (print-message *message-window* (format nil "Checkov-  \"Security reports the Galileo has reappeared in the dock!\"~%"))
                 (progn
                   (setf *shuttle-craft-location* 'off-ship)
                   (setf *shuttle-craft-quadrant* nil))))))
          ;; There used to be code to do the actual reconstruction here,
          ;; but the starchart is now part of the snapshotted galaxy state.
          (print-message *message-window* (format nil "Spock has reconstructed a correct star chart from memory~%")))
      (progn
        ;; Go forward in time
        (setf *time-taken-by-current-operation* (* -0.5 *initial-time* (log (random 1.0))))
        (print-message *message-window* (format nil "You are traveling forward in time ~A stardates.~%" *time-taken-by-current-operation*))
        ;; Cheat to make sure no tractor beams occur during time warp
        (postpone-event +tractor-beam+ *time-taken-by-current-operation*)
        (incf (aref *device-damage* +subspace-radio+) *time-taken-by-current-operation*)))
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
      (when (or (string= (aref *quadrant* new-x new-y) +empty-sector+)
                (string= (aref *quadrant* new-x new-y) +tholian-web+))
        (setf (aref *quadrant* new-x new-y) +tholian-web+)
        (cond
          ;; Move in x axis
          ((/= (coordinate-x *tholian-sector*) new-x)
           (do ((incr-x (/ (abs (- new-x (coordinate-x *tholian-sector*)))
                           (- new-x (coordinate-x *tholian-sector*)))))
               ((= (coordinate-x *tholian-sector*) new-x))
             (setf (coordinate-x *tholian-sector*) incr-x)
             (when (string= (coord-ref *quadrant* *tholian-sector*) +empty-sector+)
               (setf (coord-ref *quadrant* *tholian-sector*) +tholian-web+))))
          ;; Move in y axis
          ((/= (coordinate-y *tholian-sector*) new-y)
           (do ((incr-y (/ (abs (- new-y (coordinate-y *tholian-sector*)))
                          (- new-y (coordinate-y *tholian-sector*)))))
               ((= (coordinate-y *tholian-sector*) new-y))
             (setf (coordinate-y *tholian-sector*) incr-y)
             (when (string= (coord-ref *quadrant* *tholian-sector*) +empty-sector+)
               (setf (coord-ref *quadrant* *tholian-sector*) +tholian-web+)))))
        (setf (coord-ref *quadrant* *tholian-sector*) +tholian+)
        ;; TODO - This is fragile because the Tholian must be the last item in the array.
        ;;        Sometimes an enemy is deleted from an array by moving the last one in the
        ;;        array to the position of the deleted enemy. See other TODOs to find these.
        (setf (enemy-sector-coordinates (aref *quadrant-enemies* *enemies-here*)) *tholian-sector*)
        ;; Check to see if all holes plugged
        (do ((i 1 (1+ i))
             (all-holes-plugged-p t))
            ((or (>= i +quadrant-size+)
                 (not all-holes-plugged-p))
             (when all-holes-plugged-p
               ;; All plugged up -- Tholian splits
               (setf (coord-ref *quadrant* *tholian-sector*) +tholian-web+)
               (drop-entity-in-sector +black-hole+)
               (print-message *message-window* (format nil "***Tholian at ~A completes web.~%" (format-sector-coordinates *tholian-sector*)))
               (setf *tholians-here* 0)
               (setf *enemies-here* (1- *enemies-here*))))
          (when (and (string/= (aref *quadrant* 1 i) +tholian-web+)
                     (string/= (aref *quadrant* 1 i) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant* +quadrant-size+ i) +tholian-web+)
                     (string/= (aref *quadrant* +quadrant-size+ i) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant* i 1) +tholian-web+)
                     (string/= (aref *quadrant* i 1) +tholian+))
            (setf all-holes-plugged-p nil))
          (when (and (string/= (aref *quadrant* i +quadrant-size+ ) +tholian-web+)
                     (string/= (aref *quadrant* i +quadrant-size+) +tholian+))
            (setf all-holes-plugged-p nil)))))))

(defun try-exit (look enemy-letter enemy-index running-away-p) ; C: bool tryexit(coord look, int ienm, int loccom, bool irun)
  "A Klingon attempts to leave the current quadrant. Return true if successful."

  (let ((destination-quadrant (make-quadrant-coordinate))) ; C: iq
    (setf (coordinate-x destination-quadrant) (+ (coordinate-x *ship-quadrant*)
                                                 (/ (+ (coordinate-x look) (1- +quadrant-size+))
                                                    (1- +quadrant-size+))))
    (setf (coordinate-y destination-quadrant) (+ (coordinate-y *ship-quadrant*)
                                                 (/ (+ (coordinate-y look) (1- +quadrant-size+))
                                                    (1- +quadrant-size+))))
    ;; Check for reasons why no can do
    (when (or (not (valid-p destination-quadrant))  ; negative energy barrier
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
      (when (> (enemy-energy (aref *quadrant-enemies* enemy-index)) 1000.0)
        (return-from try-exit nil)))

    ;; Print escape message and move out of quadrant
    ;; We know this if either short or long range sensors are working
    (when (or (not (damagedp +short-range-sensors+))
              (not (damagedp +long-range-sensors+))
              *dockedp*)
      (print-message *message-window* (format nil "***~A at ~A escapes to ~A (and regains strength).~%"
                             (letter-to-name enemy-letter)
                             (format-sector-coordinates
                              (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index)))
                             (format-quadrant-coordinates destination-quadrant))))
    ;; Handle local matters related to escape
    (setf (coord-ref *quadrant*
                     (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index)))
          +empty-sector+)
    ;; TODO - this takes the last enemy in the array and puts it in the index position of the
    ;;        escaping enemy, in effect removing the escaping enemy. However, if the last enemy
    ;;        is a Tholian then the special location of the Tholian (last element in the array)
    ;;        is not preserved. Fix this by shifting all elements, or finding a better way to
    ;;        handle the Tholian.
    (setf (aref *quadrant-enemies* enemy-index) (aref *quadrant-enemies* (1- *enemies-here*)))
    (decf *klingons-here* 1)
    (decf *enemies-here* 1)
    (update-condition)
    ;; Handle global matters related to escape
    (decf (quadrant-klingons (coord-ref *galaxy* *ship-quadrant*)) 1)
    (incf (quadrant-klingons (coord-ref *galaxy* destination-quadrant)) 1)
    (if (string= enemy-letter +super-commander+)
        (progn
          (setf *super-commanders-here* 0)
          (setf *super-commander-attack-enterprise-p* nil)
          (setf *attempted-escape-from-super-commander-p* nil)
          (setf *super-commander-attacking-base* 'not-attacking)
          (schedule-event +move-super-commander+ 0.2777)
          (unschedule-event +super-commander-destroys-base+)
          (setf *super-commander-quadrant* destination-quadrant))
        (progn
          (setf *commander-quadrants* (remove *ship-quadrant* *commander-quadrants* :test #'coord-equal))
          (setf *commander-quadrants* (nconc *commander-quadrants* destination-quadrant))
          (decf *commanders-here* 1))))
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

  (let (enemy-dist ; C: dist1
        (run-away nil) ; C: irun
        (motion 0) ; C: motion
        next-sector) ; C: next
    (setf enemy-dist (enemy-distance (aref *quadrant-enemies* enemy-index)))
    (let (forces) ; C: forces
      ;; If SC, check with spy to see if should hi-tail it
      (if (and (string= enemy-letter +super-commander+)
               (or (<= (enemy-energy (aref *quadrant-enemies* enemy-index)) 500.0)
                   (and *dockedp*
                        (not (damagedp +photon-torpedoes+)))))
          (progn
            (setf run-away t)
            (setf motion (- +quadrant-size+)))
          (progn
            ;; Decide whether to advance, retreat, or hold position
            (let (enemy-multiplier) ; C: nbaddys
              ;; This should probably be just game.comhere + game.ishere
              (if (>= *skill-level* +expert+)
                  (setf enemy-multiplier (truncate (/ (+ (* *commanders-here* 2)
                                                         (* *super-commanders-here* 2)
                                                         (* *klingons-here* 1.23)
                                                         (* *romulans-here* 1.5))
                                                      2.0)))
                  (setf enemy-multiplier (+ *commanders-here* *super-commanders-here*)))
              (setf forces (+ (enemy-energy (aref *quadrant-enemies* enemy-index))
                              (* *enemies-here* 100.0)
                              (* (1- enemy-multiplier) 400))))
            (unless *shields-are-up-p*
              (incf forces 1000)) ; Good for enemy if shield is down!
            (if (or (not (damagedp +phasers+))
                    (not (damagedp +photon-torpedoes+)))
                (progn
                  (if (damagedp +phasers+) ; phasers damaged
                      (incf forces 300.0)
                      (decf forces (* (- *ship-energy* 2500.0) 2.0)))
                  (if (damagedp +photon-torpedoes+) ; photon torpedoes damaged
                      (incf forces 300.0)
                      (decf forces (* *torpedoes* 50.0))))
                ;; Phasers and photon tubes both out!
                (incf forces 1000))
            (if (and (<= forces 1000.0)
                     (not *dockedp*)) ; Typical situation
                (setf motion (- (/ (+ forces (* 200.0 (random 1.0))) 150.0) 5.0))
                (progn
                  (when (> forces 1000.0) ; Very strong -- move in for kill
                    (setf motion (+ (* (- 1.0 (expt (random 1.0) 2)) enemy-dist) 1.0)))
                  (when *dockedp* ; Protected by base -- back off!
                    (decf motion (* *skill-level* (- 2.0 (expt (random 1.0) 2)))))))
            ;; Don't move if no motion
            (when (= motion 0)
              (return-from move-one-enemy nil))
            ;; Limit motion according to skill
            (when (> (abs motion) *skill-level*)
              (if (< motion 0)
                  (setf motion (- *skill-level*))
                  (setf motion *skill-level*))))))
    ;; Calculate preferred number of steps
    (let (number-of-steps ; C: nsteps
          maximum-distance ; C: mdist, Nearest integer distance
          delta-x ; C: mx
          delta-y) ; C: my
      (setf number-of-steps (if (< motion 0) (- motion) motion))
      (setf maximum-distance (truncate (+ enemy-dist 0.5)))
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
           (look (make-sector-coordinate)); C: look
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

            ((string/= (coord-ref *quadrant* look) +empty-sector+)
             ;; See if we should ram ship
             (when (and (string= (coord-ref *quadrant* look) *ship*)
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
    (setf (coord-ref *quadrant* enemy-sector) +empty-sector+)
    (setf (coord-ref *quadrant* next-sector) enemy-letter)
    (unless (coord-equal next-sector enemy-sector)
      ;; It moved
      (setf (enemy-sector-coordinates (aref *quadrant-enemies* enemy-index)) next-sector)
      (setf (enemy-distance (aref *quadrant-enemies* enemy-index))
            (distance *ship-sector* next-sector))
      (setf (enemy-average-distance (aref *quadrant-enemies* enemy-index))
            (distance *ship-sector* next-sector))
      (when (or (not (damagedp +short-range-sensors+))
                *dockedp*)
        (print-message *message-window* (format nil "***~A from ~A ~A to ~A~%"
                               (letter-to-name enemy-letter)
                               (format-sector-coordinates enemy-sector)
                               (if (< (enemy-distance (aref *quadrant-enemies* enemy-index))
                                      enemy-dist)
                                   "advances"
                                   "retreats")
                               (format-sector-coordinates next-sector)))))))

(defun move-enemies () ; C: void moveklings(void)
  "Klingon and Romulan tactical movement, that is, movement within the current quadrant."

  ;; Figure out which Klingon is the commander (or Supercommander) and do the move
  (when (> *commanders-here* 0)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant* enemy-sector) +commander+))
         (when (string= (coord-ref *quadrant* enemy-sector) +commander+)
           (move-one-enemy enemy-sector i +commander+)))
      (setf enemy-sector (enemy-sector-coordinates (aref *quadrant-enemies* i)))))
  (when (> *super-commanders-here* 0)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (string= (coord-ref *quadrant* enemy-sector) +super-commander+))
         (when (string= (coord-ref *quadrant* enemy-sector) +super-commander+)
           (move-one-enemy enemy-sector i +super-commander+)))
      (setf enemy-sector (enemy-sector-coordinates (aref *quadrant-enemies* i)))))
  ;; If skill level is high, move other Klingons and Romulans too!
  ;; Move these last so they can base their actions on what the commander(s) do.
  (when (>= *skill-level* +expert+)
    (do ((i 0 (1+ i))
         enemy-sector)
        ((or (>= i *enemies-here*)
             (or (string= (coord-ref *quadrant* enemy-sector) +klingon+)
                   (string= (coord-ref *quadrant* enemy-sector) +romulan+)))
         (when (or (string= (coord-ref *quadrant* enemy-sector) +klingon+)
                   (string= (coord-ref *quadrant* enemy-sector) +romulan+))
           (move-one-enemy enemy-sector i (coord-ref *quadrant* enemy-sector))))
      (setf enemy-sector (enemy-sector-coordinates (aref *quadrant-enemies* i)))))
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
                         (= *skill-level* +emeritus+))
                     torpedoes-ok-p)
            (move-enemies))
          ;; If no enemies remain after movement, we're done
          (unless (or (<= *enemies-here* 0)
                      (and (= *enemies-here* 1)
                           (>= *things-here* 1)
                           (not *thing-is-angry-p*)))
            (skip-line *message-window*)
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
                   (finish 'destroyed-in-battle) ; Returning home upon your shield, not with it...
                   (return-from attack-player nil))
                 (unless attack-attempted-p
                   ;; TODO - don't print this if the only enemy in the quadrant is a Tholian, or
                   ;;        if there is a space thing that is not angry
                   (print-message *message-window* (format nil "***Enemies decide against attacking your ship.~%")))
                 (when attack-attempted-p
                   (if damage-taken-p
                       ;; Print message if starship suffered hit(s)
                       (progn
                         (print-message *message-window* (format nil "~%Energy left ~D    shields "
                                                (truncate *ship-energy*)))
                         (cond
                           (*shields-are-up-p*
                            (print-message *message-window* "up "))
                           ((not (damagedp +shields+))
                            (print-message *message-window* "down "))
                           (t
                            (print-message *message-window* "damaged, "))))
                       ;; Shields fully protect ship
                       (print-message *message-window* "Enemy attack reduces shield strength to "))
                   (print-message *message-window* (format nil "~D%,   torpedoes left ~D~%"
                                          (truncate (* 100 (/ *shield-energy*
                                                              *initial-shield-energy*)))
                                          *torpedoes*))
                   ;; Check if anyone was hurt
                   (when (or (>= hit-max 200)
                             (>= hit-total 500))
                     (let ((casualties (truncate(* hit-total (random 1.0) 0.015)))) ; C: icas
                       (when (>= casualties 2)
                         (print-message *message-window* (format nil "~%Mc Coy-  \"Sickbay to bridge.  We suffered ~D casualties~%" casualties))
                         (print-message *message-window* (format nil "   in that last attack.\"~%"))
                         (incf *casualties* casualties)
                         (decf *crew* casualties))))))
              (setf enemy-sector (enemy-sector-coordinates (aref *quadrant-enemies* n)))
              (setf enemy (coord-ref *quadrant* enemy-sector))
              (unless (or (<= (enemy-energy (aref *quadrant-enemies* n)) 0) ; too weak to attack
                          (string= enemy +tholian+)
                          (and (string= enemy +thing+)
                               (not *thing-is-angry-p*)))
                ;; Compute hit strength and diminish shield power
                ;; Increase chance of photon torpedos if docked or enemy energy low
                (when *dockedp*
                  (setf torpedo-probability (* torpedo-probability 0.25)))
                (when (< (enemy-energy (aref *quadrant-enemies* n)) 500)
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
                        (setf hit (* (enemy-energy (aref *quadrant-enemies* n))
                                     (expt dust-factor
                                           (enemy-average-distance (aref *quadrant-enemies* n)))))
                        (setf (enemy-energy (aref *quadrant-enemies* n))
                              (* (enemy-energy (aref *quadrant-enemies* n)) 0.75))))
                    (progn ; Enemy uses photon torpedo
                      (setf attack-attempted-p t)
                      (setf course (* 1.90985 (atan (- (coordinate-y *ship-sector*) (coordinate-y enemy-sector))
                                                    (- (coordinate-x enemy-sector) (coordinate-x *ship-sector*)))))
                      (print-message *message-window* "***TORPEDO INCOMING")
                      (unless (damagedp +short-range-sensors+)
                        (if (<= *skill-level* +fair+)
                            (print-message *message-window* (format nil " From ~A at ~A  ~%"
                                                   (letter-to-name enemy)
                                                   (format-sector-coordinates enemy-sector)))
                            (print-message *message-window* (format nil " From ~A at ~A  ~%"
                                                   (letter-to-name enemy)
                                                   (format-coordinates enemy-sector)))))
                      (setf random-variation (- (* (+ (random 1.0) (random 1.0)) 0.5) 0.5))
                      (incf random-variation (* 0.002
                                                (enemy-energy (aref *quadrant-enemies* n))
                                                random-variation))
                      (setf hit (move-torpedo-within-quadrant course random-variation enemy-sector 1 1))
                      (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
                        (finish 'won)) ; Klingons did themselves in!
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
                      (decf *shield-energy* absorb)
                      (decf hit hit-to-shields)
                      ;; Tiny hits have no effect if the shields were strong enough
                      (when (and (> proportional-strength 0.1)
                                 (< hit (* 0.005 *ship-energy*)))
                        (setf hit 0))))
                  ;; Hit from this opponent got through shields, so take damage
                  (when (> hit 0)
                    (setf damage-taken-p t)
                    (print-message *message-window* (format nil "~D unit hit" (truncate hit)))
                    (when (or (and (damagedp +short-range-sensors+)
                                   (eql weapon 'phasers))
                              (<= *skill-level* +fair+))
                      (print-message *message-window* (format nil " on the ~A" (format-ship-name))))
                    (when (and (not (damagedp +short-range-sensors+))
                               (eql weapon 'phasers))
                      (if (<= *skill-level* +fair+)
                          (print-message *message-window*
                                         (format nil " from ~A at ~A~%" (letter-to-name enemy)
                                                 (format-sector-coordinates enemy-sector)))
                          (print-message *message-window*
                                         (format nil " from ~A at ~A~%" (letter-to-name enemy)
                                                 (format-coordinates enemy-sector)))))
                    ;; Decide if hit is critical
                    (when (> hit hit-max)
                      (setf hit-max hit))
                    (incf hit-total hit)
                    (apply-critical-hit hit)
                    (decf *ship-energy* hit))))
            ;; Reset distances to the ship. Enemy attacks could have moved either the player or
            ;; an enemy, for example if a star nova'd by an enemy buffets another enemy.
            (do ((n 0 (1+ n)))
                ((>= n *enemies-here*)
                 (sort-klingons))
              (setf (enemy-distance (aref *quadrant-enemies* n))
                    (distance (enemy-sector-coordinates (aref *quadrant-enemies* n)) *ship-sector*))
              (setf (enemy-average-distance (aref *quadrant-enemies* n))
                    (distance (enemy-sector-coordinates (aref *quadrant-enemies* n)) *ship-sector*)))))))))

;; TODO - Error: the ship doesn't always leave the quadrant, and no E appears in the short range scan
;; seems to occur when moving at warp 10 over a distance of 3 or so quadrants. The error was seen when
;; testing the time-warp code, by moving a few quadrants at warp 10.
;; TODO - when using curses, show the ship moving within the the current quadrant and withing the
;;        destination quadrant, and on the star chart when moving "over" quadrants.
(defun move-ship-within-quadrant (&key course distance (nova-push-p nil)) ; C: imove(bool novapush)
  "In-sector movement actions for warp and impulse drives. Supernova and tractor beam events
can occur."

  (when *in-orbit-p*
    (print-message *message-window* (format nil "Helmsman Sulu- \"Leaving standard orbit.\"~%"))
    (setf *in-orbit-p* nil))

  (setf *dockedp* nil)

  ;; TODO - n probably isn't needed: stop movement when full movement distance has been covered
  (let ((tractor-beam-scheduled-p nil)
        angle delta-x delta-y bigger
        n
        (s-coord (make-sector-coordinate :x 0 :y 0))) ; C: w

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
    (setf (coord-ref *quadrant* *ship-sector*) +empty-sector+)
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
      (unless (valid-p s-coord)
        ;; Allow a final enemy attack unless being pushed by a nova.
        ;; Stas Sergeev added the condition that attacks only happen
        ;; if Klingons are present and your skill is good.
        (when (and (not nova-push-p)
                   (not (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*)))
                   (> *enemies-here* 0)
                   (> *klingons-here* 0) ; Romulans don't get another attack
                   (> *skill-level* +good+)
                   (not *cloakedp*))
            ;; The ship has moved during this turn so update average distance before attacking
            (do ((m 0 (1+ m)))
                ((>= m *enemies-here*))
              (setf (enemy-average-distance (aref *quadrant-enemies* m))
                    (/ (+ (distance s-coord (enemy-sector-coordinates (aref *quadrant-enemies* m)))
                          (enemy-distance (aref *quadrant-enemies* m)))
                       2.0)))
            (attack-player)
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
               (incf *energy-barrier-crossings* 1)
               (when (>= *energy-barrier-crossings* 3) ; Three strikes -- you're out!
                 (finish '3-negative-energy-barrier-crossings)
                 (return-from move-ship-within-quadrant t))
               (print-message *message-window*
                (format nil "~%YOU HAVE ATTEMPTED TO CROSS THE NEGATIVE ENERGY BARRIER~%"))
               (print-message *message-window*
                (format nil "AT THE EDGE OF THE GALAXY.  THE THIRD TIME YOU TRY THIS,~%"))
               (print-message *message-window* (format nil "YOU WILL BE DESTROYED.~%"))))
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
        (unless tractor-beam-scheduled-p ; Tractor beam will change the quadrant
          (setf (coordinate-x *ship-quadrant*)
                (truncate (1- (/ (+ (coordinate-x s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-y *ship-quadrant*)
                (truncate (1- (/ (+ (coordinate-y s-coord) +quadrant-size+) +quadrant-size+))))
          (setf (coordinate-x *ship-sector*) (- (coordinate-x s-coord) (* +quadrant-size+ (coordinate-x *ship-quadrant*))))
          (setf (coordinate-y *ship-sector*) (- (coordinate-y s-coord) (* +quadrant-size+ (coordinate-y *ship-quadrant*))))
          (print-message *message-window*
           (format nil "~%Entering ~A.~%" (format-quadrant-coordinates *ship-quadrant*)))
          (new-quadrant :show-thing nil)
          (when (> *skill-level* +novice+)
            (attack-player)))
        (return-from move-ship-within-quadrant t))
      ;; Object encountered in flight path
      (when (string/= (coord-ref *quadrant* s-coord) +empty-sector+)
        (setf distance (/ (distance *ship-sector* s-coord) (* +quadrant-size+ 1.0)))
        (cond
          ;; Ram enemy ship
          ((or (string= (coord-ref *quadrant* s-coord) +tholian+) ; Ram a Tholian
               (string= (coord-ref *quadrant* s-coord) +klingon+)
               (string= (coord-ref *quadrant* s-coord) +commander+)
               (string= (coord-ref *quadrant* s-coord) +super-commander+)
               (string= (coord-ref *quadrant* s-coord) +romulan+)
               (string= (coord-ref *quadrant* s-coord) +thing+))
           (setf (coordinate-x *ship-sector*) (coordinate-x s-coord))
           (setf (coordinate-y *ship-sector*) (coordinate-y s-coord))
           (ram :rammed-by-p nil :enemy (coord-ref *quadrant* s-coord)
                :enemy-coordinates *ship-sector*))
          ((string= (coord-ref *quadrant* s-coord) +black-hole+)
           (print-message *message-window* (format nil "~%***RED ALERT!  RED ALERT!~%") :print-slowly t)
           (print-message *message-window* (format nil "~%*** ~A pulled into black hole at ~A~%"
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
                    (finish 'destroyed-by-black-hole))
                (return-from move-ship-within-quadrant t))
             (when (> (aref *device-damage* index) 0) ; TODO - define a "count damaged devices" function?
               (1+ damaged-devices))))
          ;; Something else
          (t
           (let ((stop-energy (/ (* 50.0 distance) *time-taken-by-current-operation*)))
             (if (string= (coord-ref *quadrant* s-coord) +tholian-web+)
                 (print-message *message-window* (format nil "~%~A encounters Tholian web at ~A;~%"
                                        (format-ship-name) (format-sector-coordinates s-coord)))
                 (print-message *message-window* (format nil "~%~A blocked by object at ~A;~%"
                                        (format-ship-name) (format-sector-coordinates s-coord))))
             (print-message *message-window* (format nil "Emergency stop required ~,2F units of energy.~%" stop-energy))
             (setf (coordinate-x s-coord) (truncate (- prev-x delta-x))) ; simulate C float to int assignment
             (setf (coordinate-y s-coord) (truncate (- prev-y delta-y)))
             (decf *ship-energy* stop-energy)
             (when (<= *ship-energy* 0)
               (finish 'out-of-energy)
               (return-from move-ship-within-quadrant t)))))
        (setf movement-stopped-p t)))
    (setf (coordinate-x *ship-sector*) (coordinate-x s-coord))
    (setf (coordinate-y *ship-sector*) (coordinate-y s-coord))
    ;; Movement completed and no quadrant change -- compute new average enemy distances
    (setf (coord-ref *quadrant* *ship-sector*) *ship*)
    (when (> *enemies-here* 0)
      (do ((m 0 (1+ m))
           (final-distance 0))
          ((>= m *enemies-here*))
        (setf final-distance (distance *ship-sector*
                                       (enemy-sector-coordinates (aref *quadrant-enemies* m))))
        (setf (enemy-average-distance (aref *quadrant-enemies* m))
              (/ (+ final-distance (enemy-distance (aref *quadrant-enemies* m))) 2.0))
        (setf (enemy-distance (aref *quadrant-enemies* m)) final-distance))
      (sort-klingons)
      (unless (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
        (attack-player)))
    (update-condition)
    (update-windows)))

(defun get-probe-course-and-distance () ; C: void getcd(bool isprobe, int akey), for the probe
  "Get course direction and distance for moving the probe.

This program originally required input in terms of a (clock) direction and distance. Somewhere in
history, it was changed to cartesian coordinates. So we need to convert.  Probably \"manual\"
input should still be done this way -- it's a real pain if the computer isn't working! Manual mode
is still confusing because it involves giving x and y motions, yet the coordinates are always
displayed y - x, where +y is downward!"

  (let ((navigation-mode nil)
        delta-y delta-x
        direction
        distance)

    (when (damagedp +navigation-system+)
      (print-message *message-window* (format nil "Computer damaged; manual navigation only~%"))
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token
         input-item)
        (navigation-mode)
      (unless (input-available-p) ; input is "move" on first loop, nil thereafter
        (print-prompt "Manual or automatic: "))
      (setf input-item (scan-input))
      (if (numberp input-item) ; No input (<enter> key only) will loop
          (progn
            (unscan-input input-item) ; put it back so a coordinate pair can be read below
            (print-message *message-window* (format nil "Manual navigation assumed.~%")) ; Per the original docs
            (setf navigation-mode 'manual))
          (when input-item
            (setf token (match-token input-item (list "manual" "automatic")))
            (if token
                (if (string= token "manual")
                    (setf navigation-mode 'manual)
                    (setf navigation-mode 'automatic))
                (progn
                  (huh)
                  (clear-type-ahead-buffer)
                  (return-from get-probe-course-and-distance (values nil nil)))))))

    (if (eql navigation-mode 'automatic)
        (do (qx qy sx sy)
            (qx
             (when (and (not sx)
                        (not sy))
               ;; Only quadrant specified -- go to center of destination quadrant
               (setf sx 5)
               (setf sy 5))
             (if (and (valid-quadrant-p qx qy)
                      (valid-sector-p sx sy))
                 (progn
                   (skip-line *message-window*)
                   ;; Multiply sectors by 0.1 to scale them to the size of a quadrant
                   (setf delta-x (+ (- qy (coordinate-y *ship-quadrant*))
                                    (* 0.1 (- sy (coordinate-y *ship-sector*)))))
                   (setf delta-y (+ (- (coordinate-x *ship-quadrant*) qx)
                                    (* 0.1 (- (coordinate-x *ship-sector*) sx)))))
                 (progn
                   (huh)
                   (return-from get-probe-course-and-distance (values nil nil)))))
          (unless (input-available-p)
            (print-prompt "Target quadrant or quadrant&sector: ")
            (clear-type-ahead-buffer))
          (multiple-value-setq (qx qy) (scan-coordinate-pair))
          (when (two-input-items-available) ; both quadrant and sector specified
            (multiple-value-setq (sx sy) (scan-coordinate-pair))))
        (progn ; manual
          (do (input-item)
              (input-item
               (when (numberp input-item)
                 (setf delta-x input-item))
               (when (input-available-p)
                 (setf input-item (scan-input))
                 (when (numberp input-item)
                   (setf delta-y input-item)))
               (when (or (not delta-x) (not delta-y))
                 (huh)
                 (return-from get-probe-course-and-distance (values nil nil))))
            (unless (input-available-p)
              (print-prompt "X and Y displacements: ")
              (clear-type-ahead-buffer))
            (setf input-item (scan-input)))))
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
  (skip-line *message-window*)

  (when *landedp*
    (print-message *message-window* (format nil "Captain! You can't leave standard orbit until you~%"))
    (print-message *message-window* (format nil "are back aboard the ship.~%"))
    (return-from get-ship-course-and-distance (values -1.0 0)))

  (let ((navigation-mode nil)
        (need-prompt-p nil)
        (feedback-from 'chekov) ; Who acknowledges, and how, depends on movement mode and whether or not the player needed a prompt. It seems confusing.
        delta-y delta-x)

    (when (damagedp +navigation-system+)
      (print-message *message-window* (format nil "Computer damaged; manual movement only~%"))
      (setf navigation-mode 'manual)
      (clear-type-ahead-buffer))

    (do (token
         input-item)
        (navigation-mode)
      (unless (input-available-p) ; input is "move" on first loop, empty thereafter
        (print-prompt "Manual or automatic: ")
        (setf need-prompt-p t))
      (setf input-item (scan-input))
      (if (numberp input-item) ; No input (<enter> key only) will loop
          (progn
            (unscan-input input-item) ; put it back so a coordinate pair can be read below
            (print-message *message-window* (format nil "Manual movement assumed.~%")) ; Per the original docs
            (setf navigation-mode 'manual))
          (when input-item
            (setf token (match-token input-item (list "manual" "automatic")))
            (if token
                (progn
                  (if (string= token "manual")
                      (setf navigation-mode 'manual)
                      (setf navigation-mode 'automatic)))
                (progn
                  (huh)
                  (return-from get-ship-course-and-distance (values -1.0 0)))))))

    (if (eql navigation-mode 'automatic)
        (do (sx sy qx qy)
            (sx
             (if (and (valid-quadrant-p qx qy)
                      (valid-sector-p sx sy))
                 (progn
                   (if (eql feedback-from 'chekov)
                       (print-message *message-window* (format nil "Ensign Chekov- \"Course laid in, Captain.\"~%"))
                       (when need-prompt-p
                         ;; Displayed coordinates are integers, use truncate
                         (print-message *message-window* (format nil "Helmsman Sulu- \"Course locked in for ~A.\"~%"
                                                (format-sector-coordinates
                                                 (make-sector-coordinate :y (truncate sx)
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
          (unless (input-available-p)
            (print-prompt "Destination sector or quadrant&sector: ")
            (clear-type-ahead-buffer)
            (setf need-prompt-p t))
          (multiple-value-setq (sx sy) (scan-coordinate-pair))
          (when (two-input-items-available) ; both quadrant and sector specified
            (setf qx sx)
            (setf qy sy)
            (multiple-value-setq (sx sy) (scan-coordinate-pair)))
          (when (and (not qy)
                     (not qx))
            (setf qx (coordinate-x *ship-quadrant*))
            (setf qy (coordinate-y *ship-quadrant*))
            (setf feedback-from 'sulu)))
        (progn ; manual
          (do (input-item)
              (input-item
               (when (numberp input-item)
                 (setf delta-x input-item))
               (when (input-available-p)
                 (setf input-item (scan-input))
                 (when (numberp input-item)
                   (setf delta-y input-item)))
               (when (or (not delta-x)
                         (not delta-y))
                 (huh)
                 (return-from get-ship-course-and-distance (values -1.0 0)))
               (setf feedback-from 'sulu))
            (print-prompt "X and Y displacements: ")
            (clear-type-ahead-buffer)
            (setf input-item (scan-input)))))
    ;; Check for zero movement
    (when (and (= delta-x 0) (= delta-y 0))
      (return-from get-ship-course-and-distance (values -1.0 0)))
    (when (eql feedback-from 'sulu)
      (print-message *message-window* (format nil "Helmsman Sulu- \"Aye, Sir.\"~%")))
    (let ((course (* (atan delta-x delta-y) 1.90985932))
          (distance (sqrt (+ (expt delta-x 2) (expt delta-y 2)))))
      (when (< course 0.0)
        (setf course (+ course 12.0)))
    (return-from get-ship-course-and-distance (values course distance)))))

(defun move-under-impulse-power () ; C: impulse(void)

  (when (damagedp +impluse-engines+)
    (clear-type-ahead-buffer)
    (print-message *message-window*
                   (format nil "~%Engineer Scott- \"The impulse engines are damaged, Sir.\"~%"))
    (return-from move-under-impulse-power nil))

  (multiple-value-bind (course distance) (get-ship-course-and-distance)
    (when (= course -1.0) ; TODO test this
      (return-from move-under-impulse-power nil))

    (when (or (>= (+ 20.0 (* 100.0 distance)) *ship-energy*)
              (<= *ship-energy* 30.0))
      ;; Insufficient power for trip
      (print-message *message-window*
                     (format nil "~%First Officer Spock- \"Captain, the impulse engines~%"))
      (print-message *message-window*
                     (format nil "require 20.0 units to engage, plus 100.0 units per~%"))
      (if (> *ship-energy* 30.0)
          (print-message *message-window*
                         (format nil "quadrant.  We can go, therefore, a maximum of ~A quadrants.\"~%"
                                 (truncate (- (* 0.01 (- *ship-energy* 20.0)) 0.05))))
          (print-message *message-window*
                         (format nil "quadrant.  They are, therefore, useless.\"~%")))
      (clear-type-ahead-buffer)
      (return-from move-under-impulse-power nil))

    ;; Make sure enough time is left for the trip
    (when (>= (/ distance 0.095) *remaining-time*)
      (print-message *message-window*
                     (format nil "First Officer Spock- \"Captain, our speed under impulse~%"))
      (print-message *message-window* (format nil "power is only 0.95 sectors per stardate.~%"))
      (print-prompt "Are you sure we dare spend the time?\" ")
      (unless (get-y-or-n-p)
        (return-from move-under-impulse-power nil)))

    ;; Activate impulse engines and pay the cost
    ;; TODO - distance should be returned, the move-ship function changes it if the ship is tractor-beamed
    (move-ship-within-quadrant :course course :distance distance)
    (setf *action-taken-p* t)
    (when *all-done-p*
      (return-from move-under-impulse-power nil))
    (decf *ship-energy* (+ 20.0 (* 100.0 distance)))
    (setf *time-taken-by-current-operation* (/ distance 0.095)))

  (when (<= *ship-energy* 0)
    (finish 'out-of-energy)))

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
            (incf prev-x delta-x)
            (incf prev-y delta-y)
            (setf curr-x (round prev-x))
            (setf curr-y (round prev-y))
            (when (and (valid-sector-p curr-x curr-y)
                       (string/= (aref *quadrant* curr-x curr-y) +empty-sector+))
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
    (decf *ship-energy* (* distance (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))
    (when (<= *ship-energy* 0)
      (finish 'out-of-energy))
    (setf *time-taken-by-current-operation* (calculate-warp-movement-time :distance distance
                                                                          :warp-factor *warp-factor*))
    (when time-warp-p
      (time-warp))
    (when engine-damage-p
      (setf (aref *device-damage* +warp-engines+) (* *damage-factor* (+ (* 3.0 (random 1.0)) 1.0)))
      (print-message *message-window* (format nil "%Engineering to bridge--~%"))
      (print-message *message-window* (format nil "  Scott here.  The warp engines are damaged.~%"))
      (print-message *message-window* (format nil "  We'll have to reduce speed to warp 4.~%")))
    (setf *action-taken-p* t)))

(defun move-under-warp-drive () ;  C: warp(bool timewarp)

  (when *cloakedp*
    (clear-type-ahead-buffer)
    (print-message *message-window*
     (format nil "~%Engineer Scott- \"The warp engines cannot be used while cloaked, Sir.\"~%"))
    (return-from move-under-warp-drive nil))

  (when (> (aref *device-damage* +warp-engines+) 10.0)
    (clear-type-ahead-buffer)
    (print-message *message-window* (format nil "~%Engineer Scott- \"The warp engines are damaged, Sir.\"~%"))
    (return-from move-under-warp-drive nil))

  (when (and (damagedp +warp-engines+)
             (> *warp-factor* 4.0))
    (clear-type-ahead-buffer)
    (skip-line *message-window*)
    (print-message *message-window* (format nil "~%Engineer Scott- \"Sorry, Captain. Until this damage~%"))
    (print-message *message-window* (format nil "  is repaired, I can only give you warp 4.\"~%"))
    (return-from move-under-warp-drive nil))

  ;; Read in course and distance
  (multiple-value-bind (course distance) (get-ship-course-and-distance)
    (when (= course -1.0) ; TODO test this
      (return-from move-under-warp-drive nil))

    ;; TODO - put warp power calculations into a function?
    ;; Make sure starship has enough energy for the trip
    (let ((power (* (+ distance 0.05) (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))
          (iwarp (truncate (expt (/ *ship-energy* (+ distance 0.05)) 0.333333333))))
      (when (>= power *ship-energy*)
        ;; Insufficient power for trip
        (print-message *message-window* "~%Engineering to bridge--~%")
        (if (or (not *shields-are-up-p*)
                (> (* 0.5 power) *ship-energy*))
            (if (<= iwarp 0)
                (print-message *message-window*
                               (format nil
                                       "We can't do it, Captain. We don't have enough energy.~%"))
                (progn
                  (print-message *message-window*
                   (format nil "We don't have enough energy, but we could do it at warp ~A" iwarp))
                  (if *shields-are-up-p*
                      (print-message *message-window*
                                     (format nil ", if you'll lower the shields.~%"))
                      (print-message *message-window* (format nil ".~%")))))
            (print-message *message-window*
             (format nil "We haven't the energy to go that far with the shields up.~%")))
        (return-from move-under-warp-drive nil)))

    ;;Make sure enough time is left for the trip
    (setf *time-taken-by-current-operation* (calculate-warp-movement-time :distance distance
                                                                          :warp-factor *warp-factor*))
    (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
      (print-message *message-window*
                     (format nil "~%First Officer Spock- \"Captain, I compute that such~%"))
      (print-message *message-window* (format nil "  a trip would require approximately %~,2F~%"
                         (/ (* 100.0 *time-taken-by-current-operation*) *remaining-time*)))
      (print-message *message-window* (format nil " percent of our remaining time.\"~%~%"))
      (print-prompt "\"Are you sure this is wise?\" ")
      (unless (get-y-or-n-p)
        (setf *time-taken-by-current-operation* 0)
        (return-from move-under-warp-drive nil)))

    (execute-warp-move course distance)))

(defun launch-probe () ; C: probe(void)
  "Launch deep-space probe."

  ;; New code to launch a deep space probe
  (cond
    ((= *probes-available* 0)
     (clear-type-ahead-buffer)
     (if (string= *ship* +enterprise+)
         (print-message *message-window*
                        (format nil
                                "~%Engineer Scott- \"We have no more deep space probes, Sir.\"~%"))
         (print-message *message-window*
                        (format nil "~%Ye Faerie Queene has no deep space probes.~%"))))

    ((damagedp +deep-space-probe-launcher+)
     (clear-type-ahead-buffer)
     (print-message *message-window*
                    (format nil "~%Engineer Scott- \"The probe launcher is damaged, Sir.\"~%")))

    ((is-scheduled-p +move-deep-space-probe+)
     (clear-type-ahead-buffer)
     (if (subspace-radio-available-p)
         (print-message *message-window* (format nil "~%Uhura- \"The previous probe is still reporting data, Sir.\"~%"))
         (print-message *message-window* (format nil "~%Spock-  \"Records show the previous probe has not yet~%   reached its destination.\"~%"))))

    (t
     (let ((arm-probe-p nil))
       (unless (input-available-p)
         ;; Slow mode, so let Kirk know how many probes there are left
         (print-message *message-window* (format nil "~A probe~:P left.~%" *probes-available*))
         (print-prompt "Are you sure you want to fire a probe? ")
         (unless (get-y-or-n-p)
           (return-from launch-probe nil)))
       (setf arm-probe-p nil)
       (if (input-available-p)
           (let ((input-item (scan-input)))
             (cond ((numberp input-item)
                    (unscan-input input-item)) ; probably a quadrant coordinate, put it back

                   ((match-token input-item (list "yes"))
                    (setf arm-probe-p t))

                   ((match-token input-item (list "no"))
                    (setf arm-probe-p nil))

                   (t
                    (huh)
                    (return-from launch-probe nil))))
           (progn
             (print-prompt "Arm NOVAMAX warhead? ")
             (when (get-y-or-n-p)
               (setf arm-probe-p t))))
       (multiple-value-bind (direction distance) (get-probe-course-and-distance)
         (when (and (not direction)
                    (not distance))
           (return-from launch-probe nil))
         (setf *probe* (make-probe :is-armed-p arm-probe-p))
         (decf *probes-available* 1)
         (let ((angle (* (- 15.0 direction) 0.5235988))
               bigger)
           (setf (probe-inc-x *probe*) (- (sin angle))) ; C: game.probeinx = -sin(angle);
           (setf (probe-inc-y *probe*) (cos angle))
           (if (> (abs (probe-inc-x *probe*))
                  (abs (probe-inc-y *probe*)))
               (setf bigger (abs (probe-inc-x *probe*)))
               (setf bigger (abs (probe-inc-y *probe*))))
           (setf (probe-inc-x *probe*) (/ (probe-inc-x *probe*) bigger))
           (setf (probe-inc-y *probe*) (/ (probe-inc-y *probe*) bigger))
           ;; TODO - is the half-sector needed?
           (setf (probe-moves-remaining *probe*) (+ (* 10.0 distance bigger) 0.5))
           ;; We will use better packing than original: galaxy coordinates in sector units
           (setf (probe-x *probe*) (+ (* (coordinate-x *ship-quadrant*) +quadrant-size+)
                                      (coordinate-x *ship-sector*)))
           (setf (probe-y *probe*) (+ (* (coordinate-y *ship-quadrant*) +quadrant-size+)
                                      (coordinate-y *ship-sector*))))
         (schedule-event +move-deep-space-probe+ 0.01))) ; Time to move one sector
     (print-message *message-window*
                    (format nil "Ensign Chekov-  \"The deep space probe is launched, Captain.\"~%"))
     (setf *action-taken-p* t))))

(defun set-warp-factor () ; C: setwarp(void)
  "Change the warp factor."

  (let (input-item)
    (when (input-available-p)
        (setf input-item (scan-input)))
    (do ()
        (input-item)
      (print-prompt "Warp factor: ")
      (setf input-item (scan-input)))
    (if (numberp input-item)
        (cond
          ((> (aref *device-damage* +warp-engines+) 10.0)
           (print-message *message-window* (format nil "~%Warp engines inoperative.~%")))

          ((and (damagedp +warp-engines+) (> input-item 4.0))
           (print-message *message-window* (format nil "~%Engineer Scott- \"I'm doing my best, Captain,~%"))
           (print-message *message-window* (format nil "  but right now we can only go warp 4.\"~%")))

          ((> input-item 10.0)
           (print-message *message-window* (format nil "~%Helmsman Sulu- \"Our top speed is warp 10, Captain.\"~%")))

          ((< input-item 1.0)
           (print-message *message-window* (format nil "~%Helmsman Sulu- \"We can't go below warp 1, Captain.\"~%")))

          (t
           (cond
             ((or (<= input-item *warp-factor*)
                  (<= input-item 6.0))
              (print-message *message-window*
               (format nil "~%Helmsman Sulu- \"Warp factor ~A, Captain.\"~%" (truncate input-item))))

             ((< input-item 8.00)
              (print-message *message-window*
               (format nil "~%Engineer Scott- \"Aye, but our maximum safe speed is warp 6.\"~%")))

             ((= input-item 10.0)
              (print-message *message-window* (format nil "~%Engineer Scott- \"Aye, Captain, we'll try it.\"~%")))

             (t
              (print-message *message-window*
               (format nil
                       "~%Engineer Scott- \"Aye, Captain, but our engines may not take it.\"~%"))))
           (setf *warp-factor* input-item)))
        (huh))))

(defun wait () ; C: wait(void)
  "Wait on events."

  (let (original-time
        time-to-wait
        input-item)
    (setf *action-taken-p* nil)
    (unless (input-available-p)
      (print-prompt "How long? "))
    (setf input-item (scan-input))
    (unless (numberp input-item)
      (huh)
      (return-from wait nil))
    (when (<= input-item 0.0)
      (return-from wait nil))
    (setf original-time input-item)
    (setf time-to-wait input-item)
    (when (or (>= input-item *remaining-time*)
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
      (unless *restingp*
        (print-message *message-window* (format nil "~A stardates left.~%" (truncate *remaining-time*)))
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
      (decf time-to-wait temp)
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
  (if (damagedp +computer+)
      (print-message *message-window* (format nil "~%Computer damaged; cannot execute destruct sequence.~%"))
      (progn
        (print-message *message-window* (format nil "~%---WORKING---~%") :print-slowly t)
        (print-message *message-window* (format nil "SELF-DESTRUCT-SEQUENCE-ACTIVATED~%") :print-slowly t)
        (print-message *message-window* (format nil "   10~%") :print-slowly t)
        (print-message *message-window* (format nil "       9~%") :print-slowly t)
        (print-message *message-window* (format nil "          8~%") :print-slowly t)
        (print-message *message-window* (format nil "             7~%") :print-slowly t)
        (print-message *message-window* (format nil "                6~%") :print-slowly t)
        (print-message *message-window* (format nil "~%ENTER-CORRECT-PASSWORD-TO-CONTINUE-~%"))
        (print-message *message-window* (format nil "~%~%SELF-DESTRUCT-SEQUENCE-OTHERWISE-~%"))
        (print-message *message-window* (format nil "~%~%SELF-DESTRUCT-SEQUENCE-WILL-BE-ABORTED~%~%~%"))
	(print-prompt "")
        (clear-type-ahead-buffer)
        (let ((input-item (scan-input)))
          (when (numberp input-item)
            (setf input-item (write-to-string input-item)))
          (if (string= *self-destruct-password* input-item)
              (progn
                (print-message *message-window* (format nil "PASSWORD-ACCEPTED~%") :print-slowly t)
                (print-message *message-window* (format nil "                   5~%") :print-slowly t)
                (print-message *message-window* (format nil "                      4~%")) :print-slowly t
                (print-message *message-window* (format nil "                         3~%") :print-slowly t)
                (print-message *message-window* (format nil "                            2~%") :print-slowly t)
                (print-message *message-window* (format nil "                              1~%") :print-slowly t)
                (when (< (random 1.0) 0.15)
                  (print-message *message-window* (format nil "GOODBYE-CRUEL-WORLD~%") :print-slowly t))
                (kaboom))
              (progn
                (print-message *message-window* (format nil "PASSWORD-REJECTED;~%") :print-slowly t)
                (print-message *message-window* (format nil "CONTINUITY-EFFECTED~%") :print-slowly t)))))))

(defun calculate-eta () ; C: eta(void)
  "Use computer to get estimated time of arrival for a warp jump."

  (skip-line *message-window*)

  (when (damagedp +computer+)
    (print-message *message-window* (format nil "COMPUTER DAMAGED, TRAVEL CALCULATION NOT POSSIBLE.~%"))
    (return-from calculate-eta nil))

  (let (need-prompt wfl ttime twarp tpower trip-distance destination-quadrant input-item)
    (unless (two-input-items-available)
      (setf need-prompt t)
      (clear-type-ahead-buffer)
      (print-prompt "~%Destination quadrant or quadrant&sector? "))
    (multiple-value-bind (sx sy qx qy) (get-quadrant-and-sector)
      (if (and qx qy)
          (progn
            ;; The C source adds .5, why?
            (incf qx 0.5)
            (incf qy 0.5)
            ;; If not provided, calculate sector to be the nearest sector in the destination quadrant
            (if (and sx sy)
                (progn
                  (incf sx 0.5)
                  (incf sy 0.5))
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
            (setf destination-quadrant (make-quadrant-coordinate :x qx :y qy)))
          (return-from calculate-eta nil)))

    (when need-prompt
      (print-message *message-window* (format nil "Answer \"no\" if you don't know the value:~%")))
    (do ()
        ((or ttime twarp))
      (clear-type-ahead-buffer)
      (print-prompt "Time or arrival date? ")
      (setf input-item (scan-input))
      (when (numberp input-item)
        (setf ttime input-item)
        (when (> ttime *stardate*)
          (decf ttime *stardate*)) ; Actually a star date
        (setf twarp (/ (+ (floor (* (sqrt (/ (* 10.0 trip-distance) ttime)) 10.0)) 1.0) 10.0))
        (when (or (<= ttime 1e-10) ; TODO - name the constant
                  (> twarp 10))
          (print-message *message-window* (format nil "We'll never make it, sir.~%"))
          (clear-type-ahead-buffer)
          (return-from calculate-eta nil))
        (when (< twarp 1.0)
          (setf twarp 1.0)))
      (unless ttime
        (clear-type-ahead-buffer)
        (print-prompt "Warp factor? ")
        (setf input-item (scan-input))
        (when (numberp input-item)
          (setf wfl t)
          (setf twarp input-item)
          (when (or (< twarp 1.0) (> twarp 10.0))
            (huh)
            (return-from calculate-eta nil))))
      (unless (or ttime twarp)
        (print-message *message-window* (format nil "Captain, certainly you can give me one of these.~%"))))

    (skip-line *message-window*)

    (do ((try-another-warp-factor-p t))
        ((not try-another-warp-factor-p))
      (setf try-another-warp-factor-p nil)
      (clear-type-ahead-buffer)
      (setf ttime (/ (* 10.0 trip-distance) (expt twarp 2)))
      (setf tpower (* trip-distance (expt twarp 3) (if *shields-are-up-p* 2 1)))
      (if (>= tpower *ship-energy*)
          (progn
            (print-message *message-window* (format nil "Insufficient energy, sir."))
            (when (or (not *shields-are-up-p*)
                      (> tpower (* *ship-energy* 2.0)))
              (unless wfl
                (return-from calculate-eta nil))
              (print-prompt (format nil "~%New warp factor to try? "))
              (setf input-item (scan-input))
              (if (numberp input-item)
                  (if (and (>= twarp 1.0)
                           (<= twarp 10.0))
                      (progn
                        (setf wfl t)
                        (setf try-another-warp-factor-p t)
                        (setf twarp input-item))
                      (progn
                        (huh)
                        (return-from calculate-eta nil)))
                  (progn
                    (clear-type-ahead-buffer)
                    (return-from calculate-eta nil))))
            (unless try-another-warp-factor-p
              (print-message *message-window* (format nil "But if you lower your shields,~%"))
              (print-message *message-window* "remaining")
              (setf tpower (/ tpower 2))))
          (progn
            (print-message *message-window* "Remaining")))
      (unless try-another-warp-factor-p
        (print-message *message-window* (format nil " energy will be ~,2F.~%" (- *ship-energy* tpower)))
        (cond
          (wfl
           (print-message *message-window* (format nil "And we will arrive at stardate ~A.~%"
                                  (format-stardate (+ *stardate* ttime)))))

          ((= twarp 1.0)
           (print-message *message-window* (format nil "Any warp speed is adequate.~%")))

          (t
           (print-message *message-window* (format nil "Minimum warp needed is ~,2F,~%" twarp))
           (print-message *message-window* (format nil "and we will arrive at stardate ~A.~%"
                                  (format-stardate (+ *stardate* ttime))))))
        (when (< *remaining-time* ttime)
          (print-message *message-window* (format nil "Unfortunately, the Federation will be destroyed by then.~%")))
        (when (> twarp 6.0)
          (print-message *message-window* (format nil "You'll be taking risks at that speed, Captain.~%")))
        (when (or (and (eql *super-commander-attacking-base* 'attacking)
                       (coord-equal *super-commander-quadrant* destination-quadrant)
                       (< (find-event +super-commander-destroys-base+) (+ *stardate* ttime)))
                  (and (< (find-event +commander-destroys-base+) (+ *stardate* ttime))
                       (coord-equal *base-under-attack-quadrant* destination-quadrant)))
          (print-message *message-window* (format nil "The starbase there will be destroyed by then.~%")))
        (skip-line *message-window*)
        (print-prompt "New warp factor to try? ")
        (setf input-item (scan-input))
        (if (numberp input-item)
          (if (and (>= twarp 1.0)
                   (<= twarp 10.0))
              (progn
                (setf wfl t)
                (setf try-another-warp-factor-p t)
                (setf twarp input-item))
              (progn
                (huh)
                (return-from calculate-eta nil)))
          (progn
            (clear-type-ahead-buffer)
            (return-from calculate-eta nil)))))))

(defun chart () ; C: chart(void), and TODO - bring in the contents of makechart()
  "Display the start chart."

  (if (eql *starchart-window* *message-window*)
      (skip-line *starchart-window*)
      (clear-window *starchart-window*))

  (print-out *starchart-window* (format nil "       STAR CHART FOR THE KNOWN GALAXY~%"))
  (print-out *starchart-window* (format nil "      1    2    3    4    5    6    7    8~%"))
  (do ((x 0 (1+ x)))
      ((>= x +galaxy-size+))
    (print-out *starchart-window* (format nil "~A |" (1+ x)))
    (do ((y 0 (1+ y)))
        ((>= y +galaxy-size+))
      (if (and (= x (coordinate-x *ship-quadrant*))
               (= y (coordinate-y *ship-quadrant*)))
          (print-out *starchart-window* "<")
          (print-out *starchart-window* " "))
      (cond
        ((quadrant-supernovap (aref *galaxy* x y))
         (print-out *starchart-window* "***"))

        ((and (not (quadrant-chartedp (aref *galaxy* x y)))
              (> (quadrant-starbases (aref *galaxy* x y)) 0))
         (print-out *starchart-window* ".1."))

        ((quadrant-chartedp (aref *galaxy* x y))
         (print-out *starchart-window* (format nil "~3D" (+ (* (starchart-page-klingons (aref *starchart* x y)) 100)
                                         (* (starchart-page-starbases (aref *starchart* x y)) 10)
                                         (starchart-page-stars (aref *starchart* x y))))))

        (t
         (print-out *starchart-window* "...")))
      (if (and (= x (coordinate-x *ship-quadrant*))
               (= y (coordinate-y *ship-quadrant*)))
          (print-out *starchart-window* ">")
          (print-out *starchart-window* " ")))
    (print-out *starchart-window* "|")
    (when (< x +galaxy-size+)
      (skip-line *starchart-window*))))

;; TODO - make this a device status report. Show repair time of 0 when a device is not damaged.
;;        Could use green/yellow/red highlighting for a fancy effect.
;;        The goal is to have a continuously updated console showing device status.
(defun damage-report () ; C: damagereport(void)
  "List damaged devices and repair times for each."

  (if (eql *damage-report-window* *message-window*)
      (skip-line *damage-report-window*)
      (clear-window *damage-report-window*))

  (do ((i 0 (1+ i))
       (header-printed-p nil))
      ((>= i +number-of-devices+)
       (unless header-printed-p
         (print-out *damage-report-window* (format nil "All devices functional.~%"))))
    (when (damagedp i)
      (unless header-printed-p
        (print-out *damage-report-window* (format nil "~12@A~24@A~%" "DEVICE" "-REPAIR TIMES-"))
        (print-out *damage-report-window* (format nil "~21A~8@A~8@A~%" " " "IN FLIGHT" "DOCKED"))
        (setf header-printed-p t))
      (print-out *damage-report-window* (format nil "  ~17A~9,2F~9,2F~%"
                         (aref *devices* i)
                         (+ (aref *device-damage* i) 0.05)
                         (+ (* (aref *device-damage* i) +docked-repair-factor+) 0.005))))))

(defun report ()
  "Report on general game status."

  (print-message *message-window* (format nil "~%You are playing a ~A ~A game.~%"
                         *game-length*
                         (rest (assoc *skill-level* *skill-level-labels*))))
  (when *tournament-number*
    (print-message *message-window* (format nil "This is tournament game ~A.~%" *tournament-number*)))
  (print-message *message-window* (format nil "Your secret password is \"~A\"~%" *self-destruct-password*))
  (print-out *message-window*(format nil "~A of ~A Klingons have been killed"
                     (- (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)
                        (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*))
                     (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
  (cond
    ((> (- *initial-commanders* (length *commander-quadrants*)) 0)
     (print-message *message-window* (format nil ", including ~A Commander~:P.~%"
                            (- *initial-commanders* (length *commander-quadrants*)))))

    ((> (+ (- *initial-klingons* *remaining-klingons*)
           (- *initial-super-commanders* *remaining-super-commanders*))
        0)
     (print-message *message-window* (format nil ", but no Commanders.~%")))

    (t
     (print-message *message-window* (format nil ".~%"))))
  (when (> *skill-level* +fair+)
    (print-message *message-window* (format nil "The Super Commander has ~Abeen destroyed.~%"
                           (if (> *remaining-super-commanders* 0) "not " ""))))
  (if (/= *initial-bases* (length *base-quadrants*))
      (progn
        (print-out *message-window* "There ")
        (if (= (- *initial-bases* (length *base-quadrants*)) 1)
            (print-out *message-window* "has been 1 base")
            (print-out *message-window* (format nil "have been ~A bases" (- *initial-bases* (length *base-quadrants*)))))
        (print-message *message-window* (format nil " destroyed, ~A remaining.~%" (length *base-quadrants*))))
      (print-message *message-window* (format nil "There are ~A bases.~%" *initial-bases*)))
  ;; Don't report this if not seen and either the radio is damaged or not at base!
  (when (and (subspace-radio-available-p)
             *base-attack-report-seen-p*)
    (attack-report))
  (when (> *casualties* 0)
    (print-message *message-window* (format nil "~A casualt~A suffered so far.~%" *casualties* (if (= *casualties* 1) "y" "ies"))))
  (when (/= *brig-capacity* *brig-free*)
    (print-message *message-window* (format nil "~D Klingon~:P in brig.~%"
                           (- *brig-capacity* *brig-free*))))
  (when (> *captured-klingons* 0)
    (print-message *message-window* (format nil "~D captured Klingon~:P turned in to Star Fleet.~%"
                           *captured-klingons*)))
  (when (> *calls-for-help* 0)
    (print-message *message-window* (format nil "There ~A ~A call~:P for help.~%"
                           (if (= *calls-for-help* 1) "was" "were")
                           *calls-for-help*)))
  (when (string= *ship* +enterprise+)
    (print-message *message-window* (format nil "You have ~A deep space probe~:P.~%"
                           (if (> *probes-available* 0) *probes-available* "no"))))
  (when (and (subspace-radio-available-p)
             (is-scheduled-p +move-deep-space-probe+))
    (if (probe-is-armed-p *probe*)
        (print-out *message-window* "An armed deep space probe is in ")
        (print-out *message-window* "A deep space probe is in "))
    (print-message *message-window*
                   (format nil "~A.~%" (format-quadrant-coordinates
                                        (make-quadrant-coordinate :x (galaxy-sector-to-quadrant (probe-x *probe*))
                                                                  :y (galaxy-sector-to-quadrant (probe-y *probe*)))))))
  (when *dilithium-crystals-on-board-p*
    (if (<= *crystal-work-probability* 0.05)
        (print-message *message-window* (format nil "Dilithium crystals aboard ship... not yet used.~%"))
        ;; Calculate number of times crystals have been used, and display it.
        (do ((i 0 (1+ i))
             (ai 0.05 (* ai 2.0)))
            ((< *crystal-work-probability* ai)
             (print-message *message-window* (format nil "Dilithium crystals have been used ~A time~:P.~%" i)))))))

(defun request () ; C: request()
  "Request a single item of status information."

  (let (req-item)
    (unless (input-available-p)
      (print-prompt "Information desired? "))
    (setf req-item (match-token (scan-input) (list "date" "condition" "position" "lsupport"
                                                   "warpfactor" "energy" "torpedoes" "shields"
                                                   "klingons" "time")))
    (unless req-item
      (setf req-item "?")) ; blank input still results in a help message
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
       (print-message *message-window* (format nil "~%UNRECOGNIZED REQUEST. Valid requests are:~%"))
       (print-message *message-window* (format nil "  date, condition, position, lsupport, warpfactor,~%"))
       (print-message *message-window* (format nil "  energy, torpedoes, shields, klingons, time.~%"))
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
        ix iy sector-contents input-item)
    (skip-line *message-window*)
    (unless (input-available-p)
      (print-prompt "Direction? "))
    (setf input-item (scan-input))
    (unless (numberp input-item)
      (huh)
      (return-from visual-scan nil))
    (when (or (< input-item 0.0)
              (> input-item 360.0))
      (return-from visual-scan nil))
    (setf delta-index (floor (/ (+ input-item 22) 45)))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant* ix iy)))
    (print-out *message-window* (format nil "~A,~A ~A " (+ ix 1) (+ iy 1) sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant* ix iy)))
    (print-out *message-window* (format nil "~A " sector-contents))
    (setf delta-index (+ delta-index 1))
    (setf ix (+ (coordinate-x *ship-sector*) (aref visual-delta delta-index 0)))
    (setf iy (+ (coordinate-y *ship-sector*) (aref visual-delta delta-index 1)))
    (if (or (< ix 0) (>= ix +quadrant-size+) (< iy 0) (>= iy +quadrant-size+))
        (setf sector-contents "?")
        (setf sector-contents (aref *quadrant* ix iy)))
    (print-out *message-window* (format nil "~A ~A,~A~%" sector-contents (+ ix 1) (+ iy 1)))
    (setf *time-taken-by-current-operation* 0.5)
    (setf *action-taken-p* t)))

(defun initialize ()
  "One-time start up actions. Initialize I/O, curses, data values, etc."

  (setf *random-state* (make-random-state t)) ; Seed the random number generator
  (initialize-windows))

(defun clean-up ()
  "Carry out any needed housekeeping before exiting the program."

  (clean-up-windows))

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

  (skip-line *message-window*)
  (cond
    ((damagedp +shuttle+)
     ;; TODO - The -1.0 value is a bizarre way of saying the current ship is the Faerie Queene
     (cond
       ((= (aref *device-damage* +shuttle+) -1.0)
        (if (and *in-orbit-p*
                 (shuttle-landed-p *ship-quadrant*))
            (print-message *message-window* (format nil "Ye Faerie Queene has no shuttle craft bay to dock it at.~%"))
            (print-message *message-window* (format nil "Ye Faerie Queene had no shuttle craft.~%"))))

       ((> (aref *device-damage* +shuttle+) 0)
        (print-message *message-window* (format nil "The Galileo is damaged.~%")))

       (t ; C: game.damage[DSHUTTL] < 0, or (< (aref *device-damage* +shuttle+) 0)
        (print-message *message-window* (format nil "Shuttle craft is now serving Big Macs.~%")))))

    ((not *in-orbit-p*)
     (print-message *message-window* (format nil "~A not in standard orbit.~%" (format-ship-name))))

    ((and (not (shuttle-landed-p *ship-quadrant*))
          (not (eql *shuttle-craft-location* 'on-ship)))
     (print-message *message-window* (format nil "Shuttle craft not currently available.~%")))

    ((and (shuttle-landed-p *ship-quadrant*)
          (not *landedp*))
     (print-message *message-window* (format nil "You will have to beam down to retrieve the shuttle craft.~%")))

    ((or *shields-are-up-p*
         *dockedp*)
     (print-message *message-window* (format nil "Shuttle craft cannot pass through shields.~%")))

    ((not (planet-knownp (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
     (print-message *message-window* (format nil "Spock-  \"Captain, we have no information on this planet~%"))
     (print-message *message-window* (format nil "  and Starfleet Regulations clearly state that in this situation~%"))
     (print-message *message-window* (format nil "  you may not fly down.\"~%")))

    (t
     (setf *time-taken-by-current-operation* (* (expt 3.0 -5) *height-of-orbit*))
     (when (>= *time-taken-by-current-operation* (* 0.8 *remaining-time*))
       (print-message *message-window* (format nil "First Officer Spock-  \"Captain, I compute that such~%"))
       (print-message *message-window*
        (format nil "  a maneuver would require approximately ~,2F% of our remaining time.~%"
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
               (if (damagedp +transporter+)
                   (print-message *message-window* "Rescue party")
                   (progn
                     (print-prompt "Spock-  \"Would you rather use the transporter?\" ")
                     (when (get-y-or-n-p)
                       (beam)
                       (return-from shuttle nil))
                     (print-message *message-window* "Shuttle crew")))
               (print-message *message-window* (format nil " boards Galileo and swoops toward planet surface.~%"))
               (setf *shuttle-craft-location* 'off-ship)
               (setf *shuttle-craft-quadrant* *ship-quadrant*)
               (skip-line *message-window*)
               (when (consume-time)
                 (return-from shuttle nil))
               (print-message *message-window* (format nil "Trip complete.~%")))
             (progn
               ;; Ready to go back to ship
               (print-message *message-window* (format nil "You and your mining party board the~%"))
               (print-message *message-window* (format nil "shuttle craft for the trip back to the Enterprise.~%"))
               (print-message *message-window* (format nil "~%The short hop begins . . .~%"))
               (skip-line *message-window* 2)
               (let ((pl (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
                 (setf (planet-knownp pl) t)
                 (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) pl))
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
               (print-message *message-window* (format nil "Trip complete.~%"))))
         (progn
           ;; Kirk on ship
           ;; and so is Galileo
           (print-message *message-window* (format nil "Mining party assembles in the hangar deck,~%"))
           (print-message *message-window* (format nil "ready to board the shuttle craft \"Galileo\".~%"))
           (print-message *message-window* (format nil "~%The hangar doors open; the trip begins.~%"))
           (setf *in-shuttle-craft-p* t)
           (setf *shuttle-craft-location* 'off-ship)
           (setf *shuttle-craft-quadrant* *ship-quadrant*)
           (when (consume-time)
             (return-from shuttle nil))
           (setf *in-shuttle-craft-p* nil)
           (setf *landedp* t)
           (print-message *message-window* (format nil "~%Trip complete.~%")))))))

(defun beam () ; C: beam(void)
  "Use the transporter."

  (let ((energy-needed (+ (* 50 *skill-level*) (/ *height-of-orbit* 100.0))))
    (skip-line *message-window*)
    (when (damagedp +transporter+)
      (print-message *message-window* (format nil "Transporter damaged.~%"))
      ;; The shuttle is an option if it is not damaged and on the planet or on the ship
      (when (and (not (damagedp +shuttle+))
                 (or (shuttle-landed-p *ship-quadrant*)
                     (eql *shuttle-craft-location* 'on-ship)))
        (skip-line *message-window*)
        (print-prompt "Spock-  \"May I suggest the shuttle craft, Sir?\" ")
        (when (get-y-or-n-p)
          (shuttle)))
      (return-from beam nil))
    (unless *in-orbit-p*
      (print-message *message-window* (format nil "~A not in standard orbit.~%" (format-ship-name)))
      (return-from beam nil))
    (when *shields-are-up-p*
      (print-message *message-window* (format nil "Impossible to transport through shields.~%"))
      (return-from beam nil))
    (unless (planet-knownp (rest (assoc *ship-quadrant* *planets* :test #'coord-equal)))
      (print-message *message-window*
                     (format nil "Spock-  \"Captain, we have no information on this planet~%"))
      (print-message *message-window*
                     (format nil "  and Starfleet Regulations clearly state that in this situation~%"))
      (print-message *message-window* (format nil "  you may not go down.\"~%"))
      (return-from beam nil))
    (when (and (not *landedp*)
               (eql (planet-crystals (rest (assoc *ship-quadrant* *planets* :test #'coord-equal)))
                    'absent))
      (print-message *message-window* (format nil "Spock-  \"Captain, I fail to see the logic in~%"))
      (print-message *message-window*
                     (format nil "  exploring a planet with no dilithium crystals.~%"))
      (print-prompt "  Are you sure this is wise?\" ")
      (unless (get-y-or-n-p)
        (return-from beam nil)))
    (when (> energy-needed *ship-energy*)
      (print-message *message-window* (format nil "Engineering to bridge--~%"))
      (print-message *message-window*
                     (format nil "  Captain, we don't have enough energy for transportation.~%"))
      (return-from beam nil))
    (when (and (not *landedp*)
               (> (* 2 energy-needed) *ship-energy*))
      (print-message *message-window* (format nil "Engineering to bridge--~%"))
      (print-message *message-window*
                     (format nil "  Captain, we have enough energy only to transport you down to~%"))
      (print-message *message-window*
                     (format nil "  the planet, but there wouldn't be any energy for the trip back.~%"))
      (when (shuttle-landed-p *ship-quadrant*)
        (print-message *message-window*
                       (format nil "  Although the Galileo shuttle craft may still be on the surface.~%")))
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
            (print-message *message-window* (format nil
                                   "Your crew hides the Galileo to prevent capture by aliens.~%")))
          (print-message *message-window* (format nil "Landing party assembled, ready to beam up.~%"))
          (print-message *message-window* (format nil "~%Kirk whips out communicator...~%"))
          (print-message *message-window* (format nil "BEEP  BEEP  BEEP~%") :print-slowly t)
          (print-message *message-window* (format nil
                                 "~%~%\"Kirk to enterprise-  Lock on coordinates...energize.\"~%")))
        ;; Going to planet
        (progn
          (print-message *message-window* (format nil "Scotty-  \"Transporter room ready, Sir.\"~%"))
          (print-message *message-window*
           (format nil "~%Kirk and landing party prepare to beam down to planet surface.~%"))
          (print-message *message-window* (format nil "~%Kirk-  \"Energize.\"~%"))))
    (setf *action-taken-p* t)
    (print-message *message-window*
                   (format nil "~%WWHOOOIIIIIRRRRREEEE.E.E.  .  .  .  .   .    .~%~%")
                   :print-slowly t)
    (when (> (random 1.0) 0.98)
      (print-message *message-window* (format nil "BOOOIIIOOOIIOOOOIIIOIING . . .~%") :print-slowly t)
      (print-message *message-window* (format nil "~%Scotty-  \"Oh my God!  I've lost them.\"~%"))
      (finish 'transporter-failure)
      (return-from beam nil))
    (print-message *message-window* (format nil ".    .   .  .  .  .  .E.E.EEEERRRRRIIIIIOOOHWW~%") :print-slowly t)
    (setf *landedp* (not *landedp*))
    (decf *ship-energy* energy-needed)
    (print-message *message-window* (format nil "~%~%Transport complete.~%"))
    (when (and *landedp*
               (shuttle-landed-p *ship-quadrant*))
      (print-message *message-window* (format nil "The shuttle craft Galileo is here!~%")))
    (when (and (not *landedp*)
               *miningp*)
      (setf *dilithium-crystals-on-board-p* t)
      (setf *crystal-work-probability* 0.05)) ; TODO - 0.05 is also a flag value indicating mining has been completed
    (setf *miningp* nil)))

(defun deathray () ; C: deathray(void)
  "Use the big zapper."

  (skip-line *message-window*)
  (cond
    ((string= *ship* +faerie-queene+)
     (print-message *message-window* (format nil "Ye Faerie Queene has no death ray.~%")))

    ((= *enemies-here* 0)
     (print-message *message-window*
                    (format nil "Sulu-  \"But Sir, there are no enemies in this quadrant.\"~%")))

    ((damagedp +death-ray+)
     (print-message *message-window* (format nil "Death Ray is damaged.~%")))

    (t
     (print-message *message-window* (format nil "Spock-  \"Captain, the 'Experimental Death Ray'~%"))
     (print-message *message-window*
                    (format nil "  is highly unpredictible.  Considering the alternatives,~%"))
     (print-prompt "  are you sure this is wise?\" ")
     (unless (get-y-or-n-p)
       (return-from deathray nil))
     (print-message *message-window* (format nil "Spock-  \"Acknowledged.\"~%"))
     (setf *action-taken-p* t)
     (print-message *message-window*
                    (format nil "~%WHOOEE ... WHOOEE ... WHOOEE ... WHOOEE~%") :print-slowly t)
     (print-message *message-window* (format nil "Crew scrambles in emergency preparation.~%"))
     (print-message *message-window* (format nil "Spock and Scotty ready the death ray and~%"))
     (print-message *message-window* (format nil "prepare to channel all ship's power to the device.~%"))
     (print-message *message-window* (format nil "~%Spock-  \"Preparations complete, sir.\"~%"))
     (print-message *message-window* (format nil "Kirk-  \"Engage!\"~%"))
     (print-message *message-window*
                    (format nil "~%WHIRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR~%") :print-slowly t)
     (if (> (random 1.0) +deathray-failure-chance+)
         (progn
           (print-message *message-window*
                          (format nil "Sulu- \"Captain!  It's working!\"~%~%") :print-slowly t)
           ;; This loop works because dead-enemy always compacts the array of enemies so that
           ;; current enemies are at the lowest indexes.
           (do (enemy-sector)
               ((<= *enemies-here* 0))
             (setf enemy-sector (enemy-sector-coordinates (aref *quadrant-enemies* 0)))
             (dead-enemy enemy-sector (coord-ref *quadrant* enemy-sector) enemy-sector))
           (print-message *message-window*
                          (format nil "Ensign Chekov-  \"Congratulations, Captain!\"~%"))
           (print-message *message-window*
            (format nil "~%Spock-  \"Captain, I believe the `Experimental Death Ray'~%"))
           (if (<= (random 1.0) 0.05)
               (print-message *message-window* (format nil "   is still operational.\"~%"))
               (progn
                 (print-message *message-window*
                                (format nil "   has been rendered nonfunctional.\"~%"))
                 (setf (aref *device-damage* +death-ray+) 39.95)))
           ;; In the C source the victory check was done before reporting on the status of the
           ;; deathray. That seems backwards - the deathray status report is irrelevant and out
           ;; of place after the text explaining that the game has been won.
           (when (= (+ *remaining-klingons*
                       (length *commander-quadrants*) *remaining-super-commanders*)
                    0)
             (finish 'won)))
         (let ((r (random 1.0)))
           ;; Pick failure method
           (cond
             ((<= r 0.30)
              (print-message *message-window*
                             (format nil "Sulu- \"Captain!  It's working!\"~%") :print-slowly t)
              (print-message *message-window*
                             (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
              (print-message *message-window*
                             (format nil "***MATTER-ANTIMATTER IMPLOSION IMMINENT!~%"))
              (print-message *message-window*
               (format nil "~%***RED ALERT!  RED A*L********************************~%")
               :print-slowly t)
              (print-stars)
              (print-message *message-window*
               (format nil "******************   KA-BOOM!!!!   *******************~%")
               :print-slowly t)
              (kaboom))

             ((<= r 0.55)
              (print-message *message-window*
               (format nil "Sulu- \"Captain!  Yagabandaghangrapl, brachriigringlanbla!\"~%")
               :print-slowly t)
              (print-message *message-window* (format nil "Lt. Uhura-  \"Graaeek!  Graaeek!\"~%"))
              (print-message *message-window* (format nil "Spock-  \"Fascinating!  . . . All humans aboard~%"))
              (print-message *message-window* (format nil "  have apparently been transformed into strange mutations.~%"))
              (print-message *message-window* (format nil"  Vulcans do not seem to be affected.~%"))
              (print-message *message-window* (format nil "~%Kirk-  \"Raauch!  Raauch!\"~%"))
              (finish 'death-ray-malfunction))

             ((<= r 0.75)
              (print-message *message-window*
                             (format nil "Sulu- \"Captain!  It's   --WHAT?!?!\"~%") :print-slowly t)
              (print-message *message-window* "~%Spock-  \"I believe the word is")
              (print-message *message-window* " *ASTONISHING*"  :print-slowly t)
              (print-message *message-window* (format nil " Mr. Sulu.~%"))
              (do ((i 0 (1+ i)))
                  ((>= i +quadrant-size+))
                (do ((j 0 (1+ j)))
                    ((>= j +quadrant-size+))
                  (when (string= (aref *quadrant* i j) +empty-sector+)
                    (setf (aref *quadrant* i j) +thing+))))
              (print-message *message-window*
                             (format nil "  Captain, our quadrant is now infested with~%"))
              (print-message *message-window*
                             (format nil " - - - - - -  *THINGS*.~%") :print-slowly t)
              (print-message *message-window* (format nil "  I have no logical explanation.\"~%")))

             (t
              (print-message *message-window*
               (format nil "Sulu- \"Captain!  The Death Ray is creating tribbles!\"~%")
               :print-slowly t)
              (print-message *message-window*
                             (format nil "Scotty-  \"There are so many tribbles down here~%"))
              (print-message *message-window*
                             (format nil "  in Engineering, we can't move for 'em, Captain.\"~%"))
              (finish 'death-ray-creates-tribbles))))))))

(defun mine () ; C: mine(void)
  "Mine dilithium from a planet."

  (skip-line *message-window*)
  (cond
    ((not *landedp*)
     (print-message *message-window* (format nil "Mining party not on planet.~%")))

    ((eql (planet-crystals (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))) 'mined)
     (print-message *message-window* (format nil "This planet has already been mined for dilithium.~%")))

    ((eql (planet-crystals (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))) 'absent)
     (print-message *message-window* (format nil "No dilithium crystals on this planet.~%")))

    (*miningp*
     (print-message *message-window* (format nil "You've already mined enough crystals for this trip.~%")))

    ((and *dilithium-crystals-on-board-p*
          (= *crystal-work-probability* 0.05))
     (print-message *message-window* (format nil "With all those fresh crystals aboard the ~A~%" (format-ship-name)))
     (print-message *message-window* (format nil "there's no reason to mine more at this time.~%")))

    (t
     (setf *time-taken-by-current-operation*
           (* (+ 0.1 (* 0.2 (random 1.0)))
              (planet-class (rest (assoc *ship-quadrant* *planets* :test #'coord-equal)))))
     (unless (consume-time)
       (print-message *message-window* (format nil "~%Mining operation complete.~%"))
       (let ((pl (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
         (setf (planet-crystals pl) 'mined)
         (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) pl))
       (setf *miningp* t)
       (setf *action-taken-p* t)))))

(defun quadrant-exit-while-on-planet (finish-reason) ; C: atover()
  "Handle the case of the captain being on a planet when the ship suddenly exits the quadrant.
These exits occur when there is a supernova in the quadrant or a tractor beam grabs the ship.
The finish-reason determines the ending text if the captain is not successfully retrieved from
the planet."

  (clear-type-ahead-buffer)
  (when *landedp*
    (when (damagedp +transporter+)
      (finish finish-reason)
      (return-from quadrant-exit-while-on-planet nil))
    (print-message *message-window* (format nil "Scotty rushes to the transporter controls.~%"))
    (when *shields-are-up-p*
      (print-message *message-window* (format nil "But with the shields up it's hopeless.~%"))
      (finish finish-reason)
      (return-from quadrant-exit-while-on-planet nil))
    (print-message *message-window* (format nil "His desperate attempt to rescue you . . .") :print-slowly t)
    (if (<= (random 1.0) 0.05)
        (progn
          (print-message *message-window* (format nil "fails.~%"))
          (finish finish-reason))
        (progn
          (print-message *message-window* (format nil "SUCCEEDS!~%"))
          (when *miningp*
            (setf *miningp* nil)
            (print-message *message-window* "The crystals mined were ")
            (if (<= (random 1.0) 0.25)
                (print-message *message-window* (format nil "lost.~%"))
                (progn
                  (print-message *message-window* (format nil "saved.~%"))
                  (setf *dilithium-crystals-on-board-p* t))))))))

(defun emergency-supernova-exit () ; C: atover()
  "Handle emergency exit from the quadrant due to a supernova."

  (quadrant-exit-while-on-planet 'nova-destroys-planet-while-landed)
  ;; TODO - is this the correct place for the shuttle craft check?
  ;; Check to see if captain in shuttle craft
  (when *in-shuttle-craft-p*
    (finish 'super-nova-destroys-shuttle))
  (unless *all-done-p*
    ;; Inform captain of attempt to reach safety
    (skip-line *message-window*)
    (do ((power (* 0.75 *ship-energy*))
         (required-distance (+ 1.4142 (random 1.0))))
        ;; Repeat if another supernova
        ((not (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))))
      (when *just-in-p*
        (print-message *message-window* (format nil "***RED ALERT!  RED ALERT!~%") :print-slowly t)
        (print-message *message-window*
         (format nil "The ~A has stopped in a quadrant containing~%" (format-ship-name)))
        (print-message *message-window* (format nil "   a supernova.~%~%") :print-slowly t))
      (print-message *message-window*
       (format nil "***Emergency automatic override attempts to hurl ~A~%" (format-ship-name)))
      (print-message *message-window* (format nil "safely out of quadrant.~%"))
      (setf (quadrant-chartedp (coord-ref *galaxy* *ship-quadrant*)) t)
      ;; Try to use warp engines
      (when (damagedp +warp-engines+)
        (print-message *message-window* (format nil "~%Warp engines damaged.~%"))
        (finish 'destroyed-by-supernova)
        (return-from emergency-supernova-exit nil))
      (setf *warp-factor* (+ 6.0 (* 2.0 (random 1.0))))
      (print-message *message-window* (format nil "Warp factor set to ~,1F~%" *warp-factor*))
      (let ((course (* 12.0 (random 1.0))) ; How dumb! (really?)
            (distance (/ power (* (expt *warp-factor* 3) (if *shields-are-up-p* 2 1)))))
        (when (< required-distance distance)
          (setf distance required-distance))
        (setf *time-taken-by-current-operation*
              (calculate-warp-movement-time :distance distance :warp-factor *warp-factor*))
        (setf *just-in-p* nil)
        (setf *in-orbit-p* nil)
        (execute-warp-move course distance))
      (unless *just-in-p*
        ;; This is bad news, we didn't leave the quadrant.
        (when *all-done-p*
          (return-from emergency-supernova-exit nil))
        ;; TODO - Not true in the case where the ship was blocked on the way out of the quadrant.
        (print-message *message-window* (format nil "~%Insufficient energy to leave quadrant.~%"))
        (finish 'destroyed-by-supernova)
        (return-from emergency-supernova-exit nil)))
    (when (= (+ *remaining-klingons* (length *commander-quadrants*) *remaining-super-commanders*) 0)
      (finish 'won)))) ; Supernova killed remaining enemies.

(defun orbit () ; C: orbit(void)
  "Enter standard orbit."

  (skip-line *message-window*)
  (cond
    (*in-orbit-p*
     (print-message *message-window* (format nil "Already in standard orbit.~%")))

    ((and (damagedp +warp-engines+)
          (damagedp +impluse-engines+))
     (print-message *message-window* (format nil "Both warp and impulse engines damaged.~%")))

    ((or (not *current-planet*)
         (> (abs (- (coordinate-x *ship-sector*) (coordinate-x *current-planet*))) 1)
         (> (abs (- (coordinate-y *ship-sector*) (coordinate-y *current-planet*))) 1))
     (print-message *message-window* (format nil "~A not adjacent to planet.~%~%" (format-ship-name))))

    (t
     (setf *time-taken-by-current-operation* (+ 0.02 (* 0.03 (random 1.0))))
     (print-message *message-window* (format nil "Helmsman Sulu-  \"Entering standard orbit, Sir.\"~%"))
     (setf *dockedp* nil)
     (unless (consume-time)
       (setf *height-of-orbit* (+ 1400.0 (* 7200.0 (random 1.0))))
       (print-message *message-window*
                      (format nil "Sulu-  \"Entered orbit at altitude ~,2F kilometers.\"~%"
                              *height-of-orbit*))
       (setf *in-orbit-p* t)
       (setf *action-taken-p* t)))))

(defun sensor () ; C: sensor(void)
  "Examine planets in this quadrant. Inhabited planets have a name but no dilithium crystals."

  (skip-line *message-window*)
  (cond
    ((and (damagedp +short-range-sensors+)
          (not *dockedp*))
     (print-message *message-window* (format nil "Short range sensors damaged.~%")))

    ((not *current-planet*) ; sector coordinates or nil
     (print-message *message-window* (format nil "Spock- \"No planet in this quadrant, Captain.\"~%")))

    (t ; Go ahead and scan even if the planet has been scanned before
     (let ((pl (rest (assoc *ship-quadrant* *planets* :test #'coord-equal))))
       (print-message *message-window* (format nil "Spock-  \"Sensor scan for ~A -~%"
                              (format-quadrant-coordinates *ship-quadrant*)))
       (print-message *message-window* (format nil "~%         Planet "))
       (when (planet-inhabitedp pl)
         (print-message *message-window* (format nil "'~A' " (planet-name pl))))
       (print-message *message-window* (format nil "at ~A is of class ~A.~%"
                              (format-sector-coordinates *current-planet*)
                              (format-planet-class (planet-class pl))))
       (when (shuttle-landed-p *ship-quadrant*)
         (print-message *message-window* (format nil "         Sensors show Galileo still on surface.~%")))
       (print-message *message-window* "         Readings indicate")
       (unless (eql (planet-crystals pl) 'present)
         (print-message *message-window* " no"))
       (print-message *message-window* (format nil " dilithium crystals present.\"~%"))
       (setf (planet-knownp pl) t)
       (rplacd (assoc *ship-quadrant* *planets* :test #'coord-equal) pl)))))

;; TODO - a mayday failed but 2 (!!) sensor scans were displayed for the destination quadrant
;;        before the end of game messages were displayed
;; TODO - a mayday failed but the location of the ship in the starchart was shown as the
;;        destination quadrant that would have been the destination if the mayday had succeeded
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
     (print-message *message-window* (format nil "Lt. Uhura-  \"But Captain, we're already docked.\"~%")))

    ((damagedp +subspace-radio+)
     (print-message *message-window* (format nil "Subspace radio damaged.~%")))

    ((= (length *base-quadrants*) 0)
     (print-message *message-window*
      (format nil "Lt. Uhura-  \"Captain, I'm not getting any response from Starbase.\"~%")))

    (*cloakedp*
     (print-message *message-window*
      (format nil "Lt. Uhura-  \"Captain, we can't use the subspace radio while cloaked.\"")))

    (*landedp*
     (print-message *message-window* (format nil "You must be aboard the ~A.~%" (format-ship-name))))

    (t ; OK -- call for help from nearest starbase
     (setf *calls-for-help* (1+ *calls-for-help*))
     (let (dest-dist probf)
       (if (sector-coordinate-p *base-sector*)
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
             (setf (coordinate-x *ship-quadrant*) (coordinate-x dest-quadrant))
             (setf (coordinate-y *ship-quadrant*) (coordinate-y dest-quadrant))
             (new-quadrant)))
       ;; dematerialize starship
       (setf (coord-ref *quadrant* *ship-sector*) +empty-sector+)
       (print-message *message-window* (format nil "~%Starbase in ~A responds-- ~A dematerializes.~%"
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
                    (string= (aref *quadrant* x y) +empty-sector+))
           ;; found one -- finish up
           (setf *ship-sector* (make-sector-coordinate :x x :y y))
           (setf found-empty-sector-p t)))
       (unless (sector-coordinate-p *ship-sector*)
         (print-message *message-window* (format nil "~%You have been lost in space...~%"))
         (finish 'failed-to-rematerialize)
         (return-from mayday nil))
       ;; Give starbase three chances to rematerialize starship
       (setf probf (expt (- 1.0 (expt 0.98 dest-dist)) 0.33333333))
       (do ((three-tries 0 (1+ three-tries))
            (succeeds-p nil))
           ((or succeeds-p
                (>= three-tries 3 ))
            (unless succeeds-p
              (setf (coord-ref *quadrant* *ship-sector*) +thing+) ; question mark
              (setf *alivep* nil)
              ;; Don't call the sensor scan even if in curses mode. The ship has been
              ;; lost/destroyed and Spock isn't doing anything now. Fill the windows so the player
              ;; isn't left with a blank screen.
              (update-windows)
              (finish 'failed-to-rematerialize)
              (return-from mayday nil)))
         (print-message *message-window* (format nil "~A attempt to re-materialize ~A "
                                (cond
                                  ((= three-tries 0) "1st")
                                  ((= three-tries 1) "2nd")
                                  ((= three-tries 2) "3rd"))
                                (format-ship-name)))
         (warble)
         (if (> (random 1.0) probf)
             (setf succeeds-p t)
             (progn
               (set-text-color *message-window* +red+)
               (print-message *message-window* (format nil "fails.~%"))
               (set-text-color *message-window* +default-color+)))
         (sleep 0.500)) ; half of a second?
       (setf (coord-ref *quadrant* *ship-sector*) *ship*)
       (set-text-color *message-window* +green+)
       (print-message *message-window* (format nil "succeeds.~%"))
       (set-text-color *message-window* +default-color+)
       (dock))
     (print-message *message-window* (format nil "~%Lt. Uhura-  \"Captain, we made it!\"~%")))))

(defun survey () ; C: survey(void)
  "Report on known planets in the galaxy."

  (if (eql *planet-report-window* *message-window*)
      (skip-line *planet-report-window*)
      (clear-window *planet-report-window*))

  (if *planet-report-window*
      (print-out *planet-report-window* (format nil "~36@A~%" "KNOWN PLANETS"))
      (print-out *planet-report-window*
                 (format nil "Spock-  \"Planet report follows, Captain.\"~%~%")))
  (let ((one-planet-knownp-p nil)
        pl)
    (dolist (p-cons *planets*)
      (setf pl (rest p-cons))
      (unless (planet-destroyedp pl)
        (when (planet-knownp pl)
          (setf one-planet-knownp-p t)
          (print-out *planet-report-window* (format nil "~A  class ~A  "
                             (format-quadrant-coordinates (first p-cons))
                             (format-planet-class (planet-class pl))))
          (if (planet-inhabitedp pl)
              (print-out *planet-report-window* (format nil "~A~%" (planet-name pl)))
              (progn
                (unless (eql (planet-crystals pl) 'present)
                  (print-out *planet-report-window* "no "))
                (print-out *planet-report-window* (format nil "dilithium crystals present.~%"))
                (when (shuttle-landed-p (first p-cons))
                  (print-out *planet-report-window*
                             (format nil "~58@A~%" "Shuttle Craft Galileo on surface."))))))))
    (unless one-planet-knownp-p
      (print-out *planet-report-window* (format nil "No information available.~%")))))

(defun use-crystals () ; C: usecrystals(void)
  "Use dilithium crystals."

  (unless *dilithium-crystals-on-board-p*
    (print-message *message-window* (format nil "~%No dilithium crystals available.~%"))
    (return-from use-crystals nil))
  (when (>= *ship-energy* 1000.0)
    (print-message *message-window* (format nil "~%Spock-  \"Captain, Starfleet Regulations prohibit such an operation~%"))
    (print-message *message-window*
                   (format nil "  except when Condition Yellow exists due to low energy supply.~%"))
    (return-from use-crystals nil))

  (print-message *message-window* (format nil "~%Spock- \"Captain, I must warn you that loading~%"))
  (print-message *message-window* (format nil "  raw dilithium crystals into the ship's power~%"))
  (print-message *message-window* (format nil "  system may risk a severe explosion.~%"))
  (print-prompt "  Are you sure this is wise?\" ")
  (unless (get-y-or-n-p)
    (return-from use-crystals nil))
  (print-message *message-window* (format nil "~%Engineering Officer Scott-  \"(GULP) Aye Sir.~%"))
  (print-message *message-window* (format nil "  Mr. Spock and I will try it.\"~%"))
  (print-message *message-window* (format nil "~%Spock-  \"Crystals in place, Sir.~%"))
  (print-message *message-window* (format nil "  Ready to activate circuit.\"~%"))
  (print-message *message-window*
                 (format nil "~%Scotty-  \"Keep your fingers crossed, Sir!\"~%") :print-slowly t)
  (when (<= (random 1.0) *crystal-work-probability*)
    (print-message *message-window*
                   (format nil "  \"Activating now! - - No good!  It's***~%") :print-slowly t)

    (print-message *message-window*
     (format nil "~%***RED ALERT!  RED A*L********************************~%") :print-slowly t)
    (print-stars)
    (print-message *message-window*
                   (format nil "******************   KA-BOOM!!!!   *******************~%")
                   :print-slowly t)
    (kaboom)
    (return-from use-crystals nil))
  (incf *ship-energy* (* 5000.0 (+ 1.0 (* 0.9 (random 1.0)))))
  (print-message *message-window* (format nil "  \"Activating now! - - ~%") :print-slowly t)
  (print-message *message-window* (format nil "   The instruments~%"))
  (print-message *message-window* (format nil "   are going crazy, but I think it's~%"))
  (print-message *message-window* (format nil "   going to work!!  Congratulations, Sir!\"~%"))
  (setf *crystal-work-probability* (* *crystal-work-probability* 2.0))
  (setf *action-taken-p* t))

;; TODO - how to save and restore variables in other packages? :: seems inelegant
(defun restore-game-from-checkpoint (&optional (file-name +checkpoint-file-name+)) ; C: bool thaw(void)
  "Restore a saved game. Return true or false for restore success or failure."

  (with-open-file (s file-name :direction :input :if-does-not-exist nil)
    (setf sst-events::*future-events* (read s))
    (setf *random-state* (read s))
    (setf sst-terminal-io::*line-tokens* (read s))
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
    (setf *planets* (read s))
    (setf *stardate* (read s))
    (setf *base-quadrants* (read s))
    (setf *commander-quadrants* (read s))
    (setf *super-commander-quadrant* (read s))
    (setf *galaxy* (read s))
    (setf *starchart* (read s))
    (setf *snapshot* (read s))
    (setf *quadrant* (read s))
    (setf *quadrant-enemies* (read s))
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
    (setf *crystal-work-probability* (read s))
    (setf *probe* (read s))
    (setf *probes-available* (read s))
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
    (print sst-events::*future-events* s)
    (print *random-state* s)
    (print sst-terminal-io::*line-tokens* s)
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
    (print *planets* s)
    (print *stardate* s)
    (print *base-quadrants* s)
    (print *commander-quadrants* s)
    (print *super-commander-quadrant* s)
    (print *galaxy* s)
    (print *starchart* s)
    (print *snapshot* s)
    (print *quadrant* s)
    (print *quadrant-enemies* s)
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
    (print *crystal-work-probability* s)
    (print *probe* s)
    (print *probes-available* s)
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
       (tournament-number nil)
       input-item)
      (game-type
       (return-from get-game-type (values game-type tournament-number)))
    (unless (input-available-p)
      (print-prompt "Would you like a Regular or Tournament game? "))
    (setf input-item (scan-input))
    (setf game-type (match-token input-item (list "regular" "tournament")))
    (cond
      ((string= game-type "tournament")
       (unless (input-available-p)
         (print-prompt "Type in name or number of tournament: "))
       (setf input-item (scan-input))
       (if input-item
           (setf tournament-number input-item)
           (setf game-type nil))) ; no tournament name or number input so start over

      ((string= game-type "regular")
       t) ; Acknowledge the selection because any other input is an error.

      (t
       (when input-item
         (print-message *message-window* (format nil "What is \"~A\"?~%" input-item)))
       (clear-type-ahead-buffer)))))

(defun get-game-length () ; C: choose()
  "A game parameter used as a multiplier for some game settings."

  (do ((game-length nil)
       input-item)
      (game-length
       (setf *game-length* game-length))
    (unless (input-available-p)
      (print-prompt "Would you like a Short, Medium, or Long game? "))
    (setf input-item (scan-input))
    (setf game-length (match-token input-item (list "short" "medium" "long")))
    (unless game-length
      (when input-item
        (print-message *message-window* (format nil "What is \"~A\"?~%" input-item)))
      (clear-type-ahead-buffer))))

(defun get-skill-level () ; C: choose()

  (do ((skill-level nil)
       input-item)
      (skill-level
       (setf *skill-level* (first (rassoc skill-level *skill-level-labels* :test #'string=))))
    (unless (input-available-p)
      (print-prompt "Are you a Novice, Fair, Good, Expert, or Emeritus player? "))
    (setf input-item (scan-input))
    (setf skill-level (match-token input-item (list "novice" "fair" "good" "expert" "emeritus")))
    (unless skill-level
      (when input-item
        (print-message *message-window* (format nil "What is \"~A\"?~%" input-item)))
      (clear-type-ahead-buffer))))

;; TODO - add an option to generate a password if the player can't be bothered to make one up
(defun get-game-password () ; C: setpassword()
  "Set the self-destruct password."

  (do ((game-password nil)
       input-item)
      (game-password
       (return-from get-game-password game-password))
    (unless (input-available-p)
      (print-prompt "Please type in a secret password: "))
    (setf input-item (scan-input))
    (when input-item
      (setf game-password input-item)
      (when (numberp game-password)
        (setf game-password (write-to-string game-password))))))

(defun set-up-new-game ()
  "Prepare to play - set up the universe. Use input parameters to generate initial core game
values, expecially number of entities in the game."

  (skip-line *message-window*)
  (clear-type-ahead-buffer)
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
    (setf *damage-factor* (* 0.5 *skill-level*))
    (setf *initial-bases* (max (+ (random +max-bases+) 1) +min-bases+))
    (setf *initial-planets* (+ (random (- +max-uninhabitable-planets+ +min-uninhabitable-planets+))
                               +min-uninhabitable-planets+
                               +habitable-planets+))
    (setf *initial-romulans* (* (+ (random 1) 2) *skill-level*))
    (setf *remaining-romulans* *initial-romulans*)
    (setf *cloaking-violations* 0)
    (setf *initial-time* (* 7.0 (rest (assoc *game-length* *game-length-values* :test #'string=))))
    (setf *remaining-time* *initial-time*)
    (setf *initial-klingons*  (truncate (+ (* 2.0 *initial-time*
                                              (- (+ *skill-level* 1)  (random 1))
                                              *skill-level*
                                              0.1)
                                           0.15)))
    (setf *remaining-klingons* *initial-klingons*)
    (setf *captured-klingons* 0)
    (when (> *initial-klingons* 50) ; That's a lot of klingons, give the player another base
      (setf *initial-bases* (1+ *initial-bases*)))
    (setf *initial-commanders* (min +max-commanders-per-game+
                                    (+ *skill-level*
                                       (truncate (* 0.0626 *initial-klingons* (random 1.0))))))
    (if (> *skill-level* +fair+) ; higher skill levels get a super-commander
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
    (do ((i 0 (1+ i)))
        ((>= i +number-of-devices+))
      (setf (aref *device-damage* i) 0.0))
    (setf *brig-capacity* 400)
    (setf *brig-free* *brig-capacity*)
    (setf *cloakedp* nil)
    (setf *cloakingp* nil)
    (setf *dockedp* nil)

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
    (setf *super-commander-attacking-base* 'not-attacking)
    (setf *destroyed-inhabited-planets* 0)
    (setf *destroyed-uninhabited-planets* 0)
    (setf *destroyed-bases* 0)
    (setf *destroyed-stars* 0)
    (setf *shuttle-craft-location* 'on-ship)
    (setf *shuttle-craft-quadrant* nil)
    (setf *landedp* nil)
    (setf *alivep* t)
    (setf *galaxy* (make-array (list +galaxy-size+ +galaxy-size+)))
    (do ((i 0 (1+ i)))
        ((>= i +galaxy-size+))
      (do ((j 0 (1+ j)))
          ((>= j +galaxy-size+))
        (setf (aref *galaxy* i j) (make-quadrant :chartedp nil
                                                 :romulans 0
                                                 :klingons 0
                                                 :starbases 0
                                                 :supernovap nil))))

    (initialize-events)
    ;; Initialize times for extraneous events
    (schedule-event +supernova+ (expran (* 0.5 *initial-time*))) ; C: expran(0.5 * game.intime)
    (schedule-event +tractor-beam+
                    (expran (* 1.5 (/ *initial-time* *initial-commanders*)))) ; C: expran(1.5 * (game.intime / game.state.remcom))
    (schedule-event +snapshot-for-time-warp+ (1+ (random 1))); Force an early snapshot, 1.0 + Rand()
    (schedule-event +commander-attacks-base+ (expran (* 0.3 *initial-time*))) ; C: expran(0.3*game.intime)
    (when (> *remaining-super-commanders* 0)
      (schedule-event +move-super-commander+ 0.2777))
    (when (>= *skill-level* +good+)
      (schedule-event +distress-call-from-inhabited-world+ (expran (+ 1.0 *initial-time*)))) ; C: expran(1.0 + game.intime)

    ;; Initialize the starchart
    (setf *starchart* (make-array (list +galaxy-size+ +galaxy-size+)))
    (do ((i 0 (1+ i)))
        ((>= i +galaxy-size+))
      (do ((j 0 (1+ j)))
          ((>= j +galaxy-size+))
        (setf (aref *starchart* i j) (make-starchart-page :stars 0
                                                          :starbases 0
                                                          :klingons 0))))

    ;; Put stars in the galaxy
    (setf *initial-stars* 0)
    (let (sector-stars)
      (do ((x 0 (1+ x)))
          ((>= x +galaxy-size+))
        (do ((y 0 (1+ y)))
            ((>= y +galaxy-size+))
          (setf sector-stars (truncate (+ (* (random 1.0) +max-stars-per-quadrant+) 1))) ; C: Rand()*9.0 + 1.0
          (incf *initial-stars* sector-stars)
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
    (setf *base-quadrants* nil)
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
                  (decf random-threshold 0.01)))
              (setf candidate-ok-p nil)))))

    ;; Put ordinary Klingon Battle Cruisers in the galaxy
    (let ((klingons-remaining *initial-klingons*)
          (klumper (+ (* 0.25 *skill-level*
                         (- 9.0 (rest (assoc *game-length* *game-length-values* :test #'string=))))
                      1.0)))
      (when (> klumper +max-klingons-per-quadrant+)
        (setf klumper +max-klingons-per-quadrant+))
      (do (random-number klump)
          ((<= klingons-remaining 0))
        (setf random-number (random 1.0))
        (setf klump (truncate (* (- 1.0 (* random-number random-number)) klumper)))
        (when (> klump klingons-remaining)
          (setf klump klingons-remaining))
        (decf klingons-remaining klump)
        (do ((q-coord (get-random-quadrant) (get-random-quadrant))
             (klingons-placed-p nil))
            (klingons-placed-p)
          (when (and (not (quadrant-supernovap (coord-ref *galaxy* q-coord)))
                     (<= (+ (quadrant-klingons (coord-ref *galaxy* q-coord))
                            klump)
                         +max-klingons-per-quadrant+))
            (incf (quadrant-klingons (coord-ref *galaxy* q-coord)) klump)
            (setf klingons-placed-p t)))))

    ;; Put Klingon Commanders in the galaxy
    ;; Select a random location for each commander to be placed based on these filters:
    ;; - If there is a supernova in the quadrant then nothing can be placed there
    ;; - If there are 9 klingons (max klingons per quadrant) there already then don't add another
    ;; - If there are less than 9 klingons then there is a 75% chance of adding the commander
    ;; - If there is already a commander in the quadrant then don't put another one there
    (setf *commander-quadrants* ())
    (do ((i 0 (1+ i))
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
      (incf (quadrant-klingons (coord-ref *galaxy* c-coord)) 1))

    ;; Put planets in the galaxy, one planet per quadrant whether inhabited or uninhabited
    (setf *planets* ())
    (do ((i 0 (1+ i))
         pl
         q)
        ((>= i *initial-planets*))
      (setf pl (make-planet))
      (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
          ((not (assoc q-coord *planets* :test #'coord-equal)) ; nil when planet does not exist
           (setf (planet-quadrant pl) q-coord)
           (setf q q-coord))) ; dumb code alert!
      (if (< i +habitable-planets+)
          (progn
            (setf (planet-class pl) +class-m+) ; All inhabited planets are class M
            (setf (planet-crystals pl) 'absent)
            (setf (planet-knownp pl) nil)
            (setf (planet-destroyedp pl) nil)
            (setf (planet-inhabitedp pl) t)
            (setf (planet-name pl) (aref *system-names* i))
            (setf (planet-status pl) 'secure))
          (progn
            (setf (planet-class pl) (+ (random 2) 1)) ; Planet class M, N, or O
            ;;(setf (planet-crystals pl) (* (random 1.0) 1.5)) ; 1 in 3 chance of crystals
            (if (= (random 2) 0) ; 1 in 3 chance of crystals
                (setf (planet-crystals pl) 'present)
                (setf (planet-crystals pl) 'absent))
            (setf (planet-knownp pl) nil)
            (setf (planet-destroyedp pl) nil)
            (setf (planet-inhabitedp pl) nil)
            (setf (planet-name pl) "")
            (setf (planet-status pl) 'secure)))
      (setf *planets* (acons q pl *planets*)))

    ;; Put Romulans in the galaxy
    (do ((i 0 (1+ i))
         (q-coord (get-random-quadrant) (get-random-quadrant)))
        ((>= i *remaining-romulans*)
         (incf (quadrant-romulans (coord-ref *galaxy* q-coord)) 1)))

    ;; Put the Super Commander in the galaxy
    (when (> *remaining-super-commanders* 0)
      (do ((q-coord (get-random-quadrant) (get-random-quadrant)))
          ((and (not (quadrant-supernovap (coord-ref *galaxy* q-coord)))
                (<= (quadrant-klingons (coord-ref *galaxy* q-coord)) 8))
           (setf *super-commander-quadrant* q-coord)
           (incf (quadrant-klingons (coord-ref *galaxy* q-coord)) 1))))

    ;; Put the thing in the galaxy unless this is a tournament game.
    (setf *thing-location* nil)
    (when (string= game-type "regular")
      (setf *thing-location* (get-random-quadrant)))

    (setf *snapshot-taken-p* nil)

    ;; Introduce the player to the current situation.
    (skip-line *message-window* 2)
    (if (= *skill-level* +novice+)
        (progn
          (print-message *message-window* (format nil "It is stardate ~A. The Federation is being attacked by~%"
                                 (format-stardate *stardate*)))
          (print-message *message-window* (format nil "a deadly Klingon invasion force. As captain of the United~%"))
          (print-message *message-window* (format nil "Starship U.S.S. Enterprise, it is your mission to seek out~%"))
          (print-message *message-window* (format nil "and destroy this invasion force of ~A battle cruisers.~%"
                                 (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
          (print-message *message-window* (format nil "You have an initial allotment of ~A stardates to complete~%"
                                 (truncate *initial-time*)))
          (print-message *message-window* (format nil "your mission. As you proceed you may be given more time.~%"))
          (print-message *message-window* (format nil "~%You will have ~A supporting starbases.~%" *initial-bases*))
          (print-message *message-window* "Starbase locations-  "))
        (progn
          (print-message *message-window* (format nil "Stardate ~A.~%" (truncate *stardate*)))
          (print-message *message-window*
           (format nil "~%~A klingons.~%"
                   (+ *initial-klingons* *initial-commanders* *initial-super-commanders*)))
          (print-message *message-window* (format nil "An unknown number of Romulans.~%"))
          (when (> *remaining-super-commanders* 0)
            (print-message *message-window* (format nil "And one (GULP) Super-Commander.~%")))
          (print-message *message-window* (format nil "~A stardates.~%" (truncate *initial-time*)))
          (print-message *message-window* (format nil "~A starbases in " *initial-bases*))))
    (dolist (bq *base-quadrants*)
      (print-message *message-window* (format nil "~A  " (format-coordinates bq))))
    (skip-line *message-window* 2)
    (print-message *message-window* (format nil "The Enterprise is currently in ~A ~A~%"
                           (format-quadrant-coordinates *ship-quadrant*)
                           (format-sector-coordinates *ship-sector*)))
    (print-message *message-window* (format nil "~%Good Luck!"))
    (when (> *remaining-super-commanders* 0)
      (print-message *message-window* " YOU'LL NEED IT."))
    (skip-line *message-window* 2)
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
    (print-message *message-window* (format nil "The Romulan ship discovers you are breaking the Treaty of Algeron!~%"))
    (setf *cloaking-violations* (1+ *cloaking-violations*))
    (setf *cloaking-violation-reported-p* t)))

(defun display-commands (commands) ; C: listCommands(void)
  "Print a list of valid commands."

  (print-message *message-window* (format nil "Valid commands are:~%~%"))
  (dolist (command-line (split-sequence #\newline (format nil "~{~12@<~%~:;~A ~>~}" commands)))
    (print-message *message-window* (format nil "~A~%" command-line))))

(defun make-moves ()
  "Command interpretation loop. This is the heart of the game: read commands and carry them out.
The loop ends when the player wins by killing all Klingons, is killed, or decides to exit or quit."

  ;; Commands that should match the shortest possible string come first in the list.
  (do ((exit-game-p nil)
       (commands (list "abandon" "chart" "capture" "cloak" "commands" "computer" "crystals" "dock"
                       "damages" "deathray" "destruct" "emexit" "exit" "help"
                       "impulse" "lrscan" "move" "mayday" "mine" "orbit" "phasers" "photons"
                       "planets" "probe" "quit" "rest" "report" "request" "srscan" "status" "score"
                       "sensors" "shields" "shuttle" "transport" "torpedoes" "visual" "warp"
                       "windows"))
       command
       hit-me-p) ;; When true, player has taken an action which consumes game time or a turn,
                 ;; after which enemies take their action turn, usually an attack.
      ((or *all-done-p*
           exit-game-p))
    (update-windows)
    (skip-line *message-window*)
    (restart-paging *message-window*)
    (setf hit-me-p nil)
    (setf *just-in-p* nil)
    (setf *time-taken-by-current-operation* 0.0)
    (setf *action-taken-p* nil)
    (print-prompt "COMMAND: ")
    ;; TODO - possible error: after attacking but not killing a Klingon with a torpedo the Klingon
    ;;        returned fire. The next command "pho 1 4 3" was interpreted as "NIL 1 4 3"
    (clear-type-ahead-buffer)
    ;; TODO - fix match-token to ignore all previous input on -1
    (setf command (match-token (scan-input) commands))
    ;; In windowed mode commands are lost when the prompt window is refreshed.
    (unless (eql *prompt-window* *message-window*)
      (print-message *message-window* (format nil "COMMAND: ~A~A~%"
                                              command (format-input-items))))
    ;; TODO - check that commands that must be typed in full were: abandon destruct quit deathray cloak mayday
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
         (unless (damagedp +cloaking-device+) ; Don't cloak if we got damaged while cloaking!
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
       (short-range-scan *short-range-scan-window*))
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
      ((string= command "windows")
       (toggle-windows))
      (t ; Unusable input, print a list of valid commands.
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

          ;; After docking player is attacked immediately, not after events are processed. This
          ;; also means that if a player arrives by chance next to a starbase and docks he will
          ;; be attacked immediately, although without photon torpedoes.
          ((and hit-me-p
                (not *just-in-p*))
           (attack-player :torpedoes-ok-p t)
           (unless *all-done-p*
             (if (quadrant-supernovap (coord-ref *galaxy* *ship-quadrant*))
                 (emergency-supernova-exit) ; Attacker caused a supernova
                 (setf turn-ended-p t))))

          (t
           (setf turn-ended-p t)))))
    (checkpoint-game)))

(defun print-banner () ; C: prelim()
  "Print an introductory banner."

  (clear-window *message-window*)
  (print-message *message-window* (format nil "-SUPER- STAR TREK~%")))

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
        (print-message *message-window*
                       (format nil "~%Enter `help' for help, `commands' for commands.~%~%"))))
    (make-moves)
    (setf play-again-p nil)

    (skip-line *message-window*)
    (print-stars)
    (when *all-done-p*
      (skip-line *message-window*)
      (print-prompt "Do you want to play again? ")
      (setf play-again-p (get-y-or-n-p))
      (when play-again-p
        (clear-screen))))

  (clean-up)
  (format t "~%May the Great Bird of the Galaxy roost upon your home planet.~%")
  (sb-ext:exit))
